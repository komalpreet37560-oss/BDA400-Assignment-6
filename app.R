# BDA400 Assignment 6 — Interactive Portfolio Dashboard
# Data source: Yahoo Finance through quantmod. Requires internet access.
required <- c("shiny", "ggplot2", "quantmod")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Install required packages: install.packages(c(", paste(sprintf('"%s"',missing),collapse=", "), "))")
library(shiny)
library(ggplot2)
library(quantmod)

ui <- fluidPage(
  titlePanel("Technical Analysis | Portfolio Dashboard"),
  sidebarLayout(sidebarPanel(
    textInput("symbols", "Portfolio symbols (comma-separated)", "AAPL,MSFT,GOOG"),
    selectInput("symbol", "Chart stock", choices=c("AAPL","MSFT","GOOG")),
    dateRangeInput("dates", "Date range", start=Sys.Date()-365, end=Sys.Date()),
    selectInput("timeframe", "Time frame", choices=c("Daily","Weekly","Monthly")),
    selectInput("chart", "Chart type", choices=c("Line","Area","Candlestick")),
    checkboxGroupInput("indicators", "Indicators", c("Moving Averages","RSI","MACD"), selected="Moving Averages"),
    numericInput("short", "Short moving average", 20, min=2, max=100),
    numericInput("long", "Long moving average", 50, min=3, max=250),
    checkboxInput("signals", "Annotate crossover Buy/Sell signals", TRUE),
    actionButton("refresh", "Refresh portfolio")
  ),mainPanel(
    textOutput("status"), tableOutput("portfolio"), plotOutput("stock_chart",height="430px"),
    conditionalPanel("input.indicators.includes('RSI')",plotOutput("rsi_chart",height="180px")),
    conditionalPanel("input.indicators.includes('MACD')",plotOutput("macd_chart",height="180px")),
    h4("Recent trading signals"),tableOutput("signal_table"),
    helpText("Educational demonstration only. Buy/Sell markers indicate MA crossovers, not investment advice.")
  ))
)
server <- function(input, output, session) {
  symbols <- reactive({
    x <- toupper(trimws(strsplit(input$symbols, ",", fixed=TRUE)[[1]]))
    x <- unique(x[nzchar(x) & grepl("^[A-Z0-9.^-]+$",x)])
    validate(need(length(x)>0, "Enter at least one valid stock symbol"))
    head(x,10)
  })
  observeEvent(symbols(), {
    updateSelectInput(session,"symbol",choices=symbols(),selected=if(input$symbol %in% symbols()) input$symbol else symbols()[1])
  })
  fetched <- eventReactive(list(input$refresh, input$dates, symbols()), {
    syms <- symbols(); results <- setNames(vector("list",length(syms)),syms)
    for (s in syms) {
      results[[s]] <- tryCatch({
        x <- suppressWarnings(getSymbols(s,src="yahoo",from=as.Date(input$dates[1])-90,
                                          to=as.Date(input$dates[2])+1,auto.assign=FALSE))
        if(NROW(x)==0) stop("No historical prices")
        x
      },error=function(e) structure(list(error=conditionMessage(e)),class="fetch_error"))
    }
    results
  },ignoreNULL=FALSE)
  output$status <- renderText({
    bad <- names(Filter(function(x) inherits(x,"fetch_error"),fetched()))
    if(length(bad)) paste("Unavailable symbols:",paste(bad,collapse=", "),"— check internet, dates or ticker")
    else "Historical data loaded from Yahoo Finance"
  })
  output$portfolio <- renderTable({
    x <- fetched(); good <- names(Filter(function(z) !inherits(z,"fetch_error"),x))
    if(!length(good)) return(data.frame(Message="No prices available"))
    do.call(rbind,lapply(good,function(s){
      z <- x[[s]]; close <- as.numeric(Cl(z));
      data.frame(Symbol=s,Observations=length(close),Last_Close=round(tail(close,1),2),
                 Change_Pct=round(100*(tail(close,1)/head(close,1)-1),2))
    }))
  })
  selected <- reactive({
    req(input$symbol)
    z <- fetched()[[input$symbol]]
    validate(need(!is.null(z) && !inherits(z,"fetch_error"),"Selected symbol unavailable"))
    if(input$timeframe=="Weekly") z <- to.weekly(z,indexAt="endof",drop.time=TRUE)
    if(input$timeframe=="Monthly") z <- to.monthly(z,indexAt="endof",drop.time=TRUE)
    z <- z[index(z)>=as.Date(input$dates[1]) & index(z)<=as.Date(input$dates[2])]
    validate(need(NROW(z)>1,"Choose a wider date range"))
    z
  })
  enriched <- reactive({
    z <- selected(); validate(need(input$short<input$long,"Short MA must be smaller than long MA"))
    d <- data.frame(Date=as.Date(index(z)),Open=as.numeric(Op(z)),High=as.numeric(Hi(z)),
                    Low=as.numeric(Lo(z)),Close=as.numeric(Cl(z)),Volume=as.numeric(Vo(z)))
    d$ShortMA <- as.numeric(TTR::SMA(d$Close,n=input$short))
    d$LongMA <- as.numeric(TTR::SMA(d$Close,n=input$long))
    d$RSI <- as.numeric(TTR::RSI(d$Close,n=14))
    m <- TTR::MACD(d$Close,nFast=12,nSlow=26,nSig=9,maType="EMA")
    d$MACD <- as.numeric(m[,1]); d$MACDSignal <- as.numeric(m[,2])
    d$Signal <- "Hold"
    if(nrow(d)>1) for(i in 2:nrow(d)) {
      a <- d$ShortMA[i-1]; b <- d$LongMA[i-1]; c <- d$ShortMA[i]; e <- d$LongMA[i]
      if(all(is.finite(c(a,b,c,e)))) {
        if(a<=b && c>e) d$Signal[i] <- "Buy"
        if(a>=b && c<e) d$Signal[i] <- "Sell"
      }
    }
    d
  })
  output$stock_chart <- renderPlot({
    d <- enriched(); p <- ggplot(d,aes(x=Date)) + labs(title=paste(input$symbol,"price and technical indicators"),x=NULL,y="Price (USD)") + theme_minimal(base_size=12)
    if(input$chart=="Area") p <- p+geom_area(aes(y=Close),fill="#b6d9ec",alpha=.8)+geom_line(aes(y=Close),color="#14638a")
    else if(input$chart=="Candlestick") {
      d$Direction <- ifelse(d$Close>=d$Open,"Up","Down")
      p <- p+geom_linerange(data=d,aes(ymin=Low,ymax=High,color=Direction))+
        geom_rect(data=d,aes(xmin=Date-.32,xmax=Date+.32,ymin=pmin(Open,Close),ymax=pmax(Open,Close),fill=Direction),color="gray30")+
        scale_color_manual(values=c(Down="#c0392b",Up="#17865c"))+
        scale_fill_manual(values=c(Down="#c0392b",Up="#17865c"))
    } else p <- p+geom_line(aes(y=Close),color="#14638a",linewidth=.7)
    if("Moving Averages" %in% input$indicators) p <- p+geom_line(aes(y=ShortMA,color="Short MA"),linewidth=.7,na.rm=TRUE)+geom_line(aes(y=LongMA,color="Long MA"),linewidth=.7,na.rm=TRUE)+scale_color_manual(values=c("Short MA"="#ef8c2f","Long MA"="#7b4da9"))
    if(isTRUE(input$signals)) {
      events <- d[d$Signal!="Hold",]
      if(nrow(events)) p <- p+geom_point(data=events,aes(y=Close,shape=Signal),size=3,color="#202020")+
        geom_text(data=events,aes(y=Close,label=Signal),vjust=-1,size=3,check_overlap=TRUE)
    }
    p
  })
  output$rsi_chart <- renderPlot({
    d <- enriched(); ggplot(d,aes(Date,RSI))+geom_line(color="#6942a0",na.rm=TRUE)+
      geom_hline(yintercept=c(30,70),linetype="dashed",color="#b05d5d")+
      coord_cartesian(ylim=c(0,100))+labs(title="Relative Strength Index (14)",x=NULL,y="RSI")+theme_minimal()
  })
  output$macd_chart <- renderPlot({
    d <- enriched(); ggplot(d,aes(Date))+geom_col(aes(y=MACD-MACDSignal),fill="#b7cadb",na.rm=TRUE)+
      geom_line(aes(y=MACD,color="MACD"),na.rm=TRUE)+geom_line(aes(y=MACDSignal,color="Signal"),na.rm=TRUE)+
      scale_color_manual(values=c(MACD="#196b91",Signal="#d47d2b"))+
      labs(title="MACD (12,26,9)",x=NULL,y="MACD",color="Line")+theme_minimal()
  })
  output$signal_table <- renderTable({
    d <- enriched(); x <- d[d$Signal!="Hold",c("Date","Close","Signal")]
    if(!nrow(x)) return(data.frame(Message="No crossover events in selected range"))
    tail(x,12)
  })
}
shinyApp(ui,server)
