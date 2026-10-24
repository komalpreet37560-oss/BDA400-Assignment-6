# BDA400 Assignment 6 — Sarvesh Sharma

Interactive R Shiny portfolio dashboard.

## Run
1. Install R and RStudio (or use a hosted R environment).
2. Run `install.packages(c("shiny", "ggplot2", "quantmod"))`.
3. Open `app.R` and click **Run App**, or run `shiny::runApp(".")`.
4. Internet access is needed for Yahoo Finance historical stock data.
5. Enter portfolio tickers, select stock/date/time frame/chart type, toggle MA/RSI/MACD, and review crossover signals.
6. Add screenshots from a real running app and a valid GitHub repository link before submission.

## Limitations
Yahoo Finance access may be rate-limited or unavailable. No live data or RStudio execution has been verified in this package. Signals are illustrative, not investment advice. RSI/MACD are displayed in separate panels to avoid misleading price-scale overlays.
