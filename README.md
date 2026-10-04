# BDA400 - Assignment 6
## Technical Analysis using R - Visualization Phase

**Student:** Liz Valerie Villena  
**Course:** BDA400 - Data Science Tools and Techniques

### Project Description
This project is an interactive portfolio technical-analysis dashboard developed with R Shiny. It retrieves historical stock information from Yahoo Finance using `quantmod` and allows the user to explore stock performance using multiple time frames and chart types.

The dashboard includes:
- Stock-symbol input and date-range filtering
- Daily, Weekly, and Monthly time frames
- Line, Candlestick, and Area chart options
- Moving Averages with customizable short- and long-term periods
- RSI and MACD technical indicators with dynamic on/off controls
- Buy/Sell annotations based on moving-average crossover rules
- Latest trading information table
- Basic error handling for invalid or unavailable stock symbols

### Required R Packages
```r
install.packages(c("shiny", "ggplot2", "quantmod", "TTR", "dplyr"))
```

### How to Run
1. Open `app.R` in RStudio.
2. Install the required packages if necessary.
3. Click **Run App**.
4. Enter a Yahoo Finance symbol such as `AAPL` or `MSFT`.
5. Choose the date range, time frame, chart type, and technical indicators.
6. Click **Refresh Data** after changing the stock symbol or date range.

### Trading Rule
A **Buy** condition occurs when the short moving average is above the long moving average.  
A **Sell** condition occurs when the short moving average is below the long moving average.  
The chart annotates crossover points where the state changes.

### Validation
The application was tested with AAPL and MSFT, including line/candlestick views, Moving Averages, RSI, MACD, and Buy/Sell annotations.

### Repository
**Repository link:** ADD_YOUR_ASSIGNMENT_6_GITHUB_LINK_HERE

> Before LMS submission, replace the placeholder above with the public GitHub repository URL.
