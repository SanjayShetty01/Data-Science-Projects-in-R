# Loading all the Library

library(quantmod)
library(dplyr)
library(PerformanceAnalytics)

# Analyzing stock data with Quantmod

# getting Microsoft Stock Data

MSFT <- getSymbols("MSFT", auto.assign = F)

head(MSFT)
tail(MSFT)
dim(MSFT)

new <- MSFT[1:30, 1:3]

MSFT['2007-01-03']

df <-  as.data.frame(MSFT)
class(df)
head(df)

df <-  as.xts(df)

# Saving table in the local space

write.csv(df, "MSFT.csv")

# Daily Return

# get Tesla stock data (ticker sign: TSLA)

TSLA <- getSymbols("TSLA", auto.assign = F)

head(TSLA, 20)
dim(TSLA)

New_TSLA <-  dailyReturn(TSLA$TSLA.Adjusted)

plot(New_TSLA, type = "l")

 # get Microsoft Stock data

MSFT <- getSymbols("MSFT", auto.assign = F)

head(MSFT, 20)
dim(MSFT)

New_MSFT <- dailyReturn(MSFT$MSFT.Adjusted)


plot(New_MSFT, type = "l")

# Taking Facebook Stock Data

FB <- getSymbols("FB", auto.assign = F)

head(FB,20)
dim(FB)

New_FB <- dailyReturn(FB$FB.Adjusted)

plot(New_FB, type = "l")

# Comparison of all three stocks

Comb <- merge(New_TSLA, New_MSFT, New_FB)
head(Comb1)

Comb_traded <- merge(New_TSLA, New_MSFT, New_FB, all = F)
head(Comb_traded)

# Change the column names
colnames(Comb_traded) <-  c("Tesla Returns", "Microsoft Returns", "Facebook Returns")

charts.PerformanceSummary(Comb_traded, main = "Tesla v/s  Microsoft v/s Facebook")

# Sharp Ratio

table.AnnualizedReturns(Comb_traded, scale = 252, Rf = 0.04/252)

# Create a Technical indicator (Chart)

### 1. Bollinger Band

chartSeries(New_TSLA,
            subset = "2012",
            TA = c("addBBands(n =20, sd = 2)",
                    addRSI(),
                    addEMA(n =30)),
            theme = chartTheme("white"))

chartSeries(New_MSFT,
            subset = "2012",
            TA = c("addBBands(n =20, sd = 2)",
                   addRSI(),
                   addEMA(n =30)),
            theme = chartTheme("white"))

chartSeries(New_FB,
            subset = "2012",
            TA = c("addBBands(n =20, sd = 2)",
                   addRSI(),
                   addEMA(n =30)),
            theme = chartTheme("white"))


# Creating a signal to buy or sell a stock

## Use close price to create a buy or sell signal 

TSLA_Cl <- TSLA$TSLA.Close
head(TSLA_Cl)


## Create % Change in stock

daily_change <- TSLA_Cl/Lag(TSLA_Cl,1)-1

hist(daily_change, 40, col = "Blue")


## Creating a signal, where if the daily change is more than 0.04, there would be a buy signal

buy_signal <- 0.04

## Loop over all trading days
signal <- c(NULL)

for(i in 2: length(TSLA_Cl)){
  if (daily_change[i] > buy_signal){
    signal[i] <-  1
    
  }else
    signal[i] <- 0
}

## Since the signal is a dataframe and TSLA_Cl is a XTS object, re-class signal to XTS

signal <- reclass(signal, TSLA_Cl)

head(signal,20)

## Chart the series

chartSeries(TSLA_Cl,
            type = "l",
            theme = chartTheme("white"))
addTA(signal, type = "s", col = "Green")




