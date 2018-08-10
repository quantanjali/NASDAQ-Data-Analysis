#******************************************************************************************************************************************************************************
# Developer : Kumari Anjali
# NASDAQ Composite data analysis for returns, descriptive statistics and how  daily returns of NASDAQ distributed? Does it follow a normal distribution?
#******************************************************************************************************************************************************************************

# NOTE : Analysis for NASDAQ Composite data (^IXIC)
# All relevant Standard Packages are mentioned below. If its not installed on your system, please install on prompt by this program
# Platform : This code has been developed on R 3.4.3 GUI 1.70 El Capitan build (7463), Normal R version 3.4.3 for MAC OS El Capitan
# Disclaimer : I have developed and tested these codes on MacBook Pro, MAC OS - El Capitan,where its running fine. There may be some error on other OS platform


###############################################################################################################
# Installing Library Function, if required
###############################################################################################################

if (!require(quantmod)) install.packages('quantmod')
if (!require(xts)) install.packages('xts')
if (!require(zoo)) install.packages('zoo')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
if (!require(PerformanceAnalytics)) install.packages('PerformanceAnalytics')
if (!require(readr)) install.packages('readr')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(stringr)) install.packages('stringr')


###############################################################################################################
# Loading Library Function
###############################################################################################################

library(quantmod)
library(xts)
library(zoo)
library(ggplot2)
library(BatchGetSymbols)
library(PerformanceAnalytics)
library(readr)
library(tidyverse)
library(stringr)
library(reshape2)

###############################################################################################################
# Download data for last 2 years for the NASDAQ Composite. Downloaded data from Yahoo Finance
###############################################################################################################

getSymbols("^IXIC",src="yahoo", from="2016-03-07", to="2018-03-07") 
IXIC

###############################################################################################################
# Calculate Daily returns of NASDAQ composite for the last 1 year 
###############################################################################################################

getSymbols("^IXIC",src="yahoo", from="2017-03-07", to="2018-03-07") 
OHLCData <- IXIC
colnames(OHLCData)
#IXIC_adjustedClose<-OHLCData[,6]
IXIC_adjustedClose<-OHLCData$IXIC.Adjusted
IXIC_adjustedClose

# Calculation and displaying Arithmetic returns
IXIX_DailyReturn<-dailyReturn(IXIC_adjustedClose, subset=NULL, type="arithmetic", leading=TRUE)
IXIX_DailyReturn

# Calculation and displaying log returns
IXIX_Daily_LogReturn<-dailyReturn(IXIC_adjustedClose, subset=NULL, type="log", leading=TRUE)
IXIX_Daily_LogReturn


###############################################################################################################
# Graphically represent the stock prices as a line plot 
###############################################################################################################


# Below two commands can be also used for plots
plot(IXIC_adjustedClose, type="l", col="blue", main="NASDAQ Composite : One year trend", xlab= "Time", ylab="Adjusted Closing Price")

# chartSeries(IXIC_adjustedClose, theme="white", col="blue", main="NASDAQ Composite : One year trend", xlab= "Time", ylab="Adjusted Closing Price")


###############################################################################################################
#Question 4: Bucket the daily return values into bins and plot a histogram 
###############################################################################################################


# Bucketisation and Frequency Table for Overall Portfoilio
colors = c("violet", "royalblue", "green", "yellow","orange", "red")
mi = min(IXIX_DailyReturn)
mx= max(IXIX_DailyReturn) 
rnge = mx - mi
rnge
intervls = rnge/10                  #You can change class interval by changing this number
breaks = seq(mi,mx, by=intervls)
xcut = cut(IXIX_DailyReturn, breaks, right=FALSE)
freq = table(xcut)
freq

barplot(freq, col=colors, main="Overall Return Distribution", 
						xlab=" Return Interval ->", 
						ylab=" Frequency",)


hist(IXIX_DailyReturn,
     main="Histogram for daily Returns",
     xlab="Returns",
     border="yellow",
     ylab="Frequency",
     col="blue",
     xlim=c(-.03,0.03),
     las=1,
     breaks=10)                   #You can change class interval by changing break NUMBER
lines(density(IXIX_DailyReturn), col="red")   


###############################################################################################################
# Question 5. Calculate mean, median and standard deviation of Daily return values and plot them on the same graph mentioned in step IV 
###############################################################################################################

IXIX_DailyReturn
Return<-c(IXIX_DailyReturn)
mean(Return)
median(Return)
sd(Return)
summary(Return)

hist(Return, # histogram
 col = "peachpuff", # column color
 border = "black", 
 prob = TRUE, # show densities instead of frequencies
 xlim = c(-.03,0.03),
 ylim = c(0,100),
 xlab = "Returns",
 ylab = "Frequency",
 las=1,
 breaks=10,
 main = "Histogram for daily Returns")
lines(density(Return),lwd = 2,col = "chocolate3")

abline(v = mean(Return),
 col = "royalblue",
 lwd = 2)
# And a line for the median:
abline(v = median(Return),
 col = "red",
 lwd = 2)

abline(v = sd(Return),
 col = "green",
 lwd = 2)
 
legend(x = "topright", # location of legend within plot area
 c("Density plot", "Mean", "Median","sd"),
 col = c("chocolate3", "royalblue", "red","green"),
 lwd = c(2, 2, 2,2))


###############################################################################################################
# END
###############################################################################################################






