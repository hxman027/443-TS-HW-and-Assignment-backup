setwd("~/Desktop/STAT 443/Webwork and HW/Assignment 1")
library(tseries)
bradtemp <- read.csv("bradforddata.csv", header = T) 
tsbrad <- ts(bradtemp$Max.Temp, start = c(1930, 1), frequency = 12)
plot.ts(tsbrad,
        main = "Max Monthly Temperature from Jan 1930 to Dec 2017",
        xlab = "Date",
        ylab = "Max Temperature (Celsius)")
abline(h=mean(tsbrad), col = "red")
acf(tsbrad)


training <- window(tsbrad, start=c(1950,1), end =c(2014,12))
test <- window(tsbrad, start=c(2015,1), end =c(2017,12))

###Transformation is not needed 

##b)
movingavg <- decompose(training, type = c("additive"))
#movingavg$figure is the estimated seasonal effects
plot(movingavg)

loessmeth <- stl(training, s.window = "periodic")
plot(loessmeth, main = "Loess Smoothing")

##c)
#for moving avg:
timeframe <- 1:(12*(2014-1950+1)) ##list representing month number from 1 to 780
matrend <- movingavg$trend
(summary(linmod <- lm(matrend~timeframe)))

#for loess:
lodata <- loessmeth$time.series
lotrend <- lodata[,2]
(summary(lomod <- lm(lotrend~timeframe)))
loseason <- lodata[,1]

#Predictions for moving avg and loess:
predtime <- data.frame(781:816) #predicting time month number from Jan 2015 - Dec 2017
names(predtime) = "timeframe"

mvpred <- predict(linmod, newdata = predtime) + movingavg$figure[rep(1:12, 3)] 
lopredict <- predict(lomod, newdata = predtime) + loseason[rep(1:12, 3)]

tsmvpred <- ts(mvpred, start=c(2015,1), frequency = 12 )
tslopredict <- ts(lopredict, start=c(2015,1), frequency = 12 )

plot.ts(test)
lines(tsmvpred, col = "red")
lines(tslopredict, col = "blue")


(MSPEmv <- mean((mvpred - test)^2))
(MSPELo <- mean((lopredict - test)^2))

###Findings: similar result?

nontrendpred <- ts(linmod$coefficients[1] + movingavg$figure[rep(1:12, 3)], start=c(2015,1), frequency = 12)
lines(nontrendpred, col = "green")
notrend <- lm(matrend~1)
notrend$coefficients

MSPENT <- mean((nontrendpred - test)^2)
print(MSPENT)
###yes models with the trend has a lower MSPE

######2
library(zoo)
weather <- read.csv("histWeather.csv", header = T)
dat <- subset(weather, weather$AirPtCd == "KAVP")

#daily
zoodailymaT <- zoo(dat$Max_TemperatureF, as.Date(dat$Date))
zoodailymeanW <- zoo(dat$Mean_Wind_SpeedMPH, as.Date(dat$Date))

zoomonmaT <-aggregate(zoodailymaT, as.yearmon, mean)
zoomonmeanW <- aggregate(zoodailymeanW, as.yearmon, mean)

plot(zoodailymaT, col = "black", main = "Daily Max Temp(F)")
plot(zoodailymeanW, col = "hot pink", main = "Daily Mean Wind Speed")
plot(zoomonmaT, col = "black", main = "Monthly Mean Max Temp(F)")
plot(zoomonmeanW, col = "hot pink", main = "Monthly Mean Mean Wind Speed")

#head(weather)
#the data shows a periodicity of 1 year, so the series is not staiontary. There does not appear to have a clear trend. 
#There is no seasonaly variation, 

