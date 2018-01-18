#setwd("~/Desktop/STAT 443/Webwork and HW/Assignment 1")
bradtemp <- read.csv("bradforddata.csv", header = T) 
tsbrad <- ts(bradtemp$Max.Temp, start = c(1930, 1), frequency = 12)
plot.ts(tsbrad,
        main = "Max Monthly Temperature from Jan 1930 to Dec 2017",
        xlab = "Date",
        ylab = "Max Temperature (Celsius)")


training <- window(tsbrad, start=c(1950,1), end =c(2014,12))
test <- window(tsbrad, start=c(2015,1), end =c(2017,12))

###Transformation is not needed 

##b)
movingavg <- decompose(training, type = c("additive"))
#movingavg$figure #this is the estimated seasonal effects
plot(movingavg)

loessmeth <- stl(training, s.window = "periodic")
plot(loessmeth)

##c)
#for moving avg:
timeframe <- 1:(12*(2014-1950+1)) ##list representing month number from 1 to 780
matrend <- movingavg$trend
linmod <- lm(matrend~timeframe)
linmod
#for loess:
lodata <- loessmeth$time.series
lotrend <- lodata[,2]
lomod <- lm(lotrend~timeframe)
loseason <- lodata[,1]

## Predictions
#for moving avg:
predtime <- data.frame(781:816) #predicting time month number
names(predtime) = "timeframe"

linpred <- predict(linmod, newdata = predtime) + movingavg$figure[rep(1:12, 3)] 
lopredict <- predict(lomod, newdata = predtime) + loseason[rep(1:12, 3)]

tslinpred <- ts(linpred, start=c(2015,1), frequency = 12 )
tslopredict <- ts(lopredict, start=c(2015,1), frequency = 12 )

plot.ts(test)
lines(tslinpred, col = "red")
lines(tslopredict, col = "blue")


MSPElin <- mean((linpred - test)^2)
MSPELo <- mean((lopredict - test)^2)
print(MSPElin)
print(MSPELo)
###Findings: similar result?

nontrendpred <- ts(linmod$coefficients[1] + movingavg$figure[rep(1:12, 3)], start=c(2015,1), frequency = 12)
lines(nontrendpred, col = "green")

MSPENT <- mean((nontrendpred - test)^2)
print(MSPENT)
###yes models with the trend has a lower MSPE

######2
library(zoo)
weather <- read.csv("histWeather.csv", header = T)
dat <- subset(weather, weather$AirPtCd == "KAVP")

#daily
zoodailyma <- zoo(dat$Max_TemperatureF, as.Date(dat$Date))
zoodailymea <- zoo(dat$Mean_TemperatureF, as.Date(dat$Date))
plot(zoodailyma)
plot(zoodailymea)

plot(aggregate(zoodailyma, as.yearmon, mean))
plot(aggregate(zoodailymea, as.yearmon, mean))

#the data shows a periodicity of 1 year, so the series is not staiontary. There does not appear to have a clear trend. 
#There is no seasonaly variation, 

