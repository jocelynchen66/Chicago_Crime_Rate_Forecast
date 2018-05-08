library(dplyr)
require(reshape)
library(ggplot2)
library(ggmap)
library(forecast)
library(zoo)


# read the data
# data source: https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2

Crimes_._2001_to_present <- read.csv("~/Desktop/Crimes_-_2001_to_present.csv")

df <- Crimes_._2001_to_present
df$Primary.Type <- gsub("NON-CRIMINAL", "NON - CRIMINAL", df$Primary.Type)

df1 <- subset(df, Year < 2018)
df2 <- subset(df, Year == 2018)


primary_type <- ggplot(df, aes(df$Primary.Type))
primary_type + geom_histogram(stat = "count") + coord_flip()


##############################################################################
# Mapping
##############################################################################
df <- subset(df, !is.na(Longitude) & !is.na(Latitude)) # remove missing values
Chicago <- get_map(location = c(lon = mean(df$Longitude), lat = mean(df$Latitude)), zoom = 10, maptype = "roadmap", scale = 2)
# Chicago <- get_map(location = c(lon = mean(df$Longitude), lat = mean(df$Latitude)), zoom = 12, maptype = "roadmap", scale = 2)
ggmap(Chicago) + geom_point(data = df, aes(x = Longitude, y = Latitude, fill = "red", alpha = 0.8), size = 1, shape = 21) + guides(fill=FALSE, alpha=FALSE, size=FALSE)
Pos <- as.data.frame(table(round(df$Longitude,2), round(df$Latitude,2)))
Pos$Var1 <- as.numeric(as.character(Pos$Var1))
Pos$Var2 <- as.numeric(as.character(Pos$Var2))
Pos <- subset(Pos, Freq > 0)
ggmap(Chicago) + geom_tile(data = Pos, aes(x = Var1, y = Var2, alpha = Freq), fill = "red", colour = "white")


##############################################################################
# Crimes vs. Time
##############################################################################
df1$Date <- strptime(df1$Date, format = "%m/%d/%Y %I:%M:%S %p")
df1$Weekday <- weekdays(df1$Date)
df1$Hours <- df1$Date$hour
df1$Quarter <- quarters(df1$Date)
df1$Month <- months(df1$Date)

HourCount <- as.data.frame(table(df1$Weekday, df1$Hours))
HourCount$Var1 <- factor(HourCount$Var1, ordered = T, levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday"))
ggplot(HourCount, aes(x = Var2, y = Var1)) + geom_tile(aes(alpha = Freq), fill = "black", color = "white") + labs(x = "Hour", y = "Weekday")
ggplot(HourCount, aes(x = Var2, y = Freq, color = Var1)) + geom_line(alpha = 0.6) + labs(x = "Hour", y = "Frequency", colour = "Weekday")

YearCount <- as.data.frame(table(df1$Year))
ggplot(YearCount, aes(x = Var1, y = Freq, group = 1, color = Var1)) + geom_line(alpha = 0.6, size = 1.5) + labs(x = "Year", y = "Frequency")

MonthCount <- as.data.frame(table(df1$Year, df1$Month))
MonthCount$Var2 <- factor(MonthCount$Var2, ordered = T, levels = c("January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November","December"))
ggplot(MonthCount, aes(x = Var2, y = Freq, group = Var1, color = Var1)) + geom_line(alpha = 0.6) + labs(x = "Month", y = "Frequency", colour = "Year")
MonthCount <- MonthCount[order(MonthCount$Var1, MonthCount$Var2),]

QuarterCount <- as.data.frame(table(df1$Year, df1$Quarter))
ggplot(QuarterCount, aes(x = Var2, y = Freq, group = Var1, color = Var1)) + geom_line(alpha = 0.6) + labs(x = "Quarter", y = "Frequency", colour = "Year")

df2$Date <- strptime(df2$Date, format = "%m/%d/%Y %I:%M:%S %p")
df2$Month <- months(df2$Date)

MonthCount1 <- as.data.frame(table(df2$Year, df2$Month))
MonthCount1$Var2 <- factor(MonthCount1$Var2, ordered = T, levels = c("January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November","December"))
MonthCount1 <- MonthCount1[order(MonthCount1$Var1, MonthCount1$Var2),]

MonthCount2 <- as.data.frame(table(df2$Month))
MonthCount2$Var1 <- factor(MonthCount2$Var1, ordered = T, levels = c("January", "February", "March", "April"))
MonthCount2 <- MonthCount2[order(MonthCount2$Var1),]
Actu <- MonthCount2$Freq



##############################################################################
# Modeling Part
##############################################################################
freq <- ts(MonthCount$Freq, start = c(2001,1), end = c(2017,12), frequency = 12)
plot(decompose(freq, type="additive"))
acf(freq)
pacf(freq)

ets_Pred1 = forecast(freq, h = 4)
plot(ets_Pred1)

error1 <- as.numeric(ets_Pred1$mean)
accu1 <- 1 - sum(abs(Actu - error1)/Actu)

ets_Pred2 = forecast(freq, h = 12)
plot(ets_Pred2)


arima_model = auto.arima(Monthhh$Freq)
arima_Pred1 = forecast(arima_model, h=4)
plot(arima_Pred1)

error2 <- as.numeric(arima_Pred1$mean)
accu2 <- 1 - sum(abs(Actu - error2)/Actu)

arima_Pred2 = forecast(arima_model, h=12)
plot(arima_Pred2)
