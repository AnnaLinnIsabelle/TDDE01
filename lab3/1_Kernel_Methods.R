set.seed(1234567890)
library(geosphere)

stations <- read.csv("stations.csv", header=TRUE, sep=",", dec=".", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations, temps, by="station_number")

st = st[,c("date", "time", "latitude", "longitude", "air_temperature")]
st$date = as.Date(st$date)
date <- as.Date("2013-11-04") #The date to predict (up to the students)
filtered_st = st[st$date < date, ]
filtered_st$time = strptime(filtered_st$time, "%H:%M:%S")

#Set all dates to year 2017
filtered_st$date = as.Date(sub('\\d{4}(?=-)', '2000', filtered_st$date, perl=TRUE))

#calculates distance in days between a reference date as a string and a date in format date
dist_date <- function(refDate, date){
  return (as.numeric(difftime(date, as.Date(refDate), units = "days")))
}

#calculates distance in minuites between a reference time given as string and a time given as strptime
dist_time <- function(refTime, time) {
  return(as.numeric(difftime(time, strptime(refTime, "%H:%M:%S"), units = "mins")))
}

h_distance <- sd(distHaversine(c(0,0), cbind(filtered_st$longitude, filtered_st$latitude)))# These three values are up to the students
h_date <- sd(dist_date("2000-01-01", filtered_st$date))
h_time <- sd(dist_time("00:00:00", filtered_st$time))


a <- 58.4274 # The point to predict (up to the students)
b <- 14.826

times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00",
           "18:00:00", "20:00:00", "22:00:00", "24:00:00")

temp <- vector(length = length(times))

#Students code here
gaussianKernel <- function(xDiff, h) {
  u <- xDiff/h
  return (exp(-(u)^2)) 
}
k1 <- gaussianKernel(distHaversine(c(b,a), cbind(filtered_st$longitude, filtered_st$latitude)), h_distance)

pred_date <- as.Date(sub('\\d{4}(?=-)', '2000', date, perl=TRUE))
k2 <- gaussianKernel(dist_date(pred_date, filtered_st$date), h_date)

for (i in 1:length(times)) {
  k3 <- gaussianKernel(dist_time(times[i], filtered_st$time), h_time)
  
  temp[i] <-  sum((k1 + k2 + k3)*filtered_st$air_temperature) / sum(k1+k2+k3)
}

plot(temp, type="o")

#Time to validate if h-values are reasonable
st_2013_11_04 = filtered_st[dist_date("2000-11-04", filtered_st[,'date']) == 0 ,]

minTimes <- numeric(length(times))
tmp <- strptime(times, "%H:%M:%S")
for (i in 1:length(times)) {
  minTimes[i] <- dist_time("00:00:00", tmp[i])
}

plot(minTimes, temp, xlab="minuites since midnight", type="o")

for (i in 1:length(times)) {
  k3 <- gaussianKernel(dist_time(times[i], filtered_st$time), h_time)
  
  temp[i] <-  sum((k1 * k2 * k3)*filtered_st$air_temperature) / sum(k1*k2*k3)
}
plot(minTimes, temp, xlab="minuites since midnight", type="o")
