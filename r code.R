#This code is construct meanly under the paper under PCB Phillips and Shuping Shi

#All the install.packages can be eliminated if you have already installed those packages
#install.packages("psymonitor")
#The library function provide the access to the library that we use
library(psymonitor)

#set the working directory
setwd("C:/Users/YW/Google Drive/Mcmaster/fin 605")
#data source: yahoo finance https://finance.yahoo.com/quote/BTC-USD/history/
dat <- read.csv("BTC-USD.csv")

#see what variables we have
summary(dat)

#install.packages("ggplot2")
library(ggplot2)
#install.packages("scales")
library(scales)
typeof(dat$Date)
dat$Date <- as.Date(dat$Date)                                                                
ggplot(data = dat, aes(x = Date, y = Adj.Close)) + geom_line() + scale_x_date(labels = date_format("%Y"))

price <- log(dat$Adj.Close)
obs <- length(price)
r0 <- -0.01+1.8/sqrt(obs)
swindow0 <- floor(r0*obs)
dim <- obs-swindow0+1
IC <- 1
adflag <- 6
yr <- 5
Tb <- 12*yr + swindow0 + 1
nboot <- 999

bsadf <- PSY(y, swindow0, IC, adflag)
quantilesBsadf <- cvPSYwmboot(y, swindow0, IC, adflag, Tb, nboot,2)


date <- dat$Date[swindow0:obs]

quantile95 <- quantilesBsadf%*%matrix(1, nrow = 1, ncol = dim)
#I add this part in order to match the size of the request index
length(bsadf)
length(quantile95)
numbers_of_zeros <- ceiling(length(quantile95)/length(bsadf)) * length(bsadf) - length(quantile95)
numbers_of_zeros
zeros <- rep(0,numbers_of_zeros)
quantile95 <- merge(quantile95,zeros)
ind95 <- (bsadf > t(quantile95[2,]))*1

OT <- locate(ind95, date)
BCdates <- disp(OT,obs)
print(BCdates)