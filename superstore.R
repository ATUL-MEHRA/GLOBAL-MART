############################ "Global Mart"  #################################

# 1. Business Understanding

# we need to find out 2 most profitable (and consistent) segment 
# from these 21 and forecast the sales and demand for these segments.


# 2. Data Understanding
# Data having 51290 observations, 24 attributes.These attributes consists of 
# Details of order, sales, profit, Product& customer geographical location.
# these are having 7 market places & 3 segments.


# 3. Data Preparation

library(forecast)
library(tseries)
require(graphics)

Superstore <- read.csv("Global Superstore.csv", header = T)

str(Superstore)

# levels of Segment and market attribute.

levels(Superstore$Segment)

levels(Superstore$Market)


typeof(Superstore$Segment)
typeof(Superstore$Market)

#printing first few rows

head(Superstore)

#Exploring the data

summary(Superstore)

#checking missing value

sapply(Superstore, function(x) sum(is.na(x)))

# there are 41296 postal codes which are missing since we are mainly dealing 
# with market, these postal cades will not be in our usage. Thus we ignore 
# these NA. Niether any operations over it would be of interest.

Superstore$Segment <- as.character(factor(Superstore$Segment))
Superstore$Market <- as.character(factor(Superstore$Market))

# changing the order date format. Removed date so that we could aggregate on
#basis of month. 

Superstore$Order.Date <- as.Date(Superstore$Order.Date, format = "%d-%m-%Y")

Superstore <- Superstore[order(Superstore$Order.Date),]


Superstore$Order.Date <- format(Superstore$Order.Date,"%Y-%m")

# Aggregate the Sales, Quantity and profit on basis of Order date, segment, 
# market. 

Super_agg <- aggregate(Superstore[,c(19,20,22)],by = list(Superstore$Order.Date,Superstore$Segment, Superstore$Market), FUN = sum,na.rm = T)

names(Super_agg) <- c("Order Month", "Segment", "Market", "Sales", "Quantity", "Profit")

X <- c("Consumer","Corporate","Home Office")

Y <- c("Africa","APAC","Canada","EMEA","EU","LATAM","US")    

# created subsets with all combinations of Market & segments. Thus we have
# 21 subsets (7 market place X 3 segments ).

L3 <- apply(expand.grid(X, Y), 1, function(x) subset(Super_agg, Segment == x[1] & Market == x[2]))


library(forecast)


n <- 21
mat <- matrix(ncol=4, nrow=n)

for (i in 1:n) {
  var1 <-  sd(L3[[i]][[6]])/mean(L3[[i]][[6]])
  var2 <- L3[[i]][1,2]
  var3 <- L3[[i]][1,3]
  
  mat[i,] <- c(i,var1,var2,var3)
}

print(mat)


# Based on the maximum profits and profit month on month, we have chosen these Market Segements
# 
#    Market     segment         CV%  
# 1. EU         Consumer       0.624305176237368
# 2. APAC       Consumer       0.632132266189417
# 3. LATAM      Consumer       0.661482844072086
# 4. APAC       Corporate      0.698086894279087
# 5. EU         Corporate      0.763807196866622

############ 1. EU         Consumer      #####################################

##########################################Profit###############################################################################
Top_profit <- L3[[13]][[6]]
Time_val <- c(1:nrow(L3[[13]]))
EU_profit <- data.frame(Time_val,Top_profit)


Top_sales <- L3[[13]][[4]]
Time_val <- c(1:nrow(L3[[13]]))
EU_sales <- data.frame(Time_val,Top_sales)

Top_qty <- L3[[13]][[5]]
Time_val <- c(1:nrow(L3[[13]]))
EU_qty<- data.frame(Time_val,Top_qty)



total_timeser <- ts(EU_profit$Top_profit)
indata <- EU_profit[1:42,]
timeser <- ts(indata$Top_profit)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)


#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$Time_val
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'profit')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(profit ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- EU_profit[43:48,]
timevals_out <- outdata$Time_val

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

# Auto Arima
#-------------
autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#--------------------- Forecasting EU Consumer Profit------------------

EU_profit_forcast <- HoltWinters(total_timeser, beta=FALSE, gamma=FALSE)
EU_profit_forcast
plot(EU_profit_forcast)

EU_profit_forcast6months <- forecast:::forecast.HoltWinters(EU_profit_forcast,h=6)
EU_profit_forcast6months
plot(EU_profit_forcast6months)


######################################################Quantity######################################################

Top_qty <- L3[[13]][[5]]
Time_val <- c(1:nrow(L3[[13]]))
EU_qty <- data.frame(Time_val,Top_qty)



total_timeser1 <- ts(EU_qty$Top_qty)
indata1 <- EU_qty[1:42,]
timeser1 <- ts(indata1$Top_qty)
plot(timeser1)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries1 <- stats::filter(timeser1, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


#Smoothing left end of the time series

diff1 <- smoothedseries1[w+2] - smoothedseries1[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries1[i] <- smoothedseries1[i+1] - diff1
}

#Smoothing right end of the time series

n <- length(timeser1)
diff <- smoothedseries1[n-w] - smoothedseries1[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries1[i] <- smoothedseries1[i-1] + diff1
}

#Plot the smoothed time series

timevals_in1 <- indata1$Time_val
lines(smoothedseries1, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf1 <- as.data.frame(cbind(timevals_in1, as.vector(smoothedseries1)))
colnames(smootheddf1) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit1 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf1)
global_pred1 <- predict(lmfit1, Month=timevals_in1)
summary(global_pred1)
lines(timevals_in1, global_pred1, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred1 <- timeser1-global_pred1
plot(local_pred1, col='red', type = "l")
acf(local_pred1)
acf(local_pred1, type="partial")
armafit1 <- auto.arima(local_pred1)

tsdiag(armafit1)
armafit1

#We'll check if the residual series is white noise

resi1 <- local_pred1-fitted(armafit1)

adf.test(resi1,alternative = "stationary")
kpss.test(resi1)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata1 <- EU_qty[43:48,]
timevals_out1 <- outdata1$Time_val

global_pred_out1 <- predict(lmfit1,data.frame(Month =timevals_out1))

fcast1 <- global_pred_out1

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- accuracy(fcast1,outdata1[,2])[5]
MAPE_class_dec1


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred1 <- c(ts(global_pred1),ts(global_pred_out1))
plot(total_timeser1, col = "black")
lines(class_dec_pred1, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima1 <- auto.arima(timeser1)
autoarima1
tsdiag(autoarima1)
plot(autoarima1$x, col="black")
lines(fitted(autoarima1), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima1 <- timeser1 - fitted(autoarima1)

adf.test(resi_auto_arima1,alternative = "stationary")
kpss.test(resi_auto_arima1)


#Also, let's evaluate the model using MAPE
fcast_auto_arima1 <- predict(autoarima1, n.ahead = 6)

MAPE_auto_arima1 <- accuracy(fcast_auto_arima1$pred,outdata1[,2])[5]
MAPE_auto_arima1

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred1 <- c(fitted(autoarima1),ts(fcast_auto_arima1$pred))
plot(total_timeser1, col = "black")
lines(auto_arima_pred1, col = "red")

# Auto Arima
#-------------
autoarima1 <- auto.arima(timeser1)
autoarima1
tsdiag(autoarima1)
plot(autoarima1$x, col="black")
lines(fitted(autoarima1), col="red")

#--------------------- Forecasting EU Consumer Qty------------------

EU_Qty_forcast1 <- HoltWinters(total_timeser1, beta=FALSE, gamma=FALSE)
EU_Qty_forcast1
plot(EU_Qty_forcast1)

EU_Qty_forcast6months <- forecast:::forecast.HoltWinters(EU_Qty_forcast1,h=6)
EU_Qty_forcast6months
plot(EU_Qty_forcast6months)


######################################################Sales######################################################
Top_sales <- L3[[13]][[4]]
Time_val <- c(1:nrow(L3[[13]]))
EU_sales <- data.frame(Time_val,Top_sales)


total_timeser2 <- ts(EU_sales$Top_sales)
indata2 <- EU_sales[1:42,]
timeser2 <- ts(indata2$Top_sales)
plot(timeser2)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries2 <- stats::filter(timeser2, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)


#Smoothing left end of the time series

diff2 <- smoothedseries2[w+2] - smoothedseries2[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries2[i] <- smoothedseries2[i+1] - diff2
}

#Smoothing right end of the time series

n <- length(timeser2)
diff2 <- smoothedseries2[n-w] - smoothedseries2[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries2[i] <- smoothedseries2[i-1] + diff2
}

#Plot the smoothed time series

timevals_in2 <- indata1$Time_val
lines(smoothedseries2, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf2 <- as.data.frame(cbind(timevals_in2, as.vector(smoothedseries2)))
colnames(smootheddf2) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit2 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
             + Month, data=smootheddf2)
global_pred2 <- predict(lmfit2, Month=timevals_in2)
summary(global_pred2)
lines(timevals_in2, global_pred2, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred2 <- timeser2-global_pred2
plot(local_pred2, col='red', type = "l")
acf(local_pred2)
acf(local_pred2, type="partial")
armafit2 <- auto.arima(local_pred2)

tsdiag(armafit2)
armafit2

#We'll check if the residual series is white noise

resi2 <- local_pred2-fitted(armafit2)

adf.test(resi2,alternative = "stationary")
kpss.test(resi2)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata2 <- EU_sales[43:48,]
timevals_out2 <- outdata2$Time_val

global_pred_out2 <- predict(lmfit2,data.frame(Month =timevals_out2))

fcast2 <- global_pred_out2

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec2 <- accuracy(fcast2,outdata2[,2])[5]
MAPE_class_dec2


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred2 <- c(ts(global_pred2),ts(global_pred_out2))
plot(total_timeser2, col = "black")
lines(class_dec_pred2, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima2 <- auto.arima(timeser2)
autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima1=2), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima2 <- timeser2 - fitted(autoarima2)

adf.test(resi_auto_arima2,alternative = "stationary")
kpss.test(resi_auto_arima2)


#Also, let's evaluate the model using MAPE
fcast_auto_arima2 <- predict(autoarima2, n.ahead = 6)

MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$pred,outdata2[,2])[5]
MAPE_auto_arima2

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$pred))
plot(total_timeser2, col = "black")
lines(auto_arima_pred2, col = "red")

# Auto Arima
#-------------
autoarima2 <- auto.arima(timeser2)
autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")

#--------------------- Forecasting EU Consumer sales------------------

EU_Sales_forcast2 <- HoltWinters(total_timeser2, beta=FALSE, gamma=FALSE)
EU_Sales_forcast2
plot(EU_Sales_forcast2)



EU_Sales_forcast6months <- forecast:::forecast.HoltWinters(EU_Sales_forcast2,h=6)
EU_Sales_forcast6months
plot(EU_Sales_forcast6months)

###################################### APAC ##########Consumer ##################################################      

##########################################Profit###############################################################################
First_profit <- L3[[4]][[6]]
Time_val <- c(1:nrow(L3[[4]]))
APAC_profit <- data.frame(Time_val,First_profit)


First_sales <- L3[[4]][[4]]
Time_val <- c(1:nrow(L3[[4]]))
APAC_sales <- data.frame(Time_val,First_sales)

First_qty <- L3[[4]][[5]]
Time_val <- c(1:nrow(L3[[4]]))
APAC_qty<- data.frame(Time_val,First_qty)



total_timeser3 <- ts(APAC_profit$First_profit)
indata3 <- APAC_profit[1:42,]
timeser3 <- ts(indata3$First_profit)
plot(timeser3)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries3 <- stats::filter(timeser3, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)


#Smoothing left end of the time series

diff3 <- smoothedseries3[w+2] - smoothedseries3[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries3[i] <- smoothedseries3[i+1] - diff3
}

#Smoothing right end of the time series

n <- length(timeser3)
diff3 <- smoothedseries3[n-w] - smoothedseries3[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries3[i] <- smoothedseries3[i-1] + diff3
}

#Plot the smoothed time series

timevals_in3 <- indata3$Time_val
lines(smoothedseries3, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf3 <- as.data.frame(cbind(timevals_in3, as.vector(smoothedseries3)))
colnames(smootheddf3) <- c('Month', 'profit3')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit3 <- lm(profit3 ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf3)
global_pred3 <- predict(lmfit3, Month=timevals_in3)
summary(global_pred3)
lines(timevals_in3, global_pred3, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred3 <- timeser3-global_pred3
plot(local_pred3, col='red', type = "l")
acf(local_pred3)
acf(local_pred3, type="partial")
armafit3 <- auto.arima(local_pred3)

tsdiag(armafit3)
armafit3

#We'll check if the residual series is white noise

resi3 <- local_pred3-fitted(armafit3)

adf.test(resi3,alternative = "stationary")
kpss.test(resi3)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata3 <- APAC_profit[43:48,]
timevals_out3 <- outdata3$Time_val

global_pred_out3 <- predict(lmfit3,data.frame(Month =timevals_out3))

fcast3 <- global_pred_out3

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec3 <- accuracy(fcast3,outdata3[,2])[5]
MAPE_class_dec3


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred3 <- c(ts(global_pred3),ts(global_pred_out3))
plot(total_timeser3, col = "black")
lines(class_dec_pred3, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima3 <- auto.arima(timeser3)
autoarima3
tsdiag(autoarima3)
plot(autoarima3$x, col="black")
lines(fitted(autoarima3), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima3 <- timeser3 - fitted(autoarima3)

adf.test(resi_auto_arima3,alternative = "stationary")
kpss.test(resi_auto_arima3)


#Also, let's evaluate the model using MAPE
fcast_auto_arima3 <- predict(autoarima3, n.ahead = 6)

MAPE_auto_arima3 <- accuracy(fcast_auto_arima3$pred,outdata3[,2])[5]
MAPE_auto_arima3

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred3 <- c(fitted(autoarima3),ts(fcast_auto_arima3$pred))
plot(total_timeser3, col = "black")
lines(auto_arima_pred3, col = "red")

# Auto Arima
#-------------
autoarima3 <- auto.arima(timeser3)
autoarima3
tsdiag(autoarima3)
plot(autoarima3$x, col="black")
lines(fitted(autoarima3), col="red")

#--------------------- Forecasting APAC Consumer Profit------------------

APAC_profit_forcast3 <- HoltWinters(total_timeser3, beta=FALSE, gamma=FALSE)
APAC_profit_forcast3
plot(APAC_profit_forcast3)


APAC_profit_forcast6months <- forecast:::forecast.HoltWinters(APAC_profit_forcast3,h=6)
APAC_profit_forcast6months
plot(APAC_profit_forcast6months)
######################################################Quantity######################################################

First_qty <- L3[[4]][[5]]
Time_val <- c(1:nrow(L3[[4]]))
APAC_qty <- data.frame(Time_val,First_qty)



total_timeser4 <- ts(APAC_qty$First_qty)
indata4 <- APAC_qty[1:42,]
timeser4 <- ts(indata4$First_qty)
plot(timeser4)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries4 <- stats::filter(timeser4, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)


#Smoothing left end of the time series

diff4 <- smoothedseries4[w+2] - smoothedseries4[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries4[i] <- smoothedseries4[i+1] - diff4
}

#Smoothing right end of the time series

n <- length(timeser4)
diff4 <- smoothedseries4[n-w] - smoothedseries4[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries4[i] <- smoothedseries4[i-1] + diff4
}

#Plot the smoothed time series

timevals_in4 <- indata4$Time_val
lines(smoothedseries4, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf4 <- as.data.frame(cbind(timevals_in4, as.vector(smoothedseries4)))
colnames(smootheddf4) <- c('Month', 'Quantity4')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit4 <- lm(Quantity4 ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
             + Month, data=smootheddf4)
global_pred4 <- predict(lmfit4, Month=timevals_in4)
summary(global_pred4)
lines(timevals_in4, global_pred4, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred4 <- timeser4-global_pred1
plot(local_pred4, col='red', type = "l")
acf(local_pred4)
acf(local_pred4, type="partial")
armafit4 <- auto.arima(local_pred4)

tsdiag(armafit4)
armafit4

#We'll check if the residual series is white noise

resi4 <- local_pred1-fitted(armafit4)

adf.test(resi4,alternative = "stationary")
kpss.test(resi4)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata4 <- APAC_qty[43:48,]
timevals_out4 <- outdata4$Time_val

global_pred_out4 <- predict(lmfit4,data.frame(Month =timevals_out4))

fcast4 <- global_pred_out4

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec4 <- accuracy(fcast4,outdata4[,2])[5]
MAPE_class_dec4


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred4 <- c(ts(global_pred4),ts(global_pred_out4))
plot(total_timeser4, col = "black")
lines(class_dec_pred4, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima4 <- auto.arima(timeser4)
autoarima4
tsdiag(autoarima4)
plot(autoarima4$x, col="black")
lines(fitted(autoarima4), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima4 <- timeser4 - fitted(autoarima4)

adf.test(resi_auto_arima4,alternative = "stationary")
kpss.test(resi_auto_arima4)


#Also, let's evaluate the model using MAPE
fcast_auto_arima4 <- predict(autoarima4, n.ahead = 6)

MAPE_auto_arima4 <- accuracy(fcast_auto_arima4$pred,outdata4[,2])[5]
MAPE_auto_arima4

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred4 <- c(fitted(autoarima4),ts(fcast_auto_arima4$pred))
plot(total_timeser4, col = "black")
lines(auto_arima_pred4, col = "red")

# Auto Arima
#-------------
autoarima4 <- auto.arima(timeser4)
autoarima4
tsdiag(autoarima4)
plot(autoarima4$x, col="black")
lines(fitted(autoarima4), col="red")

#--------------------- Forecasting APAC Consumer Qty------------------

APAC_Qty_forcast4 <- HoltWinters(total_timeser4, beta=FALSE, gamma=FALSE)
APAC_Qty_forcast4
plot(APAC_Qty_forcast4)

APAC_Qty_forcast6months <- forecast:::forecast.HoltWinters(APAC_Qty_forcast4,h=6)
APAC_Qty_forcast6months
plot(APAC_Qty_forcast6months)

######################################################Sales######################################################
First_sales <- L3[[4]][[4]]
Time_val <- c(1:nrow(L3[[4]]))
APAC_sales <- data.frame(Time_val,First_sales)


total_timeser5 <- ts(APAC_sales$First_sales)
indata5 <- APAC_sales[1:42,]
timeser5 <- ts(indata5$First_sales)
plot(timeser5)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries5 <- stats::filter(timeser5, 
                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                 method='convolution', sides=2)


#Smoothing left end of the time series

diff5 <- smoothedseries5[w+2] - smoothedseries5[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries5[i] <- smoothedseries5[i+1] - diff5
}

#Smoothing right end of the time series

n <- length(timeser5)
diff5 <- smoothedseries5[n-w] - smoothedseries5[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries5[i] <- smoothedseries5[i-1] + diff5
}

#Plot the smoothed time series

timevals_in5 <- indata5$Time_val
lines(smoothedseries5, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf5 <- as.data.frame(cbind(timevals_in5, as.vector(smoothedseries5)))
colnames(smootheddf5) <- c('Month', 'Sales5')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit5 <- lm(Sales5 ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
             + Month, data=smootheddf5)
global_pred5 <- predict(lmfit5, Month=timevals_in5)
summary(global_pred5)
lines(timevals_in5, global_pred5, col='red', lwd=2)


#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred5 <- timeser5-global_pred5
plot(local_pred5, col='red', type = "l")
acf(local_pred5)
acf(local_pred5, type="partial")
armafit5 <- auto.arima(local_pred5)

tsdiag(armafit5)
armafit5

#We'll check if the residual series is white noise

resi5 <- local_pred5-fitted(armafit5)

adf.test(resi5,alternative = "stationary")
kpss.test(resi5)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata5 <- APAC_sales[43:48,]
timevals_out5 <- outdata5$Time_val

global_pred_out5 <- predict(lmfit5,data.frame(Month =timevals_out5))

fcast5 <- global_pred_out5

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec5 <- accuracy(fcast5,outdata5[,2])[5]
MAPE_class_dec5


#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred5 <- c(ts(global_pred5),ts(global_pred_out5))
plot(total_timeser5, col = "black")
lines(class_dec_pred5, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima5 <- auto.arima(timeser5)
autoarima5
tsdiag(autoarima5)
plot(autoarima5$x, col="black")
lines(fitted(autoarima5), col="red")


#Again, let's check if the residual series is white noise

resi_auto_arima5 <- timeser5 - fitted(autoarima5)

adf.test(resi_auto_arima5,alternative = "stationary")
kpss.test(resi_auto_arima5)


#Also, let's evaluate the model using MAPE
fcast_auto_arima5 <- predict(autoarima5, n.ahead = 6)

MAPE_auto_arima5 <- accuracy(fcast_auto_arima5$pred,outdata5[,2])[5]
MAPE_auto_arima5

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred5 <- c(fitted(autoarima5),ts(fcast_auto_arima5$pred))
plot(total_timeser5, col = "black")
lines(auto_arima_pred5, col = "red")

# Auto Arima
#-------------
autoarima5 <- auto.arima(timeser5)
autoarima5
tsdiag(autoarima5)
plot(autoarima5$x, col="black")
lines(fitted(autoarima5), col="red")

#--------------------- Forecasting APAC Consumer sales------------------

APAC_Sales_forcast5 <- HoltWinters(total_timeser5, beta=FALSE, gamma=FALSE)
APAC_Sales_forcast5
plot(APAC_Sales_forcast5)

APAC_Sales_forcast6months <- forecast:::forecast.HoltWinters(APAC_Sales_forcast5,h=6)
APAC_Sales_forcast6months
plot(APAC_Sales_forcast6months)
