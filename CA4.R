getwd()
library(readr)
#------Loading data as csv file-----------------
Naturalgas <- read.csv("Naturalgas.csv")# Load Natural gas Dataset
View(Naturalgas)
WindData <- read.csv("WindData.csv") #Load Wind Dataset.
View(WindData)
remove_words<-c("a","b","c") # removed unwanted values
Naturalgas$Year<- gsub(paste0(remove_words, collapse = "|"),"",Naturalgas$Year)
Naturalgas$Year
Natural_gas_new<-data.frame(Naturalgas$Value,Naturalgas$Year)
Natural_gas_new
str(Natural_gas_new)


Natural_gas_new$Year<-as.numeric(as.character(Natural_gas_new$Year))
str(Natural_gas_new) 
                                 
                                 
WindData$Year<- gsub(paste0(remove_words, collapse = "|"),"",WindData$Year)
WindData$Year


wind_new_value<- data.frame(WindData$Value,WindData$Year)
wind_new_value
library(lattice)


View(wind_new_value)
library(cluster)
install.packages("forecast")
install.packages("tseries")
install.packages("ggplot2")
library(forecast)

library(tseries)
tsData_natural = ts(Natural_gas_new$Naturalgas.Value,start = c(1995,1), end = c(2007,2),
            frequency = 12)
tsData_natural
plot(tsData_natural)
start(tsData_natural)
end(tsData_natural)
#View(tsData)
frequency(tsData_natural)

ts_Data_wind <- ts(wind_new_value$WindData.Value, start = c(1995,1), 
                  end = c(2007,2), frequency = 12)
ts_Data_wind
#In this plot, there are seasonal effects and random fluctuations,
#but no overall trend away from a horizontal line.
plot(ts_Data_wind)
start(ts_Data_wind)
end(ts_Data_wind)

frequency(ts_Data_wind)


lagged_ts_wind <- lag(ts_Data_wind,3)# lag function for wind
lagged_ts_wind
plot(lagged_ts_wind)
lagged_ts_natural <- lag(tsData_natural,3)
lagged_ts_natural
plot(lagged_ts_natural)

acf_val<- Acf(ts_Data_wind)# finding auto correlation
acf_val
summary(acf_val)
#if autocorrelation crosses the dashed blue line,it means that
#specific lag is significantly correlated with current series

  pacf_result <- pacf(ts_Data_wind)
pacf_result
#----adf test for stationary-----
adf_test <- adf.test(ts_Data_wind)# test for stationary 
adf_test
# p value < .05 indicates that, stationary for wind co2 gas value

adf_test_natural<- adf.test(tsData_natural)
adf_test_natural # pvalue less than .05, shows stationary for co2 natural gas   

acf_val_nat<- Acf(tsData_natural)# autocorrelation 
acf_val_nat

pcf_val_nat <- pacf(tsData_natural)# particcal auto correlation
pcf_val_nat

#---ARIMA MODEL FOR Wind gas emmison value--------------#
arima_model_wind <- Arima(ts_Data_wind,order = c(0,1,4))
arima_model_wind

accuracy(arima_model_wind)
forecast(arima_model_wind,10)


auto_arema_model<- auto.arima(ts_Data_wind)
auto_arema_model
accuracy(auto_arema_model)
  


qqnorm(auto_arema_model$residuals)# fitting the model
qqline(auto_arema_model$residuals)

Box.test(auto_arema_model$residuals, type = "Ljung-Box")# pvalue > .05 its fit for the model
forecast(auto_arema_model,12)
plot(forecast(auto_arema_model,10), xlab = "year", ylab = "Co2 emmison wind")

arima_model_natural<- Arima(tsData_natural, order = c(0,1,4))
arima_model_natural

auto_arima_nat<- auto.arima(tsData_natural)
auto_arima_nat


qqnorm(auto_arima_nat$residuals)# fitting model
qqline(auto_arima_nat$residuals)


Box.test(auto_arima_nat$residuals, type ="Ljung-Box" )# p> .05 fits for the model


plot(forecast(auto_arima_nat,12),plot(forecast(arima_model_natural,10), xlab = "year"
                              , ylab = "Co2 emisson natural gas"))







