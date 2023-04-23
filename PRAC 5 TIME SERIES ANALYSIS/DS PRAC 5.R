AirPassengers
#data(AirPassengers)
View(AirPassengers)
class(AirPassengers)
str(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)
#Exploring the data ^

#Visulization of data
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))
#Cycles gives the position in cycle of each of 
cycle(AirPassengers)
plot(aggregate(AirPassengers,FUN=mean))
boxplot(AirPassengers~cycle(AirPassengers))

#Preprocessing the data
acf(AirPassengers)#Spike crosses blue dotted line
acf(log(AirPassengers))#To make variance Stationary
acf(diff(log(AirPassengers)))
acf(diff(AirPassengers))
pacf(diff(log(AirPassengers)))
plot(diff(log(AirPassengers)))

(fit=arima(log(AirPassengers),c(0,1,1),seasonal=list(order=c(0,1,1),period=12)))
pred=predict(fit,n.ahead=10*12)
pred1=round(2.718^pred$pred,0)
pred1
ts.plot(AirPassengers,pred1,log="y",lty=c(1,3))