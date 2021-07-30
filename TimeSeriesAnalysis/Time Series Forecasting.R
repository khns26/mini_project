df <- read.csv("Harga Saham UNVR.csv", header=TRUE)
head(df) 
df_ts <- ts(df$Price,start=c(2016,1), end=c(2020,12), frequency=12) 
df_ts
summary(df_ts)
is.ts(df_ts) 

#dickey fuller test on ts data
#options(warn=1)
library(tseries)
adf.test(df_ts)
plot.ts(df_ts,xlab="Year",ylab="Differenced Stocks Price", main="UNVR Stocks Price 2016-2020") 
#Menghitung ACF manual
S0=0
for (i in 1:60){
  S0=S0+((df_ts[i]-8872)^2)
}
S0=1/60 * S0

S1=0
for (i in 2:60){
  S1=S1+((df_ts[i]-8872)*(df_ts[i-1]-8872))
}
S1=1/60 * S1
r1=S1/S0
print(r1)
acf(df_ts,plot=FALSE,lag.max=60) #acf

pacf(df_ts)
df_diff <- diff(df_ts, differencing=1) #zt - z(t-1)
df_diff
summary(df_diff)

#dickey fuller test on differenced data
options(warn=1)
library(tseries)
adf.test(df_diff)

acf(df_diff,plot=FALSE,lag.max=12) 
pacf(df_diff)
ts.plot(df_diff,xlab="Year",ylab="Differenced Stocks Price", main="UNVR Stocks Price Changes 2016-2020") 

#proses membuat model diff data dengan mean
MA_df_diff <- arima(df_diff, order=c(0,0,1),include.mean=FALSE, method='ML')
print(MA_df_diff)
checkresiduals(MA_df_diff)
#menghitung theoritical acf MA(1)
acf_theoritical<-ARMAacf(ma=-0.0327,lag.max=12)
acf_theoritical
lags<- 0:12 # Creates a variable named lags that ranges from 0 to 12
plot(lags,acf_theoritical,xlim=c(1,12),ylab = "r",type="h",main = "ACF for MA(1) with theta = -0.0327")
abline(h=0)

#menghitung residual
residual<-resid(MA_df_diff)
residual
acf(ts(residual),plot=FALSE,lag.max=60)
pred<-df_ts-residual 
pred
obs_vs_pred <- cbind(df_ts,pred,residual)
obs_vs_pred
ts.plot(df_ts,xlab="Year",ylab="Stocks Price", main="UNVR Stocks Price 2016-2020")
points(pred, type = "o", col = 2, lty = 2)

#forecasting time~
library("forecast")
ramal<-forecast(MA_df_diff, h=1)
ramal
plot(ramal)
df_ramal = as.data.frame(ramal) 
df_ramal$`Point Forecast`
jan21 <- df_ramal$`Point Forecast`+ df_ts[60]
jan21

