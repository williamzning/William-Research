# COVID-19 and Air Pollution in New York City: Code and Data
library(xts)
library(forecast)
library(ggplot2)
library(dsa)
library(ggplot2)

# Load Data (Source:World Air Quality Index (WAQI) project)
x.xts=readRDS('monthly_pm25.RDS')


y=xts2ts(x.xts)

# Figure 1
autoplot(y) + xlab("Date") + ylab("AQI") +
  ggtitle("New York City PM2.5 (Jan 2014-Jan 2021)") 

# Figure 2
ggseasonplot(window(y,end=c(2020,12)),col=c('dodgerblue',"darkturquoise","cyan3","cadetblue3","cyan2", "cyan1",'firebrick1'))+
  ylab("AQI") +
  ggtitle("Seasonal plot: New York City PM2.5")


# Figure 3
y1 <- window(y, end = c(2019, 12))
y2 <- window(y, start = c(2020, 1))
fitlinear <- tslm(y1 ~ trend)
summary(fitlinear)

y12=forecast(fitlinear, h=13)
clrs <- c("black","blue", "blue", "red")

upper <- fitlinear$fitted.values + 1.96*sd(fitlinear$residuals)
lower <- fitlinear$fitted.values - 1.96*sd(fitlinear$residuals)

autoplot(y12)+
  ggtitle("New York City PM2.5 (Linear Model Fit & Forecast)") +
  xlab("Date") + ylab("AQI") +
  autolayer(y12$mean,series='Forecast',linetype = "dashed",size=3)+
  autolayer(y,series='Actual',size=1) +
  autolayer(fitlinear$fitted.values,series='Fitted',linetype = "dashed",size=3)+
  autolayer(upper,series='Fitted+/-95% Interval',size=0.5,linetype = "dashed") +
  autolayer(lower,series='Fitted+/-95% Interval',size=0.5,linetype = "dashed") +
  scale_color_manual(values=clrs)
  
# Figure 4
ggseasonplot(window(y,end=c(2020,12)), polar=TRUE) +
  ylab("AQI (polar coordinates)") +
  ggtitle("Seasonal plot: New York City PM2.5")


# Figure 5
y1 <- window(y, end = c(2019, 12))
y2 <- window(y, start = c(2020, 1))
fit21= y1 %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) 
summary(fit21)

y12=fit21 %>% forecast(h=13)
summary(y12)

clrs <- c("black", "blue", "red")
upper <- fit21$fitted + 1.96*fit21$sigma2^0.5
lower <- fit21$fitted - 1.96*fit21$sigma2^0.5
autoplot(fit21$fitted)+
  ggtitle("New York City PM2.5 (ARIMA Model Fit Jan 2014-Dec 2019)") +
  xlab("Date") + ylab("AQI") +
  autolayer(y1,series='Actual',size=1) +
  autolayer(fit21$fitted,series='Fitted',size=2,linetype = "dotted")+
  autolayer(upper,series='Fitted+/-95% Interval',size=0.5,linetype = "dashed") +
  autolayer(lower,series='Fitted+/-95% Interval',size=0.5,linetype = "dashed") +
  scale_color_manual(values=clrs)

# table 1
library(zoo)
upper95 <- fit21$fitted + 1.96*fit21$sigma2^0.5
lower95 <- fit21$fitted - 1.96*fit21$sigma2^0.5
upper80 <- fit21$fitted + 1.28*fit21$sigma2^0.5
lower80 <- fit21$fitted - 1.28*fit21$sigma2^0.5
table1 = data.frame(Date=as.yearmon(time(y12$fitted)),
                     Actual=y12$x,
                     Fitted=y12$fitted,
                     lower80,
                     upper80,
                     lower95,
                     upper95
                     )

table1
# table 2
library(zoo)
y1 <- window(y, end = c(2019, 12))
y2 <- window(y, start = c(2020, 1))
fit21= y1 %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) 

y12=fit21 %>% forecast(h=13)
table0=summary(y12)

table2 = data.frame(Date=rownames(table0),
                    Actual=y2,
                    Forecast=y12$mean,
                    lower80=table0$`Lo 80`,
                    upper80=table0$`Hi 80`,
                    lower95=table0$`Lo 95`,
                    upper95=table0$`Hi 95`
)
table2
# Figure 6

checkresiduals(fit21)  


# Figure 7
clrs <- c("black", "#596DD5","#D5DBFF", "red")
autoplot(y12)+
  ggtitle("New York City PM2.5 (ARIMA Model Forecast Jan 2020-Jan 2021)") +
  xlab("Date") + ylab("AQI") +
  autolayer(y12$mean,series='Forecast',linetype = "dashed",size=3)+
  autolayer(y,series='Actual',size=1) +
  autolayer(y12$lower[,2],series='Fitted+/-95% Interval',linetype = "dashed",size=1)+
  autolayer(y12$upper[,2],series='Fitted+/-95% Interval',linetype = "dashed",size=1)+
  autolayer(y12$lower[,1],series='Fitted+/-80% Interval',linetype = "dashed",size=1)+
  autolayer(y12$upper[,1],series='Fitted+/-80% Interval',linetype = "dashed",size=1)+
  scale_color_manual(values=clrs) 


# Figure 8
fit2= y %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) 
foreast2021=fit2 %>% forecast(h=12) 
clrs <- c("black", "blue", "#596DD5","#D5DBFF", "red")
foreast2021%>% autoplot() +
  ggtitle("New York City PM2.5 (ARIMA Model Forecast Feb 2021-Jan 2022)") +
  xlab("Date") + ylab("AQI") +
  autolayer(foreast2021$mean,series='Forecast',linetype = "dashed",size=2)+
  autolayer(fit2$fitted,series='Fitted',linetype = "dashed",size=3)+
  autolayer(y,series='Actual',size=1) +
  autolayer(foreast2021$lower[,2],series='Fitted+/-95% Interval',linetype = "dashed",size=1)+
  autolayer(foreast2021$upper[,2],series='Fitted+/-95% Interval',linetype = "dashed",size=1)+
  autolayer(foreast2021$lower[,1],series='Fitted+/-80% Interval',linetype = "dashed",size=1)+
  autolayer(foreast2021$upper[,1],series='Fitted+/-80% Interval',linetype = "dashed",size=1)+
  scale_color_manual(values=clrs) 

