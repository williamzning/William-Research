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
ggseasonplot(window(y,end=c(2020,12)),col=c('dodgerblue',"darkturquoise","cyan3","cadetblue3","cyan2", "cyan1",'firebrick1')
             )+
  ylab("AQI") +
  ggtitle("Seasonal plot: New York City PM2.5")

# Figure 3
ggseasonplot(window(y,end=c(2020,12)), polar=TRUE,col=c('dodgerblue',"darkturquoise","cyan3","cadetblue3","cyan2", "cyan1",'firebrick1')) +
  ylab("AQI (polar coordinates)") +
  ggtitle("Seasonal plot: New York City PM2.5")

# table 1
library(zoo)
y1 <- window(y, end = c(2019, 12))
y2 <- window(y, start = c(2020, 1))
fit21= y1 %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) 

y12=fit21 %>% forecast(h=13)
table0=summary(y12)

table1 = data.frame(Date=rownames(table0),
                    Actual=y2,
                    Forecast=y12$mean,
                    lower80=table0$`Lo 80`,
                    upper80=table0$`Hi 80`,
                    lower95=table0$`Lo 95`,
                    upper95=table0$`Hi 95`
)
table1

# Figure 4
checkresiduals(fit21)  


