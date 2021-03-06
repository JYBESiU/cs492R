install.packages("ggplot2")
install.packages("corrplot")
install.packages("readxl")

library(ggplot2)
library(corrplot)
library(readxl)

setwd("C:/R/")

covid_month = read.csv("COVID19_month.csv")
covid_month = covid_month[0:8, ]
covid_month$date = as.Date(covid_month$date)
covid_month$all = as.integer(covid_month$all)
covid_month$seoul = as.integer(covid_month$seoul)
plot(covid_month$date, covid_month$all, xlab = "date", main = "Monthly COVID19 All confirmed cases")
plot(covid_month$date, covid_month$seoul, xlab = "date", main = "Monthly COVID19 Seoul confirmed cases")


jeju = read_excel("jeju_tourist.xlsx")
jeju2017 = jeju$"2017"
jeju2018 = jeju$"2018"
jeju2019 = jeju$"2019"
jeju2020 = jeju$"2020"
jeju_all = c(jeju2017, jeju2018, jeju2019, jeju2020)

par(mfrow = c(1,1))
plot(jeju_all)

par(mfrow = c(2,2))
plot(jeju2017)
plot(jeju2018)
plot(jeju2019)
plot(jeju2020)

cor.test(covid_month$all, jeju2020[2:9])
