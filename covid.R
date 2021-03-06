install.packages("ggplot2")
install.packages("corrplot")

library(ggplot2)
library(corrplot)

setwd("C:/R/")

### 코로나 데이터 생성 ###
covid = read.csv("COVID19.csv")
covid = covid[4:293, ]
names(covid) = c("date", "all_new", "seoul_new", "all_acc", "seoul_acc")
covid$date = as.Date(covid$date)
covid$all_new = as.integer(covid$all_new)
covid$seoul_new = as.integer(covid$seoul_new)
covid$all_acc = as.integer(covid$all_acc)
covid$seoul_acc = as.integer(covid$seoul_acc)

par(mfrow = c(2,2))
plot(covid$date, covid$all_new, xlab = "date", main = "Daily COVID19 All confirmed cases")
plot(covid$date, covid$seoul_new, xlab = "date", main = "Daily COVID19 Seoul confirmed cases")
plot(covid$date, covid$all_acc)
plot(covid$date, covid$seoul_acc)


### 코로나 월별 데이터 생성 ###
covid_month = read.csv("COVID19_month.csv")
covid_month = covid_month[0:8, ]
covid_month$date = as.Date(covid_month$date)
covid_month$all = as.integer(covid_month$all)
covid_month$seoul = as.integer(covid_month$seoul)
plot(covid_month$date, covid_month$all, xlab = "date", main = "Monthly COVID19 All confirmed cases")
plot(covid_month$date, covid_month$seoul, xlab = "date", main = "Monthly COVID19 Seoul confirmed cases")


### 온라인 쇼핑 데이터 생성 ###
shopping = read.csv("online_shopping.csv")
shopping$date = as.Date(shopping$date)
shopping_before = shopping[1:37, ]
shopping_after = shopping[38:45, ]

plot(shopping$date, shopping$total_on, xlab = "date", main = "Total amount of Online")
plot(shopping$date, shopping$total_off, xlab = "date", main = "Total amount of Offline")


### 온라인 쇼핑 각 항목별로 plot 그리기, linear model 비교 ###
par(mfrow = c(1,2))

computer_on_lm_before = lm(computer_on ~ date, data = shopping_before)
computer_on_lm_after = lm(computer_on ~ date, data = shopping_after)
computer_on_lm_before
computer_on_lm_after
computer_on_div = computer_on_lm_after$coefficients[2] / computer_on_lm_before$coefficients[2]
plot(shopping$date, shopping$computer_on, xlab = "date")
abline(coef(computer_on_lm_before), lwd = 2)
abline(coef(computer_on_lm_after), lwd = 2)

computer_off_lm_before = lm(computer_off ~ date, data = shopping_before)
computer_off_lm_after = lm(computer_off ~ date, data = shopping_after)
computer_off_lm_before
computer_off_lm_after
computer_off_div = computer_off_lm_after$coefficients[2] / computer_off_lm_before$coefficients[2]
plot(shopping$date, shopping$computer_off, xlab = "date")
abline(coef(computer_off_lm_before), lwd = 2)
abline(coef(computer_off_lm_after), lwd = 2)


book_on_lm_before = lm(book_on ~ date, data = shopping_before)
book_on_lm_after = lm(book_on ~ date, data = shopping_after)
book_on_lm_before
book_on_lm_after
book_on_div = book_on_lm_after$coefficients[2] / book_on_lm_before$coefficients[2]
plot(shopping$date, shopping$book_on, xlab = "date")
abline(coef(book_on_lm_before), lwd = 2)
abline(coef(book_on_lm_after), lwd = 2)

book_off_lm_before = lm(book_off ~ date, data = shopping_before)
book_off_lm_after = lm(book_off ~ date, data = shopping_after)
book_off_lm_before
book_off_lm_after
book_off_div = book_off_lm_after$coefficients[2] / book_off_lm_before$coefficients[2]
plot(shopping$date, shopping$book_off, xlab = "date")
abline(coef(book_off_lm_before), lwd = 2)
abline(coef(book_off_lm_after), lwd = 2)


accessory_on_lm_before = lm(accessory_on ~ date, data = shopping_before)
accessory_on_lm_after = lm(accessory_on ~ date, data = shopping_after)
accessory_on_lm_before
accessory_on_lm_after
accessory_on_div = accessory_on_lm_after$coefficients[2] / accessory_on_lm_before$coefficients[2]
plot(shopping$date, shopping$accessory_on, xlab = "date")
abline(coef(accessory_on_lm_before), lwd = 2)
abline(coef(accessory_on_lm_after), lwd = 2)

accessory_off_temp = shopping_before$accessory_off
accessory_off_max = accessory_off_temp[36] - accessory_off_temp[1]
accessory_off_down = (accessory_off_temp[37] - shopping_after$accessory_off[1]) / accessory_off_max


cosmetic_on_lm_before = lm(cosmetic_on ~ date, data = shopping_before)
cosmetic_on_lm_after = lm(cosmetic_on ~ date, data = shopping_after)
cosmetic_on_lm_before
cosmetic_on_lm_after
cosmetic_on_div = cosmetic_on_lm_after$coefficients[2] / cosmetic_on_lm_before$coefficients[2]
plot(shopping$date, shopping$cosmetic_on, xlab = "date")
abline(coef(cosmetic_on_lm_before), lwd = 2)
abline(coef(cosmetic_on_lm_after), lwd = 2)

cosmetic_off_temp = shopping_before$cosmetic_off
cosmetic_off_temp[-1]
cosmetic_off_max = cosmetic_off_temp[35] - cosmetic_off_temp[4]
cosmetic_off_down = (cosmetic_off_temp[37] - shopping_after$cosmetic_off[1]) / cosmetic_off_max


food_on_lm_before = lm(food_on ~ date, data = shopping_before)
food_on_lm_after = lm(food_on ~ date, data = shopping_after)
food_on_lm_before
food_on_lm_after
food_on_div = food_on_lm_after$coefficients[2] / food_on_lm_before$coefficients[2]
plot(shopping$date, shopping$food_on, xlab = "date")
abline(coef(food_on_lm_before), lwd = 2)
abline(coef(food_on_lm_after), lwd = 2)

food_off_lm_before = lm(food_off ~ date, data = shopping_before)
food_off_lm_after = lm(food_off ~ date, data = shopping_after)
food_off_lm_before
food_off_lm_after
food_off_div = food_off_lm_after$coefficients[2] / food_off_lm_before$coefficients[2]
plot(shopping$date, shopping$food_off, xlab = "date")
abline(coef(food_off_lm_before), lwd = 2)
abline(coef(food_off_lm_after), lwd = 2)


agri_marine_product_on_lm_before = lm(agri_marine_product_on ~ date, data = shopping_before)
agri_marine_product_on_lm_after = lm(agri_marine_product_on ~ date, data = shopping_after)
agri_marine_product_on_lm_before
agri_marine_product_on_lm_after
agri_marine_product_on_div = agri_marine_product_on_lm_after$coefficients[2] / agri_marine_product_on_lm_before$coefficients[2]
plot(shopping$date, shopping$agri_marine_product_on, xlab = "date")
abline(coef(agri_marine_product_on_lm_before), lwd = 2)
abline(coef(agri_marine_product_on_lm_after), lwd = 2)

agri_marine_product_off_lm_before = lm(agri_marine_product_off ~ date, data = shopping_before)
agri_marine_product_off_lm_after = lm(agri_marine_product_off ~ date, data = shopping_after)
agri_marine_product_off_lm_before
agri_marine_product_off_lm_after
agri_marine_product_off_div = agri_marine_product_off_lm_after$coefficients[2] / agri_marine_product_off_lm_before$coefficients[2]
plot(shopping$date, shopping$agri_marine_product_off, xlab = "date")
abline(coef(agri_marine_product_off_lm_before), lwd = 2)
abline(coef(agri_marine_product_off_lm_after), lwd = 2)


daily_supplies_on_lm_before = lm(daily_supplies_on ~ date, data = shopping_before)
daily_supplies_on_lm_after = lm(daily_supplies_on ~ date, data = shopping_after)
daily_supplies_on_lm_before
daily_supplies_on_lm_after
daily_supplies_on_div = daily_supplies_on_lm_after$coefficients[2] / daily_supplies_on_lm_before$coefficients[2]
plot(shopping$date, shopping$daily_supplies_on, xlab = "date")
abline(coef(daily_supplies_on_lm_before), lwd = 2)
abline(coef(daily_supplies_on_lm_after), lwd = 2)

daily_supplies_off_lm_before = lm(daily_supplies_off ~ date, data = shopping_before)
daily_supplies_off_lm_after = lm(daily_supplies_off ~ date, data = shopping_after)
daily_supplies_off_lm_before
daily_supplies_off_lm_after
daily_supplies_off_div = daily_supplies_off_lm_after$coefficients[2] / daily_supplies_off_lm_before$coefficients[2]
plot(shopping$date, shopping$daily_supplies_off, xlab = "date")
abline(coef(daily_supplies_off_lm_before), lwd = 2)
abline(coef(daily_supplies_off_lm_after), lwd = 2)


pets_on_lm_before = lm(pets_on ~ date, data = shopping_before)
pets_on_lm_after = lm(pets_on ~ date, data = shopping_after)
pets_on_lm_before
pets_on_lm_after
pets_on_div = pets_on_lm_after$coefficients[2] / pets_on_lm_before$coefficients[2]
plot(shopping$date, shopping$pets_on, xlab = "date")
abline(coef(pets_on_lm_before), lwd = 2)
abline(coef(pets_on_lm_after), lwd = 2)

pets_off_lm_before = lm(pets_off ~ date, data = shopping_before)
pets_off_lm_after = lm(pets_off ~ date, data = shopping_after)
pets_off_lm_before
pets_off_lm_after
pets_off_div = pets_off_lm_after$coefficients[2] / pets_off_lm_before$coefficients[2]
plot(shopping$date, shopping$pets_off, xlab = "date")
abline(coef(pets_off_lm_before), lwd = 2)
abline(coef(pets_off_lm_after), lwd = 2)


food_service_on_lm_before = lm(food_service_on ~ date, data = shopping_before)
food_service_on_lm_after = lm(food_service_on ~ date, data = shopping_after)
food_service_on_lm_before
food_service_on_lm_after
food_service_on_div = food_service_on_lm_after$coefficients[2] / food_service_on_lm_before$coefficients[2]
plot(shopping$date, shopping$food_service_on, xlab = "date")
abline(coef(food_service_on_lm_before), lwd = 2)
abline(coef(food_service_on_lm_after), lwd = 2)

food_service_off_lm_before = lm(food_service_off ~ date, data = shopping_before)
food_service_off_lm_after = lm(food_service_off ~ date, data = shopping_after)
food_service_off_lm_before
food_service_off_lm_after
food_service_off_div = food_service_off_lm_after$coefficients[2] / food_service_off_lm_before$coefficients[2]
plot(shopping$date, shopping$food_service_off, xlab = "date")
abline(coef(food_service_off_lm_before), lwd = 2)
abline(coef(food_service_off_lm_after), lwd = 2)

plot(shopping$date, shopping$travel_on, xlab = "date")
travel_on_temp = shopping_before$travel_on
travel_on_temp[-1]
travel_on_max = travel_on_temp[32] - travel_on_temp[3]
travel_on_down = (travel_on_temp[37] - shopping_after$travel_on[1]) / travel_on_max

travel_off_temp = shopping_before$travel_off
travel_off_temp[-1]
travel_off_max = travel_off_temp[37] - travel_off_temp[2]
travel_off_down = (travel_off_temp[37] - shopping_after$travel_off[1]) / travel_off_max


leisure_on_temp = shopping_before$leisure_on
leisure_on_temp[-1]
leisure_on_max = leisure_on_temp[34] - leisure_on_temp[2]
leisure_on_down = (leisure_on_temp[37] - shopping_after$leisure_on[1]) / leisure_on_max

leisure_off_temp = shopping_before$leisure_off
leisure_off_temp[-1]
leisure_off_max = leisure_off_temp[30] - leisure_off_temp[4]
leisure_off_down = (leisure_off_temp[37] - shopping_after$leisure_off[1]) / leisure_off_max


par(mfrow = c(1,1))
plot(-10:10, -10:10, type = "n", xlab = "online", ylab = "offline")
lines(-10:10 * 0, -10:10, lty=1)
lines(-10:10, -10:10 * 0, lty=1)

points(computer_on_div, computer_off_div, pch = 19, cex = 1.5, col = "magenta")
points(book_on_div, 0, pch = 19, cex = 1.5, col = "gold")
points(food_on_div, food_off_div, pch = 19, cex = 1.5, col = "green")
points(agri_marine_product_on_div, agri_marine_product_off_div, pch = 19, cex = 1.5, col = "pink")
points(daily_supplies_on_div, daily_supplies_off_div, pch = 19, cex = 1.5, col = "orange")
points(pets_on_div, pets_off_div, pch = 19, cex = 1.5, col = "cyan")
points(food_service_on_div, food_service_off_div, pch = 19, cex = 1.5, col = "gray")

points(accessory_on_div, accessory_off_down * -5, pch = 22, cex = 1.5, col = "red", bg = "red")
points(0, cosmetic_off_down * -5, pch = 22, cex = 1.5, col = "blue", bg = "blue")

points(travel_on_down * -5, travel_off_down * -5, pch = 24, cex = 1.5, col = "purple", bg = "purple")
points(leisure_on_down * -5, leisure_off_down * -5, pch = 24, cex = 1.5, col = "brown", bg = "brown")







par(mfrow = c(1,2))

i = 44
lm_before = lm(food_service_on ~ date, data = shopping_before)
lm_after = lm(food_service_on ~ date, data = shopping_after)
plot(shopping$date, shopping[, i], xlab = "date", main = "Total amount of food_service Online")
abline(coef(lm_before), lwd = 2)
abline(coef(lm_after), lwd = 2)
plot(covid$date, covid$all_new, xlab = "date", main = "COVID19 new confirmed cases")
plot(shopping_before$date, shopping_before[, i], xlab = "date", main = "food_service Online Before COVID19")
plot(shopping_after$date, shopping_after[, i], xlab = "date", main = "food_service Online After COVID19")


i = 4
plot(shopping$date, shopping[, i], xlab = "date", main = "Total amount of food_service Offline")
plot(covid$date, covid$all_new, xlab = "date", main = "COVID19 new confirmed cases")
plot(shopping_before$date, shopping_before[, i], xlab = "date", main = "food_service Offline Before COVID19")
plot(shopping_after$date, shopping_after[, i], xlab = "date", main = "food_service Offline After COVID19")


plot(shopping_after$date, shopping_after$computer_on, xlab = "date", main = "computer Online After COVID19")
plot(shopping_after$date, shopping_after$computer_off, xlab = "date", main = "computer Offline After COVID19")

cor(covid_month$all, shopping_after$food_service_on)
cor(covid_month$all, shopping_after$food_service_off)
cor(covid_month$seoul, shopping_after$food_service_on)
cor(covid_month$seoul, shopping_after$food_service_off)









### 전년도와 2020년, 4년치 데이터 correlation 비교 ###
computeron = shopping[, 4]
computer2017on = computeron[2:9]
computer2018on = computeron[14:21]
computer2019on = computeron[26:33]
computer2020on = computeron[38:45]
computeron = data.frame(cbind(computer2017on, computer2018on, computer2019on, computer2020on))
computeron.cor = cor(computeron)
corrplot(computeron.cor, method = "number")

bookon = shopping[, 5]
book2017on = bookon[2:9]
book2018on = bookon[14:21]
book2019on = bookon[26:33]
book2020on = bookon[38:45]
bookon = data.frame(cbind(book2017on, book2018on, book2019on, book2020on))
bookon.cor = cor(bookon)
corrplot(bookon.cor, method = "number")

bookoff = shopping[, 6]
book2017off = bookoff[2:9]
book2018off = bookoff[14:21]
book2019off = bookoff[26:33]
book2020off = bookoff[38:45]
bookoff = data.frame(cbind(book2017off, book2018off, book2019off, book2020off))
bookoff.cor = cor(bookoff)
corrplot(bookoff.cor, method = "number")

clotheson = shopping[, 7]
clothes2017on = clotheson[2:9]
clothes2018on = clotheson[14:21]
clothes2019on = clotheson[26:33]
clothes2020on = clotheson[38:45]
clotheson = data.frame(cbind(clothes2017on, clothes2018on, clothes2019on, clothes2020on))
clotheson.cor = cor(clotheson)
corrplot(clotheson.cor, method = "number")

accessoryoff = shopping[, 11]
accessory2017off = accessoryoff[2:9]
accessory2018off = accessoryoff[14:21]
accessory2019off = accessoryoff[26:33]
accessory2020off = accessoryoff[38:45]
accessoryoff = data.frame(cbind(accessory2017off, accessory2018off, accessory2019off, accessory2020off))
accessoryoff.cor = cor(accessoryoff)
corrplot(accessoryoff.cor, method = "number")

cosmeticoff = shopping[, 13]
cosmetic2017off = cosmeticoff[2:9]
cosmetic2018off = cosmeticoff[14:21]
cosmetic2019off = cosmeticoff[26:33]
cosmetic2020off = cosmeticoff[38:45]
cosmeticoff = data.frame(cbind(cosmetic2017off, cosmetic2018off, cosmetic2019off, cosmetic2020off))
cosmeticoff.cor = cor(cosmeticoff)
corrplot(cosmeticoff.cor, method = "number")

foodon = shopping[, 14]
food2017on = foodon[2:9]
food2018on = foodon[14:21]
food2019on = foodon[26:33]
food2020on = foodon[38:45]
foodon = data.frame(cbind(food2017on, food2018on, food2019on, food2020on))
foodon.cor = cor(foodon)
corrplot(foodon.cor, method = "number")

foodoff = shopping[, 15]
food2017off = foodoff[2:9]
food2018off = foodoff[14:21]
food2019off = foodoff[26:33]
food2020off = foodoff[38:45]
foodoff = data.frame(cbind(food2017off, food2018off, food2019off, food2020off))
foodoff.cor = cor(foodoff)
corrplot(foodoff.cor, method = "number")

agri_marine_producton = shopping[, 16]
agri_marine_product2017on = agri_marine_producton[2:9]
agri_marine_product2018on = agri_marine_producton[14:21]
agri_marine_product2019on = agri_marine_producton[26:33]
agri_marine_product2020on = agri_marine_producton[38:45]
agri_marine_producton = data.frame(cbind(agri_marine_product2017on, agri_marine_product2018on, agri_marine_product2019on, agri_marine_product2020on))
agri_marine_producton.cor = cor(agri_marine_producton)
corrplot(agri_marine_producton.cor, method = "number")

daily_supplieson = shopping[, 18]
daily_supplies2017on = daily_supplieson[2:9]
daily_supplies2018on = daily_supplieson[14:21]
daily_supplies2019on = daily_supplieson[26:33]
daily_supplies2020on = daily_supplieson[38:45]
daily_supplieson = data.frame(cbind(daily_supplies2017on, daily_supplies2018on, daily_supplies2019on, daily_supplies2020on))
daily_supplieson.cor = cor(daily_supplieson)
corrplot(daily_supplieson.cor, method = "number")

daily_suppliesoff = shopping[, 19]
daily_supplies2017off = daily_suppliesoff[2:9]
daily_supplies2018off = daily_suppliesoff[14:21]
daily_supplies2019off = daily_suppliesoff[26:33]
daily_supplies2020off = daily_suppliesoff[38:45]
daily_suppliesoff = data.frame(cbind(daily_supplies2017off, daily_supplies2018off, daily_supplies2019off, daily_supplies2020off))
daily_suppliesoff.cor = cor(daily_suppliesoff)
corrplot(daily_suppliesoff.cor, method = "number")

petson = shopping[, 20]
pets2017on = petson[2:9]
pets2018on = petson[14:21]
pets2019on = petson[26:33]
pets2020on = petson[38:45]
petson = data.frame(cbind(pets2017on, pets2018on, pets2019on, pets2020on))
petson.cor = cor(petson)
corrplot(petson.cor, method = "number")

travelon = shopping[, 22]
travel2017on = travelon[2:9]
travel2018on = travelon[14:21]
travel2019on = travelon[26:33]
travel2020on = travelon[38:45]
travelon = data.frame(cbind(travel2017on, travel2018on, travel2019on, travel2020on))
travelon.cor = cor(travelon)
corrplot(travelon.cor, method = "number")

leisureon = shopping[, 24]
leisure2017on = leisureon[2:9]
leisure2018on = leisureon[14:21]
leisure2019on = leisureon[26:33]
leisure2020on = leisureon[38:45]
leisureon = data.frame(cbind(leisure2017on, leisure2018on, leisure2019on, leisure2020on))
leisureon.cor = cor(leisureon)
corrplot(leisureon.cor, method = "number")

leisureoff = shopping[, 25]
leisure2017off = leisureoff[2:9]
leisure2018off = leisureoff[14:21]
leisure2019off = leisureoff[26:33]
leisure2020off = leisureoff[38:45]
leisureoff = data.frame(cbind(leisure2017off, leisure2018off, leisure2019off, leisure2020off))
leisureoff.cor = cor(leisureoff)
corrplot(leisureoff.cor, method = "number")

food_serviceon = shopping[, 26]
food_service2017on = food_serviceon[2:9]
food_service2018on = food_serviceon[14:21]
food_service2019on = food_serviceon[26:33]
food_service2020on = food_serviceon[38:45]
food_serviceon = data.frame(cbind(food_service2017on, food_service2018on, food_service2019on, food_service2020on))
food_serviceon.cor = cor(food_serviceon)
corrplot(food_serviceon.cor, method = "number")

food_serviceoff = shopping[, 27]
food_service2017off = food_serviceoff[2:9]
food_service2018off = food_serviceoff[14:21]
food_service2019off = food_serviceoff[26:33]
food_service2020off = food_serviceoff[38:45]
food_serviceoff = data.frame(cbind(food_service2017off, food_service2018off, food_service2019off, food_service2020off))
food_serviceoff.cor = cor(food_serviceoff)
corrplot(food_serviceoff.cor, method = "number")
