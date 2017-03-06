# Detergent.R
# -----------------------------------------------------------------------------
# Script to accompany the "Introduction to R" tutorial
# Author: Günter J. Hitsch


# Load men_DF (a data frame) in R data format
load("Slot-Data.RData")


# Installing packages ------------------------------------
# Uncomment the line below to install the package!
#install.packages("psych")
library(psych)

#install.packages("lattice")
library(lattice)

# Inspecting the data -------------------------------------
#summary(mayo_DF)
#describe(subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Jewel"))


# Use one variable to create groups:
#describeBy(mayo_DF, mayo_DF$product)
# Use multiple variables to create groups:
#describeBy(mayo_DF, list(mayo_DF$product, mayo_DF$market))


#Construct an marketshare by dividing sales by total sales:

table(slot_DF$denomination)

slot_DF$coin_in=slot_DF$winnings*100/slot_DF$hold

describe(slot_DF)





hist(slot_DF$hold, col="hotpink1", main="Histogram of Hold")
hist(slot_DF$days_active, col="hotpink1", main="Histogram of Hold")

slot_DF$coin_in_day = slot_DF$coin_in / slot_DF$days_active

lm_coin_hold_0=lm(log(coin_in_day)~log(hold), data=slot_DF)
summary(lm_coin_hold_0)


lm_coin_hold_1=lm(log(coin_in)~log(hold)+factor(days_active)-1, data=slot_DF)
summary(lm_coin_hold_1)

lm_coin_hold_2=lm(log(coin_in_day)~log(hold)+factor(days_active)-1, data=slot_DF)
summary(lm_coin_hold_2)

lm_coin_hold_3=lm(log(coin_in_day)~log(hold)+ days_active, data=slot_DF)
summary(lm_coin_hold_3)

lm_coin_hold_4=lm(log(coin_in_day)~log(hold)+ +factor(denomination)-1, data=slot_DF)
summary(lm_coin_hold_4)

slot_DF$coin_in_day_denomation = slot_DF$coin_in_day / slot_DF$denomination

lm_coin_hold_0_05=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="0.05"))

summary(lm_coin_hold_0_05)


lm_coin_hold_0_25=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="0.25"))

summary(lm_coin_hold_0_25)

lm_coin_hold_0_5=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="0.5"))

summary(lm_coin_hold_0_5)

lm_coin_hold_1=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="1"))

summary(lm_coin_hold_1)

lm_coin_hold_2=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="2"))

summary(lm_coin_hold_2)

lm_coin_hold_5=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="5"))

summary(lm_coin_hold_5)

lm_coin_hold_10=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="10"))

summary(lm_coin_hold_10)
lm_coin_hold_25=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="25"))

summary(lm_coin_hold_25)
lm_coin_hold_100=lm(log(coin_in_day)~log(hold), data=subset(slot_DF, denomination=="100"))

summary(lm_coin_hold_100)










aggregate(hold ~ denomination, FUN = mean, data = slot_DF)

#Histograms plots of prices gaps
histogram(~ price_gap_tide128_tide_64 | data = detergent_DF,
          lwd = 0.5, col = "lightskyblue",
          par.settings = list(strip.background = list(col = "gray95")))


hist(detergent_DF$price_gap_tide128_tide_64,
     col = "lightskyblue1",
     main = "Histogram of price gap between Tide 128oz and Tide 64Oz", xlab = "Price") 

hist(detergent_DF$price_gap_tide64_wisk_64,
     col = "lightskyblue1",
     main = "Histogram of price gap between Tide 64oz and Wisk 64Oz", xlab = "Price") 

#Construct the sales velocity:

detergent_DF$tide_128_velocity = detergent_DF$q_tide128/detergent_DF$acv
detergent_DF$tide_64_velocity = detergent_DF$q_tide64/detergent_DF$acv
detergent_DF$wisk_64_velocity = detergent_DF$q_wisk64/detergent_DF$acv

#Estimate log-linear demand models for the two Tide products by regressing the log of velocity on own all prices (own and competing products).


lm_tide_128_velocity = lm(log(tide_128_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64), data = detergent_DF) 
summary(lm_tide_128_velocity)


lm_tide_64_velocity = lm(log(tide_64_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64), data = detergent_DF) 
summary(lm_tide_64_velocity)

#Re-estimate the log-linear demand models for the two Tide products including a time trend.

lm_tide_128_velocity_time_trend = lm(log(tide_128_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64) + week, data = detergent_DF) 
summary(lm_tide_128_velocity)


lm_tide_64_velocity_time_trend = lm(log(tide_64_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64) + week, data = detergent_DF) 
summary(lm_tide_64_velocity)

#create a new data set that only includes store-weeks in which none of the products were promoted.

detergent_DF_2=subset(detergent_DF, promoflag==0)

lm_tide_128_velocity_time_trend_nonpromote = lm(log(tide_128_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64) + week, data = detergent_DF_2) 
summary(lm_tide_128_velocity_time_trend_nonpromote)

lm_tide_64_velocity_time_trend_nonpromote = lm(log(tide_64_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64) + week, data = detergent_DF_2)
summary(lm_tide_128_velocity_time_trend_nonpromote)


#Re-estimate the log-linear demand models for the two Tide products including a time trend and store fixed effects using the data for the non-promoted store-weeks.

lm_tide_128_velocity_time_trend_nonpromote_store = lm(log(tide_128_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64) + week + factor(store)-1, data = detergent_DF_2) 
summary(lm_tide_128_velocity_time_trend_nonpromote_store)

lm_tide_64_velocity_time_trend_nonpromote_store = lm(log(tide_64_velocity) ~ log(p_tide128)+ log(p_tide64) + log(p_wisk64) + week + factor(store)-1, data = detergent_DF_2)
summary(lm_tide_64_velocity_time_trend_nonpromote_store)

#Calculate base (regular) prices

describe(detergent_DF_2)




#-------------------------------------------------------------------------------------
#Time-series plots of prices

xyplot(price ~ week | product + market, data = mayo_DF,
       type = "o",
       pch = 21, lwd = 0.5, cex = 0.75, col = "gray40", fill = "hotpink1",
       par.settings = list(strip.background = list(col = "gray95")),
       xlab = "Week", ylab = "Price")

#Histograms plots of prices
histogram(~ price | product + market, data = mayo_DF,
          lwd = 0.5, col = "lightskyblue",
          par.settings = list(strip.background = list(col = "gray95")))

#unit sales vs prices plots 
xyplot(sales_units ~ price | product + market, data = mayo_DF,
       pch = 21, lwd = 0.5, cex = 0.75, col = "gray40", fill = "hotpink1",
       scales = list(relation = "free"),
       par.settings = list(strip.background = list(col = "gray95")),
       xlab = "Price", ylab = "Units")

#Construct log_price and log_sales_unit variable 
mayo_DF$log_price = log(mayo_DF$price)
mayo_DF$log_sales_units = log(mayo_DF$sales_units)


# Regression of log_price and log_sales_unit
lm_Hellman_Jewel = lm(log(sales_units) ~ log(price), data = mayo_DF, subset=(product=="Hellmans Mayo 32oz" & market=="Jewel")) 
describe(lm_Hellman_Jewel) 

lm_Kraft_Jewel = lm(log(sales_units) ~ log(price), data = mayo_DF, subset=(product=="Kraft Mayo 32oz" & market=="Jewel"))
describe(lm_Kraft_Jewel) 

lm_Hellman_Central = lm(log(sales_units) ~ log(price), data = mayo_DF, subset=(product=="Hellmans Mayo 32oz" & market=="Kraft Central")) 
describe(lm_Hellman_Central) 

lm_Kraft_Central = lm(log(sales_units) ~ log(price), data = mayo_DF, subset=(product=="Kraft Mayo 32oz" & market=="Kraft Central"))
describe(lm_Kraft_Central) 


#extract market", "product", "week", "sales_units", "price" to create the final data frame used for estimation:
mayo_DF_extract = mayo_DF[, c("market", "product", "week", "sales_units", "price")]

#reshape the data and create a new data frame with product-level sales unit and price information in columns:
mayo_DF_wide = reshape(mayo_DF_extract,
                       timevar = "product",
                       idvar = c("market", "week"),
                       direction = "wide")
#head(mayo_DF_wide)


#change long variable names:
colnames(mayo_DF_wide) = c("market", "week", "sales_H", "price_H", "sales_K", "price_K")
head(mayo_DF_wide)


lm_Hellman_Jewel_cross = lm(log(sales_H) ~ log(price_H)+ log(price_K), data = mayo_DF_wide, subset=( market=="Jewel")) 
summary(lm_Hellman_Jewel_cross) 

lm_Kraft_Jewel_cross = lm(log(sales_K) ~ log(price_H)+ log(price_K), data = mayo_DF_wide, subset=( market=="Jewel")) 
summary(lm_Kraft_Jewel_cross) 

lm_Hellman_Central_cross = lm(log(sales_H) ~ log(price_H)+ log(price_K), data = mayo_DF_wide, subset=( market=="Kraft Central")) 
summary(lm_Hellman_Central_cross) 

lm_Kraft_Central_cross = lm(log(sales_K) ~ log(price_H)+ log(price_K), data = mayo_DF_wide, subset=( market=="Kraft Central")) 
summary(lm_Kraft_Central_cross) 

