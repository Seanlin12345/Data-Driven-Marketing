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

detergent_DF$total_sales=detergent_DF$p_tide128*detergent_DF$q_tide128+detergent_DF$p_tide64*detergent_DF$q_tide64+detergent_DF$p_wisk64*detergent_DF$q_wisk64


detergent_DF$tide_128_market_share = detergent_DF$p_tide128*detergent_DF$q_tide128/detergent_DF$total_sales
detergent_DF$tide_64_market_share = detergent_DF$p_tide64*detergent_DF$q_tide64/detergent_DF$total_sales
detergent_DF$wisk_64_market_share = detergent_DF$p_wisk64*detergent_DF$q_wisk64/detergent_DF$total_sales

describe(detergent_DF)


#Calculate the price gap:
detergent_DF$price_gap_tide128_tide_64=detergent_DF$p_tide128-detergent_DF$p_tide64
detergent_DF$price_gap_tide64_wisk_64=detergent_DF$p_tide64-detergent_DF$p_wisk64


describe(detergent_DF)
  



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

