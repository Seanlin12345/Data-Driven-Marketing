# Mayo.R
# -----------------------------------------------------------------------------
# Script to accompany the "Introduction to R" tutorial
# Author: Günter J. Hitsch


# Load men_DF (a data frame) in R data format
load("Mayo.RData")


# Installing packages ------------------------------------
# Uncomment the line below to install the package!
#install.packages("psych")
library(psych)

#install.packages("lattice")
library(lattice)

# Inspecting the data -------------------------------------
summary(mayo_DF)
summary(subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Jewel"))


# Use one variable to create groups:
describeBy(mayo_DF, mayo_DF$product)
# Use multiple variables to create groups:
describeBy(mayo_DF, list(mayo_DF$product, mayo_DF$market))


#Construct an average price variable by dividing dollar sales by unit sales:
mayo_DF$price = mayo_DF$sales_dollars/mayo_DF$sales_units

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
summary(lm_Hellman_Jewel) 

lm_Kraft_Jewel = lm(log(sales_units) ~ log(price), data = mayo_DF, subset=(product=="Kraft Mayo 32oz" & market=="Jewel"))
summary(lm_Kraft_Jewel) 

lm_Hellman_Central = lm(log(sales_units) ~ log(price), data = mayo_DF, subset=(product=="Hellmans Mayo 32oz" & market=="Kraft Central")) 
summary(lm_Hellman_Central) 

lm_Kraft_Central = lm(log(sales_units) ~ log(price), data = mayo_DF, subset=(product=="Kraft Mayo 32oz" & market=="Kraft Central"))
summary(lm_Kraft_Central) 


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

