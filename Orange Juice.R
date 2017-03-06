#install.packages("psych")
library(psych)

#install.packages("lattice")
library(lattice)

library(mfx)

load("OJ-Data.RData")


describe(oj_DF)
tot_revenue_MM_64 = sum(oj_DF$p_MM64*oj_DF$q_MM64)
tot_revenue_MM_96 = sum(oj_DF$p_MM96*oj_DF$q_MM96)
tot_revenue_PL_64 = sum(oj_DF$p_PL64*oj_DF$q_PL64)
total_revenue = tot_revenue_MM_64 + tot_revenue_MM_96 + tot_revenue_PL_64
share_MM_64 = tot_revenue_MM_64 / total_revenue
share_MM_96 = tot_revenue_MM_96 / total_revenue
share_PL_64 = tot_revenue_PL_64 / total_revenue

oj_DF$price_gap_MM_96_MM_64=oj_DF$p_MM96-oj_DF$p_MM64
oj_DF$price_gap_MM_64_PL_64=oj_DF$p_MM64-oj_DF$p_PL64

describe(oj_DF)


hist(oj_DF$price_gap_MM_96_MM_64,
     col = "lightskyblue1",
     main = "Price gap between MM 96 oz and 64 oz", xlab = "Price gap ")



hist(oj_DF$price_gap_MM_64_PL_64,
     col = "lightskyblue1",
     main = "Price gap between MM 64 oz and 64 oz", xlab = "Price gap ")



oj_DF$MM_96_velocity = oj_DF$q_MM96/oj_DF$acv
oj_DF$MM_64_velocity = oj_DF$q_MM64/oj_DF$acv
oj_DF$PL_64_velocity = oj_DF$q_PL64/oj_DF$acv

#Estimate log-linear demand models for the two Tide products by regressing the log of velocity on own all prices (own and competing products).

lm_MM_96_velocity = lm(log(MM_96_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64), data = oj_DF) 
summary(lm_MM_96_velocity)



lm_MM_64_velocity = lm(log(MM_64_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64), data = oj_DF) 
summary(lm_MM_64_velocity)

#Re-estimate the log-linear demand models for the two Tide products including a time trend.

lm_MM_96_velocity_time = lm(log(MM_96_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64) + week, data = oj_DF) 
summary(lm_MM_96_velocity_time)



lm_MM_64_velocity_time = lm(log(MM_64_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64) + week, data = oj_DF) 
summary(lm_MM_64_velocity_time)



#create a new data set that only includes store-weeks in which none of the products were promoted.

oj_DF_2=subset(oj_DF, promo==0)

lm_MM_96_velocity_time_promo = lm(log(MM_96_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64) + week, data = oj_DF_2) 
summary(lm_MM_96_velocity_time_promo)



lm_MM_64_velocity_time_promo = lm(log(MM_64_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64) + week, data = oj_DF_2) 
summary(lm_MM_64_velocity_time_promo)


#Re-estimate the log-linear demand models for the two Tide products including a time trend and store fixed effects using the data for the non-promoted store-weeks.

lm_MM_96_velocity_time_promo_store = lm(log(MM_96_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64) + week + factor(store) -1 , data = oj_DF_2) 
summary(lm_MM_96_velocity_time_promo_store)



lm_MM_64_velocity_time_promo_store = lm(log(MM_64_velocity) ~ log(p_MM96)+ log(p_MM64) + log(p_PL64) + week + factor(store) -1, data = oj_DF_2) 
summary(lm_MM_64_velocity_time_promo_store)


#Calculate base (regular) prices

describe(oj_DF_2)




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










set.seed(5807)
catalog_DF$validation_sample = rbinom(nrow(catalog_DF), 1, 0.5)


fit = glm(buytabw ~ . - customer_no - validation_sample, family = binomial(link = "logit"), data = subset(catalog_DF, validation_sample == 0))

summary(fit)

fit_mx = logitmfx(buytabw ~ .- customer_no - validation_sample, data = subset(catalog_DF, validation_sample == 0), atmean = FALSE)
fit_mx

catalog_DF$Pr = predict(fit, newdata = catalog_DF, type = "response")

catalog_DF = subset(catalog_DF, validation_sample == 1)

boxplot(Pr ~ buytabw, data = catalog_DF,
        col = "hotpink1",
        xlab = "Customer did not buy (0) or bought (1)",
        ylab = "Predicted purchase probability")

source("createBins.R")

catalog_DF$Pr_index = createBins(catalog_DF$Pr, 10)


table_n_obs = aggregate(buytabw ~ Pr_index, FUN = length, data = catalog_DF)
colnames(table_n_obs)[2] = "n_obs"

table_n_buyers = aggregate(buytabw ~ Pr_index, FUN = sum, data = catalog_DF)
colnames(table_n_buyers)[2] = "n_buyers"

table_mean_predicted_probability = aggregate(Pr ~ Pr_index, FUN = mean, data = catalog_DF)
#colnames(table_mean_predicted_probability)[2] = " mean_predicted_probability"

table_mean_observed_probability = aggregate(buytabw ~ Pr_index, FUN = mean, data = catalog_DF)
#colnames(table_mean_observed_probability)[2] = " mean_observed_probability"


score_DF = merge(table_n_obs, table_n_buyers, by = "Pr_index")
score_DF = merge(score_DF, table_mean_predicted_probability, by = "Pr_index")
score_DF = merge(score_DF, table_mean_observed_probability, by = "Pr_index")

score_DF$avg_response = mean(catalog_DF$buytabw)

score_DF$lift = 100*score_DF$buytabw/score_DF$avg_response

plot(score_DF$Pr_index, score_DF$lift,
     type = "o", pch = 21, lwd = 0.4, bg = "skyblue1",
     xlab = "Score", ylab = "Lift")
axis(side = 2, at = seq(from = 0, to = 350, by = 50))
grid()

score_DF = score_DF[order(-score_DF$Pr_index),]
score_DF$cum_n_obs = cumsum(score_DF$n_obs)
score_DF$cum_n_buyers = cumsum(score_DF$n_buyers)
score_DF$cum_response = score_DF$cum_n_buyers/score_DF$cum_n_obs
score_DF$cum_lift = 100*score_DF$cum_response/score_DF$avg_response
score_DF$cum_mailed = 100*score_DF$cum_n_obs/sum(score_DF$n_obs)
plot(score_DF$cum_mailed, score_DF$cum_lift,
     type = "o", pch = 21, lwd = 0.4, bg = "skyblue1",
     xlab = "Percent Mailed", ylab = "Cumulative Lift")
axis(side = 2, at = seq(from = 0, to = 300, by = 50))
abline(h = 0)
grid()

score_DF$cum_gains = 100*score_DF$cum_n_buyers/sum(score_DF$n_buyers)
plot(score_DF$cum_mailed, score_DF$cum_gains,
     type = "o", pch = 21, lwd = 0.4, bg = "skyblue1",
     xlab = "Percent Mailed", ylab = "Cumulative Gains")
grid()
lines(x = c(0, 100), y = c(0, 100))

head(score_DF)


 

margin = 26.9
cost = 1.4
catalog_DF$expected_profit = margin*catalog_DF$Pr - cost

hist(catalog_DF$expected_profit,
     col = "lightskyblue1",
     main = "expected profit", xlab = "expected profit ")

catalog_DF = catalog_DF[order(-catalog_DF$expected_profit),]

total_customer=length(catalog_DF$buytabw)
target_DF = subset(catalog_DF, expected_profit > 0)
total_customer

profitable_customer=length(target_DF$buytabw)
profitable_customer

fraction_profitable_customers = profitable_customer/total_customer

total_profit = sum(catalog_DF$expected_profit)
total_target_profit = sum(target_DF$expected_profit)
total_target_profit

target_DF$cum_expected_profit = cumsum(target_DF$expected_profit)


target_DF$n_obs = 1

target_DF$cum_n_obs = cumsum(target_DF$n_obs)
target_DF$cum_mailed = 100*target_DF$cum_n_obs/sum(target_DF$n_obs)


plot(target_DF$cum_mailed, target_DF$cum_expected_profit,
     type = "o", pch = 21, lwd = 0.4, bg = "skyblue1",
     xlab = "Percent Mailed", ylab = "Cumulative Expected Profit")
grid()

target_DF$actual_profit = margin*target_DF$buytabw - cost
total_actual_profit = sum(target_DF$actual_profit)
total_actual_profit

catalog_DF$mass_profit = margin*catalog_DF$buytabw - cost
total_mass_profit = sum(catalog_DF$mass_profit)
total_mass_profit

percent_improvement = 100*(total_actual_profit - total_mass_profit)/total_mass_profit
percent_improvement










