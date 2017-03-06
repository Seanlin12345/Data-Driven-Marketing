#install.packages("psych")
library(psych)

#install.packages("lattice")
library(lattice)

library(mfx)

load("Catalog-Data.RData")


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










