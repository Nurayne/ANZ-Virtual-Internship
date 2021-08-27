library(stringr)
library(lubridate)
library(tidyverse)
library(modelr)
library(sp)
library(leaflet)
library(geosphere)
library(knitr)
library(rpart)
library(Metrics)
library(MASS)
library(fit)

# Check the salary payment frequency of each customer
df_inc = data.frame(customer_id = unique(df_csmp$customer_id))

# Create a mode function that will be used to find out what is the salary payment frequency
# R does not have a standard built-in function to calculate mode 
Mode = function(x){
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Loop through all salary payments for each customer
# Assume the salary level is constant for each customer over the observed period
for (i in seq(nrow(df_inc))){
  trans_data = ANZ_Dataset[ANZ_Dataset$customer_id == df_inc$customer_id[i] & ANZ_Dataset$txn_description == "PAY/SALARY", c("amount", "date")] %>% group_by(date) %>% summarise(amount = sum(amount))
  total_s = sum(trans_data$amount)
  count = dim(trans_data)[1]
  if (count == 0){
    df_inc$freq[i] = NA
    df_inc$level[i] = NA
  } else {
    s = c()
    lvl = c()
    for (j in seq(count - 1)){
      s = c(s, (trans_data$date[j+1] - trans_data$date[j]))
      lvl = c(lvl, trans_data$amount[j]) 
    }
    lvl = c(lvl, tail(trans_data$amount, n = 1))
    df_inc$freq[i] = Mode(s)
    df_inc$level[i] = Mode(lvl)
  }
}

df_inc$annual_salary = df_inc$level / df_inc$freq * 365.25

head(df_inc)

# Visualise the distribution of customers' annual salary
hist(df_inc$annual_salary[!is.na(df_inc$annual_salary)], breaks = c(seq(28000, 140000, by = 10000)), main = "Histogram of customers' annual salary", xlab = "Income ($)")


#Feature engineering
# Create a dataframe to store relevant features for customers
df_cus = df_csmp %>% select(customer_id, gender, age, amount, date, balance) %>% group_by(customer_id) %>% mutate(avg_no_weekly_trans = round(7*n()/length(unique(df$date)), 0), max_amt = max(amount), no_large_trans = sum(amount > 100), use_no_day = length(unique(date)), avg_trans_amt = mean(amount, na.rm = TRUE), med_bal = median(balance, na.rm = TRUE)) %>% select(-c("amount", "date", "balance")) %>% unique() 

# Create additional features
df_cus$age_below20 = ifelse(df_cus$age < 20, 1, 0)
df_cus$age_btw20n40 = ifelse(df_cus$age >= 20 & df_cus$age < 40, 1, 0)
df_cus$age_btw40n60 = ifelse(df_cus$age >= 40 & df_cus$age < 60, 1, 0)

# Investigate the state where customers live
# Assume they live where most transactions occured
df_region = df_csmp %>% group_by(customer_id, merchant_state) %>% summarise(trans_count = n()) %>% group_by(customer_id) %>% mutate(no_state = n()) %>% filter(trans_count == max(trans_count))

# For equal number of transactions between multiple states, pick the most likely state
n_occur = data.frame(table(df_region$customer_id))
cus_id_rep = n_occur$Var1[n_occur$Freq > 1]

state_by_cust_no = rev(names(sort(table(df_region$merchant_state), rev = TRUE)))
t = data.frame(customer_id = cus_id_rep, merchant_state = NA)

for (i in seq(length(cus_id_rep))){
  s = df_region$merchant_state[df_region$customer_id == cus_id_rep[i]]
  for (state in state_by_cust_no){
    if (state %in% s){
      t[i, 2] = state
      break
    }
  }
}

df_region <- df_region[!(df_region$customer_id %in% cus_id_rep), c(1,2)] %>% as.data.frame() %>% rbind(t) %>% rename(State = merchant_state)

head(df_region)

# Merge all the features into a single dataframe
df_cus = df_cus %>% merge(df_inc) %>% merge(df_region)
head(df_cus)

#Build a simple regression model to predict the annual salary for each customer
fit1 = lm(annual_salary ~. - customer_id - level - freq, data = df_cus)
summary(fit1)

MASS::stepAIC(fit1)

# Backwards model selection together with stepAIC yield the most appropriate model
fit2 = lm(formula = annual_salary ~ age + avg_trans_amt + med_bal + age_below20 + age_btw20n40 + age_btw40n60, data = df_cus)
summary(fit2)

# Examine the residuals to capture any missed relationships 
plot(fit2$residuals, ylab = "Residual")

rmse(fit2$fitted.values, df_cus$annual_salary)
