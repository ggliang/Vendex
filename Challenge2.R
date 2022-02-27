library(dplyr)
library(lubridate)
library(rsample)
library(pROC)
transactional <- read.csv("./transactional_data.csv")
failures <- read.csv("./machine_failures.csv")

#[3 points] Creating the relevant variable of the model:

#1. Merge the transactional dataset with the machine failures data set ----
#setting failure variable to 0 when no failure is recorded
transactional_fail <- merge(transactional,failures,by=c('machine','timestamp','column'),all.x=T)
transactional_fail[is.na(transactional_fail$failure),'failure'] = 0

#2. In the transactional data table, create a variable called “last_vend” ----
#containing the timestamp of the previous sale of each machine
a <- transactional_fail[order('machine','timestamp')]
transactional_fail <- transactional_fail[order(transactional_fail$machine,transactional_fail$timestamp),]
transactional_fail <- transactional_fail %>% 
  group_by(machine)%>% 
  mutate(last_vend = lag(timestamp))

#3. Create a new variable in the transactional data table called “deltahours” ----
#containing, for every sale, the hours that passed since the last sale
transactional_fail <- transactional_fail %>% 
  mutate(deltahours=difftime(timestamp,last_vend,units='hours'))

#4. Create an auxiliary data table called “machine_daily_average” with the average ----
#daily sales per machine. Use this auxiliary table to attach to every row of 
#the transactional data table the the average daily sales per machine. 
#You can do this by doing a merge.

machine_daily_average <- transactional %>% 
  group_by(machine) %>% 
  summarise(sales = n(),active = unique(day(as.Date(date)))) %>% 
  summarise(sales = unique(sales),active_days = n()) %>%
  mutate(daily_sales_per_machine=sales/active_days) %>% 
  summarise(machine,daily_sales_per_machine)

transactional_fail <- merge(transactional_fail,machine_daily_average,by=c('machine'),all.x=T)

#5. Create a new variable called “delta” in the transactional data table containing ----
#a normalized version of deltahours consisting on the deltahours associated with
#each sale divided by the average deltahours of each machine i.e. 
#delta = deltahours /(24/daily_sales_machine). The interpretation of delta is the 
#amount of “missed sales” if the machine was selling at a constant rate
transactional_fail <- transactional_fail %>% 
  mutate(delta=as.numeric(deltahours)/(24/daily_sales_per_machine))

#6. Select 30% of the machines in the transactional data for testing and 70% of the ----
#machines for training and train a linear logistic regression model called “m” to 
#predict whether a machine has a failure as a function of variable delta. 
#What is the value of the intercept and the coefficient accompanying variable delta?

list_machines <- unique(transactional_fail$machine)
set.seed(2022)
train_set_machines <- sample(list_machines,round(0.7*length(list_machines),0),replace=F)
test_set_machines <- list_machines[!list_machines %in% train_set_machines]

train_set <- transactional_fail[transactional_fail$machine %in% train_set_machines,]
test_set <- transactional_fail[transactional_fail$machine %in% test_set_machines,]

m <- glm(failure~delta,data = train_set, family = 'binomial')
summary(m)

# coefficient of intercept is -6.883971
# coefficient of delta is 0.202129, they are all significantly different from zero
####!!!!!Do we need to interpret these numbers? I found it difficult to interpret them...


#a. [1 point] What’s the AUC, a measure of quality, of the model you have built on ----
#the train set? and on test set?
auc(train_set$failure,predict(m, newdata = train_set))  
auc(test_set$failure,predict(m, newdata = test_set))  

#AUC score for train set is 0.9196, for test set is 0.9191



#b. [1 point] Plot the function of probability of failure with respect to delta ----
#to gain intuition:
possible_delta <- seq(0,100,0.1)
m$coefficients[1]
m$coefficients[2]

failure_fun <- function(delta){
  1/(1+exp(-m$coefficients[1]-m$coefficients[2]*delta))
}
plot(possible_delta,failure_fun(possible_delta),'l')
#!!!! Shall we add the intuition here?

#c. [1 point] Let us create alarms with two levels of priority: med-risk and ----
#high-risk. Med-risk alarms will fire when the probability of failure is >=60% 
#and High-risk when that probability is >=80%.

#i. What are the threshold deltas for each type of alarm to fire?
failure_fun_60 <- function(delta){
  1/(1+exp(-m$coefficients[1]-m$coefficients[2]*delta))-0.6
}
root_60 <- uniroot(f = failure_fun_60,c(0,100))$root


failure_fun_80 <- function(delta){
  1/(1+exp(-m$coefficients[1]-m$coefficients[2]*delta))-0.8
}
root_80 <- uniroot(f = failure_fun_80,c(0,100))$root

#med-risk:36.06
#high-risk:40.92



#ii. How many of these alarms would be fired per day on average according to your model?

unique_days <- length(unique(transactional_fail$date))

length(transactional_fail[transactional_fail$delta>=root_60&!is.na(transactional_fail$delta),'delta'])/unique_days

#39.38 alarms would be fired everyday on average



#iii. What % of these will be “false alarms” i.e. failure variable is equal to 0,
#for each level of priority?

1-mean(transactional_fail[transactional_fail$delta>=root_60&!is.na(transactional_fail$delta),'failure'])

#20.54% alarms are false alarms



#d.[2 points] In this exercise we will estimate the profit impact of our EWS system vs the current system: ----
#i. If we set the EWS only with the med-risk alarms, what is the annual profit 
#we will generate vs the current system as a % of the total profit? 
#[For simplicity, consider the total profit to be the margin per item times 
#the number of items in the period]

margin = 1.7
operation_cost = 10
alarm_60 <- transactional_fail[transactional_fail$delta>=root_60&!is.na(transactional_fail$delta),]

alarm_60 <- alarm_60 %>% 
  mutate(threshold_hours=root_60*24/daily_sales_per_machine) %>% 
  mutate(threshold_hours_fixed = threshold_hours+1.5) %>% 
  mutate(delta_fixed = threshold_hours_fixed/(24/daily_sales_per_machine)) %>% 
  mutate(won_sales = (delta-delta_fixed)*failure)

extra_revenue_60 <- sum(alarm_60$won_sales)*margin
false_alarm_cost_60 <- length(alarm_60[alarm_60$failure==0,'failure'])*operation_cost
extra_profit_60 <- extra_revenue_60-false_alarm_cost_60

total_revenue <- length(transactional_fail[,'machine'])*margin

extra_profit_60/total_revenue

#4.93% increase

#ii. And if we set the EWS only with the high-risk alarms?


margin = 1.7
operation_cost = 10
alarm_80 <- transactional_fail[transactional_fail$delta>=root_80&!is.na(transactional_fail$delta),]

alarm_80 <- alarm_80 %>% 
  mutate(threshold_hours=root_80*24/daily_sales_per_machine) %>% 
  mutate(threshold_hours_fixed = threshold_hours+1.5) %>% 
  mutate(delta_fixed = threshold_hours_fixed/(24/daily_sales_per_machine)) %>% 
  mutate(won_sales = (delta-delta_fixed)*failure)

extra_revenue_80 <- sum(alarm_80$won_sales)*margin
false_alarm_cost_80 <- length(alarm_80[alarm_80$failure==0,'failure'])*operation_cost
extra_profit_80 <- extra_revenue_80-false_alarm_cost_80

total_revenue <- length(transactional_fail[,'machine'])*margin

extra_profit_80/total_revenue
#4.55% increase
  