library(dplyr)
library(lubridate)
Machines <- read.csv("./machine_data.csv")
#1. [2 points] General overview of the data: 

#Machines:
  #a. How many machines are there? - 2495
  dim(Machines)[1]
  
  #b. What percentage of them are small?
  mean(Machines$small_machine)
#  Hint: dt %>% group_by(column_name) %>% n()will tell you how many rows
#in data table dt fulfill each column_name value
#c. How do they distribute in terms of location type i.e. transport, petrol station?
  Machines %>% 
    group_by(location_type) %>% 
    summarise(n())
  
#  Products
  Products <- read.csv("./product_data.csv")
  
#d. How many products are there? Which category has the highest number of products?
dim(Products)[1]
Products %>% 
  group_by(category) %>% 
  summarise(n()) %>% 
  head(1)
#e. Which category has the highest and lowest average price? And within snacks or drinks?
Products %>% 
  group_by(category) %>% 
  summarise(avg_price = mean(price)) %>% 
  arrange(desc(avg_price)) %>% 
  tail(1)
Products %>% 
  group_by(category) %>% 
  summarise(avg_price = mean(price)) %>% 
  arrange(desc(avg_price)) %>% 
  head(1)

#Transactional data
transactional <- read.csv("./transactional_data.csv")
#f. Restricting the transactional data to March 2017, what’s the average daily items
#among small and big machines?. 
#transactional <- transactional[1:10000,]

#transactional <- left_join(transactional,Machines[,c('machine','small_machine')],by='machine')
transactional %>% 
  filter(month(as.Date(date))==3 & year(as.Date(date))==2017) %>% 
  group_by(machine) %>% 
  summarise(sales = n(),active = unique(day(as.Date(date)))) %>% 
  summarise(sales = unique(sales),active_days = n()) %>%
  left_join(Machines[,c('machine','small_machine')],by='machine') %>% 
  group_by(small_machine) %>% 
  summarise(mean(sales/active_days))
  
#Why do you think there is such a difference? Give at least 2 possible reasons.
#Note: To calculate daily sales consider only the “active days” of a machine to exclude machine failures. For that, divide the number of items sold by a machine by the total number of “distinct” days.
#Hint: Check function month() to do the date restriction.


daily_active_machines <- transactional %>% 
  filter(month(as.Date(date)) %in%c(1:4) & year(as.Date(date))==2017) %>% 
  left_join(Machines[,c('machine','small_machine')],by='machine') %>%
  group_by(as.Date(date)) %>% 
  summarise(unique(machine)) %>% 
  summarise(active_machines = n())

daily_sales_type <- transactional %>% 
  filter(month(as.Date(date)) %in%c(1:4) & year(as.Date(date))==2017) %>% 
  left_join(Products[,c('product_name','type_drink_snack')],by='product_name') %>% 
  group_by(as.Date(date), type_drink_snack) %>% 
  summarise(total_sales = n()) 

daily_sales_type <- left_join(daily_sales_type,daily_active_machines,by='as.Date(date)')
daily_sales_type$daily_sales = daily_sales_type$total_sales/daily_sales_type$active_machines
library(ggplot2)
ggplot(daily_sales_type, aes(x = daily_sales_type$`as.Date(date)`, y = daily_sales_type$daily_sales, colour = daily_sales_type$type_drink_snack)) +
  geom_line()



