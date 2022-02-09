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

######Add comment here

#Note: To calculate daily sales consider only the “active days” of a machine to exclude machine failures. For that, divide the number of items sold by a machine by the total number of “distinct” days.
#Hint: Check function month() to do the date restriction.

#2. [2 points] Consider the following plot of the number of items sold per machine and day
#Let us focus on the different patterns present in the data:
  #a. Is there a general trend in the number of snacks and drinks as the months progress
#from January to April? Is it the same for snacks and drinks? Why do you think that
#might be so?
#  b. Is there shorter time period trend as well? Is it the same for snacks and drinks? What
#do you might be the cause?
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


#3. [2 points] Given the following distribution of average income:
summary(Machines$income_average)

#a) Are there outliers? How would you treat them? Provide code with your answer

#Answer: Yes obviously exists outliers since the max and mean value are far from median
#1. drop the top 5% outliers
quantile(Machines$income_average,0.95,na.rm = T)

#2. replace outlier's value by 95%percentile 

#3. nature log - since they are all positive number

#b)Can you give three possibilities on how to treat the NA cases? Which option
#would you choose and why? Provide code with your answer

#1. fill with median

#2. drop them missing value

#3. 

#Hint: Take a look at the relation between having NA in the income average 
#and the average daily items.



#4.[2 points] According to the following boxplot, what is the median number of
#hotels in the machine area?
boxplot(Machines$num_hotels)

#5.[2 points] In this exercise we will build a location score that tells us 
#what’s the average daily items per machine depending on the location it is placed. 
#This model could be used to a) Decide in which locations to place new machines 
#b) Construct a benchmark for each machine: how much should it sell according to its location? 
#This can be used to detect problems in machines (i.e. illumination, bad placement within a station etc.)

#For that, you will build a linear model to predict machine daily items using the following features:
