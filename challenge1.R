library(dplyr)
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

#f. Restricting the transactional data to March 2017, what’s the average daily items
#among small and big machines?. Why do you think there is such a difference? Give at least 2 possible reasons.
#Note: To calculate daily sales consider only the “active days” of a machine to exclude machine failures. For that, divide the number of items sold by a machine by the total number of “distinct” days.
#Hint: Check function month() to do the date restriction.