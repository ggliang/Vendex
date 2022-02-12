library(dplyr)
library(lubridate)
library(ggplot2)

# Question 1 --------------------------------------------------------------



#1. [2 points] General overview of the data: 
Machines <- read.csv("./machine_data.csv")
#Machines:


  #*a. How many machines are there? ----
  #- 2495
  dim(Machines)[1]
  


  #*b. What percentage of them are small? ----
  mean(Machines$small_machine)
  #  Hint: dt %>% group_by(column_name) %>% n()will tell you how many rows
  #in data table dt fulfill each column_name value
  


  #*c. How do they distribute in terms of location type i.e. transport, petrol station? ----
    Machines %>% 
      group_by(location_type) %>% 
      summarise(n())
  
  #  Products
    
  

  #*d. How many products are there? Which category has the highest number of products? ----
  Products <- read.csv("./product_data.csv")
  dim(Products)[1]
  Products %>% 
    group_by(category) %>% 
    summarise(n()) %>% 
    head(1)
  
  
  
  #*e. Which category has the highest and lowest average price? And within snacks or drinks? ----
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
  
  
  
  #*f. Restricting the transactional data to March 2017, what’s the average daily items ----
  transactional <- read.csv("./transactional_data.csv")
  #among small and big machines?. 
  #transactional <- transactional[1:10000,]

  #transactional <- left_join(transactional,Machines[,c('machine','small_machine')],by='machine')
  transactional %>% 
    filter(month(as.Date(date))==3 & year(as.Date(date))==2017) %>% 
    group_by(machine) %>% 
    summarise(sales = n(),active = unique(day(as.Date(date)))) %>% 
    summarise(sales = unique(sales),active_days = n()) %>%
    left_join(Machines[,c('machine','small_machine')],by='machine') %>% 
    mutate(daily_sales_per_machine=sales/active_days) %>% 
    group_by(small_machine) %>% 
    summarise(mean(daily_sales_per_machine))
  

    #Why do you think there is such a difference? Give at least 2 possible reasons.
    
    ######Add comment here
    
    #Note: To calculate daily sales consider only the “active days” of a machine to exclude machine failures. For that, divide the number of items sold by a machine by the total number of “distinct” days.
    #Hint: Check function month() to do the date restriction.

# Question 2 --------------------------------------------------------------


#2. [2 points] Consider the following plot of the number of items sold per machine and day
#Let us focus on the different patterns present in the data:
  
  
  #*a. Is there a general trend in the number of snacks and drinks as the months progress ----
  #from January to April? Is it the same for snacks and drinks? Why do you think that
  #might be so?
  
  
  
  #*  b. Is there shorter time period trend as well? Is it the same for snacks and drinks? What  ----
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
  
  ggplot(daily_sales_type, aes(x = daily_sales_type$`as.Date(date)`, y = daily_sales_type$daily_sales, colour = daily_sales_type$type_drink_snack)) +
    geom_line()
  

# Question 3 --------------------------------------------------------------


#3. [2 points] Given the following distribution of average income:
summary(Machines$income_average)

  #*a) Are there outliers? How would you treat them? Provide code with your answer----
  
  #Answer: Yes obviously exists outliers since the max and mean value are far from median
  #1. drop the top 5% outliers
  Machines[Machines$income_average<quantile(Machines$income_average,0.95,na.rm = T),]
  
  #2. replace outlier's value by 95%percentile 
  
  Machines[Machines$income_average>=quantile(Machines$income_average,0.95,na.rm = T)&!is.na(Machines$income_average),'income_average']=quantile(Machines$income_average,0.95,na.rm = T)
  #3. nature log - since they are all positive number
  Machines[!is.na(Machines$income_average),'income_average']=log(Machines[!is.na(Machines$income_average),'income_average'])


  
  
  
  #*b)Can you give three possibilities on how to treat the NA cases? Which option----
  #would you choose and why? Provide code with your answer
  
  #1. fill with median
  Machines[is.na(Machines$income_average),'income_average'] = median(Machines$income_average,na.rm = T)
  
  #2. drop them missing value
  remove_missing(Machines,vars = 'income_average')
  
  #3. fill with zero
  Machines[is.na(Machines$income_average),'income_average'] = 0
  
  #Hint: Take a look at the relation between having NA in the income average 
  #and the average daily items.



# Question 4 --------------------------------------------------------------


#4.[2 points] According to the following boxplot, what is the median number of
#hotels in the machine area?
boxplot(Machines$num_hotels)
median(Machines$num_hotels)


# Question 5 --------------------------------------------------------------


#5.[2 points] In this exercise we will build a location score that tells us 
#what’s the average daily items per machine depending on the location it is placed. 
#This model could be used to a) Decide in which locations to place new machines 
#b) Construct a benchmark for each machine: how much should it sell according to its location? 
#This can be used to detect problems in machines (i.e. illumination, bad placement within a station etc.)

#For that, you will build a linear model to predict machine daily items using the following features:
#1. Machine size (big or small) 
#2. Income of the area 
#3. Number of routes in the area 
#4. Number of hotels with 4 and 5 stars in the area 
#5. 0-1 Indicator of whether the machine has train_AvgDailyPassengers informed 
#meaning it is a petrol station or other type of location 
#Hint: You can generate an indicator variable of this sort using the syntax  
#6. Number of other Vendex machines in the area 

  #*a. Do all variables show statistical significance? Which ones doesn’t? How do you know? ----
  Machines <- Machines %>% 
    mutate(near_station = ifelse(is.na(train_AvgDailyPassengers),0,1)) 
  
  machine_daily_sale <- transactional %>% 
    group_by(machine) %>% 
    summarise(sales = n(),active = unique(date)) %>% 
    summarise(sales = unique(sales),active_days = n()) %>%
    mutate(machine_daily_sales=sales/active_days)
  
  Machines <- left_join(Machines,machine_daily_sale[,c('machine','machine_daily_sales')],by='machine')
  
  
  set.seed(2022)
  Machines_split <- initial_split(Machines, prop = .8, strata = small_machine)
  Machines_train <- training(Machines_split)
  Machines_test <- testing(Machines_split)
  
  model1 <- lm(formula = machine_daily_sales ~ small_machine + income_average
                          + total_number_of_routes_600 + num_hotels_45
                          + near_station + num_vendex_nearby_300, data = Machines_train)
  
  summary(model1)
  
  

  
  #*b. Build another linear model but this time instead of using the variables ----
  #“total_number_of_routes_600 use the log of that variable in base 10 calling it 
  #“log_transport”. Does this new variable show statistical significance?  
  
  Machines <- Machines %>% 
    mutate(log_transport = log(total_number_of_routes_600,10))
  
  set.seed(2022)
  Machines_split <- initial_split(Machines, prop = .8, strata = small_machine)
  Machines_train <- training(Machines_split)
  Machines_test <- testing(Machines_split)
  
  model2 <- lm(formula = machine_daily_sales ~ small_machine + income_average
               + log_transport + num_hotels_45
               + near_station + num_vendex_nearby_300, data = Machines_train)
  
  summary(model2)

  
  
  
  #* define plot function ----
  theme <- theme_light()
  predictions_vs_actual_plot <- function(df, x, y, title){
    x <- enquo(x)
    y <- enquo(y)
    ggplot(df, aes(!!x, !!y)) +
      geom_point(alpha = .5) +
      geom_abline(intercept = 0, slope = 1, color = 'orange') +
      ggtitle(title) +
      theme
  }

  

  #*c. How many daily items less do small machines sell all other factors remaining equal? ----
  final_model = model2
  ##1.78 less

  
  

  #*d. What’s effect on machine sales does having other nearby machines all other ----
  #factors remaining equal? 
  # 0.1 less or we could say very small influence


  
  #*e. Ranking all machines according to the final_model, what are the real daily sales of ----
  #the top20% machines with respect to your model prediction? And the real daily 
  #sales of the bottom20% machines according to your model? What’s the 
  #top20%/bottom20% ratio? 
  
  real_top20 <- Machines[Machines$machine_daily_sales>quantile(Machines$machine_daily_sales,.80),]
  real_bot20 <- Machines[Machines$machine_daily_sales<quantile(Machines$machine_daily_sales,.20),]
  
  real_top20 <- real_top20 %>% 
    bind_cols(predict(final_model, newdata = real_top20)) %>% 
    rename(.preds = ...15)
  
  predictions_vs_actual_plot(df = remove_missing(real_top20), x = machine_daily_sales, y = .preds, title = 'Linear regression: Test data')
  
  real_bot20 <- real_bot20 %>% 
    bind_cols(predict(final_model, newdata = real_bot20)) %>% 
    rename(.preds = ...15)
  
  predictions_vs_actual_plot(df = remove_missing(real_bot20), x = machine_daily_sales, y = .preds, title = 'Linear regression: Test data')

  
  

  #*f. Given the following 2 locations for a big machine: ----
  #i. Supermarket entrance, 2 nearby hotels of 4 stars, 20 transport routes, no 
  #nearby machines 
  #ii. Transport station, no nearby hotels of 4 or 5 stars, 10 transport routes 
  #nearby, 3 nearby Vendex machines 
  #Which location would you choose and why? 
  
  # the second
  
  new_data <- data.frame(
    small_machine = 1,
    income_average = 10,
    log_transport = c(log(20,10),log(10,10)),
    num_hotels_45 = c(2,0),
    near_station = c(0,1),
    num_vendex_nearby_300 = c(0,3)
  )
  predict(final_model, newdata = new_data)