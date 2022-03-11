library(tidyr)
a <- left_join(transactional,Machines,by='machine')
a %>% 
  group_by(location_type) %>% 
  summarise(product_name)

  
pivot_wider(a, names_from = location_type, values_from = machine)
