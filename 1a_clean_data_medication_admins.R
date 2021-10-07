library(tidyverse)
library(lubridate)
meds <- vroom::vroom(here::here('data', 'raw', 'patient_medication_orders.csv'))

classes <-meds %>% 
  select(drug_code,drug_simple_generic_name, drug_therapeutic_class_name, drug_pharmaceutical_class_name, drug_pharmaceutical_subclass_name) %>% 
  distinct()



patient_medication_admins <- vroom::vroom(here::here('data', 'raw', 'patient_medication_admins.csv'))

df <- patient_medication_admins %>% 
  left_join(classes, by='drug_code')

df <- df %>% 
  mutate(start_date=date(start_datetime),
         start_year_month=floor_date(start_date, unit='month'),
         first_day_of_week=floor_date(start_date, unit='week', week_start = 1))


saveRDS(df, here::here('data', 'medication_admins.rds'))
