library(tidyverse)
library(labelled)
library(lubridate)
library(gtsummary)
one_row_per_admission <- readRDS(here::here('data', '3_one_row_per_admission.rds'))
one_row_per_day_dot <- readRDS(here::here('data',"1_one_row_per_day_dot.rds"))
adms_long <- readRDS(here::here('data','admissions_long.rds')) 
covid_start <- '2020-03-16'
## theatre
theatre <- readRDS(here::here('data', 'theatre_activity.rds'))
theatre_not_cancelled <- theatre %>% 
  filter(procedure_cancelled==0) %>% 
  distinct(project_id, start_date) %>% 
  mutate(theatre=1)

theatre_long <- inner_join(adms_long, theatre_not_cancelled, by = c('date'= 'start_date', 'project_id')) 
theatre_weekly <- theatre_long %>% 
  mutate( first_date_of_week=floor_date(date, unit='week', week_start = 1)) %>% 
  count(patient_class, first_date_of_week, name = 'weekly_theatre') 



one_row_per_discharge <- one_row_per_admission %>% 
  mutate(year= year(end_datetime), 
         week = isoweek(end_datetime))

hospital_weekly_inpatient <- one_row_per_admission %>%
  filter(patient_class=='Inpatient') %>% 
  mutate( first_date_of_week=floor_date(start_datetime, unit='week', week_start = 1)) %>% 
  add_count(first_date_of_week, patient_class, name='weekly_admissions')  %>% 
  select(first_date_of_week, contains('weekly'))  %>% 
  distinct(first_date_of_week, .keep_all=TRUE) 

inpatient_weekly_discharges <- one_row_per_discharge %>% 
  filter(patient_class=='Inpatient') %>% 
  mutate( first_date_of_week_discharge=floor_date(end_datetime, unit='week', week_start = 1)) %>% 
  count(first_date_of_week_discharge, name='weekly_discharges') %>% 
  set_variable_labels(weekly_discharges = "Weekly discharges")

bed_nights <- adms_long %>% 
  distinct(adms_id, start_datetime, end_datetime, date) %>% 
  mutate(end_date=date(end_datetime)) %>% 
  filter(date!=end_date) %>% 
  mutate( first_date_of_week=floor_date(date, unit='week', week_start = 1)) %>%  
  count(first_date_of_week, name='weekly_bed_nights') 

bed_days <- adms_long %>% 
  distinct(adms_id, start_datetime, end_datetime, date) %>% 
  mutate(end_date=date(end_datetime)) %>% 
  mutate( first_date_of_week=floor_date(date, unit='week', week_start = 1)) %>%  
  count(first_date_of_week, name='weekly_bed_days') 

drugs_weekly <- one_row_per_day_dot %>% 
  mutate( first_date_of_week=floor_date(date, unit='week', week_start = 1)) %>% 
  group_by(patient_class, first_date_of_week) %>% 
  summarise(across(contains('dot'), sum)) %>% 
  ungroup()


hospital_weekly_inpatient <- theatre_weekly %>%
  left_join(drugs_weekly, by=c('first_date_of_week'='first_date_of_week', 'patient_class')) %>% 
  filter(patient_class == 'Inpatient') %>% 
  left_join(inpatient_weekly_discharges, by=c('first_date_of_week'='first_date_of_week_discharge')) %>% 
  left_join(hospital_weekly_inpatient, by=c('first_date_of_week') ) %>% 
  left_join(bed_nights, by=c('first_date_of_week'))  %>% 
  left_join(bed_days, by=c('first_date_of_week'))  %>% 
  set_variable_labels(weekly_admissions = "Weekly admissions", 
                      weekly_theatre='Weekly surgical encounters', 
                      weekly_bed_nights='Weekly bed nights', weekly_bed_days='Weekly bed days',
                      weekly_discharges='Weekly discharges') %>% 
  relocate(c(weekly_theatre), .after=last_col())  



# day cases

hospital_weekly_day_case <- one_row_per_admission %>% 
  filter(patient_class=='Day Case') %>% 
  mutate( first_date_of_week=floor_date(start_datetime, unit='week', week_start = 1)) %>% 
  add_count(covid, first_date_of_week, name='weekly_admissions')  %>% 
  select(first_date_of_week, contains('weekly'))  %>% 
  distinct(first_date_of_week, .keep_all=TRUE) %>% 
  # mutate(weekly_discharges=NA, weekly_bed_nights=NA) %>% 
  left_join(theatre_weekly, by = 'first_date_of_week') %>% 
  left_join(drugs_weekly, by=c('first_date_of_week', 'patient_class')) %>% 
  filter(patient_class=='Day Case') %>% 
  relocate(c(weekly_theatre), .after=last_col()) 



combine_day_cases_and_inpatient <- bind_rows(hospital_weekly_day_case, hospital_weekly_inpatient) %>% 
  select(-contains('dot')) %>% 
  set_variable_labels(weekly_admissions = "Weekly admissions", 
                    weekly_bed_nights='Weekly bed nights',
                    weekly_bed_days='Weekly bed days',
                    weekly_discharges='Weekly discharges' , 
                    weekly_theatre='Weekly surgical encounters') %>% 
  mutate(covid=first_date_of_week>=covid_start, 
         covid=factor(covid, levels = c(FALSE, TRUE), labels=c('Pre-COVID-19', 'COVID-19')))%>% 
  select(-first_date_of_week)



combine_day_cases_and_inpatient_table <- tbl_strata(combine_day_cases_and_inpatient,
           strata = patient_class,
           .tbl_fun = ~.x %>%
             tbl_summary( by = covid, missing = 'no', type = list(contains('dot') ~ 'continuous') ) %>% 
             add_p(),
           .combine_with = "tbl_stack")
combine_day_cases_and_inpatient_table[["table_body"]] <- combine_day_cases_and_inpatient_table[["table_body"]] %>% 
  filter(!str_detect(stat_1, 'NA'))

saveRDS(combine_day_cases_and_inpatient_table, here::here('output', 'tables', 'weekly_hospital.rds'))
saveRDS(combine_day_cases_and_inpatient_table, '/home/emma/Documents/upgrade/index/table/combine_day_cases_and_inpatient_table.rds')
