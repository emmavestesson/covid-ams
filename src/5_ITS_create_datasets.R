library(tidyverse)
library(broom)
library(lubridate)
library(gtsummary)


bank_holidays <- c('2019-01-01', '2019-04-19',
                   '2019-04-22', '2019-05-06',
                   '2019-05-27', '2019-08-26',
                   '2019-12-25', '2019-12-26', 
                   '2020-01-01', '2020-04-10',
                   '2020-04-13', '2020-05-08',
                   '2020-05-08', '2020-05-25',
                   '2020-08-31', '2020-12-25',
                   '2020-12-28', 
                   '2021-01-01') %>% 
  as.Date()


covid_start <- as.Date('2020-03-16')
one_row_per_day_dot <- readRDS( here::here('data', '1_one_row_per_day_dot.rds')) %>% 
  mutate(admission_type_simple = case_when( str_detect(admission_type, 'Elective') ~ 'Elective', 
                                                     str_detect(admission_type, 'Transfer') ~ 'Transfer',
                                                     str_detect(admission_type, 'Emergency') ~ 'Emergency', 
                                                     TRUE ~ 'Other') ) 
dc <- one_row_per_day_dot %>% 
  filter(patient_class == 'Day Case') %>% 
  mutate(tfc_name_lumped = fct_lump_min(tfc_name, 2000))



df <- one_row_per_day_dot %>% 
  group_by(date, patient_class) %>% 
  summarise(across(contains('dot'), sum),
            patient_count = n(), 
            theatre_p = mean(theatre),
            theatre = sum(theatre)) %>%                                                                
  ungroup() %>% 
  mutate(days_since_start = date - min(date), 
         covid = date>=covid_start, 
         covid = as.integer(covid),
         month = month(date, label = TRUE), 
         month = as.character(month), 
         dow = wday(date, label = TRUE), 
         dow = as.character(dow),
         day_of_year = yday(date),
         angle = day_of_year*pi/365 )

daily_tfc <- one_row_per_day_dot %>% 
  mutate(tfc_name_lumped = fct_lump_min(tfc_name, 2000)) %>% 
  count(date, tfc_name_lumped, patient_class) %>% 
  pivot_wider(id_cols = c(date, patient_class), 
              names_from = tfc_name_lumped,
              values_from = n, 
              values_fill = 0, 
              names_prefix = 'tfc_') %>% 
  janitor::clean_names()







## Weekly ---
# proportion of access drugs 
adms_long <- readRDS(here::here('data','admissions_long.rds')) 
infection_drugs_given <- readRDS(here::here("data", "infection_drugs_given.rds"))

adms_long <- adms_long %>% 
  filter(date>='2019-04-29' ) %>% 
  select(-principal_problem, -los_minutes, -start_date, -end_date, -start_datetime, -end_datetime) %>% 
  mutate(
    week = lubridate::floor_date(date, unit = 'week', week_start = 1)
  ) 

infection_drugs_given_adms <- left_join(adms_long, infection_drugs_given,  by = c('project_id', 'date' = 'start_date'))

abx <- infection_drugs_given_adms %>% 
  filter(drug_pharmaceutical_class_name == 'Antibacterial drugs') %>% 
  mutate(access = category == 'Access')

abx_access_proportion_weekly <- abx %>% 
  distinct(adms_id, project_id, date, drug_simple_generic_name, access, patient_class) %>% 
  mutate(week = lubridate::floor_date(date, unit = 'week', week_start = 1), 
         covid=week>=covid_start, 
         covid=factor(covid, levels = c(FALSE, TRUE), labels=c('Pre-COVID-19', 'COVID-19')), 
         covid= fct_rev(covid)) %>% 
  group_by(week, patient_class) %>% 
  summarise(access_p = mean(access, na.rm = TRUE))  %>% 
  ungroup()

dot <- function(data,  dot, ...) {
  data %>% 
    group_by(...) %>% 
    summarise(across(contains('dot'), sum,.names = '{.col}'),
              patient_days=n()) %>% 
    mutate(across(contains('dot'), list(~./patient_days*1000), .names = "{.col}_per_1000_pd")) %>% 
    ungroup()
}


weekly_theatre <- one_row_per_day_dot %>% 
  mutate(week=floor_date(date, unit = 'weeks', week_start = 1)) %>% 
  group_by(week,patient_class) %>% 
  summarise(theatre_p = mean(theatre),
            theatre_n = sum(theatre))
 
weekly_tfc <- one_row_per_day_dot %>% 
  mutate(tfc_name_lumped = fct_lump_min(tfc_name, 2000)) %>% 
  mutate(week=floor_date(date, unit = 'weeks', week_start = 1)) %>% 
  count(week, tfc_name_lumped, patient_class) %>% 
  pivot_wider(id_cols = c(week, patient_class), 
              names_from = tfc_name_lumped,
              values_from = n, 
              values_fill = 0, 
              names_prefix = 'tfc_') %>% 
  janitor::clean_names()

weekly_admission_type <- one_row_per_day_dot %>% 
  mutate(week=floor_date(date, unit = 'weeks', week_start = 1)) %>% 
  count(week, admission_type_simple, patient_class) %>% 
  pivot_wider(id_cols = c(week, patient_class), 
              names_from = admission_type_simple,
              values_from = n, 
              values_fill = 0, 
              names_prefix = 'adm_type_') %>% 
  janitor::clean_names()

dot_drugs_weekly_patient_class <- one_row_per_day_dot %>% 
  mutate(week=floor_date(date, unit = 'weeks', week_start = 1)) %>% 
  dot(dot=dot, week, patient_class) %>% 
    mutate(covid=week>=covid_start, 
           covid = as.integer(covid))%>% 
  mutate( weeks_since_start = as.double(difftime(week, min(week),unit="weeks")),
         month = month(week, label = TRUE), 
         month = as.character(month), 
         )

immuno_that_week <- one_row_per_day_dot %>% 
  mutate(week=floor_date(date, unit = 'weeks', week_start = 1)) %>% 
  group_by(week, patient_class) %>% 
  summarise(immuno_patient_count = sum(immuno_dot>0))
  
week_char  <- one_row_per_day_dot %>% 
  distinct(date) %>% 
  mutate( dow=weekdays(date),
          week=floor_date(date, unit = 'weeks', week_start = 1),
         working_day=case_when(dow %in% c('Saturday', 'Sunday') ~ 0,
                                     date %in% bank_holidays ~ 0 ,
                                     TRUE ~1)
         ) %>% 
  group_by(week) %>% 
  summarise(total_working_days = sum(working_day), 
            fewer_than_5_working_days = total_working_days<5) 

age_dist <- one_row_per_day_dot %>% 
  mutate(week=floor_date(date, unit = 'weeks', week_start = 1)) %>% 
  distinct(week,adms_id, .keep_all = TRUE) %>% 
  filter(age_groups =='<1 years') %>% 
  count(week, patient_class, age_groups, name = 'age_group_less_than_1')


its_weekly <- dot_drugs_weekly_patient_class %>% 
  left_join(abx_access_proportion_weekly, by = c("patient_class", "week")) %>%
  left_join(weekly_tfc, by = c("patient_class", "week")) %>%
  left_join(weekly_admission_type, by = c("patient_class", "week")) %>%
  left_join(weekly_theatre, by = c("patient_class", "week")) %>% 
  left_join(immuno_that_week, by = c("patient_class", "week")) %>% 
  left_join(age_dist, by = c("patient_class", "week")) %>% 
  left_join(week_char, by = c("week")) %>% 
  mutate(across(c(starts_with('tfc_'),starts_with('adm_type_'), 'theatre_n', 'immuno_patient_count', age_group_less_than_1), ~./patient_days*1000)) 


saveRDS(its_weekly, here::here('data','its_weekly.rds'))


