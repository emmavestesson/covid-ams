
library(tidyverse)
library(lubridate)
library(vroom)

## admissions -----
# read in data

meds_raw <- vroom(here::here('data', 'raw', 'patient_medication_orders.csv'))

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


adms_raw <- vroom(here::here('data', 'raw', 
                             'patient_hospital_admissions.csv'))
admitted_before_epic <- adms_raw %>% 
  mutate(start_date=date(start_datetime)) %>% 
  filter(start_date< '2019-04-19')


adms <- adms_raw %>% 
  mutate(start_date=date(start_datetime), 
         end_date=date(end_datetime), 
         los=end_date-start_date, 
         start_dow=weekdays(start_date),
         start_working_day=case_when(start_dow %in% c('Saturday', 'Sunday') ~ 0,
                                     start_date %in% bank_holidays ~ 0 ,
                                     TRUE ~1),
         end_dow=weekdays(end_date),
         end_year_month=floor_date(end_date, unit='month'),
         end_working_day=case_when(end_dow %in% c('Saturday', 'Sunday') ~ 0,
                                   start_date %in% bank_holidays ~ 0 ,
                                   TRUE ~1)) %>% 
  add_count(project_id, name = 'number_of_stays') %>% 
  mutate(start_first_day_of_week=floor_date(start_date, 'week', week_start = 1),
         end_first_day_of_week=floor_date(end_date, 'week', week_start = 1), 
         end_datetime=replace_na(end_datetime, max(end_datetime, na.rm = TRUE)),
         hosp_days_adms=date(end_datetime)-date(start_datetime) +1, 
         hosp_days_adms=as.numeric(hosp_days_adms), 
         patient_class_booked=patient_class, 
         patient_class= case_when(hosp_days_adms==1 ~ 'Day Case', 
                                  hosp_days_adms>1 ~ 'Inpatient')) %>% 
  mutate(adms_id=row_number(), .after=project_id)  %>%
  select(-hospital_service) %>% 
  mutate(original_end_date=end_date)

patient_episodes <- read_csv("data/raw/patient_episodes.csv")
patient_episodes <- patient_episodes %>% 
  mutate(start_date = lubridate::date(start_datetime))
spect <-patient_episodes %>% 
  group_by(consultant_code) %>% 
  fill(spect_nat_code, spect_nat_name) %>% 
  ungroup() %>% 
  arrange(start_datetime) %>% 
  distinct(project_id, encounter_key, .keep_all = TRUE) %>% 
  select(project_id, encounter_key, tfc_code, tfc_name, directorate)

adms <- left_join(adms, spect, by=c('encounter_key', 'project_id'), suffix=c('_adms', '_episodes')) 


not_joined <- anti_join(adms, spect, by=c('encounter_key', 'project_id'), suffix=c('_adms', '_episodes'))
# check if any merge on date and patient id
extra <- inner_join(not_joined, patient_episodes, by=c('project_id', 'start_date'), suffix=c('_adms', '_episodes'))

## drop records with multiple entries on the same day that are shorter than 1 hour
adms <- adms %>% 
  mutate(los_minutes = end_datetime-start_datetime) %>% 
  add_count(project_id, start_date) %>% 
  mutate(drop=(n>1 & los_minutes<60)) %>% 
  filter(drop==FALSE) %>% 
  select(-drop, -n)


## drop records with multiple entries with the same start and date and the same treatment function
adms <- adms %>% 
  add_count(project_id, start_date, end_date, tfc_name, name = 'dupes') %>% 
  filter(!(dupes>1)) %>% 
  select(-dupes)


## add basic demographics

demographics <- vroom::vroom(here::here('data','raw', 'patient_demographics.csv'))

adms <- left_join(adms, demographics, by='project_id') %>% 
  mutate(age_years= lubridate::time_length(date(start_datetime)-date_of_birth,unit = 'years'), 
         age_years=ifelse(age_years<0, 0, age_years)) 


saveRDS(adms, here::here('data', 'admissions.rds'))


