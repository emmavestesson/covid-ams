library(tidyverse)

covid_start <- as.Date('2020-03-16')

# Load data ----
adms_long <- readRDS(here::here('data','admissions_long.rds')) 
theatre <- readRDS(here::here('data', 'theatre_activity.rds'))

infection_drugs_given <- readRDS(here::here("data", "infection_drugs_given.rds"))
## immunosuppressants
drugs_given <- readRDS(here::here('data', 'medication_admins.rds'))

immuno <- drugs_given %>% 
  filter(drug_pharmaceutical_class_name=='Drugs affecting the immune response')

adms_long <- adms_long %>% 
  filter(date>='2019-04-29' ) %>% 
  select(-principal_problem, -los_minutes, -start_date, -end_date) %>% 
  filter(age_years<25) %>% 
  mutate(age_groups = case_when(age_years <1 ~ '<1 years', 
                                age_years < 2 ~ '[1,2) years', 
                                age_years < 5 ~ '[2,5) years', 
                                age_years < 11 ~ '[5,11) years', 
                                age_years < 18 ~ '[11,18) years', 
                                TRUE ~ '[18, 25) years'), 
         age_groups = as.factor(age_groups))  %>% 
  arrange(age_years) %>% 
  mutate(age_groups = fct_inorder(age_groups)) 



# create df with one row per patient per day and a variable with days of therapy
dot_drugs_per_patient <- infection_drugs_given %>% 
  distinct(project_id, drug_simple_generic_name, start_date) %>%
  count(project_id, start_date, name = 'dot') %>% 
  rename(date = start_date)

dot_drugs_per_patient_type <- infection_drugs_given %>% 
  distinct(project_id, drug_pharmaceutical_class_name, drug_simple_generic_name, start_date) %>%
  count(project_id, start_date, drug_pharmaceutical_class_name, name = 'dot') %>% 
  rename(date = start_date) %>% 
  pivot_wider(names_from = drug_pharmaceutical_class_name, values_from = dot, names_prefix = 'dot_', values_fill = 0) %>% 
  janitor::clean_names() 

dot_drugs_per_patient <- left_join(dot_drugs_per_patient, dot_drugs_per_patient_type, by = c('project_id', 'date'))

## immunos


dot_drugs_per_patient_immuno <- immuno %>% 
  distinct(project_id, drug_simple_generic_name, start_date) %>%
  count(project_id, start_date, name = 'immuno_dot') %>% 
  rename(date = start_date)

dot_drugs_per_patient_long <- left_join(adms_long, dot_drugs_per_patient,  by = c('project_id', 'date')) %>% 
  left_join( dot_drugs_per_patient_immuno,  by = c('project_id', 'date')) %>% 
  mutate(across(starts_with('dot'),~replace_na(., 0))) %>% 
  mutate(across(ends_with('dot'),~replace_na(., 0)))

theatre_not_cancelled <- theatre %>% 
  filter(procedure_cancelled==0) %>% 
  distinct(project_id, start_date) %>% 
  mutate(theatre=1)

dot_drugs_per_patient_long_theatre <- left_join(dot_drugs_per_patient_long, theatre_not_cancelled, by = c('date'= 'start_date', 'project_id')) %>% 
  mutate(theatre=replace_na(theatre, 0))

saveRDS(dot_drugs_per_patient_long_theatre, here::here('data', '1_one_row_per_day_dot.rds'))


## save data with patient days per patient class - to be used as a denominator ----

patients_in_hosp_daily <- adms_long %>% 
  count(patient_class, date)
saveRDS(patients_in_hosp_daily, here::here('data', '3_patients_in_hosp_daily.rds'))


## save data with one row per patient----

one_row_per_admission <- dot_drugs_per_patient_long_theatre %>% 
  group_by(adms_id, project_id, patient_class, tfc_name, admission_type, admission_source,
           start_datetime, end_datetime,original_end_date, sex, age_years,  age_groups,deceased_flag,
           hosp_days_adms) %>% 
  summarise(theatre_procedure_ever = max(theatre), 
            theatre_procedure_total = sum(theatre), 
            across(contains('dot'), sum, .names= 'total_{col}')) %>% 
  ungroup() %>% 
  mutate(covid=start_datetime>=covid_start, male = sex=='M')
saveRDS(one_row_per_admission, here::here('data', '3_one_row_per_admission.rds'))

