library(tidyverse)
library(gtsummary)

one_row_per_admission <- readRDS('~/Documents/AMR/data/3_one_row_per_admission.rds')

one_row_per_admission <- one_row_per_admission %>% 
  mutate(tfc_name_lumped = fct_lump_min(tfc_name,100), 
         covid=factor(covid, levels = c(FALSE, TRUE), labels=c('Pre-COVID-19 ', 'COVID-19 ')), 
         Male = sex=='M', 
         LOS = original_end_date - lubridate::date(start_datetime) ) %>% 
  mutate(tfc_name_lumped=fct_lump_min(tfc_name, 500, other_level='Other/missing'), 
         tfc_name_lumped=fct_explicit_na(tfc_name_lumped, na_level='Other/missing'),
         admission_type_simple = case_when( str_detect(admission_type, 'Elective') ~ 'Elective', 
                                            str_detect(admission_type, 'Transfer') ~ 'Transfer',
                                            str_detect(admission_type, 'Emergency') ~ 'Emergency', 
                                            TRUE ~ 'Other') ) 

admission_summary <- one_row_per_admission %>% 
  mutate(across(contains('dot'), list(~.>0), .names = "{.col}_ever")) %>% 
  select(covid, age_years,  Male, LOS, theatre_procedure_ever, admission_type_simple, contains('_ever'), patient_class)  %>% 
  select(-total_dot_ever, -total_dot_anthelmintics_ever) %>% 
  mutate(  covid= fct_rev(covid)) %>% 
  tbl_strata(.,
             strata = patient_class,
             .tbl_fun = ~.x %>%
               tbl_summary( by = covid, missing = 'no', 
                            label= list(age_years ~ "Age (years)",
                                        theatre_procedure_ever ~ 'Any theatre encounter',
                                        admission_type_simple ~ 'Admission type',
                                        total_dot_antibacterial_drugs_ever ~ 'Antibiotics during stay', 
                                        total_dot_antifungals_ever ~ 'Antifungals during stay',
                            total_dot_antivirals_ever ~ 'Antivirals during stay',
             total_dot_antiprotozoal_drugs_ever ~ 'Antiprotozoal during stay',
             total_immuno_dot_ever ~ 'Immunosuppressants during stay'
             )
                            ) %>%
               add_p() %>%
               modify_table_body(~.x %>% relocate(stat_2, .before=stat_1)),
             .combine_with = "tbl_stack")


saveRDS(admission_summary, here::here('output', 'tables', 'patient_admission_summary.rds'))
saveRDS(admission_summary, 'redacted_filepath/table/patient_admission_summary.rds')

