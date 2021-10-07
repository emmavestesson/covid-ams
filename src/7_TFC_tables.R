library(tidyverse)
library(lubridate)
covid_start <- as.Date('2020-03-16')
one_row_per_day_dot <- readRDS( here::here('data', '1_one_row_per_day_dot.rds')) 




## TFC to keep ----
tfc_inpatient <- one_row_per_day_dot %>% 
  filter(patient_class=='Inpatient') %>%
  distinct(adms_id, tfc_name) %>% 
  count(tfc_name) %>% 
  filter(n>200) %>% 
  pull(tfc_name)



# Define function to create variables needed to calculate dot per 1000 pds for different groups

dot <- function(data,  dot, ...) {
  data %>% 
    group_by(...) %>% 
    summarise(dot=sum(dot_antibacterial_drugs), patient_days=n(), unique_patients = n_distinct(project_id)) %>% 
    mutate(dot_per_1000_pd= dot/patient_days*1000) %>% 
    ungroup() 
  
}

df_inpatient <- one_row_per_day_dot %>% 
  filter(patient_class=='Inpatient') %>% 
  mutate(week = floor_date(date, unit = 'weeks', week_start = 1), 
         month = floor_date(date, unit = 'month'), 
         tfc_name = case_when(tfc_name %in% tfc_inpatient ~ tfc_name, 
                              is.na(tfc_name) ~ 'Other',
                              TRUE ~ 'Other')
  ) 

pd_by_tfc_week_inpatient <- df_inpatient %>% 
  dot(dot=dot, week, tfc_name) %>% 
  group_by(tfc_name) %>% 
  mutate(tfc_name_short = str_remove(tfc_name,'Paediatric ')) %>% 
  group_by(week) %>% 
  mutate(weekly_patient_days_total = sum(patient_days), 
         weekly_patient_days_p = patient_days/weekly_patient_days_total) %>% 
  ungroup() 



pd_by_tfc_inpatient_table <- pd_by_tfc_week_inpatient %>% 
  dplyr::select(week,  tfc_name_short, patient_days) %>% 
  pivot_wider(names_from = tfc_name_short, values_from = patient_days, values_fill =0) %>% 
  mutate(covid = week>= covid_start)  %>% 
  dplyr::select(-week) %>% 
  mutate(covid=factor(covid, levels = c(FALSE, TRUE), labels=c('Pre-COVID-19 ', 'COVID-19 ')), 
         covid = fct_rev(covid)) %>% 
  tbl_summary(by = covid) %>% 
  add_difference() %>% 
  modify_table_body(~.x %>% relocate(stat_2, .before=stat_1))
saveRDS(pd_by_tfc_inpatient_table, 'redacted_filepath/table/pd_by_tfc_inpatient_table.rds')

pd_by_tfc_day_inpatient <- df_inpatient %>% 
  dot(dot=dot, date, tfc_name) %>% 
  group_by(tfc_name) %>% 
  mutate(tfc_name_short = str_remove(tfc_name,'Paediatric ')) %>% 
  ungroup() 

dot_by_tfc_inpatient_table_day <- pd_by_tfc_day_inpatient %>% 
  select(date,  tfc_name_short, dot_per_1000_pd) %>% 
  pivot_wider(names_from = tfc_name_short, values_from = dot_per_1000_pd, values_fill =0) %>% 
  mutate(covid = date>= covid_start)  %>% 
  select(-date) %>% 
  mutate(covid=factor(covid, levels = c(FALSE, TRUE), labels=c('Pre-COVID-19 ', 'COVID-19 ')), 
         covid = fct_rev(covid)) %>% 
  tbl_summary(by = covid) %>% 
  add_difference() %>% 
  modify_table_body(~.x %>% relocate(stat_2, .before=stat_1))


pd_by_tfc_day_inpatient %>% 
  mutate(covid = date>= covid_start)  %>% 
  group_by(covid, tfc_name) %>% 
  summarise(across(dot_per_1000_pd, .fns = list(mean=mean, median = median))) %>% 
  ggplot(., aes(y = tfc_name, x = dot_per_1000_pd_median, group = covid, colour = covid)) + geom_point() + theme_minimal()

DOT_by_tfc <- pd_by_tfc_week_inpatient %>% 
  mutate(tfc_name_short = str_remove(tfc_name,'Paediatric '), 
         tfc_name_short = str_replace(tfc_name_short,'And ', '& ') ) %>% 
  group_by(tfc_name) %>% 
  mutate(across(dot_per_1000_pd, .fns = list(mean=mean, median = median))) %>% 
  mutate(covid = week>= covid_start , covid=factor(covid, levels = c(FALSE, TRUE), labels=c('Pre-COVID-19 ', 'COVID-19 ')))  %>% 
  ggplot(., aes(y = fct_reorder(tfc_name_short,-dot_per_1000_pd_median), x = dot_per_1000_pd, group = interaction(fct_rev(covid), tfc_name_short), colour =  covid)) + 
  geom_boxplot(outlier.colour = "white") + labs(x = '', y = '') + 
  scale_colour_brewer(palette="Set1") +
  scale_x_continuous( expand=c(0.001,0)) +
  theme_minimal() + 
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9,0.9))
ggsave(filename = 'redacted_filepath/figure/DOT_by_TFC.png', plot = DOT_by_tfc)

saveRDS(DOT_by_tfc, 'redacted_filepath/figure/DOT_by_TFC.rds')
