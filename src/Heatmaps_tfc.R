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


tfc_day_cases <- one_row_per_day_dot %>% 
  filter(patient_class=='Day Case') %>%
  distinct(adms_id, tfc_name) %>% 
  count(tfc_name) %>% 
  filter(n>200) %>% 
  pull(tfc_name)

# Define function to create variables needed to calculate dot per 1000 pds for different groups

dot <- function(data,  dot, ...) {
  data %>% 
    group_by(...) %>% 
    summarise(dot=sum(dot), patient_days=n(), unique_patients = n_distinct(project_id)) %>% 
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
  

### weekly heatmap  patient days---- 
pd_by_tfc_week <- df_inpatient %>% 
  dot(dot=dot, week, tfc_name) %>% 
  group_by(tfc_name) %>% 
  mutate(tfc_name_short = str_remove(tfc_name,'Paediatric ')) %>% 
  group_by(week) %>% 
  mutate(weekly_patient_days_total = sum(patient_days), 
         weekly_patient_days_p = patient_days/weekly_patient_days_total)


ggplot(pd_by_tfc_week, aes(x=week, y= tfc_name_short, fill = patient_days)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1) + theme_minimal() + labs(x='') +
  theme(legend.position = 'bottom')

pd_by_tfc_week %>% 
  group_by(tfc_name_short) %>% 
  mutate(normalised=(patient_days -min(patient_days))/(max(patient_days)-min(patient_days))) %>%
ggplot(., aes(x=week, y= fct_rev(tfc_name_short), fill = normalised)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1, option = 'inferno') + theme_minimal() + labs(x='', y ='') +
  scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'none', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.1, 'cm'), #change legend key size
        legend.key.height = unit(0.6, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), 
        legend.text = element_text(size = 6),
        plot.title=element_text(family="Times") )

ggsave(here::here('output', 'heatmap_inpatient_days_over_time_weekly_normalised.jpeg'), dpi=800, height=15, width=15, units = 'cm') 

ggsave('/home/emma/Documents/upgrade/index/figure/heatmap_inpatient_days_over_time_weekly_normalised.jpeg', dpi=800, height=15, width=15, units = 'cm') 




pd_by_tfc_month <- df_inpatient %>% 
  dot(dot=dot, month, tfc_name) %>% 
  filter(patient_days >5) %>%
  mutate(tfc_name_short = str_remove(tfc_name,'Paediatric ')) %>% 
  group_by(month) %>% 
  mutate(monthly_patient_days_total = sum(patient_days), 
         monthly_patient_days_p = patient_days/monthly_patient_days_total) 


ggplot(pd_by_tfc_month, aes(x=month, y= fct_rev(tfc_name_short), fill = patient_days)) + 
  geom_tile() + 
  scale_fill_viridis_c(direction = -1, option = 'inferno', na.value="white") + 
  theme_minimal() + 
  scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  labs(x='', y='',  subtitle = 'Patient days over time by treatment speciality') +
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
        plot.title=element_text(family="Times") )

ggsave(here::here('output', 'heatmap_patient_days_over_time.jpeg'), dpi=800, height=15, width=20, units = 'cm') 



### DOT per 1000 patient days---- 


ggplot(pd_by_tfc_week, aes(x=week, y= tfc_name_short, fill = dot_per_1000_pd)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1) + theme_minimal() + labs(x='') +
  theme(legend.position = 'bottom')



### DOT per 1000 patient days---- 


ggplot(pd_by_tfc_week, aes(x=week, y= tfc_name_short, fill = dot_per_1000_pd)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1) + theme_minimal() + labs(x='') +
  theme(legend.position = 'bottom')


ggplot(pd_by_tfc_month, aes(x=month, y= fct_rev(tfc_name_short), fill = dot_per_1000_pd)) + 
  geom_tile() + 
  scale_fill_viridis_c(direction = -1, option = 'cividis') +
  # scale_fill_distiller(palette="Spectral") +
  theme_minimal() + 
  scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  labs(x='', y='', subtitle = 'Days of therapy per 1000 patient days for inpatients') +
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
   plot.title=element_text(family="Times") )

ggsave(here::here('output', 'heatmap_DOT_per_1000_patient_days_inpatient.jpeg'), dpi=800, height=16, width=20, units = 'cm') 


### % of all patients in hospital ----

ggplot(pd_by_tfc_month, aes(x=month, y= fct_rev(tfc_name_short), fill = monthly_patient_days_p)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1, option = 'inferno') +
  theme_minimal() + scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  labs(x='', y='', title = 'Monthly distribution of patient days between specialties ') +
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
        # legend.justification = c("right", "top")
  )
ggsave( here::here('output', 'heatmap_patient_days_prop.jpeg'), dpi=800, height=20, width=18, units = 'cm') 



### categorical split
pd_by_tfc_month %>% 
  mutate(monthly_patient_days_cat = case_when(monthly_patient_days_p<0.05 ~ '0-5%', 
                                              monthly_patient_days_p<0.1 ~ '5-10%', 
                                              monthly_patient_days_p<0.2 ~ '10-20%', 
                                              monthly_patient_days_p <0.3 ~ '20-30%' )) %>% 
ggplot(., aes(x=month, y= fct_rev(tfc_name_short), fill = factor(monthly_patient_days_cat))) + 
  geom_tile() + scale_fill_viridis_d(direction = -1, option = 'inferno') +
  theme_minimal() + scale_x_date(breaks = '3 months', date_labels = '%b-%y') +
  labs(x='', y='', title = 'Monthly distribution of patient days between specialties ') +
  theme(plot.title.position = 'plot', 
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
        # legend.justification = c("right", "top")
  )
### normalised data ---

pd_by_tfc_month %>% 
  group_by(month) %>% 
  mutate(normalised=(patient_days -min(patient_days))/(max(patient_days)-min(patient_days))) %>%  
  ggplot(., aes(x=month, y= fct_rev(tfc_name_short), fill = normalised)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1, option = 'inferno') +
  theme_minimal() + scale_x_date(breaks = '3 months', date_labels = '%b-%y') +
  labs(x='', y='', title = 'Patient days normalised by month') +
  theme(plot.title.position = 'plot', 
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
        text = element_text(family="Times") 
        # legend.justification = c("right", "top")
  )


pd_by_tfc_month %>% 
  group_by(tfc_name_short) %>% 
  mutate(normalised=(patient_days -min(patient_days))/(max(patient_days)-min(patient_days))) %>%  
  ggplot(., aes(x=month, y= fct_rev(tfc_name_short), fill = normalised)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1, option = 'inferno') +
  theme_minimal() + scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand = c(0,0)) +
  labs(x='', y='', title = 'Patient days normalised by tfc') +
  theme(plot.title.position = 'plot', 
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
        text = element_text(family="Times") 
        # legend.justification = c("right", "top")
  )
## bar plots ---
pd_by_tfc_month %>% 
  group_by(tfc_name_short) %>% 
  summarise(total_patient_days_tfc = sum(patient_days)) %>% 
 ggplot(., aes(x=total_patient_days_tfc, y= tfc_name_short, fill=total_patient_days_tfc))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed deaths")+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.text.x=element_text(colour="Black"))
 
### day cases -----


df_dc <- one_row_per_day_dot %>% 
  filter(patient_class=='Day Case') %>% 
  mutate(week = floor_date(date, unit = 'weeks', week_start = 1), 
         month = floor_date(date, unit = 'month'), 
         tfc_name = case_when(tfc_name %in% tfc_day_cases ~ tfc_name, 
                              is.na(tfc_name) ~ 'Other',
                              TRUE ~ 'Other')
  ) 


pd_by_tfc_month_dc <- df_dc %>% 
  dot(dot=dot, month, tfc_name) %>% 
  mutate(tfc_name_short = str_remove(tfc_name,'Paediatric ')) %>% 
  group_by(month) %>% 
  mutate(monthly_patient_days_total = sum(patient_days), 
         monthly_patient_days_p = patient_days/monthly_patient_days_total) %>% 
  filter(month> '2019-04-01' & month <'2021-02-01' )

dc_total_month <- pd_by_tfc_month_dc %>% 
  summarise(total_patient_days_tfc = sum(patient_days)) %>% 
  mutate(tfc_name_short = '')


ggplot(pd_by_tfc_month_dc, aes(x=month, y= fct_rev(tfc_name_short), fill = patient_days)) + 
  geom_tile() + 
  scale_fill_viridis_c(direction = -1, option = 'inferno', na.value="white") + 
  theme_minimal() + 
  scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  labs(x='', y='',  subtitle = 'Day cases over time by treatment speciality') +
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
        plot.title=element_text(family="Times") )

ggsave(here::here('output', 'heatmap_day_cases_over_time.jpeg'), dpi=800, height=15, width=20, units = 'cm') 

pd_by_tfc_month_dc %>% 
  group_by(tfc_name_short) %>% 
  summarise(total_patient_days_tfc = sum(patient_days)) %>% 
  ggplot(., aes(x=total_patient_days_tfc, y= tfc_name_short, fill=total_patient_days_tfc))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed deaths")

day_case_bar_plot <- pd_by_tfc_month_dc %>% 
  group_by(month) %>% 
  summarise(total_patient_days_month = sum(patient_days)) %>% 
  ggplot(., aes(x=month, y= total_patient_days_month, fill=total_patient_days_month))+
  geom_col(show.legend=FALSE)+
  scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  theme_classic()+
  scale_fill_distiller(palette="Spectral")


dc_DOT_total_month <- df_dc %>% 
  dot(dot=dot, month) %>% 
  mutate(tfc_name_short = '', dot_per_1000_pd_label = round(dot_per_1000_pd) )  %>% 
  filter(month> '2019-04-01' & month <'2021-02-01' )

ggplot(pd_by_tfc_month_dc, aes(x=month, y= fct_rev(tfc_name_short), fill = dot_per_1000_pd)) + 
  geom_tile() + 
  scale_fill_viridis_c(direction = -1, option = 'cividis') +
  # scale_fill_distiller(palette="Spectral") +
  theme_minimal() + 
  scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  labs(x='', y='', subtitle = 'Days of therapy per 1000 patient days for day cases') +
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'right', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.3, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), 
        plot.title=element_text(family="Times") )

ggsave(here::here('output', 'heatmap_DOT_per_1000_patient_days_day_cases.jpeg'), dpi=800, height=16, width=20, units = 'cm') 

pd_by_tfc_week_dc <- df_dc %>% 
  dot(dot=dot, week, tfc_name) %>% 
  mutate(tfc_name_short = str_remove(tfc_name,'Paediatric ')) %>% 
  group_by(week) %>% 
  mutate(weekly_patient_days_total = sum(patient_days), 
         weekly_patient_days_p = patient_days/weekly_patient_days_total) 

pd_by_tfc_week_dc %>% 
  group_by(tfc_name_short) %>% 
  mutate(normalised=(patient_days -min(patient_days))/(max(patient_days)-min(patient_days))) %>%
  ggplot(., aes(x=week, y= fct_rev(tfc_name_short), fill = normalised)) + 
  geom_tile() + scale_fill_viridis_c(direction = -1, option = 'inferno') + theme_minimal() + labs(x='', y ='') +
  scale_x_date(breaks = '3 months', date_labels = '%b-%y', expand=c(0,0)) +
  theme(plot.title.position = 'plot', 
        text = element_text(family = 'Times'),
        axis.ticks = element_line(colour = 'grey92'),
        axis.ticks.length =  unit(0.1, 'cm'), 
        panel.grid = element_blank(),
        legend.position = 'none', 
        legend.title = element_blank(), 
        legend.key.size = unit(0.1, 'cm'), #change legend key size
        legend.key.height = unit(0.6, 'cm'), #change legend key height
        legend.key.width = unit(0.4, 'cm'), 
        legend.text = element_text(size = 6),
        plot.title=element_text(family="Times") )

ggsave(here::here('output', 'heatmap_day_cases_days_over_time_weekly_normalised.jpeg'), dpi=800, height=15, width=15, units = 'cm') 
ggsave('/home/emma/Documents/upgrade/index/figure/heatmap_day_cases_days_over_time_weekly_normalised.jpeg', dpi=800, height=15, width=15, units = 'cm') 


