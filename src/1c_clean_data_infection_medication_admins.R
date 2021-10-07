library(AMR)
library(tidyverse)
library(readxl)
drugs_given <- readRDS(here::here('data', 'medication_admins.rds'))

infections <- drugs_given %>% 
  filter(drug_therapeutic_class_name=='Infections') %>% 
  filter(!is.na(drug_pharmaceutical_class_name)) 

abx <- infections %>% 
  filter(drug_pharmaceutical_class_name == 'Antibacterial drugs') 
aware <- read_excel("data/raw/WHO-EMP-IAU-2019.11-eng.xlsx", 
                    sheet = "AWaRe Classification 2019", 
                    skip = 3) %>% 
  janitor::clean_names() %>% 
  mutate(atc_code = str_squish(atc_code)) %>% 
  select(-c(x6, x7, x8)) %>% 
  mutate(route = case_when(str_detect(antibiotic,'oral') ~ 'O', 
                           str_detect(antibiotic,'IV') ~ 'P' ))

infection_antibiotics <- infections %>% 
  filter(drug_pharmaceutical_class_name == 'Antibacterial drugs') %>% 
  distinct(drug_simple_generic_name, route_name) %>% 
  mutate(atc_code = ab_atc(drug_simple_generic_name), 
         atc_group1 = ab_atc_group1(drug_simple_generic_name), 
         atc_group2 = ab_atc_group2(drug_simple_generic_name))

infection_antibiotics <- infection_antibiotics %>% 
  mutate(atc_code = ifelse(drug_simple_generic_name=='vancomycin' & route_name %in% c("Oral", "NAS", "PEG",'PEJ', 'Mouth/Throat', 'NG') , 'A07AA09', atc_code),
         act_code = ifelse(drug_simple_generic_name=='co-amoxiclav (amoxicillin and clavulanic acid)', 'J01CR02', atc_code)) %>% 
  mutate(
         atc_code = ifelse(drug_simple_generic_name=='ceftolozane with tazobactam','J01DI54', atc_code), 
         atc_group1 = ifelse(drug_simple_generic_name=='ceftolozane with tazobactam','Other beta-lactam antibacterials', atc_group1),
         atc_group2 = ifelse(drug_simple_generic_name=='ceftolozane with tazobactam','Other cephalosporins and penems', atc_group2),
  )  %>% 
  mutate(
    atc_code = ifelse(drug_simple_generic_name=='cefTAZIDime and avibactam','J01DD52', atc_code), 
    atc_group1 = ifelse(drug_simple_generic_name=='cefTAZIDime and avibactam','Other beta-lactam antibacterials', atc_group1),
    atc_group2 = ifelse(drug_simple_generic_name=='cefTAZIDime and avibactam','Third-generation cephalosporins', atc_group2),
  ) %>% 
  mutate(
    atc_code = ifelse(drug_simple_generic_name=='bedaquiline','J04AK05', atc_code), 
    atc_group1 = ifelse(drug_simple_generic_name=='bedaquiline','Drugs for treatment of tuberculosis', atc_group1),
    atc_group2 = ifelse(drug_simple_generic_name=='bedaquiline','Other drugs for treatment of tuberculosis', atc_group2),
  ) %>% 
  distinct(atc_code,drug_simple_generic_name)
infection_antibiotics_aware <- left_join(infection_antibiotics, aware,by = "atc_code") 

# merge on ATC codes using lookup created above
infections <- left_join(infections, infection_antibiotics_aware, by = c("drug_simple_generic_name")) %>% 
  mutate( ATC_route = case_when( route_name %in% c(NULL) ~ "Implant", 
                                 route_name %in% c("Nebulisation", "Inhalation") ~ "Inhal", 
                                 route_name %in% c("TOP", "EYE", "EYEL", "EYER", "EYEB", 
                                             "EAR", "EARL", "EARR", "EARB") ~ "Instill", 
                                 route_name %in% c("NASAL", "NOST", "NOSTL", "NOSTR", "NOSTB") ~ "N", 
                                 route_name %in% c("Oral", "NAS", "PEG",'PEJ', 'Mouth/Throat', 'NG') ~ "O", 
                                 route_name %in% c("Intravenous",'Intramuscular', 'Line Lock', 'LINE LOCK (RED LUMEN)', 'LINE LOCK (WHITE LUMEN)', 'LINE LOCK (BLUE LUMEN)',
                                             'LINE LOCK (PICC)', 'LINE LOCK (PORT)', 'Intrathecal','Intraventricular',
                                             'Intravesical', "IVB", "IVI", "IMI", "IT", "IVT") ~ "P", 
                                 route_name %in% c("PR") ~ "R", 
                                 route_name %in% c("BUCC", "SB", "OROM", "SUBL") ~ "SL", 
                                 route_name %in% c("SC", "ID") ~ "TD", 
                                 route_name %in% c("PV") ~ "V", 
                                TRUE ~ NA_character_) ) %>%
  filter((ATC_route == route | is.na(route) | is.na(route_name) | is.na(ATC_route) )) 


count_table_post_cleaning <-  infections %>% 
  filter(drug_pharmaceutical_class_name == 'Antibacterial drugs') %>% 
  count(atc_code, drug_simple_generic_name, category, sort = TRUE)

saveRDS(infections, here::here('data', 'infection_drugs_given.rds'))
