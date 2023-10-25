# load packages

library(data.table)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyverse)

root_folder <- "//farr-fs1/Study Data/1516-0116/Research/anoop_update_data_2018/data_raw/"

root_folder2 <- "//farr-fs1/Study Data/1516-0116/Research/federica/"

# read RDS files
death_index_prescribing <- readRDS("data_raw/death_index_prescribing.rds")
death_index_comorbidity <- readRDS('data_raw/death_index_comorbidity.rds')

# scrub prescribing data----


# prescribing=readRDS("prescribing_1000.rds")  # smaller file used for testing
diagnosis <- readRDS(paste0(root_folder2,"code2diagnosis_all.rds"))
diagnosis <- diagnosis %>% mutate(diagnosis = str_trim(diagnosis))

death_index_prescribing  <- data.table(death_index_prescribing)

colnames(death_index_prescribing ) <- colnames(death_index_prescribing ) %>%  tolower()

colnames(death_index_prescribing ) <- c("id", "sex", "approved_name", "prescribable_item_name", 
                                        "paid_date", "paid_quantity", "bnf_item_code", "hbres", "age_in_years", 
                                        "age_in_months", "simd2009v2_sc_quintile", "simd2012_sc_quintile")

death_index_prescribing <- death_index_prescribing[,c("hbres","simd2009v2_sc_quintile", "simd2012_sc_quintile"):=NULL]

death_index_prescribing$id <- str_replace(death_index_prescribing$id,"1516-0116/D/","D_")

#match prescribing data with diagnosis data by bnf item code

diagnosis <- diagnosis[which(!duplicated(diagnosis$bnf_item_code)),] # duplicate diagnosis can be safely removed (manually checked)
diagnosis <- data.table(diagnosis)
diagnosis$bnf_item_code <- as.character(diagnosis$bnf_item_code) 
diagnosis$atc_code <- as.character(diagnosis$atc_code) 

setkey(death_index_prescribing, bnf_item_code);
setkey(diagnosis, bnf_item_code);

prescribing_match <- merge(x= death_index_prescribing, y= diagnosis, by ="bnf_item_code", all.x = TRUE, allow.cartesian=TRUE)
prescribing_match[,paid_date:=ymd(paid_date),]


# link prescribing data to death data

# link prescribing data----
death_index_comorbidity <- death_index_comorbidity %>% 
  mutate(date_of_death = ymd(date_of_death))

drugs <- inner_join(death_index_comorbidity, prescribing_match,by="id") %>% 
  filter(date_of_death-paid_date <= 365 & date_of_death-paid_date >= 0 ) 

drugs <- drugs %>% 
  select(id,diagnosis) %>% 
  distinct(id,diagnosis) %>% 
  filter(diagnosis %in% c("Cardiovascular diseases", "Chronic lower respiratory disease", 
                          "Diabetes mellitus", "Thromboembolic disease, atrial fibrillation or valvular heart disease"))

colnames(drugs) <- c("id", "variable")

drugs_wide <- drugs %>%
  distinct(id,variable) %>% 
  mutate(value=TRUE) %>%
  spread(variable,value,fill=FALSE)

death_index_comorbidity <- death_index_comorbidity %>% left_join(drugs_wide,by='id')


# drug code lists by ATC (taken from script mi_06a_adm_discharge_prescribing.R) ----
# aspirin_atc <- Extract_ATC_Code_by_drug('aspirin')
aspirin_atc <- c("B01AC06", "B01AC07")

# clopidogrel_atc <- Extract_ATC_Code_by_drug('clopidogrel')
clopidogrel_atc <- "B01AC04"

# ticagrelor_atc <- Extract_ATC_Code_by_drug('ticagrelor')
ticagrelor_atc <- "B01AC24"

# prasugrel_atc <- Extract_ATC_Code_by_drug('prasugrel')
prasugrel_atc <- 'B01AC22'

# beta_blockers_atc <- Extract_ATC_Code_by_atc_cat('\\bc07')
beta_blockers_atc <- c("C07CA03", "C07DA06", "C07CB03", 
                       "C07AB08", "C07AG02", "C07AB12", "C07AB04", "C07AB03", "C07AB07", 
                       "C07AG01", "C07AB02", "C07AA12", "C07AA02", "C07AA03", "C07AA05", 
                       "C07AA07", "C07FB03", "C07AA06", "C07BA02")

# vit_k_antagonists_atc <- Extract_ATC_Code_by_atc_cat('\\bb01aa')
vit_k_antagonists_atc <- c("B01AA07", "B01AA02", "B01AA03")

# noac_atc <- Extract_ATC_Code_by_atc_cat('\\bb01ae|\\bb01af' )
noac_atc <- c("B01AF03", "B01AE07", "B01AF01","B01AF02")

# acei_arb_atc <- Extract_ATC_Code_by_atc_cat('\\bc09')
acei_arb_atc <- c("C09AA04", "C09BA04", "C09BB04", 
                  "C09AA13", "C09AA08", "C09AA01", "C09BA01", "C09BA02", "C09AA02", 
                  "C09AA09", "C09BA03", "C09AA03", "C09BA06", "C09AA06", "C09AA05", 
                  "C09BB05", "C09AA10", "C09AA16", "C09DA04", "C09DB02", "C09DX03", 
                  "C09CA09", "C09DX04", "C09CA08", "C09CA06", "C09CA04", "C09CA01", 
                  "C09DA01", "C09CA07", "C09CA03", "C09CA02", "C09DA03", "C09DA08", 
                  "C09XA02", "C09DB01")

# statin_atc <- Extract_ATC_Code_by_atc_cat('\\bc10aa')
statin_atc <- c("C10AA07", "C10AA05", "C10AA04", 
                "C10AA03", "C10AA01")

# fibrate_atc <- Extract_ATC_Code_by_atc_cat('\\bc10ab')
fibrate_atc <- c("C10AB08", "C10AB02", "C10AB05", 
                 "C10AB04")

druglist <- c(aspirin_atc,clopidogrel_atc, ticagrelor_atc, prasugrel_atc,beta_blockers_atc,
              vit_k_antagonists_atc, noac_atc, acei_arb_atc, statin_atc, fibrate_atc)
# link pre admission drugs to death list

prescribing_match <- prescribing_match %>% 
  mutate(drug_name = case_when(atc_code %in% aspirin_atc ~ 'aspirin',
                               atc_code %in% clopidogrel_atc ~ 'clopidogrel',
                               atc_code %in% ticagrelor_atc ~ 'ticagrelor',
                               atc_code %in% prasugrel_atc ~ 'prasugrel',
                               atc_code %in% beta_blockers_atc ~ 'bblockers',
                               atc_code %in% vit_k_antagonists_atc ~ 'vka',
                               atc_code %in% noac_atc ~ 'noac',
                               atc_code %in% acei_arb_atc ~ 'ace_arb',
                               atc_code %in% statin_atc ~ 'statin',
                               atc_code %in% fibrate_atc ~ 'fibrate',))

# pre_adm and discharge drugs
pre_admission_drugs <- inner_join(death_index_comorbidity %>% select(id,date_of_death), prescribing_match %>% select(id,paid_date,drug_name), by='id') %>% 
  filter(date_of_death-paid_date <= 365 & date_of_death-paid_date >= 0 ) %>%
  drop_na(drug_name) %>% 
  mutate(drug_name = paste0(drug_name,'_adm')) %>% 
  select (id,drug_name) %>% 
  distinct(.keep_all = T)

pre_admission_drugs <- pre_admission_drugs %>% 
  mutate(value=T) %>% 
  spread(drug_name,value, fill=F)

death_index_comorbidity <- death_index_comorbidity %>% 
  left_join(pre_admission_drugs, by='id')

saveRDS(death_index_comorbidity, 'data_raw/death_index_comorbidity_prescribing.rds') 
