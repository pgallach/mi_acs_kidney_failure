library(tidyverse)
library(data.table)
library(stringr)
# read mi incidence rds
write_root_folder <- "//Farr-FS1/Study Data/1516-0116/Research/population_mi/analysis.files/data_raw/"
mi_data_incidence <- readRDS("data/mi_data_incidence.rds")

#prescribing <- readRDS("af_data_raw/prescribing_with_diagnoses.rds")


# atc code lists and comorbidity

#atc.code.list <- prescribing_with_diagnosis[,unique(atc_code),]
#dput(sort(atc.code.list))

#diagnosis.list <- prescribing_sample[,unique(diagnosis),]


# subset prescribing data for comorbidity and save data ----
# af_comorbidity_list <- c( "Cardiovascular diseases",
#                           "Chronic lower respiratory disease", 
#                           "Thromboembolic disease, atrial fibrillation or valvular heart disease", 
#                           "Diabetes mellitus")
# 
# setkey(prescribing, diagnosis)
# prescribing_af <- prescribing[diagnosis%in% af_comorbidity_list,]
# 
# prescribing_af <- prescribing_af[,c("id","prescribable_item_name","paid_date","atc_code", "diagnosis", "atc_label" )]
# prescribing_af <- prescribing_af[,paid_date:=as.integer(paid_date),]
# 
# saveRDS(prescribing_af,"af_data_raw/prescribing_with_diagnoses_af.rds")


#### read prescribing with sbe RDS from AF prescribing data (the code to derive this is that used above from line 9 to 29)
# prescribing_diagnosis_with_af contains all prescribing data meeting the comorbidity list above (line 18 -21)

prescribing_mi <- readRDS("../af/af_data_raw/prescribing_with_diagnoses_af.rds")
  


# joining for PMH for mi comorbidity list and htn ----

drugs <- inner_join(mi_data_incidence, prescribing_mi,by=c("pid"="id")) %>% 
  filter(date-paid_date <= 365 & date-paid_date >= 0 ) 

drugs <- drugs %>% 
  select(pid,eid,diagnosis) %>% 
  distinct(pid,eid,diagnosis)

colnames(drugs) <- c("pid", "eid", "variable")

drugs_wide <- drugs %>%
  distinct(pid,eid,variable) %>% 
  mutate(value=TRUE) %>%
  spread(variable,value,fill=FALSE)

nrow(drugs_wide) == nrow(unique(drugs_wide,by= "eid"))



# join comorbidity and drug data to sbe data----

mi_data_incidence_comorbidity <- drugs_wide %>% right_join(mi_data_incidence,by = 'eid')

mi_data_incidence_comorbidity <- mi_data_incidence_comorbidity %>% 
  gather(variable, value, c("Cardiovascular diseases", "Chronic lower respiratory disease", 
                            "Diabetes mellitus", "Thromboembolic disease, atrial fibrillation or valvular heart disease"))

mi_data_incidence_comorbidity <- mi_data_incidence_comorbidity %>% 
  mutate(value = case_when(
    is.na(value) == T & date >= 14610 ~ F, # as.integer(as.Date('2010-01-01')) = 14610
    TRUE ~ value
  ))

mi_data_incidence_comorbidity <- mi_data_incidence_comorbidity %>% 
  spread(variable,value) 

# combine other episode level data to mi data ----
episode_data <- readRDS("../af/af_data_raw/episode_data.rds")
episode <- episode_data %>% 
  select(age_years,agecat,eid,location,location.1,doy)

mi_data_incidence_comorbidity <- episode %>% right_join(mi_data_incidence_comorbidity, by = "eid")

# combine patient level data - i.e. gender and ethnicity ----
patient_data <- readRDS(paste0(write_root_folder,'patient_data.rds'))

patient <- patient_data %>% 
  select(id,sex) %>% 
  distinct(id,.keep_all = T)

patient_ethnic <- patient_data %>% 
  select(id,ethnic_group) %>% 
  filter(!is.na(ethnic_group)) %>% 
  distinct(id,.keep_all = T)

patient <- patient_ethnic %>% right_join(patient,by='id')

mi_data_incidence_comorbidity <- patient %>% right_join(mi_data_incidence_comorbidity, by = c("id"="pid.y"))

# save final RDS----
saveRDS(mi_data_incidence_comorbidity, "data/mi_data_incidence_comorbidity.rds")
