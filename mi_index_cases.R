library(data.table)
library(tidyverse)

# source r scripts----
root_folder <- "\\\\Farr-FS1/Study Data/1516-0116/Research/population_mi/analysis.files/mi/code"
source(paste0(root_folder,"/mi_01_load_functions_data.R"))
source(paste0(root_folder,"/mi_02a_code_lists.R"))

# assign NA is doa the date from doa.1
episode_data <- episode_data %>% 
  mutate(doa = if_else(is.na(doa), doa.1,doa))

episode_data <- as.data.table(episode_data)

# mi incidence cases ----
cols <- c("main_condition","other_condition_1") # restrict index cases to position one and two

mi_data <- episode_data[, index:= Reduce('|',lapply(.SD, grepl, pattern=paste(mi.code.list,collapse="|"))),.SDcols=cols]
mi_data <- subset(mi_data, index==T)

# apply exclusion criteria ----

mi_data_incidence <- lookback(mi_data %>% RenameFunction(), "mi") # apply lookback function over the last 5 years


saveRDS(mi_data_incidence,"\\\\Farr-FS1/Study Data/1516-0116/Research/population_mi/analysis.files/mi/data/mi_data.rds")

