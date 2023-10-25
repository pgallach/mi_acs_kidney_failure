# load libraries

library(tidyverse)
library(lubridate)
library(purrr)
library(mgcv)
library(broom)
library(cowplot)
library(AER)
library(mfp)

#  load data

mi_count <- read.csv(file = 'data/disclosure_29_11_2019/mi/mi_all_incidence.csv')
stroke_count <- read.csv(file = 'data/disclosure_29_11_2019/cvd/stroke_all_incidence.csv')
eur_std_pop <- read.csv(file = 'data/european_standard_population/european_standard_population_by_sex.csv')


##### myocardial infarction #####
# combine mi and europe data

names(eur_std_pop) <- c('age_grp', 'sex', 'pop')

eur_std_pop_mi <- eur_std_pop %>% 
  mutate(age_grp = case_when(
    age_grp %in% c("0-4 years", "5-9 years","10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years") ~ '0-39',
    age_grp %in% c("40-44 years", "45-49 years") ~ '40-49',
    age_grp %in% c("50-54 years", "55-59 years") ~ '50-59',
    age_grp %in% c("60-64 years", "65-69 years") ~ '60-69',
    age_grp %in% c("70-74 years", "75-79 years") ~ '70-79',
    age_grp %in% c("80-84 years", "85-89 years", "90plus years") ~ '80+')) %>% 
  group_by(age_grp,sex) %>% 
  summarise(count_sum = sum(pop)) 

mi_count <- mi_count %>% 
  mutate(sex = case_when(sex==1~'Male',sex==2~'Female'))

# sex stratified incidence rate

inc_poisson_model_gam <- gam(count ~ s(year)+sex+age_grp,offset=log(person_time), data= mi_count,family='quasipoisson')
summary(inc_poisson_model_gam)
augment(inc_poisson_model_gam)

# predict using GAM model

newd <- data.frame(year=rep(1990:2014,each=12), sex = rep(rep(c('Male','Female'),each=6),25), age_grp = rep(rep(c("0-39", "40-49", "50-59", "60-69", "70-79", "80+"),2),25), person_time = mi_count$person_time)
newd <- newd %>% 
  left_join(eur_std_pop_mi)

predict <- predict(inc_poisson_model_gam,newd, se=T)

# derive count by age, sex and year standardised to the ESP
mi_incidence_rate <- newd %>% 
  mutate(fit = predict$fit,
         se = predict$se.fit,
         exp_fit = exp(fit),
         pred_count_std = exp_fit*count_sum)

mi_incidence_rate_all <- mi_incidence_rate %>% 
  select(year,sex,pred_count_std) %>% 
  group_by(year,sex) %>% 
  summarise(pred_count_std = sum(pred_count_std))

##### stroke #####

eur_std_pop_stroke <- eur_std_pop %>% 
  mutate(age_grp = case_when(
    age_grp %in% c("0-4 years", "5-9 years","10-14 years", "15-19 years", "20-24 years", "25-29 years") ~ '0-29',
    age_grp %in% c("30-34 years", "35-39 years") ~ '30-39',
    age_grp %in% c("40-44 years", "45-49 years") ~ '40-49',
    age_grp %in% c("50-54 years", "55-59 years") ~ '50-59',
    age_grp %in% c("60-64 years", "65-69 years") ~ '60-69',
    age_grp %in% c("70-74 years", "75-79 years") ~ '70-79',
    age_grp %in% c("80-84 years", "85-89 years", "90plus years") ~ '80+')) %>% 
  group_by(age_grp,sex) %>% 
  summarise(count_sum = sum(pop)) 

stroke_count <- stroke_count %>% 
  mutate(sex = case_when(sex==1~'Male',sex==2~'Female'))

# sex stratified incidence rate

inc_poisson_model_gam <- gam(count ~ s(year)+sex+age_grp,offset=log(person_time), data= stroke_count,family='quasipoisson')
summary(inc_poisson_model_gam)
augment(inc_poisson_model_gam)

# predict using GAM model

newd <- data.frame(year=rep(1990:2014,each=14), sex = rep(rep(c('Male','Female'),each=7),25), age_grp = rep(rep(c("0-29","30-39" ,"40-49", "50-59", "60-69", "70-79", "80+"),2),25), person_time = stroke_count$person_time)
newd <- newd %>% 
  left_join(eur_std_pop_stroke)

predict <- predict(inc_poisson_model_gam,newd, se=T)

# derive count by age, sex and year standardised to the ESP
stroke_incidence_rate <- newd %>% 
  mutate(fit = predict$fit,
         se = predict$se.fit,
         exp_fit = exp(fit),
         pred_count_std = exp_fit*count_sum)

stroke_incidence_rate_all <- stroke_incidence_rate %>% 
  select(year,sex,pred_count_std) %>% 
  group_by(year,sex) %>% 
  summarise(pred_count_std = sum(pred_count_std))

##### combine mi and stroke age standardised rates #####

incidence_rate_all <- bind_rows('mi' = mi_incidence_rate_all, 'stroke'= stroke_incidence_rate_all,.id = 'condition')

theme_set(theme_classic())
plot.males <- ggplot(incidence_rate_all %>% filter(sex=='Male'), aes(x = year,y = pred_count_std, group=condition))+
  geom_line(aes(color=factor(condition)))+
  scale_color_manual(name="Condition", breaks=c("mi","stroke"), labels =c("Myocardial infarction", "Stroke"), values=c('blue','red'))+
  scale_x_continuous(name = "Year", limits = c(1990,2014), labels = seq(1990,2014,2),breaks = seq(1990,2014,2))+
  scale_y_continuous(name = "Rate per 100,000",limits=c(0,800),labels = seq(0,800,100), breaks = seq(0,800,100), expand=c(0,0))+
  theme(legend.position = 'bottom')

plot.males

plot.females <- ggplot(incidence_rate_all %>% filter(sex=='Female'), aes(x = year,y = pred_count_std, group=condition))+
  geom_line(aes(color=factor(condition)))+
  scale_color_manual(name="Condition", breaks=c("mi","stroke"), labels =c("Myocardial infarction", "Stroke"), values=c('blue','red'))+
  scale_x_continuous(name = "Year", limits = c(1990,2014), labels = seq(1990,2014,2),breaks = seq(1990,2014,2))+
  scale_y_continuous(name = "Rate per 100,000",limits=c(0,800),labels = seq(0,800,100), breaks = seq(0,800,100), expand=c(0,0))+
  theme(legend.position = 'bottom')

plot.females

plot <- plot_grid(plot.males,plot.females,
          labels = c('Males', 'Females'),
          align = 'h',
          label_x = 0.4)


