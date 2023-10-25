library(tidyverse)
library(data.table)
library(lubridate)
library(tableone)
library(stringr)
library(RColorBrewer)
library(broom)
library(mgcv)
library(cowplot)
library(faraway)
library(mfp)
# load data

mi <- readRDS("data/mi_final.rds")

## trial of gam

mi <- mi %>% 
  filter(year %in% 1990:2014)
mi_agg <- mi %>% 
  group_by(year, sex, age_years, cerebrovasc, hf, simd_2009) %>% 
  summarise(x = sum(death.3year),
            n = sum(death.3year %in% 0:1)) %>% 
  ungroup() %>% 
  na.omit()

glm_agg <- glm(cbind(x, n-x) ~ age_years + sex + cerebrovasc +hf+simd_2009, data=mi_agg, family = "binomial")

glm <- glm(death.3year ~  year + age_years + sex + cerebrovasc +hf+simd_2009, data=mi, family = "binomial")

mfp1     <- mfp(cbind(x, n-x)     ~  fp(year) + fp(age_years) + sex + cerebrovasc +hf+simd_2009,data=mi_agg, family = "binomial")

# saveRDS(mfp1, "data/temp_mfp1.Rds")
myform <- mfp1$formula
rhs <- as.character(myform)[3]


myform2 <- formula(paste0("death.3year   ~ ", rhs))
glm2 <- update(glm, formula = myform2)
glm_agg <- update(glm_agg, formula = mfp1$formula)

summary(glm2)


mi_agg$resid <- glm_agg$residuals
mi_agg <- mi_agg %>% 
  group_by(year) %>% 
  summarise(resid = sum(resid)) %>% 
  ungroup() %>% 
  arrange(year)

mi_agg_each <- by(mi_agg, list(mi_agg$sex,mi_agg$cerebrovasc),
                      function(x)  x$resid)
stroke_agg_each <- map(stroke_agg_each, ~ tibble(x = .x))
stroke_agg_each <- bind_rows(stroke_agg_each, .id = "grp")
tapply(stroke_agg_each$x, stroke_agg_each$x, acf)

lmtest::lrtest(glm, glm2)

# females plot
newd <- data.frame(year=1990:2014,age_years=65,sex=2,cerebrovasc=0,hf=0,simd_2009=3)
predict <- predict(glm2,newd,se=T, type='link')
plot <- data.frame(year=1990:2014, predict$fit,predict$se.fit)
plot <- plot %>% 
  mutate(prob = ilogit(predict.fit),
         ll=ilogit(predict.fit-1.96*predict.se.fit),
         ul=ilogit(predict.fit+1.96*predict.se.fit))

plot <- plot %>%
  gather(estimate, value, -c(year,predict.fit,predict.se.fit))

females.death.plot <- ggplot(plot, aes(x=year, y=value*100,group=estimate))+
  geom_line(aes(color=estimate, linetype=estimate), size=0.4)+
  scale_color_manual(values=c('blue','red','blue'))+
  scale_linetype_manual(values=c('dashed','solid','dashed'))+
  scale_y_continuous(name = 'Predicted probability of death (%)',limits = c(0,100),breaks = seq(0,100,10))+
  theme(legend.position = 'none')
females.death.plot 

female.plot <- plot

# males plot
newd <- data.frame(year=1990:2014,age_years=65,sex=1,cerebrovasc=0,mi=0,hf=0,simd_2009=3)
predict <- predict(glm2,newd,se=T, type='link')
plot <- data.frame(year=1990:2014, predict$fit,predict$se.fit)
plot <- plot %>% 
  mutate(prob = ilogit(predict.fit),
         ll=ilogit(predict.fit-1.96*predict.se.fit),
         ul=ilogit(predict.fit+1.96*predict.se.fit))

plot <- plot %>%
  gather(estimate, value, -c(year,predict.fit,predict.se.fit))

males.death.plot <- ggplot(plot, aes(x=year, y=value*100,group=estimate))+
  geom_line(aes(color=estimate, linetype=estimate), size=0.4)+
  scale_color_manual(values=c('blue','red','blue'))+
  scale_linetype_manual(values=c('dashed','solid','dashed'))+
  scale_y_continuous(name = 'Predicted probability of death (%)',limits = c(0,100),breaks = seq(0,100,10))+
  theme(legend.position = 'none')
males.death.plot

male.plot <- plot

death_plot <- plot_grid(males.death.plot,females.death.plot,labels = c('Males', 'Females'),align = 'h', hjust = c(-1.4, -1))
death_plot

ggsave("output/figure_pred_prob.pdf",death_plot,width = 14,height=10)

# extract ped prob table

female.plot<- female.plot %>% 
  select(year,estimate,value) %>% 
  mutate(value = format(round(value*100,2),nsmall=2)) %>% 
  spread(key = estimate, value = value) %>% 
  select(year, prob, ll, ul)

male.plot<- male.plot %>% 
  select(year,estimate,value) %>% 
  mutate(value = format(round(value*100,2),nsmall=2)) %>% 
  spread(key = estimate, value = value) %>% 
  select(year, prob, ll, ul)

pred.prob.table <- bind_rows('females'= female.plot, 'males'=male.plot,.id = 'sex') %>% 
  select(year, sex, prob, ll, ul) 

write.csv(pred.prob.table, 'output/pred.prob.csv')

# extract variance covariance matrix
extract_vcov <- function(x){
  vcov_out <- vcov (x)
  vcov_out[lower.tri(vcov_out)] <- NA
  vcov_out <- as.data.frame(vcov_out)
  vcov_out$rows <- rownames(vcov_out)
  vcov_out <- vcov_out %>% 
    gather("cols", "value", -rows)
  vcov_out <- vcov_out %>% 
    filter(!is.na(value))
}


vcov_outcome <- extract_vcov(glm_agg) 

write.csv(vcov_outcome, 'output/vcov_outcome.csv')

