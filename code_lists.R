# library 

library(tidyverse)

read_folder <- '//Farr-FS1/Study Data/1516-0116/Research/population_mi/analysis.files/af/af_data_raw/'
# Code lists icd 9; icd10; opcs----

# select ICD  / opcs codes for af, mi, stroke, bleeding , mitral sx , coronary revasc
cols <- c("main_condition", 
          "other_condition_1", "other_condition_2", "other_condition_3", 
          "other_condition_4", "other_condition_5") 

cols2 <- c("main_operation_a", "main_operation_b", "other_operation_1a", 
           "other_operation_1b", "other_operation_2a", "other_operation_2b")

# code.list <- episode_data %>%
#   select(main_condition, other_condition_1, other_condition_2, other_condition_3, other_condition_4, other_condition_5) %>%
#   gather(key = "position",value="icd") %>%
#   distinct()
# 
# code.list.proc <- episode_data %>%
#   select(main_operation_a, main_operation_b, other_operation_1a, other_operation_1b, other_operation_2a, other_operation_2b) %>%
#   gather(key = "position",value="opcs") %>%
#   distinct()


# write.csv(code.list,paste0(read_folder,"code.list.csv"))
# write.csv(code.list.proc,paste0(read_folder,"code.list.proc.csv"))

code.list <- read.csv(paste0(read_folder,"code.list.csv"),stringsAsFactors = F)
code.list.proc <- read.csv(paste0(read_folder,"code.list.proc.csv"),stringsAsFactors = F)

#sbe ----

sbe.code.list.icd9 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^421|^4249")) %>%
  distinct() %>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(as.character(sbe.code.list.icd9$icd)),collapse = "|") # hard code icd code

sbe.code.list.icd9 <- c("\\b4210\\b", "\\b4249\\b", "\\b4219\\b")

sbe.code.list.icd10 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^I33|^I38|^I39")) %>%
  distinct() %>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(sbe.code.list.icd10$icd),collapse = "|") # hard code icd code

sbe.code.list.icd10 <- c("\\bI330\\b", "\\bI38X\\b", "\\bI339\\b", "\\bI330 D\\b", "\\bI398 A\\b", 
                         "\\bI391 A\\b", "\\bI393 A\\b", "\\bI394 A\\b", "\\bI390 A\\b", 
                         "\\bI392 A\\b")

sbe.code.list <- c(sbe.code.list.icd9,sbe.code.list.icd10)

sbe.code.list <- sbe.code.list %>% sort() %>% dput()


# mi ----


mi.code.list.icd9 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^410")) %>%
  distinct() %>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(mi.code.list.icd9$icd),collapse = "|") # hard code icd code

mi.code.list.icd9 <- c("\\b4109\\b", "\\b41090\\b")

mi.code.list.icd10 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^I21|^I22")) %>%
  distinct() %>% 
  mutate(icd=paste0('\\b',icd,'\\b'))


paste0(dput(mi.code.list.icd10$icd),collapse = "|") # hard code icd code

mi.code.list.icd10 <- c("\\bI219\\b", "\\bI210\\b", "\\bI2190\\b", "\\bI214\\b", "\\bI211\\b", 
                        "\\bI2111\\b", "\\bI2140\\b", "\\bI228\\b", "\\bI229\\b", "\\bI2101\\b", 
                        "\\bI212\\b", "\\bI2121\\b", "\\bI2130\\b", "\\bI2110\\b", "\\bI221\\b", 
                        "\\bI2199\\b", "\\bI2290\\b", "\\bI2120\\b", "\\bI2191\\b", "\\bI2100\\b", 
                        "\\bI220\\b", "\\bI2210\\b", "\\bI2201\\b", "\\bI213\\b", "\\bI2139\\b", 
                        "\\bI2131\\b", "\\bI2211\\b", "\\bI2119\\b", "\\bI2109\\b", "\\bI2129\\b", 
                        "\\bI2209\\b", "\\bI2280\\b", "\\bI2200\\b", "\\bI2149\\b", "\\bI2291\\b", 
                        "\\bI2299\\b", "\\bI2281\\b", "\\bI2289\\b", "\\bI2141\\b", "\\bI2219\\b")

mi.code.list <- c(mi.code.list.icd9,mi.code.list.icd10)

mi.code.list <- mi.code.list %>% sort() %>% dput()

# cerebro_vasc_dis ----
paste('^','43',0:8,sep="")
cerebro_vasc_dis.code.list.icd9 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,'^430|^431|^432|^433|^434|^435|^436|^437|^438')) %>%
  distinct()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(cerebro_vasc_dis.code.list.icd9$icd),collapse = "|") # hard code icd code
cerebro_vasc_dis.code.list.icd9 <- c("\\b4369\\b", "\\b4349\\b", "\\b4379\\b", "\\b4339\\b", "\\b4341\\b", 
                                     "\\b4359\\b", "\\b4331\\b", "\\b4319\\b", "\\b4309\\b", "\\b4329\\b", 
                                     "\\b4373\\b", "\\b4389\\b", "\\b4340\\b", "\\b4321\\b", "\\b4370\\b", 
                                     "\\b4378\\b", "\\b4371\\b", "\\b4330\\b", "\\b4338\\b", "\\b4320\\b", 
                                     "\\b4333\\b", "\\b4376\\b", "\\b4332\\b", "\\b4375\\b", "\\b4372\\b", 
                                     "\\b4374\\b", "\\b43690\\b")

paste0('^','I6',0:9,sep="", collapse='|')
cerebro_vasc_dis.code.list.icd10 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^I60|^I61|^I62|^I63|^I64|^I65|^I66|^I67|^I68|^I69")) %>%
  distinct() %>%
  arrange()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(cerebro_vasc_dis.code.list.icd10$icd),collapse = "|") # hard code icd code

cerebro_vasc_dis.code.list.icd10 <- c("\\bI638\\b", "\\bI639\\b", "\\bI64X\\b", "\\bI632\\b", "\\bI652\\b", 
                                      "\\bI619\\b", "\\bI601\\b", "\\bI609\\b", "\\bI679\\b", "\\bI639 D\\b", 
                                      "\\bI678\\b", "\\bI620\\b", "\\bI629\\b", "\\bI608\\b", "\\bI600\\b", 
                                      "\\bI671\\b", "\\bI635\\b", "\\bI664\\b", "\\bI694\\b", "\\bI612\\b", 
                                      "\\bI607\\b", "\\bI610\\b", "\\bI633\\b", "\\bI602\\b", "\\bI672\\b", 
                                      "\\bI679 D\\b", "\\bI604\\b", "\\bI662 D\\b", "\\bI615\\b", "\\bI638 D\\b", 
                                      "\\bI616\\b", "\\bI613\\b", "\\bI653\\b", "\\bI614\\b", "\\bI670\\b", 
                                      "\\bI603\\b", "\\bI674\\b", "\\bI636\\b", "\\bI618\\b", "\\bI669\\b", 
                                      "\\bI660\\b", "\\bI621\\b", "\\bI611\\b", "\\bI606\\b", "\\bI634\\b", 
                                      "\\bI661 D\\b", "\\bI631\\b", "\\bI650\\b", "\\bI630\\b", "\\bI605\\b", 
                                      "\\bI677\\b", "\\bI660 D\\b", "\\bI658\\b", "\\bI680 A\\b", "\\bI698\\b", 
                                      "\\bI651\\b", "\\bI64X D\\b", "\\bI661\\b", "\\bI632 D\\b", "\\bI635 D\\b", 
                                      "\\bI663 D\\b", "\\bI668\\b", "\\bI659\\b", "\\bI677 D\\b", "\\bI676\\b", 
                                      "\\bI693\\b", "\\bI652 D\\b", "\\bI663\\b", "\\bI650 D\\b", "\\bI678 D\\b", 
                                      "\\bI673\\b", "\\bI662\\b", "\\bI675\\b", "\\bI630 D\\b", "\\bI618 D\\b", 
                                      "\\bI690\\b", "\\bI609 D\\b", "\\bI672 D\\b", "\\bI631 D\\b", 
                                      "\\bI691\\b", "\\bI692\\b", "\\bI659 D\\b", "\\bI664 D\\b", "\\bI651 D\\b", 
                                      "\\bI615 D\\b", "\\bI676 D\\b", "\\bI671 D\\b", "\\bI63\\b", 
                                      "\\bI633 D\\b", "\\bI668 D\\b", "\\bI682 A\\b", "\\bI688 A\\b", 
                                      "\\bI613 D\\b", "\\bI674 D\\b", "\\bI694 A\\b", "\\bI607 D\\b", 
                                      "\\bI681 A\\b")

cerebro_vasc_dis.code.list <- c(cerebro_vasc_dis.code.list.icd9,cerebro_vasc_dis.code.list.icd10)

cerebro_vasc_dis.code.list <- cerebro_vasc_dis.code.list %>% sort() %>% dput()


# stroke - although mappting from icd 9 to icd 10 not goog the stroke code list will need to include both for idenifying the index cases with appropriate lookback  ---- 
# cases however will be filtered from 1996 and above 
# advice sought from ISD and follwoing mapping used (not perfect) from ICD9 to ICD 10
# I60 maps to 430 ; I61 maps to 431; I63:66 maps to 433 and 434 and I64 maps to 436
# see email from coders on 31stOct 2019

paste('^','43',c(0,1,3,4,6),sep="", collapse = '|')
stroke.code.list.icd9 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^430|^431|^433|^434|^436")) %>%
  distinct() %>%
  arrange()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(stroke.code.list.icd9$icd),collapse = "|") # hard code icd code

stroke.code.list.icd9 <- c("\\b4369\\b", "\\b4349\\b", "\\b4339\\b", "\\b4341\\b", "\\b4331\\b", 
                           "\\b4319\\b", "\\b4309\\b", "\\b4340\\b", "\\b4330\\b", "\\b4338\\b", 
                           "\\b4333\\b", "\\b4332\\b", "\\b43690\\b")



paste('^','I6',c(0,1,3:6),sep="", collapse = '|')
stroke.code.list.icd10 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^I60|^I61|^I63|^I64|^I65|^I66")) %>%
  distinct() %>%
  arrange()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(stroke.code.list.icd10$icd),collapse = "|") # hard code icd code

stroke.code.list.icd10 <- c("\\bI638\\b", "\\bI639\\b", "\\bI64X\\b", "\\bI632\\b", "\\bI652\\b", 
                            "\\bI619\\b", "\\bI601\\b", "\\bI609\\b", "\\bI639 D\\b", "\\bI608\\b", 
                            "\\bI600\\b", "\\bI635\\b", "\\bI664\\b", "\\bI612\\b", "\\bI607\\b", 
                            "\\bI610\\b", "\\bI633\\b", "\\bI602\\b", "\\bI604\\b", "\\bI662 D\\b", 
                            "\\bI615\\b", "\\bI638 D\\b", "\\bI616\\b", "\\bI613\\b", "\\bI653\\b", 
                            "\\bI614\\b", "\\bI603\\b", "\\bI636\\b", "\\bI618\\b", "\\bI669\\b", 
                            "\\bI660\\b", "\\bI611\\b", "\\bI606\\b", "\\bI634\\b", "\\bI661 D\\b", 
                            "\\bI631\\b", "\\bI650\\b", "\\bI630\\b", "\\bI605\\b", "\\bI660 D\\b", 
                            "\\bI658\\b", "\\bI651\\b", "\\bI64X D\\b", "\\bI661\\b", "\\bI632 D\\b", 
                            "\\bI635 D\\b", "\\bI663 D\\b", "\\bI668\\b", "\\bI659\\b", "\\bI652 D\\b", 
                            "\\bI663\\b", "\\bI650 D\\b", "\\bI662\\b", "\\bI630 D\\b", "\\bI618 D\\b", 
                            "\\bI609 D\\b", "\\bI631 D\\b", "\\bI659 D\\b", "\\bI664 D\\b", 
                            "\\bI651 D\\b", "\\bI615 D\\b", "\\bI63\\b", "\\bI633 D\\b", 
                            "\\bI668 D\\b", "\\bI613 D\\b", "\\bI607 D\\b")

stroke.code.list <- c(stroke.code.list.icd9,stroke.code.list.icd10)

stroke.code.list <- stroke.code.list %>% sort() %>% dput()


# heart failure ----
hf.code.list.icd9 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^428|^402|^4255")) %>%
  distinct()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(hf.code.list.icd9$icd),collapse = "|") # hard code icd code
hf.code.list.icd9 <- c("\\b4281\\b", "\\b4280\\b", "\\b4289\\b", "\\b4029\\b", "\\b4255\\b", 
                       "\\b4021\\b", "\\b4020\\b")


hf.code.list.icd10 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^I110|^I130|^I132|^I426|^I50")) %>%
  distinct()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(hf.code.list.icd10$icd),collapse = "|") # hard code icd code
hf.code.list.icd10 <- c("\\bI509\\b", "\\bI500\\b", "\\bI500 D\\b", "\\bI501\\b", "\\bI5009\\b", 
                        "\\bI5011\\b", "\\bI5010\\b", "\\bI5019\\b", "\\bI5099\\b", "\\bI5000\\b", 
                        "\\bI501 D\\b", "\\bI5090\\b", "\\bI426\\b", "\\bI5001\\b", "\\bI5009D\\b", 
                        "\\bI110\\b", "\\bI5010D\\b", "\\bI5091\\b", "\\bI5091D\\b", 
                        "\\bI1329\\b", "\\bI5001D\\b", "\\bI509 D\\b", "\\bI5090D\\b", 
                        "\\bI130\\b", "\\bI132\\b", "\\bI5019D\\b", "\\bI5000D\\b", "\\bI1100\\b", 
                        "\\bI1101\\b", "\\bI5099D\\b", "\\bI1109\\b", "\\bI1321\\b", 
                        "\\bI1300\\b", "\\bI5011D\\b", "\\bI1320\\b", "\\bI1309\\b")

hf.code.list <- c(hf.code.list.icd9,hf.code.list.icd10)

hf.code.list <- hf.code.list %>% sort() %>% dput()

# haemodialysis ----
haemodialysis.code.list.icd9 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^V451|^V56")) %>%
  distinct()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(haemodialysis.code.list.icd9$icd),collapse = "|") # hard code icd code

haemodialysis.code.list.icd9 <- c("\\bV568\\b", "\\bV560\\b", "\\bV451\\b", "\\bV565\\b")

haemodialysis.code.list.icd10 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^Z992|^Z49")) %>%
  distinct()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(haemodialysis.code.list.icd10$icd),collapse = "|") # hard code icd code

haemodialysis.code.list.icd10 <- c("\\bZ490\\b", "\\bZ492\\b", "\\bZ992\\b", "\\bZ491\\b")

haemodialysis.code.list <- c(haemodialysis.code.list.icd9,haemodialysis.code.list.icd10)

haemodialysis.code.list <- haemodialysis.code.list%>% sort() %>% dput()

# valve surgery ----

mitral.sx.code.list.opcs4 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^K25")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(mitral.sx.code.list.opcs4$opcs),collapse = "|") # hard code icd code
mitral.sx.code.list.opcs4 <- c("\\bK252\\b", "\\bK255\\b", "\\bK253\\b", "\\bK254\\b", "\\bK258\\b", 
                               "\\bK259\\b", "\\bK251\\b")

mitral.sx.code.list.opcs3 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^3131|^3141")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(mitral.sx.code.list.opcs3$opcs),collapse = "|") # hard code icd code

mitral.sx.code.list.opcs3 <- c("\\b3131\\b", "\\b3141\\b")

mitral.sx.code.list <- c(mitral.sx.code.list.opcs3,mitral.sx.code.list.opcs4)

aortic.sx.code.list.opcs4 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^K26")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(aortic.sx.code.list.opcs4$opcs),collapse = "|") # hard code icd code
aortic.sx.code.list.opcs4 <- c("\\bK262\\b", "\\bK263\\b", "\\bK264\\b", "\\bK261\\b", "\\bK265\\b", 
                               "\\bK269\\b", "\\bK268\\b")


aortic.sx.code.list.opcs3 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^3132|^3142")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(aortic.sx.code.list.opcs3$opcs),collapse = "|") # hard code icd code

aortic.sx.code.list.opcs3 <- c("\\b3142\\b", "\\b3132\\b")

aortic.sx.code.list <- c(aortic.sx.code.list.opcs3,aortic.sx.code.list.opcs4)

tricuspid.sx.code.list.opcs4 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^K27")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(tricuspid.sx.code.list.opcs4$opcs),collapse = "|") # hard code icd code

tricuspid.sx.code.list.opcs4 <- c("\\bK274\\b", "\\bK276\\b", "\\bK272\\b", "\\bK271\\b", "\\bK273\\b", 
                                  "\\bK278\\b", "\\bK279\\b", "\\bK275\\b")


tricuspid.sx.code.list.opcs3 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^3133|^3143")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(tricuspid.sx.code.list.opcs3$opcs),collapse = "|") # hard code icd code
tricuspid.sx.code.list.opcs3 <- c("\\b3143\\b", "\\b3133\\b")

tricuspid.sx.code.list <- c(tricuspid.sx.code.list.opcs3,tricuspid.sx.code.list.opcs4)

pulmonary.sx.code.list <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^K28")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(pulmonary.sx.code.list$opcs),collapse = "|") # hard code icd code

pulmonary.sx.code.list <- c("\\bK281\\b", "\\bK282\\b", "\\bK283\\b", "\\bK285\\b", "\\bK284\\b", 
                            "\\bK288\\b", "\\bK289\\b")

valve.sx.code.list <- c(mitral.sx.code.list,aortic.sx.code.list,tricuspid.sx.code.list,pulmonary.sx.code.list)

# coronary revasc ----

coronary.revasc.code.list.opcs4 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^K40|^K41|^K42|^K43|^K44|^K45|^K46|^K49|^K75")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(coronary.revasc.code.list.opcs4$opcs),collapse = "|") # hard code icd code

coronary.revasc.code.list.opcs4 <- c("\\bK491\\b", "\\bK752\\b", "\\bK453\\b", "\\bK751\\b", "\\bK403\\b", 
                                     "\\bK753\\b", "\\bK442\\b", "\\bK454\\b", "\\bK401\\b", "\\bK402\\b", 
                                     "\\bK449\\b", "\\bK494\\b", "\\bK451\\b", "\\bK498\\b", "\\bK411\\b", 
                                     "\\bK492\\b", "\\bK404\\b", "\\bK499\\b", "\\bK754\\b", "\\bK433\\b", 
                                     "\\bK758\\b", "\\bK759\\b", "\\bK452\\b", "\\bK455\\b", "\\bK412\\b", 
                                     "\\bK459\\b", "\\bK463\\b", "\\bK493\\b", "\\bK432\\b", "\\bK431\\b", 
                                     "\\bK441\\b", "\\bK409\\b", "\\bK413\\b", "\\bK422\\b", "\\bK469\\b", 
                                     "\\bK414\\b", "\\bK421\\b", "\\bK461\\b", "\\bK439\\b", "\\bK448\\b", 
                                     "\\bK434\\b", "\\bK408\\b", "\\bK458\\b", "\\bK423\\b", "\\bK456\\b", 
                                     "\\bK419\\b", "\\bK468\\b", "\\bK418\\b", "\\bK462\\b", "\\bK438\\b", 
                                     "\\bK464\\b", "\\bK429\\b", "\\bK465\\b")
coronary.revasc.code.list.opcs4 <- coronary.revasc.code.list.opcs4 %>% sort() %>% dput()

coronary.revasc.code.list.opcs3 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^3042|^3043|^3048")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(coronary.revasc.code.list.opcs3$opcs),collapse = "|") # hard code icd code

coronary.revasc.code.list.opcs3 <- c("\\b3043\\b", "\\b3042\\b")

coronary.revasc.code.list <- c(coronary.revasc.code.list.opcs3,coronary.revasc.code.list.opcs4)

# bleeding ----

bleed.code.list.icd9 <- readClipboard() # taken from column of data in file bleedingv0.3
dput(paste0(noquote(paste0("^",bleed.code.list.icd9)),collapse = "|")) # hard code

bleed.code.list.icd9 <- code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^5307|^5693|^5780|^5781|^5789|^6207|^6238|^6239|^6341|^6341|^6351|^6351|^6361|^6361|^6371|^6371|^6381|^6381|^6391|^6408|^6409|^641|^6413|^6418|^6419|^6657|^6645|^6743|^7863|^7863|^7863|^9981|^5310|^5314|^5320|^5324|^5330|^5334|^5340|^5344|^5350|^5354|^641|^6413|^6418|^6419|^6660|^6661|^6662|^7703|^7703|^7703|^7703|^7848|^7863|^7863|^7863|^4560|^5312|^5316|^5322|^5326|^5332|^5336|^5342|^5346|^3628|^3792|^430|^432|^438|^438|^8520|^8520|^8520|^4230")) %>%
  distinct()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(bleed.code.list.icd9$icd),collapse = "|") # hard code icd code

bleed.code.list.icd9 <- c("\\b5781\\b", "\\b7863\\b", "\\b5789\\b", "\\b5693\\b", "\\b5314\\b", 
                          "\\b5780\\b", "\\b4309\\b", "\\b4329\\b", "\\b5324\\b", "\\b4389\\b", 
                          "\\b5307\\b", "\\b6409\\b", "\\b4321\\b", "\\b6662\\b", "\\b8520\\b", 
                          "\\b5350\\b", "\\b5316\\b", "\\b9981\\b", "\\b5326\\b", "\\b5310\\b", 
                          "\\b5334\\b", "\\b4230\\b", "\\b4560\\b", "\\b6391\\b", "\\b5320\\b", 
                          "\\b5330\\b", "\\b5354\\b", "\\b5344\\b", "\\b4320\\b", "\\b6351\\b", 
                          "\\b5322\\b", "\\b5336\\b", "\\b3628\\b", "\\b3792\\b", "\\b6341\\b", 
                          "\\b6743\\b", "\\b6371\\b", "\\b6238\\b", "\\b99818\\b", "\\b5342\\b", 
                          "\\b5340\\b", "\\b5312\\b", "\\b6239\\b", "\\b7848\\b", "\\b5346\\b", 
                          "\\b6660\\b", "\\b6661\\b", "\\b6419\\b", "\\b99819\\b", "\\b78630\\b", 
                          "\\b6408\\b", "\\b5332\\b", "\\b6207\\b", "\\b6413\\b", "\\b6381\\b", 
                          "\\b6412\\b", "\\b6410\\b", "\\b99810\\b", "\\b7703\\b", "\\b77039\\b", 
                          "\\b77031\\b")

#bleed.code.list.icd10 <- readClipboard() # taken from column of data in file bleedingv0.3
#dput(paste0(noquote(paste0("^",bleed.code.list.icd10)),collapse = "|")) # hard code

bleed.code.list.icd10 <- code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^K226|^K625|^K920|^K921|^K922|^N837|^N938|^N939|^O031|^O036|^O041|^O046|^O051|^O056|^O061|^O066|^O071|^O076|^O081|^O208|^O209|^O46|^O460|^O468|^O469|^O717|^O717|^O902|^R042|^R048|^R049|^T810|^K250|^K254|^K260|^K264|^K270|^K274|^K280|^K284|^K290|^K291|^O67|^O670|^O678|^O679|^O720|^O721|^O722|^P260|^P261|^P268|^P269|^R041|^R042|^R048|^R049|^I850|^K252|^K256|^K262|^K266|^K272|^K276|^K282|^K286|^H356|^H431|^H450|^I60|^I62|^I690|^I692|^S064|^S065|^S066|^I230|^I312")) %>%
  distinct()%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(bleed.code.list.icd10$icd),collapse = "|") # hard code icd code

bleed.code.list.icd10 <- c("\\bH431\\b", "\\bK920\\b", "\\bK264\\b", "\\bR042\\b", "\\bK922\\b", 
                           "\\bT810\\b", "\\bI601\\b", "\\bI609\\b", "\\bK625\\b", "\\bK226\\b", 
                           "\\bK921\\b", "\\bN938\\b", "\\bI620\\b", "\\bK260\\b", "\\bI629\\b", 
                           "\\bI608\\b", "\\bI600\\b", "\\bK254\\b", "\\bO041\\b", "\\bN939\\b", 
                           "\\bS066\\b", "\\bS0650\\b", "\\bO722\\b", "\\bK250\\b", "\\bI607\\b", 
                           "\\bS065\\b", "\\bI602\\b", "\\bK290\\b", "\\bI850\\b", "\\bI604\\b", 
                           "\\bK266\\b", "\\bS0660\\b", "\\bI603\\b", "\\bK291\\b", "\\bH450 A\\b", 
                           "\\bI621\\b", "\\bO209\\b", "\\bS064\\b", "\\bO031\\b", "\\bI606\\b", 
                           "\\bS0640\\b", "\\bO208\\b", "\\bI230\\b", "\\bI605\\b", "\\bK274\\b", 
                           "\\bK262\\b", "\\bR048\\b", "\\bK284\\b", "\\bK276\\b", "\\bS0641\\b", 
                           "\\bK256\\b", "\\bS0651\\b", "\\bR049\\b", "\\bH356\\b", "\\bO071\\b", 
                           "\\bO081\\b", "\\bO036\\b", "\\bO721\\b", "\\bK252\\b", "\\bR041\\b", 
                           "\\bO046\\b", "\\bK270\\b", "\\bK286\\b", "\\bS0661\\b", "\\bI312\\b", 
                           "\\bO720\\b", "\\bN837\\b", "\\bO061\\b", "\\bK280\\b", "\\bO469\\b", 
                           "\\bO902\\b", "\\bI690\\b", "\\bO051\\b", "\\bK272\\b", "\\bI609 D\\b", 
                           "\\bO076\\b", "\\bK282\\b", "\\bI692\\b", "\\bP261\\b", "\\bO066\\b", 
                           "\\bO460\\b", "\\bP269\\b", "\\bO717\\b", "\\bI607 D\\b")

bleed.code.list.icd <- c(bleed.code.list.icd9,bleed.code.list.icd10)
bleed.code.list.icd <- bleed.code.list.icd %>% sort() %>% dput


#bleed.code.list.opcs3 <- readClipboard() # taken from column of data in file barc_opcs codelist2

bleed.code.list.opcs3 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^221|^221|^221|^2596|^1908|^2392|^2392|^732|^721|^053|^053|^053|^0172|^0533|^0531")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(bleed.code.list.opcs3$opcs),collapse = "|") # hard code icd code

bleed.code.list.opcs3 <- c("\\b0531\\b", "\\b0539\\b", "\\b0532\\b", "\\b7329\\b", "\\b2219\\b", 
                           "\\b0533\\b", "\\b0172\\b")

#bleed.code.list.opcs4 <- readClipboard()
#dput(paste0(noquote(paste0("^",bleed.code.list.opcs4)),collapse = "|")) # hard code

bleed.code.list.opcs4 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^E05|^E058|^E059|^F162|^D041|^E203|^F365|^G523|^H212|^H531|^K681|^P093|^P271|^T301|^Y221|^T032|^Y321|^V032|^A052|^A053|^A054|^A103|^A401|^A411")) %>%
  distinct()%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(bleed.code.list.opcs4$opcs),collapse = "|") # hard code icd code

bleed.code.list.opcs4<- c("\\bA411\\b", "\\bE051\\b", "\\bA054\\b", "\\bA053\\b", "\\bE052\\b", 
                          "\\bT032\\b", "\\bG523\\b", "\\bT301\\b", "\\bF365\\b", "\\bH531\\b", 
                          "\\bA401\\b", "\\bE059\\b", "\\bD041\\b", "\\bF162\\b", "\\bH212\\b", 
                          "\\bE058\\b", "\\bE053\\b", "\\bA103\\b", "\\bK681\\b", "\\bP271\\b", 
                          "\\bE054\\b", "\\bV032\\b", "\\bA052\\b", "\\bP093\\b", "\\bY221\\b", 
                          "\\bY321\\b")

bleed.code.list.opcs <- c(bleed.code.list.opcs3,bleed.code.list.opcs4)

# drug abuse ----

drug_abuse.code.list.icd9 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^3040|^3042|^3044|^3045|^3046|^3047|^3048|^3049|^3053|^3055|^3056|^3057|^3059")) %>%
  distinct() %>%
  arrange(icd)%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(as.character(drug_abuse.code.list.icd9$icd)),collapse = "|") # hard code icd code

drug_abuse.code.list.icd9 <- c("\\b3040\\b", "\\b3044\\b", "\\b3046\\b", "\\b3047\\b", "\\b3048\\b", 
                               "\\b3049\\b", "\\b3053\\b", "\\b3055\\b", "\\b3056\\b", "\\b3057\\b", 
                               "\\b3059\\b")

drug_abuse.code.list.icd10 <-code.list %>%
  select(icd) %>%
  filter(str_detect(icd,"^F11|^F14|^F15|^F16|^F17|^F19")) %>%
  distinct() %>%
  arrange(icd)%>% 
  mutate(icd=paste0('\\b',icd,'\\b'))

paste0(dput(as.character(drug_abuse.code.list.icd10$icd)),collapse = "|") # hard code icd code

drug_abuse.code.list.icd10 <- c("\\bF110\\b", "\\bF1100\\b", "\\bF1101\\b", "\\bF1102\\b", 
                                "\\bF1103\\b", "\\bF111\\b", "\\bF112\\b", "\\bF1120\\b", "\\bF1121\\b", 
                                "\\bF1122\\b", "\\bF1123\\b", "\\bF1124\\b", "\\bF1125\\b", "\\bF1126\\b", 
                                "\\bF113\\b", "\\bF1130\\b", "\\bF114\\b", "\\bF115\\b", "\\bF117\\b", 
                                "\\bF1171\\b", "\\bF118\\b", "\\bF119\\b", "\\bF140\\b", "\\bF1400\\b", 
                                "\\bF1401\\b", "\\bF1402\\b", "\\bF141\\b", "\\bF142\\b", "\\bF1421\\b", 
                                "\\bF1424\\b", "\\bF1425\\b", "\\bF143\\b", "\\bF145\\b", "\\bF148\\b", 
                                "\\bF149\\b", "\\bF150\\b", "\\bF1500\\b", "\\bF1502\\b", "\\bF151\\b", 
                                "\\bF152\\b", "\\bF1522\\b", "\\bF1524\\b", "\\bF153\\b", "\\bF155\\b", 
                                "\\bF158\\b", "\\bF159\\b", "\\bF160\\b", "\\bF161\\b", "\\bF162\\b", 
                                "\\bF1621\\b", "\\bF1624\\b", "\\bF163\\b", "\\bF1640\\b", "\\bF166\\b", 
                                "\\bF167\\b", "\\bF169\\b", "\\bF170\\b", "\\bF1700\\b", "\\bF1701\\b", 
                                "\\bF171\\b", "\\bF172\\b", "\\bF1720\\b", "\\bF1721\\b", "\\bF1722\\b", 
                                "\\bF1723\\b", "\\bF1724\\b", "\\bF1725\\b", "\\bF173\\b", "\\bF1731\\b", 
                                "\\bF174\\b", "\\bF175\\b", "\\bF177\\b", "\\bF1771\\b", "\\bF178\\b", 
                                "\\bF179\\b", "\\bF190\\b", "\\bF1901\\b", "\\bF1902\\b", "\\bF1903\\b", 
                                "\\bF191\\b", "\\bF192\\b", "\\bF1920\\b", "\\bF1922\\b", "\\bF1923\\b", 
                                "\\bF1924\\b", "\\bF1925\\b", "\\bF1926\\b", "\\bF193\\b", "\\bF1930\\b", 
                                "\\bF194\\b", "\\bF195\\b", "\\bF196\\b", "\\bF197\\b", "\\bF198\\b", 
                                "\\bF199\\b")

drug_abuse.code.list <- c(drug_abuse.code.list.icd9,drug_abuse.code.list.icd10)

# devices data----


devices.code.list.opcs4 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^K59|^K60")) %>%
  distinct() %>%
  arrange(opcs)%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(as.character(devices.code.list.opcs4$opcs)),collapse = "|") # hard code icd code

devices.code.list.opcs4 <-c("\\bK591\\b", "\\bK592\\b", "\\bK593\\b", "\\bK594\\b", "\\bK595\\b", 
                            "\\bK596\\b", "\\bK597\\b", "\\bK598\\b", "\\bK599\\b", "\\bK601\\b", 
                            "\\bK602\\b", "\\bK603\\b", "\\bK604\\b", "\\bK605\\b", "\\bK606\\b", 
                            "\\bK607\\b", "\\bK608\\b", "\\bK609\\b")

devices.code.list.opcs3 <- code.list.proc %>%
  select(opcs) %>%
  filter(str_detect(opcs,"^3051|^3052|^3053|^3058|^3059|^8971|^8972")) %>%
  distinct() %>%
  arrange(opcs)%>% 
  mutate(opcs=paste0('\\b',opcs,'\\b'))

paste0(dput(as.character(devices.code.list.opcs3$opcs)),collapse = "|") # hard code icd code

devices.code.list.opcs3 <- c("\\b3051\\b", "\\b3052\\b", "\\b3053\\b", "\\b3059\\b", "\\b8971\\b", 
                             "\\b8972\\b")

devices.code.list <- c(devices.code.list.opcs3,devices.code.list.opcs4)
