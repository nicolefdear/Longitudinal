######################################################################
# PROGRAM NAME: 		   INSPIRE: reinvestigate cases and controls     
# AUTHOR:              Nicole Dear
# DATE WRITTEN:        5DEC2024
# REVIEWED BY:		     
# DATE REVIEWED:		   
# WRITTEN FOR:         AFRICOS
# PURPOSE:             Reinvestigate cases and controls 
# OVERVIEW:                                                       
# INPUT DATA:          090124_freeze  
# OUTPUT DATA:           
# RELIES ON:             
# RELIED ON BY:          
# SPECIAL INSTRUCTIONS:             
# MODIFICATION HISTORY:  
# DATE	MODIFIER	DESCRIPTION/REASON
######################################################################


## SETUP -----

# global settings
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)  # view more columns

# load packages
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
library(textclean)
library(ggpubr)
library(tidyverse)
library(lubridate)
library(dvmisc)
library(data.table)
library(writexl)
library(readxl)

# set working directory to load data
setwd("G:/DCAC/DCAC_PEPFAR/RV329/Data In/090124_freeze")

# load data
hivstat0 <- read_sas("hivstat0.sas7bdat")
names(hivstat0) <- tolower(names(hivstat0))

subjq_a0 <- read_sas("subjq_a0.sas7bdat")
names(subjq_a0) <- tolower(names(subjq_a0))

v_load0 <- read_sas("v_load0.sas7bdat")
names(v_load0) <- tolower(names(v_load0))

lymph0 <- read_sas("lymph0.sas7bdat")
names(lymph0) <- tolower(names(lymph0))

arvcode0 <- read_sas("arvcode0.sas7bdat")
names(arvcode0) <- tolower(names(arvcode0))


## DATA CLEANING -----

hivstat1 <- hivstat0 %>%
  select(subjid, visit, visitdt, gender, age, dobdtn, hivflag, hivstat, progid, diagdtn, art_sdtn, dur_art, dur_hiv) %>%
  # remove participants with duplicate visits with wrong date
  filter(!(subjid=="A01-0382" & visit==17 & visitdt=='2023-11-23')) %>%
  filter(!(subjid=="C01-0064" & visit==2 & visitdt=='2023-10-25')) %>%
  filter(!(subjid=="C01-0067" & visit==2 & visitdt=='2023-11-15')) %>%
  filter(!(subjid=="C01-0139" & visit==9 & visitdt=='2023-09-20')) %>%
  filter(!(subjid=="D01-0022" & visit==13 & visitdt=='2023-10-17')) %>%
  filter(!(subjid=="D01-0226" & visit==15 & visitdt=='2023-10-18')) %>%
  filter(!(subjid=="D01-0277" & visit==1 & visitdt=='2023-12-07'))

hivstat1$visit[hivstat1$subjid=="C01-0044" & hivstat1$visit==19 & hivstat1$visitdt=='2023-10-31'] <- 20
hivstat1$visit[hivstat1$subjid=="C01-0116" & hivstat1$visit==18 & hivstat1$visitdt=='2023-08-03'] <- 19
hivstat1$visit[hivstat1$subjid=="D01-0405" & hivstat1$visit==16 & hivstat1$visitdt=='2022-10-26'] <- 14
hivstat1$visit[hivstat1$subjid=="D02-0008" & hivstat1$visit==12 & hivstat1$visitdt=='2023-10-16'] <- 13

hivstat1 <- hivstat1 %>%
  distinct(subjid, visit, .keep_all = TRUE)

# check for dups by subjid and visit
duptest1 <- hivstat1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest1$dupe)

# added to ad-hoc query tracker
v_load0$nodetect[v_load0$subjid=="B02-0155" & v_load0$visit==19] <- NA

v_load1 <- v_load0 %>%
  select(subjid, visit, visitdt, sequence, drawper, drawdt, vl_circ, vlcopy, nodetect) %>%
  # include only rows with draw at study visit
  filter(drawper==1) %>%
  # remove duplicate rows (for D02-0124, this row with visit=2 is a dup of visit 7
  # and for B01-0033, this row is a dup where the other visit 7 row has non-missing vlcopy)
  filter(!(subjid=="B01-0033" & visit==7 & is.na(vlcopy))) %>%
  filter(!(subjid=="D02-0124" & visitdt=="2022-08-19" & visit==2)) %>%
  filter(!(subjid=="D01-0022" & visitdt=="2023-10-17" & visit==13)) %>%
  filter(!(subjid=="D01-0277" & visitdt=="2023-12-07" & visit==1)) %>%
  # remove rows that have same subjid, visit, and vlcopy
  distinct(subjid, visit, vlcopy, .keep_all = TRUE) %>%
  # variable to check if visit values are repeated within a subjid
  group_by(subjid, visit) %>%
  mutate(dupflag = if_else(n()>1, 1, 0)) %>%
  # remove rows from same visit with sequence greater than 1
  filter(!(dupflag==1 & sequence!=1)) %>% 
  # clean vl variable
  mutate(vl_clean = case_when(nodetect==1 & is.na(vlcopy) ~ "undetectable",
                              vl_circ==1 ~ paste0("<", vlcopy),
                              vl_circ==2 | is.na(vl_circ) ~ as.character(vlcopy),
                              vl_circ==3 ~ paste0(">", vlcopy))) %>% 
  mutate(vl_clean_num = case_when(nodetect==1 & is.na(vlcopy) ~ 1,
                                  is.na(nodetect) | nodetect!=1 ~ vlcopy)) %>% 
  mutate(vls1000 = case_when(nodetect==1 | vlcopy<1000 ~ 1,
                             vlcopy>=1000 ~ 0,
                             .default = NA)) %>% 
  mutate(vls200 = case_when(nodetect==1 | vlcopy<200 ~ 1,
                            vlcopy>=200 ~ 0,
                            .default = NA))

v_load1$visit[v_load1$subjid=="D01-0405" & v_load1$visit==16 & v_load1$visitdt=='2022-10-26'] <- 14
v_load1$visit[v_load1$subjid=="D02-0008" & v_load1$visit==12 & v_load1$visitdt=='2023-10-16'] <- 13

# check for dups by subjid and visit
duptest3 <- v_load1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest3$dupe)

lymph1 <- lymph0 %>%
  select(subjid, visit, visitdt, sequence, drawper, drawdt, cd3_4_n) %>%
  # include only rows with draw at study visit
  filter(drawper==1 & !is.na(cd3_4_n)) %>% 
  # drop rows with incorrect visit #s/dates
  filter(!(subjid=="A01-0023" & visitdt=="2022-11-29" & visit==2)) %>%
  filter(!(subjid=="A01-0024" & visitdt=="2022-11-08" & visit==2)) %>%
  filter(!(subjid=="A01-0030" & visitdt=="2022-11-29" & visit==2)) %>%
  filter(!(subjid=="A01-0032" & visitdt=="2022-11-21" & visit==2)) %>%
  filter(!(subjid=="A01-0036" & visitdt=="2022-12-12" & visit==2)) %>%
  filter(!(subjid=="A01-0037" & visitdt=="2022-11-01" & visit==2)) %>%
  filter(!(subjid=="A01-0042" & visitdt=="2022-11-15" & visit==2)) %>%
  filter(!(subjid=="A01-0044" & visitdt=="2022-11-16" & visit==2)) %>%
  filter(!(subjid=="A01-0047" & visitdt=="2022-11-15" & visit==2)) %>%
  filter(!(subjid=="A01-0048" & visitdt=="2022-12-05" & visit==2)) %>%
  filter(!(subjid=="A01-0051" & visitdt=="2022-11-15" & visit==2)) %>%
  filter(!(subjid=="A01-0071" & visitdt=="2022-11-01" & visit==2)) %>%
  filter(!(subjid=="A01-0081" & visitdt=="2022-12-01" & visit==2)) %>%
  filter(!(subjid=="A01-0087" & visitdt=="2022-11-28" & visit==2)) %>%
  filter(!(subjid=="A01-0089" & visitdt=="2022-11-28" & visit==2)) %>%
  filter(!(subjid=="A01-0090" & visitdt=="2022-11-30" & visit==2)) %>%
  filter(!(subjid=="A01-0093" & visitdt=="2022-12-01" & visit==2)) %>%
  filter(!(subjid=="A01-0094" & visitdt=="2022-12-12" & visit==2)) %>%
  filter(!(subjid=="A01-0096" & visitdt=="2022-12-06" & visit==2)) %>%
  filter(!(subjid=="A01-0099" & visitdt=="2022-12-14" & visit==2)) %>%
  filter(!(subjid=="A01-0107" & visitdt=="2023-01-04" & visit==2)) %>%
  filter(!(subjid=="A01-0125" & visitdt=="2023-01-18" & visit==2)) %>%
  filter(!(subjid=="C01-0139" & visitdt=="2023-09-20" & visit==9)) %>%
  filter(!(subjid=="D01-0022" & visitdt=="2023-10-17" & visit==13)) %>%
  filter(!(subjid=="D01-0058" & visitdt=="2023-08-24" & visit==10)) %>%
  filter(!(subjid=="D01-0277" & visitdt=="2023-12-07" & visit==1)) %>%
  # variable to check if visit values are repeated within a subjid
  group_by(subjid, visit) %>%
  mutate(dupflag = if_else(n()>1, 1, 0)) %>%
  # remove rows from same visit with sequence greater than 1
  filter(!(dupflag==1 & sequence!=1)) %>% 
  # create categorical version of CD4 count
  mutate(cd4cat = case_when(cd3_4_n<250 ~ 1,
                            cd3_4_n>=250 & cd3_4_n<500 ~ 2,
                            cd3_4_n>=500 ~ 3,
                            .default = NA))

lymph1$visit[lymph1$subjid=="C01-0044" & lymph1$visit==19 & lymph1$visitdt=='2023-10-31'] <- 20
lymph1$visit[lymph1$subjid=="D01-0405" & lymph1$visit==16 & lymph1$visitdt=='2022-10-26'] <- 14
lymph1$visit[lymph1$subjid=="D02-0008" & lymph1$visit==12 & lymph1$visitdt=='2023-10-16'] <- 13

lymph1 <- lymph1 %>% 
  distinct(subjid, visit, .keep_all = TRUE)

# check for dups by subjid and visit
duptest4 <- lymph1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest4$dupe)

# ART and other medication data
arvcode1 <- arvcode0 %>%
  select(subjid, visit, visitdt, medicat, arv_code, startdtcn, astartdtcn, atazanavir, darunavir, lopinavir, ritonavir)

# check for dups by subjid and visit
duptest5 <- arvcode1 %>%
  group_by(subjid, visit) %>%
  mutate(dupe = n()>1)
table(duptest5$dupe)


## MERGE DATA -----

data0 <- hivstat1 %>%
  full_join(v_load1, by = c("subjid", "visit")) %>% 
  full_join(arvcode1, by = c("subjid", "visit")) %>%
  mutate(visitdt = if_else(!is.na(visitdt.x), visitdt.x, visitdt.y, NA)) %>% 
  arrange(subjid, visit)


## ADDITIONAL DATA CLEANING ON COMBINED DATASET -----

# create new variables and convert to factor
data1 <- data0 %>%
  
  # demographics
  mutate(country = case_when(progid==1 ~ 1,
                             progid==2 | progid==3 ~ 2,
                             progid==4 ~ 3,
                             progid==5 ~ 4,
                             .default = NA)) %>%

  # fill in values for variables collected at enrollment/added in later amendment
  group_by(subjid) %>%
  fill(c(hivflag, gender, progid, country, dobdtn, ), .direction = "downup") %>%
  fill(c(diagdtn, art_sdtn), .direction = "down") %>%
  ungroup() %>%
  
  # calculate age at visit
  mutate(agev = as.numeric(difftime(visitdt, dobdtn) / 365.25), 1) %>%
  mutate(agev = round(agev, 1)) %>% 
  
  # remove anyone with missing HIV status
  filter(!is.na(hivflag))

# fill in duration since HIV dx and duration on ART if missing
j <- is.na(data1$dur_hiv)
data1$dur_hiv[j] <- difftime(data1$visitdt[j], data1$diagdtn[j], units = "days") / 365.25

k <- is.na(data1$dur_art)
data1$dur_art[k] <- difftime(data1$visitdt[k], data1$art_sdtn[k], units = "days") / 365.25


## IDENTIFY CASES & CONTROLS -----

# Cases:
#   o	HIV-infected
#   o	18-54 yo
#   o	On 2nd Line Therapy - Ritonavir boosted PI based ART
#   o	VL >1,000 - identify case visit: first treatment failure (vl>=1000) visit for those on second line therapy
# 
# Controls:
#   o	HIV-infected
#   o	18-54 yo
#   o	Ritonavir boosted PI based ART
#   o	Suppressed viral replication <1,000 to 48 weeks -
#         control visits: among those on second line therapy who never experience Tx failure,
#         take first visit 48 weeks after starting PI with VL 40-1000

## QUESTIONS -----
  # any site? any site except Tanzania
  # matching? no - given the limited availability of samples, take everything

# create inclusion criteria flags
data2 <- data1 %>%
  mutate(age_keep = if_else(agev>=18 & agev<55, 1, 0)) %>%
  mutate(pi_rboost = if_else((atazanavir==1 | darunavir==1 | lopinavir==1) & ritonavir==1, 1, 0))

# find earliest PI start date
data3 <- data2 %>%
  filter(pi_rboost == 1) %>% 
  group_by(subjid) %>%
  arrange(startdtcn) %>%
  slice(1L) %>%
  rename(startdtcn_pi = startdtcn) %>% 
  select(subjid, startdtcn_pi)

data4 <- data2 %>%
  filter(pi_rboost == 1) %>% 
  group_by(subjid) %>%
  arrange(astartdtcn) %>%
  slice(1L) %>% 
  rename(astartdtcn_pi = astartdtcn) %>% 
  select(subjid, astartdtcn_pi)

d0 <- data3 %>%
  full_join(data4, by = c("subjid"))

d1 <- data2 %>% 
  full_join(d0, by = c("subjid"))

d2 <- d1 %>% 
  rowwise() %>%
  mutate(min_pi_date = min(startdtcn_pi, astartdtcn_pi)) %>% 
  mutate(min_pi_date = if_else(is.na(min_pi_date) & !is.na(startdtcn_pi), startdtcn_pi, min_pi_date)) %>% 
  mutate(min_pi_date = if_else(is.na(min_pi_date) & !is.na(astartdtcn_pi), astartdtcn_pi, min_pi_date)) %>%
  # calculate duration on any PI in weeks
  mutate(dur_pi = if_else(visitdt >= min_pi_date, as.numeric(difftime(visitdt, min_pi_date)/7), NA)) %>% 
  mutate(dur_pi = round(dur_pi, 1))

# cases
case <- d2 %>% 
  # first treatment failure (vl=>1000) for those on second line (PI) therapy
  filter(hivflag==1 & age_keep==1 & (country==1 | country==2 | country==4) & pi_rboost==1 & vls1000==0 & !is.na(dur_pi)) %>% 
  group_by(subjid) %>%
  slice_head(n = 1) %>% 
  ungroup() %>% 
  # add labels
  mutate(progid = factor(progid,
                         levels = c(1,2,3,4,5),
                         labels = c("Kayunga, Uganda", "South Rift Valley, Kenya",
                                    "Kisumu West, Kenya", "Mbeya, Tanzania",
                                    "Abuja & Lagos Nigeria"))) %>%
  mutate(country = factor(country,
                          levels = c(1,2,3,4),
                          labels = c("Uganda", "Kenya", "Tanzania", "Nigeria"))) %>%
  mutate(gender = factor(gender,
                         levels = c(1,2),
                         labels = c("Male", "Female"))) %>%
  mutate(hivflag = factor(hivflag,
                          levels = c(1,2),
                          labels = c("PLWH", "PLWoH"))) %>%
  mutate(hivstat = factor(hivstat,
                          levels = c(1,2),
                          labels = c("PLWH", "PLWoH"))) %>% 
  select(subjid, visit, visitdt, hivflag, agev, gender, country, atazanavir, darunavir, lopinavir, ritonavir, 
         pi_rboost, min_pi_date, dur_pi, vl_clean)

# flag those already processed/used by INSPIRE team
samples0 <- read_excel("G:/DCAC/DCAC_PEPFAR/RV329/Analyses/Sample requests/INSPIRE/INSPIRE_SamplesUsed_062024.xlsx", sheet=2)

samples1 <- samples0 %>%
  arrange(subjid, visit) %>% 
  select(subjid, visit, Status)

case_final <- case %>%
  full_join(samples1, by = c("subjid", "visit")) %>% 
  arrange(subjid, visit) %>% 
  filter(!is.na(visitdt))

# controls
c0 <- d2 %>% 
  filter(hivflag==1 & pi_rboost==1) %>% 
  # sort viral load (lowest to highest) by subjid and take visit with highest viral load value while on PI
  filter(!is.na(vl_clean_num)) %>% 
  arrange(subjid, vl_clean_num) %>%
  group_by(subjid) %>%
  slice_tail(n = 1) %>%
  filter(vls1000==1) %>% 
  mutate(neverfail=1) %>% 
  select(subjid, neverfail)

# merge back with full dataset
c1 <- d2 %>% 
  full_join(c0, by = c("subjid"))

control <- c1 %>% 
  # further restrict to those who meet age and PI duration criteria
  # and flag if highest VL is between 40 and 1000 on PI (for those who never failed on second line tx)
  filter(age_keep==1 & (country==1 | country==2 | country==4) & dur_pi>=48 & vls1000==1 & vl_clean_num>=40 & vl_clean!="<40") %>%
  group_by(subjid) %>%
  slice_head(n = 1) %>% 
  ungroup() %>% 
  # add labels
  mutate(progid = factor(progid,
                         levels = c(1,2,3,4,5),
                         labels = c("Kayunga, Uganda", "South Rift Valley, Kenya",
                                    "Kisumu West, Kenya", "Mbeya, Tanzania",
                                    "Abuja & Lagos Nigeria"))) %>%
  mutate(country = factor(country,
                          levels = c(1,2,3,4),
                          labels = c("Uganda", "Kenya", "Tanzania", "Nigeria"))) %>%
  mutate(gender = factor(gender,
                         levels = c(1,2),
                         labels = c("Male", "Female"))) %>%
  mutate(hivflag = factor(hivflag,
                          levels = c(1,2),
                          labels = c("PLWH", "PLWoH"))) %>%
  mutate(hivstat = factor(hivstat,
                          levels = c(1,2),
                          labels = c("PLWH", "PLWoH"))) %>% 
  select(subjid, visit, visitdt, hivflag, agev, gender, country, atazanavir, darunavir, lopinavir, ritonavir, 
         pi_rboost, min_pi_date, dur_pi, vl_clean)


# export to excel
setwd("G:/DCAC/DCAC_PEPFAR/RV329/Analyses/Sample requests/INSPIRE")
list_datasets <- list("Cases" = case_final, "Controls" = control)
write_xlsx(list_datasets, path = "reinvestigate_cases_controls_10DEC2024.xlsx")


