######################################################################
# PROGRAM NAME: 		   Hep B incident cases (Dr Ake/ACTG request)     
# AUTHOR:              Nicole Dear
# DATE WRITTEN:        25JUN2024
# REVIEWED BY:		     Natalie Burns
# DATE REVIEWED:		   3JUL2024
# WRITTEN FOR:         AFRICOS
# PURPOSE:             Identify cases of incident hep B and 
#                      check ART regimen and viral load
# OVERVIEW:                                                       
# INPUT DATA:          data_freeze_20240301  
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
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(haven)
library(dplyr)
library(Hmisc)
library(expss) # label variables

# set working directory
setwd("G:/DCAC/DCAC_PEPFAR/RV329/Analyses/Hepatitis/ACTG HIV_HBV cure")

# load data
hivstat <- read_sas("data_freeze_20240301/hivstat0.sas7bdat")
names(hivstat) <- tolower(names(hivstat))
arvcode <- read_sas("data_freeze_20240301/arvcode0.sas7bdat")
names(arvcode) <- tolower(names(arvcode))
v_load <- read_sas("data_freeze_20240301/v_load0.sas7bdat")
names(v_load) <- tolower(names(v_load))
serology <- read_sas("data_freeze_20240301/serology0.sas7bdat")
names(serology) <- tolower(names(serology))


# DATA CLEANING -----

# select variables of interest
hivstat1 <- hivstat %>%
  select(subjid, visit, visitdt, hivflag, hivstat, progid, gender, age, diagdtn, art_sdtn)

# carry forward HIV dx date and ART start date
hivstat2 <- hivstat1 %>%
  group_by(subjid) %>%
  fill(c(diagdtn, art_sdtn), .direction = "down") %>%
  ungroup()

arvcode1 <- arvcode %>%
  select(subjid, visit, visitdt, medicat, arv_code, startdtcn, astartdtcn)

v_load1 <- v_load %>%
  select(subjid, visit, visitdt, sequence, drawper, drawdt, vl_circ, vlcopy, nodetect, vl) %>%
  # include only rows with draw at study visit
  filter(drawper==1) %>%
  # remove duplicate rows
  # (for D02-0124, this row with visit=2 is a dup of visit 7
  # and for B01-0033, this row is a dup where the other visit 7 row has non-missing vlcopy)
  filter(!(subjid=="B01-0033" & visit==7 & is.na(vlcopy))) %>%
  filter(!(subjid=="D02-0124" & visitdt=="2022-08-19" & visit==2)) %>%
  # remove rows that have same subjid, visit, and vlcopy
  distinct(subjid, visit, vlcopy, .keep_all = TRUE) %>%
  # variable to check if visit values are repeated within a subjid
  group_by(subjid, visit) %>%
  mutate(dupflag = if_else(row_number()>1, 1, 0)) %>%
  # remove rows from same visit with sequence greater than 1
  filter(!(dupflag==1 & sequence!=1))

serology1 <- serology %>% 
  select(subjid, visit, visitdt, sequence, hivflag, drawper, hepbsa, hepbsac, hepbea, hepbca) %>% 
  # remove those with draw period outside of study visit
  filter(drawper==1 | is.na(drawper)) %>%
  # keep PLWH only
  filter(hivflag==1) %>%
  # remove row with missing Hep B data for subjid with with duplicate rows at visit 1
  filter(!(subjid=="E01-0204" & visit==1 & sequence==2))

# check for dups by subjid and visit
duptest <- serology1 %>% 
  group_by(subjid, visit) %>% 
  mutate(dupe = n()>1) 

# for duplicates, drop the row where HBaAg result is missing
serology2 <- duptest %>%
  filter(dupe==FALSE | (dupe==TRUE & !(is.na(hepbsa))))

# now drop remaining duplicates
serology3 <- serology2 %>%
  distinct(subjid, visit, .keep_all = TRUE) %>% 
  select(subjid, visit, visitdt, hepbsa, hepbsac, hepbea, hepbca)

# final check to make sure no dups remain
duptest2 <- serology3 %>% 
  group_by(subjid, visit) %>% 
  mutate(dupe = n()>1)
# none found!


# IDENTIFY INCIDENT CASES OF HEP B -----

# serology4 <- serology3 %>%
#   # create new hepb variable using HBsAg result
#   mutate(hepb = if_else(!is.na(hepbsa) & hepbsa==1, 1, NA)) %>%
#   # if HBsAg result at visit 1 is missing then replace with hepbsac if available
#   mutate(hepb = if_else(is.na(hepbsa) & visit==1, hepbsac, hepb)) %>%
#   # if HBsAg result at visit 1 is still missing then assume prevalent Hep B
#   # (or at least cannot be counted as an incident case)
#   mutate(hepb = if_else(is.na(hepbsa) & visit==1, 1, hepb))

serology4 <- serology3 %>%
  # create new hepb variable using HBsAg result and confirmatory ELISA result
  # to be considered incident Hep B must have reactive HBsAg and a positive confirmatory ELISA result (hepbsac)
  mutate(hepb = if_else(hepbsa==1 & hepbsac==1, 1, NA)) %>% 
  # though confirmatory result needed, if HBsAg is missing or reactive at V1 then cannot be considered incident case
  mutate(hepb = if_else((is.na(hepbsa) | hepbsa==1) & visit==1, 1, hepb))
  
# one participant has nonreactive HBsAg but reactive confirmatory test, do not consider an incident case
serology4$hepb[serology4$subjid=="C01-0420" & serology4$visit==1] <- 1

incident <- serology4 %>%
  group_by(subjid) %>%
  # this flags first nonzero occurrence of hepb (hep b is 1 or NA) by subjid and gives all other rows a value of 0,
  mutate(first = hepb*(!duplicated(hepb))) %>%
  # if first hepb at visit 1, then change 'first' to 0
  mutate(first = ifelse(first==1 & visit==1, 0, first)) %>% 
  # if 'first' is 0 then change to NA
  mutate(first = ifelse(first==0, NA, first))

# one participant doesn't have serology data at enrollment, but has reactive HBsAg at v2, do not consider an incident case
incident$first[incident$subjid=="E01-0058"] <- NA


# PULL IN VIRAL LOAD AND ART REGIMEN FOR THE INCIDENT CASES OF HEP B -----

incident_merge0 <- hivstat2 %>%
  full_join(v_load1, by = c("subjid", "visit")) %>%
  full_join(arvcode1, by = c("subjid", "visit")) %>%
  full_join(incident, by = c("subjid", "visit"))

# next want to flag all visits for those with incident Hep B
incident_merge <- incident_merge0 %>%
  mutate(flag = first) %>% 
  group_by(subjid) %>%
  fill(c(flag, progid, hivflag), .direction = "downup") %>%
  ungroup() %>%
  arrange(subjid, visit) %>% 
  # only keep visits for participants with incident Hep B
  filter(flag==1)

# label factor variables
incident_merge$progid <- factor(incident_merge$progid,
                   levels = c(1,2,3,4,5),
                   labels = c("Kayunga, Uganda", "South Rift Valley, Kenya", "Kisumu West, Kenya", 
                              "Mbeya, Tanzania", "Abuja & Lagos Nigeria"))

incident_merge$hivflag <- factor(incident_merge$hivflag,
                    levels = c(1,2),
                    labels = c("PLWH", "PLWoH"))

incident_merge$vl_circ <- factor(incident_merge$vl_circ,
                                 levels = c(1,2,3),
                                 labels = c("<", "", ">"))

# pull in names of ART corresponding to medication codes
incident_merge$arv_name <- ""
incident_merge$arv_name[incident_merge$arv_code=="J01"] <- "Abacavir"
incident_merge$arv_name[incident_merge$arv_code=="J12"] <- "Dolutegravir"
incident_merge$arv_name[incident_merge$arv_code=="J12,J54"] <- "Dolutegravir + Abacavir + Lamivudine"
incident_merge$arv_name[incident_merge$arv_code=="J17"] <- "Efavirenz + Emtricitabine + Tenofovir"
incident_merge$arv_name[incident_merge$arv_code=="J25"] <- "Lamivudine + Nevirapine + Zidovudine"
incident_merge$arv_name[incident_merge$arv_code=="J26"] <- "Lamivudine + Zidovudine"
incident_merge$arv_name[incident_merge$arv_code=="J29,J42"] <- "Lopinavir/Ritonavir + Tenofovir + Lamivudine"
incident_merge$arv_name[incident_merge$arv_code=="J42"] <- "Tenofovir + Lamivudine"
incident_merge$arv_name[incident_merge$arv_code=="J43"] <- "Tenofovir + Lamivudine + Efavirenz"
incident_merge$arv_name[incident_merge$arv_code=="J55"] <- "Tenofovir + Lamivudine + Dolutegravir"
incident_merge$arv_name[incident_merge$arv_code=="J56"] <- "Tenofovir + Lamivudine + Efavirenz"
incident_merge$arv_name[incident_merge$arv_code=="J57"] <- "Abacair + Lamivudine + Dolutegravir"

# clean up medicat variable
incident_merge$medicat_cl <- ""
incident_merge$medicat_cl[incident_merge$medicat=="10. Atazanazir(ATZ) 300mg"] <- "Atazanazir"
incident_merge$medicat_cl[incident_merge$medicat=="11. Atazanazir/Ritonavir 300mg/100mg"] <- "Atazanazir/Ritonavir"
incident_merge$medicat_cl[incident_merge$medicat=="12. Lopinavir/Ritonavir 400mg/100mg"] <- "Lopinavir/Ritonavir"
incident_merge$medicat_cl[incident_merge$medicat=="12. Lopinavir/Ritonavir 400mg/100mg ; 4. Lamivudine(3TC) 300mg"] <- "Lopinavir/Ritonavir + Lamivudine"
incident_merge$medicat_cl[incident_merge$medicat=="12. Lopinavir/Ritonavir 400mg/100mg ; 4. Lamivudine(3TC) 300mg ; 1. Abacair(ABC) 300mg"] <- "Lopinavir/Ritonavir + Lamivudine + Abacair"
incident_merge$medicat_cl[incident_merge$medicat=="15. Lamivudine/Nevirapine/Zidovudine"] <- "Lamivudine + Nevirapine + Zidovudine"
incident_merge$medicat_cl[incident_merge$medicat=="16. Lamivudine/Zidovudine 150/300mg"] <- "Lamivudine + Zidovudine"
incident_merge$medicat_cl[incident_merge$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 11. Atazanazir/Ritonavir 300mg/100mg"] <- "Lamivudine + Zidovudine + Atazanazir/Ritonavir"
incident_merge$medicat_cl[incident_merge$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 16. Lamivudine/Zidovudine 150/300mg ; 8. Efavirenz(EFV) 600mg"] <- "Lamivudine + Zidovudine + Efavirenz"
incident_merge$medicat_cl[incident_merge$medicat=="16. Lamivudine/Zidovudine 150/300mg ; 8. Efavirenz(EFV) 600mg"] <- "Lamivudine + Zidovudine + Efavirenz"
incident_merge$medicat_cl[incident_merge$medicat=="17. Tenofovir/Lamivudine 300/300mg"] <- "Tenofovir + Lamivudine"
incident_merge$medicat_cl[incident_merge$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 10. Atazanazir(ATZ) 300mg"] <- "Tenofovir + Lamivudine + Atazanazir"
incident_merge$medicat_cl[incident_merge$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 11. Atazanazir/Ritonavir 300mg/100mg"] <- "Tenofovir + Lamivudine + Atazanazir/Ritonavir"
incident_merge$medicat_cl[incident_merge$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 12. Lopinavir/Ritonavir 400mg/100mg"] <- "Tenofovir + Lamivudine + Lopinavir/Ritonavir"
incident_merge$medicat_cl[incident_merge$medicat=="17. Tenofovir/Lamivudine 300/300mg ; 9. Nevirapine(NVP) 200mg"] <- "Tenofovir + Lamivudine + Nevirapine"
incident_merge$medicat_cl[incident_merge$medicat=="18. Tenofovir/Lamivudine/Efavirenz 3"] <- "Tenofovir + Lamivudine + Efavirenz"
incident_merge$medicat_cl[incident_merge$medicat=="19. Tenofovir/Lamivudine/Dolutegravi"] <- "Tenofovir + Lamivudine + Dolutegravir"
incident_merge$medicat_cl[incident_merge$medicat=="20. Tenofovir/Lamivudine/Efavirenz 3"] <- "Tenofovir + Lamivudine + Efavirenz"
incident_merge$medicat_cl[incident_merge$medicat=="8. Efavirenz(EFV) 600mg"] <- "Efavirenz"
incident_merge$medicat_cl[incident_merge$medicat=="8. Efavirenz(EFV) 600mg ; 6. Tenofoivr(TDF) 300mg ; 4. Lamivudine(3TC) 300mg"] <- "Tenofovir + Lamivudine + Efavirenz"
incident_merge$medicat_cl[incident_merge$medicat=="9. Nevirapine(NVP) 200mg"] <- "Nevirapine"

# create single variable for ART regimen
incident_merge$art_regimen <- paste(incident_merge$medicat_cl, incident_merge$arv_name)

# remove extra spaces
incident_merge$art_regimen <- gsub("\\s", "", incident_merge$art_regimen)

# manually fill in missing ART regimen using v5 data -- cross checked with v5 data in Fauci using SAS studio
incident_merge$art_regimen[incident_merge$subjid=="A01-0087" & incident_merge$visit==19] <- "Abacair+Lamivudine+Dolutegravir"
incident_merge$art_regimen[incident_merge$subjid=="A01-0147" & incident_merge$visit==20] <- "Abacair+Lamivudine+Dolutegravir"
incident_merge$art_regimen[incident_merge$subjid=="A01-0166" & incident_merge$visit==19] <- "Abacair+Lamivudine+Dolutegravir"
incident_merge$art_regimen[incident_merge$subjid=="A01-0193" & incident_merge$visit==19] <- "Abacair+Lamivudine+Dolutegravir"
incident_merge$art_regimen[incident_merge$subjid=="A01-0432" & incident_merge$visit==15] <- "Lamivudine+Zidovudine+Dolutegravir"
incident_merge$art_regimen[incident_merge$subjid=="A01-0489" & incident_merge$visit==15] <- "Abacair+Lamivudine+Dolutegravir"
incident_merge$art_regimen[incident_merge$subjid=="E03-0014" & incident_merge$visit==2] <- "No record of any ART; died on 15DEC2015"

# additional manual change - subjid C01-0213 initiated ART on 2015-11-27, so prior visits (V1 and V2) were ART naive
incident_merge$art_regimen[incident_merge$subjid=="C01-0213" & incident_merge$visit==1] <- "ART naive"
incident_merge$art_regimen[incident_merge$subjid=="C01-0213" & incident_merge$visit==2] <- "ART naive"

# then create lag ART variable by participant
incident_merge <- incident_merge %>% 
  arrange(subjid, visit) %>% 
  group_by(subjid) %>% 
  mutate(art_regimen_priorvisit = lag(art_regimen))

# create single viral load variable
incident_merge$viral_load <- paste(incident_merge$vl_circ, incident_merge$vlcopy)

# remove extra spaces
incident_merge$viral_load <- gsub("NA", "", incident_merge$viral_load)
incident_merge$viral_load <- gsub("\\s", "", incident_merge$viral_load)

# fill in missing visit dates
incident_merge <- incident_merge %>% 
  mutate(visit_date = if_else(!is.na(visitdt.x), visitdt.x,
                              if_else(!is.na(visitdt.y), visitdt.y, visitdt.x.x, NA)))

# select variables of interest
incident_merge1 <- incident_merge %>%
  mutate(viral_load = if_else(is.na(vlcopy) & nodetect==1, "Undetectable", viral_load)) %>% 
  select(subjid, visit, visit_date, hivflag, progid, hepbsa, hepbsac, first, viral_load, art_regimen, art_regimen_priorvisit)

# filter for incident cases only
hepb <- incident_merge1 %>% 
  filter(first==1)

# summary table of ART regimen at time of incident Hep B
table_a <- as.data.frame(table(hepb$art_regimen, useNA = "ifany"))

# summary table of ART regimen at time point prior to incident Hep B
table_b <- as.data.frame(table(hepb$art_regimen_priorvisit, useNA = "ifany"))

# sort by frequency (largest to smallest)
table_a1 <- table_a %>% 
  arrange(desc(Freq))

table_b1 <- table_b %>% 
  arrange(desc(Freq))

# label variables for export
incident_merge1 = apply_labels(incident_merge1,
                               progid = "Program site",
                               hivflag = "HIV status",
                               visit_date = "Visit date",
                               first = "First timepoint with reactive HBsAg",
                               viral_load = "Viral load (copies/mL)",
                               art_regimen = "ART regimen",
                               art_regimen_priorvisit = "Prior ART regimen")

hepb = apply_labels(hepb,
                    progid = "Program site",
                    hivflag = "HIV status",
                    visit_date = "Visit date",
                    first = "First timepoint with reactive HBsAg",
                    viral_load = "Viral load (copies/mL)",
                    art_regimen = "ART regimen",
                    art_regimen_priorvisit = "Prior ART regimen")

table_a1 = apply_labels(table_a1,
                      Freq = "Frequency",
                      Var1 = "ART regimen")

table_b1 = apply_labels(table_b1,
                        Freq = "Frequency",
                        Var1 = "ART regimen")

names(incident_merge1) <- label(incident_merge1)
names(hepb) <- label(hepb)
names(table_a1) <- label(table_a1)
names(table_b1) <- label(table_b1)

# export to excel
list_of_datasets <- list("Summary cases by ART" = table_a1, "Summary cases by prior ART" = table_b1, 
                         "Incident cases" = hepb, "All visits" = incident_merge1)
write.xlsx(list_of_datasets, file = "incident_hepb_listing_2JUL2024.xlsx")
