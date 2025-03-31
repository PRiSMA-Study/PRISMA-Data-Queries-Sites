#*QUERY #5.5 -- CHECK FOR MISSING DATA AND FORMS 
#* Written by: Precious Williams

#* Date Started:21 June 2023
#* Last Updated:29 November 2023
#* Updated queries to only identify missing forms
#* Updated EDD/GA variables for India_CMC
#* Updated EDD Query to include US vs LMP GA & US queries 

#rm(list = ls())

# load packages 

library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(data.table)
library(lubridate)

# UPDATE EACH RUN: set site variable - this is necessary to call in the correct MNH25 variables from the data dictionary (each site has their own MNH25)
site = "Kenya"

# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: load in the WIDE data we generated from 00_DataImport code -- for duplicates
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_wide.Rdata", sep = "")) 

# UPDATE EACH RUN:load in the LONG data we generated from 00_DataImport code -- for protocol checks 
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_long.Rdata", sep = "")) 


# UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

setwd (path_to_save)

#Step 0 - if MOMID or PREGID is not present in MNH01, we want to transfer the MOMID and PREGIDs from MNH02 into MNH01 by SCRNID
#Step 0 - if MOMID or PREGID is not present in MNH01, we want to transfer the MOMID and PREGIDs from MNH02 into MNH01 by SCRNID

# Check for different variations of "N/A" in the MOMID column

na_variations <- c("n/a", "NA", "N/A", "na", NA, "")

if (sum(mnh01$MOMID %in% na_variations | is.na(mnh01$MOMID), na.rm = TRUE) > 2) {
  # If more than ten MOMID values match the variations of "N/A",
  # perform the merge by SCRNID.
  mnh01 <- merge(mnh01[, !names(mnh01) %in% c("MOMID", "PREGID")],
                 mnh02[, c("SCRNID", "MOMID", "PREGID")], by = "SCRNID", all.x = TRUE)
} else {
  # If not, print a message indicating that MOMID and PREGID are present.
  
  print("MOMID and PREGID are present.")
  
}

mnh01 <- mnh01[!(mnh01$MOMID %in% na_variations | is.na(mnh01$MOMID)), ]

#Parse MNH01 dates to one date format
mnh01$US_OHOSTDAT <- ymd(parse_date_time(mnh01$US_OHOSTDAT,c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS1 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS1, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS2 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS2, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS3 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS3, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$US_EDD_BRTHDAT_FTS4 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS4, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$ESTIMATED_EDD_SCDAT <- ymd(parse_date_time(mnh01$ESTIMATED_EDD_SCDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS1 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS1, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS2 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS2, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS3 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS3, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh01$CAL_EDD_BRTHDAT_FTS4 <- ymd(parse_date_time(mnh01$CAL_EDD_BRTHDAT_FTS4, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

# mnh01$US_OHOSTDAT <- ymd(parse_date_time(mnh01$US_OHOSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS1 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS1, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS2 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS2, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS3 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS3, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
# mnh01$US_EDD_BRTHDAT_FTS4 <- ymd(parse_date_time(mnh01$US_EDD_BRTHDAT_FTS4, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

#Calculating GA and choosing the "latest" possible GA 

mnh01 <- mnh01 %>% 
  #making default values equal to NA
  mutate(US_GA_WKS_AGE_FTS1 = ifelse(US_GA_WKS_AGE_FTS1 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS1),
         US_GA_DAYS_AGE_FTS1 = ifelse(US_GA_DAYS_AGE_FTS1 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS1),
         US_GA_WKS_AGE_FTS2 = ifelse(US_GA_WKS_AGE_FTS2 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS2),
         US_GA_DAYS_AGE_FTS2 = ifelse(US_GA_DAYS_AGE_FTS2 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS2),
         US_GA_WKS_AGE_FTS3 = ifelse(US_GA_WKS_AGE_FTS3 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS3),
         US_GA_DAYS_AGE_FTS3 = ifelse(US_GA_DAYS_AGE_FTS3 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS3),
         US_GA_WKS_AGE_FTS4 = ifelse(US_GA_WKS_AGE_FTS4 %in% c(-7, 77), NA, US_GA_WKS_AGE_FTS4),
         US_GA_DAYS_AGE_FTS4 = ifelse(US_GA_DAYS_AGE_FTS4 %in% c(-7, 77), NA, US_GA_DAYS_AGE_FTS4),
         GA_LMP_WEEKS_SCORRES = ifelse(GA_LMP_WEEKS_SCORRES %in% c(-7, 77), NA, GA_LMP_WEEKS_SCORRES),
         GA_LMP_DAYS_SCORRES = ifelse(GA_LMP_DAYS_SCORRES %in% c(-7, 77), NA, GA_LMP_DAYS_SCORRES)
         
  ) %>%  
  
  #making the variables numeric
  mutate (US_GA_WKS_AGE_FTS1 = as.numeric(US_GA_WKS_AGE_FTS1),
          US_GA_DAYS_AGE_FTS1 = as.numeric(US_GA_DAYS_AGE_FTS1),
          US_GA_WKS_AGE_FTS2 = as.numeric(US_GA_WKS_AGE_FTS2),
          US_GA_DAYS_AGE_FTS2 = as.numeric(US_GA_DAYS_AGE_FTS2),
          US_GA_WKS_AGE_FTS3 = as.numeric(US_GA_WKS_AGE_FTS3),
          US_GA_DAYS_AGE_FTS3= as.numeric(US_GA_DAYS_AGE_FTS3),
          US_GA_WKS_AGE_FTS4 = as.numeric(US_GA_WKS_AGE_FTS4),
          US_GA_DAYS_AGE_FTS4 = as.numeric(US_GA_DAYS_AGE_FTS4),
          GA_LMP_WEEKS_SCORRES = as.numeric(GA_LMP_WEEKS_SCORRES),
          GA_LMP_DAYS_SCORRES = as.numeric(GA_LMP_DAYS_SCORRES)) %>%
  
  mutate (US_GA_DAYS_AGE_FTS1 = ifelse(is.na(US_GA_DAYS_AGE_FTS1), 0, US_GA_DAYS_AGE_FTS1), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS2 = ifelse(is.na(US_GA_DAYS_AGE_FTS2), 0, US_GA_DAYS_AGE_FTS2), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS3 = ifelse(is.na(US_GA_DAYS_AGE_FTS3), 0, US_GA_DAYS_AGE_FTS3), #we want to make days 0 if it is empty 
          US_GA_DAYS_AGE_FTS4 = ifelse(is.na(US_GA_DAYS_AGE_FTS4), 0, US_GA_DAYS_AGE_FTS4), #we want to make days 0 if it is empty 
          GA_LMP_WEEKS_SCORRES = ifelse(is.na(GA_LMP_WEEKS_SCORRES), 0, GA_LMP_WEEKS_SCORRES),
          GA_LMP_DAYS_SCORRES = ifelse(is.na(GA_LMP_DAYS_SCORRES), 0, GA_LMP_DAYS_SCORRES),
          
          GA_DAYS1 = (US_GA_WKS_AGE_FTS1 * 7) + US_GA_DAYS_AGE_FTS1, #Calculating gestational age in days 
          GA_DAYS2 = (US_GA_WKS_AGE_FTS2 * 7) + US_GA_DAYS_AGE_FTS2, #Calculating gestational age in days  
          GA_DAYS3 = (US_GA_WKS_AGE_FTS3 * 7) + US_GA_DAYS_AGE_FTS3, #Calculating gestational age in days  
          GA_DAYS4 = (US_GA_WKS_AGE_FTS4 * 7) + US_GA_DAYS_AGE_FTS4, #Calculating gestational age in days 
          GA_LMP = (GA_LMP_WEEKS_SCORRES * 7) + GA_LMP_DAYS_SCORRES #Calculating gestational age by LMP in days 
          
  )%>% 
  mutate(GA_DAYS = pmax(GA_DAYS1, GA_DAYS2, GA_DAYS3, GA_DAYS4,  na.rm = TRUE))

#Choosing the earliest EDD Date, for multiple pregnancies
mnh01 <- mnh01 %>% mutate(US_EDD_BRTHDAT_FTS1 = replace(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS1== ymd("1907-07-07") | US_EDD_BRTHDAT_FTS1== ymd("2007-07-07") , NA), 
                          US_EDD_BRTHDAT_FTS2 = replace(US_EDD_BRTHDAT_FTS2, US_EDD_BRTHDAT_FTS2==ymd("1907-07-07") | US_EDD_BRTHDAT_FTS2== ymd("2007-07-07"), NA),
                          US_EDD_BRTHDAT_FTS3 = replace(US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS3== ymd("1907-07-07")| US_EDD_BRTHDAT_FTS3== ymd("2007-07-07"), NA), 
                          US_EDD_BRTHDAT_FTS4 = replace(US_EDD_BRTHDAT_FTS4, US_EDD_BRTHDAT_FTS4==ymd("1907-07-07")| US_EDD_BRTHDAT_FTS4== ymd("2007-07-07"), NA),
                          ESTIMATED_EDD_SCDAT = replace(ESTIMATED_EDD_SCDAT, ESTIMATED_EDD_SCDAT==ymd("1907-07-07")| ESTIMATED_EDD_SCDAT== ymd("2007-07-07"), NA)) %>% 
  mutate(EDD = pmin(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS2, 
                    US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS4,  na.rm = TRUE))

mnh01$EDD <- ymd(parse_date_time(mnh01$EDD, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))


na_variations <- c("n/a", "NA", "N/A", "na", NA, "")

# Filter all participants that met the inclusion criteria in MNH process M02 data
Eligible <- mnh02 %>% filter (CONSENT_IEORRES == 1 & AGE_IEORRES == 1  & PC_IEORRES == 1  & CATCHMENT_IEORRES == 1  & CATCH_REMAIN_IEORRES == 1) %>%
  mutate (Eligible = 1) %>% select(SCRNID, Eligible)  %>%
  distinct(SCRNID, .keep_all = TRUE)

#Step 0.5a:Developing a Masterlist of Enrolled Individuals
Screened <- mnh01 %>% filter (TYPE_VISIT == 1 & GA_DAYS <= 146 & GA_DAYS != 0 & MAT_VISIT_MNH01 %in% c(1,2) ) %>% 
  select (MOMID, PREGID, SCRNID, US_OHOSTDAT, EDD, GA_DAYS, TYPE_VISIT, ESTIMATED_EDD_SCDAT,GA_LMP)  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) 

Enrolled <- merge(Screened, Eligible, by = c("SCRNID"))  %>% select (-c("TYPE_VISIT"))  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)

#Use the Ultrasound GA to calculate An Estimated EDD (EDD_EST)
EDD <- Enrolled %>%
  mutate(
    UPLOADDT = as.Date(UploadDate),
    EST_CONC_DATE = US_OHOSTDAT - days(GA_DAYS), 
    EDD_EST = EST_CONC_DATE + days(280), 
    DIFF = abs(as.numeric(difftime(EDD, EDD_EST, units = "days"))),
    DIFF_GA = abs(as.numeric(GA_LMP - GA_DAYS)),
    DIFF_EDD = abs(as.numeric(difftime(ESTIMATED_EDD_SCDAT, EDD, units = "days")))) %>%
  select(
    MOMID,
    SCRNID,
    PREGID,
    VisitDate = US_OHOSTDAT,
    ESTIMATED_EDD_SCDAT,
    EDD,
    UPLOADDT,
    EDD_EST,
    GA_DAYS,
    GA_LMP,
    EST_CONC_DATE, 
    DIFF,
    DIFF_GA,
    DIFF_EDD) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>% filter (GA_DAYS < 175)

#PART A: EDD QUERY CODE
#We want to create an EDD Query where the difference should be less than 14/-14

EDD_query <- EDD %>% 
  mutate (Significant = ifelse((DIFF > 14 & GA_DAYS != 0) | (DIFF_GA > 50 & GA_LMP != 0 ) | (DIFF_EDD > 50 & ESTIMATED_EDD_SCDAT != 0 & EDD != 0), "TRUE", "FALSE"), #if its greater than 7 days then there is a significant difference
          Invalid = ifelse(EDD %in%  c( 1907-07-07, 77, 55, -7, -5, NA), "TRUE", "FALSE")) %>% 
  filter(Significant == "TRUE" | Invalid == "TRUE") %>% 
  mutate(
    EditType = ifelse(EDD %in%  c( 1907-07-07, 77, 55, -7, -5, NA), "Missing US EDD at Enrolment",
                      ifelse((VisitDate == EDD), "Visit Date is the same as EDD",
                             ifelse((DIFF > 14 | (DIFF < -14)) & (!(VisitDate >= UploadDate)), "Inaccurate Ultrasound EDD",
                                    ifelse((DIFF_GA > 50) & (!(VisitDate >= UploadDate)) & (GA_LMP != 0), "Inconsistencies Between LMP GA and US GA",
                                           ifelse((DIFF_EDD > 50) & (!(VisitDate >= UploadDate)) & (ESTIMATED_EDD_SCDAT != 0), "Inconsistencies Between LMP EDD and US EDD",
                                                  ifelse((VisitDate >= UploadDate), "Inaccurate Visit Date","Error in US EDD"))))))) %>% 
  mutate(
    `Variable Name` = ifelse((EditType == "Inconsistencies Between LMP EDD and US EDD"), "ESTIMATED_EDD_SCDAT",
                             ifelse((EditType == "Inconsistencies Between LMP GA and US GA"), "GA_LMP_WEEKS_SCORRES/GA_LMP_DAYS_SCORRES", "US_EDD_BRTHDAT_FTS1")),
    `Variable Value` = ifelse((EditType == "Inconsistencies Between LMP EDD and US EDD"), (format(ESTIMATED_EDD_SCDAT, "%Y-%m-%d")),
                              ifelse((EditType == "Inconsistencies Between LMP GA and US GA"), GA_LMP, (format(EDD, "%Y-%m-%d")))),
    
    UploadDate = UPLOADDT, 
    ScrnID = SCRNID,
    Form = "MNH01",
    VisitType = "1",
    InfantID = "NA",
    FieldType = "Date", 
    MomID = MOMID,
    PregID = PREGID,
    DateEditReported = format(Sys.time(), "%Y-%m-%d"),
    Form_Edit_Type = paste(Form,"_",EditType),
    QueryID = paste0(Form, "_", VisitDate, "_", MomID, "_", Form) 
  ) %>%
  select (QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, VisitType, VisitDate, Form, 
          'Variable Name', 'Variable Value', FieldType, EditType, DateEditReported, Form_Edit_Type)

EDD_query <- EDD_query %>%
  mutate_all(as.character)

Facility <- mnh01  %>%  filter(TYPE_VISIT == 1) %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE)  %>% 
  rename(MomID = MOMID, PregID = PREGID, Location = US_OHOLOC, Facility =US_FAC_SPFY_OHOLOC, 
         Personell = FORMCOMPLID_MNH01 ) %>% 
  select (MomID, PregID, Location, Facility, Personell)

EDD_query_comments <- EDD %>%
  rename(MomID = MOMID, PregID = PREGID, Visitdate = VisitDate) %>%
  right_join(EDD_query, by = c("MomID", "PregID")) %>%
  left_join(Facility,  by = c("MomID", "PregID")) %>%
  filter(!(EditType == "Missing US EDD at Enrolment")) %>% 
  select(QueryID, MomID, PregID, UploadDate, VisitDate, `Reported Ultrasound EDD` = EDD,
         `Expected EDD` = EDD_EST,   `Difference:Expected-Site_EDD` = DIFF, 
         ESTIMATED_EDD_SCDAT, `US-LMP:EDD_Difference:` = DIFF_EDD,
         `US Gestational Age in Days` = GA_DAYS, `LMP Gestational Age in Days` = GA_LMP, `GA_Differnce: EDD vs LMP` = DIFF_GA,
         EditType, Location, Facility, Personell)

EDD_query_comments <- EDD_query_comments %>%
  mutate_all(as.character)

if (nrow(EDD_query_comments) >= 1) {
  save(EDD_query_comments, file = "EDD_query_comments.rda")
} 

# Save the EDD_query dataframe as an .rda file
if (nrow(EDD_query) >= 1) {
  save(EDD_query, file = "EDD_query.rda")
} 


##Labour and Delivery Query
if (exists("mnh09") == TRUE){
  
  mnh09_sub <- mnh09 %>%
    select(MOMID, PREGID, MAT_VISIT_MNH09, LABOR_MHOCCUR, LABOR_MHSTTIM, LABOR_MHSTDAT, INDUCED_PROCCUR,
           MEMBRANE_RUPT_MHTERM, MEMBRANE_RUPT_MHSTTIM, MEMBRANE_RUPT_MHSTDAT, INFANTS_FAORRES, MAT_LD_OHOSTDAT,
           INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4, DELIV_DSSTDAT_INF1,
           DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, 
           DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4, BIRTH_DSTERM_INF1,BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3,
           BIRTH_DSTERM_INF4, SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4,
           CES_PRINDC_INF1_1, CES_PRINDC_INF2_1, CES_PRINDC_INF3_1, CES_PRINDC_INF4_1,
           CES_PRINDC_INF1_3, CES_PRINDC_INF2_3, CES_PRINDC_INF3_3, CES_PRINDC_INF4_3,
           CES_PRINDC_INF1_4, CES_PRINDC_INF2_4, CES_PRINDC_INF3_4, CES_PRINDC_INF4_4,
           CES_PRINDC_INF1_9, CES_PRINDC_INF2_9, CES_PRINDC_INF3_9, CES_PRINDC_INF4_9, 
           CES_PRINDC_INF1_15, CES_PRINDC_INF2_15, CES_PRINDC_INF3_15, CES_PRINDC_INF4_15, 
           DELIV_PRROUTE_INF1, DELIV_PRROUTE_INF2, DELIV_PRROUTE_INF3, DELIV_PRROUTE_INF4) %>% 
    
    mutate(DELIV_DSSTDAT_INF1 = ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF2 = ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF3 = ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF4 = ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           MEMBRANE_RUPT_MHSTDAT = ymd(parse_date_time(MEMBRANE_RUPT_MHSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           LABOR_MHSTDAT = ymd(parse_date_time(LABOR_MHSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           
           SEX_INF1 = as.numeric(SEX_INF1), SEX_INF2 = as.numeric(SEX_INF2), SEX_INF3 = as.numeric(SEX_INF3), SEX_INF4 = as.numeric(SEX_INF4)
    ) %>%
    # replace default value date with NA 
    mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1== ymd("1907-07-07") | DELIV_DSSTDAT_INF1== ymd("2007-07-07"), NA),
           DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2==ymd("1907-07-07") | DELIV_DSSTDAT_INF2== ymd("2007-07-07"), NA),
           DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3==ymd("1907-07-07") | DELIV_DSSTDAT_INF3== ymd("2007-07-07"), NA),
           DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4==ymd("1907-07-07") | DELIV_DSSTDAT_INF4== ymd("2007-07-07"), NA),
           MEMBRANE_RUPT_MHSTDAT = replace(MEMBRANE_RUPT_MHSTDAT, MEMBRANE_RUPT_MHSTDAT==ymd("1907-07-07") | MEMBRANE_RUPT_MHSTDAT == ymd("2007-07-07"), NA),
           LABOR_MHSTDAT = replace(LABOR_MHSTDAT, LABOR_MHSTDAT==ymd("1907-07-07") | LABOR_MHSTDAT == ymd("2007-07-07") , NA)) %>%
    # replace default value time with NA 
    mutate(DELIV_DSSTTIM_INF1 = replace(DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF1=="77:77" | DELIV_DSSTTIM_INF1=="99:99", NA),  ## should be 77:77, but pak is using 07:07
           DELIV_DSSTTIM_INF2 = replace(DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF2=="77:77" | DELIV_DSSTTIM_INF2=="99:99", NA),
           DELIV_DSSTTIM_INF3 = replace(DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF3=="77:77" | DELIV_DSSTTIM_INF3=="99:99", NA),
           DELIV_DSSTTIM_INF4 = replace(DELIV_DSSTTIM_INF4, DELIV_DSSTTIM_INF4=="77:77" | DELIV_DSSTTIM_INF4=="99:99", NA),
           LABOR_MHSTTIM = replace(LABOR_MHSTTIM, LABOR_MHSTTIM=="77:77" | LABOR_MHSTTIM=="99:99", NA),
           MEMBRANE_RUPT_MHSTTIM = replace(MEMBRANE_RUPT_MHSTTIM, MEMBRANE_RUPT_MHSTTIM=="77:77" | MEMBRANE_RUPT_MHSTTIM=="99:99", NA)) %>%
    # Convert time to time format
    mutate(
      DELIV_DSSTTIM_INF1 = if_else(!is.na(DELIV_DSSTTIM_INF1), as.ITime(DELIV_DSSTTIM_INF1), NA),
      DELIV_DSSTTIM_INF2 = if_else(!is.na(DELIV_DSSTTIM_INF2), as.ITime(DELIV_DSSTTIM_INF2), NA),
      DELIV_DSSTTIM_INF3 = if_else(!is.na(DELIV_DSSTTIM_INF3), as.ITime(DELIV_DSSTTIM_INF3), NA),
      DELIV_DSSTTIM_INF4 = if_else(!is.na(DELIV_DSSTTIM_INF4), as.ITime(DELIV_DSSTTIM_INF4), NA),
      LABOR_MHSTTIM = if_else(!is.na(LABOR_MHSTTIM), as.ITime(LABOR_MHSTTIM), NA),
      MEMBRANE_RUPT_MHSTTIM = if_else(!is.na(MEMBRANE_RUPT_MHSTTIM), as.ITime(MEMBRANE_RUPT_MHSTTIM), NA)
    )%>%
    # Concatenate dates and times and convert to datetime format
    mutate(
      DELIVERY_DATETIME_INF1 = if_else(!is.na(DELIV_DSSTDAT_INF1) & !is.na(DELIV_DSSTTIM_INF1),
                                       as.POSIXct(paste(DELIV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1), format = "%Y-%m-%d %H:%M:%S"),
                                       DELIV_DSSTDAT_INF1),
      DELIVERY_DATETIME_INF2 = if_else(!is.na(DELIV_DSSTDAT_INF2) & !is.na(DELIV_DSSTTIM_INF2),
                                       as.POSIXct(paste(DELIV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2), format = "%Y-%m-%d %H:%M:%S"),
                                       DELIV_DSSTDAT_INF2),
      DELIVERY_DATETIME_INF3 = if_else(!is.na(DELIV_DSSTDAT_INF3) & !is.na(DELIV_DSSTTIM_INF3),
                                       as.POSIXct(paste(DELIV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3), format = "%Y-%m-%d %H:%M:%S"),
                                       DELIV_DSSTDAT_INF3),
      DELIVERY_DATETIME_INF4 = if_else(!is.na(DELIV_DSSTDAT_INF4) & !is.na(DELIV_DSSTTIM_INF4),
                                       as.POSIXct(paste(DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF4), format = "%Y-%m-%d %H:%M:%S"),
                                       DELIV_DSSTDAT_INF4),
      LABOUR_DATETIME = if_else(!is.na(LABOR_MHSTDAT) & !is.na(LABOR_MHSTTIM),
                                as.POSIXct(paste(LABOR_MHSTDAT, LABOR_MHSTTIM), format = "%Y-%m-%d %H:%M:%S"),
                                LABOR_MHSTDAT),
      MEM_RUPT_DATETIME = if_else(!is.na(MEMBRANE_RUPT_MHSTDAT) & !is.na(MEMBRANE_RUPT_MHSTTIM),
                                  as.POSIXct(paste(MEMBRANE_RUPT_MHSTDAT, MEMBRANE_RUPT_MHSTTIM), format = "%Y-%m-%d %H:%M:%S"),
                                  MEMBRANE_RUPT_MHSTDAT)) 
  
  
  names(mnh09_sub) <- gsub("(CES_PRINDC)_INF(\\d+)_(\\d+)", "\\1_\\3_INF\\2", names(mnh09_sub))
  
  # Getting the Date of Birth, Sex and Birth Outcome for Each ID
  infant_long <- mnh09_sub %>%
    # Pivot the data from wide to long format
    pivot_longer(
      # Select columns to pivot (INFANTID_INF1-4 and DELIVERY_DATETIME_INF1-4)
      cols <- c(
        "INFANTID_INF1", "INFANTID_INF2", "INFANTID_INF3", "INFANTID_INF4", "DELIV_DSSTDAT_INF1",
        "DELIV_DSSTDAT_INF2", "DELIV_DSSTDAT_INF3", "DELIV_DSSTDAT_INF4", "DELIV_DSSTTIM_INF1", "DELIV_DSSTTIM_INF2", 
        "DELIV_DSSTTIM_INF3", "DELIV_DSSTTIM_INF4", "BIRTH_DSTERM_INF1", "BIRTH_DSTERM_INF2", "BIRTH_DSTERM_INF3",
        "DELIVERY_DATETIME_INF1", "DELIVERY_DATETIME_INF2", "DELIVERY_DATETIME_INF3", "DELIVERY_DATETIME_INF4",
        "BIRTH_DSTERM_INF4", "SEX_INF1", "SEX_INF2", "SEX_INF3", "SEX_INF4",
        "CES_PRINDC_1_INF1", "CES_PRINDC_1_INF2", "CES_PRINDC_1_INF3", "CES_PRINDC_1_INF4",
        "CES_PRINDC_3_INF1", "CES_PRINDC_3_INF2", "CES_PRINDC_3_INF3", "CES_PRINDC_3_INF4",
        "CES_PRINDC_4_INF1", "CES_PRINDC_4_INF2", "CES_PRINDC_4_INF3", "CES_PRINDC_4_INF4",
        "CES_PRINDC_9_INF1", "CES_PRINDC_9_INF2", "CES_PRINDC_9_INF3", "CES_PRINDC_9_INF4", 
        "CES_PRINDC_15_INF1", "CES_PRINDC_15_INF2", "CES_PRINDC_15_INF3", "CES_PRINDC_15_INF4", 
        "DELIV_PRROUTE_INF1", "DELIV_PRROUTE_INF2", "DELIV_PRROUTE_INF3", "DELIV_PRROUTE_INF4"
      ),
      # Specify how to separate column names: extract suffixes and values
      names_to = c(".value", "infant_suffix"),
      # Define the pattern: splitting by "_INF" and matching the suffix
      names_pattern = "(.*)_INF(\\d)$"
    ) %>%
    # Rename the columns
    rename(
      INFANTID = INFANTID,
      DOB = DELIVERY_DATETIME,
      INF_ORDER = infant_suffix
    ) %>%
    # Filter out rows where INFANTID is NA
    filter(INFANTID != "" ) %>% 
    filter( INFANTID != "n/a" )
  
  infant_ga <- infant_long %>%
    left_join(EDD %>% select(MOMID, PREGID, EST_CONC_DATE, GA_DAYS, EDD), by = c("MOMID", "PREGID")) 
    
  infant_ga <- infant_ga %>% 
    mutate ( DOB_Date = as.Date(trunc(DOB, 'days')),  # Assuming DOB exists 
             GA_BIRTH = round(as.numeric(difftime(DOB_Date, EST_CONC_DATE, units = "days")), 0))
    
  delivery_query <- infant_ga %>% 
    mutate(Error = "",
           VARIABLE_NAME = "",
           VARIABLE_VALUE = NA) %>% 
    slice(0) %>% 
    mutate(across(everything(), as.character))
  
  ##Query 1: Failed induction as indication (CES_PRINDC_INF1_4) and induced labor (INDUCED_PROCCUR=1)
  
  failed_ind <- infant_ga %>% 
    filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
    mutate(Error = case_when(
      CES_PRINDC_4 == 1 & INDUCED_PROCCUR != 1 ~ "failed induction, labour not induced",
      TRUE ~ "No Error"
    ),
    VARIABLE_NAME = "INDUCED_PROCCUR",
    VARIABLE_VALUE = INDUCED_PROCCUR ) %>% 
    filter (Error != "No Error") %>% 
    mutate(across(everything(), as.character))
  
  
  if (nrow(failed_ind > 1)){
    delivery_query <- bind_rows (delivery_query, failed_ind)
  } 
  
 ##Query 2: Failure to progress as indication (CES_PRINDC_INF1_3) and experienced labor (LABOR_MHOCCUR=1)
  
  failed_labour <- infant_ga %>% 
    filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
    mutate(Error = case_when(
      CES_PRINDC_3 == 1 & LABOR_MHOCCUR != 1 ~ "failed to progress, no labor recorded",
      TRUE ~ "No Error"
    ),
    VARIABLE_NAME = "LABOR_MHOCCUR",
    VARIABLE_VALUE = LABOR_MHOCCUR ) %>% 
    filter (Error != "No Error") %>% 
    mutate(across(everything(), as.character))
  
  if (nrow(failed_labour > 1)){
    delivery_query <- bind_rows (delivery_query, failed_labour)
  } 
  
  ##Query 3: Post-term as indication (CES_PRINDC_INF1_1) and GA at pregnancy endpoint
  
  postterm_ind <- infant_ga %>% 
    filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
    mutate(Error = case_when(
      CES_PRINDC_1 == 1 & GA_BIRTH < 286 ~ "postterm indication, GA < 41wks",
      TRUE ~ "No Error"
    ),
    VARIABLE_NAME = "CONSTRUCTED GA_DELIVERY",
    VARIABLE_VALUE = GA_BIRTH ) %>% 
    filter (Error != "No Error") %>% 
    mutate(across(everything(), as.character))
  
  if (nrow(postterm_ind > 1)){
    delivery_query <- bind_rows (delivery_query, postterm_ind)
  } 
  
  ##Query 4: PMTCT as indication (CES_PRINDC_INF1_15) and HIV infection
  
  #pull all HIV infections
  
  mom_hiv <- tibble(MOMID = character(), PREGID = character())
  
  # Check if mnh04 exists
  if (exists("mnh04") == TRUE) {
    
    # Filter for HIV-related conditions in mnh04
    mnh04_hiv <- mnh04 %>% 
      filter(HIV_EVER_MHOCCUR == 1 | HIV_CMOCCUR == 1 | HIV_MHOCCUR == 1) %>% 
      select(MOMID, PREGID) %>% 
      distinct(PREGID, .keep_all = TRUE)
    
    # If there are more than zero rows in mnh04_hiv, bind the rows to mom_hiv
    if (nrow(mnh04_hiv) > 0) {
      mom_hiv <- bind_rows(mom_hiv, mnh04_hiv) %>% 
        distinct(PREGID, .keep_all = TRUE)
    }
  }
    
  if (exists("mnh06") == TRUE) {
    
    mnh06_hiv <- mnh06 %>% 
      filter (HIV_POC_LBORRES == 1) %>% 
      select (MOMID, PREGID) %>% 
      distinct(PREGID, .keep_all = TRUE)
    
    # If there are more than zero rows in mnh06_hiv, bind the rows to mom_hiv
    if (nrow(mnh06_hiv) > 0) {
      mom_hiv <- bind_rows(mom_hiv, mnh06_hiv) %>% 
        distinct(PREGID, .keep_all = TRUE)
    }
    
  }
  
  if (exists("mnh19") == TRUE) {
    
    mnh19_hiv <- mnh19 %>% 
      filter (HIV_LBORRES == 1) %>% 
      select (MOMID, PREGID) %>% 
      distinct(PREGID, .keep_all = TRUE)
    
    # If there are more than zero rows in mnh06_hiv, bind the rows to mom_hiv
    if (nrow(mnh19_hiv) > 0) {
      mom_hiv <- bind_rows(mom_hiv, mnh19_hiv) %>% 
        distinct(PREGID, .keep_all = TRUE)
    }
  }
  
  mom_hiv_vect <- mom_hiv$PREGID
    
  #query: if indicated and not in vector then query
  hiv_ind <- infant_ga %>% 
    filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
    mutate(Error = case_when(
      CES_PRINDC_15 == 1 & (!PREGID %in% mom_hiv_vect) ~ "hiv indication, no diagnosis",
      TRUE ~ "No Error"
    ),
    VARIABLE_NAME = paste0("CES_PRINDC_INF", INF_ORDER, "_15"),
    VARIABLE_VALUE = CES_PRINDC_15 ) %>% 
    filter (Error != "No Error") %>% 
    mutate(across(everything(), as.character))
  
  # If there are more than zero rows in mnh06_hiv, bind the rows to mom_hiv
  if (nrow(hiv_ind) > 0) {
    delivery_query <- bind_rows(delivery_query, hiv_ind) 
  }
  
  ##Query 5:No labor (LABOR_MHOCCUR = 0), but the pregnancy ends in a vaginal delivery (DELIV_PRROUTE_INF1 = 1)
  
  labour_vagd <- infant_ga %>% 
    filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
    mutate(Error = case_when(
      LABOR_MHOCCUR == 0 & DELIV_PRROUTE == 1 ~ "vaginal delivery , no labour",
      TRUE ~ "No Error"
    ),
    VARIABLE_NAME = "LABOR_MHOCCUR",
    VARIABLE_VALUE = LABOR_MHOCCUR ) %>% 
    filter (Error != "No Error") %>% 
    mutate(across(everything(), as.character))
  
  if (nrow(labour_vagd) > 0) {
    delivery_query <- bind_rows(delivery_query, labour_vagd) 
  }
  
  # ##Query 6:Missing membrane rupture information
  # 
  # rupture_vagd <- infant_ga %>% 
  #   filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
  #   mutate(Error = case_when(
  #     MEMBRANE_RUPT_MHTERM == 77 & DELIV_PRROUTE == 1 ~ "vaginal delivery , missing rupture info",
  #     TRUE ~ "No Error"
  #   ),
  #   VARIABLE_NAME = "MEMBRANE_RUPT_MHTERM",
  #   VARIABLE_VALUE = MEMBRANE_RUPT_MHTERM ) %>% 
  #   filter (Error != "No Error") %>% 
  #   mutate(across(everything(), as.character))
  # 
  # if (nrow(rupture_vagd) > 0) {
  #   delivery_query <- bind_rows(delivery_query, rupture_vagd) 
  # }
  
  
  ##Query 7:Among twins, query if 1st infant is c-section (DELIV_PRROUTE_INF1 = 2)
  #         2nd infant is vaginal delivery (DELIV_PRROUTE_INF2 = 1)
  
  # Create a flag to check if the delivery route is ordered correctly for twins/triplets
  inf_del <- infant_ga %>%
    filter (INFANTS_FAORRES > 1) %>%
    arrange(PREGID, INF_ORDER) %>%
    group_by(PREGID) %>%
    mutate(
      Error = case_when(
        INF_ORDER > 1 & (lag(DELIV_PRROUTE) == 2 & DELIV_PRROUTE == 1) ~ "multiple birth: c-section before vaginal",
        TRUE ~ "Delivery Order Correct"
      ),
      VARIABLE_NAME = "DELIV_PRROUTE",
      VARIABLE_VALUE = DELIV_PRROUTE 
    ) %>%
    ungroup() %>%
    filter(Error != "Delivery Order Correct") %>% 
    mutate(across(everything(), as.character))
  
  if (nrow(inf_del) > 0) {
    delivery_query <- bind_rows(delivery_query, inf_del) 
  }
  
  
  ##Query 8: If labor occurred, then date/time of labor onset should be prior to date/time of delivery 
  
  lab_b4_vagd <- infant_ga %>% 
    filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
    mutate(Error = case_when(
      LABOR_MHOCCUR == 1 & LABOUR_DATETIME > DOB ~ "delivery date-time before labour",
      TRUE ~ "No Error"
    ),
    VARIABLE_NAME = "CONSTRUCTED LABOUR_DATETIME",
    VARIABLE_VALUE = LABOUR_DATETIME ) %>% 
    filter (Error != "No Error") %>% 
    mutate(across(everything(), as.character))
  
  if (nrow(lab_b4_vagd) > 0) {
    delivery_query <- bind_rows(delivery_query, lab_b4_vagd) 
  }
  
  ##Query 10: If membrane rupture occurred, then date/time of rupture onset should be prior to date/time of delivery 
  
  rup_b4_vagd <- infant_ga %>% 
    filter(MAT_VISIT_MNH09 %in% c(1, 2)) %>% 
    mutate(Error = case_when(
      MEMBRANE_RUPT_MHTERM == 1 & MEM_RUPT_DATETIME > DOB ~ "delivery date-time before membrane rupture",
      TRUE ~ "No Error"
    ),
    VARIABLE_NAME = "CONSTRUCTED MEM_RUPT_DATETIME",
    VARIABLE_VALUE = MEM_RUPT_DATETIME ) %>% 
    filter (Error != "No Error") %>% 
    mutate(across(everything(), as.character))
  
  if (nrow(rup_b4_vagd) > 0) {
    delivery_query <- bind_rows(delivery_query, rup_b4_vagd) 
  }
  
} else { 
  print(paste0("No MNH09 uploaded ", site))
  stop("Process stopped due to missing delivery form.")
}


if (exists("delivery_query") == TRUE) {

  # extract variables included in query template
  delivery_query_to_export <- delivery_query %>%
    mutate(SCRNID = NA,
           TYPE_VISIT = NA,
           FIELD_TYPE = "Number",
           EDIT_TYPE = "Inconsistent L&D Event Record",
           VISITDATE = MAT_LD_OHOSTDAT,
           FORM = "MNH09") %>%
    select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VISITDATE, FORM, VARIABLE_NAME, VARIABLE_VALUE, FIELD_TYPE, EDIT_TYPE)

  # update naming
  names(delivery_query_to_export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")

  LabourDelivery_query_extra <- delivery_query  %>% 
    mutate(`VAR TO CHECK` = case_when(
      Error == "failed induction, labour not induced" ~ paste0("CES_PRINDC_INF", INF_ORDER, "_4 / INDUCED_PROCCUR"),
      Error == "failed to progress, no labor recorded" ~  paste0("CES_PRINDC_INF", INF_ORDER, "_3 / LABOR_MHOCCUR"),
      Error == "postterm indication, GA < 41wks" ~  paste0("CES_PRINDC_INF", INF_ORDER, "_1 / Gestational Age At Birth"),
      Error == "hiv indication, no diagnosis" ~ paste0("CES_PRINDC_INF", INF_ORDER, "_15 / HIV status check"),
      Error == "vaginal delivery , no labour" ~  paste0("LABOR_MHOCCUR / DELIV_PRROUTE_INF", INF_ORDER),
      Error == "delivery date-time before labour" ~ paste0("LABOUR_DATETIME / DOB_INF", INF_ORDER),
      Error == "delivery date-time before membrane rupture" ~ paste0("MEM_RUPT_DATETIME / DOB_INF", INF_ORDER),
      Error == "multiple birth: c-section before vaginal" ~ paste0("DELIV_PRROUTE_INF", INF_ORDER),
      TRUE ~ "No issues"
    )) %>% 
    mutate( FORM = "MNH09",
            QueryID = paste0(FORM, "_",MAT_LD_OHOSTDAT, "_", PREGID, "_", VARIABLE_NAME , "_", VARIABLE_VALUE, "_", "07")) %>%
    select (QueryID, MOMID, PREGID, INFANTID, ERROR = Error, `VAR TO CHECK`, GA_BIRTH, DOB, LABOUR_DATETIME,
            MEM_RUPT_DATETIME, INDUCED_PROCCUR, LABOR_MHOCCUR, DELIV_PRROUTE) %>% 
    filter (ERROR != "No issues")
  

  ## add additional columns
  delivery_query_to_export = cbind(QueryID = NA,
                                   UploadDate = UploadDate,
                                   delivery_query_to_export,
                                   DateEditReported = format(Sys.time(), "%Y-%m-%d"))

  # combine form/edit type var
  delivery_query_to_export$Form_Edit_Type <- paste(delivery_query_to_export$Form,"_",delivery_query_to_export$EditType)

  ## assign queryid 
  delivery_query_to_export <- delivery_query_to_export %>%
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",PregID, "_",`Variable Name`, "_", `Variable Value`, "_", "07"))

  LabourDelivery_query <- delivery_query_to_export
  
  ## export variable checking query
  save(LabourDelivery_query, file = "LabourDelivery_query.rda")
  
  save(LabourDelivery_query_extra, file = "LabourDelivery_query_comments.rda")
  
  print ("Labour and Delivery Query Ran Succesfully")
  
} else { 
  
  print(paste0("No L&D Query for ", site))
  
  stop("Process stopped due to no L&D Query.")
  
}

# write.xlsx(list("Query Report" = LabourDelivery_query, "L&D Errors" = LabourDelivery_query_extra), 
#            file = paste0("queries", "/", site, "_L&D_query_report.xlsx"))
