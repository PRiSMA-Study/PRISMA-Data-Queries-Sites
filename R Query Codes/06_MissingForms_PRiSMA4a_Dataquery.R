#*QUERY #6 -- CHECK FOR MISSING VISITS AND FORMS 
#* Written by: Precious Williams

#* Date Started: 21 June 2023
#* Last Updated: 7 May 2024
#* Updated queries to compile all visits into one line of query
#* developed a comments rda file
#* Updated the GA generation codes to include all India Sites specifications
#* Updated code to account for mnh01 ANC32 issues and India sites uploading data from 15 days ago
#* Updated fetal loss date

rm(list = ls())

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

setwd(path_to_save)
#Step 0 - if MOMID or PREGID is not present in MNH01, we want to transfer the MOMID and PREGIDs from MNH02 into MNH01 by SCRNID

#Check for different variations of "N/A" in the MOMID column

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

#Calculating GA and choosing the "latest" possible GA 
India_Sites <- c("India_CMC", "India-CMC", "India_SAS", "India-SAS")

if (!(site %in% India_Sites)) { 
  mnh01_mod <- mnh01 %>% mutate (GA_US_DAYS_FTS1 =  ifelse(US_GA_WKS_AGE_FTS1!= -7 & US_GA_DAYS_AGE_FTS1 != -7,  (US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1), NA), 
                                 GA_US_DAYS_FTS2 =  ifelse(US_GA_WKS_AGE_FTS2!= -7 & US_GA_DAYS_AGE_FTS2 != -7,  (US_GA_WKS_AGE_FTS2 * 7 + US_GA_DAYS_AGE_FTS2), NA),
                                 GA_US_DAYS_FTS3 =  ifelse(US_GA_WKS_AGE_FTS3!= -7 & US_GA_DAYS_AGE_FTS3 != -7,  (US_GA_WKS_AGE_FTS3 * 7 + US_GA_DAYS_AGE_FTS3), NA),
                                 GA_US_DAYS_FTS4 =  ifelse(US_GA_WKS_AGE_FTS4!= -7 & US_GA_DAYS_AGE_FTS4 != -7,  (US_GA_WKS_AGE_FTS4 * 7 + US_GA_DAYS_AGE_FTS4), NA),
                                 GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE),
                                 GA_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4,  na.rm = TRUE )) 
  
  #Choosing the earliest EDD Date, for multiple pregnancies
  mnh01_mod <- mnh01_mod %>% mutate(US_EDD_BRTHDAT_FTS1 = replace(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS1== ymd("1907-07-07") | US_EDD_BRTHDAT_FTS1== ymd("2007-07-07") , NA), 
                                    US_EDD_BRTHDAT_FTS2 = replace(US_EDD_BRTHDAT_FTS2, US_EDD_BRTHDAT_FTS2==ymd("1907-07-07") | US_EDD_BRTHDAT_FTS2== ymd("2007-07-07"), NA),
                                    US_EDD_BRTHDAT_FTS3 = replace(US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS3== ymd("1907-07-07")| US_EDD_BRTHDAT_FTS3== ymd("2007-07-07"), NA), 
                                    US_EDD_BRTHDAT_FTS4 = replace(US_EDD_BRTHDAT_FTS4, US_EDD_BRTHDAT_FTS4==ymd("1907-07-07")| US_EDD_BRTHDAT_FTS4== ymd("2007-07-07"), NA),
                                    ESTIMATED_EDD_SCDAT = replace(ESTIMATED_EDD_SCDAT, ESTIMATED_EDD_SCDAT==ymd("1907-07-07")| ESTIMATED_EDD_SCDAT== ymd("2007-07-07"), NA)) %>% 
    mutate(EDD = pmin(US_EDD_BRTHDAT_FTS1, US_EDD_BRTHDAT_FTS2, 
                      US_EDD_BRTHDAT_FTS3, US_EDD_BRTHDAT_FTS4,  na.rm = TRUE))
  
  mnh01_mod$EDD <- ymd(parse_date_time(mnh01_mod$EDD, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  # Filter all participants that met the inclusion criteria in MNH process M02 data
  Eligible <- mnh02 %>% filter (CONSENT_IEORRES == 1 & AGE_IEORRES == 1  & PC_IEORRES == 1  & CATCHMENT_IEORRES == 1  & CATCH_REMAIN_IEORRES == 1 ) %>%
    mutate (Eligible = 1) %>% select(SCRNID, Eligible)  %>%
    distinct(SCRNID, .keep_all = TRUE)
  
  #Step 0.5a:Developing a Masterlist of Enrolled Individuals
  Screened <- mnh01_mod %>% filter (GA_DAYS <= 139 & GA_DAYS != 0 & MAT_VISIT_MNH01 %in% c(1,2) ) %>% 
    select (MOMID, PREGID, SCRNID, US_OHOSTDAT, EDD, GA_DAYS, TYPE_VISIT, ESTIMATED_EDD_SCDAT)  %>%
    distinct(MOMID, PREGID, .keep_all = TRUE) 
  
  Enroled <- merge(Screened, Eligible, by = c("SCRNID"))  %>% select (-c("TYPE_VISIT"))  %>%
    distinct(MOMID, PREGID, .keep_all = TRUE)
  
  #Use the Ultrasound GA to calculate An Estimated EDD (EDD_EST)
  EDD <- Enroled %>%
    mutate( UPLOADDT = as.Date(UploadDate),
            EST_CONC_DATE = US_OHOSTDAT - days(GA_DAYS), 
            EDD_EST = EST_CONC_DATE + days(280), 
            DIFF = abs(as.numeric(difftime(EDD, EDD_EST, units = "days")))) %>%
    select( MOMID, SCRNID, PREGID, VisitDate = US_OHOSTDAT, EDD,
            UPLOADDT, EDD_EST, GA_DAYS, EST_CONC_DATE, DIFF) %>%
    distinct(MOMID, PREGID, .keep_all = TRUE) %>% filter (GA_DAYS < 175)
  
  Enrolled <- Enroled %>% mutate(GA_TODAY = round(GA_DAYS + as.numeric(difftime(UploadDate, US_OHOSTDAT, units = "days"))))  %>%
    filter (GA_TODAY >= 139) 
  
} 

if (site %in% India_Sites) {
  mnh01_mod <- mnh01 %>% 
    ## extract the maximum gestational age for each woman 
    mutate(GA_US_DAYS_FTS1 =  ifelse(CAL_GA_WKS_AGE_FTS1!= -7 & CAL_GA_DAYS_AGE_FTS1 != -7,  (CAL_GA_WKS_AGE_FTS1 * 7 + CAL_GA_DAYS_AGE_FTS1), NA), 
           GA_US_DAYS_FTS2 =  ifelse(CAL_GA_WKS_AGE_FTS2!= -7 & CAL_GA_DAYS_AGE_FTS2 != -7,  (CAL_GA_WKS_AGE_FTS2 * 7 + CAL_GA_DAYS_AGE_FTS2), NA),
           GA_US_DAYS_FTS3 =  ifelse(CAL_GA_WKS_AGE_FTS3!= -7 & CAL_GA_DAYS_AGE_FTS3 != -7,  (CAL_GA_WKS_AGE_FTS3 * 7 + CAL_GA_DAYS_AGE_FTS3), NA),
           GA_US_DAYS_FTS4 =  ifelse(CAL_GA_WKS_AGE_FTS4!= -7 & CAL_GA_DAYS_AGE_FTS4 != -7,  (CAL_GA_WKS_AGE_FTS4 * 7 + CAL_GA_DAYS_AGE_FTS4), NA)) %>% 
    mutate(GA_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>%
    mutate(CAL_EDD_BRTHDAT_FTS1 = replace(CAL_EDD_BRTHDAT_FTS1, CAL_EDD_BRTHDAT_FTS1== ymd("1907-07-07") | CAL_EDD_BRTHDAT_FTS1== ymd("2007-07-07") , NA), 
           CAL_EDD_BRTHDAT_FTS2 = replace(CAL_EDD_BRTHDAT_FTS2, CAL_EDD_BRTHDAT_FTS2==ymd("1907-07-07") | CAL_EDD_BRTHDAT_FTS2== ymd("2007-07-07"), NA),
           CAL_EDD_BRTHDAT_FTS3 = replace(CAL_EDD_BRTHDAT_FTS3, CAL_EDD_BRTHDAT_FTS3== ymd("1907-07-07")| CAL_EDD_BRTHDAT_FTS3== ymd("2007-07-07"), NA), 
           CAL_EDD_BRTHDAT_FTS4 = replace(CAL_EDD_BRTHDAT_FTS4, CAL_EDD_BRTHDAT_FTS4==ymd("1907-07-07")| CAL_EDD_BRTHDAT_FTS4== ymd("2007-07-07"), NA)) %>%
    mutate(EDD = pmin(CAL_EDD_BRTHDAT_FTS1, CAL_EDD_BRTHDAT_FTS2, 
                      CAL_EDD_BRTHDAT_FTS3, CAL_EDD_BRTHDAT_FTS4,  na.rm = TRUE)) %>%
    filter (!is.na(EDD) & TYPE_VISIT == 1)
  
  mnh01_mod$EDD <- ymd(parse_date_time(mnh01_mod$EDD, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  # Filter all participants that met the inclusion criteria in MNH process M02 data
  Eligible <- mnh02 %>% filter (CONSENT_IEORRES == 1 & AGE_IEORRES == 1  & PC_IEORRES == 1  & CATCHMENT_IEORRES == 1  & CATCH_REMAIN_IEORRES == 1 ) %>%
    mutate (Eligible = 1) %>% select(SCRNID, Eligible)  %>%
    distinct(SCRNID, .keep_all = TRUE)
  
  #Step 0.5a:Developing a Masterlist of Enrolled Individuals
  Screened <- mnh01_mod %>% filter (GA_DAYS <= 139 & GA_DAYS != 0 & MAT_VISIT_MNH01 %in% c(1,2) ) %>% 
    select (MOMID, PREGID, SCRNID, US_OHOSTDAT, EDD, GA_DAYS, TYPE_VISIT, ESTIMATED_EDD_SCDAT)  %>%
    distinct(MOMID, PREGID, .keep_all = TRUE) 
  
  Enroled <- merge(Screened, Eligible, by = c("SCRNID"))  %>% select (-c("TYPE_VISIT"))  %>%
    distinct(MOMID, PREGID, .keep_all = TRUE)
  
  #Use the Ultrasound GA to calculate An Estimated EDD (EDD_EST)
  EDD <- Enroled %>%
    mutate( UploadDate = as.Date(UploadDate),
            UPLOADDT = UploadDate - 15, #India site reporting uploading data from 15days ago
            EST_CONC_DATE = US_OHOSTDAT - days(GA_DAYS), 
            EDD_EST = EST_CONC_DATE + days(280), 
            DIFF = abs(as.numeric(difftime(EDD, EDD_EST, units = "days")))) %>%
    select( MOMID, SCRNID, PREGID, VisitDate = US_OHOSTDAT, EDD,
            UPLOADDT, EDD_EST, GA_DAYS, EST_CONC_DATE, DIFF) %>%
    distinct(MOMID, PREGID, .keep_all = TRUE) %>% filter (GA_DAYS < 175)
  
  Enrolled <- Enroled %>% mutate(GA_TODAY = round(GA_DAYS + as.numeric(difftime(UploadDate, US_OHOSTDAT, units = "days"))))  %>%
    filter (GA_TODAY >= 139) 
}


#**************************************************************************************************************
#PART B: MISSING FORM QUERY
# Create date bounds to get a lower bound and upper bound visit time period
edd_date <- EDD %>%
  mutate(
    LW20 = EST_CONC_DATE + days(160), 
    LW28 = EST_CONC_DATE + days(216),
    LW32 = EST_CONC_DATE + days(237),
    LW36 = EST_CONC_DATE + days(272),
    UP20 = EST_CONC_DATE + days(181), 
    UP28 = EST_CONC_DATE + days(216), 
    UP32 = EST_CONC_DATE + days(237), 
    UP36 = EST_CONC_DATE + days(272)) %>% 
  select(MOMID, PREGID, VisitDate, UPLOADDT, EDD, EDD_EST, GA_DAYS, DIFF, EST_CONC_DATE,
         LW20, LW28, LW32, LW36, UP20, UP28, UP32, UP36) %>%  
  mutate(MOMID = ifelse(MOMID %in% na_variations, NA, MOMID),
         PREGID = ifelse(PREGID %in% na_variations, NA, PREGID)) %>%
  filter(complete.cases(MOMID, PREGID)) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE)

#Create a due dates for ANC20, 3, 4, 5 timeline based on the calculations FALSE - not due, TRUE - due period, 2 - past due
edd_date$DUE20 <- ifelse ((edd_date$UPLOADDT > edd_date$UP20) & (edd_date$GA_DAYS <= 125), TRUE, FALSE) 
edd_date$DUE28 <- ifelse (edd_date$UPLOADDT > edd_date$UP28, TRUE, FALSE)
edd_date$DUE32 <- ifelse (edd_date$UPLOADDT > edd_date$UP32, TRUE, FALSE)
edd_date$DUE36 <- ifelse (edd_date$UPLOADDT > edd_date$UP36, TRUE, FALSE)

if (exists("mnh09")==TRUE){
  # Parse date for MAT_LD_OHOSTDAT in the appropriate formats
  mnh09$MAT_LD_OHOSTDAT <- ymd(parse_date_time(mnh09$MAT_LD_OHOSTDAT, 
                                               c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y")))
  
  # Left join mnh09 with EDD, calculate gestational age (GA_DELIV), and categorize prematurity status
  delivered_moms <- mnh09 %>% 
    left_join(EDD, by = c("MOMID", "PREGID")) %>% 
    mutate(GA_DELIV = 280 - (as.integer(difftime(EDD, MAT_LD_OHOSTDAT, units = "days"))),
           Delivered = 1,
           Premies = case_when(
             GA_DELIV <= 181 ~ "Yes_20",
             GA_DELIV >= 182 & GA_DELIV <= 216 ~ "Yes_28",
             GA_DELIV >= 217 & GA_DELIV <= 237 ~ "Yes_32",
             GA_DELIV >= 238 & GA_DELIV <= 272 ~ "Yes_36",
             TRUE ~ "No")) %>% 
    select(MOMID, PREGID, GA_DELIV, Premies, Delivered, GA_DAYS, VisitDate, MAT_LD_OHOSTDAT)
  
  # Create subsets for different prematurity categories
  premies_20_ <- delivered_moms %>% filter(Premies == "Yes_20") %>% select(MOMID, PREGID)
  premies_28_ <- delivered_moms %>% filter(Premies %in% c("Yes_20", "Yes_28")) %>% select(MOMID, PREGID)
  premies_32_ <- delivered_moms %>% filter(Premies %in% c("Yes_20", "Yes_28", "Yes_32")) %>% select(MOMID, PREGID)
  premies_36_ <- delivered_moms %>% filter(Premies %in% c("Yes_20", "Yes_28", "Yes_32", "Yes_36")) %>% select(MOMID, PREGID)
  miss_36_d <- delivered_moms %>% filter(Premies == "Yes_36") %>% mutate(Delivered = 1) %>% select(MOMID, PREGID, Delivered)
  
  # Subset and mutate mnh09 data with date and time conversions
  mnh09_sub <- mnh09 %>%
    select(MOMID, PREGID, MAT_VISIT_MNH09, LABOR_MHOCCUR, LABOR_MHSTTIM, LABOR_MHSTDAT, INDUCED_PROCCUR,
           MEMBRANE_RUPT_MHTERM, MEMBRANE_RUPT_MHSTTIM, MEMBRANE_RUPT_MHSTDAT, INFANTS_FAORRES, MAT_LD_OHOSTDAT,
           INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4, DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, 
           DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF3, 
           DELIV_DSSTTIM_INF4, BIRTH_DSTERM_INF1, BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3, BIRTH_DSTERM_INF4, 
           SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4, CES_PRINDC_INF1_1, CES_PRINDC_INF2_1, CES_PRINDC_INF3_1, 
           CES_PRINDC_INF4_1, CES_PRINDC_INF1_3, CES_PRINDC_INF2_3, CES_PRINDC_INF3_3, CES_PRINDC_INF4_3, 
           CES_PRINDC_INF1_4, CES_PRINDC_INF2_4, CES_PRINDC_INF3_4, CES_PRINDC_INF4_4, CES_PRINDC_INF1_9, 
           CES_PRINDC_INF2_9, CES_PRINDC_INF3_9, CES_PRINDC_INF4_9, CES_PRINDC_INF1_15, CES_PRINDC_INF2_15, 
           CES_PRINDC_INF3_15, CES_PRINDC_INF4_15, DELIV_PRROUTE_INF1, DELIV_PRROUTE_INF2, DELIV_PRROUTE_INF3, 
           DELIV_PRROUTE_INF4) %>%
    
    # Date parsing and conversion
    mutate(across(c(DELIV_DSSTDAT_INF1:DELIV_DSSTDAT_INF4), 
                  ~ ymd(parse_date_time(.x, c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))))) %>%
    
    # Replace default dates and times with NA
    mutate(across(starts_with("DELIV_DSSTDAT_INF"), 
                  ~ replace(.x, .x %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA)),
           across(starts_with("DELIV_DSSTTIM_INF"), 
                  ~ replace(.x, .x == "77:77", NA))) %>%
    
    # Time conversion
    mutate(across(starts_with("DELIV_DSSTTIM_INF"), ~ if_else(!is.na(.x), as.ITime(.x), NA))) %>%
    
    # Concatenate date and time into datetime
    mutate(DELIVERY_DATETIME_INF1 = if_else(!is.na(DELIV_DSSTDAT_INF1) & !is.na(DELIV_DSSTTIM_INF1),
                                            as.POSIXct(paste(DELIV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1), format = "%Y-%m-%d %H:%M:%S"), 
                                            NA),
           DELIVERY_DATETIME_INF2 = if_else(!is.na(DELIV_DSSTDAT_INF2) & !is.na(DELIV_DSSTTIM_INF2),
                                            as.POSIXct(paste(DELIV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2), format = "%Y-%m-%d %H:%M:%S"), 
                                            NA),
           DELIVERY_DATETIME_INF3 = if_else(!is.na(DELIV_DSSTDAT_INF3) & !is.na(DELIV_DSSTTIM_INF3),
                                            as.POSIXct(paste(DELIV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3), format = "%Y-%m-%d %H:%M:%S"), 
                                            NA),
           DELIVERY_DATETIME_INF4 = if_else(!is.na(DELIV_DSSTDAT_INF4) & !is.na(DELIV_DSSTTIM_INF4),
                                            as.POSIXct(paste(DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF4), format = "%Y-%m-%d %H:%M:%S"), 
                                            NA)) %>% 
    # Define maternal delivery date as the earliest infant delivery date
    mutate(MAT_DELIVERY_DATE = pmin(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4, na.rm = TRUE))
  
  #create maternal pnc due dates
  due_mat_inf <- mnh09_sub %>% 
    filter (BIRTH_DSTERM_INF1 == 1 | BIRTH_DSTERM_INF2 == 1 |
              BIRTH_DSTERM_INF3 == 1 | BIRTH_DSTERM_INF4 == 1 ) %>% 
    mutate (
      # Calculate due dates for each visit based on latest window
      UP00 = MAT_DELIVERY_DATE + 5,  # 3 to 5 days window
      UP01 = MAT_DELIVERY_DATE + 14, # 7 to 14 days window
      UP04 = MAT_DELIVERY_DATE + 35, # 28 to 35 days window
      UP06 = MAT_DELIVERY_DATE + 104, # 6 to 7 weeks window
      UP26 = MAT_DELIVERY_DATE + 279, # 26 to 28 weeks window
      UP52 = MAT_DELIVERY_DATE + 454, # 52 to 54 weeks window
      VisitDate = MAT_LD_OHOSTDAT,
      UPLOADDT = UploadDate) %>%  
    select(MOMID, PREGID, INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4,
           MAT_DELIVERY_DATE, VisitDate, UPLOADDT, UP00, UP01, UP04, UP06, UP26, UP52) %>%  
    distinct(MOMID, PREGID, .keep_all = TRUE)
  
  #Create a due dates for all maternal PNC visits based on timeline based on the calculations FALSE - not due, TRUE - due period, 2 - past due
  due_mat_inf$DUE00 <- ifelse (due_mat_inf$UPLOADDT > due_mat_inf$UP00, TRUE, FALSE)
  due_mat_inf$DUE01 <- ifelse (due_mat_inf$UPLOADDT > due_mat_inf$UP01, TRUE, FALSE)
  due_mat_inf$DUE04 <- ifelse (due_mat_inf$UPLOADDT > due_mat_inf$UP04, TRUE, FALSE)
  due_mat_inf$DUE06 <- ifelse (due_mat_inf$UPLOADDT > due_mat_inf$UP06, TRUE, FALSE)
  due_mat_inf$DUE26 <- ifelse (due_mat_inf$UPLOADDT > due_mat_inf$UP26, TRUE, FALSE)
  due_mat_inf$DUE52 <- ifelse (due_mat_inf$UPLOADDT > due_mat_inf$UP52, TRUE, FALSE)
  
  # Pivot infant delivery data into long format
  infant_dob <- mnh09_sub %>%
    pivot_longer(cols = starts_with("INFANTID") | starts_with("DELIVERY_DATETIME") | starts_with("BIRTH_DSTERM") | starts_with("SEX"), 
                 names_to = c(".value", "infant_suffix"), 
                 names_pattern = "(.*)_INF(\\d)") %>%
    rename(INFANTID = INFANTID, DOB = DELIVERY_DATETIME) %>%
    select(MOMID, PREGID, INFANTID, DOB, BIRTH_DSTERM, SEX) %>%
    filter(INFANTID != "" & INFANTID != "n/a")
  
  #living infant
  inf_live <- infant_dob %>% filter (BIRTH_DSTERM == 1)
  
  # Filter living infants
  due_inf <- inf_live %>%
    mutate(
      # Ensure DOB is treated as a Date object and add days for each visit window
      LW00 = as.Date(DOB) + 3,   # 3 to 5 days window
      LW01 = as.Date(DOB) + 7,  # 7 to 14 days window
      LW04 = as.Date(DOB) + 28,  # 28 to 35 days window

      # Ensure DOB is treated as a Date object and add days for each visit window
      UP00 = as.Date(DOB) + 5,   # 3 to 5 days window
      UP01 = as.Date(DOB) + 14,  # 7 to 14 days window
      UP04 = as.Date(DOB) + 35,  # 28 to 35 days window
      UP06 = as.Date(DOB) + 104,  # 6 to 7 weeks window
      UP26 = as.Date(DOB) + 279, # 26 to 28 weeks window
      UP52 = as.Date(DOB) + 454, # 52 to 54 weeks window
      BILI_STARTDATE = case_when(
        site == "Ghana" ~ Sys.Date(),  # Today's date for Ghana
        site %in% c( "India-CMC", "India_CMC") ~ as.Date("2024-11-13"),
        site %in% c( "India-SAS", "India_SAS") ~ as.Date("2024-12-03"),
        site == "Kenya" ~ as.Date("2025-01-13"),
        site == "Pakistan" ~ as.Date("2024-10-24"),
        site == "Zambia" ~ as.Date("2024-11-04"),
        TRUE ~ NA_Date_ ),
      UPLOADDT = UploadDate) %>%
    select(MOMID, PREGID, INFANTID, DOB, UPLOADDT, BILI_STARTDATE, starts_with("UP"), starts_with("LW")) %>%
    distinct(INFANTID, PREGID, .keep_all = TRUE)
  
  due_inf$DUE00 <- ifelse (due_inf$UPLOADDT > due_inf$UP00, TRUE, FALSE)
  due_inf$DUE01 <- ifelse (due_inf$UPLOADDT > due_inf$UP01, TRUE, FALSE)
  due_inf$DUE04 <- ifelse (due_inf$UPLOADDT > due_inf$UP04, TRUE, FALSE)
  due_inf$DUE06 <- ifelse (due_inf$UPLOADDT > due_inf$UP06, TRUE, FALSE)
  due_inf$DUE26 <- ifelse (due_inf$UPLOADDT > due_inf$UP26, TRUE, FALSE)
  due_inf$DUE52 <- ifelse (due_inf$UPLOADDT > due_inf$UP52, TRUE, FALSE)
  
  
  due_inf$DUE00_BILI <- ifelse (due_inf$BILI_STARTDATE < due_inf$LW00, TRUE, FALSE)
  due_inf$DUE01_BILI <- ifelse (due_inf$BILI_STARTDATE < due_inf$LW01, TRUE, FALSE)
  due_inf$DUE04_BILI <- ifelse (due_inf$BILI_STARTDATE < due_inf$LW04, TRUE, FALSE)

}

if (exists("mnh23")==TRUE){
  
  mnh23$CLOSE_DSSTDAT <- ymd(parse_date_time(mnh23$CLOSE_DSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  mnh23$DTHDAT <- ymd(parse_date_time(mnh23$DTHDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y")))
  
  censored_moms <- mnh23 %>%
    left_join(EDD, by = c("MOMID", "PREGID")) %>%
    mutate(
      CLOSE_DSSTDAT = if_else(CLOSE_DSSTDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA_Date_, CLOSE_DSSTDAT),
      DTHDAT = if_else(DTHDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA_Date_, DTHDAT),
      CLOSE_DAT = pmin(DTHDAT, CLOSE_DSSTDAT, na.rm = TRUE) # Find the earliest date
    ) %>%
    mutate(
      GA_CEN = ifelse(!is.na(GA_DAYS) & !is.na(CLOSE_DAT) & !is.na(VisitDate),
                      GA_DAYS + (as.integer(difftime(CLOSE_DAT, VisitDate, units = "days"))), NA),
      Censored = 1,
      Cen_Time = case_when(
        is.na (CLOSE_DSSTDAT) ~ "Missing",  # Handle missing values
        GA_CEN >= 273 ~ "No",
        GA_CEN <= 181 ~ "Yes_20",
        GA_CEN >= 182 & GA_CEN <= 216 ~ "Yes_28",
        GA_CEN >= 217 & GA_CEN <= 237 ~ "Yes_32",
        GA_CEN >= 238 & GA_CEN <= 272 ~ "Yes_36",
        TRUE ~ "Remove_All"
      )
    ) %>%
    select(MOMID, PREGID, GA_CEN, Censored, Cen_Time, GA_DAYS, VisitDate, CLOSE_DSSTDAT, DTHDAT, CLOSE_DAT )
  
  
  cens_20_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All", "Yes_20")) %>% select(MOMID, PREGID)
  cens_28_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All","Yes_20", "Yes_28")) %>% select(MOMID, PREGID)
  cens_32_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All","Yes_20", "Yes_28", "Yes_32")) %>% select(MOMID, PREGID)
  cens_36_ <- censored_moms %>% filter(censored_moms$Cen_Time %in% c("Remove_All","Yes_20", "Yes_28", "Yes_32", "Yes_36")) %>% select(MOMID, PREGID)
  
}


if (exists("mnh24")==TRUE){
  
  # Convert date columns with appropriate formats
  mnh24$CLOSE_DSSTDAT <- ymd(parse_date_time(mnh24$CLOSE_DSSTDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y")))
  mnh24$DTHDAT <- ymd(parse_date_time(mnh24$DTHDAT, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y")))
  
  # Create the censored_inf dataframe with correct date differences
  censored_inf <- mnh24 %>%
    left_join(infant_dob, by = c("INFANTID", "PREGID", "MOMID")) %>%
    select(MOMID, PREGID, INFANTID, CLOSE_DSSTDAT, DTHDAT, DOB) %>%
    mutate(
      CLOSE_DSSTDAT = if_else(CLOSE_DSSTDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA_Date_, CLOSE_DSSTDAT),
      DTHDAT = if_else(DTHDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA_Date_, DTHDAT),
      CLOSE_DAT = pmin(DTHDAT, CLOSE_DSSTDAT, na.rm = TRUE) # Find the earliest date
    ) %>%
    mutate(
      Age = as.integer(difftime(CLOSE_DAT, DOB, units = "days")), # Calculate age in days
      cens_time = case_when(
        is.na(CLOSE_DSSTDAT) ~ "Missing",
        Age < 3 ~ "PNC0",
        Age >= 3 & Age < 7 ~ "PNC1",
        Age >= 7 & Age < 28 ~ "PNC4",
        Age >= 28 & Age < 42 ~ "PNC6",
        Age >= 42 & Age < 182 ~ "PNC26",
        Age >= 182 & Age <= 454 ~ "PNC52",
        Age >= 455 ~ "No",
        TRUE ~ "Remove_All"))
  
  # Filtering censored_inf based on cens_time
  cens_00_ <- censored_inf %>% 
    filter(cens_time %in% c("Remove_All", "PNC0")) %>% 
    select(MOMID, PREGID, INFANTID)
  
  cens_01_ <- censored_inf %>% 
    filter(cens_time %in% c("Remove_All", "PNC0", "PNC1")) %>% 
    select(MOMID, PREGID, INFANTID)
  
  # Adjusted filter for cens_04_ and other variables to match the correct `cens_time` values
  cens_04_ <- censored_inf %>%
    filter(cens_time %in% c("Remove_All", "PNC0", "PNC1", "PNC4")) %>% 
    select(MOMID, PREGID, INFANTID)
  
  cens_06_ <- censored_inf %>%
    filter(cens_time %in% c("Remove_All", "PNC0", "PNC1", "PNC4", "PNC6")) %>%
    select(MOMID, PREGID, INFANTID)
  
  cens_26_ <- censored_inf %>%
    filter(cens_time %in% c("Remove_All", "PNC0", "PNC1", "PNC4", "PNC6", "PNC26")) %>%
    select(MOMID, PREGID, INFANTID)
  
  cens_52_ <- censored_inf %>%
    filter(cens_time %in% c("Remove_All", "PNC0", "PNC1", "PNC4", "PNC6", "PNC26", "PNC52")) %>%
    select(MOMID, PREGID, INFANTID)
}

if (exists("mnh04")==TRUE) {
  
  mnh04$ANC_OBSSTDAT <- ymd(parse_date_time(mnh04$ANC_OBSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  mnh04$FETAL_LOSS_DSSTDAT <- ymd(parse_date_time(mnh04$FETAL_LOSS_DSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
  
  mnh04$ANC_OBSSTDAT <- ifelse(mnh04$ANC_OBSSTDAT == as.Date("2007-07-07") | mnh04$ANC_OBSSTDAT == as.Date("1907-07-07"), NA, mnh04$ANC_OBSSTDAT)
  mnh04$FETAL_LOSS_DSSTDAT <- ifelse(mnh04$FETAL_LOSS_DSSTDAT == as.Date("2007-07-07") | mnh04$FETAL_LOSS_DSSTDAT == as.Date("1907-07-07"), NA, mnh04$FETAL_LOSS_DSSTDAT)
  
  
  miscarriage <- mnh04 %>%
    filter(FETAL_LOSS_DSDECOD %in% c(1, 2, 3)) %>% 
    mutate(Fetal_Loss = ifelse(!is.na(FETAL_LOSS_DSSTDAT), FETAL_LOSS_DSSTDAT, ANC_OBSSTDAT)) %>%  
    select(MOMID, PREGID, Fetal_Loss, FETAL_LOSS_DSSTDAT,ANC_OBSSTDAT,FETAL_LOSS_DSDECOD )
  
  
  miscarriage$Fetal_Loss <- as.Date(miscarriage$Fetal_Loss, origin = "1970-01-01")
  miscarriage$ANC_OBSSTDAT <- as.Date(miscarriage$ANC_OBSSTDAT, origin = "1970-01-01")
  miscarriage$FETAL_LOSS_DSSTDAT <- as.Date(miscarriage$FETAL_LOSS_DSSTDAT, origin = "1970-01-01")
  
  
  miscarriage <- miscarriage %>%
    left_join(EDD, by = c("MOMID", "PREGID")) %>% 
    mutate(GA_Visit = GA_DAYS + as.numeric (difftime (Fetal_Loss, VisitDate, unit = "days")),  
           Miss_Time = case_when(
             GA_Visit <= 181 ~ "Yes_20",
             GA_Visit >= 182 & GA_Visit <= 216 ~ "Yes_28",
             GA_Visit >= 217 & GA_Visit <= 237 ~ "Yes_32", 
             GA_Visit >= 238 & GA_Visit <= 272 ~ "Yes_36",
             GA_Visit > 272 ~ "No",
             is.na (FETAL_LOSS_DSSTDAT) ~ "Missing",
             TRUE ~ "Remove_All"))
  
  miscarriage_miss <- miscarriage %>% filter (Miss_Time == "Missing") %>% mutate (GW_Comment = "MomID Missing Fetal Loss Date")
  
  miss_20_ <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20")) %>% select(MOMID, PREGID)
  miss_28_ <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20", "Yes_28")) %>% select(MOMID, PREGID)
  miss_32_ <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20", "Yes_28", "Yes_32")) %>% select(MOMID, PREGID)
  miss_36_  <- miscarriage %>% filter(miscarriage$Miss_Time %in% c("Remove_All","Yes_20","Yes_28", "Yes_32",  "Yes_36")) %>% select(MOMID, PREGID)
  
}


#Create a ReMAPP expected list for each site

ReMapp_df <- Enrolled %>%
  mutate( 
    ReMAPPDate = case_when(
      site == "Ghana" ~ as.Date("2022-12-28"),
      site == "Kenya" ~ as.Date("2023-04-03"),
      site == "Zambia" ~ as.Date("2022-12-15"),
      site == "Pakistan" ~ as.Date("2022-09-22"),
      site %in% c("India-CMC", "India_CMC") ~ as.Date("2023-06-20"),
      site %in% c("India-SAS", "India_SAS") ~ as.Date("2023-08-15"),
      TRUE ~ NA_Date_
    ),
    
    ReMAPPEnd = case_when(
      site == "Pakistan" ~ as.Date("2024-04-05"),
      site == "Ghana" ~ as.Date("2024-10-29"),
      TRUE ~ as.Date(UploadDate) # Ensure UploadDate is a Date
    ),
    ReMAPP = ifelse(US_OHOSTDAT >= ReMAPPDate & US_OHOSTDAT < ReMAPPEnd, 1, 0)
  ) %>% select (-GA_TODAY)

# Create vector of PREGID values where ReMAPP == 1
ReMAPPEnrolled <- ReMapp_df %>%
  filter(ReMAPP == 1) 

if (exists("mnh25")) {

  mnh25$OBSSTDAT  <- ymd(parse_date_time(mnh25$OBSSTDAT , c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y")))
  
  mnh25_df <- mnh25 %>% 
   left_join(ReMAPPEnrolled, by = c("MOMID", "PREGID")) %>% 
    left_join(mnh09_sub %>% select(MOMID, PREGID, MAT_DELIVERY_DATE), by = c("MOMID", "PREGID")) %>% 
    left_join(miscarriage %>% select(MOMID, PREGID, FETAL_LOSS_DSDECOD, FETAL_LOSS_DSSTDAT), by = c("MOMID", "PREGID")) %>% 
    mutate(
      GA_TODAY = round(GA_DAYS + as.numeric(difftime(OBSSTDAT, US_OHOSTDAT, units = "days"))),
      
      AGE_TODAY = case_when(
        !is.na(MAT_DELIVERY_DATE) & OBSSTDAT >= MAT_DELIVERY_DATE ~ 
          round(as.numeric(difftime(OBSSTDAT, MAT_DELIVERY_DATE, units = "days"))),
        
        !is.na(FETAL_LOSS_DSSTDAT) & OBSSTDAT >= FETAL_LOSS_DSSTDAT ~ 
          round(as.numeric(difftime(OBSSTDAT, FETAL_LOSS_DSSTDAT, units = "days"))),
        TRUE ~ NA ),
      DELIVERED = ifelse((!is.na(MAT_DELIVERY_DATE) & OBSSTDAT >= MAT_DELIVERY_DATE) |
                        ( !is.na(FETAL_LOSS_DSSTDAT) & OBSSTDAT >= FETAL_LOSS_DSSTDAT), 1, 0)) %>%  
  mutate(
    EST_TYPE_VISIT = case_when(
      TYPE_VISIT == 10 | (TYPE_VISIT == 14 & AGE_TODAY >= 42 & AGE_TODAY <= 104 & DELIVERED == 1) ~ 10, 
      TYPE_VISIT %in% c(1,2) | (TYPE_VISIT == 13 & GA_TODAY <= 181 & DELIVERED == 0) ~ 2,
      TYPE_VISIT %in% c(4,5) | (TYPE_VISIT == 13 & GA_TODAY >= 217 & GA_TODAY <= 310 & DELIVERED == 0) ~ 4, 
      TRUE ~ 55)) %>% 
  filter(TYPE_VISIT %in% c(2,4,10,13,14)) %>%  # Ensure proper filtering
  select( MOMID, PREGID, OBSSTDAT, TYPE_VISIT, EST_TYPE_VISIT, MAT_VITAL_MNH25,
          MAT_VISIT_MNH25, MAT_VISIT_OTHR_MNH25, US_OHOSTDAT, EDD, GA_DAYS,
          ESTIMATED_EDD_SCDAT, MAT_DELIVERY_DATE, FETAL_LOSS_DSSTDAT, DELIVERED,
          FETAL_LOSS_DSDECOD, GA_TODAY, AGE_TODAY)
  
}

if (exists("mnh26")) {
  
  mnh26$FTGE_OBSTDAT  <- ymd(parse_date_time(mnh26$FTGE_OBSTDAT , c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y")))
  
  mnh26_df <- mnh26 %>% 
    left_join(ReMAPPEnrolled, by = c("MOMID", "PREGID")) %>% 
    left_join(mnh09_sub %>% select(MOMID, PREGID, MAT_DELIVERY_DATE), by = c("MOMID", "PREGID")) %>% 
    left_join(miscarriage %>% select(MOMID, PREGID, FETAL_LOSS_DSDECOD, FETAL_LOSS_DSSTDAT), by = c("MOMID", "PREGID")) %>% 
    mutate(
      GA_TODAY = round(GA_DAYS + as.numeric(difftime(FTGE_OBSTDAT, US_OHOSTDAT, units = "days"))),
      
      AGE_TODAY = case_when(
        !is.na(MAT_DELIVERY_DATE) & FTGE_OBSTDAT >= MAT_DELIVERY_DATE ~ 
          round(as.numeric(difftime(FTGE_OBSTDAT, MAT_DELIVERY_DATE, units = "days"))),
        
        !is.na(FETAL_LOSS_DSSTDAT) & FTGE_OBSTDAT >= FETAL_LOSS_DSSTDAT ~ 
          round(as.numeric(difftime(FTGE_OBSTDAT, FETAL_LOSS_DSSTDAT, units = "days"))),
        TRUE ~ NA ),
      DELIVERED = ifelse((!is.na(MAT_DELIVERY_DATE) & FTGE_OBSTDAT >= MAT_DELIVERY_DATE) |
                           ( !is.na(FETAL_LOSS_DSSTDAT) & FTGE_OBSTDAT >= FETAL_LOSS_DSSTDAT), 1, 0)) %>%  
    mutate(
      EST_TYPE_VISIT = case_when(
        TYPE_VISIT == 10 | (TYPE_VISIT == 14 & AGE_TODAY >= 42 & AGE_TODAY <= 104 & DELIVERED == 1) ~ 10, 
        TYPE_VISIT %in% c(1,2) | (TYPE_VISIT == 13 & GA_TODAY <= 181 & DELIVERED == 0) ~ 2,
        TYPE_VISIT %in% c(4,5) | (TYPE_VISIT == 13 & GA_TODAY >= 217 & GA_TODAY <= 310 & DELIVERED == 0) ~ 4, 
        TRUE ~ 55)) %>% 
    filter(TYPE_VISIT %in% c(2,4,10,13,14)) %>%  # Ensure proper filtering
    select( MOMID, PREGID, FTGE_OBSTDAT, TYPE_VISIT, EST_TYPE_VISIT, MAT_VITAL_MNH26,
            MAT_VISIT_MNH26, MAT_VISIT_OTHR_MNH26, US_OHOSTDAT, EDD, GA_DAYS,
            ESTIMATED_EDD_SCDAT, MAT_DELIVERY_DATE, FETAL_LOSS_DSSTDAT, DELIVERED,
            FETAL_LOSS_DSDECOD, GA_TODAY, AGE_TODAY)
  
}


# Create data frames for different due times
due20_df <- edd_date %>%
  select (-c("DUE28", "DUE32", "DUE36")) %>%
  filter (DUE20 == TRUE)  %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(cens_20_, by = c("MOMID", "PREGID")) %>%
  anti_join(premies_20_, by = c("MOMID", "PREGID")) %>%
  anti_join(miss_20_, by = c("MOMID", "PREGID"))

due28_df <- edd_date %>%
  select (-c("DUE20", "DUE32", "DUE36")) %>%
  filter (DUE28 == TRUE) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(cens_28_, by = c("MOMID", "PREGID")) %>%
  anti_join(premies_28_, by = c("MOMID", "PREGID"))  %>%
  anti_join(miss_28_, by = c("MOMID", "PREGID"))

due32_df <- edd_date %>%
  select (-c("DUE20", "DUE28", "DUE36")) %>%
  filter (DUE32 == TRUE) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(cens_32_, by = c("MOMID", "PREGID")) %>%
  anti_join(premies_32_, by = c("MOMID", "PREGID"))  %>%
  anti_join(miss_32_, by = c("MOMID", "PREGID")) 

due36_df <- edd_date %>%
  select (-c("DUE20", "DUE28", "DUE32")) %>%
  filter (DUE36 == TRUE) %>%
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  anti_join(premies_36_, by = c("MOMID", "PREGID")) %>%
  anti_join(cens_36_, by = c("MOMID", "PREGID"))  %>% 
  anti_join(miss_36_d, by = c("MOMID", "PREGID")) %>%
  anti_join(miss_36_, by = c("MOMID", "PREGID"))   

due00_df <- due_inf %>%
  filter (DUE00 == TRUE) %>%
  distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE) %>%
  anti_join(cens_00_, by = c("PREGID", "INFANTID")) %>% 
  select (MOMID, PREGID, INFANTID, UPLOADDT, DOB, DUE00, DUE00_BILI)

due01_df <- due_inf %>%
  filter (DUE01 == TRUE) %>%
  distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE) %>%
  anti_join(cens_01_, by = c("PREGID", "INFANTID")) %>% 
  select (MOMID, PREGID, INFANTID, UPLOADDT, DOB, DUE01, DUE01_BILI)

due04_df <- due_inf %>%
  filter (DUE04 == TRUE) %>%
  distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE) %>%
  anti_join(cens_04_, by = c("PREGID", "INFANTID")) %>% 
  select (MOMID, PREGID, INFANTID, UPLOADDT, DOB, DUE04, DUE04_BILI)

due06_df <- due_inf %>%
  filter (DUE06 == TRUE) %>%
  distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE) %>%
  anti_join(cens_06_, by = c("PREGID", "INFANTID")) %>% 
  select (MOMID, PREGID, INFANTID, UPLOADDT, DOB, DUE06)

due26_df <- due_inf %>%
  filter (DUE26 == TRUE) %>%
  distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE) %>%
  anti_join(cens_26_, by = c("PREGID", "INFANTID")) %>% 
  select (MOMID, PREGID, INFANTID, UPLOADDT, DOB, DUE26)

due52_df <- due_inf %>%
  filter (DUE52 == TRUE) %>%
  distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE) %>%
  anti_join(cens_52_, by = c("PREGID", "INFANTID")) %>% 
  select (MOMID, PREGID, INFANTID, UPLOADDT, DOB, DUE52)

#Begin query for MNH01 to find missing
## Because there are some enrolled individuals that are missing mnh01 forms

Enrol_01 <- mnh01  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH01) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_01 <- Enrolled %>% left_join (Enrol_01, by = c("MOMID", "PREGID")) %>% 
  mutate(
    Query = ifelse(MAT_VISIT_MNH01 %in% c(1, 2, 3, 4, 5, 6, 7, 8), FALSE,
                   ifelse(!(MAT_VISIT_MNH01 %in% c(1:8))  & !is.na(MAT_VISIT_MNH01), "Invalid Data Entry for Enrolment Visit")),  
    Variable_Value = MAT_VISIT_MNH01,
    Form = "MNH01", 
    Varname = "MAT_VISIT_MNH01", 
    VisitDate = US_OHOSTDAT,
    VisitType = "1", 
    UPLOADDT = UploadDate) %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH01 - ANC 32
ANC32_01 <- mnh01  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH01, US_OHOSTDAT ) 

ANC36_visit <- mnh01 %>% filter (TYPE_VISIT == 5 & MAT_VISIT_MNH01 == 1) 
ANC36_ids <- ANC36_visit$PREGID 

Missing_01_32 <- due32_df %>% left_join (ANC32_01, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH01 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH01) & DUE32 == TRUE, "Missing ANC32 MNH01 Form",
                                 ifelse(!(MAT_VISIT_MNH01 %in% c(1:8, 88)) & (DUE32 == TRUE)  & !is.na (MAT_VISIT_MNH01), "Invalid Data Entry for ANC32 MNH01 Form",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH01,
    Form = "MNH01",
    VisitDate = US_OHOSTDAT,
    VisitType = "4", 
    UPLOADDT = UploadDate,
    Varname = "MAT_VISIT_MNH01") %>%
  filter(!(Query %in% c(FALSE, "Not Due", "Not Applicable"))) %>%
  filter(!(PREGID %in% ANC36_ids)) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH03 - ENROLMENT
#There should only be one form per participant, hence the distinct MOMID code 
#We want to identify forms that are not present, or have a default value in the MAT_Visit Variable
Missing_03 <- Enrolled %>% 
  left_join(mnh03,  by = c("MOMID", "PREGID"))  %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  mutate(Query = ifelse(MAT_VISIT_MNH03 %in% c(1, 2, 3, 4, 5, 6, 7, 8), FALSE,
                        ifelse(!(MAT_VISIT_MNH03 %in% c (1:8)) & !is.na(MAT_VISIT_MNH03), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH03 Enrolment Form")),
         Form = "MNH03",
         Varname = "MAT_VISIT_MNH03",
         VisitDate = SD_OBSSTDAT,
         VisitType = "1", 
         UPLOADDT = UploadDate,
         Variable_Value = MAT_VISIT_MNH03
  ) %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query) %>% mutate_all(as.character)

#MNH04 
#ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36
#MNH04 - ENROLMENT
Enrol_04 <- mnh04  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH04, ANC_OBSSTDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_04 <- Enrolled %>% left_join (Enrol_04, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH04 %in% c(1, 2, 3, 4, 5, 6, 7, 8)), FALSE,
                        ifelse(!(MAT_VISIT_MNH04 %in% c(1:8)) & !is.na (MAT_VISIT_MNH04), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH04 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH04,
         Form = "MNH04",
         VisitDate = ANC_OBSSTDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH04") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 20 (For those who need to have ANC20 done)
ANC20_04 <- mnh04  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH04,ANC_OBSSTDAT ) 
Missing_04_20 <- due20_df %>% left_join (ANC20_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",   
                          ifelse(is.na(MAT_VISIT_MNH04) & DUE20 == TRUE, "Missing ANC20 MNH04 Form",        
                                 ifelse(!(MAT_VISIT_MNH04 %in% c(1:8, 88)) & (DUE20 == TRUE) & !is.na (MAT_VISIT_MNH04), "Invalid Data Entry for MNH04 ANC20",
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH04 - ANC 28
ANC28_04 <- mnh04  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH04, ANC_OBSSTDAT) 
Missing_04_28 <- due28_df %>% left_join (ANC28_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH04) & DUE28 == TRUE, "Missing ANC28 MNH04 Form", 
                                 ifelse(!(MAT_VISIT_MNH04 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH04) & (DUE28 == TRUE), "Invalid Data Entry for MNH04 ANC28",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH04 - ANC 32
ANC32_04 <- mnh04  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH04, ANC_OBSSTDAT ) 
Missing_04_32 <- due32_df %>% left_join (ANC32_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH04) & DUE32 == TRUE, "Missing ANC32 MNH04 Form", 
                                 ifelse(!(MAT_VISIT_MNH04 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH04) & (DUE32 == TRUE), "Invalid Data Entry for ANC32",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH04 - ANC 36
ANC36_04 <- mnh04  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH04,ANC_OBSSTDAT ) 
Missing_04_36 <- due36_df %>% left_join (ANC36_04, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH04 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH04) & DUE36 == TRUE, "Missing ANC36 MNH04 Form", 
                                 ifelse(!(MAT_VISIT_MNH04 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH04) & (DUE36 == TRUE), "Invalid Data Entry for MNH04 ANC36",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH04,
    Form = "MNH04",
    VisitDate = ANC_OBSSTDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH04") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#MNH05 - ENROLMENT
Enrol_05 <- mnh05  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH05, ANT_PEDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_05 <- Enrolled %>% left_join (Enrol_05, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH05 %in% c(1, 2, 3, 4, 5, 6, 7, 8)), FALSE,
                        ifelse(!(MAT_VISIT_MNH05 %in% c(1:8)) & !is.na(MAT_VISIT_MNH05), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH05 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH05,
         Form = "MNH05",
         VisitDate = ANT_PEDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH05") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 20 (For those who need to have ANC20 done)
ANC20_05 <- mnh05  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_20 <- due20_df %>% left_join (ANC20_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH05) & DUE20 == TRUE, "Missing ANC20 MNH05 Form",        
                                 ifelse(!(MAT_VISIT_MNH05 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH05) & (DUE20 == TRUE), "Invalid Data Entry for MNH05 ANC20",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 28
ANC28_05 <- mnh05  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_28 <- due28_df %>% left_join (ANC28_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH05) & DUE28 == TRUE, "Missing ANC28 MNH05 Form", 
                                 ifelse(!(MAT_VISIT_MNH05 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH05) & (DUE28 == TRUE), "Invalid Data Entry for MNH05 ANC28",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH05 - ANC 32
ANC32_05 <- mnh05  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_32 <- due32_df %>% left_join (ANC32_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH05) & DUE32 == TRUE, "Missing ANC32 MNH05 Form", 
                                 ifelse(!(MAT_VISIT_MNH05 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH05) & (DUE32 == TRUE), "Invalid Data Entry for MNH05 ANC32",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH05 - ANC 36
ANC36_05 <- mnh05  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH05,ANT_PEDAT ) 
Missing_05_36 <- due36_df %>% left_join (ANC36_05, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH05) & DUE36 == TRUE, "Missing ANC36 MNH05 Form",       
                                 ifelse(!(MAT_VISIT_MNH05 %in% c(1:8,88)) & !is.na(MAT_VISIT_MNH05) & (DUE36 == TRUE), "Invalid Data Entry for MNH05 ANC36",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH05,
    Form = "MNH05",
    VisitDate = ANT_PEDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH05") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#MNH06 - ENROLMENT
Enrol_06 <- mnh06  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH06, DIAG_VSDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_06 <- Enrolled %>% left_join (Enrol_06, by = c("MOMID", "PREGID")) %>% 
  mutate(Query = ifelse((MAT_VISIT_MNH06 %in% c(1, 2, 3, 4, 5, 6, 7, 8)), FALSE,
                        ifelse(!(MAT_VISIT_MNH06 %in% c(1:8)) & !is.na(MAT_VISIT_MNH06), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH06 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH06,
         Form = "MNH06",
         VisitDate = DIAG_VSDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH06") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 20 (For those who need to have ANC20 done)
ANC20_06 <- mnh06  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_20 <- due20_df %>% left_join (ANC20_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH06) & DUE20 == TRUE, "Missing ANC20 MNH06 Form", 
                                 ifelse(!(MAT_VISIT_MNH06 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH06) & (DUE20 == TRUE), "Invalid Data Entry for MNH06 ANC20",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 28
ANC28_06 <- mnh06  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_28 <- due28_df %>% left_join (ANC28_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH06) & DUE28 == TRUE, "Missing ANC28 MNH06 Form", 
                                 ifelse(!(MAT_VISIT_MNH06 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH06) & (DUE28 == TRUE), "Invalid Data Entry for MNH06 ANC28 ",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH06 - ANC 32
ANC32_06 <- mnh06  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_32 <- due32_df %>% left_join (ANC32_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH06) & DUE32 == TRUE, "Missing ANC32 MNH06 Form", 
                                 ifelse(!(MAT_VISIT_MNH06 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH06) & (DUE32 == TRUE), "Invalid Data Entry for MNH06 ANC32",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH06 - ANC 36
ANC36_06 <- mnh06  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH06,DIAG_VSDAT ) 
Missing_06_36 <- due36_df %>% left_join (ANC36_06, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH06) & DUE36 == TRUE, "Missing ANC36 MNH06 Form",
                                 ifelse(!(MAT_VISIT_MNH06 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH06) & (DUE36 == TRUE), "Invalid Data Entry for MNH06 ANC36",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH06,
    Form = "MNH06",
    VisitDate = DIAG_VSDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH06") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36

#MNH07 ENROLMENT
Enrol_07 <- mnh07  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH07, MAT_SPEC_COLLECT_DAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_07 <- Enrolled %>% left_join (Enrol_07, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8)), FALSE,
                        ifelse(!(MAT_VISIT_MNH07 %in% c(1:8)) & !is.na(MAT_VISIT_MNH07), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH07 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH07,
         Form = "MNH07",
         VisitDate = MAT_SPEC_COLLECT_DAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH07") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 20 (For those who need to have ANC20 done)
ANC20_07 <- mnh07  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_20 <- due20_df %>% left_join (ANC20_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH07) & DUE20 == TRUE, "Missing ANC20 MNH07 Form", 
                                 ifelse(!(MAT_VISIT_MNH07 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH07) & (DUE20 == TRUE), "Invalid Data Entry for MNH07 ANC20",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 28
ANC28_07 <- mnh07  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_28 <- due28_df %>% left_join (ANC28_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE28 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH07) & DUE28 == TRUE, "Missing ANC28 MNH07 Form",        
                                 ifelse(!(MAT_VISIT_MNH07 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH07) & (DUE28 == TRUE), "Invalid Data Entry for ANC28  MNH07 Form",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "3", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH07 - ANC 32
ANC32_07 <- mnh07  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_32 <- due32_df %>% left_join (ANC32_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH07) & DUE32 == TRUE, "Missing ANC32 MNH07 Form", 
                                 ifelse(!(MAT_VISIT_MNH07 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH07) & (DUE32 == TRUE), "Invalid Data Entry for ANC32  MNH07 Form",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH07 - ANC 36
ANC36_07 <- mnh07  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH07,MAT_SPEC_COLLECT_DAT ) 
Missing_07_36 <- due36_df %>% left_join (ANC36_07, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH07) & DUE36 == TRUE, "Missing ANC36 MNH07 Form",
                                 ifelse(!(MAT_VISIT_MNH07 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH07) & (DUE36 == TRUE), "Invalid Data Entry for ANC36  MNH07 Form",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH07,
    Form = "MNH07",
    VisitDate = MAT_SPEC_COLLECT_DAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH07") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


#MNH08 - ENROLMENT, ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC28, ANC32, ANC36
#MNH08 - ENROLMENT
Enrol_08 <- mnh08  %>% filter(TYPE_VISIT == 1) %>% select(MOMID, PREGID, MAT_VISIT_MNH08, LBSTDAT) %>% distinct(MOMID, PREGID, .keep_all = TRUE)

Miss_enrol_08 <- Enrolled %>% left_join (Enrol_08, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse((MAT_VISIT_MNH08 %in% c(1, 2, 3, 4, 5, 6, 7, 8)), FALSE,
                        ifelse(!(MAT_VISIT_MNH08 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH08), "Invalid Data Entry for Enrolment Visit", 
                               "Missing MNH08 Enrolment Form")),
         Variable_Value = MAT_VISIT_MNH08,
         Form = "MNH08",
         VisitDate = LBSTDAT,
         UPLOADDT = UploadDate,
         VisitType = "1", 
         Varname = "MAT_VISIT_MNH08") %>%
  filter(Query != FALSE) %>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 20 (For those who need to have ANC20 done)
ANC20_08 <- mnh08  %>% filter(TYPE_VISIT == 2) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_20 <- due20_df %>% left_join (ANC20_08, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH08) & DUE20 == TRUE, "Missing ANC20 MNH08 Form", 
                                 ifelse(!(MAT_VISIT_MNH08 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH08) & (DUE20 == TRUE), "Invalid Data Entry for ANC20",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 28
ANC28_08 <- mnh08  %>% filter(TYPE_VISIT == 3) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_28 <- due28_df %>% left_join (ANC28_08, by = c("MOMID", "PREGID")) %>%
  mutate(Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                        ifelse(DUE28 == FALSE, "Not Due",
                               ifelse(is.na(MAT_VISIT_MNH08) & DUE28 == TRUE, "Missing ANC28 MNH08 Form",
                                      ifelse(!(MAT_VISIT_MNH08 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH08) & (DUE28 == TRUE), "Invalid Data Entry for ANC28",   
                                             "Not Applicable")))),
         Variable_Value = MAT_VISIT_MNH08,
         Form = "MNH08",
         VisitDate = LBSTDAT,
         VisitType = "3", 
         Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 32
ANC32_08 <- mnh08  %>% filter(TYPE_VISIT == 4) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_32 <- due32_df %>% left_join (ANC32_08, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3,4,5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH08) & DUE32 == TRUE, "Missing ANC32 MNH08 Form", 
                                 ifelse(!(MAT_VISIT_MNH08 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH08) & (DUE32 == TRUE), "Invalid Data Entry for ANC32",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH08 - ANC 36
ANC36_08 <- mnh08  %>% filter(TYPE_VISIT == 5) %>% select(MOMID, PREGID, MAT_VISIT_MNH08,LBSTDAT ) 
Missing_08_36 <- due36_df %>% left_join (ANC36_08, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE36 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH08) & DUE36 == TRUE, "Missing ANC36 MNH08 Form", 
                                 ifelse(!(MAT_VISIT_MNH08 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH08) & (DUE36 == TRUE), "Invalid Data Entry for ANC36",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH08,
    Form = "MNH08",
    VisitDate = LBSTDAT,
    VisitType = "5", 
    Varname = "MAT_VISIT_MNH08") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


##MNH25 - ANC20, ANC32
#MNH25 - ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC32

#MNH25 - ANC 20 (For those who need to have ANC20 done)
ANC20_25 <- mnh25_df  %>% filter(EST_TYPE_VISIT == 2) %>% 
  select(MOMID, PREGID, MAT_VISIT_MNH25, OBSSTDAT) 

Missing_25_20 <- due20_df %>% filter (PREGID %in% ReMAPPEnrolled$PREGID) %>% 
  left_join (ANC20_25, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH25 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH25) & DUE20 == TRUE, "Missing ANC20 MNH25 Form", 
                                 ifelse(!(MAT_VISIT_MNH25 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH25) & (DUE20 == TRUE), "Invalid Data Entry",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH25,
    Form = "MNH25",
    VisitDate = OBSSTDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH25") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH25 - ANC 32
ANC32_25 <- mnh25_df  %>% filter(EST_TYPE_VISIT == 4) %>% 
  select(MOMID, PREGID, MAT_VISIT_MNH25, OBSSTDAT) 

Missing_25_32 <- due32_df %>% filter (PREGID %in% ReMAPPEnrolled$PREGID) %>% 
  left_join (ANC32_25, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH25 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH25) & DUE32 == TRUE, "Missing ANC32 MNH25 Form", 
                                 ifelse(!(MAT_VISIT_MNH25 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH25) & (DUE32 == TRUE), "Invalid Data Entry",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH25,
    Form = "MNH25",
    VisitDate = OBSSTDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH25") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)



#MNH26 - ANC20 (for selected persons i.e. if they were less than 18wks at enrolment), ANC32

#MNH26 - ANC 20 (For those who need to have ANC20 done)
ANC20_26 <- mnh26_df  %>% filter(EST_TYPE_VISIT == 2) %>% 
                          select(MOMID, PREGID, MAT_VISIT_MNH26, FTGE_OBSTDAT) 

Missing_26_20 <- due20_df %>% filter (PREGID %in% ReMAPPEnrolled$PREGID) %>% 
                              left_join (ANC20_26, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH26 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE20 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH26) & DUE20 == TRUE, "Missing ANC20 MNH26 Form", 
                                 ifelse(!(MAT_VISIT_MNH26 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH26) & (DUE20 == TRUE), "Invalid Data Entry",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH26,
    Form = "MNH26",
    VisitDate = FTGE_OBSTDAT,
    VisitType = "2", 
    Varname = "MAT_VISIT_MNH26") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)

#MNH26 - ANC 32
ANC32_26 <- mnh26_df  %>% filter(EST_TYPE_VISIT == 4) %>% 
  select(MOMID, PREGID, MAT_VISIT_MNH26, FTGE_OBSTDAT) 

Missing_26_32 <- due32_df %>% filter (PREGID %in% ReMAPPEnrolled$PREGID) %>% 
  left_join (ANC32_26, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH26 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE32 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH26) & DUE32 == TRUE, "Missing ANC32 MNH26 Form", 
                                 ifelse(!(MAT_VISIT_MNH26 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH26) & (DUE32 == TRUE), "Invalid Data Entry",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH26,
    Form = "MNH26",
    VisitDate = FTGE_OBSTDAT,
    VisitType = "4", 
    Varname = "MAT_VISIT_MNH26") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, EDD, Varname, Variable_Value, Form, Query)


# Combine all anc missing forms data
# List of dataframes to check
df_list_anc <- c("Miss_enrol_01", "Missing_01_32", "Missing_03", "Miss_enrol_04", "Missing_04_20",
                 "Missing_04_28", "Missing_04_32", "Missing_04_36", "Miss_enrol_05", 
                 "Missing_05_20", "Missing_05_28", "Missing_05_32", "Missing_05_36",
                 "Miss_enrol_06", "Missing_06_20", "Missing_06_28", "Missing_06_32", "Missing_06_36",
                 "Miss_enrol_07", "Missing_07_20", "Missing_07_28", "Missing_07_32", "Missing_07_36",
                 "Miss_enrol_08", "Missing_08_20", "Missing_08_28", "Missing_08_32", "Missing_08_36",
                 "Missing_25_20", "Missing_25_32","Missing_26_20", "Missing_26_32")

# Initialize an empty list to hold the dataframes that exist
anc_existing_dfs <- lapply(df_list_anc, function(df) {
  if (exists(df)) {
    get(df) %>% mutate(across(everything(), as.character))  # Mutate to character if exists
  } else {
    NULL  # Return NULL if the dataframe doesn't exist
  }
})

# Remove NULL entries and combine the remaining dataframes
all_anc_missing <- bind_rows(anc_existing_dfs[!sapply(anc_existing_dfs, is.null)])

# Check if the combined dataframe has any rows
if (nrow(all_anc_missing) > 0) {
  
  #Truncating and Reshaping ANC Missingness
  # Separate missing and invalid queries
  missing_anc_query <- all_anc_missing %>% filter(is.na(Variable_Value))
  invalid_anc_entry <- all_anc_missing %>% filter(!is.na(Variable_Value)) %>% 
    select (MOMID, PREGID, VisitType, Form, Varname, Variable_Value, EditType = Query)
  
  missing_forms_category <- missing_anc_query %>%
    group_by(MOMID, PREGID, VisitType) %>%
    summarise(Missing_Forms = paste(Form, collapse = ", "))
  
  missing_forms_count <- missing_anc_query %>%
    group_by(MOMID, PREGID, VisitType) %>%
    count(name = "Frequency")
  
  missing_forms_merged <- merge(missing_forms_count, missing_forms_category, by = c("VisitType", "MOMID", "PREGID"))
  
  missing_forms_merged$VisitTypeLabel <- case_when(
    missing_forms_merged$VisitType == 2 ~ "ANC20",
    missing_forms_merged$VisitType == 1 ~ "Enrolment",
    missing_forms_merged$VisitType == 3 ~ "ANC28",
    missing_forms_merged$VisitType == 4 ~ "ANC32",
    missing_forms_merged$VisitType == 5 ~ "ANC36"
  )
  missing_forms_merged <- missing_forms_merged %>%
    mutate(EditType = ifelse(Frequency >= 5, paste0("Missing All ", VisitTypeLabel, " Forms"),
                             ifelse(Frequency < 5 & Frequency > 1, paste0("Missing Multiple", " ", VisitTypeLabel, " Forms"), 
                                    paste0("Missing ", VisitTypeLabel, " ",  Missing_Forms, " Form"))),
           Form = Missing_Forms,
           Varname = NA,
           Variable_Value = NA)  
  
  all_missing_label <- missing_forms_merged %>% 
    select (MOMID, PREGID, VisitType, Form, Varname, Variable_Value, EditType)
  
  if (exists("invalid_data_entry")) {
    # Combine all types of missing entries
    combined_query <- rbind(all_missing_label, invalid_data_entry)
    
  } else {
    combined_query <- all_missing_label
  }
  
  #Bind all Missing Forms
  MatMissingForms_query <- combined_query  %>% 
    mutate(UploadDate = UploadDate, 
           ScrnID = "NA",
           `Variable Name` = Varname,
           `Variable Value` = Variable_Value,
           InfantID = "NA",
           FieldType = "NA",  
           MomID = MOMID,
           PregID = PREGID,
           VisitDate = "NA",
           DateEditReported = format(Sys.time(), "%Y-%m-%d"),
           Form_Edit_Type = paste(Form,"_", EditType),
           QueryID = paste0(MomID, "_", UploadDate, "_", EditType, "_", "05" ) 
    )  %>%
    select ( QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, VisitType, VisitDate, Form, 
             'Variable Name', 'Variable Value', FieldType, EditType, DateEditReported, Form_Edit_Type)
  
  save(MatMissingForms_query, file = "MatMissingForms_query.rda") 
  
  
  #create comments to specify missing forms
  missingEDD <- missing_forms_merged %>% left_join(EDD, by = c ("MOMID","PREGID" )) %>% 
    select (MOMID, PREGID, GA_DAYS, VisitDate, VisitType, Missing_Forms, EditType )
  
  
  MatMissingFormsQuery_comments <- missingEDD %>% 
    mutate( `GA at Upload Date` = round(GA_DAYS + as.numeric(difftime(UploadDate, VisitDate, units = "days"))),
            QueryID = paste0(MOMID, "_", UploadDate, "_", EditType, "_", "05" ))  %>% 
    rename(`GA at Enrolment` = GA_DAYS, `Enrolment Date` = VisitDate, `Missing Forms` = Missing_Forms)
  
    closeout_miss <- censored_moms %>% 
    filter (Cen_Time == "Missing") 
  
  if (nrow(closeout_miss) > 0) {   
    closeout_miss <- closeout_miss %>% 
      mutate(GW_Comment = "MomID Missing MNH23 Closeout Date") %>% 
      select(MOMID, PREGID, GW_Comment)
    
    MatMissingFormsQuery_comments <- MatMissingFormsQuery_comments %>%
     left_join(closeout_miss, by = c("MOMID", "PREGID"))
    
    if (nrow (miscarriage_miss) > 0) {
      MatMissingFormsQuery_comments <- MatMissingFormsQuery_comments %>%
        left_join(miscarriage_miss, by = c("MOMID", "PREGID","GW_Comment"))
    }
    
  }  else {MatMissingFormsQuery_comments <- MatMissingFormsQuery_comments %>% mutate (GW_Comment = "")}

  
  save(MatMissingFormsQuery_comments, file = "MatMissingFormsQuery_comments.rda") 
  
  print("All ANC Forms Combined")
  
  
} else {
  
  print("No missing ANC Forms exist to combine.")
  
}


##MISSING PNC FORMS 

#Create Loops for every visit for each form

if (exists("mnh05")) {
  
  #Loop MNH05 Missing Forms (TYPE-VISIT %in% c(10,11,12))
  generate_mnh05_report <- function(visit_type, due_df, suffix, due_var) {
    # Select relevant columns from MNH05 based on the visit type
    PNC_05 <- mnh05 %>% filter(TYPE_VISIT == visit_type) %>% select(MOMID, PREGID, MAT_VISIT_MNH05, ANT_PEDAT)
    
    # Generate missing report by joining with the due dataframe
    result <- due_df %>%
      distinct(MOMID, PREGID, .keep_all = TRUE) %>%
      left_join(PNC_05, by = c("MOMID", "PREGID")) %>%
      mutate(
        Query = ifelse(MAT_VISIT_MNH05 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                       ifelse(!!sym(due_var) == FALSE, "Not Due", 
                              ifelse(is.na(MAT_VISIT_MNH05) & !!sym(due_var) == TRUE, 
                                     paste0("Missing PNC", suffix, " MNH05 Form"),
                                     ifelse(!(MAT_VISIT_MNH05 %in% c(1:8, 88)) & !is.na (MAT_VISIT_MNH05) & !!sym(due_var) == TRUE, 
                                            paste0("Invalid Data Entry for MNH05 PNC", suffix),   
                                            "Not Applicable")))),
        Variable_Value = MAT_VISIT_MNH05,
        Form = "MNH05",
        VisitDate = ANT_PEDAT,
        VisitType = as.character(visit_type), 
        Varname = "MAT_VISIT_MNH05"
      ) %>%
      filter(!(Query %in% c(FALSE, "Not Due"))) %>%
      select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
    
    return(result)
  }
  
  # Define the visit types and corresponding due dataframes and due variables
  visit_types <- list(
    list("visit_type" = 10, "suffix" = 6, "due_df" = due06_df, "due_var" = "DUE06"),
    list("visit_type" = 11, "suffix" = 26, "due_df" = due26_df, "due_var" = "DUE26"),
    list("visit_type" = 12, "suffix" = 52, "due_df" = due52_df, "due_var" = "DUE52")
  )
  
  # Generate the missing reports using lapply
  missing_reports <- lapply(visit_types, function(v) {
    generate_mnh05_report(v$visit_type, v$due_df, v$suffix, v$due_var)
  })
  
  
  # Combine all reports into a single dataframe
  Missing_05_PNC <- do.call(rbind, missing_reports)
  
} else {print ("MNH05 dataset not uploaded")}

if (exists("mnh06")) {
  
  #Loop MNH06 Missing Forms (TYPE-VISIT %in% c(7,8,9,10,11,12))
  generate_missing_report <- function(visit_type, due_df, suffix, PNC_df, due_var) {
    PNC_06 <- mnh06 %>% filter(TYPE_VISIT == visit_type) %>% select(MOMID, PREGID, MAT_VISIT_MNH06, DIAG_VSDAT)
    
    result <- due_df %>%
      distinct(MOMID, PREGID, .keep_all = TRUE) %>%
      left_join(PNC_06, by = c("MOMID", "PREGID")) %>%
      mutate(
        Query = ifelse(MAT_VISIT_MNH06 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                       ifelse(!!sym(due_var) == FALSE, "Not Due",
                              ifelse(is.na(MAT_VISIT_MNH06) & !!sym(due_var) == TRUE, 
                                     paste0("Missing PNC", suffix, " MNH06 Form"),
                                     ifelse(!(MAT_VISIT_MNH06 %in% c(1:8, 88))  & !is.na (MAT_VISIT_MNH06) &  !!sym(due_var) == TRUE, 
                                            paste0("Invalid Data Entry for MNH06 PNC", suffix), NA)))),
        Variable_Value = MAT_VISIT_MNH06,
        Form = "MNH06",
        VisitDate = DIAG_VSDAT,
        VisitType = as.character(visit_type),
        Varname = "MAT_VISIT_MNH06"
      ) %>%
      filter(!(Query %in% c(FALSE, "Not Due", NA))) %>%
      select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
    
    return(result)
  }
  
  visit_types <- list(
    list("visit_type" = 7, "suffix" = 0, "due_df" = due00_df, "due_var" = "DUE00"),
    list("visit_type" = 8, "suffix" = 1, "due_df" = due01_df, "due_var" = "DUE01"),
    list("visit_type" = 9, "suffix" = 4, "due_df" = due04_df, "due_var" = "DUE04"),
    list("visit_type" = 10,"suffix" = 6,"due_df" = due06_df, "due_var" = "DUE06"),
    list("visit_type" = 11, "suffix" = 26, "due_df" = due26_df, "due_var" = "DUE26"),
    list("visit_type" = 12, "suffix" = 52, "due_df" = due52_df, "due_var" = "DUE52")
  )
  
  missing_reports <- lapply(visit_types, function(v) {
    generate_missing_report(v$visit_type, v$due_df, v$suffix, PNC_df, v$due_var)
  })
  
  Missing_06_PNC <- do.call(rbind, missing_reports)
  
} else {print ("MNH06 dataset not uploaded")}

if (exists("mnh07")) {
  
  # Loop MNH07 Missing Forms (TYPE-VISIT %in% c(10,11))
  generate_mnh07_report <- function(visit_type, due_df, suffix, due_var) {
    # Select relevant columns from MNH07 based on the visit type
    PNC_07 <- mnh07 %>% filter(TYPE_VISIT == visit_type) %>% 
      select(MOMID, PREGID, MAT_VISIT_MNH07, MAT_SPEC_COLLECT_DAT)
    
    # Generate missing report by joining with the due dataframe
    result <- due_df %>%
      distinct(MOMID, PREGID, .keep_all = TRUE) %>%
      left_join(PNC_07, by = c("MOMID", "PREGID")) %>%
      mutate(
        Query = ifelse(is.na(MAT_VISIT_MNH07) & !!sym(due_var) == TRUE, 
                       paste0("Missing PNC", suffix, " MNH07 Form"),
                       ifelse(MAT_VISIT_MNH07 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                              ifelse(!!sym(due_var) == FALSE, "Not Due",
                                     ifelse(!(MAT_VISIT_MNH07 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH07) & !!sym(due_var) == TRUE, 
                                            paste0("Invalid Data Entry for MNH07 PNC", suffix),   
                                            "Not Applicable")))),
        Variable_Value = MAT_VISIT_MNH07,
        Form = "MNH07",
        VisitDate = MAT_SPEC_COLLECT_DAT,
        VisitType = as.character(visit_type), 
        Varname = "MAT_VISIT_MNH07"
      ) %>%
      filter(!(Query %in% c(FALSE, "Not Due"))) %>%
      select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
    
    return(result)
  }
  
  # Define the visit types and corresponding due dataframes and due variables
  visit_types <- list(
    list("visit_type" = 10, "suffix" = 6, "due_df" = due06_df, "due_var" = "DUE06")
    # list("visit_type" = 11, "suffix" = 26, "due_df" = due26_df, "due_var" = "DUE26")
  )
  
  # Generate the missing reports using lapply
  missing_reports <- lapply(visit_types, function(v) {
    generate_mnh07_report(v$visit_type, v$due_df, v$suffix, v$due_var)
  })
  
  # Combine all reports into a single dataframe
  Missing_07_PNC <- do.call(rbind, missing_reports)
  
} else {
  print("MNH07 dataset not uploaded")
}

if (exists("mnh08")) {
  
  #Loop MNH08 Missing Forms (TYPE-VISIT %in% c(10,11))
  generate_mnh08_report <- function(visit_type, due_df, suffix, due_var) {
    # Select relevant columns from MNH08 based on the visit type
    PNC_08 <- mnh08 %>% filter(TYPE_VISIT == visit_type) %>% select(MOMID, PREGID, MAT_VISIT_MNH08, LBSTDAT)
    
    # Generate missing report by joining with the due dataframe
    result <- due_df %>%
      distinct(MOMID, PREGID, .keep_all = TRUE) %>%
      left_join(PNC_08, by = c("MOMID", "PREGID")) %>%
      mutate(
        Query = ifelse(MAT_VISIT_MNH08 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                       ifelse(!!sym(due_var) == FALSE, "Not Due",
                              ifelse(is.na(MAT_VISIT_MNH08) & !!sym(due_var) == TRUE, 
                                     paste0("Missing PNC", suffix, " MNH08 Form"),
                                     ifelse(!(MAT_VISIT_MNH08 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH08) & !!sym(due_var) == TRUE, 
                                            paste0("Invalid Data Entry for MNH08 PNC", suffix), "Not Applicable")))),
        Variable_Value = MAT_VISIT_MNH08,
        Form = "MNH08",
        VisitDate = LBSTDAT,
        VisitType = as.character(visit_type), 
        Varname = "MAT_VISIT_MNH08"
      ) %>%
      filter(!(Query %in% c(FALSE, "Not Due"))) %>%
      select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
    
    return(result)
  }
  
  # Define the visit types and corresponding due dataframes and due variables
  visit_types <- list(
    list("visit_type" = 10, "suffix" = 6, "due_df" = due06_df, "due_var" = "DUE06")
    # list("visit_type" = 11, "suffix" = 26, "due_df" = due26_df, "due_var" = "DUE26")
  )
  
  # Generate the missing reports using lapply
  missing_reports <- lapply(visit_types, function(v) {
    generate_mnh08_report(v$visit_type, v$due_df, v$suffix, v$due_var)
  })
  
  
  # Combine all reports into a single dataframe
  Missing_08_PNC <- do.call(rbind, missing_reports)
  
} else {print ("MNH08 dataset not uploaded")}

if (exists("mnh012")) {
  
  #Loop MNH12 Missing Forms (TYPE-VISIT %in% c(7,8,9,10,11,12))
  generate_missing_report <- function(visit_type, due_df, suffix, PNC_df, due_var) {
    PNC_12 <- mnh12 %>% filter(TYPE_VISIT == visit_type) %>% select(MOMID, PREGID, MAT_VISIT_MNH12, VISIT_OBSSTDAT)
    
    result <- due_df %>%
      distinct(MOMID, PREGID, .keep_all = TRUE) %>%
      left_join(PNC_12, by = c("MOMID", "PREGID")) %>%
      mutate(
        Query = ifelse(MAT_VISIT_MNH12 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                       ifelse(!!sym(due_var) == FALSE, "Not Due",
                              ifelse(is.na(MAT_VISIT_MNH12) & !!sym(due_var) == TRUE, 
                                     paste0("Missing PNC", suffix, " MNH12 Form"),
                                     ifelse(!(MAT_VISIT_MNH12 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH12) & !!sym(due_var) == TRUE, 
                                            paste0("Invalid Data Entry for MNH12 PNC", suffix), NA)))),
        Variable_Value = MAT_VISIT_MNH12,
        Form = "MNH12",
        VisitDate = VISIT_OBSSTDAT,
        VisitType = as.character(visit_type),
        Varname = "MAT_VISIT_MNH12"
      ) %>%
      filter(!(Query %in% c(FALSE, "Not Due", NA))) %>%
      select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
    
    return(result)
  }
  
  visit_types <- list(
    list("visit_type" = 7, "suffix" = 0, "due_df" = due00_df, "due_var" = "DUE00"),
    list("visit_type" = 8, "suffix" = 1, "due_df" = due01_df, "due_var" = "DUE01"),
    list("visit_type" = 9, "suffix" = 4, "due_df" = due04_df, "due_var" = "DUE04"),
    list("visit_type" = 10,"suffix" = 6,"due_df" = due06_df, "due_var" = "DUE06"),
    list("visit_type" = 11, "suffix" = 26, "due_df" = due26_df, "due_var" = "DUE26"),
    list("visit_type" = 12, "suffix" = 52, "due_df" = due52_df, "due_var" = "DUE52")
  )
  
  missing_reports <- lapply(visit_types, function(v) {
    generate_missing_report(v$visit_type, v$due_df, v$suffix, PNC_df, v$due_var)
  })
  
  Missing_12_PNC <- do.call(rbind, missing_reports)
  
} else {print ("MNH12 dataset not uploaded")}


#Loop MNH13 to 15 Missing Forms (TYPE-VISIT %in% c(7,8,9,10,11,12))
generate_missing_report <- function(visit_type, due_df, suffix, form_df, due_var, form_suffix, visit_date_var) {
  
  form_var <- paste0("INF_VISIT_MNH", form_suffix)
  
  clean_df <- form_df %>% 
    filter(TYPE_VISIT == visit_type) %>% 
    select(MOMID, PREGID, INFANTID, !!sym(form_var), !!sym(visit_date_var))
  
  result <- due_df %>%
    distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE) %>%
    left_join(clean_df, by = c("MOMID", "PREGID", "INFANTID")) %>%
    mutate(
      Query = case_when(
        !!sym(form_var) %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 88) ~ "No Issue",  # Convert FALSE to a string
        !!sym(due_var) == FALSE ~ "Not Due",  # Leave this as a string
        is.na(!!sym(form_var)) & !!sym(due_var) == TRUE ~ 
          paste0("Missing PNC", suffix, " MNH", form_suffix, " Form"),
        !(!!sym(form_var) %in% c(1:8, 88)) & !is.na (!!sym(form_var)) & !!sym(due_var) == TRUE ~ 
          paste0("Invalid Data Entry for MNH", form_suffix, " PNC", suffix),
        TRUE ~ NA_character_  # Return NA as a character
      ),
      Variable_Value = !!sym(form_var),
      Form = paste0("MNH", form_suffix),
      VisitDate = !!sym(visit_date_var),
      VisitType = as.character(visit_type),
      Varname = form_var
    ) %>%
    filter(!(Query %in% c("No Issue", "Not Due", NA))) %>%  # Filter strings instead of logical values
    select(MOMID, PREGID, INFANTID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
  
  return(result)
}

# Function to generate missing reports across MNH forms, with different visit date vars
generate_missing_reports_for_MNH <- function(form_df, due_dfs, form_suffix, visit_date_var) {
  visit_types <- list(
    list("visit_type" = 7, "suffix" = 0, "due_var" = "DUE00"),
    list("visit_type" = 8, "suffix" = 1, "due_var" = "DUE01"),
    list("visit_type" = 9, "suffix" = 4, "due_var" = "DUE04"),
    list("visit_type" = 10, "suffix" = 6, "due_var" = "DUE06"),
    list("visit_type" = 11, "suffix" = 26, "due_var" = "DUE26"),
    list("visit_type" = 12, "suffix" = 52, "due_var" = "DUE52")
  )
  
  reports <- lapply(visit_types, function(v) {
    generate_missing_report(v$visit_type, due_dfs[[v$due_var]], v$suffix, form_df, v$due_var, form_suffix, visit_date_var)
  })
  
  return(do.call(rbind, reports))
}


if (exists("mnh13")) {
  
  # Generate reports for MNH13 and MNH14 using VISIT_OBSSTDAT
  Missing_13_PNC <- generate_missing_reports_for_MNH(mnh13, list(DUE00 = due00_df, DUE01 = due01_df, DUE04 = due04_df, DUE06 = due06_df, DUE26 = due26_df, DUE52 = due52_df), 13, "VISIT_OBSSTDAT")
  
} else {print ("MNH13 dataset not uploaded")}

if (exists("mnh14")) {
  
  Missing_14_PNC <- generate_missing_reports_for_MNH(mnh14, list(DUE00 = due00_df, DUE01 = due01_df, DUE04 = due04_df, DUE06 = due06_df, DUE26 = due26_df, DUE52 = due52_df), 14, "VISIT_OBSSTDAT")
  
} else {print ("MNH14 dataset not uploaded")}

if (exists("mnh15")) {
  
  mnh15$MOMID = as.character(mnh15$MOMID)
  mnh15$PREGID = as.character(mnh15$PREGID)
  mnh15$INFANTID = as.character(mnh15$INFANTID)
  
  # Generate reports for MNH15 using VOBSSTDAT
  
  Missing_15_PNC <- generate_missing_reports_for_MNH(mnh15, list(DUE00 = due00_df, DUE01 = due01_df, DUE04 = due04_df, DUE06 = due06_df, DUE26 = due26_df, DUE52 = due52_df), 15, "OBSSTDAT")
  
} else {print ("MNH15 dataset not uploaded")}

if (exists("mnh25")) {
  
  #MNH25 - ANC 32
  PNC06_25 <- mnh25_df  %>% filter(EST_TYPE_VISIT == 10) %>% 
    select(MOMID, PREGID, MAT_VISIT_MNH25, OBSSTDAT, MAT_DELIVERY_DATE) 
  
  Missing_25_PNC <- due06_df %>% 
    distinct(MOMID, PREGID, .keep_all = TRUE) %>%
    filter (PREGID %in% ReMAPPEnrolled$PREGID) %>% 
    left_join (PNC06_25, by = c("MOMID", "PREGID")) %>%
    mutate(
      Query = ifelse(MAT_VISIT_MNH25 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                     ifelse(DUE06 == FALSE, "Not Due",
                            ifelse(is.na(MAT_VISIT_MNH25) & DUE06 == TRUE, "Missing PNC06 MNH25 Form", 
                                   ifelse(!(MAT_VISIT_MNH25 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH25) & (DUE06 == TRUE), "Invalid Data Entry",   
                                          "Not Applicable")))),
      Variable_Value = MAT_VISIT_MNH25,
      Form = "MNH25",
      VisitDate = OBSSTDAT,
      VisitType = "10", 
      Varname = "MAT_VISIT_MNH25") %>%
    filter(!(Query %in% c(FALSE, "Not Due")))%>%
    select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
  
} else {print ("MNH25 dataset not uploaded")}


if (exists("mnh26")) {
  
#MNH26 - ANC 32
PNC06_26 <- mnh26_df  %>% filter(EST_TYPE_VISIT == 10) %>% 
  select(MOMID, PREGID, MAT_VISIT_MNH26, FTGE_OBSTDAT, MAT_DELIVERY_DATE) 

Missing_26_PNC <- due06_df %>% 
  distinct(MOMID, PREGID, .keep_all = TRUE) %>%
  filter (PREGID %in% ReMAPPEnrolled$PREGID) %>% 
  left_join (PNC06_26, by = c("MOMID", "PREGID")) %>%
  mutate(
    Query = ifelse(MAT_VISIT_MNH26 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                   ifelse(DUE06 == FALSE, "Not Due",
                          ifelse(is.na(MAT_VISIT_MNH26) & DUE06 == TRUE, "Missing PNC06 MNH26 Form", 
                                 ifelse(!(MAT_VISIT_MNH26 %in% c(1:8, 88)) & !is.na(MAT_VISIT_MNH26) & (DUE06 == TRUE), "Invalid Data Entry",   
                                        "Not Applicable")))),
    Variable_Value = MAT_VISIT_MNH26,
    Form = "MNH26",
    VisitDate = FTGE_OBSTDAT,
    VisitType = "10", 
    Varname = "MAT_VISIT_MNH26") %>%
  filter(!(Query %in% c(FALSE, "Not Due")))%>%
  select(MOMID, PREGID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)

} else {print ("MNH26 dataset not uploaded")}

### For MNH36 Missing Forms
if (exists("mnh36")) {
  
  # Loop MNH36 Missing Forms (TYPE-VISIT %in% c(7,8,9))
  generate_mnh36_report <- function(visit_type, due_df, suffix, due_var, due_bili) {
    # Select relevant columns from MNH36 based on the visit type
    PNC_36 <- mnh36 %>% filter(TYPE_VISIT == visit_type) %>% 
      mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
      select(MOMID, PREGID, INFANTID, INF_VISIT_MNH36, VISIT_OBSSTDAT)
    
    # Generate missing report by joining with the due dataframe
    result <- due_df %>%
      distinct(INFANTID, MOMID, PREGID, .keep_all = TRUE) %>%
      left_join(PNC_36, by = c("MOMID", "PREGID", "INFANTID")) %>%
      mutate(
        Query = ifelse(is.na(INF_VISIT_MNH36) & !!sym(due_var) == TRUE & !!sym(due_bili) == TRUE, 
                       paste0("Missing PNC", suffix, " MNH36 Form"),
                       
                       ifelse(INF_VISIT_MNH36 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 88), FALSE,
                              
                              ifelse(!!sym(due_var) == FALSE | !!sym(due_bili) == FALSE, "Not Due",
                                     
                                     ifelse(!(INF_VISIT_MNH36 %in% c(1:8, 88)) & !is.na(INF_VISIT_MNH36) & !!sym(due_var) == TRUE & !!sym(due_bili) == TRUE, 
                                            paste0("Invalid Data Entry for MNH36 PNC", suffix),   
                                            
                                            "Not Applicable")))),
        
        Variable_Value = INF_VISIT_MNH36,
        Form = "MNH36",
        VisitDate = VISIT_OBSSTDAT,
        VisitType = as.character(visit_type), 
        Varname = "INF_VISIT_MNH36"
      ) %>%
      filter(!(Query %in% c(FALSE, "Not Due"))) %>%
      select(MOMID, PREGID, INFANTID, VisitDate, VisitType, UPLOADDT, DOB, Varname, Variable_Value, Form, Query)
    
    return(result)
  }
  
  # Define the visit types and corresponding due dataframes and due variables
  visit_types <- list(
    list("visit_type" = 7, "suffix" = 0, "due_df" = due00_df, "due_var" = "DUE00", "due_bili" = "DUE00_BILI"),
    list("visit_type" = 8, "suffix" = 1, "due_df" = due01_df, "due_var" = "DUE01", "due_bili" = "DUE01_BILI"),
    list("visit_type" = 9, "suffix" = 4, "due_df" = due04_df, "due_var" = "DUE04", "due_bili" = "DUE04_BILI")
  )
  
  # Generate the missing reports using lapply
  missing_reports <- lapply(visit_types, function(v) {
    generate_mnh36_report(v$visit_type, v$due_df, v$suffix, v$due_var, v$due_bili)
  })
  
  # Combine all reports into a single dataframe
  Missing_36_PNC_raw <- do.call(rbind, missing_reports)
  
  # Filter out specific INFANTIDs for VisitType == 9
  PNC06_BILI_IDs <- mnh36 %>% filter(INF_VISIT_MNH36 %in% c(1, 2) & TYPE_VISIT == 10) %>% pull(INFANTID)
  
  Missing_36_PNC <- Missing_36_PNC_raw %>% filter(!(INFANTID %in% PNC06_BILI_IDs & VisitType == 9))
  
} else {
  print("MNH36 dataset not uploaded")
}

#Bind All PNC Forms
# List of dataframes to check
df_list_pnc <- c("Missing_05_PNC", "Missing_06_PNC", "Missing_07_PNC", "Missing_08_PNC",
                 "Missing_12_PNC", "Missing_13_PNC", "Missing_14_PNC", "Missing_15_PNC", 
                 "Missing_25_PNC","Missing_26_PNC","Missing_36_PNC" )

# Initialize an empty list to hold the dataframes that exist
existing_dfs <- lapply(df_list_pnc, function(df) {
  if (exists(df)) {
    get(df) %>% mutate(across(everything(), as.character))  # Mutate to character if exists
  } else {
    NULL  # Return NULL if the dataframe doesn't exist
  }
})

# Remove NULL entries and combine the remaining dataframes
all_pnc_missing <- bind_rows(existing_dfs[!sapply(existing_dfs, is.null)])

# Check if the combined dataframe has any rows
if (nrow(all_pnc_missing) > 0) {
  
  #Truncating and Reshaping PNC Missingness
  # Separate missing and invalid queries
  missing_pnc_query <- all_pnc_missing %>% filter(is.na(Variable_Value))
  
  invalid_pnc_entry <- all_pnc_missing %>% filter(!is.na(Variable_Value)) %>% 
    select (MOMID, PREGID, INFANTID, VisitType, Form, Varname, Variable_Value, EditType = Query)
  
  missing_pnc_category <- missing_pnc_query %>%
    group_by(MOMID, PREGID, INFANTID, VisitType) %>%
    summarise(Missing_Forms = paste(Form, collapse = ", "))
  
  missing_pnc_count <- missing_pnc_query %>%
    group_by(MOMID, PREGID, INFANTID, VisitType) %>%
    count(name = "Frequency")
  
  missing_pnc_merged <- merge(missing_pnc_count, missing_pnc_category, 
                              by = c("VisitType", "MOMID", "PREGID", "INFANTID"))
  
  missing_pnc_merged$VisitTypeLabel <- case_when(
    missing_pnc_merged$VisitType == 7 ~ "PNC0",
    missing_pnc_merged$VisitType == 8 ~ "PNC1",
    missing_pnc_merged$VisitType == 9 ~ "PNC4",
    missing_pnc_merged$VisitType == 10 ~ "PNC6",
    missing_pnc_merged$VisitType == 11 ~ "PNC26",
    missing_pnc_merged$VisitType == 12 ~ "PNC52",
  )
  
  missing_pnc_merged <- missing_pnc_merged %>%
    mutate(EditType = 
             #Merging Maternal PNC Forms
             ifelse(Frequency >= 5 & is.na (INFANTID), paste0("Missing All Mat ", VisitTypeLabel, " Forms"),
                    ifelse(Frequency < 5 & Frequency > 1 & is.na (INFANTID), paste0("Missing Multiple Mat", " ", VisitTypeLabel, " Forms"), 
                           #Merging Maternal PNC Forms          
                           ifelse(Frequency >= 3 & !is.na (INFANTID), paste0("Missing All Inf ", VisitTypeLabel, " Forms"),
                                  ifelse(Frequency < 3 & Frequency > 1 & is.na (INFANTID), paste0("Missing Multiple Inf", " ", VisitTypeLabel, " Forms"), 
                                         paste0("Missing ", VisitTypeLabel, " ",  Missing_Forms, " Form"))))),
           Form = Missing_Forms,
           Varname = NA,
           Variable_Value = NA)  
  
  all_pnc_label <- missing_pnc_merged %>% 
    select (MOMID, PREGID, INFANTID, VisitType, Form, Varname, Variable_Value, EditType)
  
  if (exists("invalid_data_entry") && !is.null(invalid_data_entry)) {
    # Combine all types of missing entries
    combinedPNC_query <- rbind(all_pnc_label, invalid_data_entry)
    
  } else {
    combinedPNC_query <- all_pnc_label
  }
  
  #Bind all Missing Forms
  InfMissingForms_query <- combinedPNC_query  %>% 
    mutate(UploadDate = UploadDate, 
           ScrnID = "NA",
           `Variable Name` = Varname,
           `Variable Value` = Variable_Value,
           InfantID = INFANTID,
           FieldType = "NA",  
           MomID = MOMID,
           PregID = PREGID,
           VisitDate = "NA",
           DateEditReported = format(Sys.time(), "%Y-%m-%d"),
           Form_Edit_Type = paste(Form,"_", EditType),
           QueryID = paste0(MomID, "_", UploadDate, "_", EditType, "_", "05" ) 
    )  %>%
    select ( QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, VisitType, VisitDate, Form, 
             'Variable Name', 'Variable Value', FieldType, EditType, DateEditReported, Form_Edit_Type)
  
  save(InfMissingForms_query, file = "InfMissingForms_query.rda") 
  
  
  #create comments to specify missing forms
  
  InfMissingFormsQuery_comments <- missing_pnc_merged %>%
    select(MOMID, PREGID, INFANTID, VISITTYPE = VisitType, `PNC VISIT` = VisitTypeLabel,
           `MISSING FORMS` = Missing_Forms, EDITTYPE = EditType) %>%
    
    # First join by MOMID, PREGID, and INFANTID
    left_join(infant_dob, 
              by = c("MOMID", "PREGID", "INFANTID")) %>%
    
    # Second join: Select the earliest DOB for each MOMID and PREGID, retaining INFANTID and DOB
    left_join(infant_dob %>%
                group_by(MOMID, PREGID) %>%
                slice_min(order_by = DOB, with_ties = FALSE) %>%
                select(MOMID, PREGID, INFANTID, DOB) %>%
                ungroup(),
              by = c("MOMID", "PREGID")) %>%
    # Use coalesce to combine INFANTID.x and INFANTID.y, preferring INFANTID.x
    mutate(INFANTID = coalesce(INFANTID.x, INFANTID.y),
           DOB = coalesce(DOB.x, DOB.y)) %>%
    # Calculate age based on difference between UploadDate and DOB
    mutate(UploadDate = as.Date(UploadDate), # Convert UploadDate to Date format if necessary
           DOB = as.Date(DOB),               # Convert DOB to Date format
           `INFANT AGE TODAY` = as.numeric(difftime(UploadDate, DOB, units = "days"))) %>%
    arrange(VISITTYPE)  %>%
    mutate (QueryID = paste0(MOMID, "_", UploadDate, "_", EDITTYPE, "_", "05" )) %>%
    # Convert all columns to character
    mutate(across(everything(), as.character)) %>%
    # Select the relevant columns
    select(QueryID, MOMID, PREGID, INFANTID, DOB, `INFANT AGE TODAY`, VISITTYPE,
           `MISSING FORMS`, EDITTYPE )
  
  closeout_miss_inf <- censored_inf %>% 
    filter (cens_time == "Missing") 
  
  if (nrow(closeout_miss_inf) > 0) {   
    closeout_miss_inf <- closeout_miss_inf %>% 
      mutate(GW_Comment = "InfantID Missing MNH23 Closeout Date") %>% 
      select(INFANTID, MOMID, PREGID, GW_Comment)
    
    InfMissingFormsQuery_comments <- InfMissingFormsQuery_comments %>%
      left_join(closeout_miss_inf, by = c("MOMID", "PREGID", "INFANTID"))
    
  } else {InfMissingFormsQuery_comments <- InfMissingFormsQuery_comments %>% mutate (GW_Comment = "")}
  
  
  save(InfMissingFormsQuery_comments, file = "InfMissingFormsQuery_comments.rda") 
  
  print("PNC Missing Forms Query ran successfully")
  
} else {
  
  print("No Missing PNC Forms")
  
}
