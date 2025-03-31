#*****************************************************************************
#*QUERY #4 -- VISIT TYPE QUERIES
#* Written by: Stacie Loisate 
#* Last updated: 26 February 2024

#*Input: Wide data
#*Function: check for visit types that do not match based on estimated gestational age at visit 
#*Output: 
  # .rda file with all mismatched visit types to include in query report tab 
  # .rda file with all mismatched visit types + additional information to include as an extra tab in query report
  # .rda file with all instances of visit types occurring on the same day
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 

#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for
#* 3. Set your main directory 

#* Once the previous lines of code are updated, you can start to run the script 
# clear environment 
# rm(list = ls())

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)
library(openxlsx)

# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: Set "site" variable to the site you are running the query for 
site = "Kenya"

# UPDATE EACH RUN: load in the WIDE data we generated from 00_DataImport code
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_wide.Rdata", sep = "")) 

# UPDATE EACH RUN: load in the LONG data we generated from 00_DataImport code 
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_long.Rdata", sep = "")) 

## UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"


mnh02_ids <- mnh02 %>% select(SCRNID, MOMID, PREGID)

if (site == "Zambia"){
  # Zambia is reporting some values as "." - this will break the flow. the following code will replace with "-7"
  mnh01_newids <- mnh01 %>% select(-MOMID, -PREGID) %>% ## remove momid and pregid vars in mnh01 - will use ids from mnh02
    left_join(mnh02_ids, by = c("SCRNID")) %>% 
    mutate_at(vars(starts_with("US_GA_")), ~ as.numeric(ifelse(. == ".", -7, as.character(.)))) 
  
} else {
  mnh01_newids <- mnh01 %>% select(-MOMID, -PREGID) %>% ## remove momid and pregid vars in mnh01 - will use ids from mnh02
    left_join(mnh02_ids, by = c("SCRNID"))
  
}

#*****************************************************************************
#*Export key variables from each dataframe: MOMID, PREGID, VISIT DATE 
#* for MNH01 and 09: export GA and delivery information 
#* Generate boe estimate
#*****************************************************************************

# India-CMC is already reporting acog (CAL_GA_WKS_AGE_FTS1-4 & CAL_GA_DAYS_AGE_FTS1-4) - use these estimates as the BOE for CMC site
if (site=="India-CMC" | site=="India-SAS"){
  
  mnh01_raw <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT, contains("CAL_GA_WKS_AGE_"), contains("CAL_GA_DAYS_AGE_"))
  
  ## only extract key variables from each of the forms that check for visit type 
  ## for MNH01, need to extract the maximum GA age (currently using US -- will eventually update to ACOG)
  mnh01_sub <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT, contains("CAL_GA_WKS_AGE_"), contains("CAL_GA_DAYS_AGE_")) %>% 
    ## extract the maximum gestational age for each woman 
    mutate(GA_US_DAYS_FTS1 =  ifelse(CAL_GA_WKS_AGE_FTS1!= -7 & CAL_GA_DAYS_AGE_FTS1 != -7,  (CAL_GA_WKS_AGE_FTS1 * 7 + CAL_GA_DAYS_AGE_FTS1), NA), 
           GA_US_DAYS_FTS2 =  ifelse(CAL_GA_WKS_AGE_FTS2!= -7 & CAL_GA_DAYS_AGE_FTS2 != -7,  (CAL_GA_WKS_AGE_FTS2 * 7 + CAL_GA_DAYS_AGE_FTS2), NA),
           GA_US_DAYS_FTS3 =  ifelse(CAL_GA_WKS_AGE_FTS3!= -7 & CAL_GA_DAYS_AGE_FTS3 != -7,  (CAL_GA_WKS_AGE_FTS3 * 7 + CAL_GA_DAYS_AGE_FTS3), NA),
           GA_US_DAYS_FTS4 =  ifelse(CAL_GA_WKS_AGE_FTS4!= -7 & CAL_GA_DAYS_AGE_FTS4 != -7,  (CAL_GA_WKS_AGE_FTS4 * 7 + CAL_GA_DAYS_AGE_FTS4), NA)) %>% 
    mutate(GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
    ## convert to weeks 
    mutate(GA_US_WKS = GA_US_DAYS %/% 7) %>% 
    mutate(GA_US_WKS = as.numeric(paste0(GA_US_WKS, ".", (GA_US_WKS-(GA_US_WKS*7))))) %>% 
    mutate(US_OHOSTDAT = ymd(parse_date_time(US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
    ## remove any unscheduled visit types
    filter(TYPE_VISIT != 13) %>% 
    # "zero out" GA and obtain the estimated "date of conception" 
    mutate(EST_CONCEP_DATE = US_OHOSTDAT - GA_US_DAYS) %>% 
    select(MOMID, PREGID, TYPE_VISIT, US_OHOSTDAT,GA_US_DAYS, GA_US_WKS, EST_CONCEP_DATE)
  
} else {
  
  mnh01_raw <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT,
                                       US_GA_WKS_AGE_FTS1, US_GA_DAYS_AGE_FTS1, US_GA_WKS_AGE_FTS2, US_GA_DAYS_AGE_FTS2,
                                       US_GA_WKS_AGE_FTS3, US_GA_DAYS_AGE_FTS3, US_GA_WKS_AGE_FTS4, US_GA_DAYS_AGE_FTS4) 
  ## only extract key variables from each of the forms that check for visit type 
  ## for MNH01, need to extract the maximum GA age (currently using US -- will eventually update to ACOG)
  mnh01_sub <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT,
                                       US_GA_WKS_AGE_FTS1, US_GA_DAYS_AGE_FTS1, US_GA_WKS_AGE_FTS2, US_GA_DAYS_AGE_FTS2,
                                       US_GA_WKS_AGE_FTS3, US_GA_DAYS_AGE_FTS3, US_GA_WKS_AGE_FTS4, US_GA_DAYS_AGE_FTS4) %>% 
    ## extract the maximum gestational age for each woman 
    mutate(GA_US_DAYS_FTS1 =  ifelse(US_GA_WKS_AGE_FTS1!= -7 & US_GA_DAYS_AGE_FTS1 != -7,  (US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1), NA), 
           GA_US_DAYS_FTS2 =  ifelse(US_GA_WKS_AGE_FTS2!= -7 & US_GA_DAYS_AGE_FTS2 != -7,  (US_GA_WKS_AGE_FTS2 * 7 + US_GA_DAYS_AGE_FTS2), NA),
           GA_US_DAYS_FTS3 =  ifelse(US_GA_WKS_AGE_FTS3!= -7 & US_GA_DAYS_AGE_FTS3 != -7,  (US_GA_WKS_AGE_FTS3 * 7 + US_GA_DAYS_AGE_FTS3), NA),
           GA_US_DAYS_FTS4 =  ifelse(US_GA_WKS_AGE_FTS4!= -7 & US_GA_DAYS_AGE_FTS4 != -7,  (US_GA_WKS_AGE_FTS4 * 7 + US_GA_DAYS_AGE_FTS4), NA)) %>% 
    mutate(GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
    ## convert to weeks 
    mutate(GA_US_WKS = GA_US_DAYS %/% 7) %>%
    mutate(GA_US_WKS = ifelse(!is.na(GA_US_WKS), paste0(GA_US_WKS, ".", (GA_US_DAYS-(GA_US_WKS*7))), NA),
           GA_US_WKS = as.numeric(GA_US_WKS)) %>% 
    mutate(US_OHOSTDAT = ymd(parse_date_time(US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
    ## remove any unscheduled visit types
    filter(TYPE_VISIT != 13) %>% 
    # "zero out" GA and obtain the estimated "date of conception" 
    mutate(EST_CONCEP_DATE = US_OHOSTDAT - GA_US_DAYS) %>% 
    select(MOMID, PREGID, TYPE_VISIT, US_OHOSTDAT,GA_US_DAYS, GA_US_WKS, EST_CONCEP_DATE)
}

## remove any duplicates -- these will be flagged in the duplicate id code
out_dup_ids <- mnh01_sub[duplicated(mnh01_sub[,1:3]),]
out_dup_ids <- out_dup_ids %>% pull(MOMID)
mnh01_sub <- mnh01_sub %>% filter(!(MOMID %in% out_dup_ids))

## separate into 2 dataframes: one with GA at enrollment visit only -- we will use this to calculate GA at each visit.
#  and one with the other visit types  
mnh01_ga_at_enrl <- mnh01_sub %>% filter(TYPE_VISIT == 1) %>% select(-TYPE_VISIT)
mnh01_sub <- mnh01_sub %>% filter (TYPE_VISIT != 1) %>% rename("VISITDATE" = "US_OHOSTDAT") %>% 
  select(-c(GA_US_DAYS, GA_US_WKS, EST_CONCEP_DATE)) %>% 
  mutate(FORM = "MNH01")

## extract key variables from each of the forms that require visit type (MNH4, 5, 6, 7, 8, 12)
mnh04_sub <- mnh04 %>% select(MOMID, PREGID, TYPE_VISIT, ANC_OBSSTDAT) %>% 
  mutate(ANC_OBSSTDAT = ymd(parse_date_time(ANC_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename("VISITDATE" = "ANC_OBSSTDAT") %>% 
  mutate(FORM = "MNH04")

mnh05_sub <- mnh05 %>% select(MOMID, PREGID, TYPE_VISIT, ANT_PEDAT) %>%
  mutate(ANT_PEDAT = ymd(parse_date_time(ANT_PEDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename("VISITDATE" = "ANT_PEDAT") %>%
  mutate(FORM = "MNH05")

mnh06_sub <- mnh06 %>% select(MOMID, PREGID, TYPE_VISIT, DIAG_VSDAT) %>% 
  mutate(DIAG_VSDAT = ymd(parse_date_time(DIAG_VSDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename("VISITDATE" = "DIAG_VSDAT") %>% 
  mutate(FORM = "MNH06")

mnh07_sub <- mnh07 %>% select(MOMID, PREGID, TYPE_VISIT, MAT_SPEC_COLLECT_DAT) %>%
  mutate(MAT_SPEC_COLLECT_DAT = ymd(parse_date_time(MAT_SPEC_COLLECT_DAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename("VISITDATE" = "MAT_SPEC_COLLECT_DAT") %>% 
  mutate(FORM = "MNH07")

mnh08_sub <- mnh08 %>% select(MOMID, PREGID, TYPE_VISIT, LBSTDAT) %>% 
  mutate(LBSTDAT = ymd(parse_date_time(LBSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename("VISITDATE" = "LBSTDAT") %>% 
  mutate(FORM = "MNH08")

mnh25_sub <- mnh25 %>% select(MOMID, PREGID, TYPE_VISIT, OBSSTDAT) %>% 
  mutate (OBSSTDAT = ymd(parse_date_time(OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename ("VISITDATE" = "OBSSTDAT") %>% 
  mutate (FORM = "MNH25")

mnh26_sub <- mnh26 %>% select(MOMID, PREGID, TYPE_VISIT, FTGE_OBSTDAT) %>% 
  mutate(FTGE_OBSTDAT = ymd(parse_date_time(FTGE_OBSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename("VISITDATE" = "FTGE_OBSTDAT") %>% 
  mutate(FORM = "MNH26")

## For MNH09: 
# we need to extract the minimum delivery datetime in the event of a multiples pregnancy
# if site == Kenya, change default date format to 2007-07-07; all other sites will have 1907-07-07

if (site=="Kenya"){
  mnh09_sub <- mnh09 %>% select(MOMID, PREGID, 
                                DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, 
                                DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,
                                DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, 
                                DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4)  %>% 
    mutate(DELIV_DSSTDAT_INF1 = ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF2 = ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF3 = ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF4 = ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    ) %>% 
    # replace default value date with NA 
    mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1==ymd("2007-07-07"), NA),
           DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2==ymd("2007-07-07"), NA),
           DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3==ymd("2007-07-07"), NA),
           DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4==ymd("2007-07-07"), NA)) %>%
    
    # replace default value time with NA 
    mutate(DELIV_DSSTTIM_INF1 = replace(DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF1=="77:77", NA),  ## should be 77:77, but pak is using 07:07
           DELIV_DSSTTIM_INF2 = replace(DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF2=="77:77", NA),
           DELIV_DSSTTIM_INF3 = replace(DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF3=="77:77", NA),
           DELIV_DSSTTIM_INF4 = replace(DELIV_DSSTTIM_INF4, DELIV_DSSTTIM_INF4=="77:77", NA)) %>%
    # concatenate date and time of birth
    mutate(DELIVERY_DATETIME_INF1 = paste(DELIV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1),
           DELIVERY_DATETIME_INF2 = paste(DELIV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2),
           DELIVERY_DATETIME_INF3 = paste(DELIV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3),
           DELIVERY_DATETIME_INF4 = paste(DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF4)) %>%
    # assign time field type for time of birth
    mutate(DELIVERY_DATETIME_INF1 = as.POSIXct(DELIVERY_DATETIME_INF1, format= "%Y-%m-%d %H:%M"),
           DELIVERY_DATETIME_INF2 = as.POSIXct(DELIVERY_DATETIME_INF2, format= "%Y-%m-%d %H:%M"),
           DELIVERY_DATETIME_INF3 = as.POSIXct(DELIVERY_DATETIME_INF3, format= "%Y-%m-%d %H:%M"),
           DELIVERY_DATETIME_INF4 = as.POSIXct(DELIVERY_DATETIME_INF4, format= "%Y-%m-%d %H:%M")) %>%
    mutate(DOB = 
             pmin(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2,
                  DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
    select(MOMID, PREGID, DOB) %>% 
    mutate(DOB = ymd(DOB))
  
} else {
  mnh09_sub <- mnh09 %>% select(MOMID, PREGID, 
                                DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, 
                                DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,
                                DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, 
                                DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4)  %>% 
    mutate(DELIV_DSSTDAT_INF1 = ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF2 = ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF3 = ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF4 = ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    ) %>% 
    # replace default value date with NA 
    mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1==ymd("1907-07-07"), NA),
           DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2==ymd("1907-07-07"), NA),
           DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3==ymd("1907-07-07"), NA),
           DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4==ymd("1907-07-07"), NA)) %>%
    
    # replace default value time with NA 
    mutate(DELIV_DSSTTIM_INF1 = replace(DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF1=="77:77", NA),  ## should be 77:77, but pak is using 07:07
           DELIV_DSSTTIM_INF2 = replace(DELIV_DSSTTIM_INF2, DELIV_DSSTTIM_INF2=="77:77", NA),
           DELIV_DSSTTIM_INF3 = replace(DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF3=="77:77", NA),
           DELIV_DSSTTIM_INF4 = replace(DELIV_DSSTTIM_INF4, DELIV_DSSTTIM_INF4=="77:77", NA)) %>%
    # concatenate date and time of birth
    mutate(DELIVERY_DATETIME_INF1 = paste(DELIV_DSSTDAT_INF1, DELIV_DSSTTIM_INF1),
           DELIVERY_DATETIME_INF2 = paste(DELIV_DSSTDAT_INF2, DELIV_DSSTTIM_INF2),
           DELIVERY_DATETIME_INF3 = paste(DELIV_DSSTDAT_INF3, DELIV_DSSTTIM_INF3),
           DELIVERY_DATETIME_INF4 = paste(DELIV_DSSTDAT_INF4, DELIV_DSSTTIM_INF4)) %>%
    # assign time field type for time of birth
    mutate(DELIVERY_DATETIME_INF1 = as.POSIXct(DELIVERY_DATETIME_INF1, format= "%Y-%m-%d %H:%M"),
           DELIVERY_DATETIME_INF2 = as.POSIXct(DELIVERY_DATETIME_INF2, format= "%Y-%m-%d %H:%M"),
           DELIVERY_DATETIME_INF3 = as.POSIXct(DELIVERY_DATETIME_INF3, format= "%Y-%m-%d %H:%M"),
           DELIVERY_DATETIME_INF4 = as.POSIXct(DELIVERY_DATETIME_INF4, format= "%Y-%m-%d %H:%M")) %>%
    mutate(DOB = 
             pmin(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2,
                  DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4, na.rm = TRUE)) %>% 
    select(MOMID, PREGID, DOB) %>% 
    mutate(DOB = ymd(DOB))
  
}

mnh12_sub <- mnh12 %>% select(MOMID, PREGID, TYPE_VISIT, VISIT_OBSSTDAT) %>% 
  mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% 
  rename("VISITDATE" = "VISIT_OBSSTDAT") %>% 
  mutate(FORM = "MNH12")

#*****************************************************************************
## Rbind all the data frames together 
#*****************************************************************************
# bind rows of all forms except delivery and enrollment ultrasound
maternal_all <- bind_rows(mnh01_sub, mnh04_sub, mnh05_sub,
                          mnh06_sub,mnh07_sub, mnh08_sub, 
                          mnh25_sub, mnh26_sub) 

## Merge in enrollment ultrasound data 
maternal_all <- left_join(maternal_all, mnh01_ga_at_enrl, by = c("MOMID", "PREGID"))

## Merge in DOB data 
maternal_all <- left_join(maternal_all, mnh09_sub, by = c("MOMID", "PREGID"))

#*****************************************************************************
## ANC visits
#*****************************************************************************
## Data processing
# Extract all ANC visits 
# calculate GA at each vist for ANC visits 
# generate EXPECTED type visits 
anc_visits = c(1,2,3,4,5)
maternal_all_anc = maternal_all %>% filter(TYPE_VISIT %in% anc_visits) %>% 
  mutate(GA_AT_VISIT_DAYS = as.numeric(VISITDATE - EST_CONCEP_DATE, na.rm= TRUE)) %>% 
  ## convert to weeks where 5.6 = 5 weeks, 6 days
  mutate(GA_AT_VISIT_WKS = GA_AT_VISIT_DAYS %/% 7) %>% 
  mutate(GA_AT_VISIT_WKS = ifelse(!is.na(GA_AT_VISIT_WKS), paste0(GA_AT_VISIT_WKS, ".", (GA_AT_VISIT_DAYS-(GA_AT_VISIT_WKS*7))), NA),
         GA_AT_VISIT_WKS = as.numeric(GA_AT_VISIT_WKS)) %>% 
  group_by(MOMID, PREGID) %>% 
  mutate(PERIOD = "ANC") %>% 
  rename("REPORTED_TYPE_VISIT" = "TYPE_VISIT") %>% 
  # extract the previous visit the participant had 
  arrange(-desc(REPORTED_TYPE_VISIT)) %>% 
  mutate(PREV_VISIT_TYPE = dplyr::lag(REPORTED_TYPE_VISIT, n = 1)) %>% 
  # generate binary var if the participant had the previous visit where 1=Yes and 0=No
  mutate(PREV_VISIT_BINARY = ifelse(REPORTED_TYPE_VISIT == 1 & is.na(PREV_VISIT_TYPE), 1, ## no previous visit before enrollment
                                    ifelse(REPORTED_TYPE_VISIT == 3 & PREV_VISIT_TYPE ==1 & GA_US_DAYS > 125 ,1, # if visit type = 3 and you enrolled later (at 18/19 weeks) then your expected previous visit is enrollment
                                           ifelse(REPORTED_TYPE_VISIT == REPORTED_TYPE_VISIT & PREV_VISIT_TYPE==(REPORTED_TYPE_VISIT-1), 1, 0)))) %>% 
  ungroup() %>% 
  # generate variable for expected visit type 
  mutate(EXPECTED_TYPE_VISIT = ifelse(GA_AT_VISIT_DAYS >= 126 & GA_AT_VISIT_DAYS <= 181 & GA_US_DAYS <= 125, 2, ## only participants who are <= 17wks at enrollment will have this visit 
                                      ifelse(GA_AT_VISIT_DAYS <=139, 1, 
                                             ifelse(GA_AT_VISIT_DAYS >= 182 & GA_AT_VISIT_DAYS <= 216, 3, 
                                                    ifelse(GA_AT_VISIT_DAYS >= 217 & GA_AT_VISIT_DAYS <= 237, 4, ## MIGHT BE UPDATING THE 237 NUMBER 
                                                           ## Since all visit types are assigned based on the GA at the time of assessment, we don't need to worry about 4 vs 5. 
                                                           # if ANC32 is missed, it is conducted at ANC36 visit
                                                           ifelse(GA_AT_VISIT_DAYS >= 238 & GA_AT_VISIT_DAYS <= 300, 5, 88)))))) %>% 
  # if you enroll >17wks gestation, do you have an ANC20 visit when you are not required to have one -- these should not be queried
  mutate(UNEXPECTED_ANC20 = ifelse(REPORTED_TYPE_VISIT == 2 & PREV_VISIT_BINARY == 1 & (GA_US_DAYS >= 126 & GA_US_DAYS <140), 1, 0))
#filter(UNEXPECTED_ANC20 != 1) ## we can filter here if we do not want to include these

## Extracting Visit type errors: 
# generate new var if the reported visit is early or late compared to expected
# look for duplicates (using MOMID, PREGID, TYPE_VISIT) as keys 
# generate new variable with all reported visit types for each participant
if (dim(maternal_all_anc)[1] >= 1) {
  mat_visit_type_errors_anc <- maternal_all_anc %>% 
    # determine if the window is ontime or late (if exp-obs is positive, then reported visit is late; if negative, the reported visit is early)
    mutate(VISIT_TYPE_DIFF = EXPECTED_TYPE_VISIT-REPORTED_TYPE_VISIT) %>% 
    mutate(DIRECTION = ifelse(VISIT_TYPE_DIFF>0, "late", 
                              ifelse(VISIT_TYPE_DIFF<0, "early", NA))) %>% 
    # generate new variable "N" that counts the number of times a unique MOMID and REPORTED_TYPE_VISIT exist in the data
    group_by(MOMID,PREGID, FORM, REPORTED_TYPE_VISIT) %>% 
    mutate(N = n()) %>% 
    ungroup() %>% 
    # if a REPORTED_TYPE_VISIT exists more than once for a participant (N>1) then we are considering this a duplicate
    mutate(DUP_REPORTED_TYPE_VISIT = ifelse(N>1, 1, 0)) %>% 
    group_by(MOMID, PREGID, FORM) %>% 
    # generate a new variable "ALL_REPORTED_VISITS" that contains a string with all reported visit types for that participant
    mutate(ALL_REPORTED_VISITS = paste(REPORTED_TYPE_VISIT, collapse = ",")) %>% 
    ungroup() %>% 
    filter(EXPECTED_TYPE_VISIT != 88)
  
  # Generate query error messages with the following logic, where DIRECTION indicates if the reported visit is earlier or later than expected:
  ## if direction = LATE: 
  # QUERY if REPORTED_TYPE_VISIT is a duplicate (DUP_REPORTED_TYPE_VISIT == 1)
  # QUERY if EXPECTED_TYPE_VISIT already exists (EXISTS_EXPECTED_TYPE_VISIT == 1)
  ## if direction = EARLY: 
  # QUERY if EXPECTED_TYPE_VISIT does NOT already exists (EXISTS_EXPECTED_TYPE_VISIT == 0)
  mat_visit_type_errors_anc_out <- mat_visit_type_errors_anc %>% 
    group_by(MOMID, PREGID, FORM) %>%
    mutate(REPORTED_TYPE_VISIT = as.numeric(REPORTED_TYPE_VISIT)) %>% 
    # generate variable if expected visit type already exists for the particpant
    mutate(EXISTS_EXPECTED_TYPE_VISIT = ifelse(grepl(EXPECTED_TYPE_VISIT, ALL_REPORTED_VISITS), 1, 0)) %>% 
    # generate error message variable
    mutate(ERROR_MESSAGE = ifelse(EXPECTED_TYPE_VISIT == REPORTED_TYPE_VISIT, "no error",
                                  ifelse(VISITDATE == ymd("2007-07-07") | VISITDATE== ymd("1907-07-07"), "visit date is default value",
                                         ifelse(DIRECTION == "early" & EXISTS_EXPECTED_TYPE_VISIT == 0,"too early for reported visit type and expected visit type does not yet exist for this particpant for this form",
                                                ifelse(DIRECTION == "late" &  DUP_REPORTED_TYPE_VISIT == 1, "reported visit type is a duplicate for this form", 
                                                       ifelse(DIRECTION == "late" &  EXISTS_EXPECTED_TYPE_VISIT == 1,"expected visit type already exists for this particpant for this form",
                                                              "no error"))))))%>% 
    # if visit date == default value, replace DIRECTION with "invalid visit date"
    mutate(DIRECTION = ifelse(ERROR_MESSAGE == "visit date is default value", "invalid visit date", DIRECTION)) %>% 
    filter(ERROR_MESSAGE != "no error") # filter out non-queries
  
  
  # extract visit dates that are default values to a separate dataframe - we will print these out later
  visit_date_defualt_value_anc <- mat_visit_type_errors_anc_out %>% 
    filter(ERROR_MESSAGE == "visit date is default value")
  
  # filter out default value dates, add INFANTID column, and subset variable names
  mat_visit_type_errors_anc_out <- mat_visit_type_errors_anc_out %>% 
    mutate(INFANTID = NA) %>% 
    filter(!EXPECTED_TYPE_VISIT %in% c(13,14)) %>% 
    select(FORM, MOMID, PREGID, INFANTID, PERIOD, VISITDATE, GA_US_WKS, GA_AT_VISIT_DAYS, GA_AT_VISIT_WKS,
           REPORTED_TYPE_VISIT, EXPECTED_TYPE_VISIT, DIRECTION,ALL_REPORTED_VISITS,ERROR_MESSAGE)
  
  # rename columns
  names(mat_visit_type_errors_anc_out) = c("FORM", "MOMID", "PREGID","INFANTID", "PERIOD", "VISITDATE",
                                           "GESTAGE_AT_ENROLLMENT_WKS",  "GESTAGE_AT_VISIT_DAYS", "GESTAGE_AT_VISIT_WKS",
                                           "REPORTED_TYPE_VISIT", "EXPECTED_TYPE_VISIT","DIRECTION","ALL_REPORTED_VISITS",
                                           "ERROR_MESSAGE")
}



#*****************************************************************************
## PNC visits 
#*****************************************************************************
## Data processing
# Extract all PNC visits 
# calculate GA at each visits for PNC visits 
# generate EXPECTED type visits 
pnc_visits = c(7,8,9,10,11, 12)
maternal_all_pnc = maternal_all %>% filter(TYPE_VISIT %in% pnc_visits) %>% 
  select(-GA_US_DAYS, -GA_US_WKS, -US_OHOSTDAT) %>% 
  group_by(MOMID, PREGID, FORM) %>% 
  mutate(PERIOD = "PNC") %>% 
  rename("REPORTED_TYPE_VISIT" = "TYPE_VISIT") %>% 
  # extract the previous visit the participant had 
  arrange(-desc(REPORTED_TYPE_VISIT)) %>% 
  mutate(PREV_VISIT_TYPE = dplyr::lag(REPORTED_TYPE_VISIT, n = 1)) %>% 
  # generate binary var if the participant had the previous visit where 1=Yes and 0=No
  mutate(PREV_VISIT_BINARY = ifelse(REPORTED_TYPE_VISIT == REPORTED_TYPE_VISIT & PREV_VISIT_TYPE==(REPORTED_TYPE_VISIT-1), 1, 0)) %>% 
  ungroup() %>% 
  # generate variable for expected visit type 
  ## need to confirm if this needs to have a +1 to account for day 1 of life 
  mutate(DAYS_PNC = as.numeric(VISITDATE - DOB)) %>%  ## calculate the days difference from the visit to the enrollment ultrasound
  ## convert to weeks
  mutate(WKS_PNC = DAYS_PNC %/% 7) %>% 
  mutate(EXPECTED_TYPE_VISIT = ifelse(DAYS_PNC >= 3 & DAYS_PNC <= 5, 7, 
                                      ifelse(DAYS_PNC >= 7 & DAYS_PNC <= 14, 8, 
                                             ifelse(DAYS_PNC >= 28 & DAYS_PNC <= 35, 9, 
                                                    ifelse(DAYS_PNC >= 42 & DAYS_PNC <= 104, 10, 
                                                           ifelse(DAYS_PNC >= 182 & DAYS_PNC <= 279, 11,
                                                                  ifelse(DAYS_PNC >= 364 & DAYS_PNC <= 454, 12,
                                                                         ## code unscheduled visits
                                                                         ifelse((DAYS_PNC>0 & DAYS_PNC <= 2) | (DAYS_PNC >=6 & DAYS_PNC < 7) | 
                                                                                  (DAYS_PNC >=15 & DAYS_PNC <=27) | (DAYS_PNC >=36 & DAYS_PNC <= 41) |
                                                                                  (DAYS_PNC >=105 & DAYS_PNC <= 181) | (DAYS_PNC >=280 & DAYS_PNC <= 363), 14, 88))))))))

## remove any duplicates -- these will be flagged in the duplicate id code
out_dup_ids <- maternal_all_pnc[duplicated(maternal_all_pnc[,c("MOMID", "PREGID", "REPORTED_TYPE_VISIT", "FORM")]),]
out_dup_ids <- out_dup_ids %>% pull(MOMID)
out_dup_ids <- maternal_all_pnc %>% filter(!(MOMID %in% out_dup_ids))

## Extracting Visit type errors: 
# generate new var if the reported visit is early or late compared to expected
# look for duplicates (using MOMID, PREGID, TYPE_VISIT) as keys 
# generate new variable with all reported visit types for each participant

if (dim(maternal_all_pnc)[1] >= 1) {
  
  mat_visit_type_errors_pnc <- maternal_all_pnc %>% 
    # determine if the window is ontime or late (if exp-obs is positive, then reported visit is late; if negative, the reported visit is early)
    mutate(VISIT_TYPE_DIFF = EXPECTED_TYPE_VISIT-REPORTED_TYPE_VISIT) %>% 
    mutate(DIRECTION = ifelse(VISIT_TYPE_DIFF>0, "late", 
                              ifelse(VISIT_TYPE_DIFF<0, "early", NA))) %>% 
    # generate new variable "N" that counts the number of times a unique MOMID and REPORTED_TYPE_VISIT exist in the data
    group_by(MOMID,PREGID, FORM, REPORTED_TYPE_VISIT) %>% 
    mutate(N = n()) %>% 
    ungroup() %>% 
    # if a REPORTED_TYPE_VISIT exists more than once for a participant (N>1) then we are considering this a duplicate
    mutate(DUP_REPORTED_TYPE_VISIT = ifelse(N>1, 1, 0)) %>% 
    group_by(MOMID, PREGID, FORM) %>% 
    # generate a new variable "ALL_REPORTED_VISITS" that contains a string with all reported visit types for that participant
    mutate(ALL_REPORTED_VISITS = paste(REPORTED_TYPE_VISIT, collapse = ",")) %>% 
    ungroup() %>% 
    filter(EXPECTED_TYPE_VISIT != 88)
  
  # Generate query error messages with the following logic, where DIRECTION indicates if the reported visit is earlier or later than expected:
  ## if direction = LATE: 
  # QUERY if REPORTED_TYPE_VISIT is a duplicate (DUP_REPORTED_TYPE_VISIT == 1)
  # QUERY if EXPECTED_TYPE_VISIT already exists (EXISTS_EXPECTED_TYPE_VISIT == 1)
  ## if direction = EARLY: 
  # QUERY if EXPECTED_TYPE_VISIT does NOT already exists (EXISTS_EXPECTED_TYPE_VISIT == 0)
  
  mat_visit_type_errors_pnc_out <- mat_visit_type_errors_pnc %>% 
    group_by(MOMID, PREGID, FORM) %>% 
    mutate(EXISTS_EXPECTED_TYPE_VISIT = ifelse(grepl(EXPECTED_TYPE_VISIT, ALL_REPORTED_VISITS), 1, 0)) %>% 
    mutate(ERROR_MESSAGE = ifelse(EXPECTED_TYPE_VISIT == REPORTED_TYPE_VISIT, "no error",
                                  ifelse(VISITDATE == ymd("2007-07-07") | VISITDATE== ymd("1907-07-07"), "visit date is default value",
                                         ifelse(DIRECTION == "early" & EXISTS_EXPECTED_TYPE_VISIT == 0,"too early for reported visit type and expected visit type does not yet exist for this particpant for this form",
                                                ifelse(DIRECTION == "late" &  DUP_REPORTED_TYPE_VISIT == 1, "reported visit type is a duplicate for this form", 
                                                       ifelse(DIRECTION == "late" &  EXISTS_EXPECTED_TYPE_VISIT == 1,"expected visit type already exists for this particpant for this form",
                                                              "no error")))))) %>% 
    # if visit date == default value, replace DIRECTION with "invalid visit date"
    mutate(DIRECTION = ifelse(ERROR_MESSAGE == "visit date is default value", "invalid visit date", DIRECTION)) %>% 
    # remove non-queries
    filter(ERROR_MESSAGE != "no error") 
  
  # extract visit dates that are default values to a separate dataframe - we will print these out later
  visit_date_defualt_value_pnc <- mat_visit_type_errors_pnc_out %>% 
    filter(ERROR_MESSAGE == "visit date is default value")
  
  # filter out default value dates, add INFANTID column, and subset variable names
  mat_visit_type_errors_pnc_out <- mat_visit_type_errors_pnc_out %>% 
    filter(ERROR_MESSAGE != "visit date is default value",
           !EXPECTED_TYPE_VISIT %in% c(13,14)) %>% 
    mutate(INFANTID = NA) %>% 
    select(FORM, MOMID, PREGID, INFANTID, PERIOD, VISITDATE, DAYS_PNC, WKS_PNC,
           REPORTED_TYPE_VISIT, EXPECTED_TYPE_VISIT, DIRECTION, ALL_REPORTED_VISITS, ERROR_MESSAGE)
  
  # rename columns 
  names(mat_visit_type_errors_pnc_out) = c("FORM", "MOMID", "PREGID","INFANTID", "PERIOD", "VISITDATE",
                                           "GESTAGE_AT_VISIT_DAYS", "GESTAGE_AT_VISIT_WKS",
                                           "REPORTED_TYPE_VISIT", "EXPECTED_TYPE_VISIT","DIRECTION",
                                           "ALL_REPORTED_VISITS", "ERROR_MESSAGE")
  
  
}

#*****************************************************************************
## Format to match query template
#*****************************************************************************
if (exists("mat_visit_type_errors_anc_out")== TRUE){
  # extract variables included in query template
  maternal_all_anc_query <- mat_visit_type_errors_anc_out %>% 
    rename("TYPE_VISIT" = "REPORTED_TYPE_VISIT") %>% 
    mutate(SCRNID = NA,
           INFANTID = NA, 
           VARIABLENAME = "TYPE_VISIT", 
           VARIABLEVALUE = TYPE_VISIT, 
           FIELD_TYPE = "Number", 
           EDIT_TYPE = "Invalid Visit Type") %>% 
    select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT,VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE,FIELD_TYPE, EDIT_TYPE) 
  
  
  
  # update naming 
  names(maternal_all_anc_query) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
}

if (exists("mat_visit_type_errors_pnc_out") == TRUE) {
  
  # extract variables included in query template
  maternal_all_pnc_query <- mat_visit_type_errors_pnc_out %>% 
    rename("TYPE_VISIT" = "REPORTED_TYPE_VISIT") %>% 
    mutate(SCRNID = NA,
           INFANTID = NA, 
           TYPE_VISIT = TYPE_VISIT,
           VARIABLENAME = "TYPE_VISIT", 
           VARIABLEVALUE = TYPE_VISIT, 
           FIELD_TYPE = "Number", 
           EDIT_TYPE = "Invalid Visit Type") %>% 
    select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE,FIELD_TYPE, EDIT_TYPE) 
  
  # update naming 
  names(maternal_all_pnc_query) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
  
}

## merge anc and pnc visit queries together
if (exists("maternal_all_anc_query")== TRUE & exists("maternal_all_pnc_query")== TRUE ){
  
  MaternalVisitType_query_to_export <- bind_rows(maternal_all_anc_query, maternal_all_pnc_query)
  
} else if(exists("maternal_all_anc_query")== TRUE & exists("maternal_all_pnc_query")== FALSE ) {
  
  MaternalVisitType_query_to_export <- maternal_all_anc_query
  
} else if(exists("maternal_all_anc_query")== FALSE  & exists("maternal_all_pnc_query")== TRUE ) {
  
  MaternalVisitType_query_to_export <- maternal_all_pnc_query
  
}

## pull MNH22 protocol deviation IDs that need to be removed from the visit type queries 
if (exists("mnh22")==TRUE){
  m22_ids_to_remove <- mnh22 %>% filter(DVDECOD==2) %>%  # option 2, Follow-up visit conducted outside of the approved window
    rename("MomID" =MOMID) %>% 
    pull(MomID)
}

if (dim(MaternalVisitType_query_to_export)[1] > 0){
  
  if (exists("m22_ids_to_remove")){
    ## remove all IDs that have a visit type protocol deviation
    MaternalVisitType_query_to_export <- MaternalVisitType_query_to_export %>% filter(!MomID %in% m22_ids_to_remove)
  }
  
  ## add additional columns 
  MaternalVisitType_query_to_export = cbind(QueryID = NA, 
                                            UploadDate = UploadDate, 
                                            MaternalVisitType_query_to_export, 
                                            DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  MaternalVisitType_query_to_export$Form_Edit_Type <- paste(MaternalVisitType_query_to_export$Form,"_",MaternalVisitType_query_to_export$EditType)
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  MaternalVisitType_query_to_export <- MaternalVisitType_query_to_export %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "06"))
  
  MaternalVisitType_query <- MaternalVisitType_query_to_export
  ## export variable checking query 
  save(MaternalVisitType_query, file = paste0(path_to_save,"/MaternalVisitType_query.rda"))
  
}

#*****************************************************************************
## Additional query to extract MOMIDs with visits happening on the same day
# EXAMPLE: visit type = 4 & visit date = 01 Jan 2023
# AND visit type = 5 & visit date = 01 Jan 2023
# we want to flag these 
#*****************************************************************************
# ANC (using "maternal_all_anc" data frame generate above)
library(lubridate)

# Check if maternal_all_anc exists
if (exists("maternal_all_anc")) {
  
  # Ensure VISITDATE is in Date format
  maternal_all_anc <- maternal_all_anc %>%
    mutate(VISITDATE = as.Date(VISITDATE)) 
  
  # Identify duplicate visit dates excluding specific ones
  duplicate_visit_dates_anc <- maternal_all_anc %>%
    group_by(MOMID, PREGID, VISITDATE, FORM) %>%
    filter(!VISITDATE %in% as.Date(c("2007-07-07", "1907-07-07", "1905-05-05"))) %>%  # Fix date filtering
    mutate(N = n()) %>%
    filter(N > 1) %>%
    arrange(desc(MOMID), desc(VISITDATE), desc(FORM)) %>% filter 
}
# PNC (using "maternal_all_pnc" data frame generate above)
if (exists("maternal_all_pnc") == TRUE){
  
  duplicate_visit_dates_pnc <- maternal_all_pnc %>% 
    mutate(VISITDATE = as.Date(VISITDATE)) %>%
    group_by(MOMID, PREGID, VISITDATE, FORM) %>% 
      filter(!VISITDATE %in% as.Date(c("2007-07-07", "1907-07-07", "1905-05-05"))) %>%  # Fix date filtering
    mutate(N = n()) %>% 
    filter(N>1) %>% 
    arrange(desc(MOMID), desc(VISITDATE), desc(FORM))
  
}

if (exists("duplicate_visit_dates_anc")== TRUE & exists("duplicate_visit_dates_pnc")== TRUE ){
  
  duplicate_visit_dates_to_export <- bind_rows(duplicate_visit_dates_anc, duplicate_visit_dates_pnc)
  
} else if (exists("duplicate_visit_dates_anc")== TRUE  & exists("duplicate_visit_dates_pnc")== FALSE ){
  
  duplicate_visit_dates_to_export <- duplicate_visit_dates_anc
  
} else if (exists("duplicate_visit_dates_anc")== FALSE  & exists("duplicate_visit_dates_pnc")== TRUE ) {
  
  duplicate_visit_dates_to_export <- duplicate_visit_dates_pnc
}


if (exists("duplicate_visit_dates_to_export")) {
  
  # extract variables included in query template
  Duplicate_Visit_Dates_query_to_export <- duplicate_visit_dates_to_export %>% 
    rename("TYPE_VISIT" = "REPORTED_TYPE_VISIT") %>% 
    mutate(SCRNID = NA,
           INFANTID = NA, 
           TYPE_VISIT = TYPE_VISIT,
           VARIABLENAME = "TYPE_VISIT", 
           VARIABLEVALUE = TYPE_VISIT, 
           FIELD_TYPE = "Number", 
           EDIT_TYPE = "Duplicate Visit Type") %>% 
    select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE,FIELD_TYPE, EDIT_TYPE) 
  
  # update naming 
  names(Duplicate_Visit_Dates_query_to_export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
  
  ## add additional columns 
  Duplicate_Visit_Dates_query_to_export = cbind(QueryID = NA, 
                                                UploadDate = UploadDate, 
                                                Duplicate_Visit_Dates_query_to_export, 
                                                DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  Duplicate_Visit_Dates_query_to_export$Form_Edit_Type <- paste(Duplicate_Visit_Dates_query_to_export$Form,"_",Duplicate_Visit_Dates_query_to_export$EditType)
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  Duplicate_Visit_Dates_query_to_export <- Duplicate_Visit_Dates_query_to_export %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "06"))
  
  Duplicate_Visit_Dates_query <- Duplicate_Visit_Dates_query_to_export
  ## export variable checking query 
  save(Duplicate_Visit_Dates_query, file = paste0(path_to_save,"/DuplicateVisitDates_query.rda"))
  
}

# dim(visit_type_query_extra_tab)
# dim(MaternalVisitType_query_to_export)
# dim(duplicate_visit_dates_anc)
# dim(Duplicate_Visit_Dates_query_to_export)

#*****************************************************************************
## generate new tab in query report to store more details  
#*****************************************************************************

if (exists("mat_visit_type_errors_anc_out")== TRUE & exists("mat_visit_type_errors_pnc_out")== TRUE ){
  
  maternal_visit_types_full <- bind_rows(mat_visit_type_errors_anc_out, mat_visit_type_errors_pnc_out)
  
} else if (exists("mat_visit_type_errors_anc_out")== TRUE  & exists("mat_visit_type_errors_pnc_out")== FALSE ){
  
  maternal_visit_types_full <- mat_visit_type_errors_anc_out
  
} else if (exists("mat_visit_type_errors_anc_out")== FALSE  & exists("mat_visit_type_errors_pnc_out")== TRUE ) {
  
  maternal_visit_types_full <- mat_visit_type_errors_pnc_out
}


visit_type_query_extra_tab <- maternal_visit_types_full # will eventually add in in infant here: bind_rows(maternal_visit_types_full, infant_all_pnc_out_full)

if (dim(visit_type_query_extra_tab)[1] >0){
  
  names(visit_type_query_extra_tab) = c("Form", "MomID", "PregID", "InfantID", "Period", "VisitDate", "Gestage_At_Enrollment_Wks", 
                                        "Gestage_At_Visit_Days", "Gestage_At_Visit_Wks", "Reported_Type_Visit", "Expected_Type_Visit", 
                                        "Direction", "All_Reported_Visits", "Error_Message")
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  visit_type_query_extra_tab <- visit_type_query_extra_tab %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_","TYPE_VISIT", "_", Reported_Type_Visit, "_", "06"))
  
  
  if (exists("m22_ids_to_remove")){
    ## remove all IDs that have a visit type protocol deviation
    visit_type_query_extra_tab <- visit_type_query_extra_tab %>% filter(!MomID %in% m22_ids_to_remove)
  }
  
  ## export 
  save(visit_type_query_extra_tab, file = paste0(path_to_save,"/visit_type_extra_tab.rda"))
  
}


#*****************************************************************************
## Additional GSED Query: 
#1. To determine if age of assessment correlates with TYPE_VISIT variable
#2. To identify missing TYPE_VISIT variables 
#*****************************************************************************
if (exists("mnh09") == TRUE){
  
  mnh09_dob <- mnh09 %>%
    select(MOMID, PREGID, INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4, DELIV_DSSTDAT_INF1,
           DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, 
           DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4, BIRTH_DSTERM_INF1,BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3,
           BIRTH_DSTERM_INF4, SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4) %>% 
    mutate(SEX_INF1 = as.numeric(SEX_INF1), SEX_INF2 = as.numeric(SEX_INF2), SEX_INF3 = as.numeric(SEX_INF3), SEX_INF4 = as.numeric(SEX_INF4)) %>%    # Date parsing and conversion
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
                                            NA))
  
  # Getting the Date of Birth, Sex and Birth Outcome for Each ID
  Infant_DOB <- mnh09_dob %>%
    # Pivot the data from wide to long format
    pivot_longer(
      # Select columns to pivot (INFANTID_INF1-4 and DELIVERY_DATETIME_INF1-4)
      cols = c(
        INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4,
        DELIVERY_DATETIME_INF1, DELIVERY_DATETIME_INF2, DELIVERY_DATETIME_INF3, DELIVERY_DATETIME_INF4, BIRTH_DSTERM_INF1,
        BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3, BIRTH_DSTERM_INF4,SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4),
      # Specify how to separate column names: extract suffixes and values
      names_to = c(".value", "infant_suffix"),
      # Define the pattern: splitting by "_INF" and matching the suffix
      names_pattern = "(.*)_INF(\\d)$"
    ) %>%
    # Rename the columns
    rename(
      INFANTID = INFANTID,
      DOB = DELIVERY_DATETIME
    ) %>%
    # Drop the suffix column since it was used for reshaping
    select(MOMID, PREGID, INFANTID, DOB, BIRTH_DSTERM, SEX )  %>%
    # Filter out rows where INFANTID is NA
    filter(INFANTID != "") %>% 
    mutate (MNH09_COMP = 1)
  
} else { 
  print(paste0("No MNH09 uploaded ", site))
  stop("Process stopped due to missing delivery form.")
}

if (exists("mnh08") == TRUE){
  
  remapp_ids <- mnh08 %>% filter (LB_REMAPP3 == 1 | BLEAD_LBPERF_1 == 1 | SCHISTO_LBPERF_1 == 1 |
                                    SCHISTO_LBPERF_STOOL == 1 | HELM_LBPERF_1 == 1 | BLD_MORPH_LBPERF_1 == 1 |
                                    BLD_MORPH_LBPERF_2 == 1 | BLD_MORPH_LBPERF_3 == 1| BLD_MORPH_LBPERF_4 == 1 ) %>% 
    select (MOMID, PREGID) %>% 
    distinct()
  
}

#Function to rename variables
# List of standard variable names to ensure are present and to move to the front
standard_names <- c("MOMID", "PREGID", "INFANTID", "TYPE_VISIT", "VISIT_OBSSTDAT", "DATE.TODAY")
standard_names_v <- c("MOMID", "PREGID", "INFANTID", "TYPE_VISIT", "VISIT_OBSSTDAT")
standard_names_d <- c("MOMID", "PREGID", "INFANTID", "TYPE_VISIT", "DATE.TODAY")



# A helper function to find and rename similar variable names
rename_similar_vars <- function(df, standard_names) {
  
  # Get current variable names in the dataset
  current_names <- names(df)
  
  # Loop through each standard variable name
  for (standard_name in standard_names) {
    
    # Find any current names that are similar to the standard name (based on string similarity)
    similar_name <- current_names[str_detect(current_names, regex(standard_name, ignore_case = TRUE))]
    
    # If a similar name is found but it's not exactly the standard name, rename it
    if (length(similar_name) == 1 && similar_name != standard_name) {
      df <- df %>%
        rename(!!standard_name := all_of(similar_name))
      message(paste("Renamed", similar_name, "to", standard_name))
    }
  }
  
  return(df)
}

# GSED Short Form 

# List of forms to process
form_list <- list("mnh30", "mnh31", "mnh32")
output_list <- list()
extra_list <- list()

# Function to process each form
process_form <- function(form_name) {
  if (exists(form_name)) {
    form_data <- get(form_name)
    
    # Capitalize variable names
    names(form_data) <- toupper(names(form_data))
    
    # Apply the function to clean and standardize variable names
    form_cleaned <- rename_similar_vars(form_data, standard_names)
    
    # Check for visit date columns and create VISITDATE
    if ("VISIT_OBSSTDAT" %in% colnames(form_cleaned)) {
      form_reordered <- form_cleaned %>%
        select(all_of(standard_names_v)) %>%
        mutate(
          DATE_CHANGED = cleaned_dates <- gsub(" .*", "", VISIT_OBSSTDAT),
          VISITDATE = case_when(
            # Check if the first part of the date seems to be the day
            grepl("^([0-2][0-9]|3[0-1])/([0-1][0-9])/", DATE_CHANGED) ~ dmy(DATE_CHANGED),
            # Check if the first part of the date seems to be the month
            grepl("^([0-1][0-9])/([0-2][0-9]|3[0-1])/", DATE_CHANGED) ~ mdy(DATE_CHANGED),
            # Default to parsing with common formats
            TRUE ~ ymd(parse_date_time(DATE_CHANGED, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))
          ),
          VISITDATE = replace(VISITDATE, VISITDATE %in% c(ymd("1907-07-07"), ymd("1905-05-05"), ymd("2007-07-07")), NA),
          FORM_COMP = 1,
          REPORTED_DATE = VISIT_OBSSTDAT
        )
    } else if ("DATE.TODAY" %in% colnames(form_cleaned)) {
      form_reordered <- form_cleaned %>%
        select(all_of(standard_names_d)) %>%
        mutate(
          VISITDATE = case_when(
            # Check if the first part of the date seems to be the day
            grepl("^([0-2][0-9]|3[0-1])/([0-1][0-9])/", DATE.TODAY) ~ dmy(DATE.TODAY),
            # Check if the first part of the date seems to be the month
            grepl("^([0-1][0-9])/([0-2][0-9]|3[0-1])/", DATE.TODAY) ~ mdy(DATE.TODAY),
            # Default to parsing with common formats
            TRUE ~ ymd(parse_date_time(DATE.TODAY, orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))
          ),
          VISITDATE = replace(VISITDATE, VISITDATE %in% c(ymd("1907-07-07"), ymd("1905-05-05"), ymd("2007-07-07")), NA),
          FORM_COMP = 1,
          REPORTED_DATE = DATE.TODAY
        )
    } else {
      print(paste("No Visit Date in", form_name, "so skip to next form"))
      return(NULL)
    }
    # Perform the left join with INFANT_DOB by MOMID, PREGID, INFANTID
    form_prep <- form_reordered %>%
      left_join(Infant_DOB, by = c("MOMID", "PREGID", "INFANTID")) %>%
      mutate(
        DOB_Date = as.Date(trunc(DOB, 'days')),
        AGE_IN_DAYS = round(as.numeric(difftime(VISITDATE, DOB_Date, units = "days")), 0),
        AGE_IN_WEEKS = AGE_IN_DAYS %/% 7,
        TYPE_VISIT_CONS = case_when(
          AGE_IN_DAYS >= 182 & AGE_IN_DAYS <= 363 ~ 11,
          AGE_IN_DAYS >= 364 & AGE_IN_DAYS <= 426 ~ 12,
          AGE_IN_DAYS >= 91 & AGE_IN_DAYS <= 181 ~ 14,
          TRUE ~ NA
        ),
        Visit_Window = case_when(
          AGE_IN_DAYS >= 364 & AGE_IN_DAYS <= 426 ~ "On-Time: 52 to 54 wks; Late: 55 to <61 wks",
          AGE_IN_DAYS >= 182 & AGE_IN_DAYS <= 363 ~ "On-Time: 26 to 28 wks; Late: 29 to <52 wks",
          AGE_IN_DAYS >= 91 & AGE_IN_DAYS <= 181 ~ "On-Time: 13 to 15 wks; Late: 16 to <26 wks",
          AGE_IN_DAYS < 0 | is.na(VISITDATE) ~ "Visit date error",
          TRUE ~ "Not expected: protocol deviation"
        )
      )
    
    # Filter non-queries
    form_out <- form_prep %>%
      filter((!is.na(DOB) & TYPE_VISIT != TYPE_VISIT_CONS) | is.na(VISITDATE) | is.na(TYPE_VISIT))
    
    if (nrow(form_out) > 0) {
      form_query <- form_out %>%
        mutate(
          Error_message = case_when(
            is.na(VISITDATE) ~ "visit date is missing",
            AGE_IN_DAYS < 0 ~ "infant age is less than 0",
            AGE_IN_DAYS < 91 & TYPE_VISIT_CONS == 14 ~ "three-month visit (VisitType 14) completed early",
            TYPE_VISIT_CONS == 14 & TYPE_VISIT != 14 ~ "three-month visit (VisitType 14) expected",
            TYPE_VISIT_CONS == 11 & TYPE_VISIT != 11 ~ "six-month visit (VisitType 11) expected",
            TYPE_VISIT_CONS == 12 & TYPE_VISIT != 12 ~ "twelve-month visit (VisitType 12) expected",
            AGE_IN_DAYS > 426 ~ "twelve-month (VisitType 12) visit completed late",
            is.na(TYPE_VISIT) ~ "visit type is missing",
            TRUE ~ "visit type error"
          )
        ) %>%
        mutate(
          SCRNID = NA,
          FORM = toupper(form_name),
          VARIABLENAME = "TYPE_VISIT",
          VARIABLEVALUE = TYPE_VISIT,
          FIELD_TYPE = "Number",
          EDIT_TYPE = "GSED Visit Type Error"
        )
      
      output <- form_query %>%
        select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE, FIELD_TYPE, EDIT_TYPE)
      
      extra <- form_query %>%
        select(FORM, MOMID, PREGID, INFANTID, REPORTED_DATE, VISITDATE, DOB_Date, AGE_IN_DAYS, AGE_IN_WEEKS, Visit_Window, TYPE_VISIT, TYPE_VISIT_CONS, Error_message)
      
      names(extra) <- c("Form", "MomID", "PregID", "InfantID", "Reported Date" , "VisitDate", "DOB", "Age (Days)", "Age (Weeks)", "Expected Window", "Reported Visit Type", "Expected Visit Type", "Error Message")
      
      names(output) <- c("ScrnID", "MomID", "PregID", "InfantID", "VisitType", "VisitDate", "Form", "Variable Name", "Variable Value", "FieldType", "EditType")
      
      return(list(output = output, extra = extra))
    } else {
      print(paste(form_name, "query not uploaded"))
      return(NULL)
    }
  } else {
    print(paste(form_name, "form not uploaded"))
    return(NULL)
  }
}

# Process all forms
for (form in form_list) {
  result <- process_form(form)
  if (!is.null(result)) {
    output_list[[form]] <- result$output
    extra_list[[form]] <- result$extra
  }
}

# Combine all outputs and extras
GSED_visit_query_output <- bind_rows(output_list)
GSED_visit_query_extra <- bind_rows(extra_list) %>%
mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_", "TYPE_VISIT", "_", `Reported Visit Type`, "_", "06"))

if (exists("GSED_visit_query_output") == TRUE) {
  
  ## add additional columns 
  GSED_visit_query_output_to_export = cbind(QueryID = NA, 
                                            UploadDate = UploadDate, 
                                            GSED_visit_query_output, 
                                            DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  GSED_visit_query_output_to_export$Form_Edit_Type <- paste(GSED_visit_query_output_to_export$Form,"_",GSED_visit_query_output_to_export$EditType)
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  GSED_visit_query_output_to_export <- GSED_visit_query_output_to_export %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "06"))
  
  GSED_visit_query <- GSED_visit_query_output_to_export
  ## export variable checking query 
  save(GSED_visit_query, file = paste0(path_to_save,"/GSEDVisitType_query.rda"))
  save(GSED_visit_query_extra, file = paste0(path_to_save,"/GSEDVisitType_query_extra.rda"))
  
} else { 
  print(paste0("No GSED Query for ", site))
  stop("Process stopped due to missing GSED Query.")
}
