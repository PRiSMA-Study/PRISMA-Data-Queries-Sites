#*****************************************************************************
#*QUERY #5 -- 
# 1. CONFIRM ALL "ENROLLED" PARTICIPANTS MEET PRISMA ENROLLMENT ELIGIBILITY CRITERIA
# 2. CONFIRM ALL PARTICIPANTS WHO ARE NOT ELIGIBLE BY PRESCREENING CRITERIA HAVE AN EXCLUSION CRITERIA
# 3. CONFIRM ALL PARTICPANTS WHO HAVE PASSED THE IPC WINDOW HAVE A REPORTED BIRTH OUTCOME

#*Input: Wide data (all raw .csv files) & Long data
#*Function: Confirm all enrolled particpants meet our enrollment criteria as in MNH02
#*Output: .rda file with all MOMIDs that do not meet enrollment criteria 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* Last updated: 13 Feb 2025 
#* 
#* Changes to the eligibility reason query
#* Changes the variable for MOMID missing enrolment form to PREGID
#* Included consent variable in the ineligibility criteria
#* SCRN_RETURN removal for Ghana

#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for

#* Once the previous lines of code are updated, you can start to run the script 
#*****************************************************************************

# clear environment 
rm(list = ls())

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
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

#*****************************************************************************
####################################################################################################
# 1. CONFIRM ALL "ENROLLED" PARTICIPANTS MEET PRISMA ENROLLMENT ELIGIBILITY CRITERIA
####################################################################################################

####################################################################################################
# 1. CONFIRM ALL "ENROLLED" PARTICIPANTS MEET PRISMA ENROLLMENT ELIGIBILITY CRITERIA
####################################################################################################

## extract MOMIDs in all forms following MNH02. We will assume that any participant with these forms are considered "enrolled" by sites 
## PJW Edits - Changing MOMID to PREGID - to account for reenrolled moms 
all_momid <- data_long %>% filter(form != "MNH02" & form != "MNH00" & form != "MNH01") %>% distinct(PREGID) %>% pull(PREGID)

## subset all MOMIDs that have forms 03-25 from MNH02 (enrollment form)
momid_enroll_forms <- mnh02 %>% filter(PREGID %in% all_momid)

## Among these women who the sites are considering "enrolled", do they meet our enrollment criteria?
# 1, Yes meets enrollment criteria 
# 0, No does not meet enrollment criteria
momid_enroll_forms$MEET_ENROLL_CRIT = ifelse(momid_enroll_forms$AGE_IEORRES == 1 & momid_enroll_forms$PC_IEORRES == 1 & 
                                               momid_enroll_forms$CATCHMENT_IEORRES == 1 & momid_enroll_forms$CATCH_REMAIN_IEORRES == 1 &
                                               momid_enroll_forms$CONSENT_IEORRES == 1, 1, 0)

## Extract only MOMIDs that do not meet enrollment criteria (MEET_ENROLL_CRIT == 0)
momid_enroll_forms <- momid_enroll_forms %>% filter(MEET_ENROLL_CRIT==0)

## Update formatting - we only want to keep certain columns
momid_enroll_forms = momid_enroll_forms %>% select(SCRNID, MOMID, PREGID, SCRN_OBSSTDAT) %>% 
  mutate(InfantID = NA, 
         Form = "MNH02",
         TYPE_VISIT = NA)

## reorder columns 
momid_enroll_forms <- momid_enroll_forms[, c("SCRNID", "MOMID", "PREGID", "InfantID","TYPE_VISIT", "SCRN_OBSSTDAT", "Form")] 

## update naming
names(momid_enroll_forms) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form")

# rename data frame 
MomidNotEligible <- momid_enroll_forms

if (dim(MomidNotEligible)[1] >= 1){
  
  ## add additional columns 
  MomidNotEligible_query = cbind(QueryID = NA, 
                                 UploadDate = UploadDate, 
                                 #MomID = "NA", PregID = "NA",
                                 #VisitDate = "NA", 
                                 MomidNotEligible, 
                                 `Variable Name` = "NA",
                                 `Variable Value` = "NA",
                                 FieldType = "Text", 
                                 EditType = "MomID Ineligible", 
                                 DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  MomidNotEligible_query <- add_column(MomidNotEligible_query,Form_Edit_Type = paste(MomidNotEligible_query$Form,"_",MomidNotEligible_query$EditType))
  
  ## assign queryid -- 
  # edit type id for momid inelgibile is 08
  MomidNotEligible_query <- MomidNotEligible_query %>% 
    mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "08"))
  
  ## export 
  save(MomidNotEligible_query, file = paste0(path_to_save,"/queries/MomidNotEligible_query.rda"))
  
}

####################################################################################################
# 2. CONFIRM ALL PARTICIPANTS WHO ARE NOT ELIGIBLE BY PRESCREENING CRITERIA HAVE AN EXCLUSION CRITERIA
####################################################################################################

# Filter ineligible cases
if (site == "Pakistan") {
  Mom_ineligible <- mnh00 %>%
    filter(!(PREGNANT_IEORRES == 1 & EGA_LT25_IEORRES==77 & AGE_IEORRES == 1 & CATCHMENT_IEORRES == 1 & OTHR_IEORRES %in% c(0,77) & CON_YN_DSDECOD == 1))
  
} else {
  Mom_ineligible <- mnh00 %>%
    filter(!(PREGNANT_IEORRES == 1 & EGA_LT25_IEORRES == 1 & AGE_IEORRES == 1 & CATCHMENT_IEORRES == 1 & OTHR_IEORRES == 0 & CON_YN_DSDECOD == 1))
}


# Further filter ineligible cases with specified "Other Reasons" and cases where consent is not given
Mom_ineligible_filtered <- Mom_ineligible %>% 
  filter(!(OTHR_IEORRES == 1 & OTHR_REASON_IEORRES %in% c(1:8, 88))) %>% #filter where there were other reasons why we shoudl exclude
  filter (!(CON_YN_DSDECOD == 0)) %>% #filter out of the df where woman didn't consent
#Further filter where an ineligibility criteria was provided
  filter (!(PREGNANT_IEORRES == 0 | EGA_LT25_IEORRES == 0 | AGE_IEORRES == 0 | CATCHMENT_IEORRES == 0)) %>%
  select (SCRNID, SCRN_OBSSTDAT, PREGNANT_IEORRES, EGA_LT25_IEORRES, AGE_IEORRES, CATCHMENT_IEORRES, CON_YN_DSDECOD ,OTHR_IEORRES, OTHR_REASON_IEORRES)

# Extract eligibility reasons
EligibilityReason <- Mom_ineligible_filtered %>% 
  mutate(
    
    Form = "MNH00",
    EditType = case_when(
      (PREGNANT_IEORRES == 77 | EGA_LT25_IEORRES == 77 | AGE_IEORRES == 77 |  CATCHMENT_IEORRES == 77 ) & OTHR_IEORRES %in% c(0,77) ~ "Provide Ineligibility Criteria",
      (PREGNANT_IEORRES == 0 | EGA_LT25_IEORRES == 0 | AGE_IEORRES == 0 | CATCHMENT_IEORRES == 0) & OTHR_IEORRES == 1 & OTHR_REASON_IEORRES == 77 ~ "Ineligibility Skip Pattern Error",
      PREGNANT_IEORRES %in% c(1, 77) & EGA_LT25_IEORRES %in% c(1, 77) & AGE_IEORRES %in% c(1, 77) & CATCHMENT_IEORRES %in% c(1, 77) & OTHR_IEORRES == 1 & OTHR_REASON_IEORRES == 77 ~ "Specify Other Reason", 
      (PREGNANT_IEORRES == 1 & EGA_LT25_IEORRES == 1 & AGE_IEORRES == 1 & CATCHMENT_IEORRES == 1 & OTHR_IEORRES == 0) & CON_YN_DSDECOD %in% c(77, 55, 66, 99) ~ "Provide consent confirmation",
      TRUE ~ "No Error"),
    Form_Edit_Type = paste(Form, "_", EditType)) %>% 
  
  select(SCRNID, SCRN_OBSSTDAT, EditType, Form_Edit_Type, PREGNANT_IEORRES, EGA_LT25_IEORRES, AGE_IEORRES, CATCHMENT_IEORRES, CON_YN_DSDECOD , OTHR_IEORRES, OTHR_REASON_IEORRES) %>% 
  
  filter(!(EditType == "No Error"))

# Format Query
if (dim(EligibilityReason)[1] >= 1){
  # Add additional columns
  InEligibilityCriteria_query <- EligibilityReason %>%
    mutate(QueryID = NA, 
           UploadDate = UploadDate, 
           ScrnID = SCRNID,
           MomID = "NA", 
           PregID = "NA",
           InfantID = "NA",
           Form = "MNH00",
           VisitType = "NA",
           VisitDate = SCRN_OBSSTDAT, 
           `Variable Name` = "NA",
           `Variable Value` = "NA",
           FieldType = "Text", 
           DateEditReported = format(Sys.time(), "%Y-%m-%d")) %>%
    mutate(QueryID = paste0("MNH00_", VisitDate, "_", ScrnID, "_", EditType, "_", "10")) %>%
    select(QueryID, UploadDate, ScrnID, MomID, PregID, InfantID, Form, VisitType, VisitDate, `Variable Name`, `Variable Value`, FieldType, EditType, DateEditReported, Form_Edit_Type)
  
  #Add QueryID to comments
  QueryID <-  InEligibilityCriteria_query %>% select (QueryID, SCRNID = ScrnID)
  Comment <- merge(QueryID, EligibilityReason, by = "SCRNID")
  
  # Export data
  save(InEligibilityCriteria_query, file = paste0(path_to_save,"/queries/InEligibilityCriteria_query.rda"))
  save(Comment, file = paste0(path_to_save,"/queries/InEligibilityCriteria_comments.rda"))
}

# library(openxlsx)
# 
# file_path <- paste0(path_to_save,"/queries/", site,"_", "InEligibilityCriteria_query.xlsx") 
# 
# # Write the dataframe to an Excel file
# write.xlsx(EligibilityReason, file_path)
# 
# # Confirm that the file has been saved
# print (paste("Excel file saved to:", file_path))



####################################################################################################
# 3. CONFIRM ALL PARTICPANTS WHO HAVE PASSED THE IPC WINDOW HAVE A REPORTED BIRTH OUTCOME

# Logic: If a participant has not yet had a birth outcome reported in MNH04 OR MNH09 and has passed the 
  # late window for IPC with GA>=48 weeks, a query will be pulled. 
####################################################################################################

# pull all enrolled participants -- some sites reporting mnh01 for all screened but not enrolled participants
    ## only enrolled particpants will be expected to have a birth outcome

if ("SCRN_RETURN" %in% names(mnh02) & site != "Ghana") {

enrolled_ids <- mnh02 %>% 
  mutate(ENROLL = ifelse(AGE_IEORRES == 1 & 
                         PC_IEORRES == 1 & 
                         CATCHMENT_IEORRES == 1 & 
                         CATCH_REMAIN_IEORRES == 1 & 
                         CONSENT_IEORRES == 1, 1, 0)) %>% 
  filter(ENROLL == 1) %>% 
  select(SCRNID, MOMID, PREGID)

} else {
  
  enrolled_ids <- mnh02 %>% 
    mutate(ENROLL = ifelse(AGE_IEORRES == 1 & 
                             PC_IEORRES == 1 & 
                             CATCHMENT_IEORRES == 1 & 
                             CATCH_REMAIN_IEORRES == 1 & 
                             CONSENT_IEORRES == 1, 1, 0)) %>% 
    filter(ENROLL == 1) %>% 
    select(SCRNID, MOMID, PREGID)
  
  
}


if (site == "Zambia"){
  # Zambia is reporting some values as "." - this will break the flow. the following code will replace with "-7"
  mnh01_newids <- mnh01 %>% select(-MOMID, -PREGID) %>% ## remove momid and pregid vars in mnh01 - will use ids from mnh02
    right_join(enrolled_ids, by = c("SCRNID")) %>% 
    mutate_at(vars(starts_with("US_GA_")), ~ as.numeric(ifelse(. == ".", -7, as.character(.)))) 
  
} else {
  mnh01_newids <- mnh01 %>% select(-MOMID, -PREGID) %>% ## remove momid and pregid vars in mnh01 - will use ids from mnh02
    right_join(enrolled_ids, by = c("SCRNID"))
  
}

## if a site has all the forms needed for this query, the remainder of the code will run. if not, the code will stop
if (exists("mnh01")== TRUE & exists("mnh09")== TRUE){
  
  print("All forms required to run the GA>42 weeks query exist.")
  
} else {
  
  stop("SITE does not have all forms required to run the GA>42 weeks query - don't run.")
  
}

# India-CMC is already reporting acog (CAL_GA_WKS_AGE_FTS1-4 & CAL_GA_DAYS_AGE_FTS1-4) - use these estimates as the BOE for CMC site
if (site=="India-CMC" | site=="India-SAS"){
  
  ## only extract key variables from each of the forms that check for visit type 
  mnh01_sub <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT, contains("CAL_GA_WKS_AGE_"), contains("CAL_GA_DAYS_AGE_")) %>% 
    filter(TYPE_VISIT == 1) %>% 
    ## extract the maximum gestational age for each woman 
    mutate(GA_US_DAYS_FTS1 =  ifelse(CAL_GA_WKS_AGE_FTS1!= -7 & CAL_GA_DAYS_AGE_FTS1 != -7,  (CAL_GA_WKS_AGE_FTS1 * 7 + CAL_GA_DAYS_AGE_FTS1), NA), 
           GA_US_DAYS_FTS2 =  ifelse(CAL_GA_WKS_AGE_FTS2!= -7 & CAL_GA_DAYS_AGE_FTS2 != -7,  (CAL_GA_WKS_AGE_FTS2 * 7 + CAL_GA_DAYS_AGE_FTS2), NA),
           GA_US_DAYS_FTS3 =  ifelse(CAL_GA_WKS_AGE_FTS3!= -7 & CAL_GA_DAYS_AGE_FTS3 != -7,  (CAL_GA_WKS_AGE_FTS3 * 7 + CAL_GA_DAYS_AGE_FTS3), NA),
           GA_US_DAYS_FTS4 =  ifelse(CAL_GA_WKS_AGE_FTS4!= -7 & CAL_GA_DAYS_AGE_FTS4 != -7,  (CAL_GA_WKS_AGE_FTS4 * 7 + CAL_GA_DAYS_AGE_FTS4), NA)) %>% 
    mutate(GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
    ## convert to weeks 
    mutate(GA_US_WKS = GA_US_DAYS %/% 7) %>% 
    mutate(US_OHOSTDAT = ymd(parse_date_time(US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
    ## remove any unscheduled visit types
    filter(TYPE_VISIT != 13) %>% 
    # "zero out" GA and obtain the estimated "date of conception" 
    mutate(EST_CONCEP_DATE = US_OHOSTDAT - GA_US_DAYS) %>% 
    select(MOMID, PREGID, TYPE_VISIT, US_OHOSTDAT,GA_US_DAYS, GA_US_WKS, EST_CONCEP_DATE)
  
} else {
  
  ## only extract key variables from each of the forms that check for visit type 
  mnh01_sub <- mnh01_newids %>% select(MOMID, PREGID, US_OHOSTDAT, TYPE_VISIT,
                                       US_GA_WKS_AGE_FTS1, US_GA_DAYS_AGE_FTS1, US_GA_WKS_AGE_FTS2, US_GA_DAYS_AGE_FTS2,
                                       US_GA_WKS_AGE_FTS3, US_GA_DAYS_AGE_FTS3, US_GA_WKS_AGE_FTS4, US_GA_DAYS_AGE_FTS4) %>% 
    filter(TYPE_VISIT == 1) %>% 
    ## extract the maximum gestational age for each woman 
    mutate(GA_US_DAYS_FTS1 =  ifelse(US_GA_WKS_AGE_FTS1!= -7 & US_GA_DAYS_AGE_FTS1 != -7,  (US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1), NA), 
           GA_US_DAYS_FTS2 =  ifelse(US_GA_WKS_AGE_FTS2!= -7 & US_GA_DAYS_AGE_FTS2 != -7,  (US_GA_WKS_AGE_FTS2 * 7 + US_GA_DAYS_AGE_FTS2), NA),
           GA_US_DAYS_FTS3 =  ifelse(US_GA_WKS_AGE_FTS3!= -7 & US_GA_DAYS_AGE_FTS3 != -7,  (US_GA_WKS_AGE_FTS3 * 7 + US_GA_DAYS_AGE_FTS3), NA),
           GA_US_DAYS_FTS4 =  ifelse(US_GA_WKS_AGE_FTS4!= -7 & US_GA_DAYS_AGE_FTS4 != -7,  (US_GA_WKS_AGE_FTS4 * 7 + US_GA_DAYS_AGE_FTS4), NA)) %>% 
    mutate(GA_US_DAYS = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
    ## convert to weeks 
    mutate(GA_US_WKS = GA_US_DAYS %/% 7) %>% 
    mutate(US_OHOSTDAT = ymd(parse_date_time(US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
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

# mnh04 
mnh04_sub <- mnh04 %>% 
  left_join(mnh01_sub[c("MOMID", "PREGID", "EST_CONCEP_DATE")], by = c("MOMID", "PREGID")) %>%
  mutate(FETAL_LOSS_DSSTDAT = ymd(parse_date_time(FETAL_LOSS_DSSTDAT, 
                                                  order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>%
  # replace default value date with NA
  mutate(FETAL_LOSS_DSSTDAT = replace(FETAL_LOSS_DSSTDAT, FETAL_LOSS_DSSTDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA)) %>%
  # calculate gestational age at fetal loss
  mutate(GESTAGE_FETAL_LOSS_DAYS = as.numeric(FETAL_LOSS_DSSTDAT-EST_CONCEP_DATE),
         GESTAGE_FETAL_LOSS_WKS = GESTAGE_FETAL_LOSS_DAYS %/% 7) %>%
  select(MOMID, PREGID, FETAL_LOSS_DSSTDAT, GESTAGE_FETAL_LOSS_DAYS, GESTAGE_FETAL_LOSS_WKS) %>% 
  filter(!is.na(FETAL_LOSS_DSSTDAT))
  


# mnh09 
mnh09_sub <- mnh09 %>% 
  select(MOMID, PREGID, MAT_VISIT_MNH09, contains("DELIV_DSSTDAT_INF")) %>% 
  mutate(DELIV_DSSTDAT_INF1 = ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF2 = ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF3 = ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DELIV_DSSTDAT_INF4 = ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  mutate(DELIV_DSSTDAT_INF1 = replace(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF1 %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA),
         DELIV_DSSTDAT_INF2 = replace(DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF2 %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA),
         DELIV_DSSTDAT_INF3 = replace(DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF3 %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA),
         DELIV_DSSTDAT_INF4 = replace(DELIV_DSSTDAT_INF4, DELIV_DSSTDAT_INF4 %in% c(ymd("1907-07-07"), ymd("2007-07-07")), NA)) %>% 
  mutate(DOB = pmin(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4, na.rm = TRUE)) 

## merge in mnh10 and/or mnh23 if forms exist
  # we need to merge these in to generate a variable that looks at if ANY ipc visit is complete (requires mnh10)
  # and if a woman has closed out (mnh23)

if (exists("mnh10")==TRUE & exists("mnh23")==TRUE) {
  
  ipc_forms_merged <-  mnh01_sub %>% 
    select(MOMID, PREGID, EST_CONCEP_DATE) %>% 
    # merge in mnh04 fetal loss information
    full_join(mnh04_sub, by = c("MOMID", "PREGID")) %>% 
    # merge in l&d data 
    full_join(mnh09_sub, by = c("MOMID", "PREGID")) %>% 
    # merge in mnh10 
    full_join(mnh10[c("MOMID", "PREGID", "MAT_VISIT_MNH10")], by = c("MOMID", "PREGID")) %>% 
    # merge in closeout data 
    full_join(mnh23[c("MOMID", "PREGID", "CLOSE_DSDECOD", "CLOSE_DSSTDAT")], by = c("MOMID", "PREGID"))

} else if (exists("mnh10")==TRUE & exists("mnh23")==FALSE) {
    
  ipc_forms_merged <-  mnh01_sub %>% 
    select(MOMID, PREGID, EST_CONCEP_DATE) %>% 
    # merge in mnh04 fetal loss information
    full_join(mnh04_sub, by = c("MOMID", "PREGID")) %>% 
    # merge in l&d data 
    full_join(mnh09_sub, by = c("MOMID", "PREGID")) %>% 
    # merge in mnh10 
    full_join(mnh10[c("MOMID", "PREGID", "MAT_VISIT_MNH10")], by = c("MOMID", "PREGID")) %>% 
    # since closeout data does not exist, make an empty column with closeout code and closeout date
    mutate(CLOSE_DSDECOD = NA, 
           CLOSE_DSSTDAT = NA)
  
} else if (exists("mnh10")==FALSE & exists("mnh23")==TRUE) {
  
  ipc_forms_merged <-  mnh01_sub %>% 
    select(MOMID, PREGID, EST_CONCEP_DATE) %>% 
    # merge in mnh04 fetal loss information
    full_join(mnh04_sub, by = c("MOMID", "PREGID")) %>% 
    # merge in l&d data 
    full_join(mnh09_sub, by = c("MOMID", "PREGID")) %>% 
    # merge in closeout data 
    full_join(mnh23[c("MOMID", "PREGID", "CLOSE_DSDECOD", "CLOSE_DSSTDAT")], by = c("MOMID", "PREGID"))
    # since mnh10 data does not exist, make an empty column with visit status
    mutate(MAT_VISIT_MNH10 = NA)
}

## generate query dataset (late window, passed windows, etc)
ipc_forms_merged_query <- ipc_forms_merged %>% 
  # generate a single variable if a mom has an end of pregnancy outcome
  mutate(ENDPREG = case_when(!is.na(FETAL_LOSS_DSSTDAT) | !is.na(DOB) ~ 1, TRUE ~0)) %>% 
  ## CALCULATE LATE IPC WINDOWS
  mutate(IPC_LATE_WINDOW = EST_CONCEP_DATE + 300) %>% 
  ## using upload date
  mutate(IPC_LATE_WINDOW_PASS = ifelse(IPC_LATE_WINDOW<UploadDate, 1, 0)) %>%  
  # merge in closeout data 
  # full_join(mnh23[c("MOMID", "PREGID", "CLOSE_DSDECOD", "CLOSE_DSSTDAT")], by = c("MOMID", "PREGID")) %>% 
  mutate(CLOSE_DSSTDAT = ymd(parse_date_time(CLOSE_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  # if a woman any closeout selected 
  mutate(CLOSED_OUT =  ifelse(is.na(CLOSE_DSDECOD), 0, 
                              ifelse(CLOSE_DSDECOD %in% c(1, 2, 3, 4, 5, 6), 1, 0)))  %>% 
  # generate visit complete variables for ANY ipc form 
  mutate(ANY_TYPE_VISIT_COMPLETE_IPC = case_when(MAT_VISIT_MNH09 %in% c(1,2) | MAT_VISIT_MNH10 %in% c(1,2) ~ 1, TRUE ~ 0)) %>% 
  ## CALCULATE INDICATOR VARIALBE FOR DENOMINATOR
  mutate(IPC_DENOM = ifelse((ANY_TYPE_VISIT_COMPLETE_IPC==1 | IPC_LATE_WINDOW_PASS==1) & 
                              ((CLOSE_DSSTDAT > IPC_LATE_WINDOW) | is.na(CLOSE_DSSTDAT)) &  # only include participants who have not yet closed out
                              CLOSED_OUT != 1, 1, 0)) %>% ## remove any participant who has been lost to follow up NEW 
  ## CALCULATE INDICATOR VARIALBE FOR MISSING ALL VISIT TYPE = 6
  mutate(GA42_MISSING_IPC = ifelse((IPC_DENOM ==1) & ANY_TYPE_VISIT_COMPLETE_IPC==0, 1, 0)) %>% 
  # generate estiamted GA today (using upload date as a proxy)
  mutate(ESTIMATE_GA_TODAY_DAYS = as.Date(UploadDate)-EST_CONCEP_DATE,
         ESTIMATE_GA_TODAY_WKS = as.numeric(ESTIMATE_GA_TODAY_DAYS) %/%7) %>% 
  # generate variable for any particpant who is missing IPC has an estimated GA >48wks 
  mutate(MISSING_IPC_GA48 = case_when(GA42_MISSING_IPC==1 & ESTIMATE_GA_TODAY_WKS>=48 ~ 1, TRUE ~ 0)) %>% 
  # only extract the queries 
  filter(MISSING_IPC_GA48 ==1) %>% 
  select(MOMID, PREGID, ENDPREG, IPC_LATE_WINDOW, IPC_LATE_WINDOW_PASS, CLOSED_OUT, CLOSE_DSSTDAT, ANY_TYPE_VISIT_COMPLETE_IPC,IPC_DENOM, GA42_MISSING_IPC, ESTIMATE_GA_TODAY_WKS, MISSING_IPC_GA48)


## FORMATTING FOR QUERY REPORT
if (exists("ipc_forms_merged_query")==TRUE) {
  
  # extract variables included in query template
  ipc_forms_merged_query_to_export <- ipc_forms_merged_query %>% 
    mutate(SCRNID = NA,
           INFANTID = NA, 
           TYPE_VISIT = NA,
           VISITDATE = NA,
           FORM = "MNH09",
           VARIABLENAME = "Estimated GA at time of upload (constructed)", 
           VARIABLEVALUE = ESTIMATE_GA_TODAY_WKS, 
           FIELD_TYPE = "Number", 
           EDIT_TYPE = "Participant GA >=48 weeks with no reported birth outcome (MNH04 or MNH09)") %>% 
    select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE,FIELD_TYPE, EDIT_TYPE) 
  
  # update naming 
  names(ipc_forms_merged_query_to_export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
  
  ## add additional columns 
  ipc_forms_merged_query_to_export = cbind(QueryID = NA, 
                                           UploadDate = UploadDate, 
                                           ipc_forms_merged_query_to_export, 
                                           DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  ipc_forms_merged_query_to_export$Form_Edit_Type <- paste(ipc_forms_merged_query_to_export$Form,"_",ipc_forms_merged_query_to_export$EditType)
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  ipc_forms_merged_query_to_export <- ipc_forms_merged_query_to_export %>% 
    mutate(QueryID = paste0(Form, "_",MomID, "_",PregID, "_", `Variable Name`))
  
  Missing_Ipc_Outcome_query <- ipc_forms_merged_query_to_export
  ## export variable checking query 
  save(Missing_Ipc_Outcome_query, file = paste0(path_to_save, "/queries/Missing_Ipc_Outcome_query.rda"))
  
}
