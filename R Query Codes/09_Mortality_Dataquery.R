#*****************************************************************************
#* QUERY #9 -- DEAD THAN ALIVE QUERY: INFANT
#* Input: Wide data (all raw .csv files) & Long data
#* Function:  Are there are new visits occurring after an infant has reported a death 
#* Output: .rda file with all InfantIDS that have a visit that occurs after the reported death
#* Logic: extract all visit dates into a column; if these dates occur after the reported death date, these are flagged for queries 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* Last updated: 29 January 2025, Stacie

#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for

#* Once the previous lines of code are updated, you can start to run the script 
#*****************************************************************************

rm(list = ls())

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)

## UPDATE EACH RUN ## 
# 1. Update "UploadDate" (this should match the folder name in synapse)
# 2. Set "site" variable to the site you are running the query for 
UploadDate = "2025-03-07"
site = "Ghana"

# 3. Set your main directory 
maindir <- paste0("~/PRiSMAv2Data/", site,"/", UploadDate, sep = "")
#*****************************************************************************
#* load data
#*****************************************************************************
## Load in long data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_long.Rdata", sep = "")) 

## Load in wide data 
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_wide.Rdata", sep = "")) 


## RUN THE FOLLOWING CODE TO ENSURE THE SITE HAS THE DATA REQUIRED TO RUN THE CODE. 
## if a site has all the forms needed for this query, the remainder of the code will run. if not, the code will stop
out_vars_needed_1 <- data_long %>% 
  filter(form %in% c("MNH13", "MNH20", "MNH24")) 

out_vars_needed_2 <- data_long %>% 
  ## select all visit date variables 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT", "INF_VITAL_MNH11", "INF_VITAL_MNH13",
                        "INF_VITAL_MNH14", "INF_VITAL_MNH15",  "INF_VITAL_MNH20")) 

if (exists("mnh04")== TRUE & exists("mnh09")== TRUE & exists("mnh11")== TRUE & 
    dim(out_vars_needed_1)[1]>0 &  dim(out_vars_needed_2)[1]>0 ){
  
  print("All forms required to run this query exist.")
  
} else {
  
  stop("SITE does not have all data required to run this query - don't run.")
  
}
#*****************************************************************************
## Types: 
# liveborn dead than alive 
# stillborn then alive
# fetal loss then alive 

## fetal death
#* miscarriage or induced abortion: reported on MNH04 (no IPC forms required) - likely won't have infantid 
#* stillbirth: document loss on one of the following - MNH04, MNH09, MNH19 + NEED to fillout IPC forms, MNH28 required, MNH24 closeout is optional
#* infant death: documentd on MNH13 plus MNH20 if hospitalized plus MNH24
#*****************************************************************************
#* PULL IDS OF INFANTS
#*****************************************************************************
# pull all infantids from mnh09
delivered_infantids <- mnh09 %>% 
  select(MOMID, PREGID, contains("INFANTID_INF")) %>% 
  pivot_longer(cols = c(-MOMID, -PREGID), 
               names_to = "var",
               values_to = "INFANTID") %>% 
  filter(!INFANTID %in% c("n/a", "0", "77", "1907-07-07") , 
         !is.na(INFANTID)) %>% 
  group_by(INFANTID) %>% 
  distinct() %>% 
  select(-var)

#*****************************************************************************
#* Run query for Infant dead than alive
#* Protocol in the event of infant death:
# must have MNH13 reported (INFANT_DTHDAT, INFANT_DTHTIM) 
# if hospitalized, reported on MNH20 (DTHDAT, DTHTIM)
# MNH24 closeout filled (DTHDAT, DTHTIM)

#* Query Logic:
# 1. Extract all liveborn infants 
# 2. Extract all infants with reported deaths in MNH13 OR MNH20 OR MNH24
# 3. Generate data subset will provide the date and vital status for each visit among infants with a reported date of death
# 4. Pull any visit dates that occur AFTER the reported date of death and vital status at the time of visit is "alive"
#*****************************************************************************
## extract infants with mnh13 (infant clinical status), mnh20 (infant hospitalizaiton), or mnh24 (closeout)
out_infants_dth_date <- data_long %>% 
  filter(form %in% c("MNH13", "MNH20", "MNH24")) %>% 
  filter(INFANTID %in% as.vector(delivered_infantids$INFANTID)) %>% 
  filter(varname %in% c("INFANT_DTHDAT", "DTHDAT")) %>% 
  mutate(DEATH_DATE =  ymd(parse_date_time(response, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  filter(!DEATH_DATE %in% c(ymd("2007-07-07"), ymd("1907-07-07"), ymd ("1905-05-05"))) %>% 
  rename("FORM_DTH_REPORTED" = "form") %>% 
  select(MOMID, PREGID, INFANTID, DEATH_DATE, FORM_DTH_REPORTED) %>% 
  distinct(MOMID, PREGID, INFANTID,.keep_all = TRUE)

# Extract all infant IDs reporting a death from the data 
# this data subset will provide the date and vital status for each visit among infants with a reported date of death (generated above) 
all_infid_data <- data_long %>% filter(INFANTID %in% as.vector(out_infants_dth_date$INFANTID)) %>% 
  ## select all visit date variables 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT", "INF_VITAL_MNH11", "INF_VITAL_MNH13",
                        "INF_VITAL_MNH14", "INF_VITAL_MNH15",  "INF_VITAL_MNH20")) %>% 
  mutate(VISIT_DATE =  ymd(parse_date_time(response, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  mutate(VisitDate =  ymd(parse_date_time(VisitDate, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  filter(!VISIT_DATE %in% c(ymd("2007-07-07"), ymd("1907-07-07"), ymd ("1905-05-05"))) %>% 
  # generate a single variable that combines all infant vital status variables across forms 
  # mutate(INF_VITAL = case_when(
  #   str_detect(varname, "INF_VITAL") ~ response, 
  #   TRUE ~ NA
  # )) %>% 
  mutate(INF_VITAL = ifelse(str_detect(varname, "INF_VITAL"), response, NA)) %>%
  rename("FORM" = "form") %>% 
  # fill the vital status for each infant at a specified visit 
  group_by(INFANTID, FORM, VisitDate) %>% 
  fill(INF_VITAL, .direction = "up") %>% 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT"))  %>% 
  select(-response, -varname, -VISIT_DATE)


# merge infants with a reported death and all visit data
if (dim(out_infants_dth_date)[1]>0 & dim(all_infid_data)[1]>0){
  ## merge 
  inf_dead_then_alive <- left_join(out_infants_dth_date, all_infid_data, by = c("MOMID", "PREGID", "INFANTID"))
  
}

## generate query column 
if (exists("inf_dead_then_alive")){
  # logic: if a visit date is AFTER the reported date of death, flag as a query
  inf_dead_then_alive_query <- inf_dead_then_alive %>% 
    rename("VISIT_DATE" = "VisitDate")  %>% 
    ## if the visit date is after the death date AND the infant vital status at that visit is "1, alive" then query
    mutate(VISIT_DATE_AFTER_DTH = case_when(VISIT_DATE >  DEATH_DATE & INF_VITAL == 1 ~ 1, 
                                            TRUE ~ 0)) %>% 
    ## extract the queries
    filter(VISIT_DATE_AFTER_DTH == 1) %>% 
    setcolorder(c("SCRNID", "MOMID", "PREGID", "INFANTID"))
}

#*****************************************************************************
## Formatting for the query report file
#*****************************************************************************
if (exists("inf_dead_then_alive_query")){
  
  ## QUESTION: should the mnh24 date of closeout be excluded because it is not an actual visit?
  
  # rename data frame 
  Infant_DeadThenAlive_query <- inf_dead_then_alive_query %>% 
    mutate(VisitType = TYPE_VISIT) %>% 
    relocate("VisitType", .before = "VISIT_DATE") %>% 
    select(-DEATH_DATE, -FORM_DTH_REPORTED,-TYPE_VISIT, -INF_VITAL, -VISIT_DATE_AFTER_DTH)
  
  # update naming
  names(Infant_DeadThenAlive_query) = c("ScrnID","MomID", "PregID","InfantID", "VisitType", "VisitDate", "Form")
  
  if (dim(Infant_DeadThenAlive_query)[1] > 0){
    
    ## add additional columns 
    Infant_DeadThenAlive_query = cbind(QueryID = NA, 
                                       UploadDate = UploadDate, 
                                       #MomID = "NA", PregID = "NA",
                                       #VisitDate = "NA",
                                       Infant_DeadThenAlive_query,
                                       `Variable Name` = NA, 
                                       `Variable Value` = NA,
                                       FieldType = "NA", 
                                       EditType = "Invalid visit following reported infant death", 
                                       DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    
    
    
    # combine form/edit type var 
    Infant_DeadThenAlive_query <- add_column(Infant_DeadThenAlive_query,Form_Edit_Type = paste(Infant_DeadThenAlive_query$Form,"_",Infant_DeadThenAlive_query$EditType))
    
    # assign queryid -- 
    # edit type id for Invalid Visit Following reported death is 11
    Infant_DeadThenAlive_query <- Infant_DeadThenAlive_query %>% 
      mutate(QueryID = paste0(Form, "_", VisitDate, "_",PregID, "_", InfantID,  "_", "11"))
    
    # Export data
    save(Infant_DeadThenAlive_query, file = paste0(maindir,"/queries/Infant_DeadThenAlive_query.rda"))
    
  }
}
#*****************************************************************************
## Generate "extra tab" to be included in the query report
#*****************************************************************************
## generate extra tab that will be include in the query report
if (exists("inf_dead_then_alive_query") & dim(Infant_DeadThenAlive_query)[1] >= 1){
  # logic: if a visit date is AFTER the reported date of death, flag as a query
  inf_dead_than_alive_extra_tab <- inf_dead_then_alive_query %>%
    rename("INF_VITAL_STATUS_VISIT" = "INF_VITAL"
    )  %>% 
    mutate(TYPE_LOSS = "Infant Death",
           BIRTH_OUTCOME = "Live birth",
           INF_VITAL_STATUS_VISIT = case_when(INF_VITAL_STATUS_VISIT ==1 ~ "Alive", 
                                              INF_VITAL_STATUS_VISIT ==2 ~ "Died",
                                              INF_VITAL_STATUS_VISIT ==99 ~ "Don't know",
                                              TRUE ~ NA),
           VISIT_DATE_AFTER_DTH = case_when(VISIT_DATE_AFTER_DTH ==1 ~ "Yes",
                                            VISIT_DATE_AFTER_DTH ==0 ~ "No",
                                            TRUE ~ NA),
           EDIT_TYPE = "Invalid visit following reported infant death") %>% 
    select(MOMID, PREGID, INFANTID, BIRTH_OUTCOME, TYPE_LOSS, DEATH_DATE, FORM_DTH_REPORTED, VISIT_DATE, FORM, INF_VITAL_STATUS_VISIT, VISIT_DATE_AFTER_DTH, EDIT_TYPE)
}

#*****************************************************************************
#* Stillbirth then alive
#* Protocol in the event of a stillbirth:
# Document loss on at least 1 of the forms 
#* MNH04 (FETAL_LOSS_DSSTDAT), MNH09 (DELIV_DSSTDAT_INF1-4, BIRTH_DSTERM_INF1-4), MNH19 (no fetal loss date on this form)
# IPC forms to be filled [MNH09 (DELIV_DSSTDAT_INF1-4, BIRTH_DSTERM_INF1-4), MNH11 (DTHDAT, DTHTIM)]
# MNH24 is optional (DTHDAT, DTHTIM)

# if hospitalized, reported on MNH20 (DTHDAT, DTHTIM)
# MNH24 closeout filled (DTHDAT, DTHTIM)

#* Query Logic:
# 1. Extract all reported stillbirths from MNH09 OR MNH11 OR MNH19
# 3. Generate data subset will provide the date and vital status for each visit among stillbirths with a reported date of death
# 4. Pull any visit dates that occur AFTER the reported date of death and vital status at the time of visit is "alive"
#*****************************************************************************
## need to convert MNH09 to long format to use the infant data within; infant variables are named with "_INFx" format - remove suffix and make long. Should have one row for each mom-baby pair 
if (exists("mnh09")==TRUE){
  
  ## first need to make m09 long format for each infant 
  m09_INF1 <- mnh09 %>% 
    rename("INFANTID" = "INFANTID_INF1") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF2"), -contains("_INF3"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF1')) %>% 
    mutate(INFANTID = as.character(INFANTID)) %>% 
    mutate_all(as.character)
  
  m09_INF2 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF2") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF3"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF2')) %>% 
    mutate(INFANTID = as.character(INFANTID))%>% 
    mutate_all(as.character)
  
  m09_INF3 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF3") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF2"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF3')) %>% 
    mutate(INFANTID = as.character(INFANTID))%>% 
    mutate_all(as.character)
  
  m09_INF4 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF4") %>%
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF2"), -contains("_INF3")) %>% 
    rename_with(~str_remove(., '_INF4')) %>% 
    mutate(INFANTID = as.character(INFANTID))%>% 
    mutate_all(as.character)
  
  ## bind all infants together
  mnh09_update <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) 
  
  ## remove INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4
  infantids_to_remove <- c("INFANTID_INF1", "INFANTID_INF2", "INFANTID_INF3", "INFANTID_INF4")
  mnh09_update <- mnh09_update %>% select(-any_of(infantids_to_remove))
  
  mnh09_long <- mnh09_update %>% bind_cols(SCRNID = NA) %>% 
    mutate(VisitDate = MAT_LD_OHOSTDAT) %>% 
    mutate(form = "MNH09",
           BIRTH_DSTERM = as.numeric(BIRTH_DSTERM)) %>% 
    select(MOMID, PREGID, INFANTID,MAT_LD_OHOSTDAT, DELIV_DSSTDAT, BIRTH_DSTERM) 
}

## extract infants with MNH04 (maternal clinical status), MNH09 (labor and delivery outcome), or MNH11 (infant delivery outcome)
out_stillbirth_dth_date <- mnh04 %>% 
  select("MOMID", "PREGID", "FETAL_LOSS_DSSTDAT", "PRG_DSDECOD", "FETAL_LOSS_DSDECOD") %>% 
  full_join(mnh09_long[c("MOMID", "PREGID", "INFANTID", "DELIV_DSSTDAT", "BIRTH_DSTERM")], by = c("MOMID", "PREGID")) %>% 
  full_join(mnh11[c("MOMID", "PREGID","INFANTID", "DTHDAT", "DTHTIM", "INF_DSTERM")], by = c("MOMID", "PREGID", "INFANTID")) %>%  
  ## convert date variables to date class
  mutate(DELIV_DSSTDAT =  ymd(parse_date_time(DELIV_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         DTHDAT =  ymd(parse_date_time(DTHDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))),
         FETAL_LOSS_DSSTDAT =  ymd(parse_date_time(FETAL_LOSS_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>%
  ## new variable if a loss was reported on MNH04 [PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3] or MNH09 [BIRTH_DSTERM== 2]
  mutate(LOSS_REPORTED = case_when(BIRTH_DSTERM== 2 | (PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3) ~ 1,
                                   TRUE ~ 0)) %>% 
  # filter for participants who have had a loss reported 
  filter(LOSS_REPORTED == 1) %>% 
  # replace any default value dates with NA
  mutate(DELIV_DSSTDAT = replace(DELIV_DSSTDAT, DELIV_DSSTDAT==ymd("1907-07-07"), NA),
         DTHDAT = replace(DTHDAT, DTHDAT==ymd("1907-07-07"), NA),
         FETAL_LOSS_DSSTDAT = replace(FETAL_LOSS_DSSTDAT, FETAL_LOSS_DSSTDAT==ymd("1907-07-07"), NA)) %>% 
  # generate new variable with date fetal loss was reported (either MNH09 or MNH04)
  mutate(DATE_LOSS_REPORTED = case_when(BIRTH_DSTERM == 2 & !is.na(DELIV_DSSTDAT) ~ DELIV_DSSTDAT, 
                                        (PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3) ~ FETAL_LOSS_DSSTDAT,
                                        TRUE ~ NA
  )) %>% 
  # generate new variable with what form fetal loss was reported on (either MNH09 or MNH04)
  mutate(FORM_DTH_REPORTED = case_when(BIRTH_DSTERM == 2 ~ "MNH09",
                                       (PRG_DSDECOD==2 & FETAL_LOSS_DSDECOD == 3) ~  "MNH04",
                                       TRUE ~ NA)) %>% 
  select(MOMID, PREGID, INFANTID, DATE_LOSS_REPORTED, FORM_DTH_REPORTED,BIRTH_DSTERM,INF_DSTERM) %>% 
  distinct(MOMID, PREGID, INFANTID, .keep_all = TRUE)


# Extract all infant IDs reporting a death from the data 
# this data subset will provide the date and vital status for each visit among infants with a reported date of death (generated above) 

all_infid_data <- data_long %>% filter(INFANTID %in% as.vector(out_stillbirth_dth_date$INFANTID)) %>% 
  ## select all visit date variables 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT", "INF_VITAL_MNH11", "INF_VITAL_MNH13",
                        "INF_VITAL_MNH14", "INF_VITAL_MNH15",  "INF_VITAL_MNH20")) %>% 
  # update date class for date variables
  mutate(VISIT_DATE =  ymd(parse_date_time(response, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  mutate(VisitDate =  ymd(parse_date_time(VisitDate, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  # generate a single variable that combines all infant vital status variables across forms 
  mutate(INF_VITAL = case_when(
    str_detect(varname, "INF_VITAL") ~ response, 
    TRUE ~ NA
  )) %>% 
  rename("FORM" = "form") %>% 
  group_by(INFANTID, FORM, VisitDate) %>% 
  # fill the vital status for each infant at a specified visit 
  fill(INF_VITAL, .direction = "up") %>% 
  filter(varname %in% c("VISIT_OBSSTDAT", "OBSSTDAT"))  %>% 
  select(-response, -varname, -VISIT_DATE)


# merge reported stillbirths with all visit data
if (dim(out_stillbirth_dth_date)[1]>0 & dim(all_infid_data)[1]>0){
  stillbirth_then_alive <- full_join(out_stillbirth_dth_date, all_infid_data, by = c("MOMID", "PREGID", "INFANTID"))
} 


## generate query column
if (exists("stillbirth_then_alive")){
  stillbirth_then_alive_query <- stillbirth_then_alive %>% 
    rename("VISIT_DATE" = "VisitDate")  %>% 
    ## if the visit date is after the death date AND the infant vital status at that visit is "1, alive" then query
    mutate(DATE_AFTER_DTH = case_when(VISIT_DATE >  DATE_LOSS_REPORTED & INF_VITAL == 1 ~ 1, 
                                      TRUE ~ 0)) %>% 
    ## filter and format
    filter(DATE_AFTER_DTH == 1) %>% 
    setcolorder(c("SCRNID", "MOMID", "PREGID", "INFANTID"))
}

# Question: the logic is if the visit date is AFTER the death date AND the vital status at the time of visit is alive, then we are going to query 
# if the visit date is after the death date AND the vital status at the time of visit is dead then that is not a query -- is this correct?
#*****************************************************************************
## Formatting for the query report file
#*****************************************************************************
if (exists("stillbirth_then_alive_query")){
  
  # rename data frame 
  StillbirthThenAlive_query <- stillbirth_then_alive_query %>% 
    rename(VisitType = TYPE_VISIT) %>% 
    relocate("VisitType", .before = "VISIT_DATE") %>% 
    select(-DATE_LOSS_REPORTED, -FORM_DTH_REPORTED, -BIRTH_DSTERM,-INF_DSTERM, -INF_VITAL, -DATE_AFTER_DTH)
  
  # update naming
  names(StillbirthThenAlive_query) = c("ScrnID","MomID", "PregID","InfantID", "VisitType", "VisitDate", "Form")
  
  if (dim(StillbirthThenAlive_query)[1] > 0){
    
    ## add additional columns 
    StillbirthThenAlive_query = cbind(QueryID = NA, 
                                      UploadDate = UploadDate, 
                                      #MomID = "NA", PregID = "NA",
                                      #VisitDate = "NA", 
                                      StillbirthThenAlive_query, 
                                      `Variable Name` = NA, 
                                      `Variable Value` = NA,
                                      FieldType = "NA", 
                                      EditType = "Invalid visit following reported stillbirth", 
                                      DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    
    
    
    # combine form/edit type var 
    StillbirthThenAlive_query <- add_column(StillbirthThenAlive_query,Form_Edit_Type = paste(StillbirthThenAlive_query$Form,"_",StillbirthThenAlive_query$EditType))
    
    ## assign queryid -- 
    # edit type id for Invalid Visit Following reported death is 11
    StillbirthThenAlive_query <- StillbirthThenAlive_query %>% 
      mutate(QueryID = paste0(Form, "_", VisitDate, "_",PregID, "_", InfantID,  "_", "11"))
    
    # Export data
    save(StillbirthThenAlive_query, file = paste0(maindir,"/queries/StillbirthThenAlive_query.rda"))
    
  }
}

#*****************************************************************************
## Generate "extra tab" to be included in the query report
#*****************************************************************************

## generate extra tab that will be include in the query report
if (dim(stillbirth_then_alive_query)[1]>0){
  # logic: if a visit date is AFTER the reported date of death, flag as a query
  stillbirth_than_alive_extra_tab <- stillbirth_then_alive_query %>%
    rename("INF_VITAL_STATUS_VISIT" = "INF_VITAL",
           "VISIT_DATE_AFTER_DTH" = "DATE_AFTER_DTH",
           "DEATH_DATE" = "DATE_LOSS_REPORTED"
    )  %>% 
    mutate(TYPE_LOSS = "Stillbirth",
           BIRTH_OUTCOME = case_when(BIRTH_DSTERM ==1 ~ "Live birth",
                                     BIRTH_DSTERM ==2 ~ "Fetal loss",
                                     TRUE ~ NA),
           INF_VITAL_STATUS_VISIT = case_when(INF_VITAL_STATUS_VISIT ==1 ~ "Alive", 
                                              INF_VITAL_STATUS_VISIT ==2 ~ "Died",
                                              INF_VITAL_STATUS_VISIT ==99 ~ "Don't know",
                                              TRUE ~ NA),
           VISIT_DATE_AFTER_DTH = case_when(VISIT_DATE_AFTER_DTH ==1 ~ "Yes",
                                            VISIT_DATE_AFTER_DTH ==0 ~ "No",
                                            TRUE ~ NA),
           EDIT_TYPE = "Invalid visit following reported stillbirth") %>% 
    select(MOMID, PREGID, INFANTID, BIRTH_OUTCOME, TYPE_LOSS, DEATH_DATE, FORM_DTH_REPORTED, VISIT_DATE, FORM, INF_VITAL_STATUS_VISIT, VISIT_DATE_AFTER_DTH, EDIT_TYPE)
}

#*****************************************************************************
## Bind & export infant dead then alive and stillbirth then alive "extra tabs" 
#*****************************************************************************

if (exists("inf_dead_than_alive_extra_tab")== TRUE & exists("stillbirth_than_alive_extra_tab")== TRUE){
  
  # bind infant and stillbirth queries
  inf_dead_than_alive_extra_tab <- bind_rows(inf_dead_than_alive_extra_tab, stillbirth_than_alive_extra_tab) %>% 
    mutate(QueryID = paste0(FORM, "_", VISIT_DATE, "_",PREGID, "_", INFANTID,  "_", "11")) %>% 
    relocate(QueryID, .before = MOMID)
  
  # Export data
  save(inf_dead_than_alive_extra_tab, file = paste0(maindir,"/queries/inf_dead_than_alive_extra_tab.rda"))
  
} else if (exists("inf_dead_than_alive_extra_tab")== TRUE & exists("stillbirth_than_alive_extra_tab")== FALSE) {
  
  inf_dead_than_alive_extra_tab <- inf_dead_than_alive_extra_tab %>% 
    mutate(QueryID = paste0(FORM, "_", VISIT_DATE, "_",PREGID, "_", INFANTID,  "_", "11")) %>% 
    relocate(QueryID, .before = MOMID)
  
  # Export data
  save(inf_dead_than_alive_extra_tab, file = paste0(maindir,"/queries/inf_dead_than_alive_extra_tab.rda"))
  
} else if (exists("inf_dead_than_alive_extra_tab")== FALSE & exists("stillbirth_than_alive_extra_tab")== TRUE) {
  
  inf_dead_than_alive_extra_tab <- stillbirth_than_alive_extra_tab %>% 
    mutate(QueryID = paste0(FORM, "_", VISIT_DATE, "_",PREGID, "_", INFANTID,  "_", "11")) %>% 
    relocate(QueryID, .before = MOMID)
  
  # Export data
  save(inf_dead_than_alive_extra_tab, file = paste0(maindir,"/queries/inf_dead_than_alive_extra_tab.rda"))
  
}


#*****************************************************************************
#MNH23 Query: 
#1. To check if there is an outcome before CLOSE_DSDECOD %in% c(1,2) is being completed
#2a. To check if CLOSE_DSDECOD == 1 is being done then diff between CLOSEOUT date and Outcome date is greater than 365 days
#2b. To check if CLOSE_DSDECOD == 2 is being done then diff between CLOSEOUT date and Outcome date is greater than 42 days
#*****************************************************************************

mnh04$ANC_OBSSTDAT <- ymd(parse_date_time(mnh04$ANC_OBSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh04$FETAL_LOSS_DSSTDAT <- ymd(parse_date_time(mnh04$FETAL_LOSS_DSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

mnh04$ANC_OBSSTDAT <- case_when (mnh04$ANC_OBSSTDAT %in% c(ymd("2007-07-07"), ymd("1907-07-07"), ymd("1905-05-05")) ~ NA, TRUE ~ mnh04$ANC_OBSSTDAT) 
mnh04$FETAL_LOSS_DSSTDAT <- case_when (mnh04$FETAL_LOSS_DSSTDAT %in% c( ymd("2007-07-07"), ymd("1907-07-07"), ymd("1905-05-05"))~ NA, TRUE ~ mnh04$FETAL_LOSS_DSSTDAT)

miscarriage <- mnh04 %>%
  filter(FETAL_LOSS_DSDECOD %in% c(1, 2, 3)) %>% 
  mutate(FETALLOSS_DATE = case_when (!is.na(FETAL_LOSS_DSSTDAT) ~ FETAL_LOSS_DSSTDAT, TRUE ~ ANC_OBSSTDAT)) %>%  
  select(MOMID, PREGID, FETALLOSS_DATE, FETAL_LOSS_DSSTDAT, ANC_OBSSTDAT, FETAL_LOSS_DSDECOD, FETALLOSS_DATE) %>%
  group_by(MOMID, PREGID) %>%
  slice_min(order_by = FETALLOSS_DATE, with_ties = FALSE) %>%
  ungroup()

mnh09_sub <- mnh09 %>%
  select(MOMID, PREGID, MAT_VISIT_MNH09, INFANTS_FAORRES, MAT_LD_OHOSTDAT,
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
  mutate(MAT_DELIVERY_DATE = pmin(DELIV_DSSTDAT_INF1, DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4, na.rm = TRUE),
         DELIVERED = 1) %>%
  group_by(MOMID, PREGID) %>%
  slice_min(order_by = MAT_DELIVERY_DATE, with_ties = FALSE) %>%
  ungroup() %>% 
  select (MOMID, PREGID, DELIVERED, MAT_LD_OHOSTDAT, MAT_DELIVERY_DATE, DELIVERED ) 

mnh23$CLOSE_DSSTDAT <- ymd(parse_date_time(mnh23$CLOSE_DSSTDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))
mnh23$DTHDAT <- ymd(parse_date_time(mnh23$DTHDAT, c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))

mom_closeout <- mnh23 %>%
  left_join(mnh09_sub, by = c("MOMID", "PREGID")) %>% 
  left_join(miscarriage, by = c("MOMID", "PREGID")) %>% 
  mutate(
    CLOSE_DDAT = case_when(CLOSE_DSSTDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07"), ymd("1905-05-05")) ~ NA, TRUE ~ CLOSE_DSSTDAT),
    DTHDAT = case_when (DTHDAT %in% c(ymd("1907-07-07"), ymd("2007-07-07"), ymd("1905-05-05")) ~ NA, TRUE ~ DTHDAT),
    LOSS_DIFF = as.numeric(difftime(CLOSE_DDAT, FETAL_LOSS_DSSTDAT, units = "days")),
    DELIV_DIFF = as.numeric(difftime(CLOSE_DDAT, MAT_DELIVERY_DATE, units = "days")))  %>% 
  
  mutate (Query_1 = case_when (CLOSE_DSSTDAT %in% c(ymd("1907-07-07"), ymd("1905-05-05"), ymd("2007-07-07")) | 
                                 is.na (CLOSE_DSSTDAT) ~ "PregID Missing Closeout Date", TRUE ~ "No Query"),
          
          Query_2 = case_when (FETAL_LOSS_DSSTDAT %in% c(ymd("1907-07-07"), ymd("1905-05-05"), ymd("2007-07-07")) | 
                                 is.na (FETAL_LOSS_DSSTDAT) & FETAL_LOSS_DSDECOD %in% c(1,2) ~ 
                                 "PregID Missing Fetal Loss Date", TRUE ~ "No Query"),
          
          Query_3 = case_when (CLOSE_DSDECOD == 1 & is.na (FETAL_LOSS_DSDECOD) & is.na (DELIVERED) ~ 
                                 "1-year Followup Missing MNH04/09 Form",
                               CLOSE_DSDECOD == 2 & is.na (FETAL_LOSS_DSDECOD) & is.na (DELIVERED) ~ 
                                 "42days Followup Missing MNH04/09 Form",TRUE ~ "No Query"),
          
          Query_4 = case_when (CLOSE_DSDECOD == 1 & !is.na (DELIVERED) & DELIV_DIFF < 365 ~ 
                                 "1yr Followup: Closeout Too Soon",
                               CLOSE_DSDECOD == 2 & FETAL_LOSS_DSDECOD %in% c(1,2) & LOSS_DIFF < 42 ~ 
                                 "42days Followup: Closeout Too Soon",TRUE ~ "No Query"))


mom_closeout_long <- mom_closeout %>% 
  pivot_longer(cols = starts_with("Query_"), names_to = "Query_Type", values_to = "Query") %>%
  filter(!(Query == "No Query")) %>% 
  mutate(
    Form = ifelse(Query_Type == "Query_2","MNH04","MNH23"),  # Form name
    `Variable Name` = case_when(
      Query == "PregID Missing Closeout Date" ~ "CLOSE_DSSTDAT",
      Query == "PregID Missing Fetal Loss Date" ~ "FETAL_LOSS_DSSTDAT",
      Query == "PregID Missing Death Date" ~ "DTHDAT",
      Query %in% c("1-year Followup Missing MNH04/09 Form", 
                   "42days Followup Missing MNH04/09 Form") ~ " ",
      Query %in% c("1yr Followup: Closeout Too Soon", 
                   "42days Followup: Closeout Too Soon") ~ "CLOSE_DSDECOD",
      TRUE ~ NA_character_),
    
    `Variable Value` = case_when(
      Query == "PregID Missing Closeout Date" ~ as.character(CLOSE_DSSTDAT),
      Query == "PregID Missing Fetal Loss Date" ~ as.character(FETAL_LOSS_DSSTDAT),
      Query == "PregID Missing Death Date" ~ as.character(DTHDAT),
      Query %in% c("1-year Followup Missing MNH04/09 Form", 
                   "42days Followup Missing MNH04/09 Form") ~ " ",
      Query %in% c( "1yr Followup: Closeout Too Soon", 
                    "42days Followup: Closeout Too Soon") ~ as.character(CLOSE_DSDECOD),
      TRUE ~ NA_character_),
    
    VisitDate = CLOSE_DSSTDAT,  # Use Closeout Date as Visit Date
    EditType = Query,
    SCRNID = NA_character_, 
    TYPE_VISIT = NA_character_,
    INFANTID = NA_character_) %>%
  select( SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VisitDate, 
          `Variable Name`, `Variable Value`, EditType, Form ) 

# Append additional columns and format
if (nrow(mom_closeout_long) > 0) {
  
  MomIDCloseout_query <- mom_closeout_long %>%
    mutate(
      QueryID = paste0("MNH23_", "_", PREGID, "_", VisitDate),
      UploadDate = format(Sys.time(), "%Y-%m-%d"),
      DateEditReported = format(Sys.time(), "%Y-%m-%d"),
      Form_Edit_Type = paste(Form, "_", EditType)) %>%
    select (QueryID, ScrnID = SCRNID, MomID = MOMID, PregID = PREGID, 
            InfantID = INFANTID, VisitType = TYPE_VISIT, 
            VisitDate, EditType, Form, `Variable Name`, 
            `Variable Value`, UploadDate, DateEditReported, Form_Edit_Type) %>%
    
    mutate_all (as.character())
  
  # Save the query results to an RDA file
  save(MomIDCloseout_query, file = paste0(maindir, "/queries/MomIDCloseout_query.rda"))
  
} else { print("No MNH23 queries found!") }


#*****************************************************************************
#MNH37 Query: 
#1. To check if there is correct COD Identified
#2. To confirm if 
#*****************************************************************************
cod_df_1 <- mnh37 %>%
  { if (!"VA_TYPE" %in% names(.)) mutate(., VA_TYPE = NA) else . } %>%
  mutate(
    FINAL_MAT_CS = as.numeric(FINAL_MAT_CS),
    FINAL_INF_CS = as.numeric(FINAL_INF_CS)
  ) %>%
  mutate(
    Query_1 = case_when(
      (VA_TYPE == 1 | VA_COMPL_FORM %in% c(1)) & FINAL_MAT_CS %in% c(55, 77, NA) ~ "PregID MNH37 Missing COD",
      TRUE ~ "No Query"
    ),
    Query_2 = case_when(
      (VA_TYPE == 2 | VA_COMPL_FORM %in% c(2, 3)) & FINAL_INF_CS %in% c(55, 77, NA) ~ "InfantID MNH37 Missing COD",
      TRUE ~ "No Query"
    ),
    Query_3 = case_when(
      (VA_TYPE == 2 | VA_COMPL_FORM %in% c(2, 3)) & FINAL_INF_CS == 18 &
        FINAL_INF_OTHR_SPFY_CS %in% c("N/A", "n/a", "na", "NA", "") ~ "Specify Other Inf COD",
      TRUE ~ "No Query"
    ),
    Query_4 = case_when(
      (VA_TYPE == 1 | VA_COMPL_FORM == 1) & FINAL_MAT_CS == 17 &
        FINAL_MAT_OTHR_SPFY_CS %in% c("N/A", "n/a", "na", "NA", "") ~ "Specify Other Mat COD",
      TRUE ~ "No Query"
    )
  )
# List of valid COD_TEXT patterns
valid_cod_mat <- c(
  "Road traffic accident",
  "Obstetric haemorrhage",
  "Pregnancy-induced hypertension|Pregnancy induced HTN",
  "Pregnancy-related sepsis|Pregnancy related sepsis",
  "ARI, including pneumonia|ARI including pneumonia",
  "HIV/AIDS related death|HIV/AIDS death",
  "Reproductive neoplasms MMF|Reproductive neoplasms",
  "Pulmonary TB|Pulmonary tuberculosis",
  "Malaria",
  "Meningitis",
  "Diarrheal diseases|Diarrheal dis",
  "Abortion-related death|Abortion related death",
  "Ectopic pregnancy",
  "Other and unspecified cardiac diseases|Other cardiac dis",
  "Other infectious diseases|Other infect dis",
  "Other NCD",
  "Indeterminate")

valid_inf_cod <- c(
  "Fresh stillbirth",
  "Macerated stillbirth",
  "Prematurity",
  "Birth asphyxia",
  "Tetanus",
  "Congenital malformation",
  "Diarrheal diseases|Diarrhoeal diseases",
  "Acute respiratory infection including pneumonia",
  "Meningitis and encephalitis",
  "Neonatal pneumonia",
  "Neonatal sepsis",
  "Road traffic accident",
  "Other and unspecified infectious diseases|Other and unspecified infect dis",
  "Other and unspecified cardiac diseases|Other and unspecified cardiac dis",
  "Severe malnutrition",
  "Renal failure",
  "Indeterminate"
)

# Combine patterns into a single regex
mat_cod_regex <- paste(valid_cod_mat, collapse = "|")
inf_cod_regex <- paste(valid_inf_cod, collapse = "|")


# Filter rows where COD == 17 and COD_TEXT matches any valid COD pattern
cod_df_1 <- cod_df_1 %>%
  mutate(
    Query_5 = case_when(
      FINAL_MAT_CS == 17 & str_detect(FINAL_MAT_OTHR_SPFY_CS, regex(mat_cod_regex, ignore_case = TRUE)) ~
        "Review Mat Death Coded As 'Other' (17)",
      TRUE ~ "No Query"
    ),
    Query_6 = case_when(
      FINAL_INF_CS == 18 & str_detect(FINAL_INF_OTHR_SPFY_CS, regex(inf_cod_regex, ignore_case = TRUE)) ~
        "Review Inf Death Coded As 'Other' (18)",
      TRUE ~ "No Query"
    )
  )

cod_df_long <- cod_df_1 %>% 
  pivot_longer(cols = starts_with("Query_"), names_to = "Query_Type", values_to = "Query") %>%
  filter(!(Query == "No Query")) %>% 
  mutate(
    Form = "MNH37",  # Form name
    `Variable Name` = case_when(
      Query_Type == "Query_1" ~ "FINAL_MAT_CS",
      Query_Type == "Query_2" ~ "FINAL_INF_CS ",
      Query_Type == "Query_3" ~ "FINAL_INF_OTHR_SPFY_CS",
      Query_Type == "Query_4" ~ "FINAL_MAT_OTHR_SPFY_CS",
      Query_Type == "Query_5" ~ "FINAL_MAT_OTHR_SPFY_CS",
      Query_Type == "Query_6" ~ "FINAL_INF_OTHR_SPFY_CS",
      TRUE ~ NA_character_),
    
    `Variable Value` = case_when(
      Query_Type == "Query_1" ~ as.character (FINAL_MAT_CS),
      Query_Type == "Query_2" ~ as.character (FINAL_INF_CS),
      Query_Type == "Query_3" ~ as.character (FINAL_INF_OTHR_SPFY_CS),
      Query_Type == "Query_4" ~ as.character (FINAL_MAT_OTHR_SPFY_CS),
      Query_Type == "Query_5" ~ as.character (FINAL_MAT_OTHR_SPFY_CS),
      Query_Type == "Query_6" ~ as.character (FINAL_INF_OTHR_SPFY_CS),
      TRUE ~ NA_character_),
    
    VisitDate = VA_FORM_DAT,  # Use Closeout Date as Visit Date
    EditType = Query,
    SCRNID = NA_character_, 
    TYPE_VISIT = NA_character_) %>%
  select( SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VisitDate, 
          `Variable Name`, `Variable Value`, EditType, Form, Query_Type ) 

# Append additional columns and format
if (nrow(cod_df_long) > 0) {
  
  CauseOfDeath_query <- cod_df_long %>%
    mutate(
      QueryID = case_when(
        Query_Type %in% c("Query_2", "Query_3", "Query_6") ~ 
          paste0("MNH37_", INFANTID, "_", VisitDate),
        TRUE ~ 
          paste0("MNH37_", PREGID, "_", VisitDate)
      ),
      UploadDate = format(Sys.time(), "%Y-%m-%d"),
      DateEditReported = format(Sys.time(), "%Y-%m-%d"),
      Form_Edit_Type = paste0(Form, "_", EditType)
    ) %>%
    select(
      QueryID,
      ScrnID = SCRNID,
      MomID = MOMID,
      PregID = PREGID,
      InfantID = INFANTID,
      VisitType = TYPE_VISIT,
      VisitDate,
      EditType,
      Form,
      `Variable Name`,
      `Variable Value`,
      UploadDate,
      DateEditReported,
      Form_Edit_Type
    ) %>%
    mutate(across(everything(), as.character))  # replaces mutate_all
  
  # Save the query results to an RDA file
  save(CauseOfDeath_query, file = paste0(maindir, "/queries/CauseOfDeath_query.rda"))
  
} else {
  print("No MNH37 queries found!")
}


#*****************************************************************************
#*Missing Death Dates Query
#*****************************************************************************

form_names <- c("mnh01", "mnh04", "mnh09", "mnh10", "mnh11", 
                "mnh19", "mnh21", "mnh23", "mnh37")  # Add all forms here

# Define dynamic conditions for each dataset (modify as needed)
conditions_list <- list(
  mnh04 = list(
    list(condition_var = "MAT_VISIT_MNH04", condition_value = 8, death_var = "DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH04", condition_value = 2, death_var = "DTHDAT")),
  
  mnh09 = list(
    list(condition_var = "MAT_VISIT_MNH09", condition_value = 8, death_var = "MAT_DEATH_DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH09", condition_value = 2, death_var = "MAT_DEATH_DTHDAT")),
  
  
  mnh10 = list(
    list(condition_var = "MAT_VISIT_MNH10", condition_value = 8, death_var = "MAT_DEATH_DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH10", condition_value = 2, death_var = "MAT_DEATH_DTHDAT")),
  
  mnh12 = list(
    list(condition_var = "MAT_VISIT_MNH12", condition_value = 8, death_var = "MAT_DEATH_DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH12", condition_value = 2, death_var = "MAT_DEATH_DTHDAT")),
  
  mnh19 = list(
    list(condition_var = "VISIT_FAORRES", condition_value = 5, death_var = "DTHDAT"),
    list(condition_var = "MAT_ARRIVAL_DSDECOD", condition_value = 2, death_var = "DTHDAT"),
    list(condition_var = "ADMIT_DSTERM", condition_value = 3, death_var = "DTHDAT")),
  
  mnh20 = list(
    list(condition_var = "ADMIT_DSTERM", condition_value = 3, death_var = "DTHDAT")),
  
  mnh21 = list(
    list(condition_var = "AETERM", condition_value = 1, death_var = "DTHDAT")),
  
  mnh23 = list(
    list(condition_var = "CLOSE_DSDECOD", condition_value = 3, death_var = "DTHDAT")),
  
  mnh24 = list(
    list(condition_var = "CLOSE_DSDECOD", condition_value = 3, death_var = "DTHDAT")),
  
  mnh37 = list(
    list(condition_var = "VA_TYPE", condition_value = 1, death_var = "FINAL_MAT_DAT"),
    list(condition_var = "VA_TYPE", condition_value = 2, death_var = "FINAL_INF_DAT"))
)


# Define invalid death dates
invalid_death_dates <- c(ymd("1907-07-07"), ymd("1905-05-05"), ymd("2007-07-07"))

# Initialize an empty dataframe for queries
query_results <- data.frame()

# Loop through each dataset
for (form_name in form_names) {
  # Skip if dataset does not exist
  if (!exists(form_name, where = .GlobalEnv, inherits = FALSE)) {
    message(paste("Skipping", form_name, "- dataset does not exist."))
    next
  }
  
  form_data <- get(form_name, envir = .GlobalEnv)
  
  # Skip if dataset is NULL
  if (is.null(form_data)) {
    message(paste("Skipping", form_name, "- dataset is NULL."))
    next
  }
  
  # Skip if form is not in the condition list
  if (!form_name %in% names(conditions_list)) next
  
  # Find the correct Visit Date column
  possible_visit_vars <- c("VISIT_OBSSTDAT", "MAT_LD_OHOSTDAT", "ANC_OBSSTDAT",
                           "OBSSTDAT", "AESTDAT", "CLOSE_DSSTDAT", "VA_FORM_DAT")
  visit_var <- possible_visit_vars[possible_visit_vars %in% names(form_data)]
  
  # If no valid Visit Date variable found, create a placeholder
  if (length(visit_var) == 0) {
    visit_var <- "VISITDATE"
    form_data[[visit_var]] <- NA  # Assign NA if missing
  } else {
    visit_var <- visit_var[1]  # Use the first matching column
  }
  
  # Process each condition for the form
  for (condition in conditions_list[[form_name]]) {
    
    condition_var <- condition$condition_var
    condition_value <- condition$condition_value
    death_var <- condition$death_var
    
    # Ensure required columns exist; if missing, create them as NA
    required_columns <- c("SCRNID", "MOMID", "PREGID", "INFANTID", "TYPE_VISIT", condition_var, death_var, visit_var)
    
    for (col in required_columns) {
      if (!(col %in% names(form_data))) {
        form_data[[col]] <- NA
      }
    }
    
    # Skip if death variable is missing entirely
    if (all(is.na(form_data[[death_var]]))) {
      message(paste("Skipping", form_name, "- death variable missing."))
      next
    }
    
    # Convert death date variable to proper date format
    form_data <- form_data %>%
      mutate(
        !!sym(death_var) := suppressWarnings(
          ymd(parse_date_time(.data[[death_var]], orders = c("d/m/Y", "d-m-Y", "Y-m-d", "d-b-y")))
        )
      )
    
    # Filter for condition met & invalid/missing death date
    query_data <- form_data %>% 
      filter(.data[[condition_var]] == condition_value & 
               (.data[[death_var]] %in% invalid_death_dates | is.na(.data[[death_var]]))) %>%
      mutate(Query = case_when (form_name %in% c("mnh20", "mnh24") | death_var == "FINAL_INF_DAT" ~ paste0("InfantID in ", toupper(form_name), " Missing Death Date"),
                                TRUE ~ paste0("PregID in ", toupper(form_name), " Missing Death Date")),
             Form = toupper(form_name),
             `Variable Name` = death_var,
             `Variable Value` = .data[[death_var]], 
             VisitDate = .data[[visit_var]]) %>%
      select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VisitDate, `Variable Name`, `Variable Value`, Query, Form)
    
    # Append results
    if (nrow(query_data) > 0) {
      query_results <- bind_rows(query_results, query_data)
    } else {
      print(paste0("No ", form_name, " death date query for ", condition_var))
    }
  }
}

# Print or save results
if (nrow(query_results) > 0) {
  
  MissingDeathDate_query <- query_results %>% 
    select (ScrnID = SCRNID, MomID = MOMID, PregID = PREGID, InfantID = INFANTID, VisitType = TYPE_VISIT, 
            VisitDate, EditType = Query, Form, `Variable Name`, `Variable Value`) 
  
  ## add additional columns 
  MissingDeathDate_query = cbind(QueryID = NA, 
                                 UploadDate = UploadDate, 
                                 MissingDeathDate_query, 
                                 DateEditReported = format(Sys.time(), "%Y-%m-%d")) 
  
  
  # combine form/edit type var 
  MissingDeathDate_query <- add_column(MissingDeathDate_query,Form_Edit_Type = paste(MissingDeathDate_query$Form, "_", MissingDeathDate_query$EditType))
  
  
  ## assign queryid 
  MissingDeathDate_query <- MissingDeathDate_query %>% 
    mutate(QueryID = if_else (`Variable Name` %in% c("FINAL_INF_DAT") | Form %in% c("MNH20", "MNH24"),
                              paste0("Death_Date", "_", InfantID, "_",  VisitDate, "_", `Variable Value`, "_", "04"),
                              paste0("Death_Date", "_", PregID, "_",  VisitDate, "_", `Variable Value`, "_", "04"))) %>% 
    mutate_all(as.character())
  
  #export Mom ID not matched query 
  save(MissingDeathDate_query, file = paste0(maindir,"/queries/MissingDeathDate_query.rda"))
  
} else { print("No queries found!") }



#*****************************************************************************
#* Run query for Mom dead than alive

#* Query Logic:
# 1. Extract all instances of death recorded and take in the earliest death date 
# 2a. Then loop through all maternal forms get where Vital of Mom is Alive
# 2b. In the cases where Mom are identified as alive, compare to see if the 
#     visit date is after the earliest death date 
# 3. Pull any visit dates that occur AFTER the reported date of death and 
#    vital status at the time of visit is "alive"
#*****************************************************************************
# Define the form names
mat_form_names <- c("mnh04", "mnh09", "mnh10", "mnh12", 
                    "mnh19", "mnh21", "mnh23", "mnh37")

# Define dynamic conditions for where mom would be recorded as death in each dataset
mat_conditions_list <- list(
  mnh04 = list(
    list(condition_var = "MAT_VISIT_MNH04", condition_value = 8, death_var = "DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH04", condition_value = 2, death_var = "DTHDAT")),
  
  mnh09 = list(
    list(condition_var = "MAT_VISIT_MNH09", condition_value = 8, death_var = "MAT_DEATH_DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH09", condition_value = 2, death_var = "MAT_DEATH_DTHDAT")),
  
  mnh10 = list(
    list(condition_var = "MAT_VISIT_MNH10", condition_value = 8, death_var = "MAT_DEATH_DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH10", condition_value = 2, death_var = "MAT_DEATH_DTHDAT")),
  
  mnh12 = list(
    list(condition_var = "MAT_VISIT_MNH12", condition_value = 8, death_var = "MAT_DEATH_DTHDAT"),
    list(condition_var = "MAT_VITAL_MNH12", condition_value = 2, death_var = "MAT_DEATH_DTHDAT")),
  
  mnh19 = list(
    list(condition_var = "VISIT_FAORRES", condition_value = 5, death_var = "DTHDAT"),
    list(condition_var = "MAT_ARRIVAL_DSDECOD", condition_value = 2, death_var = "DTHDAT"),
    list(condition_var = "ADMIT_DSTERM", condition_value = 3, death_var = "DTHDAT")),
  
  mnh21 = list(
    list(condition_var = "AETERM", condition_value = 1, death_var = "DTHDAT")),
  
  mnh23 = list(
    list(condition_var = "CLOSE_DSDECOD", condition_value = 3, death_var = "DTHDAT")),
  
  mnh37 = list(
    list(condition_var = "VA_TYPE", condition_value = 1, death_var = "FINAL_MAT_DAT"))
)

# Define invalid death dates
invalid_death_dates <- c(ymd("1907-07-07"), ymd("1905-05-05"), ymd("2007-07-07"))

# Initialize a list to collect death dates
all_deaths <- data.frame()

# Iterate through each form and its conditions
for (form_name in mat_form_names) {
  # Skip if the dataset does not exist
  if (!exists(form_name, where = .GlobalEnv, inherits = FALSE)) {
    message(paste("Skipping", form_name, "- dataset does not exist."))
    next
  }
  
  # Get the dataset
  form_data <- get(form_name)  # assuming each form dataset is loaded with the same name
  
  # Skip if dataset is NULL
  if (is.null(form_data)) {
    message(paste("Skipping", form_name, "- dataset is NULL."))
    next
  }
  
  # Get the condition list for this form
  conditions <- mat_conditions_list[[form_name]]
  
  # Skip if form is not in the condition list
  if (!form_name %in% names(mat_conditions_list)) next
  
  for (cond in conditions) {
    cond_var <- cond$condition_var
    cond_value <- cond$condition_value
    death_var <- cond$death_var
    
    # Check if required columns exist in the dataset
    if (all(c(cond_var, death_var, "MOMID", "PREGID") %in% names(form_data))) {
      # Filter rows based on the condition
      filtered_rows <- form_data %>%
        filter(!!sym(cond_var) == cond_value)
      
      # Parse dates
      parsed_dates <- parse_date_time(filtered_rows[[death_var]], 
                                      orders = c("%d/%m/%Y", "%d-%m-%Y", "%Y-%m-%d", "%d-%b-%y"), 
                                      exact = FALSE)
      parsed_dates <- ymd(parsed_dates)
      
      # Filter valid dates
      valid_mask <- !is.na(parsed_dates) & !(parsed_dates %in% invalid_death_dates)
      valid_rows <- filtered_rows[valid_mask, ]
      valid_dates <- parsed_dates[valid_mask]
      
      # Append to final collection
      if (nrow(valid_rows) > 0) {
        tmp <- valid_rows %>%
          select(MOMID, PREGID) %>%
          mutate(death_date = valid_dates,
                 form = form_name)
        
        all_deaths <- bind_rows(all_deaths, tmp)
      }
    }
  }
}

# Transform to wide format
if (nrow(all_deaths) > 0) {
  # Pivot to wide format
  all_deaths_wide <- all_deaths %>%
    pivot_wider(names_from = form, values_from = death_date, values_fn = list)
  
  # Calculate earliest death date
  all_deaths_wide <- all_deaths_wide %>%
    rowwise() %>%
    mutate(earliest_death_date = as.Date(min(c_across(starts_with("mnh")), na.rm = TRUE))) %>%
    ungroup()
  
  # Print the wide dataframe
  print("All death records in wide format with earliest death date:")
  
  # Assuming 'all_deaths_wide' from previous step contains ID, MOMID, PREGID, earlist_death_date
  
  visit_date_vars <- list(
    mnh00 = "SCRN_OBSSTDAT",
    mnh01 = "US_OHOSTDAT",
    mnh02 = "SCRN_OBSSTDAT",
    mnh03 = "SD_OBSSTDAT",
    mnh04 = "ANC_OBSSTDAT",
    mnh05 = "ANT_PEDAT",
    mnh06 = "DIAG_VSDAT",
    mnh07 = "MAT_SPEC_COLLECT_DAT",
    mnh08 = "LBSTDAT",
    mnh09 = "MAT_LD_OHOSTDAT",
    mnh10 = "VISIT_OBSSTDAT",
    mnh11 = "VISIT_OBSSTDAT",
    mnh12 = "VISIT_OBSSTDAT",
    mnh16 = "VISDAT",
    mnh17 = "VISDAT",
    mnh18 = "VISDAT",
    mnh19 = "OBSSTDAT",
    mnh25 = "OBSSTDAT",
    mnh26 = "FTGE_OBSTDAT"
  )
  
  # Initialize an empty dataframe to collect mismatches
  visit_after_death <- data.frame()
  
  # Loop through forms with MAT_VITAL_MNH00-36 checks
  for (form_name in names(visit_date_vars)) {
    
    if (!exists(form_name, where = .GlobalEnv, inherits = FALSE)) {
      message(paste("Skipping", form_name, "- dataset does not exist."))
      next
    }
    # form_name = "mnh26"
    form_data <- get(form_name)  # assume dataset loaded as form_name
    
    vital_var <- paste0("MAT_VITAL_", toupper(form_name))  # dynamic vital var like MAT_VITAL_MNH04
    visit_var <- visit_date_vars[[form_name]]
    
    if (all(c("MOMID", "PREGID", vital_var, visit_var) %in% names(form_data))) {
      
      form_subset <- form_data %>%
        filter(!!sym(vital_var) == 1)
      
      if (nrow(form_subset) > 0) {
        # Join with all_deaths_wide on MOMID & PREGID
        joined <- form_subset %>%
          right_join(all_deaths_wide, by = c("MOMID", "PREGID")) %>% 
          mutate(
            visit_date = parse_date_time(!!sym(visit_var), 
                                         orders = c("d/m/Y", "d-m-Y", "Y-m-d", "d-b-y", "d-m-y"), 
                                         exact = FALSE),
            check = ifelse(visit_date > earliest_death_date, 1, 0)
          ) %>% 
          filter(check == 1)
        
        # Keep violations
        if (nrow(joined) > 0) {
          mismatch_rows <- joined %>%
            mutate(
              VisitDate = visit_date,
              TYPE_VISIT = ifelse("TYPE_VISIT" %in% names(joined), as.character(TYPE_VISIT), NA_character_),
              VITAL_STATUS = !!sym(vital_var),
              `Variable Name` = visit_var,
              Form = toupper (form_name)
            ) %>%
            select(MOMID, PREGID, VisitDate, TYPE_VISIT, VITAL_STATUS, `Variable Name`, Form, DEATH_DATE = earliest_death_date)
          
          visit_after_death <- bind_rows(visit_after_death, mismatch_rows)
        }
      }
    }
  }
  
  
  # Output the mismatches
  if (nrow(visit_after_death) > 0) {
    # rename data frame 
    Mom_DeadThenAlive_query <- visit_after_death %>% 
      mutate(VisitType = TYPE_VISIT, 
             ScrnID = NA_character_,
             InfantID = NA_character_) %>% 
      select(ScrnID, MomID = MOMID, PregID = PREGID, InfantID, VisitType, VisitDate, Form)
    
    
    if (dim(Mom_DeadThenAlive_query)[1] > 0){
      
      ## add additional columns 
      Mom_DeadThenAlive_query = cbind(QueryID = NA, 
                                      UploadDate = UploadDate, 
                                      Mom_DeadThenAlive_query,
                                      `Variable Name` = NA, 
                                      `Variable Value` = NA,
                                      FieldType = "NA", 
                                      EditType = "Invalid visit following reported mom death", 
                                      DateEditReported = format(Sys.time(), "%Y-%m-%d"))
      
      Mom_DeadThenAlive_query = Mom_DeadThenAlive_query %>% mutate_all(as.character())
      
      # combine form/edit type var 
      Mom_DeadThenAlive_query <- add_column(Mom_DeadThenAlive_query,Form_Edit_Type = paste(Mom_DeadThenAlive_query$Form,"_",Mom_DeadThenAlive_query$EditType))
      
      # assign queryid -- 
      # edit type id for Invalid Visit Following reported death is 11
      Mom_DeadThenAlive_query <- Mom_DeadThenAlive_query %>% 
        mutate(QueryID = paste0(Form, "_", VisitDate, "_",PregID, "_", "11"))
      
      # Export data
      save(Mom_DeadThenAlive_query, file = paste0(maindir,"/queries/Mom_DeadThenAlive_query.rda"))
    }
  }
  
} else {
  print("No valid maternal death records found.")
}
