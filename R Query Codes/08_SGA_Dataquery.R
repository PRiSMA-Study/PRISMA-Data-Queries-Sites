#*****************************************************************************
#* QUERY #8 -- Small for Gestational age 
#* Input: Wide data (all raw .csv files) & Long data
#* Function:  Pull any size for gestational age <0.5 or >99.5 percentiles using INTERGROWTH standards
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* Last Updated: 30 March 2025
#* Updated the codes to new version

#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for

#* Once the previous lines of code are updated, you can start to run the script 
#*****************************************************************************
# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)
library(growthstandards)
# The `growthstandards` R package allows the user to pull INTERGROWTH centiles for 
# newborn length, weight, and head circumference. 
# R Package linked: (https://ki-tools.github.io/growthstandards/articles/usage.html#intergrowth-newborn-standard-1).

# the package can be downloaded using the following code: 
# install.packages("remotes") # if "remotes" is not already installed
# remotes::install_github("ki-tools/growthstandards")

## UPDATE EACH RUN ## 
# 1. Update "UploadDate" (this should match the folder name in synapse)
# 2. Set "site" variable to the site you are running the query for 
# UPDATE EACH RUN:
site = "Kenya"

# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

## if a site has all the forms needed for this query, the remainder of the code will run. if not, the code will stop
if (exists("mnh01")== TRUE & exists("mnh09")== TRUE & exists("mnh11")== TRUE){
  
  print("All forms required to run this query exist.")

} else {
  
  stop("SITE does not have all forms required to run this query - don't run.")
  
}
#*****************************************************************************
#* not all sites are reporting momid and pregid in mnh01 - this will cause some issues in this workflow 
#* solution: merge in momid and pregid from MNH02 into MNH01 based on scrnid
#*****************************************************************************

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
    mutate(US_OHOSTDAT = ymd(parse_date_time(US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
    ## remove any unscheduled visit types
    filter(TYPE_VISIT != 13) %>% 
    # "zero out" GA and obtain the estimated "date of conception" 
    mutate(EST_CONCEP_DATE = US_OHOSTDAT - GA_US_DAYS) %>% 
    ## only want enrollment visit
    filter(TYPE_VISIT==1) %>% 
    select(MOMID, PREGID, GA_US_DAYS, GA_US_WKS, EST_CONCEP_DATE)
  
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
    mutate(US_OHOSTDAT = ymd(parse_date_time(US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
    ## remove any unscheduled visit types
    filter(TYPE_VISIT != 13) %>% 
    # "zero out" GA and obtain the estimated "date of conception" 
    mutate(EST_CONCEP_DATE = US_OHOSTDAT - GA_US_DAYS) %>% 
    ## only want enrollment visit
    filter(TYPE_VISIT==1) %>% 
    select(MOMID, PREGID, GA_US_DAYS, GA_US_WKS, EST_CONCEP_DATE)
}


## need to convert MNH09 to long format to use the infant data within; infant variables are named with "_INFx" format - remove suffix and make long. Should have one row for each mom-baby pair 
if (exists("mnh09")==TRUE){
  
  ## first need to make m09 long format for each infant 
  m09_INF1 <- mnh09 %>% 
    rename("INFANTID" = "INFANTID_INF1") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF2"), -contains("_INF3"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF1')) %>% 
    mutate(INFANTID = as.character(INFANTID))
  
  m09_INF2 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF2") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF3"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF2')) %>% 
    mutate(INFANTID = as.character(INFANTID))
  
  m09_INF3 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF3") %>% 
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF2"), -contains("_INF4")) %>% 
    rename_with(~str_remove(., '_INF3')) %>% 
    mutate(INFANTID = as.character(INFANTID))
  
  m09_INF4 <- mnh09 %>% rename("INFANTID" = "INFANTID_INF4") %>%
    filter(INFANTID != "n/a") %>% 
    select(-contains("_INF1"), -contains("_INF2"), -contains("_INF3")) %>% 
    rename_with(~str_remove(., '_INF4')) %>% 
    mutate(INFANTID = as.character(INFANTID))
  
  ## bind all infants together 
  mnh09_update <- bind_rows(m09_INF1, m09_INF2, m09_INF3, m09_INF4) 
  
  ## remove INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4
  infantids_to_remove <- c("INFANTID_INF1", "INFANTID_INF2", "INFANTID_INF3", "INFANTID_INF4")
  mnh09_update <- mnh09_update %>% select(-any_of(infantids_to_remove))
  
  mnh09_long <- mnh09_update %>% bind_cols(SCRNID = NA) %>% 
    mutate(VisitDate = MAT_LD_OHOSTDAT) %>% 
    mutate(form = "MNH09") %>% 
    select(MOMID, PREGID, INFANTID,MAT_LD_OHOSTDAT, SEX, INFANTS_FAORRES, 
           contains("DELIV_DSSTDAT"), contains("BIRTH_DSTERM")) %>% 
    mutate(DELIV_DSSTDAT = ymd(parse_date_time(DELIV_DSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y"))))
    
}

## PRISMA birhweights 
# logic:
# PRISMA BIRTHWEIGHT (varname: BWEIGHT_PRISMA)
  #1. if prisma birthweight is available & hours <72 --> use prisma birthweight
  #2. if prisma birthweight is available but hours are missing --> use prisma birthweight
  #3. if prisma birthweight is available but hours are missing --> use facility-reported birthweight
# ANY BIRTHWEIGHT (varname: BWEIGHT_ANY)
  #4. if BWEIGHT_PRISMA (generated with the logic above) is available --> use BWEIGHT_PRISMA
  #5. if BWEIGHT_PRISMA is not available --> use facility-reported birthweight
mnh11_sub <- mnh11 %>% 
  # Generate variable for PRISMA-measured birthweight (<72 hours)
  mutate(BWEIGHT_PRISMA = case_when(BW_EST_FAORRES >=0 & BW_EST_FAORRES < 72 & BW_FAORRES > 0 ~ BW_FAORRES,  # if time since birth infant was weight is between 0 & 72 hours
                                    BW_FAORRES > 0 & (is.na(BW_EST_FAORRES) | BW_EST_FAORRES %in% c(-5,-7)) ~ BW_FAORRES, # if prisma birthweight available and no time reported, use prisma
                                    BW_FAORRES > 0 & BW_EST_FAORRES >=72 ~ -5, # if prisma birthweight is available but time is >= 72 hours, not usable
                                    BW_FAORRES < 0 ~ -5, # if prisma birthweight is missing, missing
                                    TRUE ~ -5), # if prisma birthweight is missing, replace with default value -5
         # Generate variable using facility birthweight where PRISMA-measured birthweight is not available
         BWEIGHT_ANY = case_when((BWEIGHT_PRISMA <= 0  & BW_FAORRES_REPORT > 0) | ## if PRISMA is missing and facility is not 
                                   (BWEIGHT_PRISMA < 0 & BW_EST_FAORRES >= 72 & BW_FAORRES_REPORT >0) ~ BW_FAORRES_REPORT, ## OR if prisma is not missing but time is >7days, select facility
                                 BWEIGHT_PRISMA < 0 &  BW_FAORRES_REPORT < 0 ~ -5, # if prisma is available but the time is invalid, use facility
                                 TRUE ~ BW_FAORRES)) %>% 
  select(MOMID, PREGID, INFANTID, BW_FAORRES,  BW_EST_FAORRES, BW_EST_FAORRES,BWEIGHT_PRISMA, BWEIGHT_ANY) #INF_DSTERM

#*****************************************************************************
#* Run query for Size for Gestational Age (SGA)
## Logic: 
  # if SGA centile (generated from INTERGROWTH package) is < 0.5 --> query
  # if SGA centile (generated from INTERGROWTH package) is > 99.5 --> query
#*****************************************************************************
sga <- mnh09_long %>% 
  select(INFANTID, MOMID, PREGID,INFANTS_FAORRES, BIRTH_DSTERM,DELIV_DSSTDAT, SEX)  %>% 
  # merge in MNH01 to get gestational age at birth variables 
  left_join(mnh01_sub, by = c("MOMID", "PREGID")) %>% 
  # calculate the estimated gestational age at birth 
  mutate(GESTAGEBIRTH_DAYS = as.numeric(DELIV_DSSTDAT- EST_CONCEP_DATE),
         GESTAGEBIRTH_WKS = GESTAGEBIRTH_DAYS %/% 7) %>% 
  # convert to numeric 
  mutate(across(c(GESTAGEBIRTH_DAYS, GESTAGEBIRTH_WKS, INFANTS_FAORRES, BIRTH_DSTERM, SEX), as.numeric)) %>%
  ## only want live births 
  filter(BIRTH_DSTERM == 1) %>% 
  ## merge with mnh11 
  left_join(mnh11_sub, by = c("MOMID", "PREGID", "INFANTID")) %>% 
  ## convert weight from grams to kg 
  mutate(BWEIGHT_PRISMA_KG = case_when(BWEIGHT_PRISMA > 0 ~ BWEIGHT_PRISMA/1000,
                                       TRUE ~ -5), 
         BWEIGHT_ANY_KG = case_when(BWEIGHT_ANY < 0 | is.na(BWEIGHT_ANY) ~ -5, 
                                    TRUE ~ BWEIGHT_ANY/1000)) %>% 
  ## calculate percentile
  mutate(SGA_CENTILE = case_when(is.na(GESTAGEBIRTH_DAYS) ~ -5,  
                                 SEX == 1  & BWEIGHT_ANY_KG > 0 ~ igb_wtkg2centile(GESTAGEBIRTH_DAYS, BWEIGHT_ANY_KG, sex = "Male"), # Sex = 1, Male
                                 SEX == 2 & BWEIGHT_ANY_KG > 0 ~ igb_wtkg2centile(GESTAGEBIRTH_DAYS, BWEIGHT_ANY_KG, sex = "Female"), # Sex = 2, Female
                                 BWEIGHT_ANY_KG <= 0 ~ -5, 
                                 TRUE ~ -5)) %>% 
  # format percentile 
  mutate(SGA_CENTILE = round(SGA_CENTILE, 3)) %>% 
  select(MOMID, PREGID, INFANTID,BIRTH_DSTERM, GESTAGEBIRTH_DAYS, GESTAGEBIRTH_WKS, SEX, BWEIGHT_ANY_KG,BWEIGHT_PRISMA_KG, SGA_CENTILE) %>% 
  filter(BWEIGHT_ANY_KG > 0 & GESTAGEBIRTH_DAYS <= 300) %>%
  ## generate new query variable; if <0.5 or >99.5
  mutate(QUERY = case_when(SGA_CENTILE < 0.5 | SGA_CENTILE > 99.5 ~ 1,
                               TRUE ~ 0)) %>% 
  filter(QUERY == 1) %>% 
  select(MOMID, PREGID, INFANTID, BIRTH_DSTERM, GESTAGEBIRTH_WKS, SEX, BWEIGHT_ANY_KG, SGA_CENTILE) %>% 
  mutate(SEX = case_when(SEX == 1 ~ "Male",
                         SEX == 2 ~ "Female", 
                         TRUE ~ NA)) %>% 
  mutate(BIRTH_DSTERM = case_when(BIRTH_DSTERM == 1 ~ "Live birth", 
                                  BIRTH_DSTERM == 2 ~ "Fetal death",
                                  TRUE ~ NA )) %>% 
  ## remove any SGA_CENTILE = -5; this is likely due to missing birthweigth or sex
  filter(SGA_CENTILE != -5)

#*****************************************************************************
## Formatting for the query report file
#*****************************************************************************

if (dim(sga)[1] >0){
## generate extra tab to accompany the query report
sga_extra_tab <- sga 
names(sga_extra_tab) = c("MOMID", "PREGID","INFANTID", "BIRTH_OUTCOME", "GESTAGEBIRTH_WKS",
                                   "SEX", "BWEIGHT_ANY_KG", "SGA_CENTILE")

## formatting for the main query report: 
## FORMATTING: ## 
# Update formatting to match query report 
SGA_query <- sga %>% 
  mutate(SCRNID = NA, 
         `Variable Name` = NA,
         `Variable Value` = SGA_CENTILE) %>% 
  select(SCRNID, MOMID, PREGID, INFANTID, `Variable Name`, `Variable Value`) 

# update naming
names(SGA_query) = c("ScrnID","MomID", "PregID","InfantID", "Variable Name", "Variable Value")

}


if (exists("SGA_query")== TRUE){
  
  ## add additional columns 
  SGA_query = cbind(QueryID = NA, 
                    UploadDate = UploadDate, 
                    #MomID = "NA", PregID = "NA",
                    VisitDate = NA,
                    Form = "MNH11",
                    SGA_query, 
                    FieldType = "Number", 
                    EditType = "Size for gestational age either <0.5 or >99.5 percentile", 
                    DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  
  
  # combine form/edit type var 
  SGA_query <- add_column(SGA_query, Form_Edit_Type = paste(SGA_query$Form,"_",SGA_query$EditType))
  
  ## assign queryid -- 
  # edit type id for Invalid Visit Following reported death is 11
  SGA_query <- SGA_query %>% 
    mutate(QueryID = paste0(Form, "_", PregID, "_",InfantID, "_", `Variable Value`, "_", "11"))
  
  # Export data
  save(SGA_query, file = paste0(path_to_save,"/queries/SGA_query.rda"))
  
}

#*****************************************************************************
## Export "extra tab" to be included in the query report
#*****************************************************************************

if (exists("sga_extra_tab")== TRUE){
  
  sga_extra_tab <- sga_extra_tab %>% 
    mutate(QueryID = paste0("MNH11_", PREGID, "_",INFANTID, "_", SGA_CENTILE, "_", "11")) %>% 
    relocate(QueryID, .before = "MOMID")
  
  # Export data
  save(sga_extra_tab, file = paste0(path_to_save,"/queries/sga_extra_tab.rda")) 
} 
