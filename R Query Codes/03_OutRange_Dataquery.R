#*****************************************************************************
#*QUERY #3 -- CHECK FOR OUT OF RANGE VALUES 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated:  25 February 2025
#* Updates: 
  ## added code to check for Blood Pressure discrepances 

#*Input: Long data 
#*Function: Extract any values that either (1) do not match a valid response options or (2) is out of range 
#*Output: .rda file with all out of range values 

#*****************************************************************************
#* Data setup 
#*****************************************************************************

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)

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

#*import data dictionary from github folder
varNames_sheet <- read_excel("~/PRiSMAv2Data/PRISMA-Data-Queries-Sites/R Query Codes/PRISMA-MNH-Data-Dictionary-Repository-V.2.7-25OCT2024_queries.xlsx",
                             sheet = "Data Dictionary")
varNames_sheet <- varNames_sheet %>% dplyr::select(Form, `Variable Name`, `Response Options`, `Query Category`,`Value`, `Field Type (Date, Time, Number, Text)`, `Minimum Value`, `Maximum Value`)
varNames_sheet$`Variable Name` = toupper(varNames_sheet$`Variable Name`)

## rename variables in data dictionary
varNames_sheet <- varNames_sheet %>% rename("varname" = `Variable Name`, "form" = "Form", "ResponseRange" = `Value`)
#*****************************************************************************
#* Fetal Biometry Checks 
#******************************************************************************
#* CAL_GA_WKS_AGE_FTS1 -- estimated GA in days by ACOG 
#* FL_PERES_01_FTS1; FL_PERES_MEAN_FTS1 
#* AC_PERES_01_FTS1; AC_PERES_MEAN_FTS1
#* HC_PERES_01_FTS1; HC_PERES_MEAN_FTS1
#* BPD_PERES_01_FTS1; BPD_PERES_MEAN_FTS1
fetalRange_sheet <- read_excel("~/PRiSMAv2Data/PRISMA-Data-Queries-Sites/R Query Codes/fetal_biometry_range.xlsx")

## will need to update US_TYPE or TYPE_VISIT depending on what sites are reporting 
fetal_biometry_vars = c("SCRNID","MOMID","PREGID","US_OHOSTDAT", "TYPE_VISIT", "US_VISIT", 
                        "US_GA_WKS_AGE_FTS1", "US_GA_DAYS_AGE_FTS1", "US_GA_WKS_AGE_FTS2", "US_GA_DAYS_AGE_FTS2", ##  GA 
                        "US_GA_WKS_AGE_FTS3", "US_GA_DAYS_AGE_FTS3", "US_GA_WKS_AGE_FTS4", "US_GA_DAYS_AGE_FTS4", 
                        "FL_PERES_01_FTS1", "FL_PERED_01_FTS2", "FL_PERED_01_FTS3", "FL_PERED_01_FTS4", ## first measurement of FL 
                        "FL_PERES_02_FTS1", "FL_PERED_02_FTS2", "FL_PERED_02_FTS3", "FL_PERED_02_FTS4", ## second measurement of FL
                        "FL_PERES_MEAN_FTS1", "FL_PERES_MEAN_FTS2", "FL_PERES_MEAN_FTS3", "FL_PERES_MEAN_FTS4",   ## FL mean 
                        "AC_PERES_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", ## first measurement of AC 
                        "AC_PERES_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", ## second measurement of AC 
                        "AC_PERES_MEAN_FTS1", "AC_PERES_MEAN_FTS2", "AC_PERES_MEAN_FTS3", "AC_PERES_MEAN_FTS4", ## AC mean
                        "HC_PERES_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", ## first measurement of HC
                        "HC_PERES_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", ## second measurement of HC
                        "HC_PERES_MEAN_FTS1", "HC_PERES_MEAN_FTS2", "HC_PERES_MEAN_FTS3", "HC_PERES_MEAN_FTS4", ## HC mean 
                        "BPD_PERES_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", ## first measurement of bpd 
                        "BPD_PERES_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", ## second measurement of bpd 
                        "BPD_PERES_MEAN_FTS1", "BPD_PERES_MEAN_FTS2", "BPD_PERES_MEAN_FTS3", "BPD_PERES_MEAN_FTS4") ## BPD mean 

# extract fetal biometry  variables from the data 
#forms_fetal_biomery <- data_long %>% filter(varname %in% fetal_biometry_vars)
forms_fetal_biomery <- mnh01 %>% select(any_of(fetal_biometry_vars))

# convert visit date variable to date class 
forms_fetal_biomery$US_OHOSTDAT = ymd(parse_date_time(forms_fetal_biomery$US_OHOSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%d-%b-%y","%Y-%m-%d")))

# extract ultrasound visit 1 and calculate GA_US_BASLINE_DAYS 
forms_fetal_biomery_visit1 <- forms_fetal_biomery %>% 
  filter(TYPE_VISIT == 1) %>% 
  ## identify max GA 
  mutate(GA_US_DAYS_FTS1 =  ifelse(US_GA_WKS_AGE_FTS1!= -7 & US_GA_DAYS_AGE_FTS1 != -7,  (US_GA_WKS_AGE_FTS1 * 7 + US_GA_DAYS_AGE_FTS1), NA), 
         GA_US_DAYS_FTS2 =  ifelse(US_GA_WKS_AGE_FTS2!= -7 & US_GA_DAYS_AGE_FTS2 != -7,  (US_GA_WKS_AGE_FTS2 * 7 + US_GA_DAYS_AGE_FTS2), NA),
         GA_US_DAYS_FTS3 =  ifelse(US_GA_WKS_AGE_FTS3!= -7 & US_GA_DAYS_AGE_FTS3 != -7,  (US_GA_WKS_AGE_FTS3 * 7 + US_GA_DAYS_AGE_FTS3), NA),
         GA_US_DAYS_FTS4 =  ifelse(US_GA_WKS_AGE_FTS4!= -7 & US_GA_DAYS_AGE_FTS4 != -7,  (US_GA_WKS_AGE_FTS4 * 7 + US_GA_DAYS_AGE_FTS4), NA)) %>% 
  mutate(GestAge_Days = pmax(GA_US_DAYS_FTS1, GA_US_DAYS_FTS2, GA_US_DAYS_FTS3, GA_US_DAYS_FTS4, na.rm = TRUE)) %>% 
  select(-GA_US_DAYS_FTS1, -GA_US_DAYS_FTS2, -GA_US_DAYS_FTS3, -GA_US_DAYS_FTS4)

## in order to calculate GA at at the other US visits, we need to merge in the GA at screening date with the subset of data for the other visits 
## below i am creating a subset of the  forms_fetal_biomery_visit1 dataset to merge 
visit1_to_merge <- forms_fetal_biomery_visit1 %>% 
  # rename visit date to "baseline date" - this will help with merging 
  rename("BASELINE_DATE" = US_OHOSTDAT) %>% 
  rename("GA_US_BASELINE_DAYS" = GestAge_Days) %>% 
  select(SCRNID, BASELINE_DATE, GA_US_BASELINE_DAYS)

# extract non-screening ultrasound visits  
forms_fetal_biomery_other_visits <- forms_fetal_biomery %>% 
  filter(TYPE_VISIT != 1) 

# merge screening visit and non-screening visits (we need GA_US_BASELINE_DAYS as its own column so we can calculate GA at visit)
forms_fetal_biomery_cal_ga <- left_join(forms_fetal_biomery_other_visits, visit1_to_merge, by = c("SCRNID"))
forms_fetal_biomery_cal_ga <- forms_fetal_biomery_cal_ga %>% 
  # calculate the difference in days between US visits 
  mutate(DIFF_DAYS = as.numeric(US_OHOSTDAT-BASELINE_DATE)) %>% 
  # add the days difference to the GA at screening ultrasound to get the expected GA at non-screening ultrasound
  mutate(GestAge_Days = GA_US_BASELINE_DAYS + DIFF_DAYS) %>% 
  select(-BASELINE_DATE, -GA_US_BASELINE_DAYS, -DIFF_DAYS)

## rbind all visits together
forms_fetal_biomery_all_visits <- bind_rows(forms_fetal_biomery_visit1, forms_fetal_biomery_other_visits)
forms_fetal_biomery_all_visits <- forms_fetal_biomery_all_visits %>% 
  relocate("GestAge_Days", .after = "PREGID") %>% 
  select(-contains("US_GA_WKS_AGE_FTS"), -contains("US_GA_DAYS_AGE_FTS")) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = -c(1:6), 
               names_to = "varname", values_to = "response") %>% 
  rename(VisitDate = "US_OHOSTDAT") %>% 
  ## make sure GA is floored 
  mutate(GestAge_Days = floor(as.numeric(GestAge_Days)))

## merge fetal biometry excel sheet and the data 
forms_fetal_biomery_ranges <- merge(forms_fetal_biomery_all_visits, fetalRange_sheet, by = "GestAge_Days")

# query BPD 
bpd <- c("BPD_PERES_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", "BPD_PERED_01_FTS1", ## first measurement of bpd 
         "BPD_PERES_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", "BPD_PERED_02_FTS1", ## second measurement of bpd 
         "BPD_PERES_MEAN_FTS1", "BPD_PERES_MEAN_FTS2", "BPD_PERES_MEAN_FTS3", "BPD_PERES_MEAN_FTS4")## BPD mean 

forms_fetal_biomery_bpd <- forms_fetal_biomery_ranges %>% filter(varname %in% bpd, response !=-7) ## remove default value
forms_fetal_biomery_bpd$outrange <- ifelse((forms_fetal_biomery_bpd$response >= forms_fetal_biomery_bpd$bpd_min & forms_fetal_biomery_bpd$response <= forms_fetal_biomery_bpd$bpd_max) | forms_fetal_biomery_bpd$response!= "-7", "", forms_fetal_biomery_bpd$response)

# query HC
hc <- c("HC_PERES_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", "HC_PERED_01_FTS1", ## first measurement of HC
        "HC_PERES_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", "HC_PERED_02_FTS1", ## second measurement of HC
        "HC_PERES_MEAN_FTS1", "HC_PERES_MEAN_FTS2", "HC_PERES_MEAN_FTS3", "HC_PERES_MEAN_FTS4") ## HC mean 

forms_fetal_biomery_hc <- forms_fetal_biomery_ranges %>% filter(varname %in% hc, response !=-7)
forms_fetal_biomery_hc$outrange <- ifelse((forms_fetal_biomery_hc$response >= forms_fetal_biomery_hc$hc_min & forms_fetal_biomery_hc$response <= forms_fetal_biomery_hc$hc_max) | forms_fetal_biomery_hc$response!= "-7", "", forms_fetal_biomery_hc$response)

# query AC 
ac <- c("AC_PERES_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", "AC_PERED_01_FTS1", ## first measurement of AC 
        "AC_PERES_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", "AC_PERED_02_FTS1", ## second measurement of AC 
        "AC_PERES_MEAN_FTS1", "AC_PERES_MEAN_FTS2", "AC_PERES_MEAN_FTS3", "AC_PERES_MEAN_FTS4") ## AC mean

forms_fetal_biomery_ac <- forms_fetal_biomery_ranges %>% filter(varname %in% ac, response !=-7)
forms_fetal_biomery_ac$outrange <- ifelse((forms_fetal_biomery_ac$response >= forms_fetal_biomery_ac$ac_min & forms_fetal_biomery_ac$response <= forms_fetal_biomery_ac$ac_max) | forms_fetal_biomery_ac$response!= "-7", "", forms_fetal_biomery_ac$response)

# query FL 
fl <- c("FL_PERES_01_FTS1", "FL_PERED_01_FTS2", "FL_PERED_01_FTS3", "FL_PERED_01_FTS4", ## first measurement of FL 
        "FL_PERES_02_FTS1", "FL_PERED_02_FTS2", "FL_PERED_02_FTS3", "FL_PERED_02_FTS4", ## second measurement of FL
        "FL_PERES_MEAN_FTS1", "FL_PERES_MEAN_FTS2", "FL_PERES_MEAN_FTS3", "FL_PERES_MEAN_FTS4")  ## FL mean 

forms_fetal_biomery_fl <- forms_fetal_biomery_ranges %>% filter(varname %in% fl, response !=-7)
forms_fetal_biomery_fl$outrange <- ifelse((forms_fetal_biomery_fl$response >= forms_fetal_biomery_fl$fl_min & forms_fetal_biomery_fl$response <= forms_fetal_biomery_fl$fl_max) | forms_fetal_biomery_fl$response!= "-7", "", forms_fetal_biomery_fl$response)

# rbind all fetal biometry measurements 
forms_fetal_biometry_all <- rbind(forms_fetal_biomery_bpd, forms_fetal_biomery_hc, forms_fetal_biomery_ac, forms_fetal_biomery_fl)

# make edit message 
forms_fetal_biometry_all$editmessage <- ifelse(forms_fetal_biometry_all$outrange == "", "NoError", "Out of Range")

# filter out those that are out of range 
forms_fetal_biometry_query <- forms_fetal_biometry_all %>% filter(editmessage == "Out of Range")

## only keep the first 7 columns 
forms_fetal_biometry_query = forms_fetal_biometry_query %>% select(SCRNID, MOMID, PREGID, TYPE_VISIT, VisitDate, varname, response)

## add infant ID  column
forms_fetal_biometry_query <- add_column(forms_fetal_biometry_query, InfantID = NA , .after = "PREGID")

## add form column
forms_fetal_biometry_query <- add_column(forms_fetal_biometry_query, Form = "MNH01" , .after = "VisitDate")

FetalBioRangeQuery_Export <- forms_fetal_biometry_query

# update naming
names(FetalBioRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")

## add additional columns 
if (nrow(FetalBioRangeQuery_Export)>=1){
  FetalBioRangeQuery_Export = cbind(QueryID = NA, 
                                    UploadDate = UploadDate, 
                                    #MomID = "NA", PregID = "NA",
                                    #VisitDate = "NA", 
                                    FetalBioRangeQuery_Export, 
                                    #`Variable Name` = "NA",
                                    FieldType = "Number", 
                                    EditType = "Out of Range", 
                                    DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

if (nrow(FetalBioRangeQuery_Export)>=1){
  FetalBioRangeQuery_Export$`Variable Value` = as.character(FetalBioRangeQuery_Export$`Variable Value`)
}

#*****************************************************************************
#*Categorical Variable Queries 
#*****************************************************************************

## merge data dictionary and site data  
invalid_response_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
## make vector of MNH25 forms - we will check these separately at the end 
m25_forms = c("MNH25_Ghana", "MNH25_India", "MNH25_Kenya", "MNH25_Zambia", "MNH25_Pakistan")

#*****************************************************************************
#*Categorical Variable Queries
#*****************************************************************************
## merge data dictionary and site data
invalid_response_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
## make vector of MNH25 forms - we will check these separately at the end
m25_forms = c("MNH25_Ghana", "MNH25_India", "MNH25_Kenya", "MNH25_Zambia", "MNH25_Pakistan")

invalid_response_categorical <- invalid_response_merge %>% filter(`Query Category` == "Number", !(form %in% m25_forms))
# Step 0: Remove default values from the new query dataframe
default_values <- c("55", "88", "77", "99", "66")
# Step 1: Create a new dataframe excluding NA ResponseRange
filtered_response_categorical <- invalid_response_categorical %>%
  filter(!is.na(ResponseRange))  %>%
  filter(!is.na(response)) %>%
  filter(!response %in% default_values)
# Step 2: Convert Response and ResponseRange to character
filtered_response_categorical <- filtered_response_categorical %>%
  mutate(
    response = as.character(response),
    response_range = as.character(ResponseRange)
  )
# Step 3: Convert ResponseRange into a vector of values by splitting on commas (if applicable)
filtered_response_categorical <- filtered_response_categorical %>%
  mutate(response_range_vector = strsplit(response_range, ",")) %>%
  mutate(in_range = map2_lgl(response, response_range_vector, ~ .x %in% .y)) %>%
  mutate(editmessage = ifelse(!in_range, "Invalid Response Option", "No Error"))
# Step 6: Create a new dataframe for invalid responses only
invalid_response_categorical_query <- filtered_response_categorical %>%
  filter(editmessage == "Invalid Response Option")  %>%
  filter(!response %in% default_values)
# Step 7: Remove MNH25 variables
invalid_response_categorical_query <- invalid_response_categorical_query %>%
  filter(form != "MNH25") %>%
  filter (!is.na(response))
## only keep the first 7 columns
InvalidResponseCategoricalQuery_Export = invalid_response_categorical_query %>%
  select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VisitDate, form, varname, response)

# update naming
names(InvalidResponseCategoricalQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")

## add additional columns 
if (nrow(InvalidResponseCategoricalQuery_Export)>=1){
  InvalidResponseCategoricalQuery_Export = cbind(QueryID = NA, 
                                                 UploadDate = UploadDate, 
                                                 #MomID = "NA", PregID = "NA",
                                                 #VisitDate = "NA", 
                                                 InvalidResponseCategoricalQuery_Export, 
                                                 #`Variable Name` = "NA",
                                                 FieldType = "Number", 
                                                 EditType = "Invalid Response Option", 
                                                 DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

if (nrow(InvalidResponseCategoricalQuery_Export)>=1){
  InvalidResponseCategoricalQuery_Export$`Variable Value` = as.character(InvalidResponseCategoricalQuery_Export$`Variable Value`)
}

#*****************************************************************************
#*Continuous 
#*****************************************************************************
# extract continuous variables from the data dictionary 
## make vector of MNH25 forms - we will check these separately at the end 
m25_forms = c("MNH25_Ghana", "MNH25_India", "MNH25_Kenya", "MNH25_Zambia", "MNH25_Pakistan")
requested_varNames_out <- varNames_sheet %>% filter(`Query Category` == "Continuous" & (!is.na(`Minimum Value`) | !is.na(`Maximum Value`)), 
                                                    !(`varname` %in% fetal_biometry_vars), !(form %in% m25_forms)) 
form_num = toupper(form_num)
requested_varNames_out_var <- requested_varNames_out %>% filter(form %in% form_num) %>% 
  #rename("varname" = `Variable Name`) %>% 
  pull(varname)

# extract the continuous variables that don't have min max values -- save for later 
requested_varNames_Nominmax <- varNames_sheet %>% filter(`Query Category` == "Continuous" & (is.na(`Minimum Value`) | is.na(`Maximum Value`)))

# extract continuous variables from the data 
forms_con <- data_long %>% filter(varname %in% requested_varNames_out_var)

# merge together the min/max value columns 
requested_varNames_out_w_MinMax <- requested_varNames_out %>% dplyr::select(form, `varname`, `Minimum Value`, `Maximum Value`) #`DefaultValue`)
names(requested_varNames_out_w_MinMax) = c("form", "varname", "min", "max") #, "DefaultValue")
forms_con_merged <- merge(forms_con, requested_varNames_out_w_MinMax, by = c("form", "varname"))

# make sure response options, min, and max values are numeric 
forms_con_merged$response = as.numeric(forms_con_merged$response, na.rm = TRUE)
forms_con_merged$min = as.numeric(forms_con_merged$min)
forms_con_merged$max = as.numeric(forms_con_merged$max)
#forms_con_merged$DefaultValue = as.numeric(forms_con_merged$DefaultValue)

# query -- if greater than min and lower than max column or does not equaly the default value 
forms_con_merged$outrange <- ifelse((forms_con_merged$response >= forms_con_merged$min & forms_con_merged$response <= forms_con_merged$max), "", forms_con_merged$response)

# make edit message 
forms_con_merged$editmessage <- ifelse(forms_con_merged$outrange == "", "NoError", "Out of Range")

# filter out those that are out of range 
forms_con_query <- forms_con_merged %>% filter(editmessage == "Out of Range")

# get tab of variables that have default values 
default_values_continuous <- as.numeric(c("-5", "-8", "-7", "-9", "-6"))
out_range_default_value_continuous <- forms_con_merged %>% 
  group_by(varname) %>% 
  add_count(name = "n_total") %>%                    ## get the total count for each variable 
  filter(editmessage == "Out of Range") %>%          ## only look at the variables that do not include default values in the resposne options
  group_by(form, varname, response,n_total) %>%      ## group by variable name, response, total, and form
  count(name ="n_response") %>%                      ## count the number each specific response is reported 
  mutate(pcnt_total = (n_response/n_total)*100) %>%  ## get percentange each reponse
  filter(response %in% default_values_continuous)    ## exclude to only the default values 

# remove default values from query, but will review tabulation at end of script 
forms_con_query <- forms_con_query %>% filter(!(response %in% default_values_continuous))

## only keep the first 7 columns 
forms_con_query <- forms_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) %>%
  setcolfirst(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT,VisitDate, form, varname, response) 
ConRangeQuery_Export <- forms_con_query


# remove fetal biometry variables from this query since we checked them earlier 
ConRangeQuery_Export <- ConRangeQuery_Export %>% filter(!(varname %in% fetal_biometry_vars))

# remove MNH25 variables -- we will check these later 
ConRangeQuery_Export <- ConRangeQuery_Export %>% filter(form != "MNH25")

# update naming
names(ConRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")

#*****************************************************************************
#* EXTRACTING SITE-SPECIFIC LAB RANGE QUERIES
#*****************************************************************************

ConRangeQuery_Export <- ConRangeQuery_Export %>% 
  mutate( var_range_out = case_when(
          # WHITE BLOOD CELL
          `Variable Name` == "CBC_WBC_LBORRES" & site %in% c("Ghana","Kenya", "Zambia") & ( `Variable Value` >=1 & `Variable Value` <=14) ~ 1,
          `Variable Name` == "CBC_WBC_LBORRES" & site %in% c("India-CMC","India-SAS", "Pakistan") & (`Variable Value` >=2.5 & `Variable Value` <=18 ) ~ 1,
          
          # NEUTROPHILS
          `Variable Name` == "CBC_NEU_FCC_LBORRES" & site %in% c("Ghana","Kenya", "Zambia") & (`Variable Value` >=0.5 & `Variable Value` <=12) ~ 1,
          `Variable Name` == "CBC_NEU_FCC_LBORRES" & site %in% c("India-CMC","India-SAS", "Pakistan") & (`Variable Value` >=1.5 & `Variable Value` <=13) ~ 1,
          
          # SERUM CREATININE (common to all sites)
          `Variable Name` == "CREAT_UMOLL_LBORRES" & site %in% c("Ghana","Kenya", "Zambia", "India-CMC","India-SAS", "Pakistan") & (`Variable Value` >=0.5 & `Variable Value` <=10 ) ~ 1,
          
          TRUE ~ 0)) %>%
  filter(var_range_out == 0) %>%
  select(-var_range_out)


if (site == "India-SAS") {
  
  ConRangeQuery_Export <- ConRangeQuery_Export %>% mutate(VisitDate = ymd(parse_date_time(VisitDate, order = c("%m/%d/%y", "%Y-%m-%d")))) 
  
} else {
  
  ConRangeQuery_Export = ConRangeQuery_Export
}

## add additional columns 

if (nrow(ConRangeQuery_Export)>=1){
  
  ConRangeQuery_Export = cbind(QueryID = NA, 
                               UploadDate = UploadDate, 
                               #MomID = "NA", PregID = "NA",
                               #VisitDate = "NA", 
                               ConRangeQuery_Export, 
                               FieldType = "Number", 
                               EditType = "Out of Range", 
                               DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

if (nrow(ConRangeQuery_Export)>=1){
  ConRangeQuery_Export$`Variable Value` = as.character(ConRangeQuery_Export$`Variable Value`)
}


#*****************************************************************************
#* Out of range dates
#*****************************************************************************
# extract the date variables from the data dictionary
## also remove variables we would expect very early or futuristic days such as mothers date of birth and estimated due date (EDD dates will be checked in 07_EDD_query)
requested_varNames_vec <- varNames_sheet %>% filter(`Field Type (Date, Time, Number, Text)` == "Date") %>% 
  filter(!varname %in% c("BRTHDAT", "US_EDD_BRTHDAT_FTS1", "US_EDD_BRTHDAT_FTS2", "US_EDD_BRTHDAT_FTS3", "US_EDD_BRTHDAT_FTS4", 
                         "ESTIMATED_EDD_SCDAT", "CAL_EDD_BRTHDAT_FTS1", "CAL_EDD_BRTHDAT_FTS2", "CAL_EDD_BRTHDAT_FTS3", "CAL_EDD_BRTHDAT_FTS4")) %>% 
  pull(varname)

# extract continuous variables from the data 
forms_date_full <- data_long %>% filter(varname %in% requested_varNames_vec) 

# convert to date class 
forms_date <- forms_date_full %>% 
  mutate(response = ymd(parse_date_time(response, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) %>% 
  ## filter out any default values 
  filter(!response %in% c(ymd("2007-07-07"), ymd("1907-07-07"),  ymd("1905-05-05"), ymd("1909-09-09")))

# query -- if greater than the upload date OR less than 2000, then query
forms_date$outrange <- ifelse((forms_date$response > ymd(paste0(UploadDate)) | forms_date$response < ymd("2000-01-01")), forms_date$response, "")

# make edit message 
forms_date$editmessage <- ifelse(forms_date$outrange == "", "NoError", "Out of Range")

# filter out those that are out of range 
forms_date_query <- forms_date %>% filter(editmessage == "Out of Range")

## only keep the first 7 columns 
forms_date_query <- forms_date_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) %>% 
  setcolfirst(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) 

DateRangeQuery_Export <- forms_date_query

# update naming
names(DateRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")

## add additional columns 

if (nrow(DateRangeQuery_Export)>=1){
  
  DateRangeQuery_Export = cbind(QueryID = NA, 
                                UploadDate = UploadDate, 
                                #MomID = "NA", PregID = "NA",
                                #VisitDate = "NA", 
                                DateRangeQuery_Export, 
                                #`Variable Name` = "NA",
                                FieldType = "Date", 
                                EditType = "Date Out of Range", 
                                DateEditReported = format(Sys.time(), "%Y-%m-%d"))
}

if (nrow(DateRangeQuery_Export)>=1){
  DateRangeQuery_Export$`Variable Value` = as.character(DateRangeQuery_Export$`Variable Value`)
}







#*****************************************************************************
#*MNH25 Variable Queries 
#*****************************************************************************
# Set MNH25 form
mnh25_to_exclude <- c("MNH25_Ghana", "MNH25_Kenya", "MNH25_Zambia", "MNH25_Pakistan", "MNH25_India") # make a vector of all MNH25 forms 

## extra coding to work around 2 india sites 
if (site == "India-CMC" | site == "India-SAS"){
  site_mnh25 = "India"
} else {
  site_mnh25 = site
}

mnh25_to_exclude <- mnh25_to_exclude[!grepl(paste0(site_mnh25), mnh25_to_exclude)] # exclude the form of the site we are currently running. We will this to filter out all the forms we DONT need from the data dictionary

varNames_sheet <- varNames_sheet %>%
  filter(!(form %in% mnh25_to_exclude))

if (site_mnh25=="Kenya"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Kenya") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range" & (!response %in% c(-7,-9,-5)))
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VisitDate, form, varname, response) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) 
  M25_ConRangeQuery_Export <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery_Export)>=1){
    
    M25_ConRangeQuery_Export = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_ConRangeQuery_Export, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Kenya")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery_Export = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery_Export) = c("ScrnID","MomID", "PregID","InfantID", "TypeVisit", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery_Export)>=1){
    M25_OutRangeNumericQuery_Export = cbind(QueryID = NA, 
                                            UploadDate = UploadDate, 
                                            #MomID = "NA", PregID = "NA",
                                            #VisitDate = "NA", 
                                            M25_OutRangeNumericQuery_Export, 
                                            #`Variable Name` = "NA",
                                            FieldType = "Number", 
                                            EditType = "Out of Range", 
                                            DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}

if (site_mnh25=="Pakistan"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Pakistan") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range" & (!response %in% c(-7,-9,-5)))
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VisitDate, form, varname, response) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) 
  M25_ConRangeQuery_Export <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery_Export)>=1){
    
    M25_ConRangeQuery_Export = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_ConRangeQuery_Export, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Pakistan")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery_Export = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT,VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery_Export)>=1){
    M25_OutRangeNumericQuery_Export = cbind(QueryID = NA, 
                                            UploadDate = UploadDate, 
                                            #MomID = "NA", PregID = "NA",
                                            #VisitDate = "NA", 
                                            M25_OutRangeNumericQuery_Export, 
                                            #`Variable Name` = "NA",
                                            FieldType = "Number", 
                                            EditType = "Out of Range", 
                                            DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}

if (site_mnh25=="Ghana"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Ghana") 
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range" & (!response %in% c(-7,-9,-5)))
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID,VisitDate, form, varname, response) 
  M25_ConRangeQuery_Export <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery_Export)>=1){
    
    M25_ConRangeQuery_Export = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_ConRangeQuery_Export, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Ghana")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery_Export = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery_Export)>=1){
    M25_OutRangeNumericQuery_Export = cbind(QueryID = NA, 
                                            UploadDate = UploadDate, 
                                            #MomID = "NA", PregID = "NA",
                                            #VisitDate = "NA", 
                                            M25_OutRangeNumericQuery_Export, 
                                            #`Variable Name` = "NA",
                                            FieldType = "Number", 
                                            EditType = "Out of Range", 
                                            DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}

if (site_mnh25=="Zambia"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_Zambia") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range" & (!response %in% c(-7,-9,-5)))
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) %>%
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) 
  M25_ConRangeQuery_Export <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery_Export)>=1){
    
    M25_ConRangeQuery_Export = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_ConRangeQuery_Export, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract categorical variables from the data dictionary 
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_Zambia")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery_Export = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery_Export)>=1){
    M25_OutRangeNumericQuery_Export = cbind(QueryID = NA, 
                                            UploadDate = UploadDate, 
                                            #MomID = "NA", PregID = "NA",
                                            #VisitDate = "NA", 
                                            M25_OutRangeNumericQuery_Export, 
                                            #`Variable Name` = "NA",
                                            FieldType = "Number", 
                                            EditType = "Out of Range", 
                                            DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}


if (site_mnh25=="India"){
  
  # extract continuous variables from the data dictionary 
  requested_varNames_out <- varNames_sheet %>% filter(form == "MNH25_India") 
  form_num = toupper(form_num)
  requested_varNames_out_var <- requested_varNames_out %>%
    filter(`Query Category` == "Continuous") %>% 
    pull(varname)
  
  # extract continuous variables from the data 
  forms_dep <- data_long %>% filter(varname %in% requested_varNames_out_var)
  
  # look at continuous range and query 
  mnh25_con <- forms_dep %>% filter(varname == "EPDS01_SCORRES")
  mnh25_con$editmessage <- ifelse(as.numeric(mnh25_con$response) >= 0 & as.numeric(mnh25_con$response) <= 30, "NoError", "Out of Range")
  
  # filter out those that are out of range 
  mnh25_con_query <- mnh25_con %>% filter(editmessage == "Out of Range" & (!response %in% c(-7,-9,-5)))
  
  ## only keep the first 7 columns 
  mnh25_con_query <- mnh25_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) %>% 
    setcolfirst(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) 
  M25_ConRangeQuery_Export <- mnh25_con_query
  
  # update naming
  names(M25_ConRangeQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  
  if (nrow(M25_ConRangeQuery_Export)>=1){
    
    M25_ConRangeQuery_Export = cbind(QueryID = NA, 
                                     UploadDate = UploadDate, 
                                     #MomID = "NA", PregID = "NA",
                                     #VisitDate = "NA", 
                                     M25_ConRangeQuery_Export, 
                                     #`Variable Name` = "NA",
                                     FieldType = "Number", 
                                     EditType = "Out of Range", 
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
  # extract default values 
  M25_OutRangeNumericQuery_Export
  
  
  ## merge data dictionary and site data  
  out_range_merge <- left_join(varNames_sheet, data_long, by = c("varname", "form"))
  out_range_numeric_M25 <- out_range_merge %>% filter(`Query Category` == "Number", form == "MNH25_India")
  
  # make the response range a vector 
  out_range_numeric_M25$response_range = as.vector(c(out_range_numeric_M25$`ResponseRange`))
  
  # run the query
  out_range_numeric_M25$in_range <- apply(out_range_numeric_M25,1, function(x){
    grepl(x["response"], x["response_range"])
  })
  
  # make edit message 
  out_range_numeric_M25$editmessage <- ifelse(out_range_numeric_M25$in_range == "FALSE", "Out of Range", "No Error") 
  
  # filter out those that are out of range 
  out_range_numeric_M25_query <- out_range_numeric_M25 %>% filter(editmessage == "Out of Range")
  
  # remove default values for query but will review at the end of script 
  default_values <- c("55", "88", "77", "99", "66")
  out_range_numeric_M25_query <- out_range_numeric_M25_query %>% filter(!(response %in% default_values)) 
  
  
  ## only keep the first 7 columns 
  M25_OutRangeNumericQuery_Export = out_range_numeric_M25_query %>% select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response)
  
  # update naming
  names(M25_OutRangeNumericQuery_Export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name", "Variable Value")
  
  ## add additional columns 
  if (nrow(M25_OutRangeNumericQuery_Export)>=1){
    M25_OutRangeNumericQuery_Export = cbind(QueryID = NA, 
                                            UploadDate = UploadDate, 
                                            #MomID = "NA", PregID = "NA",
                                            #VisitDate = "NA", 
                                            M25_OutRangeNumericQuery_Export, 
                                            #`Variable Name` = "NA",
                                            FieldType = "Number", 
                                            EditType = "Out of Range", 
                                            DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  }
  
}


if (nrow(M25_OutRangeNumericQuery_Export)>=1){
  M25_OutRangeNumericQuery_Export$`Variable Value` = as.character(M25_OutRangeNumericQuery_Export$`Variable Value`)
}

# default_values_continuous <- as.numeric(c("-5", "-8", "-7", "-9", "-6"))
# out_range_default_value_continuous <- forms_con_merged %>% 
#   group_by(varname) %>% 
#   add_count(name = "n_total") %>%                    ## get the total count for each variable 
#   filter(editmessage == "Out of Range") %>%          ## only look at the variables that do not include default values in the resposne options
#   group_by(form, varname, response,n_total) %>%      ## group by variable name, response, total, and form
#   count(name ="n_response") %>%                      ## count the number each specific response is reported 
#   mutate(pcnt_total = (n_response/n_total)*100) %>%  ## get percentage each response
#   filter(response %in% default_values_continuous)    ## exclude to only the default values 
# 
# # remove default values from query, but will review tabulation at end of script 
# forms_con_query <- forms_con_query %>% filter(!(response %in% default_values_continuous))
# 
# ## only keep the first 7 columns 
# forms_con_query <- forms_con_query %>% dplyr:: select(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT, VisitDate, form, varname, response) %>%
#   setcolfirst(SCRNID, MOMID, PREGID, INFANTID,TYPE_VISIT,VisitDate, form, varname, response) 
# ConRangeQuery_Export <- forms_con_query


#*****************************************************************************
#* Blood Pressure Measurement
#* Goal 1: To compare the difference between bps to ensure that Systolic bp is ALWAYS higher than Diastolic bp: Query if negative
#* Goal 2: Calculate PP: Sys - Dia BP. Ranges should be between +40 to +60; Query less than 1/8 systolic bp or greater than 100.
#*****************************************************************************
# Check if mnh10 exists and apply mutation if it does
if (exists("mnh10")) {
  mnh10 <- mnh10 %>%
    mutate(TYPE_VISIT = 6, VisitDate = VISIT_OBSSTDAT)
}

# Check if mnh09 exists and apply mutation if it does
if (exists("mnh09")) {
  mnh09 <- mnh09 %>%
    mutate(TYPE_VISIT = 6, VisitDate = MAT_LD_OHOSTDAT)
}

# Check if mnh06 exists and apply mutation if it does
if (exists("mnh06")) {
  mnh06 <- mnh06 %>%
    mutate(VisitDate = DIAG_VSDAT)
}

# Define a function to perform the analysis on each data frame
analyze_blood_pressure <- function(df_name, suffix) {
  # Check if the data frame exists in the global environment
  if (!exists(df_name)) {
    return(NULL)
  }
  
  # Retrieve the data frame from the global environment
  df <- get(df_name)
  # Define the variable suffix
  suffix <- toupper(suffix)
  # Filter and process data based on the given suffix
  blood_pressure_df <- df %>%
    filter(get(paste0("MAT_VISIT_", suffix)) %in% c(1, 2) & get(paste0("MAT_VITAL_", suffix)) == 1 & get("BP_VSSTAT") == 1) %>%
    mutate(SCRNID = NA, INFANTID = NA, Form = toupper (df_name)) %>%
    select(SCRNID, MOMID, PREGID, INFANTID, VisitDate, VisitType = TYPE_VISIT, Form,
           starts_with(paste0("BP_SYS")), starts_with(paste0("BP_DIA"))) %>%
    mutate(across(starts_with(paste0("BP_SYS")) | starts_with(paste0("BP_DIA")), as.numeric)) %>%
    mutate(across(starts_with(paste0("BP_SYS")) | starts_with(paste0("BP_DIA")), ~ ifelse(. %in% c(-7, -5, -9), NA, .)))
  
  # Initialize an empty list to store results
  QueryList <- list()
  
  # Loop through the measurements
  for (i in 1:3) {
    # Define column names
    sys_col <- paste0("BP_SYS_VSORRES_", i)
    dia_col <- paste0("BP_DIA_VSORRES_", i)
    
    # Compute the difference
    Query <- blood_pressure_df %>%
      mutate(
        PulsePressure = ifelse((!is.na(.[[sys_col]]) & !is.na(.[[dia_col]])), (.[[sys_col]] - .[[dia_col]]), NA),
        Threshold = ifelse(!is.na(.[[sys_col]]), .[[sys_col]] * 0.125, NA),
        EditType = ifelse(is.na(PulsePressure) | is.na(Threshold), "N/A",
                          ifelse(PulsePressure == 0, paste0("Systolic BP is equal to Diastolic BP"),
                                 ifelse(PulsePressure < 0, paste0("Systolic BP is less than Diastolic BP"),
                                        ifelse(PulsePressure < Threshold, paste0("Pulse Pressure (Narrow) Out of Range"),
                                               ifelse(PulsePressure > 100, "Pulse Pressure (Wide) Out of Range", "Within Range"))))),
        Variable_Name = paste0("Constructed Diff ", sys_col, "-", dia_col),
        Variable_Value = PulsePressure) %>%
      filter(EditType != "N/A" & EditType != "Within Range") %>%
      mutate_all(as.character)
    
    # Store results in lists
    QueryList[[i]] <- Query
  }
  
  # Combine the queries into a single data frame
  blood_pressure_query <- do.call(rbind, QueryList)
  
  # Select relevant columns for export
  blood_pressure_query_export <- blood_pressure_query %>%
    select(SCRNID, MOMID, PREGID, INFANTID, VisitType, VisitDate, Form, Variable_Name, Variable_Value, EditType)
  
  # Rename columns for final export
  colnames(blood_pressure_query_export) <- c("ScrnID", "MomID", "PregID", "InfantID", "VisitType", "VisitDate", "Form", "Variable Name", "Variable Value", "EditType")
  
  # Add additional columns for export
  if (nrow(blood_pressure_query_export) >= 1) {
    BloodPressureQuery_Export <- cbind(QueryID = NA, UploadDate = UploadDate, blood_pressure_query_export,
                                       FieldType = "Number", DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    
    return(list(BloodPressureQuery_Export = BloodPressureQuery_Export, 
                blood_pressure_query_all = blood_pressure_query))
  } else {
    return(NULL)
  }
}

# List of data frame names and corresponding suffixes
df_names <- c("mnh06", "mnh09", "mnh10")
suffixes <- c("MNH06", "MNH09", "MNH10")

# Initialize a list to store all results
all_results <- list()

# Iterate over each data frame name and corresponding suffix
for (i in seq_along(df_names)) {
  df_name <- df_names[i]
  suffix <- suffixes[i]
  
  # Call the function with the data frame name and suffix
  result <- analyze_blood_pressure(df_name, suffix)
  
  # If there is a result, store it in the list
  if (!is.null(result)) {
    all_results[[df_name]] <- result
  }
}

# Combine all results into a single data frame if there are results
if (length(all_results) > 0) {
  
  result_comment <- do.call(rbind, lapply(all_results, function(x) x$blood_pressure_query_all))
  combined_results <- do.call(rbind, lapply(all_results, function(x) x$BloodPressureQuery_Export))
  
  BloodPressureQuery_Export <- as.data.frame (combined_results)
  BloodPressureQuery_Comment <- as.data.frame (result_comment)
  
  
  if (site == "Zambia") {
    # Define the file path
    file_path <- paste0(path_to_save, "/Blood Pressure.xlsx")
    # Save the raw_bp_data DataFrame to the specified file path in Excel format
    write.xlsx(BloodPressureQuery_Comment, file = file_path, overwrite = TRUE) }
  
  # Print summary
  print("Blood Pressure Query Exported Successfully")
} else {
  print("No Blood Pressure Query Found")
}
  


#*****************************************************************************
#* Remove all emply dataframe 
#* this means that the only data frames left are the ones with true queries   
#*****************************************************************************
# 
## create a function that returns a logical value
isEmpty <- function(x) {
  is.data.frame(x) && nrow(x) == 0L
}
## apply it over the environment
empty <- unlist(eapply(.GlobalEnv, isEmpty))

## remove the empties
rm(list = names(empty)[empty])

## export 
## bind forms 
out <- rbindlist(mget(ls(pattern = "*Query_Export$")), use.names = TRUE) 

# combine form/edit type var -- will have to do for each of forms that have duplicates 
out <- add_column(out, Form_Edit_Type=paste(out$Form,"_",out$EditType))

# confirm date class 
out <- out %>% 
  mutate(VisitDate = ymd(parse_date_time(VisitDate, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y")))) 

OutRangeCheck_query <- out

## assign queryid -- 
# edit type id for out of range is 05 
# edit type id for invalid response option is 09

OutRangeCheck_query <- OutRangeCheck_query %>% 
  mutate(QueryID = ifelse(EditType == "Invalid Response Option", paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "05"), 
                          ifelse(EditType == "Out of Range", paste0(Form, "_", VisitDate, "_",MomID, "_", `Variable Name`, "_", `Variable Value`, "_", "09"),
                                 ifelse(EditType == "Date Out of Range" & !is.na(MomID), paste0(Form, "_", VisitDate, "_",MomID, "_", `Variable Name`, "_", `Variable Value`, "_", "09"),     
                                        ifelse(EditType == "Date Out of Range" & is.na(MomID), paste0(Form, "_", VisitDate, "_",ScrnID, "_", `Variable Name`, "_", `Variable Value`, "_", "09"), 
                                               ifelse ( grepl("pressure|BP", EditType, ignore.case = TRUE),  paste0(Form, "_", VisitDate, "_",MomID, "_", `Variable Name`, "_", `Variable Value`, "_", "09"), NA) 
                                        )))))

# export out of range queries
save(OutRangeCheck_query, file = paste0(path_to_save,"/OutRangeCheck_query.rda"))

