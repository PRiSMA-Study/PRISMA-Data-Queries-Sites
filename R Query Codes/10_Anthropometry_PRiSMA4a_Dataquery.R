#*****************************************************************************
#*QUERY #UNKNOWN. -- CHECK FOR OUT OF RANGE ANTHROPOMETRIC VALUES 
#* Written by: Precious Williams
#* Date Created:  29 February 2024
#* Last updated: 5 May 2024
#* Updates (Recent):Added Z_score 
#* Updates (Past): Reconstructed code to include edit messages 

#*Input: Wide data 
#*Function: check for any out of range values  
#*Output: .rda file with all out of range values 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 

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
library(tidyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(anthro)

## UPDATE EACH RUN ## 
# 1. Update "UploadDate" (this should match the folder name in synapse)
# 2. Set "site" variable to the site you are running the query for 
UploadDate = "2024-05-03"
site = "Ghana"

# 3. Set your main directory 
maindir <- paste0("~/PRiSMAv2Data/", site,"/", UploadDate,"/queries/", sep = "")


if (exists("mnh05")) {
  
# Clean data to remove cases with Maternal Death, Incomplete Visits and Unscheduled Visits
mnh05_filter <- mnh05 %>%
  filter(MAT_VISIT_MNH05 %in% c(1, 2) &
           MAT_VITAL_MNH05 == 1 &
           (TYPE_VISIT %in% c(1:5)) &
           WEIGHT_PEPERF == 1) %>%
  select(MOMID, PREGID, TYPE_VISIT, WEIGHT_PERES, WEIGHT_PEPERF)

# Replace -7 and -5 with NA
mnh05_filter[mnh05_filter %in% c(-7, -5)] <- NA

# Sort the dataframe by 'MOMID', 'PREGID', and 'TYPE_VISIT'
mnh05_filter <- mnh05_filter %>%
  arrange(MOMID, PREGID, TYPE_VISIT)

# Pivot dataframe wider
wide_df <- pivot_wider(mnh05_filter, names_from = TYPE_VISIT, values_from = WEIGHT_PERES, id_cols = c(MOMID, PREGID))

for (i in 1:5) {
  # Define the name of the variable
  variable_name <- as.character(i)
  # Check if the variable exists
  if (!(variable_name %in% colnames(wide_df))) {
    # If not, create it with NA values
    wide_df[[variable_name]] <- NA
  }
}

# Calculate weight differences during ANC period
weight_difference <- wide_df %>%
  mutate(weight_diff_1_2 = ifelse(!is.na(`1`) & !is.na(`2`), `2` - `1`, NA),
         weight_diff_2_3 = ifelse(!is.na(`2`) & !is.na(`3`), `3` - `2`, NA),
         weight_diff_3_4 = ifelse(!is.na(`3`) & !is.na(`4`), `4` - `3`, NA),
         weight_diff_4_5 = ifelse(!is.na(`4`) & !is.na(`5`), `5` - `4`, NA)) %>%
  select(MOMID, PREGID, starts_with("weight_diff_"))

# Transform wide dataframe into long format and filter weight differences
weight_difference_long <- weight_difference %>%
  pivot_longer(cols = starts_with("weight_diff_"),
               names_to = "Period",
               values_to = "Weight_diff") %>%
  filter(Weight_diff > 15 | Weight_diff < -10)

# Create queries based on weight difference periods
weight_difference_query <- weight_difference_long %>%
  mutate(EDIT_TYPE = case_when(
    Period == "weight_diff_1_2" ~ paste("Confirm maternal weight for Visit 1 and 2", 
                                        ifelse(Weight_diff > 0, " (High)", 
                                               ifelse(Weight_diff < 0, " (Low)", ""))),
    Period == "weight_diff_2_3" ~ paste("Confirm maternal weight for Visit 2 and 3", 
                                        ifelse(Weight_diff > 0, " (High)", 
                                               ifelse(Weight_diff < 0, " (Low)", ""))),
    Period == "weight_diff_3_4" ~ paste("Confirm maternal weight for Visit 3 and 4", 
                                        ifelse(Weight_diff > 0, " (High)", 
                                               ifelse(Weight_diff < 0, " (Low)", ""))),
    Period == "weight_diff_4_5" ~ paste("Confirm maternal weight for Visit 4 and 5", 
                                        ifelse(Weight_diff > 0, " (High)", 
                                               ifelse(Weight_diff < 0, " (Low)", ""))),
    TRUE ~ "NA_character_"
  ))

if (nrow(weight_difference_query) > 0) {
  
  # Prepare data for export
  MaternalAnthropometry_query_export <- weight_difference_query %>%
    mutate(SCRNID = NA,
           INFANTID = NA,
           TYPE_VISIT = NA,
           VISITDATE = NA,
           FORM = "MNH05",
           VARIABLENAME = "Constructed Weight Difference",
           VARIABLEVALUE = Weight_diff,
           FIELD_TYPE = "Number") %>%
    select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE, FIELD_TYPE, EDIT_TYPE) 
  
  names(MaternalAnthropometry_query_export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
  
  # Add additional columns
  MaternalAnthropometry_query_export <- cbind(QueryID = NA,
                                              UploadDate = format(Sys.time(), "%Y-%m-%d"),
                                              MaternalAnthropometry_query_export,
                                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  # combine form/edit type var 
  MaternalAnthropometry_query_export$Form_Edit_Type <- paste(MaternalAnthropometry_query_export$Form,"_",MaternalAnthropometry_query_export$EditType)
  
  ## assign queryid -- edit type id for invalid visit types is 06 
  MaternalAnthropometry_query <- MaternalAnthropometry_query_export %>% mutate(QueryID = paste0(Form, "_", MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "10")) %>% mutate_all(as.character)

  
  # Export variable checking query
  save(MaternalAnthropometry_query, file = paste0(maindir, "/MaternalAnthropometry_query.rda"))
  
  print("Maternal Anthropometry Query ran and saved successfully")
  
} else { print("No maternal weight discrepancies in MNH05")}

} else { print("MNH05 not present to complete maternal anthropometry query ")}

#*****************************************************************************
#*Infant Weight Query 
#*****************************************************************************

if (exists("mnh13") & exists("mnh11")) {
 
   #Clean data to remove cases with Infant Death, Incomplete Visits and Unscheduled Visits
  mnh11_filter <- mnh11 %>% 
    mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           TYPE_VISIT = 6,
           WEIGHT_PERES = BW_FAORRES) %>% 
    filter(INF_VISIT_MNH11 %in% c(1, 2, 3) & # Filter rows where INF_VISIT_MNH13 is 1 or 2
             INF_VITAL_MNH11 == 1) %>% # Filter rows where INF_VITAL_MNH13 is 1
    arrange(MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_OBSSTDAT) %>% # Arrange rows by INFANTID, TYPE_VISIT, and VISIT_OBSSTDAT
    distinct(MOMID, PREGID, INFANTID, TYPE_VISIT, .keep_all = TRUE) %>% # Keep distinct rows based on INFANTID and TYPE_VISIT while retaining all columns
    select(MOMID, PREGID, INFANTID, TYPE_VISIT, WEIGHT_PERES)  %>% # Select specified columns
    filter(!(is.na(WEIGHT_PERES)))
  
  mnh11_filter$WEIGHT_PERES[mnh11_filter$WEIGHT_PERES %in% c(-7, -5)] <- NA
  
  
  #Clean data to remove cases with Infant Death, Incomplete Visits and Unscheduled Visits
  mnh13_filter <- mnh13 %>% 
    mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% # Convert VISIT_OBSSTDAT to date format
    filter(INF_VISIT_MNH13 %in% c(1, 2) & # Filter rows where INF_VISIT_MNH13 is 1 or 2
             INF_VITAL_MNH13 == 1 & # Filter rows where INF_VITAL_MNH13 is 1
             TYPE_VISIT %in% c(7:11)) %>% # Filter rows where TYPE_VISIT is between 7 and 12
    arrange(MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_OBSSTDAT) %>% # Arrange rows by INFANTID, TYPE_VISIT, and VISIT_OBSSTDAT
    distinct(MOMID, PREGID, INFANTID, TYPE_VISIT, .keep_all = TRUE) %>% # Keep distinct rows based on INFANTID and TYPE_VISIT while retaining all columns
    select(MOMID, PREGID, INFANTID, TYPE_VISIT, WEIGHT_PERES)  %>% # Select specified columns
    filter(!(is.na(WEIGHT_PERES)))
  
  mnh13_filter$WEIGHT_PERES[mnh13_filter$WEIGHT_PERES %in% c(-7, -5)] <- NA
  
  long_inf_weight <- bind_rows(mnh11_filter, mnh13_filter)
  
  # Pivot dataframe wider
  wide_inf_weight <- pivot_wider(long_inf_weight, names_from = TYPE_VISIT, values_from = WEIGHT_PERES, id_cols = c(MOMID, PREGID, INFANTID))
  
  # Loop through variables 7 to 11
  for (i in 6:12) {
    # Define the name of the variable
    variable_name <- as.character(i)
    # Check if the variable exists
    if (!(variable_name %in% colnames(wide_inf_weight))) {
      # If not, create it with NA values
      wide_inf_weight[[variable_name]] <- NA
    }
  }
  # Calculate weight differences during ANC period
  inf_weight_difference <- wide_inf_weight %>%
    mutate(weight_diff_6_7 = ifelse(!is.na(`6`) & !is.na(`7`), `7` - `6`, NA),
           weight_diff_7_8 = ifelse(!is.na(`7`) & !is.na(`8`), `8` - `7`, NA),
           weight_diff_8_9 = ifelse(!is.na(`8`) & !is.na(`9`), `9` - `8`, NA),
           weight_diff_9_10 = ifelse(!is.na(`9`) & !is.na(`10`), `10` - `9`, NA),
           weight_diff_10_11 = ifelse(!is.na(`10`) & !is.na(`11`), `11` - `10`, NA),
           weight_diff_11_12 = ifelse(!is.na(`11`) & !is.na(`12`), `12` - `11`, NA)) %>%
    select(MOMID, PREGID, INFANTID, starts_with("weight_diff_"))
  
  # Transform wide dataframe into long format and filter weight differences
  inf_weight_difference_long <- inf_weight_difference %>%
    pivot_longer(cols = starts_with("weight_diff_"),
                 names_to = "Period",
                 values_to = "Weight_diff") %>%
    mutate(
      Query = case_when(
          (Period == "weight_diff_6_7" & (Weight_diff > 500 | Weight_diff < -300)) |
          (Period == "weight_diff_7_8" & (Weight_diff > 500 | Weight_diff < -300)) |
          (Period == "weight_diff_8_9" & (Weight_diff > 2000 | Weight_diff < -1000)) |
          (Period == "weight_diff_9_10" & (Weight_diff > 2000 | Weight_diff < -1000)) |
          (Period == "weight_diff_10_11" & (Weight_diff > 5000 | Weight_diff < -3000)) |
          (Period == "weight_diff_11_12" & (Weight_diff > 7000 | Weight_diff < -5000)) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(Query == TRUE)
  
  # Create queries based on weight difference periods
  inf_weight_difference_query <- inf_weight_difference_long %>%
    mutate(EDIT_TYPE = case_when(
      Period == "weight_diff_6_7" ~ paste("Confirm infant weight for IPC Visit and Visit 7", 
                                          ifelse(Weight_diff > 0, " (High)", 
                                                 ifelse(Weight_diff < 0, " (Low)", ""))),
      Period == "weight_diff_7_8" ~ paste("Confirm infant weight for Visit 7 and 8", 
                                          ifelse(Weight_diff > 0, " (High)", 
                                                 ifelse(Weight_diff < 0, " (Low)", ""))),
      Period == "weight_diff_8_9" ~ paste("Confirm infant weight for Visit 8 and 9", 
                                          ifelse(Weight_diff > 0, " (High)", 
                                                 ifelse(Weight_diff < 0, " (Low)", ""))),
      Period == "weight_diff_9_10" ~ paste("Confirm infant weight for Visit 9 and 10", 
                                           ifelse(Weight_diff > 0, " (High)", 
                                                  ifelse(Weight_diff < 0, " (Low)", ""))),
      Period == "weight_diff_10_11" ~ paste("Confirm infant weight for Visit 10 and 11", 
                                            ifelse(Weight_diff > 0, " (High)", 
                                                   ifelse(Weight_diff < 0, " (Low)", ""))),
      Period == "weight_diff_11_12" ~ paste("Confirm infant weight for Visit 11 and 12", 
                                            ifelse(Weight_diff > 0, " (High)", 
                                                   ifelse(Weight_diff < 0, " (Low)", ""))),
      TRUE ~ NA_character_
    ))
  
  
  if (nrow(inf_weight_difference_query) > 0) {
    
    # Prepare data for export
    InfantAnthropometry_query_export <- inf_weight_difference_query %>%
      mutate(SCRNID = NA,
             TYPE_VISIT = NA,
             VISITDATE = NA,
             FORM = "MNH13",
             VARIABLENAME = "Constructed Weight Difference",
             VARIABLEVALUE = Weight_diff,
             FIELD_TYPE = "Number") %>%
      select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE, FIELD_TYPE, EDIT_TYPE) 
    
    names(InfantAnthropometry_query_export) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
    
    # Add additional columns
    InfantAnthropometry_query_export <- cbind(QueryID = NA,
                                              UploadDate = format(Sys.time(), "%Y-%m-%d"),
                                              InfantAnthropometry_query_export,
                                              DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    # combine form/edit type var 
    InfantAnthropometry_query_export$Form_Edit_Type <- paste(InfantAnthropometry_query_export$Form,"_",InfantAnthropometry_query_export$EditType)
    
    ## assign queryid -- edit type id for invalid weight types is 10
    InfantAnthropometry_query_export <- InfantAnthropometry_query_export %>% mutate(QueryID = paste0(Form, "_", MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "11")) %>% mutate_all(as.character)

    print("Infant Weight Anthropometry Query ran and saved successfully")
    
  } else {print("No infant weight discrepancies in MNH11 & MNH13") }
  
}  else {print("Forms (MNH11 & MNH13) required to complete Infant Weigth Query is Unavailable ") }
  
  #*****************************************************************************
  #*Infant Length Query 
  #*****************************************************************************
  if (exists("mnh13")) {
    
  #Clean data to remove cases with Infant Death, Incomplete Visits and Unscheduled Visits
  length_df <- mnh13 %>% 
    mutate(VISIT_OBSSTDAT = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y")))) %>% # Convert VISIT_OBSSTDAT to date format
    filter(INF_VISIT_MNH13 %in% c(1, 2) & # Filter rows where INF_VISIT_MNH13 is 1 or 2
           INF_VITAL_MNH13 == 1 & # Filter rows where INF_VITAL_MNH13 is 1
           TYPE_VISIT %in% c(9:12)) %>% # Filter rows where TYPE_VISIT is between 7 and 12
    arrange(MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_OBSSTDAT) %>% # Arrange rows by INFANTID, TYPE_VISIT, and VISIT_OBSSTDAT
    distinct(MOMID, PREGID, INFANTID, TYPE_VISIT, .keep_all = TRUE) %>% # Keep distinct rows based on INFANTID and TYPE_VISIT while retaining all columns
    select(MOMID, PREGID, INFANTID, TYPE_VISIT,LENGTH_PERES_1 , LENGTH_PERES_2, LENGTH_PERES_3) 

  if (nrow(length_df) > 0) {
    
  length_df$LENGTH_PERES_1[length_df$LENGTH_PERES_1 %in% c(-7, -5)] <- NA
  length_df$LENGTH_PERES_2[length_df$LENGTH_PERES_2 %in% c(-7, -5)] <- NA
  length_df$LENGTH_PERES_3[length_df$LENGTH_PERES_3 %in% c(-7, -5)] <- NA
  
  #* Query 1: Is Infant Length decreasing visit by visit?
  avg_length_df <- length_df %>%
    mutate(LENGTH_PERES = floor(rowMeans(select(., c(LENGTH_PERES_1, LENGTH_PERES_2)), na.rm = TRUE) * 100) / 100) %>%
    select(MOMID, PREGID, INFANTID, TYPE_VISIT, LENGTH_PERES)
  
  
  # Pivot dataframe wider
  wide_length <- pivot_wider(avg_length_df, names_from = TYPE_VISIT, values_from = LENGTH_PERES, id_cols = c(MOMID, PREGID, INFANTID))
  
  for (i in 9:12) {
    # Define the name of the variable
    variable_name <- as.character(i)
    # Check if the variable exists
    if (!(variable_name %in% colnames(wide_length))) {
      # If not, create it with NA values
      wide_length[[variable_name]] <- NA
    }
  }
  
  length_difference <- wide_length %>%
    # Calculate length differences during required PNC period
    mutate( length_diff_9_10 = ifelse(!is.na(`9`) & !is.na(`10`), `10` - `9`, NA),
            length_diff_10_11 = ifelse(!is.na(`10`) & !is.na(`11`), `11` - `10`, NA),
            length_diff_11_12 = ifelse(!is.na(`11`) & !is.na(`12`), `12` - `11`, NA)) %>%
    select(MOMID, PREGID, INFANTID, starts_with("length_diff"))
  
  # Transform wide dataframe into long format and filter length differences
  length_difference_long <- length_difference %>%
    pivot_longer(cols = starts_with("length_diff_"),
                 names_to = "Period",
                 values_to = "length_diff") %>%
    filter(length_diff < -0.5 )
  
  # Create queries based on length difference 
  inf_length_difference <- length_difference_long %>%
    mutate(EDIT_TYPE = case_when(
      Period == "length_diff_9_10" ~ "Decreasing infant length between Visit 9 and 10",
      Period == "length_diff_10_11" ~ "Decreasing infant length between  Visit 10 and 11",
      Period == "length_diff_11_12" ~ "Decreasing infant length between  Visit 11 and 12",
      TRUE ~ NA_character_),
      VARIABLEVALUE = length_diff) %>%
    select(MOMID, PREGID, INFANTID, Period, EDIT_TYPE, VARIABLEVALUE)
  
  inf_length_difference_query <- inf_length_difference
  
  #* Query 2: Is the two length measurements for specific TYPE_VIST far apart from each other?
  Visit_length_df <- length_df %>%
    mutate(Difference = ifelse(!is.na(LENGTH_PERES_1) & !is.na(LENGTH_PERES_2),(LENGTH_PERES_1 - LENGTH_PERES_2), NA),
           EDIT_TYPE = case_when (Difference >= 0.5 & LENGTH_PERES_3 == NA ~ "Length difference exceeds 0.5cm", 
                                  TRUE ~ NA),
           VARIABLEVALUE = Difference,
           Period = NA) %>%
    filter(!is.na(EDIT_TYPE))  %>%
    select(MOMID, PREGID, INFANTID, Period, EDIT_TYPE, VARIABLEVALUE)
  
  if (nrow(Visit_length_df) > 0) {
    
    inf_length_difference_query <- bind_rows(inf_length_difference_query, Visit_length_df)
    
  } else { print("No infant length discrepancies between length 1 and 2 measurements") }
  
  if (nrow(inf_length_difference_query) > 0) {
    
    # Prepare data for export
    InfantLength_query <- inf_length_difference_query %>%
      mutate(SCRNID = NA,
             TYPE_VISIT = NA,
             VISITDATE = NA,
             FORM = "MNH13",
             VARIABLENAME = "Constructed Length Difference",
             FIELD_TYPE = "Number") %>%
      select(SCRNID, MOMID, PREGID, INFANTID, TYPE_VISIT, VISITDATE, FORM, VARIABLENAME, VARIABLEVALUE, FIELD_TYPE, EDIT_TYPE) 
    
    names(InfantLength_query) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
    
    # Add additional columns
    InfantLength_query_export <- cbind(QueryID = NA,
                                       UploadDate = format(Sys.time(), "%Y-%m-%d"),
                                       InfantLength_query,
                                       DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    # combine form/edit type var 
    InfantLength_query_export$Form_Edit_Type <- paste(InfantLength_query_export$Form,"_",InfantLength_query_export$EditType)
    
    ## assign queryid -- edit type id for invalid length types is 10
    InfantLength_query_export <- InfantLength_query_export %>% mutate(QueryID = paste0(Form, "_", MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "11")) %>% mutate_all(as.character)
    
    print("Infant Length Anthropometry Query ran and saved successfully") 
    
} else { print ("no infant length (MNH13) discrepancies in dataset") }
  
} else { print ("insufficient dataset to complete query") }
  
} else { print ("no infant POC (MNH13) form present") }  


if (exists("mnh09")== TRUE & exists("mnh13")== TRUE){
  
  mnh09_sub <- mnh09 %>%
  select(MOMID, PREGID, INFANTID_INF1, INFANTID_INF2, INFANTID_INF3, INFANTID_INF4, DELIV_DSSTDAT_INF1,
         DELIV_DSSTDAT_INF2, DELIV_DSSTDAT_INF3, DELIV_DSSTDAT_INF4,DELIV_DSSTTIM_INF1, DELIV_DSSTTIM_INF2, 
         DELIV_DSSTTIM_INF3, DELIV_DSSTTIM_INF4, BIRTH_DSTERM_INF1,BIRTH_DSTERM_INF2, BIRTH_DSTERM_INF3,
         BIRTH_DSTERM_INF4, SEX_INF1, SEX_INF2, SEX_INF3, SEX_INF4) %>% 
    mutate(DELIV_DSSTDAT_INF1 = ymd(parse_date_time(DELIV_DSSTDAT_INF1, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF2 = ymd(parse_date_time(DELIV_DSSTDAT_INF2, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF3 = ymd(parse_date_time(DELIV_DSSTDAT_INF3, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           DELIV_DSSTDAT_INF4 = ymd(parse_date_time(DELIV_DSSTDAT_INF4, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
           SEX_INF1 = as.numeric(SEX_INF1), SEX_INF2 = as.numeric(SEX_INF2), SEX_INF3 = as.numeric(SEX_INF3), SEX_INF4 = as.numeric(SEX_INF4)
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
  # Convert time to time format
  mutate(
    DELIV_DSSTTIM_INF1 = if_else(!is.na(DELIV_DSSTTIM_INF1), as.ITime(DELIV_DSSTTIM_INF1), NA),
    DELIV_DSSTTIM_INF2 = if_else(!is.na(DELIV_DSSTTIM_INF2), as.ITime(DELIV_DSSTTIM_INF2), NA),
    DELIV_DSSTTIM_INF3 = if_else(!is.na(DELIV_DSSTTIM_INF3), as.ITime(DELIV_DSSTTIM_INF3), NA),
    DELIV_DSSTTIM_INF4 = if_else(!is.na(DELIV_DSSTTIM_INF4), as.ITime(DELIV_DSSTTIM_INF4), NA)
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
                                     DELIV_DSSTDAT_INF4)) 
 
# Getting the Date of Birth, Sex and Birth Outcome for Each ID
Infant_DOB <- mnh09_sub %>%
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
  filter(INFANTID != "" & INFANTID != "n/a")

#Living infants
Infant_live <- Infant_DOB %>% filter (BIRTH_DSTERM == 1)

if (nrow(Infant_live) > 0) {
  
#We want to look at visits where the 
sub_mnh13 <- mnh13 %>% 
  filter(TYPE_VISIT %in% c(7, 8, 9, 10, 11, 12) & INF_VITAL_MNH13 == 1 & INF_VISIT_MNH13 %in% c(1, 2, 3))

sub_mnh13$WEIGHT_PERES[sub_mnh13$WEIGHT_PERES %in% c(-7, -5)] <- NA
sub_mnh13$LENGTH_PERES_1[sub_mnh13$LENGTH_PERES_1 %in% c(-7, -5)] <- NA
sub_mnh13$LENGTH_PERES_2[sub_mnh13$LENGTH_PERES_2 %in% c(-7, -5)] <- NA
sub_mnh13$LENGTH_PERES_3[sub_mnh13$LENGTH_PERES_3 %in% c(-7, -5)] <- NA

sub_mnh13$LENGTH_PERES_1 <- as.numeric(sub_mnh13$LENGTH_PERES_1)
sub_mnh13$LENGTH_PERES_2 <- as.numeric(sub_mnh13$LENGTH_PERES_2)
sub_mnh13$LENGTH_PERES_3 <- as.numeric(sub_mnh13$LENGTH_PERES_3)

# Join sub_mnh13 with Infant_live
Measure_df <- left_join(sub_mnh13, Infant_live, by = c("INFANTID", "MOMID", "PREGID")) %>% 
  mutate(
    Visit_Date = ymd(parse_date_time(VISIT_OBSSTDAT, order = c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d", "%d-%b-%y", "%d-%m-%y"))),
    age_in_days = round(as.numeric(difftime(Visit_Date, DOB, units = "days")), 0),
    weight = round(WEIGHT_PERES / 1000, 2),
    lenhei =  case_when(abs(LENGTH_PERES_1 - LENGTH_PERES_2) > 0.5 & !is.na(LENGTH_PERES_3) ~ rowMeans(select(., c(LENGTH_PERES_1, LENGTH_PERES_2, LENGTH_PERES_3 )), na.rm = TRUE),
                        !is.na(LENGTH_PERES_1) & !is.na (LENGTH_PERES_2) ~  round((LENGTH_PERES_1 + LENGTH_PERES_2) / 2, 2),
                        !is.na(LENGTH_PERES_1) &  is.na (LENGTH_PERES_2) ~  LENGTH_PERES_1,
                        TRUE ~ NA),
    Visit_Type = TYPE_VISIT,
    sex = case_when(
      toupper(SEX) %in% c("M", "1") ~ 1,
      toupper(SEX) %in% c("F", "2") ~ 2,
      TRUE ~ NA_integer_)
  ) %>% 
  select(MOMID, PREGID, INFANTID, sex, Visit_Date, Visit_Type, DOB, age_in_days, weight, lenhei)

# Z_Weight <- Measure_df %>% filter (!is.na(weight)) 
# Z_Length <- Measure_df %>% filter (!is.na(lenhei))

Z_weight_Length <- Measure_df %>% filter (!is.na(lenhei) & !is.na(weight) & age_in_days > 0) %>%
  mutate(with( ., anthro_zscores(sex = sex, age = age_in_days, 
                                 weight = weight, lenhei = lenhei))) %>%
  select(MOMID, PREGID, INFANTID, sex, Visit_Date, Visit_Type, DOB, age_in_days, weight, lenhei, 
         clenhei, cbmi, cmeasure, csex, zlen, flen, zwei, fwei, zwfl, fwfl) 

# Filter based on z-scores
Filtered_Z_Data <- Z_weight_Length %>%
  filter(abs(zwfl) >= 5 | abs(zwei) >= 5 | abs(zlen) >= 5)

Long_Query_Data <- Filtered_Z_Data %>%
  pivot_longer(cols = starts_with("z"), names_to = "Measure", values_to = "Z_score") %>%
  filter(abs(Z_score) >= 5)

if (nrow(Long_Query_Data) > 0) {
  
  Inf_Grwth_Query <- Long_Query_Data %>%
    mutate(EDIT_TYPE = case_when(  abs(fwfl) == 1 & abs(fwei) == 1 & abs(flen) == 1 ~ "All inf growth measures out of range",
                                   Measure == "zwfl" & abs(Z_score) >= 5 ~ "Inf Length for Weight Out of Range",
                                   Measure == "zwei" & abs(Z_score) >= 5 ~ "Inf Weight for Age Out of Range",
                                   Measure == "zlen" & abs(Z_score) >= 5 ~ "Inf Length for Age Out of Range",
                                   TRUE ~ NA_character_  # Assign NA if none of the conditions are met
    ),
    SCRNID = NA,
    FORM = "MNH13",
    VARIABLENAME = case_when (Measure == "zwfl" ~ "Constructed Z Score (WFL)",
                              Measure == "zwei" ~ "Constructed Z score (WAZ)",
                              Measure == "zlen" ~ "Constructed Z score (LAZ)",
                              TRUE ~ NA_character_ ),
    VARIABLEVALUE = Z_score,
    FIELD_TYPE = "Number") 
  
  InfGrwth_query <- Inf_Grwth_Query %>%
    mutate_all(as.character) %>%
    select(SCRNID, MOMID, PREGID, INFANTID, Visit_Type, Visit_Date, FORM, VARIABLENAME, VARIABLEVALUE, FIELD_TYPE, EDIT_TYPE)
  
  names(InfGrwth_query) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
  
  # Add additional columns
  InfantGrowth_query_export <- cbind(QueryID = NA,
                                     UploadDate = format(Sys.time(), "%Y-%m-%d"),
                                     InfGrwth_query,
                                     DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  # combine form/edit type var 
  InfantGrowth_query_export$Form_Edit_Type <- paste(InfantGrowth_query_export$Form,"_",InfantGrowth_query_export$EditType)
 
   ## assign queryid -- edit type id for out of range is 10
  InfantGrowth_query_export <- InfantGrowth_query_export %>% mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "10")) %>% mutate_all(as.character)

  
  #Extract QueryID
  QueryID <-  InfantGrowth_query_export  %>% select (QueryID, MomID, PregID, InfantID, Visit_Type = VisitType, Measure = "Variable Name") %>% mutate_all(as.character)
  
  #Create an extratab to include more information for sites 
  InfantGrowth <- Long_Query_Data %>% 
    select(MomID = MOMID, PregID = PREGID, InfantID = INFANTID, SEX = sex, Visit_Date, Visit_Type, `Date of Birth` = DOB, Age_in_days = age_in_days, 
            "Weight (kg)" = weight, "Length (cm)" = lenhei, Measure, Z_score) %>% 
    mutate(Measure = case_when(
      Measure == "zwfl" ~ "Constructed Z Score (WFL)",
      Measure == "zwei" ~ "Constructed Z score (WAZ)",
      Measure == "zlen" ~ "Constructed Z score (LAZ)",
      TRUE ~ NA_character_
    )) %>% mutate_all(as.character)

  
  InfantGrowth_query_comments <- left_join(InfantGrowth, QueryID, by = c("MomID", "PregID", "InfantID", "Visit_Type", "Measure"))
  save(InfantGrowth_query_comments, file = paste0(maindir, "/InfantGrowth_query_comments.rda"))
  
  
  print ("Infant Growth Anthropometry Query ran and saved successfully") 
  
} else {print ("No infant growth velocity discrepancy") }

} else {print("No data for any living infant") }
 
} else {print("SITE does not have all forms required to run Infant Growth - don't run.") }

#Bind Weight and Length Anthropometry Query
bind_dataframes <- function(dfs_list) {
  valid_dfs <- list()
  for (df_name in dfs_list) {
    if (exists(df_name)) {
      valid_dfs[[length(valid_dfs) + 1]] <- get(df_name)
    }
  }
  if (length(valid_dfs) > 1) {
    bind <- do.call(rbind, valid_dfs)
    return(bind)
  } else if (length(valid_dfs) == 1) {
    bind <- as.data.frame(valid_dfs) 
    return(bind)
  } else if (length(valid_dfs) == 0) {
    message("No valid dataframes found to bind.")
    return(NULL)
  }
}

dfs_list <- (c("InfantAnthropometry_query_export", "InfantLength_query_export", "InfantGrowth_query_export"))

# Apply the function to create InfantAnthropometry_query
InfantAnthropometry_query <- bind_dataframes(dfs_list)

#Export to query folder
if (exists("InfantAnthropometry_query") && is.data.frame(InfantAnthropometry_query)) {
  
    save(InfantAnthropometry_query, file = paste0(maindir, "/InfantAnthropometry_query.rda"))
  
    print("Infant Anthropometry Query ran and saved successfully")
    
} else {print ("No Infant Anthropometry Query")}
