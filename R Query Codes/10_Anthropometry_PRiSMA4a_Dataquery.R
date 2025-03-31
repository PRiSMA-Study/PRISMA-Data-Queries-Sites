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

# UPDATE EACH RUN:
site = "Kenya"

# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: load in the WIDE data we generated from 00_DataImport code 
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_wide.Rdata", sep = "")) 

# UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

if (exists("mnh05")) {
  
  # Clean data to remove cases with Maternal Death, Incomplete Visits and Unscheduled Visits
  mnh05_filter <- mnh05 %>%
    filter(MAT_VISIT_MNH05 %in% c(1, 2) &
             MAT_VITAL_MNH05 == 1 &
             (TYPE_VISIT %in% c(1:5)) &
             WEIGHT_PEPERF == 1) %>%
    select(MOMID, PREGID, TYPE_VISIT, WEIGHT_PERES, WEIGHT_PEPERF)
  
  # Replace -7 and -5 with NA
  mnh05_filter[mnh05_filter %in% c(-7, -5, ".")] <- NA
  
  # Sort the dataframe by 'MOMID', 'PREGID', and 'TYPE_VISIT'
  mnh05_filter <- mnh05_filter %>%
    arrange(MOMID, PREGID, TYPE_VISIT) %>%  # Ensure ordering if needed
    distinct(MOMID, PREGID, TYPE_VISIT, .keep_all = TRUE)
  
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
    
    # Extract visit identifiers from Period
    weight_visit_df <- weight_difference_query %>%
      mutate(
        VISIT_A = paste0(gsub(".*_(\\d+)_.*", "\\1", Period)),
        VISIT_B = paste0(gsub(".*_(\\d+)$", "\\1", Period))
      )
    
    # Filter sub_mnh13 based on MOMID-PREGID and visits in Period
    mat_fil_df <- mnh05_filter %>%
      select(MOMID, PREGID, TYPE_VISIT, WEIGHT_PERES) %>%
      right_join(weight_visit_df, by = c("MOMID", "PREGID")) %>%
      filter(TYPE_VISIT %in% c(VISIT_A, VISIT_B)) %>%
      rename(WEIGHT_DIFF = Weight_diff)
    
    # Separate data for Visit A and Visit B
    mat_visit_a <- mat_fil_df %>%
      filter(TYPE_VISIT == VISIT_A) %>%
      rename_with(~ paste0(.x, "_A"), starts_with("WEIGHT_PERES")) %>%
      rename ()
    
    mat_visit_b <- mat_fil_df %>%
      filter(TYPE_VISIT == VISIT_B) %>%
      rename_with(~ paste0(.x, "_B"), starts_with("WEIGHT_PERES"))
    
    # Combine Visit A and Visit B into a single row for each MOMID-PREGID
    mat_query_comment <- mat_visit_a %>%
      select(MOMID, PREGID, EDIT_TYPE, WEIGHT_DIFF, ends_with("_A")) %>%
      left_join(
        mat_visit_b %>% select(MOMID, PREGID, EDIT_TYPE, WEIGHT_DIFF, ends_with("_B")),
        by = c ("MOMID", "PREGID", "EDIT_TYPE", "WEIGHT_DIFF")) 
    
    MaternalAnthro_comment_export <- mat_query_comment %>%  
      mutate (FORM = "MNH05",
              VARIABLE = "WEIGHT_PERES",
              QueryID = paste0(FORM, "_", MOMID, "_", "Constructed Weight Difference", "_", WEIGHT_DIFF, "_", "10")) %>%
      select(QueryID, MOMID, PREGID, FORM, EDIT_TYPE, WEIGHT_DIFF, 
             starts_with("VISIT_"),starts_with("WEIGHT_PERES"))
    
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
    save(MaternalAnthropometry_query, file = paste0(path_to_save, "/MaternalAnthropometry_query.rda"))
    
    save(MaternalAnthro_comment_export, file = paste0(path_to_save, "/MaternalAnthro_comment_export.rda"))
    
    
    
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
    
    
    # Extract visit identifiers from Period
    inf_weight_visit_df <- inf_weight_difference_query %>%
      mutate(
        VISIT_A = paste0(gsub(".*_(\\d+)_.*", "\\1", Period)),
        VISIT_B = paste0(gsub(".*_(\\d+)$", "\\1", Period))
      )
    
    # Filter sub_mnh13 based on MOMID-PREGID and visits in Period
    inf_fil_df <- long_inf_weight %>%
      select(MOMID, PREGID, INFANTID, TYPE_VISIT, WEIGHT_PERES) %>%
      right_join(inf_weight_visit_df, by = c("MOMID", "PREGID", "INFANTID")) %>%
      filter(TYPE_VISIT %in% c(VISIT_A, VISIT_B)) %>%
      rename(WEIGHT_DIFF = Weight_diff)
    
    # Separate data for Visit A and Visit B
    inf_visit_a <- inf_fil_df %>%
      filter(TYPE_VISIT == VISIT_A) %>%
      rename_with(~ paste0(.x, "_A"), starts_with("WEIGHT_PERES")) %>%
      rename ()
    
    inf_visit_b <- inf_fil_df %>%
      filter(TYPE_VISIT == VISIT_B) %>%
      rename_with(~ paste0(.x, "_B"), starts_with("WEIGHT_PERES"))
    
    # Combine Visit A and Visit B into a single row for each MOMID-PREGID
    inf_query_comment <- inf_visit_a %>%
      select(MOMID, PREGID, INFANTID, EDIT_TYPE, WEIGHT_DIFF, ends_with("_A")) %>%
      left_join(
        inf_visit_b %>% select(MOMID, PREGID, INFANTID, EDIT_TYPE, WEIGHT_DIFF, ends_with("_B")),
        by = c ("MOMID", "PREGID", "INFANTID", "EDIT_TYPE", "WEIGHT_DIFF")) 
    
    InfWeight_comment_export <- inf_query_comment %>%  
      mutate (FORM = ifelse( VISIT_A == 6, "MNH11/MNH13", "MNH13"),
              VARIABLE =  ifelse( VISIT_A == 6, "BW_FAORRES/WEIGHT_PERES", "WEIGHT_PERES"),
              QueryID = paste0("MNH13", "_", MOMID, "_", "Constructed Weight Difference", "_", WEIGHT_DIFF, "_", "10")) %>%
      select(QueryID, MOMID, PREGID, INFANTID, FORM, VARIABLE, EDIT_TYPE, WEIGHT_DIFF, 
             starts_with("VISIT_"),starts_with("WEIGHT_PERES"))
    
    
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
    
    ##Save the infant weight comment as RDA file
    save(InfWeight_comment_export, file = paste0(path_to_save, "/InfWeight_comment_export.rda"))
        
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
      mutate(LENGTH_PERES = floor(rowMeans(select(., c(LENGTH_PERES_1, LENGTH_PERES_2, , LENGTH_PERES_3 )), na.rm = TRUE) * 100) / 100) %>%
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
      mutate(
        length_diff_9_10 = ifelse(!is.na(`9`) & !is.na(`10`), round(`10` - `9`, 2), NA),
        length_diff_10_11 = ifelse(!is.na(`10`) & !is.na(`11`), round(`11` - `10`, 2), NA),
        length_diff_11_12 = ifelse(!is.na(`11`) & !is.na(`12`), round(`12` - `11`, 2), NA)
      ) %>%
      # Select relevant columns
      select(MOMID, PREGID, INFANTID, starts_with("length_diff"))
    
    
    # Transform wide dataframe into long format and filter length differences
    length_difference_long <- length_difference %>%
      pivot_longer(cols = starts_with("length_diff_"),
                   names_to = "Period",
                   values_to = "length_diff") %>%
      filter(length_diff < -1 )
    
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
    
    # #* Query 2: Is the two length measurements for specific TYPE_VIST far apart from each other?
    # Visit_length_df <- length_df %>%
    #   mutate(Difference = ifelse(!is.na(LENGTH_PERES_1) & !is.na(LENGTH_PERES_2),(LENGTH_PERES_1 - LENGTH_PERES_2), NA),
    #          EDIT_TYPE = case_when (Difference >= 0.5 & LENGTH_PERES_3 == NA ~ "Length difference exceeds 0.5cm", 
    #                                 TRUE ~ NA),
    #          VARIABLEVALUE = Difference,
    #          Period = NA) %>%
    #   filter(!is.na(EDIT_TYPE))  %>%
    #   select(MOMID, PREGID, INFANTID, Period, EDIT_TYPE, VARIABLEVALUE)
    # 
    # if (nrow(Visit_length_df) > 0) {
    #   
    #   inf_length_difference_query <- bind_rows(inf_length_difference_query, Visit_length_df)
    #   
    #   
    # } else { print("No infant length discrepancies between length 1 and 2 measurements") }

    #Creating an extra tab for sites to be able to makke it easier for sites 
    #to send to their field team
    
    if (nrow(inf_length_difference_query) > 0) {
      
      # Extract visit identifiers from Period
      inf_length_visit_df <- inf_length_difference %>%
        mutate(
          VISIT_A = paste0(gsub(".*_(\\d+)_.*", "\\1", Period)),
          VISIT_B = paste0(gsub(".*_(\\d+)$", "\\1", Period))
        )
      
      # Filter sub_mnh13 based on INFANTID and visits in Period
      inf_length_df <- length_df %>%
        select (INFANTID, TYPE_VISIT, LENGTH_PERES_1, LENGTH_PERES_2, LENGTH_PERES_3) %>%
        right_join(inf_length_visit_df, by = "INFANTID") %>%
        filter(TYPE_VISIT %in% c(VISIT_A, VISIT_B)) %>%
        rename (LENGTH_DIFF = VARIABLEVALUE)
      
      # Separate data for Visit A and Visit B
      len_visit_a <- inf_length_df %>%
        filter(TYPE_VISIT == VISIT_A) %>%
        rename_with(~ paste0(.x, "_A"), starts_with("LENGTH_PERES")) %>%
        rename ()
      
      len_visit_b <- inf_length_df %>%
        filter(TYPE_VISIT == VISIT_B) %>%
        rename_with(~ paste0(.x, "_B"), starts_with("LENGTH_PERES"))
      
      # Combine Visit A and Visit B into a single row for each INFANTID
      inflength_query_comment <- len_visit_a %>%
        select(MOMID, PREGID, INFANTID, EDIT_TYPE, LENGTH_DIFF, ends_with("_A")) %>%
        left_join(
          len_visit_b %>% select(MOMID, PREGID, INFANTID, EDIT_TYPE, LENGTH_DIFF, ends_with("_B")),
          by = c ("MOMID", "PREGID", "INFANTID", "EDIT_TYPE", "LENGTH_DIFF")) 
      
      InfLength_comment_export <- inflength_query_comment %>% 
        mutate (FORM = "MNH13",
                VARIABLE = "LENGTH_PERES_1-3",
                QueryID = paste0("MNH13", "_", MOMID, "_", "Constructed Length Difference", "_", LENGTH_DIFF, "_", "10")) %>%
        select(QueryID, MOMID, PREGID, INFANTID, FORM, VARIABLE, EDIT_TYPE, LENGTH_DIFF, 
               starts_with("VISIT_"),starts_with("LENGTH_PERES"))
      
      save(InfLength_comment_export, file = paste0(path_to_save, "/InfLength_comment_export.rda"))
      
      
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
    filter(INFANTID != "")
  
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
      save(InfantGrowth_query_comments, file = paste0(path_to_save, "/InfantGrowth_query_comments.rda"))
      
      
      print ("Infant Growth Anthropometry Query ran and saved successfully") 
      
    } else {print ("No infant growth velocity discrepancy") }
    
  } else {print("No data for any living infant") }
  
} else {print("SITE does not have all forms required to run Infant Growth - don't run.") }

#Bind Weight and Length Anthropometry Query
bind_dataframes <- function(dfs_list) {
  
  valid_dfs <- list()  # Initialize an empty list to hold valid dataframes
  
  # Loop through the list of dataframe names
  for (df_name in dfs_list) {
    if (exists(df_name)) {  # Check if the dataframe exists in the environment
      valid_dfs[[length(valid_dfs) + 1]] <- get(df_name)  # Retrieve the dataframe and append it to the list
    }
  }
  
  # Check the number of valid dataframes found
  if (length(valid_dfs) > 1) {
    bind <- do.call(rbind, valid_dfs)  # Bind dataframes row-wise
    return(bind)
  } else if (length(valid_dfs) == 1) {
    bind <- valid_dfs[[1]]  # Return the single dataframe
    return(bind)
  } else {
    message("No valid dataframes found to bind.")  # Print a message if no valid dataframes are found
    return(NULL)  # Return NULL
  }
}

# List of dataframe names to bind
dfs_list <- c("InfantAnthropometry_query_export", "InfantLength_query_export", "InfantGrowth_query_export")

# Apply the function to create InfantAnthropometry_query
InfantAnthropometry_query <- bind_dataframes(dfs_list)

#Export to query folder
if (exists("InfantAnthropometry_query") && is.data.frame(InfantAnthropometry_query)) {
  
  save(InfantAnthropometry_query, file = paste0(path_to_save, "/InfantAnthropometry_query.rda"))
  
  print("Infant Anthropometry Query ran and saved successfully")
  
} else {print ("No Infant Anthropometry Query")}


#*****************************************************************************
#* BILI-RULER QUERY
#*****************************************************************************

if (exists("mnh36") == TRUE){
  
  # Query No 1: Quality checks for Monk Skin Tone and Bili-ruler measurements
  bili_mst_df <- mnh36 %>%
    # Filter for relevant visit types
    filter(INF_VISIT_MNH36 %in% c(1, 2)) %>%
    # Convert measurement variables to numeric to avoid type mismatches
    mutate(
      MST_1 = as.numeric(MST_1),
      MST_2 = as.numeric(MST_2),
      BILI_1 = as.numeric(BILI_1),
      BILI_2 = as.numeric(BILI_2),
      
      # Identify cases where only one measurement was taken despite two staff members being present
      measure_one = if_else(NUM_PRISMA_STAFF == 0 & MST_BILI_COMPL == 1, 1, 0),
      measure_two = if_else(NUM_PRISMA_STAFF == 1 & MST_BILI_COMPL == 1, 1, 0),
      
      # Calculate absolute differences in Monk Skin Tone and Bili-ruler measurements if two staff members were present
      mst_diff = if_else(measure_two == 1 & !(MST_1 %in% c(77,55)) & !(MST_2 %in% c(77,55)), abs(MST_1 - MST_2), NA_real_),
      bili_diff = if_else(measure_two == 1 & !(BILI_1 %in% c(77,55)) & !(BILI_2 %in% c(77,55)), abs(BILI_1 - BILI_2), NA_real_)
    )
  
  
  # Process MST queries
  mst_df <- bili_mst_df %>%
    mutate(
      
      # Check if no measurements were taken if one staff was present
      Q_MST_None_One_Staff = if_else(measure_one == 1 & (MST_1 %in% c(55, 77) ), "MST: Measurement is missing", NA_character_),
      Q_MST_None_Two_Staff = if_else(measure_two == 1  & MST_1 %in% c(55, 77) & MST_2 %in% c(55, 77), "MST: Both measurements are missing", NA_character_),
      
      # Check if only one measurement was taken when two staff members were present
      Q_MST_Single_Two_Staff = if_else(measure_two == 1 & (MST_1 %in% c(55, 77) | MST_2 %in% c(55, 77)), "MST: Single measurement with 2 staff", NA_character_),
      # Check if two measurements were taken when only one staff member was present
      Q_MST_Two_One_Staff = if_else(measure_one == 1 & (MST_1 %in% c(1:9) & MST_2 %in% c(1:9)), "MST: Two measurements with 1 staff", NA_character_),
      # Check if a third measurement is required based on large differences between first two measurements
      Q_MST_Third_Required = if_else(measure_two == 1 & mst_diff >= 3 & MST_JOINT %in% c(55, 77), "MST: Third measurement required", NA_character_),
      Q_MST_DIFF3_Invalid = if_else(measure_two == 1 & mst_diff >= 3 & MST_DIFF3 != 1, "MST_DIFF3: Invalid Data Entry", NA_character_)
    ) %>%
    pivot_longer(cols = starts_with("Q_MST_"), names_to = "Query_Type", values_to = "Query") %>%
    filter(!is.na(Query))
  
  # Process BILI queries
  bili_df <- bili_mst_df %>%
    mutate(
      # Check if no measurements were taken if one staff or if both staff were present and both are missing
      Q_BILI_None_One_Staff = if_else(measure_one == 1  & (BILI_1 %in% c(55, 77) ), "BILI: Measurement is missing", NA_character_),
      Q_BILI_None_Two_Staff = if_else(measure_two == 1  & BILI_1 %in% c(55, 77) & BILI_2 %in% c(55, 77), "BILI: Both measurements are missing", NA_character_),
      
      # Check if only one measurement was taken when two staff members were present
      Q_BILI_Single_Two_Staff = if_else(measure_two == 1 & (BILI_1 %in% c(55, 77) | BILI_2 %in% c(55, 77)), "BILI: Single measurement with 2 staff", NA_character_),
      # Check if two measurements were taken when only one staff member was present
      Q_BILI_Two_One_Staff = if_else(measure_one == 1 & (BILI_1 %in% c(1:9) & BILI_2 %in% c(1:9)), "BILI: Two measurements with 1 staff", NA_character_),
      # Check if a third measurement is required based on large differences between first two measurements
      Q_BILI_Third_Required = if_else(measure_two == 1 & bili_diff >= 2 & BILI_JOINT %in% c(55, 77, -5, -7), "BILI: Third measurement required", NA_character_),
      Q_BILI_DIFF2_Invalid = if_else(measure_two == 1 & bili_diff >= 2 & BILI_DIFF2 %in% c(0, 77), "BILI_DIFF2: Invalid Data Entry", NA_character_)
    ) %>%
    pivot_longer(cols = starts_with("Q_BILI_"), names_to = "Query_Type", values_to = "Query") %>%
    filter(!is.na(Query))
  
  bili_query <- bili_df %>%
    select (INFANTID, MOMID, PREGID, VISIT_OBSSTDAT, TYPE_VISIT,  Query, Query_Type, NUM_PRISMA_STAFF, starts_with("MST"), starts_with("BILI"),
            starts_with("measure"), matches("diff"))
  
  mst_query <- mst_df %>%
    select (INFANTID, MOMID, PREGID, VISIT_OBSSTDAT, TYPE_VISIT,  Query, Query_Type, NUM_PRISMA_STAFF, starts_with("MST"), starts_with("BILI"),
            starts_with("measure"), matches("diff"))
  
  
  if (nrow(bili_query) == 0 & nrow(mst_query) == 0) {
    print("No queries found in BILI or MST. Skipping binding.")
    all_bm_query <- NULL
  } else if (nrow(bili_query) == 0) {
    print("No BILI queries found. Binding only MST queries.")
    all_bm_query <- mst_query %>% rename_with(toupper)
  } else if (nrow(mst_query) == 0) {
    print("No MST queries found. Binding only BILI queries.")
    all_bm_query <- bili_query %>% rename_with(toupper)
  } else {
    # Both datasets are available; bind and convert column names to uppercase
    all_bm_query <- bind_rows(bili_query, mst_query) %>% rename_with(toupper)
  }
  
  if (nrow(bili_query) >= 1)  {
    
    # Create Bili_Query_Df with proper case_when syntax
    Bili_Query_Df <- all_bm_query %>%
      mutate(
        EDIT_TYPE = QUERY,
        SCRNID = NA,
        VISIT_TYPE = TYPE_VISIT,
        VISIT_DATE = VISIT_OBSSTDAT,
        FORM = "MNH36",
        
        VARIABLENAME = case_when(
          grepl("BILI", EDIT_TYPE) & (grepl("Both measurements are missing", EDIT_TYPE)) ~ "BILI_1/BILI_2",
          grepl("BILI", EDIT_TYPE) &  (grepl("Measurement is missing", EDIT_TYPE)) ~ "BILI_1",
          grepl("BILI", EDIT_TYPE) & (grepl("Single", EDIT_TYPE) | grepl("Two", EDIT_TYPE)) ~ "BILI_2",
          grepl("BILI", EDIT_TYPE) & grepl("Third", EDIT_TYPE) ~ "BILI_JOINT",
          grepl("BILI_DIFF2", EDIT_TYPE) ~ "BILI_DIFF2",
          
          grepl("MST", EDIT_TYPE) & (grepl("Both measurements are missing", EDIT_TYPE)) ~ "MST_1/MST_2",
          grepl("MST", EDIT_TYPE) &  (grepl("Measurement is missing", EDIT_TYPE)) ~ "MST_1",
          grepl("MST", EDIT_TYPE) & (grepl("Single", EDIT_TYPE) | grepl("Two", EDIT_TYPE)) ~ "MST_2",
          grepl("MST", EDIT_TYPE) & grepl("Third", EDIT_TYPE) ~ "MST_JOINT",
          grepl("MST_DIFF3", EDIT_TYPE) ~ "MST_DIFF3",
          TRUE ~ NA_character_
        ),
        
        VARIABLEVALUE = case_when(
          grepl("BILI", EDIT_TYPE) & grepl("Measurement is missing", EDIT_TYPE) ~ as.character(BILI_1),
          grepl("BILI", EDIT_TYPE) & grepl("Both measurements are missing", EDIT_TYPE) ~ paste0(BILI_1, "/", BILI_2),
          grepl("BILI", EDIT_TYPE) & (grepl("Single", EDIT_TYPE) | grepl("Two", EDIT_TYPE)) ~ as.character(BILI_2),
          grepl("BILI", EDIT_TYPE) & grepl("Third", EDIT_TYPE) ~ as.character(BILI_JOINT),
          grepl("BILI_DIFF2", EDIT_TYPE) ~ as.character(BILI_DIFF2),
          
          grepl("MST", EDIT_TYPE) & grepl("Both measurements are missing", EDIT_TYPE) ~ paste0(MST_1, "/", MST_2),
          grepl("MST", EDIT_TYPE) & grepl("Measurement is missing", EDIT_TYPE) ~ as.character(MST_1),
          grepl("MST", EDIT_TYPE) & (grepl("Single", EDIT_TYPE) | grepl("Two", EDIT_TYPE)) ~ as.character(MST_2),
          grepl("MST", EDIT_TYPE) & grepl("Third", EDIT_TYPE) ~ as.character(MST_JOINT),
          grepl("MST_DIFF3", EDIT_TYPE) ~ as.character(MST_DIFF3),
          TRUE ~ NA_character_
        ),
        
        FIELD_TYPE = "Number")
    
    BiliQuery_Prep <- Bili_Query_Df %>%
      mutate_all(as.character) %>%
      select(SCRNID, MOMID, PREGID, INFANTID, VISIT_TYPE, VISIT_DATE, FORM, VARIABLENAME, VARIABLEVALUE, FIELD_TYPE, EDIT_TYPE)
    
    names(BiliQuery_Prep) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
    
    # Add additional columns
    Bili_query_export <- cbind(QueryID = NA,
                               UploadDate = format(Sys.time(), "%Y-%m-%d"),
                               BiliQuery_Prep,
                               DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    # combine form/edit type var
    Bili_query_export$Form_Edit_Type <- paste(Bili_query_export$Form,"_",Bili_query_export$EditType)
    
    ## assign queryid -- edit type id for out of range is 10
    Bili_query_export <- Bili_query_export %>% mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "10")) %>% mutate_all(as.character)
    
  } else {print ("No Biliruler Measurement Query")}
  
  # test_mst <- mst_df %>% filter (measure_two == 1)
  #  View(test_mst)
  #
  # edit_type_counts <- Bili_Query_Df %>%
  #   count(EDIT_TYPE, sort = TRUE)
  #
  # print(edit_type_counts)
  
  #Query No 2:
  # At IPC, a referral was made, but at PNC-0, they say that a referral was not made.
  # Likewise at PNC-0, a referral was made, but they say that the referral was not made at PNC-1
  # What this should look like at IPC to PNC-0 is:
  # At IPC, JAUND_REF_MADE_TODAY = 0 (no) and then at PNC-0, JAUND_REF_MADE_PREV = 0 (No),
  # or these variables should match each other
  # If a referral was not made, then the referral variables (everything in section G) should all be 77.
  # If a referral was made, then there should be valid responses in section G (Not for every question,
  # but for SEEK_TX_JAUND, TSB_LBPERF, and JAUND_CEOCCUR)
  # And then likewise from PNC-0 to PNC1 and from PNC1 to EITHER PNC-4 or PNC-6.
  
  
  # Define the expected visit order
  visit_order <- c(7:12)
  
  bili_ref_df <- mnh36 %>% filter (!(TYPE_VISIT %in% c(6, 14)) & INF_VISIT_MNH36 %in% c(1,2)) %>%
    filter(TYPE_VISIT %in% visit_order) %>%  # Filter only relevant visits
    arrange(INFANTID, MOMID, PREGID, factor(TYPE_VISIT, levels = visit_order), VISIT_OBSSTDAT)
  
  # Process the dataset
  referral_check <- bili_ref_df %>%
    
    arrange(INFANTID, MOMID, PREGID, factor(TYPE_VISIT, levels = visit_order), VISIT_OBSSTDAT) %>%  # Ensure visit order
    
    # Create lagged variables for referral comparison
    group_by(INFANTID, MOMID, PREGID) %>%
    mutate(
      prev_visit = lag(TYPE_VISIT),
      prev_JAUND_REF_MADE_TODAY = lag(JAUND_REF_MADE_TODAY),
      
      # Identify mismatches between previous and current visit
      referral_mismatch = case_when(
        prev_JAUND_REF_MADE_TODAY == 1 & JAUND_REF_MADE_PREV == 0 ~ "Previous Referral: Missing In Current Visit",
        prev_JAUND_REF_MADE_TODAY == 0 & JAUND_REF_MADE_PREV == 1 ~ "Previous Referral Not Made: Marked In Current Visit",
        TRUE ~ NA_character_
      )) %>% ungroup()
  
  referral_check_query <- referral_check %>%
    mutate(
      # Section G: Each variable should match referral status
      SEEK_TX_JAUND_Error = case_when(
        JAUND_REF_MADE_PREV == 0 & SEEK_TX_JAUND == 1 ~ "No Previous Referral: Inf Treated",
        JAUND_REF_MADE_PREV == 1 & SEEK_TX_JAUND == 77 ~ "Previous Referral: Inf Treat Status Unknown",
        TRUE ~ NA_character_
      ),
      
      TSB_LBPERF_Error = case_when(
        JAUND_REF_MADE_PREV == 0 & TSB_LBPERF == 1 ~ "No Previous Referral: TSB Measured",
        JAUND_REF_MADE_PREV == 1 & TSB_LBPERF == 77 & SEEK_TX_JAUND == 1 ~ "Previous Referral: TSB Status Unknown",
        TRUE ~ NA_character_
      ),
      
      JAUND_CEOCCUR_Error = case_when(
        JAUND_REF_MADE_PREV == 0 & JAUND_CEOCCUR == 1 ~ "No Previous Referral: Jaundice Diagnosed",
        JAUND_REF_MADE_PREV == 1 & JAUND_CEOCCUR == 77 & SEEK_TX_JAUND == 1 ~ "Previous Referral: Jaundice Diagnosis Unknown",
        TRUE ~ NA_character_
      ),
      
      TSB_LBORRES_Error = case_when(
        TSB_LBPERF == 1 & (TSB_LBORRES < 0 | is.na (TSB_LBORRES)) ~ "Missing TSB result",
        TRUE ~ NA_character_
      )
    ) %>%
    
    # Reshape dataset to capture issues per row
    pivot_longer(cols = c(referral_mismatch, SEEK_TX_JAUND_Error, TSB_LBPERF_Error, JAUND_CEOCCUR_Error, TSB_LBORRES_Error),
                 names_to = "Query_Type", values_to = "Query") %>%
    filter(!is.na(Query))
  
  if (nrow(referral_check_query) >= 1)  {
    
    # Create Bili_Query_Df with proper case_when syntax
    Ref_Query_Df <- referral_check_query %>%
      mutate(
        EDIT_TYPE = Query,
        SCRNID = NA,
        VISIT_TYPE = TYPE_VISIT,
        VISIT_DATE = VISIT_OBSSTDAT,
        FORM = "MNH36",
        
        VARIABLENAME = case_when(
          Query_Type == "referral_mismatch" ~ "JAUND_REF_MADE_PREV",
          Query_Type == "SEEK_TX_JAUND_Error" ~ "SEEK_TX_JAUND",
          Query_Type == "TSB_LBPERF_Error" ~ "TSB_LBPERF",
          Query_Type == "JAUND_CEOCCUR_Error" ~ "JAUND_CEOCCUR",
          Query_Type == "TSB_LBORRES_Error" ~ "TSB_LBORRES",
          TRUE ~ NA_character_
        ),
        
        # Create VARIABLEVALUE by extracting the actual value of the corresponding variable
        VARIABLEVALUE = case_when(
          Query_Type == "referral_mismatch" ~ as.character(JAUND_REF_MADE_PREV),
          Query_Type == "SEEK_TX_JAUND_Error" ~ as.character(SEEK_TX_JAUND),
          Query_Type == "TSB_LBPERF_Error" ~ as.character(TSB_LBPERF),
          Query_Type == "JAUND_CEOCCUR_Error" ~ as.character(JAUND_CEOCCUR),
          Query_Type == "TSB_LBORRES_Error" ~ as.character(TSB_LBORRES),
          TRUE ~ NA_character_
        ),
        FIELD_TYPE = "Number")
    
    RefQuery_Prep <- Ref_Query_Df %>%
      mutate_all(as.character) %>%
      select(SCRNID, MOMID, PREGID, INFANTID, VISIT_TYPE, VISIT_DATE, FORM, VARIABLENAME, VARIABLEVALUE, FIELD_TYPE, EDIT_TYPE)
    
    names(RefQuery_Prep) = c("ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",  "Variable Value","FieldType", "EditType")
    
    # Add additional columns
    RefBili_query_export <- cbind(QueryID = NA,
                                  UploadDate = format(Sys.time(), "%Y-%m-%d"),
                                  RefQuery_Prep,
                                  DateEditReported = format(Sys.time(), "%Y-%m-%d"))
    # combine form/edit type var
    RefBili_query_export$Form_Edit_Type <- paste(RefBili_query_export$Form,"_",RefBili_query_export$EditType)
    
    ## assign queryid -- edit type id for out of range is 10
    RefBili_query_export <- RefBili_query_export %>% mutate(QueryID = paste0(Form, "_", VisitDate, "_",MomID, "_",`Variable Name`, "_", `Variable Value`, "_", "10")) %>% mutate_all(as.character)
    
  } else {print ("No Bili-ruler Referral Query")}
  
  if (!exists("RefBili_query_export") & !exists("Bili_query_export")) {
    print("No queries found in measurement or referral bili query")
    
  } else if (!exists("RefBili_query_export") & exists("Bili_query_export")) {
    print("No referral queries found. Binding only measurement queries.")
    
    BiliRuler_export_query <- Bili_query_export
    save(BiliRuler_export_query, file = paste0(path_to_save, "/BiliRuler_export_query.rda"))
    
  } else if (exists("RefBili_query_export") && !exists("Bili_query_export") ) {
    print("No measurement queries found. Binding only referral queries.")
    
    BiliRuler_export_query <- RefBili_query_export
    save(BiliRuler_export_query, file = paste0(path_to_save, "/BiliRuler_export_query.rda"))
    
  } else {
    # Both datasets are available; bind and convert column names to uppercase
    BiliRuler_export_query <- bind_rows(Bili_query_export, RefBili_query_export)
    save(BiliRuler_export_query, file = paste0(path_to_save, "/BiliRuler_export_query.rda"))
  }
  
  # This else statement seems to be part of an outer if condition
  # Make sure it is correctly placed in your actual code
} else {
  print("MNH36 Form Not Present")
}
