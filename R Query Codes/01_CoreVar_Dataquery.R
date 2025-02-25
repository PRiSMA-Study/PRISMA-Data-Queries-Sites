#*****************************************************************************
#*QUERY #1 -- CHECK FOR CORE VARIABLE NAMES 
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 25 February 2025

#*Input: Wide data (all raw .csv files)
#*Function: check to make sure all variables exist in the data and match data dictionary formatting 
#*Output: .rda file with all missing or extra variable names 
#*****************************************************************************
#* Items to Update before running script 
#* You can copy and paste "UPDATE EACH RUN" to find where to update 
#* 1. Update "UploadDate" 
#* 2. Set "site" variable to the site you are running the query for 
#* 3. Set your main directory 
#* 
#* Once the previous lines of code are updated, you can highlight the entire script and run 

#* Notes: 
#* Make sure the data dictionary is in the correct folder 
#*****************************************************************************
#*****************************************************************************
#* DATA SETUP
#*****************************************************************************

# load packages 
library(tidyverse)
library(readxl)
library(tibble)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)
library(openxlsx)
library(stringr)

# UPDATE EACH RUN: set site variable - this is nsecessary to call in the correct MNH25 variables from the data dictionary (each site has their own MNH25)
site = "Kenya"

# UPDATE EACH RUN: Update "UploadDate" (this should match the folder name in synapse)
UploadDate = "2023-08-25"

# UPDATE EACH RUN: load in the WIDE data we generated from 00_DataImport code 
load(paste0("~/PRiSMAv2Data/Kenya/2023-08-25/data/2023-08-25_wide.Rdata", sep = "")) 

## UPDATE EACH RUN: set path to location where you want to save the query output below 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

## call in the data dictionary 
variable_names <- read_excel("~/PRiSMAv2Data/PRISMA-Data-Queries-GW/R/PRISMA-MNH-Data-Dictionary-Repository-V.2.7-25OCT2024_queries.xlsx",
                             sheet = "Data Dictionary")

variable_names <- variable_names %>%
  filter(is.na(`Operational status notes`)) %>% select(Form, `Variable Name`) ## `Operational status notes` will remove any variables that are still in the data dictionary but are no longer in use
names(variable_names) = c("Form", "VarName")
variable_names$VarName = toupper(variable_names$VarName)

# Define the forms
forms <- c("mnh00", "mnh01", "mnh02", "mnh03", "mnh04", "mnh05", "mnh06", "mnh07", "mnh08", "mnh09",
           "mnh10", "mnh11", "mnh12", "mnh13", "mnh14", "mnh15", "mnh16", "mnh17", "mnh18",
           "mnh19", "mnh20", "mnh21", "mnh22", "mnh23", "mnh24", "mnh25", "mnh26",
           # "mnh27","mnh28", "mnh29",
           "mnh30","mnh31","mnh32","mnh36","mnh37")

# forms <- c("mnh25")

# Initialize data frames to store missing and extra variables
VarNamesMissing <- data.frame(Form = character(), `Missing Variables` = character(), stringsAsFactors = FALSE)
VarNamesExtra <- data.frame(Form = character(), `Extra Variables` = character(), stringsAsFactors = FALSE)

# Loop through each form
for (form in forms) {
  if (exists(form)) {
    
    # Special handling for MNH25
    if (form == "mnh25") {
      site_mnh25 <- case_when(site %in% c("India-CMC", "India-SAS") ~ "India", TRUE ~ site)
      VarNames_form <- variable_names %>% 
        filter(Form == paste0("MNH25_", site_mnh25)) %>%
        select("VarName")
      print(site_mnh25)
      print(paste0("MNH25_", site_mnh25))
    } else {
      VarNames_form <- variable_names %>% 
        filter(Form == toupper(form)) %>%
        select("VarName")
    }
    
    # Get variable list for the form
    # VarNames_form <- variable_names %>% filter(Form == toupper(form)) %>% select("VarName")
    VarNames_dict <- as.vector(VarNames_form$VarName)
    
    # Get variable names from the dataset
    VarNames_data <- colnames(get(form))
    
    # Identify missing variables (in dictionary but not in data)
    VarNamesMissing_tmp <- setdiff(VarNames_dict, VarNames_data)
    if (length(VarNamesMissing_tmp) > 0) {
      VarNamesMissing_tmp <- data.frame(Form = toupper(form), `Missing Variables` = VarNamesMissing_tmp, stringsAsFactors = FALSE)
      VarNamesMissing <- rbind(VarNamesMissing, VarNamesMissing_tmp)
    }
    
    # Identify extra variables (in data but not in dictionary)
    VarNamesExtra_tmp <- setdiff(VarNames_data, VarNames_dict)
    if (length(VarNamesExtra_tmp) > 0) {
      VarNamesExtra_tmp <- data.frame(Form = toupper(form), `Extra Variables` = VarNamesExtra_tmp, stringsAsFactors = FALSE)
      VarNamesExtra <- rbind(VarNamesExtra, VarNamesExtra_tmp)
      
    }
  }
}



# Print results
# view(VarNamesMissing)
# view(VarNamesExtra)

dim(VarNamesMissing)
dim(VarNamesExtra)
#*****************************************************************************
#* Organize the data to match the query report template 
#*****************************************************************************
if (dim(VarNamesMissing)[1] > 1){
  
  # update naming 
  names(VarNamesMissing) = c("Form", "Variable Name")
  
  # remove first empty row 
  VarNamesMissing = VarNamesMissing[-1,]
  
  ## add additional columns 
  VarNamesMissing = cbind(QueryID = NA, 
                          UploadDate = UploadDate, 
                          ScrnID = "NA", MomID = "NA",
                          PregID = "NA", InfantID = "NA",
                          VisitType = "NA",
                          VisitDate = "NA", 
                          VarNamesMissing, 
                          `Variable Value` = "NA",
                          FieldType = "NA", 
                          EditType = "Missing Variable", 
                          DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  VarNamesMissing$Form_Edit_Type <- paste(VarNamesMissing$Form,"_",VarNamesMissing$EditType)
  
  ## assign queryid -- edit type id for missing variables is 03 
  VarNamesMissing <- VarNamesMissing %>% 
    mutate(QueryID = paste0(Form, "_", `Variable Name`, "_", "03")
    )
  
}

## assign queryid -- 
# edit type id for out of range is 05 
# edit type id for invalid response option is 09

if (dim(VarNamesExtra)[1] > 1){
  
  VarNamesExtra <- VarNamesExtra %>% filter(!(Form %in% c("MNH04", "MNH19") & `Extra.Variables` != "IRON_CMOCCUR"))
  
  # update naming 
  names(VarNamesExtra) = c("Form", "Variable Name")
  
  # remove first empty row 
  VarNamesExtra = VarNamesExtra[-1,]
  
  
  ## add additional columns 
  VarNamesExtra = cbind(QueryID = NA, 
                        UploadDate = UploadDate, 
                        ScrnID = "NA", MomID = "NA",
                        PregID = "NA", InfantID = "NA",
                        VisitType = "NA",
                        VisitDate = "NA", 
                        VarNamesExtra, 
                        `Variable Value` = "NA",
                        FieldType = "NA", 
                        EditType = "Extra Variable", 
                        DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  VarNamesExtra$Form_Edit_Type <- paste(VarNamesExtra$Form,"_",VarNamesExtra$EditType)
  
  ## assign queryid -- edit type id for missing variables is 02 
  VarNamesExtra <- VarNamesExtra %>% 
    mutate(QueryID = paste0(Form, "_", `Variable Name`, "_", "02")
    )
  
}

# merge together 
if (dim(VarNamesMissing)[1] >= 1 & dim(VarNamesExtra)[1] >= 1){
  
  missing_query <- bind_rows(VarNamesMissing, VarNamesExtra)
  
} else if (dim(VarNamesMissing)[1] >= 1 & dim(VarNamesExtra)[1] < 1){
  
  missing_query <- VarNamesMissing
  
} else if (dim(VarNamesMissing)[1] < 1 & dim(VarNamesExtra)[1] >=1) {
  
  missing_query <- VarNamesExtra
}

if (exists("missing_query")==TRUE) {
  missing_query <- missing_query %>% filter(!is.na(`Form`))
  
  ## export variable checking query 
  save(missing_query, file = paste0(path_to_save, "/queries/missing_query.rda"))
  
}
