#*****************************************************************************
#*QUERY #6 -- Generate Query Report 
#* Written by: Stacie Loisate & Xiaoyan Hu & Precious Williams
#* Last updated: 26 February 2024

#*Input: all .rds files 
#*Function: merge all queries together and assign query ID 
#*Output: excel sheet with a full query report 
#*****************************************************************************
#*****************************************************************************
#* Data setup
#*****************************************************************************

# clear environment 
#rm(list = ls())

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

# UPDATE EACH RUN: load in the PREVIOUS WEEK's query that has the "RemoveEdit" column filled out
previous_report_site_response <- read_excel(paste0("~/PRiSMAv2Data/Kenya/2023-08-11/PRISMA-Kenya_query_report_2023-08-11_response.xlsx", sep = ""),
                                            sheet = "Query Report") 

# UPDATE EACH RUN: load in on-going "non-queries" document 
non_queries <-  read_excel(paste0("~/PRiSMAv2Data/Kenya/PRISMA-Kenya_non-queries-ongoing.xlsx", sep = ""))

# UPDATE EACH RUN: set path to location where query output is stored
path_to_data <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

# UPDATE EACH RUN: set path to location where you want to save the final query report output 
path_to_save <- "~/PRiSMAv2Data/Kenya/2023-08-25/queries/"

## pull all .csv files from the folder specified above 
query_list <- list() # create an empty list first.
list_query <- dir(path = path_to_data, pattern = "*.rda", full.names = TRUE) #creates a list of all the csv files in the directory
for (data_file in list_query[]) { #can test by just bringing in a small number (add 1:2 inside the bracket to do so)
  form_num <- basename(data_file) #basename() pulls out just the name of the file from the entire directory/path.
  print(paste("Reading", form_num))
  query_list[[form_num]] <- load(data_file)
}

#High_Freq_Invalid_query = High_Freq_Invalid_extra_tab
## Merge all the queries togehter and convert all varables to character 
  # converting to character will avoid any merging issues between columns of different classes
report <- rbindlist(lapply(mget(ls(pattern = "*query$")), function(x) { 
  x[] <- lapply(x,type.convert, as.is = TRUE); x}),fill=TRUE)


#*****************************************************************************
#* Generating summary tables -- High Frequency Tab
# here we can remove any variables that pulled a lot of queries 
# we can note them but then remove from the query report tab (still include them in the last Form_Varname_Edit tab)
# Examples here might include a specific lab that is pulling 150 queries - this might be a range or unit issue - just bring up in email


#* Note: We will separate out the non-out of range queries and the out of range queries
## The out of range queries have an added step where the min and max from the data dictionary is merged in 
#*****************************************************************************
## add in new column that had form_varname_edittype
report = add_column(report, VarFormEdit=paste(report$Form, "_", report$`Variable Name`,"_", report$EditType, sep = ""))

## add in new columns for RemoveEdit variable and notes  
report = add_column(report, RemoveEdit = "", Notes = "") 

# extract out of range queries that are pulled >40 times in the query report - we are pulling by variable value!!
high_freq_to_exclude_outrange <- report %>%
  filter(EditType %in% c("Out of Range", "Invalid Response Option")) %>%
  group_by(`Variable Value`, VarFormEdit) %>% 
  count(name = "Frequency") %>% 
  filter(Frequency > 40) 

# extract non-out of range queries that are pulled >100 times in the query report
high_freq_to_exclude <- report %>%
  filter(!EditType %in% c("Out of Range", "Invalid Response Option")) %>%
  group_by(VarFormEdit) %>% 
  count(name = "Frequency") %>% 
  filter(Frequency > 100) 

## remove these high frequency variables from the report (since there are two dataframes i.e out_range and non-out of range)
report_query <- report %>% filter(!(VarFormEdit %in% high_freq_to_exclude$VarFormEdit))
report_query <- report_query %>% filter(!(VarFormEdit %in% high_freq_to_exclude_outrange$VarFormEdit &
                                            `Variable Value` %in% high_freq_to_exclude_outrange$`Variable Value`))

## filter out high frequency queries from the report 
high_freq_outrange <- report %>% filter((VarFormEdit %in% high_freq_to_exclude_outrange$VarFormEdit &
                                           `Variable Value` %in% high_freq_to_exclude_outrange$`Variable Value`)) %>%
  group_by(`Variable Value`, VarFormEdit, Form, `Variable Name`, EditType ) %>% 
  count(name = "Frequency") %>% 
  filter(Frequency > 500) %>% 
  mutate_all(as.character)

high_freq  <- report %>%  filter(VarFormEdit %in% high_freq_to_exclude$VarFormEdit ) %>%
  group_by(VarFormEdit, Form, `Variable Name`, EditType ) %>% 
  count(name = "Frequency") %>% 
  filter(Frequency > 500)  %>% 
  mutate (`Variable Value` = NA )%>% 
  mutate_all(as.character)


report_high_freq_bind <- bind_rows(high_freq_outrange, high_freq)

#to add the min, max, default value, etc.. from data dictionary into high frequency tab
report_high_freq <- left_join(report_high_freq_bind, varNames_sheet, by = c("Form", "Variable Name"))

## Add recommendations
table_high_freq <- report_high_freq %>% 
  mutate(
    Recommendations = case_when(
      EditType == "Extra Variable" ~ "Check variable naming and spelling or delete extra variable",
      EditType == "Missing Variable" ~ "Check variable naming and spelling or provide missing variable",
      EditType == "Duplicate ID" ~ "Investigate and resolve duplicated MomIDs for data integrity",
      EditType == "MomID Missing Enrollment Form" ~ "Ensure all ANC/PNC forms have corresponding enrollment forms",
      EditType == "Invalid Response" ~ "Correct invalid responses based on defined options in data dictionary/CRFs",
      EditType == "Out of Range" ~ "Review and correct data values outside min and max range",
      EditType == "MomID Ineligible" ~ "Verify enrollment criteria for ANC/PNC forms",
      EditType == "Visit Type Error" ~ "Review visit types and dates for accuracy",
      EditType == "Duplicate Visit Type" ~ "Investigate and resolve duplicate visit types",
      grepl("Infant", EditType) & grepl("Form", EditType) ~ "Ensure infant IDs are included in all required forms",
      EditType == "InfantID Missing Delivery Form" ~ "Ensure all InfantIDs in PNC forms are included in delivery form",
      grepl("Missing", EditType) & grepl("Form", EditType) ~ "Provide missing form or complete required forms for missed scheduled visits",
      EditType == "Inaccurate EDD" ~ "Review and verify accuracy of Estimated Due Date",
      EditType == "Inconsistencies Between LMP EDD and US EDD" ~ "Investigate discrepancies between LMP EDD and Ultrasound EDD",
      EditType == "Inconsistencies Between LMP GA and US GA" ~ "Investigate discrepancies between LMP GA and Ultrasound GA",
      EditType == "Visit Date is the same as EDD" ~ "Review and correct inconsistencies between visit dates and EDD",
      EditType == "Provide Ineligibility Criteria" ~ "Provide eligibility criteria for excluded participants",
      EditType == "Ineligibility Skip Pattern Error" ~ "Review and investigate OTHER exclusion criteria variable",
      EditType == "Specify Other Reason" ~ "Specify exclusion criteria for participants without identified reasons",
      EditType == "Size for gestational age either <0.5 or >99.5 percentile" ~ "Review birth weight and/or gestational age at birth",
      EditType == "Invalid visit following reported infant death" ~ "Review visit date where infant was reported `alive` following report of death",
      EditType == "Invalid visit following reported stillbirth" ~ "Review visit date where infant was reported `alive` following report of stillbirth",
      EditType == "Invalid Visit Type" ~ "Review visit type extra tab and correct discrepancy",
      EditType == "Participant GA >=48 weeks with no reported birth outcome (MNH04 or MNH09)" ~ "Confirm birth outcome with particpant or closeout" ),
    `Form and Edit Type` = paste(Form, EditType, sep = " - "))

table_high_freq <- ungroup(table_high_freq)

table_high_freq_to_export <- table_high_freq %>%
  select(`Form and Edit Type`, `Variable Name`, Form, EditType, Response = `Variable Value`, Frequency, 
         `Minimum Value`, `Maximum Value`, `Response Range`, `Default Value`, Recommendations, -VarFormEdit) %>%
  mutate(`Remode Edit` = NA,
         Notes = NA)
#*****************************************************************************
#* Extracting previous week's queries 
#*****************************************************************************
# we want to remove any query that the site said was not actually something that needed to be flagged - remove below
## Step 1. Extract the Query IDs that are not queries (Remove edit == 1)
queryid_to_remove = previous_report_site_response %>%
  #select(QueryID, `Remove edit (if true query, put 0, it not please put 1)`) %>%  ## select the columns you need
  #rename("RemoveEdit" = `Remove edit (if true query, put 0, it not please put 1)`) %>% ## rename remove edit column
  filter(RemoveEdit == 1) %>% ## only keep the queries that should be removed
  relocate(QueryID, .before = ScrnID) %>% 
  select(-Notes, -UploadDate, -VisitType, -VarFormEdit, -DateEditReported, -RemoveEdit)

## Step 2. rbind all new non-queries into running document
non_queries_merged <- bind_rows(non_queries, queryid_to_remove)

non_queries_merged <- non_queries_merged %>% distinct() ## extract unique elements (only want the newest entries included)

## Step 3. Extract all non-queries into running document 
write.xlsx(non_queries_merged, file = paste("~/PRiSMAv2Data/", site, "/", site_label, "_non-queries-ongoing", ".xlsx", sep = ""))

## Step 4. Convert to vector
queryid_to_remove_vec =  non_queries_merged %>% pull(as.vector(QueryID)) ## extract vector of query IDs to remove

## Step 3. Remove the query ids that should not be pulled from THIS week's report 
report_query_to_export = report_query %>% filter(!(QueryID %in% queryid_to_remove_vec))%>% 
  filter(!((VisitType %in% c(13,14))))

#*****************************************************************************
#* Removing "non-queries" from additional tabs in the report
#*****************************************************************************


if (exists("EDD_query_comments")== TRUE){
  EDD_query_comments_to_export = EDD_query_comments %>% filter(!(QueryID %in% queryid_to_remove_vec))
}

if (exists("visit_type_query_extra_tab")== TRUE){
  visit_type_query_extra_tab_to_export = visit_type_query_extra_tab %>% filter(!(QueryID %in% queryid_to_remove_vec))
}

if (exists("sga_extra_tab")== TRUE){
  sga_extra_tab_to_export = sga_extra_tab %>% filter(!(QueryID %in% queryid_to_remove_vec))
}

if (exists("inf_dead_than_alive_extra_tab")== TRUE){
  inf_dead_than_alive_extra_tab_to_export = inf_dead_than_alive_extra_tab %>% filter(!(QueryID %in% queryid_to_remove_vec))
}

#*****************************************************************************
#* Generating summary table 
#*****************************************************************************

table_freq_FmNmEd <- report_query_to_export %>%
  filter(!(`Variable Name` %in% high_freq_to_exclude)) %>%  ## remove high frequency variables 
  group_by(Form_Edit_Type) %>%
  count(name ="Frequency")  %>%
  rename("Form and Edit Type" = "Form_Edit_Type") %>%
  filter(!(is.na(`Form and Edit Type`)))

#*****************************************************************************
#* export this week's query report all to a new excel sheet 
#*****************************************************************************
st=format(Sys.time(), "%Y-%m-%d")
xl_lst <- list('Summary' = table_freq_FmNmEd, 'Query Report' = report_query_to_export,
               'High Frequency Tab' = table_high_freq_to_export,
               'Invalid Visit Types' = visit_type_query_extra_tab_to_export,
               'Invalid EDDs' = EDD_query_comments_to_export
)

write.xlsx(xl_lst, file = paste0(path_to_data,"/",  site_label, "_query_report_", st, ".xlsx"))

