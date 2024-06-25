#*****************************************************************************
#*Skip logic check
#*--check if the skip logic applied correctly and if default value used correctly
#*By Xiaoyan Hu
#*Edited and reviewed by Precious Williams
#*****************************************************************************
rm(list = ls())

# install.packages("xlsx")
# install.packages("openxlsx")

library(tidyverse)
library(readxl)
library(xlsx)
library(data.table)
library(openxlsx)
library(dplyr)
library(magrittr)

site = "Kenya" #first place to revise every time
upload_date = "2024-06-14" #second place to revise every time

#*****************************************************************************
#* Part 1. Read data in (df)
#*****************************************************************************
#1. set directory

load(paste0("~/PRiSMAv2Data/", site, "/", upload_date,"/data/", upload_date, "_wide.Rdata", sep = "")) 

setwd(paste0("~/PRiSMAv2Data/", site, "/", upload_date, "/", sep = ""))


if (exists("mnh00") == TRUE){
  mnh00 <- mnh00 %>%
    mutate("visit_date" = SCRN_OBSSTDAT,
           "Visit_Type" = NA) 
}

if (exists("mnh01") == TRUE){
  mnh01 <- mnh01 %>%
    mutate("visit_date" = US_OHOSTDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(MAT_VISIT_MNH01 %in% c(1, 2))
}

if (exists("mnh02") == TRUE){
  mnh02 <- mnh02 %>%
    mutate("visit_date" = SCRN_OBSSTDAT,
           "Visit_Type" = NA) 
}

if (exists("mnh03") == TRUE){
  mnh03 <- mnh03 %>%
    mutate("visit_date" = SD_OBSSTDAT,
           "Visit_Type" = NA)  %>%
    filter(MAT_VISIT_MNH03 %in% c(1, 2))
}

if (exists("mnh04") == TRUE){
  mnh04 <- mnh04 %>%
    mutate("visit_date" = ANC_OBSSTDAT,
           "Visit_Type" = TYPE_VISIT) %>%
  filter(MAT_VISIT_MNH04 %in% c(1, 2))
}

if (exists("mnh05") == TRUE){
  mnh05 <- mnh05 %>%
    mutate("visit_date" = ANT_PEDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(MAT_VISIT_MNH05 %in% c(1, 2))
  }

if (exists("mnh06") == TRUE){
  mnh06 <- mnh06 %>%
    mutate("visit_date" = DIAG_VSDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(MAT_VISIT_MNH06 %in% c(1, 2))
}

if (exists("mnh07") == TRUE){
  mnh07 <- mnh07 %>%
    mutate("visit_date" = MAT_SPEC_COLLECT_DAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(MAT_VISIT_MNH07 %in% c(1, 2))
}

if (exists("mnh08") == TRUE){
  mnh08 <- mnh08 %>%
    mutate("visit_date" = LBSTDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(MAT_VISIT_MNH08 %in% c(1, 2))
}

if (exists("mnh09") == TRUE){
  mnh09 <- mnh09 %>%
    mutate("visit_date" = MAT_LD_OHOSTDAT,
           "Visit_Type" = NA) %>%
    filter(MAT_VISIT_MNH09 %in% c(1, 2))
}

if (exists("mnh10") == TRUE){
  mnh10 <- mnh10 %>%
    mutate("visit_date" = VISIT_OBSSTDAT,
           "Visit_Type" = NA) %>%
    filter(MAT_VISIT_MNH10 %in% c(1, 2))
}

if (exists("mnh11") == TRUE){
  mnh11 <- mnh11 %>%
    mutate("visit_date" = VISIT_OBSSTDAT,
           "Visit_Type" = NA) %>%
    filter(INF_VISIT_MNH11 %in% c(1, 2))
}

if (exists("mnh12") == TRUE){
  mnh12 <- mnh12 %>%
    mutate("visit_date" = VISIT_OBSSTDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(MAT_VISIT_MNH12 %in% c(1, 2))
}

if (exists("mnh13") == TRUE){
  mnh13 <- mnh13 %>%
    mutate("visit_date" = VISIT_OBSSTDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(INF_VISIT_MNH13 %in% c(1, 2, 3))
}

if (exists("mnh14") == TRUE){
  mnh14 <- mnh14 %>%
    mutate("visit_date" = VISIT_OBSSTDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(INF_VISIT_MNH14 %in% c(1, 2, 3))
}

if (exists("mnh15") == TRUE){
  mnh15 <- mnh15 %>%
    mutate("visit_date" = OBSSTDAT,
           "Visit_Type" = TYPE_VISIT) %>%
    filter(INF_VISIT_MNH15 %in% c(1, 2, 3))
}

if (exists("mnh16") == TRUE){
  mnh16 <- mnh16 %>%
    mutate("visit_date" = VISDAT,
           "Visit_Type" = NA) %>%
    filter(MAT_VISIT_MNH16 %in% c(1, 2))
}

if (exists("mnh17") == TRUE){
  mnh17 <- mnh17 %>%
    mutate("visit_date" = VISDAT,
           "Visit_Type" = NA) %>%
    filter(MAT_VISIT_MNH17 %in% c(1, 2))
}

if (exists("mnh18") == TRUE){
  mnh18 <- mnh18 %>%
    mutate("visit_date" = VISDAT,
           "Visit_Type" = NA) %>%
    filter(MAT_VISIT_MNH18 %in% c(1, 2))
}

if (exists("mnh19") == TRUE){
  mnh19 <- mnh19 %>%
    mutate("visit_date" = OBSSTDAT,
           "Visit_Type" = NA) 
}

if (exists("mnh20") == TRUE){
  mnh20 <- mnh20 %>%
    mutate("visit_date" = OBSSTDAT,
           "Visit_Type" = NA) 
}

if (exists("mnh21") == TRUE){
  mnh21 <- mnh21 %>%
    mutate("visit_date" = AESTDAT,
           "Visit_Type" = NA) 
}

if (exists("mnh22") == TRUE){
  mnh22 <- mnh22 %>%
    mutate("visit_date" = DVSTDAT,
           "Visit_Type" = NA)
}

if (exists("mnh23") == TRUE){
  mnh23 <- mnh23 %>%
    mutate("visit_date" = CLOSE_DSSTDAT,
           "Visit_Type" = NA)
}

if (exists("mnh24") == TRUE){
  mnh24 <- mnh24 %>%
    mutate("visit_date" = CLOSE_DSSTDAT,
           "Visit_Type" = NA)
}

if (exists("mnh25") == TRUE){
  mnh25 <- mnh25 %>%
    mutate("visit_date" = OBSSTDAT,
           "Visit_Type" = TYPE_VISIT)%>%
    filter(MAT_VISIT_MNH25 %in% c(1, 2))
}

if (exists("mnh26") == TRUE){
  mnh26 <- mnh26 %>%
    mutate("visit_date" = FTGE_OBSTDAT,
           "Visit_Type" = TYPE_VISIT)%>%
    filter(MAT_VISIT_MNH26 %in% c(1, 2))
}


#*****************************************************************************
#*Part 2. Read data dictionary (ff)
#*****************************************************************************
#1. read dictionary and keep the columns needed (Change to complete data dictionary location)


# Define your extract_variable_names function
extract_variable_names <- function(temp_var) {
  pattern <- "\\b(==|>=|<=|&|\\|)\\s*\\d+\\b"  
  clean_var <- gsub(pattern, "", temp_var)
  filter_vars <- regmatches(clean_var, gregexpr("\\b\\w+\\b", clean_var))[[1]]
  filter_vars <- filter_vars[!grepl("^\\d+$", filter_vars)]
  return(filter_vars)
}

# Read your Excel file and preprocess the data
dict <- read_excel("~/Analysis/Dictionary/Data_Dictionary_Skip_Logic_v2.4",
                   sheet = "Data Dictionary") %>%
  select(Form, `Variable Name`, `Field Label (Question Text)`, 
         `Response Options`, `Value`, Filter, 
         `Field Type (Date, Time, Number, Text)`) %>%
  rename(variable_name = `Variable Name`) %>%
  rename(field_label = `Field Label (Question Text)`) %>%
  rename(response_option = `Response Options`) %>%
  rename(field_type = `Field Type (Date, Time, Number, Text)`) %>%
  mutate(variable_name = toupper(variable_name),
         Filter = toupper(Filter))

# Add a new column with the extracted variable names
dict <- dict %>%
  mutate(extracted_variables = sapply(Filter, extract_variable_names))

India_Sites <- c("India_CMC", "India-CMC", "India_SAS", "India-SAS")

mnh25_edit <- dict  %>%
  mutate(Form = case_when(
    site == "Ghana" & Form ==  "MNH25_Ghana" ~ "MNH25",
    site %in% India_Sites & Form ==  "MNH25_India" ~ "MNH25",
    site == "Pakistan"  & Form ==  "MNH25_Pakistan" ~ "MNH25",
    site == "Zambia"  & Form ==  "MNH25_Zambia" ~ "MNH25",
    site == "Kenya"  & Form ==  "MNH25_Kenya" ~ "MNH25",
    TRUE ~ Form
  ))

#2. list each forms as in data dictionary
filter <- lapply(c(paste0("MNH0",c(0:9)),
                   paste0("MNH",c(10:26))),
                   function(n) {
                   filter <- mnh25_edit %>% filter(Form == n)
                 }
)
names(filter) <- c(paste0("fil0", c(0:9)),paste0("fil",c(10:26)))

#3. split filter list
list2env(filter, globalenv())

#*****************************************************************************
#*Part 3. logic check function
#*****************************************************************************
logic_check <- function(df, ff){
  # df = mnh21 #delete later
  # ff = fil21 #delete later
  #******************First sheet (error form)******************  
  #******1. create variables for checking logic
  var_name = colnames(df)
  ff$exp_n = NA
  ff$dat_n = NA
  ff$n77 = NA
  ff$n55 = NA
  
  #define case_list
  missing_vars_list <- list()
  
  #******2. skip logic error check for error form
  for (var in var_name) {
    
    for (i in 1:nrow(ff)) {
      
      # Check if the filter variables are present in the dataframe
      if(all(ff$extracted_variables[[i]] %in% names(df)) || ff$Filter[i] %in% c("ALL", "OPTIONAL")) {
        
      #2.1. Filter == "ALL"
      if (ff$variable_name[i] == var & ff$Filter[i] == "ALL") { 
        
        #a. exp_n - expected n (all field type)
        ff$exp_n[i] = nrow(df)
        #b. dat_n - n in data ()
        #field type: Number-continuous, Text, Date, Time (no input in value column)
        if (is.na(ff$Value[i])) {
          ff$dat_n[i] = sum(
            !is.na(df[[var]]) & #not NA
              !(df[[var]] %in% c(-7,"n/a","07/07/1907","77:77",-5,"missing","05/05/1905","55:55", -7.00)))  #not default value 77,55
        }
        #field type: Number-categorical
        else {
          ff$dat_n[i] = sum(df[[var]] %in% eval(parse(text = paste0("c(",ff$Value[i],")"))))
        }
        #c. n77
        #field type: Number-continuous, Text, Date, Time (no input in value column)
        if (is.na(ff$Value[i])) {
          ff$n77[i] = sum(df[[var]] %in% c(-7,"n/a", "07/07/1907", "77:77", -7.00))
        }
        #field type: Number-categorical
        else {
          ff$n77[i] = sum(df[[var]] == 77 & !(77 %in% eval(parse(text = paste0("c(",ff$Value[i],")")))))
        }
        #d. n55
        #field type: Number-continuous, Text, Date, Time (no input in value column)
        if (is.na(ff$Value[i])) {
          ff$n55[i] = sum(df[[var]] %in% c(-5,"missing","05/05/1905","55:55"))
        }
        #field type: Number-categorical
        else {
          ff$n55[i] = sum((df[[var]] == 55 | df[[var]] == "55" ) & !(55 %in% eval(parse(text = paste0("c(",ff$Value[i],")")))))
        }
      }
      
      #2.2. Filter == "OPTIONAL" (skip check)
      else if (ff$variable_name[i] == var & ff$Filter[i] == "OPTIONAL") {next}
      
      #2.3. Filter with skip logic
     
      #field type: Number-continuous, Text, Date, Time (no input in value column)
      #a. exp_n
      else if (ff$variable_name[i] == var & is.na(ff$Value[i])) {
        
        
        #a. one filter condition
        if (str_count(ff$Filter[i], "\\&|\\|") == 0) {
          
          condition <- ff$Filter[i]
      
          # Filter the data frame based on the condition
          fil_df1 <- df %>% filter(eval(parse(text = condition)))
          # # Count the number of rows in the filtered data frame and store the result
          ff$exp_n[i] <- nrow(fil_df1) 
          
          #a.1. dat_n
          ff$dat_n[i] = sum(
            !is.na(fil_df1[[var]]) & #not NA
              !(fil_df1[[var]] %in% c(-7,"n/a","07/07/1907","77:77",-5,"missing","05/05/1905","55:55", -7.00)))  #not default value 77,55
          
          #a.2. n77
          ff$n77[i] = sum(fil_df1[[var]] %in% c(-7, "n/a","07/07/1907", "77:77", -7.00))
          #d. n55
          ff$n55[i] = sum(fil_df1[[var]] %in% c(-5, "missing","05/05/1905", "55:55", "."))
          
        }
        # b. two filter condition
        else if (str_count(ff$Filter[i], "\\&|\\|") == 1) {
          
          condition <- strsplit(ff$Filter[i], "\\&|\\|", fixed = TRUE)[[1]]
          # Filter the data frame based on the condition
          fil_df2 <- df %>% filter(eval(parse(text = condition)))
          ff$exp_n[i] = nrow(fil_df2) 
          
          #b.1. dat_n
          ff$dat_n[i] = sum(
            !is.na(fil_df2[[var]]) & #not NA
              !(fil_df2[[var]] %in% c(-7,"n/a","07/07/1907","77:77",-5,"missing","05/05/1905","55:55", -7.00)))  #not default value 77,55
          
          #b.2. n77
          ff$n77[i] = sum(fil_df2[[var]] %in% c(-7, "n/a","07/07/1907", "77:77", -7.00))
          #d. n55
          ff$n55[i] = sum(fil_df2[[var]] %in% c(-5, "missing","05/05/1905", "55:55", "."))
        } 
        
        
      }
      
      #field type: Number-categorical
      #a. exp_n
      else if (ff$variable_name[i] == var & !is.na(ff$Value[i])) {
        # one filter condition
        if (str_count(ff$Filter[i], "\\&|\\|") == 0) {
          
          condition <- ff$Filter[i]
          # Filter the data frame based on the condition
          fil_df3 <- df %>% filter(eval(parse(text = condition)))
          # # Count the number of rows in the filtered data frame and store the result
          ff$exp_n[i] <- nrow(fil_df3)
          #b. dat_n
          ff$dat_n[i] = sum(fil_df3[[var]] %in% eval(parse(text = paste0("c(",ff$Value[i],")"))))
          #c. n77
          ff$n77[i] = sum(fil_df3[[var]] == 77 & !(77 %in% eval(parse(text = paste0("c(",ff$Value[i],")")))))
          #d. n55
          ff$n55[i] = sum(fil_df3[[var]] == 55 & !(55 %in% eval(parse(text = paste0("c(",ff$Value[i],")")))))
        }
        
        # two filter condition
        else if (str_count(ff$Filter[i], "\\&|\\|") == 1) {
          condition <- strsplit(ff$Filter[i], "\\&|\\|", fixed = TRUE)[[1]]
          # Filter the data frame based on the condition
          fil_df4 <- df %>% filter(eval(parse(text = condition)))
          ff$exp_n[i] = nrow(fil_df4) 
          
          #b. dat_n
          ff$dat_n[i] = sum(fil_df4[[var]] %in% eval(parse(text = paste0("c(",ff$Value[i],")"))))
          #c. n77
          ff$n77[i] = sum(fil_df4[[var]] == 77 & !(77 %in% eval(parse(text = paste0("c(",ff$Value[i],")")))))
          #d. n55
          ff$n55[i] = sum(fil_df4[[var]] == 55 & !(55 %in% eval(parse(text = paste0("c(",ff$Value[i],")")))))
        } 
      }
    } 
        else {
      
        missing_vars <- ff$extracted_variables[[i]][!ff$extracted_variables[[i]] %in% names(df)]
        
        if (length(missing_vars) > 0) {
          for (missing_var in missing_vars) {
            ff_missing <- ff[ff$variable_name == var, ]
            
            # Check if ff_missing$Form is not empty
            if (nrow(ff_missing) > 0) {
              missing_df <- data.frame(Form = ff_missing$Form, Missing_Variable = missing_var)
              missing_vars_list[[length(missing_vars_list) + 1]] <- missing_df
            }
          }
        }
    }
  }
}
  
  #******3. clean error form
  ef <- ff %>% 
    mutate(
      skip_error = case_when(
        exp_n == dat_n ~ "no error", 
        exp_n > dat_n ~ "missing", #missing data
        exp_n < dat_n ~ "extra"), #extra data
      miss_n = exp_n - dat_n
    ) %>% 
    filter(skip_error == "missing") %>% 
    #filter(skip_error == "missing" | skip_error == "extra") %>% 
    mutate(Comments = "")
  

  
  #******************Second sheet (error case)******************  
  
  #******4. get "ID" variables from data form
  ID <- c("SCRNID", "MOMID", "PREGID", "INFANTID", "Visit_Type", "visit_date")
  # Ensure there are no leading/trailing spaces in column names
  colnames(df) <- trimws(colnames(df))
  
  # Check for missing columns and create them with NA if necessary
  missing_columns <- setdiff(ID, colnames(df))
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      df[[col]] <- NA
    }
  }
  #******5. skip logic error check for error cases
  #define case_list
  case_list <- list()
  
  if (nrow(ef) > 0) {
  
  #list out the details of variable with errors in query report format 
  for (l in 1:nrow(ef)) {
    #5.1. Filter == "ALL"
    if (ef$Filter[l] == "ALL") {
      
      case <- df %>% 
        select(all_of(ID), ef$variable_name[l]) %>% #select ID and var name
        mutate(skip_error = case_when(
          #5.1.1. missing cases 
          #list cases without input 
          is.na(eval(parse(text = ef$variable_name[l]))) ~ "missing",
          #list cases with input
          #a. field type: Number-continuous, Text, Date, Time (only list default value - other invalid value should be in out of range query)
          eval(parse(text = ef$variable_name[l])) %in% c(-7,"n/a","07/07/1907", "77:77", -5,"missing", "05/05/1905", "55:55", ".", -7.00, -77) ~ "missing", 
          #b. field type: Number-categorical
          !is.na(eval(parse(text = ef$variable_name[l]))) & 
            !is.na(ef$Value[l]) &
            !(eval(parse(text = ef$variable_name[l])) %in% eval(parse(text = paste0("c(",ef$Value[l],")")))) &
              eval(parse(text = ef$variable_name[l])) %in% c(-7, 77, -5, ".", 55,-55, -77) ~ "missing",
          TRUE ~ NA
        )  
        ) 
    }
    #5.2. Filter with skip logic
    else if(ef$Filter[l] != "ALL") {
      
      extract_variable_names <- function(temp_var) {
        
        # Define the pattern to match operators and numbers to be removed
        pattern <- "\\b(==|>=|<=|&|\\|)\\s*\\d+\\b"  # Pattern to match operators followed by numbers
        
        # Remove operators and numbers using the regular expression
        clean_var <- gsub(pattern, "", temp_var)
        
        # Extract variable names using the regular expression
        filter_vars <- regmatches(clean_var, gregexpr("\\b\\w+\\b", clean_var))[[1]]
        
        # Filter out standalone numbers
        filter_vars <- filter_vars[!grepl("^\\d+$", filter_vars)]
        
        return(filter_vars)
      }
      
      #ef$Filter[l] <- 'ANC_N_1 == 1 & PH_PREV_RPORRES == 1'
      temp_var <- ef$Filter[l]
      #temp_var <- str_replace_all(ef$Filter[l], c(" > .*" = "", " == .*" = "", "&" = "&"))
      filter_vars <- extract_variable_names(temp_var)
      filter_vars_list <- as.list(filter_vars)
      
      print(filter_vars_list)
      
      case_temp <- df %>%
        select(all_of(ID), ef$variable_name[l], !!!filter_vars_list)
       
      # case_temp <- df %>%
      # select(all_of(ID), (ef$variable_name[l]),
      #          (str_replace_all(ef$Filter[l], c(" > .*" = "", " == .*" = ""))))
      
      #5.2.1. one filter condition
      if (str_count(ef$Filter[l], "\\&|\\|") == 0) {
        case <- case_temp %>% 
          mutate(skip_error = case_when(
            #A. missing cases 
            #a. list cases without input 
            (eval(parse(text = ef$Filter[l]))) & #meet filter
              is.na(eval(parse(text = ef$variable_name[l]))) ~ "missing",
            #b. list cases with input
            #field type: Number-continuous, Text, Date, Time (only list default value - other invalid value should be in out of range query)
            (eval(parse(text = ef$Filter[l]))) & 
              is.na(ef$Value[l]) &
              eval(parse(text = ef$variable_name[l])) %in% c(-7,"n/a","07/07/1907", "77:77", -5,"missing", "05/05/1905", "55:55") ~ "missing", 
            #field type: Number-categorical
            (eval(parse(text = ef$Filter[l]))) & 
              !is.na(eval(parse(text = ef$variable_name[l]))) & 
              !is.na(ef$Value[l]) &
              !(eval(parse(text = ef$variable_name[l])) %in% eval(parse(text = paste0("c(",ef$Value[l],")")))) &
                eval(parse(text = ef$variable_name[l])) %in% c(-7, 77, -5, ".", 55,-55, -77) ~ "missing",
            TRUE ~ NA
          )
          )
      }
      #5.2.2. two filter condition
      else if (str_count(ef$Filter[l], "\\&|\\|") == 1) {
        case <- case_temp %>% 
          mutate(skip_error = case_when(
            #A. missing cases 
            #a. list cases without input 
            eval(parse(text = ef$Filter[l])) & #meet filter
              is.na(eval(parse(text = ef$variable_name[l]))) ~ "missing",
            #b. list cases with input
            #field type: Number-continuous, Text, Date, Time (only list default value - other invalid value should be in out of range query)
            eval(parse(text = ef$Filter[l])) & #meet filter
              eval(parse(text = ef$variable_name[l])) %in% c(-7,"n/a","07/07/1907", "77:77", -5,"missing", "05/05/1905", "55:55", ".", "N/A", "-7.00", "-77") ~ "missing", 
            #field type: Number-categorical
            eval(parse(text = ef$Filter[l])) & #meet filter
              !is.na(eval(parse(text = ef$variable_name[l]))) & 
              !is.na(ef$Value[l]) &
              !(eval(parse(text = ef$variable_name[l])) %in% eval(parse(text = paste0("c(",ef$Value[l],")")))) &
                eval(parse(text = ef$variable_name[l])) %in% c(-7, 77, -5, ".", 55,-55, -77) ~ "missing", 
            TRUE ~ NA
          )
          )
      }
    }
    
    case <- case %>% 
      add_column(
        Form = ef$Form[l],
        variable_name = ef$variable_name[l], 
        field_type = ef$field_type[l]
      ) %>% 
      rename(variable_value = ef$variable_name[l]) %>% 
      select(Form, all_of(ID), variable_name, variable_value, field_type, skip_error) %>%  
      filter(!is.na(skip_error)) %>% 
      mutate(across(everything(), as.character))
    case_list[[l]] = case
  }
  
  } else {
    print("There are no errors in the data frame.")
  }
  
  #******6. save all error cases and missing variables in one data frame
  suppressWarnings({
    error_case <- case_list %>% bind_rows()
    missing_list <- missing_vars_list %>% bind_rows() %>% distinct()  # Making missing_list unique
  })
  
  #******7. list outputs
  error_list <- list()
  error_list$ef <- ef
  error_list$error_case <- error_case
  error_list$missing_variables <- missing_list
 
  #******8. return error list
  return(error_list)
  
}

#*****************************************************************************
#*Part 4. skip logic check for each form
#*****************************************************************************
#1. get form names and filter names for all forms exist

# Create myfiles
myfiles <- list()

for (i in c(0:9, 10:26)) {
  index <- sprintf("%02d", i)
  if (exists(paste0("mnh", index)) && nrow(get(paste0("mnh", index))) > 0) {
    myfiles[[paste0("mnh", index)]] <- get(paste0("mnh", index))
    }
} 

data_name <- names(myfiles)
filter_name <- names(filter[str_replace_all(data_name, "mnh", "fil")])

#2. set empty dataframe for error_overview sheet and error_detail sheet
error_overview <- data.frame(matrix(nrow = 0, ncol = 0))
error_detail <- data.frame(matrix(nrow = 0, ncol = 0))
missing_variable <- data.frame(matrix(nrow = 0, ncol = 0))

#3. bind all error check results together
for (x in seq_along(data_name)) { 
  # Assign name error## for each form check
  df <- myfiles[[data_name[x]]]
  ff <- filter[[filter_name[x]]]
  error_name <- paste0("error", str_replace_all(data_name[x], "mnh", ""))
  
  assign(error_name, logic_check(df, ff))
  
  error_overview <- error_overview %>% 
    bind_rows(get(error_name)$ef)
  
  error_detail <- error_detail %>% 
    bind_rows(get(error_name)$error_case)
  
  missing_variable <- missing_variable %>% 
    bind_rows(get(error_name)$missing_variables)
  
}

#4. add columns for query format
query_temp <- error_detail %>%
  mutate(
    QueryID = paste0(Form, "_", visit_date, "_",MOMID, "_",variable_name, "_",variable_value, "_", "60"),
    UploadDate = upload_date,
    Filter = ifelse(error_detail$variable_name %in% error_overview$variable_name, 
                    error_overview$Filter[match(error_detail$variable_name , error_overview$variable_name)], NA), 
    EditType = case_when(
      skip_error == "missing" ~ "Skip Logic - missing"
      #skip_error == "extra" ~ "Skip logic - extra"
    ),
    DateEditReported = format(Sys.time(), "%Y-%m-%d"))


error_overview_query <- error_overview %>% rename ( FieldLabel = field_label,
                                                  `Response Options` = response_option,
                                                  `Variable Name` = variable_name,
                                                   `Expected (n)` = exp_n,
                                                  ` Reported (n)` = dat_n,
                                                    SkipError = skip_error,
                                                    FieldType = field_type) %>% 
                                          select (-extracted_variables)


  
query <- query_temp %>%
  mutate(
    FormEditType = paste0(query_temp$Form, "_", query_temp$EditType),
    VarFormEdit = paste0(query_temp$Form, "_", query_temp$`Variable Name`,"_", query_temp$EditType), 
    `Remove edit (if true query, put 0, it not please put 1)` = NA,
    Notes = NA
  ) %>%
  select(QueryID, UploadDate, SCRNID, MOMID, PREGID, INFANTID, Visit_Type, visit_date, 
         Form, variable_name, variable_value, field_type, EditType, DateEditReported,
         FormEditType, VarFormEdit, `Remove edit (if true query, put 0, it not please put 1)`,
         Notes) #%>% filter (EditType == "Skip logic - extra")

names(query) = c("QueryID", "UploadDate","ScrnID","MomID", "PregID","InfantID","VisitType", "VisitDate", "Form", "Variable Name",
                 "Variable Value","FieldType", "EditType",  "DateEditReported",
                 "FormEditType", "VarFormEdit", "Remove edit (if true query, put 0, it not please put 1)",
                 "Notes")

#*****************************************************************************
#*Part 5. export excel for skip logic errors
#*****************************************************************************
# write.xlsx(as.data.frame(error_overview), 
#            file = paste0(site, " - Skip Logic Error - ", Sys.Date(), ".xlsx"), 
#            sheetName = "Overview", col.names=TRUE,  row.names = FALSE, 
#            append = TRUE, showNA = FALSE)
# 
# write.xlsx(as.data.frame(query),
#            file = paste0(site, " - Skip Logic Error - ", Sys.Date(), ".xlsx"),
#            sheetName = "Query", col.names=TRUE,  row.names = FALSE,
#            append = TRUE, showNA = FALSE)

st <- format(Sys.time(), "%Y-%m-%d")
# Create a new Excel workbook
wb <- createWorkbook()

# Add sheets
addWorksheet(wb, sheetName = 'Overiview')
addWorksheet(wb, sheetName = 'Query Report')

# Write data frames to sheets
writeData(wb, sheet = 'Overiview', x = as.data.frame(error_overview_query))
writeData(wb, sheet = 'Query Report', x = as.data.frame(query))

# Get the current working directory
working_directory <- (paste0("~/PRiSMAv2Data/", site, "/", upload_date,"/queries",  sep = "")) 


# Define the file name
file_name <- paste(site, "_Skip_Logic_Error_", st, ".xlsx", sep = "")

# Concatenate the directory and file name to get the full path
excel_file_path <- file.path(working_directory, file_name)

# Save the workbook
openxlsx::saveWorkbook(wb, file = excel_file_path, overwrite = TRUE)
