#*****************************************************************************
#*QUERY #2 -- CHECK FOR DUPLICATE IDs
#* Written by: Stacie Loisate & Xiaoyan Hu
#* Last updated: 22 February 2025

#*Input: Wide data (all raw .csv files) & Long data 
#*Functions: 
    # Duplicate IDs: identifies any duplicate IDs present in the data 
    # Maternal Protocol check: do all enrolled participants have a MNH02 enrollment form?
    # Infant Protocol check: are all infants represented in a delivery form
#*Output: 
    # .rda file with all duplicate IDs (duplicates_query.rda)
    # .rda file with all MomIDs missing an enrollment form (MomidNotMatched_query.rda)
    # .rda file with all InfantIDs missing from a delivery form (InfidNotMatched_query.rda)

#* Notes: 
#* If you get an error that says it cannot bind because there are 0 rows, that means there are no duplicates 
#*****************************************************************************
#*****************************************************************************
#* Data Setup
#*****************************************************************************

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
#* check duplicated IDs, also check the rest vars' info
#*****************************************************************************
#*Make empty dataframe 
VarNamesDuplicate <- as.data.frame(matrix(nrow = 1, ncol = 6))
names(VarNamesDuplicate) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate", "Form")

#*Make empty dataframe 
VarNamesDuplicate_Inf <- as.data.frame(matrix(nrow = 1, ncol = 7))
names(VarNamesDuplicate_Inf) = c("SCRNID","MOMID", "PREGID", "INFANTID", "VisitType", "VisitDate", "Form")

#****************************************
#* SCRNID --> 00 and 02
#****************************************
if (exists("mnh00")==TRUE){
  
  dup_SCRNID <- function(form) {
    ID <- form %>% 
      dplyr::select(SCRNID)
    dup <- form[duplicated(ID),] %>% 
      arrange(SCRNID) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dupID)
  }
  
  m00_id_dup <- dup_SCRNID(mnh00)
  
  # export key variables 
  m00_id_dup <- m00_id_dup %>% 
    group_by(SCRNID) %>% 
    filter(OTHR_IEORRES != 1 & OTHR_IEORRES == 77) %>% ## remove the women would had some reason to be excluded 
    dplyr::select(SCRNID, MOMID, PREGID, SCRN_OBSSTDAT) 
  
  # add visit type column 
  m00_id_dup <- add_column(m00_id_dup,VisitType = NA , .after = "PREGID")
  
  # rename columns 
  names(m00_id_dup) = c("SCRNID","MOMID", "PREGID", "VisitType", "VisitDate")
  
  # add form column 
  m00_id_dup <- add_column(m00_id_dup,Form = "MNH00" , .after = "VisitDate")
  
  #*bind with other forms
  if (nrow(m00_id_dup > 1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m00_id_dup)
  } 
  
}
#****************************************
#* SCRNID --> 02
#****************************************
if (exists("mnh02")==TRUE){
  
  dup_SCRNID <- function(form) {
    ID <- form %>% 
      dplyr::select(SCRNID)
    dup <- form[duplicated(ID),] %>% 
      arrange(SCRNID) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dupID)
  }
  
  m02_id_dup <- dup_SCRNID(mnh02)
  
  # export key variables 
  m02_id_dup <- m02_id_dup %>% 
    dplyr::select(SCRNID, MOMID, PREGID, SCRN_OBSSTDAT)
  
  
  # add visit type column 
  m02_id_dup <- add_column(m02_id_dup, VisitType = NA , .after = "PREGID")
  
  # rename columns 
  names(m02_id_dup) = c("SCRNID","MOMID", "PREGID", "VisitType", "VisitDate")
  
  # add form column 
  m02_id_dup <- add_column(m02_id_dup,Form = "MNH02" , .after = "VisitDate")
  
  
  #*bind with other forms
  if (nrow(m02_id_dup > 1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m02_id_dup)
  } 
}

#****************************************
#*SCRNID & US_VISIT & US_OHOSTDAT  --> 01
#*
#*WILL NEED TO UPDATE ONCE SITES INTEGRATE TYPE VISIT VARIABLE 
#*THOUGHTS ON USING VISIT DATE OR VISIT TYPE FOR SCREENING ULTRASOUND 
#****************************************
if (exists("mnh01")==TRUE){
  
  #check US_OHOSTDAT if duplicates with SCRNID & US_VISIT
  dup_US <- function(form) {
    ID <- form %>% 
      dplyr::select(SCRNID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(SCRNID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  out_us <- dup_US(mnh01)
  
  # export key variables if duplicates exists 
  out_us <- out_us %>% select(SCRNID, MOMID, PREGID, TYPE_VISIT, US_OHOSTDAT)
  # out_us <- out_us %>% select(SCRNID,MOMID, PREGID, TYPE_VISIT)
  # out_us <- add_column(out_us,FORMCOMPLDAT_MNH01 =NA)
  
  # rename columns 
  names(out_us) = c("SCRNID","MOMID", "PREGID", "VisitType", "VisitDate")
  
  # add form column 
  out_us <- add_column(out_us,Form = "MNH01")
  
  #*bind with other forms
  if (nrow(out_us > 1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, out_us)
  } 
}
#****************************************
#*#MOMID & PREGID --> 03
#****************************************
if (exists("mnh03")==TRUE){
  
  names(mnh03) <- toupper(names(mnh03))
  datalist <- list(mnh03)
  
  dup_MOMID_PREGID <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  xmomid_dup <- datalist %>% map(dup_MOMID_PREGID)
  names(xmomid_dup) = c("m03")
  
  for (i in seq(xmomid_dup)){
    assign(paste0(names(xmomid_dup[i]),"_id_dup"), xmomid_dup[[i]])
  }
  
  # export key variables if duplicates exists 
  m03_id_dup <- m03_id_dup %>% select(MOMID, PREGID, SD_OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m03_id_dup >1)) {
    m03_id_dup = cbind(SCRNID = "NA", m03_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m03_id_dup >1)) {
    names(m03_id_dup) = c("SCRNID","MOMID", "PREGID", "VisitDate")
    
    # add form column
    m03_id_dup <- add_column(m03_id_dup,Form = "MNH03")
    
    # add visit type column
    m03_id_dup <- add_column(m03_id_dup,VisitType = "NA", .before = "VisitDate")
    
    m03_id_dup <- m03_id_dup %>% unique() ## only need to include 1 instance of the duplicate
    
  }
  
  
  #*bind with other forms
  if (nrow(m03_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m03_id_dup)
  }
}
#****************************************
#* MOMID & PREGID & US_VISIT --> MNH04
#****************************************
if (exists("mnh04")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  id_visit_dup_m04 <- dup_MOM_PREG_TV(mnh04)
  
  # export key variables if duplicates exists 
  if (length(id_visit_dup_m04 >1)) {
    id_visit_dup_m04 <- id_visit_dup_m04 %>% select(MOMID, PREGID, TYPE_VISIT,ANC_OBSSTDAT)
  }
  
  # add SCRNID column if duplicates exist 
  if (length(id_visit_dup_m04 >1)) {
    id_visit_dup_m04 = cbind(SCRNID = "NA", id_visit_dup_m04)
  }
  
  # rename columns if duplicates exist 
  if (length(id_visit_dup_m04 >1)) {
    names(id_visit_dup_m04) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  id_visit_dup_m04 <- add_column(id_visit_dup_m04,Form = "MNH04")
  
  id_visit_dup_m04 <- id_visit_dup_m04 %>% unique() ## only need to include 1 instance of the duplicate
  
  #*bind with other forms
  if (nrow(id_visit_dup_m04 >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, id_visit_dup_m04)
  }
  
}

#****************************************
#* MOMID & PREGID & TYPE_VISIT --> MNH05
#****************************************
if (exists("mnh05")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  id_visit_dup_m05 <- dup_MOM_PREG_TV(mnh05)
  
  # export key variables if duplicates exists 
  if (length(id_visit_dup_m05 >1)) {
    id_visit_dup_m05 <- id_visit_dup_m05 %>% select(MOMID, PREGID, TYPE_VISIT,ANT_PEDAT)
  }
  
  # add SCRNID column if duplicates exist 
  if (length(id_visit_dup_m05 >1)) {
    id_visit_dup_m05 = cbind(SCRNID = "NA", id_visit_dup_m05)
  }
  
  # rename columns if duplicates exist 
  if (length(id_visit_dup_m05 >1)) {
    names(id_visit_dup_m05) = c("SCRNID","MOMID", "PREGID", "VisitType","VisitDate")
  }
  
  # add form column 
  id_visit_dup_m05 <- add_column(id_visit_dup_m05,Form = "MNH05")
  
  id_visit_dup_m05 <- id_visit_dup_m05 %>% unique() ## only need to include 1 instance of the duplicate
  
  
  #*bind with other forms
  if (nrow(id_visit_dup_m05 >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, id_visit_dup_m05)
  }
}
#****************************************
#* MOMID & PREGID & US_VISIT --> MNH06
#****************************************
if (exists("mnh06")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  id_visit_dup_m06 <- dup_MOM_PREG_TV(mnh06)
  
  # export key variables if duplicates exists 
  if (length(id_visit_dup_m06 >1)) {
    id_visit_dup_m06 <- id_visit_dup_m06 %>% select(MOMID, PREGID, TYPE_VISIT,DIAG_VSDAT)
  }
  
  # add SCRNID column if duplicates exist 
  if (length(id_visit_dup_m06 >1)) {
    id_visit_dup_m06 = cbind(SCRNID = "NA", id_visit_dup_m06)
  }
  
  # rename columns if duplicates exist 
  if (length(id_visit_dup_m06 >1)) {
    names(id_visit_dup_m06) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  id_visit_dup_m06 <- add_column(id_visit_dup_m06,Form = "MNH06")
  
  ## only need to include 1 instance of the duplicate
  id_visit_dup_m06 <- id_visit_dup_m06 %>% unique() 
  
  #*bind with other forms
  if (nrow(id_visit_dup_m06 >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, id_visit_dup_m06)
  }
  
}
#****************************************
#* MNH07 
#****************************************
if (exists("mnh07")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m07_id_dup <- dup_MOM_PREG_TV(mnh07)
  
  # export key variables if duplicates exists 
  m07_id_dup <- m07_id_dup %>% select(MOMID, PREGID, TYPE_VISIT, MAT_SPEC_COLLECT_DAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m07_id_dup >1)) {
    m07_id_dup = cbind(SCRNID = "NA", m07_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m07_id_dup >1)) {
    names(m07_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column
  m07_id_dup <- add_column(m07_id_dup,Form = "MNH07")
  
  ## only need to include 1 instance of the duplicate
  m07_id_dup <- m07_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m07_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m07_id_dup)
  }
  
}
#****************************************
#* MNH08
#****************************************
if (exists("mnh08")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m08_id_dup <- dup_MOM_PREG_TV(mnh08)
  
  # export key variables if duplicates exists 
  m08_id_dup <- m08_id_dup %>% select(MOMID, PREGID,TYPE_VISIT, LBSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m08_id_dup >1)) {
    m08_id_dup = cbind(SCRNID = "NA", m08_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m08_id_dup >1)) {
    names(m08_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m08_id_dup <- add_column(m08_id_dup,Form = "MNH08")
  
  ## only need to include 1 instance of the duplicate
  m08_id_dup <- m08_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m08_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m08_id_dup)
  }
  
}
#****************************************
#* MNH09
#****************************************
if (exists("mnh09")==TRUE){
  
  dup_MOMID_PREGID <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m09_id_dup <- dup_MOMID_PREGID(mnh09)
  
  # export key variables if duplicates exists 
  m09_id_dup <- m09_id_dup %>% select(MOMID, PREGID, MAT_LD_OHOSTDAT)
  
  
  # add SCRNID column if duplicates exist 
  if (length(m09_id_dup >1)) {
    m09_id_dup = cbind(SCRNID = "NA", m09_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m09_id_dup >1)) {
    m09_id_dup = add_column(m09_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m09_id_dup >1)) {
    names(m09_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m09_id_dup <- add_column(m09_id_dup,Form = "MNH09")
  
  ## only need to include 1 instance of the duplicate
  m09_id_dup <- m09_id_dup %>% unique() 
  
  
  #*bind with other forms
  if (nrow(m09_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m09_id_dup)
  }
}
#****************************************
#* MNH10
#****************************************
if (exists("mnh10")==TRUE){
  
  dup_MOMID_PREGID <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m10_id_dup <- dup_MOMID_PREGID(mnh10)
  
  # export key variables if duplicates exists 
  m10_id_dup <- m10_id_dup %>% select(MOMID, PREGID, FORMCOMPLDAT_MNH10) ## kenya missing date var here 
  
  # add SCRNID column if duplicates exist 
  if (length(m10_id_dup >1)) {
    m10_id_dup = cbind(SCRNID = "NA", m10_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m10_id_dup >1)) {
    m10_id_dup = add_column(m10_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m10_id_dup >1)) {
    names(m10_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m10_id_dup <- add_column(m10_id_dup,Form = "MNH10")
  
  ## only need to include 1 instance of the duplicate
  m10_id_dup <- m10_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m10_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m10_id_dup)
  }
  
}

#****************************************
#* MNH11 -- create new variable names for 
#* duplicates for infants and merge later
#****************************************
if (exists("mnh11")==TRUE){
  
  dup_INFANTID <- function(form) {
    ID <- form %>% 
      select(INFANTID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(INFANTID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m11_id_dup <- dup_INFANTID(mnh11)
  
  # export key variables if duplicates exists 
  m11_id_dup <- m11_id_dup %>% select(MOMID, PREGID,INFANTID, VISIT_OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m11_id_dup >1)) {
    m11_id_dup = cbind(SCRNID = "NA", m11_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m11_id_dup >1)) {
    m11_id_dup = add_column(m11_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m11_id_dup >1)) {
    names(m11_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column 
  m11_id_dup <- add_column(m11_id_dup,Form = "MNH11")
  
  ## only need to include 1 instance of the duplicate
  m11_id_dup <- m11_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m11_id_dup >1)){
    VarNamesDuplicate_Inf <- m11_id_dup
  }
}
#****************************************
#* MNH12
#****************************************
#*
#*PNC_N_VISIT
if (exists("mnh12")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m12_id_dup <- dup_MOM_PREG_TV(mnh12)
  
  # export key variables if duplicates exists 
  m12_id_dup <- m12_id_dup %>% select(MOMID, PREGID,TYPE_VISIT, VISIT_OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m12_id_dup >1)) {
    m12_id_dup = cbind(SCRNID = "NA", m12_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m12_id_dup >1)) {
    names(m12_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m12_id_dup <- add_column(m12_id_dup,Form = "MNH12")
  
  ## only need to include 1 instance of the duplicate
  m12_id_dup <- m12_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m12_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m12_id_dup)
  }
}
#****************************************
#* MNH13
#****************************************
if (exists("mnh13")==TRUE){
  
  dup_INF_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID,INFANTID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID,INFANTID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m13_id_dup <- dup_INF_TV(mnh13)
  
  # export key variables if duplicates exists 
  m13_id_dup <- m13_id_dup %>% select(MOMID, PREGID,INFANTID,TYPE_VISIT, VISIT_OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m13_id_dup >1)) {
    m13_id_dup = cbind(SCRNID = "NA", m13_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m13_id_dup >1)) {
    names(m13_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column 
  m13_id_dup <- add_column(m13_id_dup,Form = "MNH13")
  
  ## only need to include 1 instance of the duplicate
  m13_id_dup <- m13_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m13_id_dup >1)){
    VarNamesDuplicate_Inf <- rbind(VarNamesDuplicate_Inf, m13_id_dup)
  } 
}
#****************************************
#* MNH14
#****************************************
if (exists("mnh14")==TRUE){
  
  dup_INF_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID,INFANTID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID,INFANTID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m14_id_dup <- dup_INF_TV(mnh14)
  
  # export key variables if duplicates exists 
  m14_id_dup <- m14_id_dup %>% select(MOMID, PREGID,INFANTID,TYPE_VISIT, VISIT_OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m14_id_dup >1)) {
    m14_id_dup = cbind(SCRNID = "NA", m14_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m14_id_dup >1)) {
    names(m14_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column 
  m14_id_dup <- add_column(m14_id_dup,Form = "MNH14")
  
  ## only need to include 1 instance of the duplicate
  m14_id_dup <- m14_id_dup %>% unique() 
  
  
  #*bind with other forms
  if (nrow(m14_id_dup >1)){
    VarNamesDuplicate_Inf <- rbind(VarNamesDuplicate_Inf, m14_id_dup)
  } 
}
#****************************************
#* MNH15
#****************************************
if (exists("mnh15")==TRUE){
  
  dup_INF_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID,INFANTID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID,INFANTID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m15_id_dup <- dup_INF_TV(mnh15)
  
  # export key variables if duplicates exists 
  m15_id_dup <- m15_id_dup %>% select(MOMID, PREGID,INFANTID,TYPE_VISIT, OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m15_id_dup >1)) {
    m15_id_dup = cbind(SCRNID = "NA", m15_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m15_id_dup >1)) {
    names(m15_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column
  m15_id_dup <- add_column(m15_id_dup,Form = "MNH15")
  
  ## only need to include 1 instance of the duplicate
  m15_id_dup <- m15_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m15_id_dup >1)){
    VarNamesDuplicate_Inf <- rbind(VarNamesDuplicate_Inf, m15_id_dup)
  } 
}
#****************************************
#* MNH16
#****************************************
if (exists("mnh16")==TRUE){
  
  dup_MOMID_PREGID <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m16_id_dup <- dup_MOMID_PREGID(mnh16)
  
  # export key variables if duplicates exists 
  m16_id_dup <- m16_id_dup %>% select(MOMID, PREGID, VISDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m16_id_dup >1)) {
    m16_id_dup = cbind(SCRNID = "NA", m16_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m16_id_dup >1)) {
    m16_id_dup = add_column(m16_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m16_id_dup >1)) {
    names(m16_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  
  # add form column 
  m16_id_dup <- add_column(m16_id_dup,Form = "MNH16")
  
  ## only need to include 1 instance of the duplicate
  m16_id_dup <- m16_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m16_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m16_id_dup)
  }
}
#****************************************
#* MNH17
#****************************************
if (exists("mnh17")==TRUE){
  
  dup_MOMID_PREGID <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m17_id_dup <- dup_MOMID_PREGID(mnh17)
  
  # export key variables if duplicates exists 
  m17_id_dup <- m17_id_dup %>% select(MOMID, PREGID, VISDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m17_id_dup >1)) {
    m17_id_dup = cbind(SCRNID = "NA", m17_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m17_id_dup >1)) {
    m17_id_dup = add_column(m17_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m17_id_dup >1)) {
    names(m17_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m17_id_dup <- add_column(m17_id_dup,Form = "MNH17")
  
  ## only need to include 1 instance of the duplicate
  m17_id_dup <- m17_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m17_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m17_id_dup)
  }
}
#****************************************
#* MNH18
#****************************************
if (exists("mnh18")==TRUE){
  
  dup_MOMID_PREGID <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m18_id_dup <- dup_MOMID_PREGID(mnh18)
  
  # export key variables if duplicates exists 
  m18_id_dup <- m18_id_dup %>% select(MOMID, PREGID, VISDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m18_id_dup >1)) {
    m18_id_dup = cbind(SCRNID = "NA", m18_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m18_id_dup >1)) {
    m18_id_dup = add_column(m18_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m18_id_dup >1)) {
    names(m18_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m18_id_dup <- add_column(m18_id_dup,Form = "MNH18")
  
  ## only need to include 1 instance of the duplicate
  m18_id_dup <- m18_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m18_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m18_id_dup)
  }
}
#****************************************
#* MNH19
#****************************************
if (exists("mnh19")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, OBSSTDAT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, OBSSTDAT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m19_id_dup <- dup_MOM_PREG_TV(mnh19)
  
  # export key variables if duplicates exists 
  m19_id_dup <- m19_id_dup %>% select(MOMID, PREGID, OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m19_id_dup >1)) {
    m19_id_dup = cbind(SCRNID = "NA", m19_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m19_id_dup >1)) {
    m19_id_dup = add_column(m19_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m19_id_dup >1)) {
    names(m19_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m19_id_dup <- add_column(m19_id_dup,Form = "MNH19")
  
  ## only need to include 1 instance of the duplicate
  m19_id_dup <- m19_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m19_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m19_id_dup)
  }
}
#****************************************
#* MNH20
#****************************************
if (exists("mnh20")==TRUE){
  
  dup_INF_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, INFANTID, OBSSTDAT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, INFANTID,  OBSSTDAT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m20_id_dup <- dup_INF_TV(mnh20)
  
  # export key variables if duplicates exists 
  m20_id_dup <- m20_id_dup %>% select(MOMID, PREGID,INFANTID, OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m20_id_dup >1)) {
    m20_id_dup = cbind(SCRNID = "NA", m20_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m20_id_dup >1)) {
    m20_id_dup = add_column(m20_id_dup, VisitType = NA , .after = "INFANTID")
  }
  
  # rename columns if duplicates exist 
  if (length(m20_id_dup >1)) {
    names(m20_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column 
  m20_id_dup <- add_column(m20_id_dup,Form = "MNH20")
  
  ## only need to include 1 instance of the duplicate
  m20_id_dup <- m20_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m20_id_dup) >1){
    VarNamesDuplicate_Inf <- rbind(VarNamesDuplicate_Inf, m20_id_dup)
  } 
  
}

#****************************************
#* MNH21 -- save as separate form 
#* VarNamesDuplicate_adverse
#****************************************
if (exists("mnh21")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, INFANTID, AESTDAT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID,INFANTID, AESTDAT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m21_id_dup <- dup_MOM_PREG_TV(mnh21)
  
  # export key variables if duplicates exists 
  m21_id_dup <- m21_id_dup %>% select(MOMID, PREGID, INFANTID, AESTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m21_id_dup >1)) {
    m21_id_dup = cbind(SCRNID = "NA", m21_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m21_id_dup >1)) {
    m21_id_dup = add_column(m21_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m21_id_dup >1)) {
    names(m21_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column 
  m21_id_dup <- add_column(m21_id_dup,Form = "MNH21")
  
  ## only need to include 1 instance of the duplicate
  m21_id_dup <- m21_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m21_id_dup >1)){
    VarNamesDuplicate_adverse <-  m21_id_dup
  }
}

#****************************************
#* MNH22
#****************************************
if (exists("mnh22")==TRUE){
  
  dup_INF_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, DVSTDAT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, DVSTDAT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m22_id_dup <- dup_INF_TV(mnh22)
  
  # export key variables if duplicates exists 
  m22_id_dup <- m22_id_dup %>% select(MOMID, PREGID,INFANTID, DVSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m22_id_dup >1)) {
    m22_id_dup = cbind(SCRNID = "NA", m22_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m22_id_dup >1)) {
    m22_id_dup = add_column(m22_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m22_id_dup >1)) {
    names(m22_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column 
  m22_id_dup <- add_column(m22_id_dup,Form = "MNH22")
  
  ## only need to include 1 instance of the duplicate
  m22_id_dup <- m22_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m22_id_dup >1)){
    VarNamesDuplicate_Inf <- rbind(VarNamesDuplicate_Inf, m22_id_dup)
  } 
  
}
#****************************************
#* MNH23
#****************************************
if (exists("mnh23")==TRUE){
  
  dup_MOMID_PREGID <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m23_id_dup <- dup_MOMID_PREGID(mnh23)
  
  # export key variables if duplicates exists 
  m23_id_dup <- m23_id_dup %>% select(MOMID, PREGID, CLOSE_DSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m23_id_dup >1)) {
    m23_id_dup = cbind(SCRNID = "NA", m23_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m23_id_dup >1)) {
    m23_id_dup = add_column(m23_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m23_id_dup >1)) {
    names(m23_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m23_id_dup <- add_column(m23_id_dup,Form = "MNH23")
  
  ## only need to include 1 instance of the duplicate
  m23_id_dup <- m23_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m23_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m23_id_dup)
  }
}
#****************************************
#* MNH24
#****************************************
if (exists("mnh24")==TRUE){
  
  dup_INFANTID <- function(form) {
    ID <- form %>% 
      select(INFANTID)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(INFANTID)
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m24_id_dup <- dup_INFANTID(mnh24)
  
  # export key variables if duplicates exists 
  m24_id_dup <- m24_id_dup %>% select(MOMID, PREGID,INFANTID, CLOSE_DSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m24_id_dup >1)) {
    m24_id_dup = cbind(SCRNID = "NA", m24_id_dup)
  }
  
  # add visit type column  if duplicates exist 
  if (length(m24_id_dup >1)) {
    m24_id_dup = add_column(m24_id_dup,VisitType = NA , .after = "PREGID")
  }
  
  # rename columns if duplicates exist 
  if (length(m24_id_dup >1)) {
    names(m24_id_dup) = c("SCRNID","MOMID", "PREGID","INFANTID","VisitType", "VisitDate")
  }
  
  # add form column 
  m24_id_dup <- add_column(m24_id_dup,Form = "MNH24")
  
  ## only need to include 1 instance of the duplicate
  m24_id_dup <- m24_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m24_id_dup >1)){
    VarNamesDuplicate_Inf <- m24_id_dup
  }
}

#****************************************
#* MNH25
#****************************************
if (exists("mnh25")==TRUE){
  ## replaced TYPE_VISIT with ANC_VISIT_N
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m25_id_dup <- dup_MOM_PREG_TV(mnh25)
  
  # export key variables if duplicates exists 
  m25_id_dup <- m25_id_dup %>% select(MOMID, PREGID,TYPE_VISIT, OBSSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m25_id_dup >1)) {
    m25_id_dup = cbind(SCRNID = "NA", m25_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m25_id_dup >1)) {
    names(m25_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m25_id_dup <- add_column(m25_id_dup,Form = "MNH25")
  
  ## only need to include 1 instance of the duplicate
  m25_id_dup <- m25_id_dup %>% unique() 
  
  #*bind with other forms
  if (nrow(m25_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m25_id_dup)
  }
}
#****************************************
#* MNH26
#****************************************
if (exists("mnh26")==TRUE){
  
  dup_MOM_PREG_TV <- function(form) {
    ID <- form %>% 
      select(MOMID, PREGID, TYPE_VISIT)
    dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
      arrange(MOMID, PREGID, TYPE_VISIT) 
    dupID <-print(dup, n=nrow(dup), na.print=NULL)
    return(dup)
  }
  
  m26_id_dup <- dup_MOM_PREG_TV(mnh26)
  
  # export key variables if duplicates exists 
  m26_id_dup <- m26_id_dup %>% select(MOMID, PREGID,TYPE_VISIT, FTGE_OBSTDAT)
  
  # add SCRNID column if duplicates exist 
  if (length(m26_id_dup >1)) {
    m26_id_dup = cbind(SCRNID = "NA", m26_id_dup)
  }
  
  # rename columns if duplicates exist 
  if (length(m26_id_dup >1)) {
    names(m26_id_dup) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate")
  }
  
  # add form column 
  m26_id_dup <- add_column(m26_id_dup,Form = "MNH26")
  
  ## only need to include 1 instance of the duplicate
  m26_id_dup <- m26_id_dup %>% unique() 
  
  
  #*bind with other forms
  if (nrow(m26_id_dup >1)){
    VarNamesDuplicate <- rbind(VarNamesDuplicate, m26_id_dup)
  }
}



#****************************************
#* For MNH30, 31, 32 and 36
#****************************************
datasets <- c("mnh31", "mnh32", "mnh30", "mnh36")

# Define the function to find duplicates
dup_MOM_PREG_TV <- function(form) {
  ID <- form %>% 
    select(MOMID, PREGID, INFANTID, TYPE_VISIT)
  dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
    arrange(MOMID, PREGID, INFANTID, TYPE_VISIT) 
  dupID <- print(dup, n=nrow(dup), na.print=NULL)
  return(dup)
}

# Loop through each dataset
for (dataset_name in datasets) {
  if (exists(dataset_name)) {
    # Get the dataset
    form <- get(dataset_name)
    
    # Find duplicates
    id_dup <- dup_MOM_PREG_TV(form)
    
    # Export key variables if duplicates exist
    id_dup <- id_dup %>% select(MOMID, PREGID, INFANTID, TYPE_VISIT, VISIT_OBSSTDAT)
    
    # Add SCRNID column if duplicates exist
    if (nrow(id_dup) > 0) {
      id_dup <- cbind(SCRNID = "NA", id_dup)
    }
    
    # Rename columns if duplicates exist
    if (nrow(id_dup) > 0) {
      names(id_dup) <- c("SCRNID", "MOMID", "PREGID", "INFANTID", "VisitType", "VisitDate")
    }
    
    # Add form column
    id_dup <- add_column(id_dup, Form = toupper(dataset_name))
    
    # Only need to include 1 instance of the duplicate
    id_dup <- id_dup %>% unique()
    
    # Bind with other forms
    if (nrow(id_dup) > 0) {
      VarNamesDuplicate_Inf <- rbind(VarNamesDuplicate_Inf, id_dup)
    }
  }
}


# #****************************************
# #* ADJUST FOR VISIT TYPE = 13
# #****************************************
# ## since a woman can have multiple visit type = 13 for each form, we need to find any duplicate women who have the same visit type = 13 AND visit date
# # create new sub dataset so extract visit type 13 
# VarNamesDuplicate_visit13 = VarNamesDuplicate %>%  filter(VisitType == 13)
# 
# dup_visit_13 <- function(form) {
#   ID <- form %>% 
#     select(MOMID, PREGID, VisitDate)
#   dup <- form[duplicated(ID) | duplicated(ID, fromLast = TRUE),] %>% 
#     arrange(MOMID, PREGID, VisitDate) 
#   dupID <-print(dup, n=nrow(dup), na.print=NULL)
#   return(dup)
# }
# 
# # this will produce the list of true duplicate visit type = 13s that have the same visit day 
# dup_visit_13_out <- dup_visit_13(VarNamesDuplicate_visit13)
# 
# # to finalize the duplicate query output, we need to replace the old visit type = 13 queries pull and replace with these new queries 
# # step 1. remove old visit = 13 queries pulled from all of the codes above
# VarNamesDuplicate = VarNamesDuplicate %>% filter(VisitType != 13) ## keep everything but visit type = 13
# # step 2. re-bind the visit type = 13 that we just fixed 
# VarNamesDuplicate = rbind(VarNamesDuplicate, VarNamesDuplicate_visit13)
# 
# # fix random issues with VarNamesDuplicate_Inf visit type class 
# VarNamesDuplicate$VisitType = as.character(VarNamesDuplicate$VisitType)
# 
#****************************************
#* BIND ALL DATA FRAMES 
#****************************************
# 08/04 UPDATE: THERE ARE INSTANCES OF MISSING INFANTIDS IN INFANT FORMS -- ADD NEW ERROR TYPE HERE 
# add infant id column to momid dataframe 

names(VarNamesDuplicate) = c("SCRNID","MOMID", "PREGID","VisitType", "VisitDate", "Form")
VarNamesDuplicate <- add_column(VarNamesDuplicate, INFANTID = NA, .after = "PREGID")

## update variable value column 
VarNamesDuplicate <- VarNamesDuplicate %>% 
  mutate(`Variable Value` = ifelse((Form == "MNH01" & VisitType == 1) | (Form == "MNH00") , SCRNID, MOMID), 
         `Variable Name` = ifelse((Form == "MNH01" & VisitType == 1) | (Form == "MNH00") , "ScrnID", "MomID"),
         FieldType = "Text", 
         EditType = "Duplicate ID"
  )

# fix random issues with VarNamesDuplicate_Inf visit type class 
VarNamesDuplicate_Inf$VisitType = as.character(VarNamesDuplicate_Inf$VisitType)
VarNamesDuplicate$VisitType = as.character(VarNamesDuplicate$VisitType)

## update variable value column 
VarNamesDuplicate_Inf <- VarNamesDuplicate_Inf %>% 
  mutate(`Variable Value` = INFANTID, 
         `Variable Name` = "InfantID",
         FieldType = "Text", 
         EditType = ifelse(is.na(INFANTID),"InfantID missing from infant form", "Duplicate ID")
  )

## bind in duplicate infant ids
VarNamesDuplicate <- bind_rows(VarNamesDuplicate, VarNamesDuplicate_Inf) %>% unique()
VarNamesDuplicate <- VarNamesDuplicate %>% filter(!(is.na(MOMID)))

# update naming
VarNamesDuplicate = VarNamesDuplicate[-1,]
names(VarNamesDuplicate) = c("ScrnID","MomID", "PregID","InfantID", "VisitType", "VisitDate", "Form", "Variable Name", "Variable Value", "FieldType", "EditType")

## add additional columns 
VarNamesDuplicate = cbind(QueryID = NA, 
                          UploadDate = UploadDate, 
                          #MomID = "NA", PregID = "NA",
                          #VisitDate = "NA", 
                          VarNamesDuplicate, 
                          #`Variable Name` = "NA",
                          #`Variable Value` = "NA",
                          #FieldType = "Text", 
                          #EditType = "Duplicate ID", 
                          DateEditReported = format(Sys.time(), "%Y-%m-%d"))
# combine form/edit type var 
VarNamesDuplicate$Form_Edit_Type <- paste(VarNamesDuplicate$Form,"_",VarNamesDuplicate$EditType)

## assign queryid -- edit type id for duplicate IDs is 01 
VarNamesDuplicate <- VarNamesDuplicate %>% 
  mutate(QueryID = paste0(Form, "_", VisitDate, "_",`Variable Name`, "_", `Variable Value`, "_", "01")
  )

duplicates_query <- VarNamesDuplicate

## REMOVE VISIT TYPE = 13 or 14 
duplicates_query <- duplicates_query %>% filter(!VisitType %in% c(13,14) & !(Form %in% c("MNH30", "MNH31", "MNH32")))

#export
save(duplicates_query, file = paste0(path_to_save,"/duplicates_query.rda"))

#*****************************************************************************
#* comparing mom id 
#* This code will check that all moms who are enrolled had an enrollment form 
#*****************************************************************************
## Load in long data  
load(paste0("~/PRiSMAv2Data/", site, "/", UploadDate,"/data/", UploadDate, "_long.Rdata", sep = "")) 

na_variations <- c("n/a", "NA", "N/A", "na", NA, " ", "", ".")

# Check conditions for merging
if ((sum(mnh01$MOMID %in% na_variations | is.na(mnh01$MOMID), na.rm = TRUE) >= 1) | 
    !(all(c("MOMID", "PREGID") %in% names(mnh01)))) {
  
  # Merge data frames by SCRNID
  new_mnh01 <- merge(x = mnh01[, !(names(mnh01) %in% c("MOMID", "PREGID"))],
                     y = mnh02[, c("SCRNID", "MOMID", "PREGID")],
                     by = "SCRNID", all.x = TRUE)
  
  # Check if the merge was successful
  if (nrow(new_mnh01) == 0) {
    print("No data merged. Check if SCRNID exists in both data frames.")
  } else {
    print("Data merged successfully.")
  }
  
} else {
  # Print a message indicating that MOMID and PREGID are present
  print("MOMID and PREGID are present.")
}

## get MOMIDs in enrollment form 
enroll_momid <- data_long %>% filter(form == "MNH02")

enroll_momid_vec <- as.vector(unique(enroll_momid$PREGID))

## get MOMIDs in all forms 
all_momid <- data_long %>% filter(form != "MNH02" & form != "MNH00" & form != "MNH01")

## subset all MOMIDs that have forms 03-25 but not enrollment 
out <- subset(all_momid, !(all_momid$PREGID %in% enroll_momid$PREGID))

## only need the first four columns
out <- out %>% select(SCRNID, MOMID, PREGID, INFANTID, FORM = form, VISITDATE = VisitDate, TYPE_VISIT)  %>% 
               distinct(MOMID, PREGID, INFANTID, FORM, TYPE_VISIT,  .keep_all = TRUE)

## Export out the MOMID together with the forms we see them in
xl_lst <- list( 'MOMID Missing MNH02' = out)

write.xlsx(xl_lst, file = paste0(path_to_save, "/",  site, "_MOMIDNOTMATCHED", ".xlsx"))


## only need to keep 1 instance of the MOMID 
out <- out %>% distinct(MOMID, PREGID, INFANTID,  .keep_all = TRUE) %>% select(SCRNID, MOMID, PREGID, INFANTID)

# rename dataframe 
MomidNotMatched <- out

# update naming
names(MomidNotMatched) = c("ScrnID","MomID", "PregID","InfantID")

# add visit type column 
MomidNotMatched <- add_column(MomidNotMatched, VisitType = NA , .after = "InfantID")

if (dim(MomidNotMatched)[1] >= 1){
  
  ## add additional columns 
  MomidNotMatched_query = cbind(QueryID = NA, 
                                UploadDate = UploadDate, 
                                #MomID = "NA", PregID = "NA",
                                VisitDate = "NA", 
                                MomidNotMatched, 
                                Form = "NA",
                                `Variable Name` = "NA",
                                `Variable Value` = "NA",
                                FieldType = "Text", 
                                EditType = "MomID Missing Enrollment Form", 
                                DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  
  # combine form/edit type var 
  MomidNotMatched_query <- add_column(MomidNotMatched_query,Form_Edit_Type = paste(MomidNotMatched_query$EditType))
  
  # assign momid to the variable value column 
  MomidNotMatched_query <- MomidNotMatched_query %>% 
    mutate(`Variable Value` = MomID, 
           `Variable Name` = "MomID")
  
  ## assign queryid -- edit type id missing enrollment form is 04
  MomidNotMatched_query <- MomidNotMatched_query %>% 
    mutate(QueryID = paste0("MissingMNH02", "_", `Variable Name`, "_", `Variable Value`, "_", "04")
    )
  
  #export Mom ID not matched query 
  save(MomidNotMatched_query, file = paste0(path_to_save,"/MomidNotMatched_query.rda"))
  
}

#*****************************************************************************
#* comparing infant id 
#* This code will check that all infants have a delivery form 
#*****************************************************************************
## Extract infant IDs in MNH09 
deliv_infid <- data_long %>% filter(form == "MNH09")
deliv_infid_vec <- as.vector(unique(deliv_infid$INFANTID))

## get infant IDs in all infant forms 
infant_forms <- c("MNH11", "MNH13", "MNH14", "MNH15","MNH20", "MHNH24", "MNH36")
all_infid <- data_long %>% filter(form %in% infant_forms)

## subset all MOMIDs that have forms 11, 13, 14, 15, 20, & 24,  but not a delivery form 
out<-subset(all_infid, !(all_infid$INFANTID %in% deliv_infid$INFANTID))


## only need the first four columns
out <- out %>% select(SCRNID, MOMID, PREGID, INFANTID)

## only need to keep 1 instance of the MOMID 
out <- out %>% distinct(MOMID, PREGID, INFANTID,  .keep_all = TRUE)

# rename dataframe
InfidNotMatched <- out

# update naming
names(InfidNotMatched) = c("ScrnID","MomID", "PregID","InfantID")

# current output will print every missing infant for each form FOR EACH VISIT - we only need it to print once. Fix below
# we can do this by only select unique observations as defined by a unique momid, pregid, infantid, and form 
# InfidNotMatched <- InfidNotMatched %>% distinct(MomID, PregID,InfantID, Form, .keep_all = TRUE)

# add visit type column 
InfidNotMatched <- add_column(InfidNotMatched, VisitType = NA , .after = "InfantID")

if (dim(InfidNotMatched)[1] >= 1){
  
  ## add additional columns 
  InfidNotMatched = cbind(QueryID = NA, 
                          UploadDate = UploadDate, 
                          #MomID = "NA", PregID = "NA",
                          VisitDate = "NA", 
                          InfidNotMatched, 
                          Form = "NA",
                          `Variable Name` = "NA",
                          `Variable Value` = "NA",
                          FieldType = "Text", 
                          EditType = "InfantID Missing Delivery Form", 
                          DateEditReported = format(Sys.time(), "%Y-%m-%d"))
  
  # combine form/edit type var 
  InfidNotMatched_query <- add_column(InfidNotMatched,Form_Edit_Type = paste(InfidNotMatched$EditType))
  
  # reassing momid to the variable value column 
  InfidNotMatched_query <- InfidNotMatched_query %>% 
    mutate(`Variable Value` = InfantID, 
           `Variable Name` = "InfantID")
  
  ## assign queryid -- edit type id for infant missing delivery form is 07
  InfidNotMatched_query <- InfidNotMatched_query %>% 
    mutate(QueryID = paste0("MissingMNH09", "_",`Variable Name`, "_", `Variable Value`, "_", "07")
    )
  
  
  #export Inf ID not matched query 
  save(InfidNotMatched_query, file = paste0(path_to_save,"/InfidNotMatched_query.rda"))
