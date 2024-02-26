# PRISMA Data Query Codes
This repository contains all PRISMA codes that will be used internally and by sites. Right now this folder holds all of the query codes 
#### :pushpin: *Last updated on 26 February 2024*

## File Structure: 
* The query codes in this repository follow a similar naming structure as Synapse with the main folder being the date of upload in yyyy-mm-dd format 
* The code will export data to two folders:
  1\. data: where wide and long data will be stored 
  2\. queries: where each query output from the codes will be stored
* An example of the overall naming structure compatible with the codes is displayed below -
  - ### Main folder: 2023-04-23
  	- ### Sub folder: data
  	- ### Sub folder: queries
 
     
## Queries Included: 
**1\. `00_DataImport_MatDataquery.R`** 
   - input: All MNH raw data
   - output:
     - Two .RData files of the data - wide and long 
   - function: 
     - Import raw data from sites uploaded to synapse 
				
**2\. `01_CoreVar_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all missing or extra variable names 
   - function: 
     - Check to make sure all variables exist in the data and match data dictionary formatting 

**3\. `02_DupID_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all duplicate IDs 
     - One .rda file with all MomIDs missing enrollment form
   - function: 
     - Identify any duplicate IDs 
     - Idenity any MomIDs that are in the study (have forms MNH03-MNH25) but are missing an enrollment form (MNH02) 


**4\. `03_OutRange_Dataquery.R`** 
   - input: Long data 
   - output:
     - One .rda file with all out of range values 
   - function: 
     - Check for any out of range values  

**5\. `04_VisitType_Dataquery.R`** 
   - input: Wide data (all raw .csv files)
   - output:
     - One .rda file with all mismatched visit types to include in query report tab
     - One .rda file with all mismatched visit types + additional information to include as an extra tab in query report
     - One .rda file with all instances of visit types occurring on the same day
   - function: 
     - Check for visit types that do not match the PRISMA window and gestational age at time of visit

**5\. `05_MissingEnrollCrit_Dataquery.R`** 
   - input: Wide data (all raw .csv files) and Long data
   - output:
     - One .rda file with all MOMIDs that do not meet enrollment criteria 
   - function: 
     - Confirm all enrolled particpants meet our enrollment criteria as in MNH02.

**6\. `07_EddGA_Dataquery.R`** 
   - input: Wide data (all raw .csv files) and Long data
   - output:
     - One .rda file with all MOMIDs that have discrepancies between ultrasound reported values and LMP
   - function: 
     - Identifies the differences between Ultrasound reported values and LMP (last menstral period) reported values.

**7\. `DataExport_Dataquery.R`** 
   - input: All .rds files 
   - output:
     - Excel sheet with a full query report 
   - function: 
     - Merge all queries together and assign query ID
    
**Documents in this repository:** 
   - Current data dictionary (v2.3) (filename: PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023.xlsx)
   - Excel file with fetal biometry ranges. This is required for the out of range codes (filename: fetal_biometry_range.xlsx)
   - PRISMA Query Report Template (filename: PRISMA-Query-Template-07Sept2023.xlsx)
   - PRISMA Non-Query Template (filename: PRISMA-Non-Queries-template-07Sept2023.xlsx)
