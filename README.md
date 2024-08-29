# PRISMA-Data-Codes
## Description
This repository contains all PRISMA codes uploaded by GW team that will be shared and used by sites. We ask that each site please **create their own fork of this repository (tutorial at the end of this page)** and upload their code and output there. 

#### :pushpin: *Updated on 29 August:* 
    - Updated duplicate ID and core variable checking codes in Stata. 
    - Added updated data dictionary (v2.5 MAY062024)

## Folder Structure
**R Query Codes** 
   - Description: Query codes in R language. These are the same codes that are run by GW data team to generate weekly query reports. 
   - Date codes last updated: 13 March 2023 
   - Codes: 
     - **`00_DataImport_PRiSMA4a_MatDataquery.R`** 
     - **`01_CoreVar_PRiSMA4a_Mat_Dataquery.R`** 
     - **`02_DupID_PRiSMA4a_Mat_Dataquery.R`** 
     - **`03_OutRange_PRiSMA4a_Mat_Dataquery.R`** 
     - **`04_VisitType_PRiSMA4a_Dataquery.R`** 
     - **`05_DataExport_PRiSMA4a_Dataquery.R`** 
   - Documents: 
     - *PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023_Queries.xlsx*
     - *PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023_Queries.xlsx*
     - *fetal_biometry_range.xlsx*
     
**Stata Query Codes** 
#### :pushpin: *GW team currently working on translating all R codes to Stata*
   - Description: Query codes in Stata language. 
   - Date codes last updated: 23 April 2023 
   - Codes: 
     - **`PRiSMA1a_daily_duplicates_check.do`** 
     - **`PRiSMA1b_core_variables_check.do`** 
   - Documents: 
     - *PRiSMA-MNH-Data-Dictionary-Repository-V.2.3-MAR272023_Queries.xlsx*
     - *PRiSMA-MNH-Data-Dictionary-Repository-V.2.2-FEB012023_Queries.xlsx* 
     - Example Output: 
       - *Core variable check_20230221.smcl*
       - *Core variable check_20230221.pdf*

##  **How To Fork A Respository** 
### :pushpin: 1\. **Open your Github repository and click the “fork” button in the top right corner**
![image](https://github.com/PRiSMA-Study/PRiSMA-Data-Queries-Sites/assets/107492595/8f364b2e-48f8-4192-af8c-af1df9841d33)

### :pushpin: 2\. **A window will pop up asking if you would like to create a new fork. Click “create fork”**
![image](https://github.com/PRiSMA-Study/PRiSMA-Data-Queries-Sites/assets/107492595/5c57226d-b764-41c3-b673-7883336b5065)

### :pushpin: 2\. **You will now be in your own forked repository, which is essentially a copy of the main
branch. Any site-specific updates to codes can be uploaded to your forked repository**
![image](https://github.com/PRiSMA-Study/PRiSMA-Data-Queries-Sites/assets/107492595/149d55aa-1dd9-4b6c-92cb-74eee68d9705)




