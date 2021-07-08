# DISCUS datasprint 9th July 2021

## Introduction

This repository contains some documents which may be useful for the unlocking data Sussex Integrated Dataset (SID) datasprint.

We will be exploring data from the SID and asking questions about the population, their demographics, and diagnoses. To this end, a few codelists are available here to try to find patients with certain conditions. These have been collated using lists on https://portal.caliberresearch.org and https://clinicalcodes.rss.mhs.man.ac.uk :

| Condition    | Filename                              | Comment                                                                      |
| ------------ | ------------------------------------- | ---------------------------------------------------------------------------- |
| Hypertension | hypertension_datasprint_readcodes.csv |
| Cancer       | cancer_datasprint_readcodes.csv       | This includes breast, cervical, and colorectal malignant neoplasms           |
| Diabetes     | diabetes_datasprint_readcodes.csv     | This includes type 1 and type 2 diabetes, as well as codes for complications |

We have also searched a recent extract from SID containing unique read codes.

| Condition    | Filename                      | Comment                                                                      |
| ------------ | ----------------------------- | ---------------------------------------------------------------------------- |
| Hypertension | Hypertension_snomed_codes.csv |
| Cancer       | Cancer_snomed_codes.csv       | This includes breast, cervical, and colorectal malignant neoplasms           |
| Diabetes     | Diabetes_snomed_codes.csv     | This includes type 1 and type 2 diabetes, as well as codes for complications |

### The readcode lists may not be complete and may include codes (such as screening, or family history, or condition in remission) which may need to be excluded from queries. We can make these decisions on the day.

We may also want to explore aspects related to the GP practices, as well as where people live and so it may be helpful to have some information to lookup around neighbourhoods (Lower-layer super output area) and residential based deprivation

| Item                           | Filename                         | Comment                                                                                                                                         |
| ------------------------------ | -------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| GP practice metadata           | epraccur_cleaned.csv             | This is a list of all primary care organisations, and their addresses, from NHS Digital's Organisational Data Service (as at May 2021)          |
| LSOAs in the South East region | South_east_lsoas_imd2019.geojson | This is a geojson file of the 5,000+ lsoas in the south east region with information on the 2019 English index of multiple deprivation attached |
| Datasprint code bases          | Datasprint_code_base.R           | This is an R script with a few lines of code for some tasks                                                                                     |
