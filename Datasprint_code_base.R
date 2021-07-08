library(readr)
library(readxl)
library(plyr)
library(dplyr) # This allows us to use pipes ('%>%') to shorten the code and apply several things to one single object
library(tidyverse)

options( scipen = 999)

# Deprivation ####

# For more information on the English indices of deprivation (2019) see - https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

IMD_2019 <- read_csv('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv') %>% 
  select("LSOA code (2011)",  "Local Authority District name (2019)", "Index of Multiple Deprivation (IMD) Score", "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)", "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)") %>% 
  rename(lsoa_code = 'LSOA code (2011)',
         LTLA = 'Local Authority District name (2019)',
         IMD_2019_score = 'Index of Multiple Deprivation (IMD) Score',
         IMD_2019_rank = "Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)", 
         IMD_2019_decile = "Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)") %>% 
  mutate(IMD_2019_decile = factor(ifelse(IMD_2019_decile == 1, '10% most deprived',  ifelse(IMD_2019_decile == 2, 'Decile 2',  ifelse(IMD_2019_decile == 3, 'Decile 3',  ifelse(IMD_2019_decile == 4, 'Decile 4',  ifelse(IMD_2019_decile == 5, 'Decile 5',  ifelse(IMD_2019_decile == 6, 'Decile 6',  ifelse(IMD_2019_decile == 7, 'Decile 7',  ifelse(IMD_2019_decile == 8, 'Decile 8',  ifelse(IMD_2019_decile == 9, 'Decile 9',  ifelse(IMD_2019_decile == 10, '10% least deprived', NA)))))))))), levels = c('10% most deprived', 'Decile 2', 'Decile 3', 'Decile 4', 'Decile 5', 'Decile 6', 'Decile 7', 'Decile 8', 'Decile 9', '10% least deprived'))) %>% 
  rename(LSOA11CD = lsoa_code) %>% 
  arrange(LSOA11CD) 

# LSOA metadata ####

lsoa_lookup <- read_csv('https://opendata.arcgis.com/datasets/a46c859088a94898a7c462eeffa0f31a_0.csv') %>%
  left_join(read_csv('https://opendata.arcgis.com/datasets/180c271e84fc400d92ca6dcc7f6ff780_0.csv')[c('OA11CD', 'RGN11NM')], by = 'OA11CD') %>% 
  select(LSOA11CD, MSOA11CD, MSOA11NM, LAD20NM, RGN11NM) %>% 
  unique()

se_lsoas <- lsoa_lookup %>% 
  filter(RGN11NM == 'South East')

# We can use this to filter the imd file 

se_imd_2019 <- IMD_2019 %>% 
  filter(LSOA11CD %in% se_lsoas$LSOA11CD) %>% 
  arrange(LSOA11CD)

# Reading GIS layers
library(geojsonio)
library(spdplyr)
library(rmapshaper)

lsoa_spdf <- geojson_read("https://opendata.arcgis.com/datasets/8bbadffa6ddc493a94078c195a1e293b_0.geojson",  what = "sp")

se_lsoa_spdf <- lsoa_spdf %>% 
  filter(LSOA11CD %in% se_lsoas$LSOA11CD) %>% 
  arrange(LSOA11CD)

# Because we have filtered the geojson file, the row numners are slighly out. As a result, we need to do some reworking to export the failsafe file

df <- data.frame(ID = character())
# Get the IDs of spatial polygon
for (i in se_lsoa_spdf@polygons ) { df <- rbind(df, data.frame(ID = i@ID, stringsAsFactors = FALSE))  }
# and set rowname = ID
row.names(se_imd_2019) <- df$ID
# Then use df as the second argument to the spatial dataframe conversion function:
lsoa_boundaries_json <- SpatialPolygonsDataFrame(se_lsoa_spdf, se_imd_2019)  

geojson_write(ms_simplify(geojson_json(lsoa_boundaries_json), keep = 0.2), file = paste0('./South_east_lsoas_imd2019.geojson'))

# Read codes ####

Read_SNOMED_Data <- read_excel("~/GP_profile_work/Read_SNOMED_Data.xlsx", 
                               sheet = "All indicators")

#  filter(str_detect(IndicatorName, "prevalence|[Dd]epri|long-term|nursing|aged")) 

# Diabetes_snomed_codes <- Read_SNOMED_Data %>%  
#   filter(str_detect(read_code_desc, "Diabetes")) %>%
#   mutate(SNOMED_Code = ifelse(SNOMED_Code == 'NULL', NA, SNOMED_Code)) %>% 
#   filter(!(is.na(read_code) & is.na(SNOMED_Code)))
# 

# Diabetes_snomed_codes %>% 
#   write.csv(., './Diabetes_snomed_codes.csv', row.names = FALSE)
# We need to manually screen this and remove duplicates

# These are the codes found on code lists for papers
Diabetes_readcodes <- read_csv('https://raw.githubusercontent.com/psychty/datasprint_J21/main/diabetes_datasprint_readcodes.csv') 

Diabetes_snomed_codes <- read_csv('https://raw.githubusercontent.com/psychty/datasprint_J21/main/Diabetes_snomed_codes.csv')

# Hypertension
# Hypertension_snomed_codes <- Read_SNOMED_Data %>%
#   filter(str_detect(read_code_desc, "Hypertension")) 

# Hypertension_snomed_codes %>%
#   write.csv(., './Hypertension_snomed_codes.csv', row.names = FALSE)
# We need to manually screen this and remove duplicates

# These are the codes found on code lists for papers
Hypertension_readcodes <- read_csv('https://raw.githubusercontent.com/psychty/datasprint_J21/main/diabetes_datasprint_readcodes.csv') 

Hypertension_snomed_codes <- read_csv('https://raw.githubusercontent.com/psychty/datasprint_J21/main/Hypertension_snomed_codes.csv')

# Cancer 
# We have to get this as close as possible
# Cancer_snomed_codes <- Read_SNOMED_Data %>%
#   filter(str_detect(read_code_desc, "neoplasm|tumour|tumor|cancer")) %>% 
#   filter(!str_detect(read_code_desc, "Benign|benign|screen|brain|Brain|lung|Lung|liver|Liver|prostate|skin|ovarian|Ovarian"))

# Cancer_snomed_codes %>%
#   write.csv(., './Cancer_snomed_codes.csv', row.names = FALSE)
# We need to manually screen this and remove duplicates

Cancer_readcodes <- read_csv('https://raw.githubusercontent.com/psychty/datasprint_J21/main/cancer_datasprint_readcodes.csv') 

Cancer_snomed_codes <- read_csv('https://raw.githubusercontent.com/psychty/datasprint_J21/main/Cancer_snomed_codes.csv')