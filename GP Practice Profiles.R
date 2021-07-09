
# Task #### 
# what do we know about the practice populations currently in SID. 
# Are there data sets we can use to get an idea of how complete SID data are

# Packages ####

# You only need to install packages once but you need to load them every time you start a new R 'session' and so when I am starting a new script I tend to use a package called easypackages (as below) to specify all the packages I want to load as it makes the code shorter but you can use library() to load packages individually. I think it might be easier to load the packages throughout the script as we need them so you can see what does what

# library(easypackages)
# libraries("readxl", "readr", "plyr", "dplyr", "ggplot2", "rgdal", 'rgeos', "tmaptools", 'sp', 'sf', 'maptools', 'leaflet', 'leaflet.extras', 'spdplyr', 'geojsonio', 'rmapshaper', 'jsonlite', 'grid', 'aweek', 'xml2', 'rvest', 'officer', 'flextable', 'viridis', 'epitools', 'PostcodesioR', 'showtext', 'httr', 'ggpol')

# Installing packages at ESCC ####
# you can use install.packages('') to install anything that isn't on your machine already. 

# When trying to install packages I get a warning message about RTools ('WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:'). Rtools is another piece of software, but we dont need it to load the packages we need at the moment and can ignore.


# I have found that I get an error ('Error in install.packages : ERROR: failed to lock directory '\\eschdata\RichT$\R\win-library\4.0' for modifying Try removing '\\eschdata\RichT$\R\win-library\4.0/00LOCK')

# To get around this I can set the options of R to ignore the lock directory when installing new packages
# options("install.lock" = FALSE)
# install.packages('lemon') # lemon is a real package by the way, it supports facetting (small multiples) plot axis labelling
# You only need to do that options() thing once and only if you're installing packages. You do not need it to load packages already installed.

# Loading some data ####

# You can use relative file paths to say where stuff is, so for us sharing a script it might be easier to use those rather than hardcoding directory/richt/
# You can use some base functions to find out if directories exist, and use if() functions if they don't you can create them.

# Does a directory called GP_profile_work exist in your current working directory
dir.exists('~/GP_profile_work')
# This will return TRUE or FALSE

# You can use this TRUE/FALSE value
if(dir.exists('~/GP_profile_work') == TRUE){
  print(paste0('The GP profile directory is already in your working directory'))
}

if(dir.exists('~/GP_profile_work') == FALSE){
  print(paste0('The GP profile directory is not in your working directory. R has just created it'))
dir.create('~/GP_profile_work')
}

# I have used another script to get some data out of the Organisation Data Service from NHS Digital and clean up some of the fields for primary care organisations. ODS updates once a quarter i think so it is good to periodically rerun the gpraccur cleaning stuff to capture new practices/changes etc. 
# I am using a file called epraccur_cleaned.csv which needs to be in the GP_profile_work
if(file.exists('~/GP_profile_work/epraccur_cleaned.csv') == FALSE){
  print(paste0('The GP practice cleaned ODS metadata file is not in the GP_profile_work directory'))
}

# I also saved the table from Jon Elsom's data dictionary detailing the organisations currently in the dataset as a csv 'SID_orgs_in_data.csv'
if(file.exists('~/GP_profile_work/SID_orgs_in_data.csv') == FALSE){
  print(paste0('The SID_orgs_in_data.csv file is not in the GP_profile_work directory'))
}

# Also specify a location for outputs, we can change this when we have somewhere else
output_directory <- '~/GP_profile_work'

# Basic practice info #### 
library(readxl) # xlsx reading
library(readr) # csv reading

epraccur <- read_csv('~/GP_profile_work/epraccur_cleaned.csv')

# we might want to make the epraccur object even tidier 
names(epraccur) # What headings are in epraccur
epraccur1 <- epraccur[c('Code', 'Name', 'Address_1', 'Postcode', 'Commissioner', 'Health_geography')] # create a subset object called epraccur1 which has six named fields
epraccur1 <- subset(epraccur1, Commissioner %in% c('09D', '97R', '70F')) # Keep only Sussex commisioners

library(plyr)
library(dplyr) # This allows us to use pipes ('%>%') to shorten the code and apply several things to one single object
library(tidyverse)

epraccur1 <- epraccur %>% 
  filter(Commissioner %in% c('09D', '97R', '70F')) %>% 
  mutate(Address = gsub('NANA', '', paste(Address_1, Address_2, Address_3, Address_4, sep = ', '))) %>% # here I concatenated the address fields, and I also got rid of any NANA values using gsub which is a find and replace command
  select(Code, Address, Postcode, Commissioner, Status, Prescribing_setting)

# You can combine this with loading and joining data
SID_primary_care_orgs <- read_csv('~/GP_profile_work/SID_orgs_in_data.csv') %>% 
  left_join(epraccur1, by = c('ODS_Code' = 'Code')) %>% 
  filter(Prescribing_setting == 'GP Practice')

# So now we have address details for our 19 practices.
# We can do some more stuff with this in R like plotting where they are across the counties

# GIS ####
library(PostcodesioR)

# We can use a package called PostcodesioR to get lat/long coordinates for our GP practices. The PostcodesioR is just a wrapper function to access the latest ONS Postcode directory. It does not go down to household level geolocation but postcode is good enough for seeing roughly where practices are.

# You could download the ONS postcode directory from the OpenGeographyPortal but for 19 postcodes this will be fine

postcode_lookup('TN34 3TW') # returns a lot of stuff we dont need so we can use the select function to return just what we want

postcode_lookup('TN34 3TW') %>% 
  select(postcode, longitude, latitude)

# The above just gives us a single result for a specified postcode but we can loop through the SID_primary_care_orgs table to get data for each postcode.

# Create a blank table to store the values
lookup_result <- data.frame(postcode = character(), longitude = double(), latitude = double())

# Loop through each row of the SID_primary_care_orgs
for(i in 1:nrow(SID_primary_care_orgs)){
  lookup_result_x <- postcode_lookup(SID_primary_care_orgs$Postcode[i]) %>% 
    select(postcode, longitude, latitude)
  
  lookup_result <- lookup_result_x %>% 
    bind_rows(lookup_result)
}

# Now we can join the lookup_result to the SID_primary_care_orgs 
SID_primary_care_orgs <- SID_primary_care_orgs %>% 
  left_join(lookup_result, by = c('Postcode' = 'postcode')) 

# Reading GIS layers
library(geojsonio)
library(spdplyr)

lad_boundary <- geojson_read('https://opendata.arcgis.com/datasets/69cd46d7d2664e02b30c2f8dcc2bfaf7_0.geojson', what = 'sp') %>% 
  filter(LAD19NM %in% c('Brighton and Hove','Eastbourne','Hastings','Lewes','Rother', 'Wealden','Adur', 'Arun', 'Chichester','Crawley','Horsham','Mid Sussex', 'Worthing'))

# We can create interactive maps using a package called leaflet
library(leaflet)

leaflet() %>%  
  addControl(paste0("<font size = '1px'><b>GP Practices currently onboarded to the Sussex Integrated Dataset (SID), as at ", format(Sys.Date(), '%d %B %Y'),"</font>"),
             position='topright') %>% 
  addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br>Contains Royal Mail data &copy Royal Mail copyright and database right 2020<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons and click on an dot to find out more.') %>%
  addPolygons(data = lad_boundary,
              fillColor = '#dbdbdb',
              stroke = TRUE, 
              color = "#000000",
              weight = 1,
              fillOpacity = .25,
              popup = lad_boundary$LAD19NM,
              group = "Show lower tier Local Authorities") %>% 
  addCircleMarkers(lng = SID_primary_care_orgs$longitude,
                   lat = SID_primary_care_orgs$latitude,
                   radius = 3.5,
                   color = '#000000',
                   fillColor = 'red',
                   stroke = TRUE,
                   weight = .75,
                   fillOpacity = 1,
                   popup = paste0(SID_primary_care_orgs$Name, '<br><br>', SID_primary_care_orgs$Address, '<br>', SID_primary_care_orgs$Postcode),
                   group = 'Show GP practices in SID') %>% 
  addScaleBar(position = "bottomleft") %>% 
  addMeasure(position = 'bottomleft',
             primaryAreaUnit = 'sqmiles',
             primaryLengthUnit = 'miles') %>% 
  addLayersControl(
    overlayGroups = c('Show GP practices in SID'),
    options = layersControlOptions(collapsed = FALSE)) 

# GP registered population ####

# We do not know the exact url we need for this because it is updated every month.
# But we can use some webscraping pages to find the right url

library(httr)
library(rvest)

# This isn't the exact page we want but it does say what the latest publication date (and subsequent url) is
calls_patient_numbers_webpage <- read_html('https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice') %>%
  html_nodes("a") %>%
  html_attr("href")

# we know the actual page we want has a url which starts with the following string, so reduce the scraped list above to those which include it
calls_patient_numbers_webpage <- unique(grep('/data-and-information/publications/statistical/patients-registered-at-a-gp-practice', calls_patient_numbers_webpage, value = T))

# We also know that the top result will be the latest version (even though the second result is the next upcoming version)
calls_patient_numbers_webpage <- read_html(paste0('https://digital.nhs.uk/',calls_patient_numbers_webpage[1])) %>%
                                             html_nodes("a") %>%
                                             html_attr("href")

# Now we know that the file we want contains the string 'gp-reg-pat-prac-quin-age.csv' we can use that in the read_csv call.
# I have also tidied it a little bit by renaming the Sex field and giving R some meta data about the order in which the age groups should be
latest_gp_practice_numbers <- read_csv(unique(grep('gp-reg-pat-prac-quin-age.csv', calls_patient_numbers_webpage, value = T))) %>% 
  mutate(Sex = factor(ifelse(SEX == 'FEMALE', 'Female', ifelse(SEX == 'MALE', 'Male', ifelse(SEX == 'ALL', 'Persons', NA))), levels = c('Female', 'Male'))) %>%
  mutate(Age_group = factor(paste0(gsub('_', '-', AGE_GROUP_5), ' years'), levels = c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80-84 years', '85-89 years', '90-94 years', '95+ years'))) %>% 
  filter(ORG_CODE %in% SID_primary_care_orgs$ODS_Code) %>% 
  filter(AGE_GROUP_5 != 'ALL') %>% 
  filter(Sex != 'Persons') %>% 
  rename(ODS_Code = ORG_CODE,
         Patients = NUMBER_OF_PATIENTS) %>% 
  left_join(SID_primary_care_orgs[c('ODS_Code', 'Name', 'LTLA')], by = 'ODS_Code') %>% 
  select(ODS_Code, Name, Sex, Age_group, Patients, LTLA) %>% 
  group_by(ODS_Code, Name) %>% 
  mutate(Proportion = Patients / sum(Patients)) %>%  # We may also want to standardise the pyramid to compare bigger and smaller practices by their age structure
  ungroup()

# You can use the group_by and summarise functions to crosstabulate
latest_gp_practice_numbers %>% 
  group_by(ODS_Code, Name, Age_group) %>% 
  summarise(n())

# This will count the number of records for each group, so we should have two records, one for males and one for females for each age group in a practice

# We can check the proportion calculation worked
latest_gp_practice_numbers %>% 
  filter(ODS_Code == SID_primary_care_orgs$ODS_Code[1]) %>% # SID_primary_care_orgs$ODS_Code[1] is the first record
  summarise(sum(Proportion))

latest_gp_practice_numbers %>% 
  group_by(Sex) %>%
  summarise(Patients = sum(Patients, na.rm = TRUE))

latest_gp_practice_numbers %>% 
  group_by(Sex) %>%
  summarise(Patients = sum(Patients, na.rm = TRUE))

latest_gp_practice_numbers %>% 
  group_by(LTLA) %>%
  summarise(Patients = sum(Patients, na.rm = TRUE))

unique(latest_gp_practice_numbers$Age_group)

latest_gp_practice_numbers %>% 
  mutate(Age = ifelse(Age_group %in% c('0-4 years', '5-9 years','10-14 years','15-19 years'), '0-19 years', ifelse(Age_group %in% c('20-24 years','25-29 years','30-34 years','35-39 years'), '20-39 years', ifelse(Age_group %in% c('40-44 years','45-49 years', '50-54 years','55-59 years', '60-64 years'), '40-64 years', ifelse(Age_group %in% c('65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90-94 years','95+ years'), '65 and over', NA))))) %>% 
  group_by(Sex, Age) %>% 
  summarise(Patients = sum(Patients, na.rm = TRUE)) %>% 
  pivot_wider(values_from = Patients,
              names_from = Sex)

# Population pyramid ####

# Let's get an overall population in SID 
overall_orgs_in_SID <- latest_gp_practice_numbers %>% 
  group_by(Age_group, Sex) %>% 
  summarise(Patients = sum(Patients, na.rm = TRUE),
            Name = 'All practices in SID') %>% 
  ungroup() %>% 
  mutate(Proportion = Patients / sum(Patients)) %>%
  mutate(Proportion_t = ifelse(Sex == 'Female', 0 - Proportion, Proportion)) %>% # To plot these pyramids we need to transform the data slightly so that the axes are reversed.
  mutate(Patients_t = ifelse(Sex == 'Female', 0 - Patients, Patients))

# We need to do some extra stuff on labels
library(scales)

# Create a custom function to remove the negative sign on the axis labelling though
abs_comma <- function (x, ...) {
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)}

# You can also hijack the percent function and return axes labels with "%" as a suffix (or you could do any suffix)
abs_percent <- function (x, ...) {
  if (length(x) == 0)
    return(character())
  paste0(comma(abs(x) * 100), "%")}

pyramid_theme <- function(){
  theme(plot.background = element_rect(fill = "white", colour = "#ffffff"),
        panel.background = element_rect(fill = "white"),
        axis.text = element_text(colour = "#000000", size = 7),
        plot.title = element_text(colour = "#000000", face = "bold", size = 9, vjust = 1),
        plot.subtitle = element_text(colour = "#000000", face = "italic", size = 8, vjust = 1),
        axis.title = element_text(colour = "#000000", face = "bold", size = 8),
        panel.grid.major.x = element_line(colour = "#E2E2E3", linetype = "longdash", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(colour = "#000000", size = 7, face = 'bold'),
        strip.background = element_rect(fill = "#ffffff"),
        axis.ticks = element_line(colour = "#000000"),
        legend.position = 'none',
        plot.margin = margin(t = 0, r = 1, b = 0,l = 0, unit = 'cm'))}

# Pyramid X - we can later supply R with the appropriate table we want (e.g. loop through practices). 
pyramid_df_final <- overall_orgs_in_SID

# pyramid_df_final <- latest_gp_practice_numbers %>% 
#   filter(ODS_Code == SID_primary_care_orgs$ODS_Code[1]) %>% 
#   mutate(Proportion = Patients / sum(Patients)) %>%
#   mutate(Proportion_t = ifelse(Sex == 'Female', 0 - Proportion, Proportion)) %>% # To plot these pyramids we need to transform the data slightly so that the axes are reversed.
#   mutate(Patients_t = ifelse(Sex == 'Female', 0 - Patients, Patients))

# Get the max bar length
pyramid_x_df_limit <- as.numeric(pyramid_df_final %>% 
                                   filter(Patients == max(Patients, na.rm = TRUE)) %>% 
                                   select(Patients) %>% 
                                   unique())

# We can use round_any() to round this up, but we may want to round up to a different break point depending on the practice (or if we're doing all practices together)
pyramid_x_df_limit <- ifelse(pyramid_x_df_limit < 90, round_any(pyramid_x_df_limit, 10, ceiling), ifelse(pyramid_x_df_limit <= 200, round_any(pyramid_x_df_limit, 20, ceiling), ifelse(pyramid_x_df_limit <= 500, round_any(pyramid_x_df_limit, 50, ceiling), ifelse(pyramid_x_df_limit <= 1000, round_any(pyramid_x_df_limit, 100, ceiling), ifelse(pyramid_x_df_limit <= 5000, round_any(pyramid_x_df_limit, 500, ceiling), ifelse(pyramid_x_df_limit <= 10000, round_any(pyramid_x_df_limit, 1000, ceiling), round_any(pyramid_x_df_limit, 2000, ceiling)))))))

# Same for breaks
pyramid_x_breaks <- ifelse(pyramid_x_df_limit <= 100, 10, ifelse(pyramid_x_df_limit <= 200, 20,  ifelse(pyramid_x_df_limit <= 500, 50,  ifelse(pyramid_x_df_limit <= 1000, 100,  ifelse(pyramid_x_df_limit <= 5000, 500,  ifelse(pyramid_x_df_limit <= 8000, 1000, 2000))))))

pyramid_x_perc_limit <- as.numeric(pyramid_df_final %>% 
                                   filter(Proportion == max(Proportion, na.rm = TRUE)) %>% 
                                   select(Proportion) %>% 
                                   unique())

pyramid_x_perc_limit <- round_any(pyramid_x_perc_limit, .05, ceiling)
pyramid_x_perc_breaks <- ifelse(pyramid_x_perc_limit <= .1, .01, .02)

male_total <- pyramid_df_final %>% 
    filter(Sex == 'Male') %>% 
    summarise(Patients = sum(Patients, na.rm = TRUE))
  
female_total <- pyramid_df_final %>% 
    filter(Sex == 'Female') %>% 
    summarise(Patients = sum(Patients, na.rm = TRUE))
  
area_x_dummy <- pyramid_df_final %>% 
    mutate(Patients = ifelse(Sex == 'Female', 0 - pyramid_x_df_limit, pyramid_x_df_limit)) 
  
# we need facet_share() from the ggpol package
library(ggpol)
  
area_x_pyramid_plot <-  ggplot(data = pyramid_df_final, aes(x = Age_group, y = Patients_t, fill = Sex)) +
    geom_bar(data = area_x_dummy, aes(x = Age_group,
                                      y = Patients),
             stat = "identity",
             fill = NA) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values =  c("#841f27", "#354e71")) +
    labs(x = '',
         y = 'Number of registered patients',
         title = paste0('Number of patients registered: ', unique(pyramid_df_final$Name)),
         subtitle = paste0('Data correct as at ', format(Sys.Date(), '%d %B'))) +
    facet_share(~Sex,
                dir = "h",
                scales = "free",
                switch = 'both',
                reverse_num = FALSE) +
    scale_y_continuous(labels = abs_comma,
                       breaks = seq(0-pyramid_x_df_limit, pyramid_x_df_limit, pyramid_x_breaks)) +
    coord_flip() +
    pyramid_theme()
  
  png(paste0(output_directory,'/Population_registered_', gsub(' ', '_', unique(pyramid_df_final$Name)),'.png'), width = 1050,height = 850, res = 180)
  print(area_x_pyramid_plot)
  dev.off()
  
  area_x_perc_dummy <- pyramid_df_final %>% 
    mutate(Proportion = ifelse(Sex == 'Female', 0 - pyramid_x_perc_limit, pyramid_x_perc_limit)) 
  
area_x_perc_pyramid_plot <- ggplot(data = pyramid_df_final, aes(x = Age_group, y = Proportion_t, fill = Sex)) +
    geom_bar(data = area_x_perc_dummy, aes(x = Age_group,
                                      y = Proportion),
             stat = "identity",
             fill = NA) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values =  c("#841f27", "#354e71")) +
    labs(x = '',
         y = 'Number of registered patients',
         title = paste0('Proportion of patients registered: ', unique(pyramid_df_final$Name)),
         subtitle = paste0('Data correct as at ', format(Sys.Date(), '%d %B'))) +
    facet_share(~Sex,
                dir = "h",
                scales = "free",
                switch = 'both',
                reverse_num = FALSE) +
    scale_y_continuous(labels = abs_percent,
                       breaks = seq(0-pyramid_x_perc_limit, pyramid_x_perc_limit, pyramid_x_perc_breaks)) +
    coord_flip() +
    pyramid_theme()
  
png(paste0(output_directory,'/Population_registered_', gsub(' ', '_', unique(pyramid_df_final$Name)),'_proportion.png'), width = 1050,height = 850, res = 130)
  print(area_x_perc_pyramid_plot)
dev.off()

# Deprivation at LSOA level ####

# We only get lsoa data from the April release of GP patient numbers

# download.file('https://files.digital.nhs.uk/52/2D964D/gp-reg-pat-prac-lsoa-male-female-Apr-21.zip', '~/GP_profile_work/GP_reg_lsoas.zip', mode = 'wb')
# unzip('~/GP_profile_work/GP_reg_lsoas.zip', exdir = "./GP_profile_work") # unzip the folder into the directory

lsoa_covered_extracted <- as.character(read_csv('~/GP_profile_work/gp-reg-pat-prac-lsoa-all.csv') %>% 
  select(EXTRACT_DATE) %>% 
  unique())

lsoa_covered <- read_csv('~/GP_profile_work/gp-reg-pat-prac-lsoa-all.csv') %>% 
  filter(PRACTICE_CODE %in% SID_primary_care_orgs$ODS_Code) %>% 
  rename(ODS_Code = PRACTICE_CODE,
         Patients = `Number of Patients`,
         LSOA11CD = LSOA_CODE) %>% 
  select(ODS_Code, LSOA11CD, Patients) %>% 
  left_join(SID_primary_care_orgs[c('ODS_Code', 'Name')], by = 'ODS_Code') %>% 
  select(ODS_Code, Name, LSOA11CD, Patients) %>% 
  ungroup()

overall_lsoas_covered_by_SID <- lsoa_covered %>% 
  group_by(LSOA11CD) %>% 
  summarise(Patients = sum(Patients, na.rm = TRUE))

# Read in the lsoa geojson boundaries for our lsoas (actually this downloads all 30,000+ and then we filter)
lsoa_spdf <- geojson_read("https://opendata.arcgis.com/datasets/8bbadffa6ddc493a94078c195a1e293b_0.geojson",  what = "sp") %>%
  filter(LSOA11CD %in% overall_lsoas_covered_by_SID$LSOA11CD) 

overall_lsoas_covered_by_SID_spdf <- lsoa_spdf %>% 
  left_join(overall_lsoas_covered_by_SID, by = 'LSOA11CD') %>% 
  filter(Patients >= 10)

# Deprivation ####

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

lsoas_in_SID_IMD_spdf <- lsoa_spdf %>% 
  left_join(IMD_2019, by = 'LSOA11CD') %>% 
  left_join(overall_lsoas_covered_by_SID, by = 'LSOA11CD') %>% 
  filter(Patients >= 10)

# Number and proportion of patients we believe are in SID by neighbourhood deprivation decile (England ranks)
lsoas_in_SID_IMD <- overall_lsoas_covered_by_SID %>% 
  left_join(IMD_2019, by = 'LSOA11CD')

# overall_lsoas_covered_by_SID %>% view()

lsoas_in_SID_IMD_summary <- lsoas_in_SID_IMD  %>% 
  group_by(IMD_2019_decile) %>% 
  summarise(Patients = sum(Patients, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Proportion = Patients / sum(Patients, na.rm = TRUE))

lsoas_in_SID_IMD_summary %>% write.csv(., './dep_patients.csv')

deprivation_colours <- c("#0000FF",  "#2080FF", "#40E0FF", "#70FFD0",  "#90FFB0",  "#C0E1B0",  "#E0FFA0",  "#E0FF70",  "#F0FF30",  "#FFFF00")

deppal <- colorFactor(
  palette = deprivation_colours,
  domain = lsoas_in_SID_IMD$IMD_2019_decile)

# Improving the leaflet map ####

# We can add the lsoa layer to our existing leaflet code
pal <- colorNumeric(
  palette = rev(c('#000004','#56106E','#BB3754','#F98C0A','#FCFFA4')),
  domain = overall_lsoas_covered_by_SID_spdf$Patients)

# Data_sprint_leaflet <-
  leaflet(overall_lsoas_covered_by_SID_spdf) %>%  
  addControl(paste0("<font size = '1px'><b>GP Practices currently onboarded to the Sussex Integrated Dataset (SID), as at ", format(Sys.Date(), '%d %B %Y'),"</font><br><font size = '1px'>LSOA data as at 1st April 2021.</font>"),
             position='topright') %>% 
  addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png',attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br>Contains Royal Mail data &copy Royal Mail copyright and database right 2020<br>Deprivation data source: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019<br>Registered patients data source: https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons and click on an dot to find out more.') %>%
  addPolygons(data = lad_boundary,
              fillColor = '#dbdbdb',
              stroke = TRUE, 
              color = "#000000",
              weight = 1,
              fillOpacity = .25,
              popup = lad_boundary$LAD19NM,
              group = "Show lower tier Local Authorities") %>% 
  addPolygons(data = overall_lsoas_covered_by_SID_spdf,
              stroke = TRUE,
              color = '#000000',
              fillColor = ~pal(Patients),
              weight = 1,
              fillOpacity = .85,
              popup = paste0(overall_lsoas_covered_by_SID_spdf$LSOA11NM, '<br><br>Total patients: ', format(overall_lsoas_covered_by_SID_spdf$Patients, big.mark = ',')),
              group = "Show lower super output areas") %>% 
  addPolygons(data = lsoas_in_SID_IMD_spdf,
              stroke = TRUE,
              color = '#000000',
              fillColor = ~deppal(IMD_2019_decile),
              weight = 1,
              fillOpacity = .85,
              popup = paste0(lsoas_in_SID_IMD_spdf$LSOA11NM, '<br><br>Decile: ', lsoas_in_SID_IMD_spdf$IMD_2019_decile),
              group = "Show lower super output area deprivation") %>% 
  addCircleMarkers(lng = SID_primary_care_orgs$longitude,
                   lat = SID_primary_care_orgs$latitude,
                   radius = 5,
                   color = '#ffffff',
                   fillColor = '#000000',
                   stroke = TRUE,
                   weight = .75,
                   fillOpacity = 1,
                   popup = paste0(SID_primary_care_orgs$Name, '<br><br>', SID_primary_care_orgs$Address, '<br>', SID_primary_care_orgs$Postcode),
                   group = 'Show GP practices in SID') %>% 
  # addLegend("bottomright", 
  #           pal = pal, 
  #           values = ~Patients,
  #           title = "Number of patients<br>(LSOAs with 10+<br>registered patients)",
  #           # labFormat = labelFormat(prefix = "$"),
  #           opacity = 1) %>% 
  addLegend("bottomright",
            pal = deppal,
            values = lsoas_in_SID_IMD_spdf$IMD_2019_decile,
            title = "National deprivation<br>deciles",
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>%
  addScaleBar(position = "bottomleft") %>% 
  addMeasure(position = 'bottomleft',
             primaryAreaUnit = 'sqmiles',
             primaryLengthUnit = 'miles') %>% 
  addLayersControl(
    overlayGroups = c('Show GP practices in SID'),
    baseGroups = c('Show lower super output areas','Show lower super output area deprivation'),
    options = layersControlOptions(collapsed = FALSE)) 

# Exporting does not currently work - I need to debug this
# library(htmlwidgets)
# saveWidget(Data_sprint_leaflet,
#            file = 'Sussex_practices_onboarded_to_SID.html',
#            title = 'Practices onboarded to SID',
#            selfcontained = TRUE)

Data_sprint_leaflet

lsoas_in_SID_IMD_summary

# PHE Fingertips ####

# FingertipsR - There is an R package for using the Fingertips API but it is a bit clunky. They did have it on CRAN (the official R verified package repo) but it was taken down for not meeting certain requirements. It has a lot of dependencies 

# I have found I can get fingertipsR to work if I install the following... 

# install.packages('DT')
# install.packages('miniUI')
# install.packages('shinycssloaders')
# install.packages("fingertipsR", repos = "https://dev.ropensci.org")
# library(fingertipsR)

# However, you do not need fingertipsR package to get to the data, and i find that it can be easier to use use read_csv and paste0() to create the strings needed to get data out of fingertipsR

# You can build a string in R to read_csv() for data from the fingertips API.

# You can use paste0() to build up the query
# Chosen_Profile = 87 # Profile for LAPE
# Chosen_Ind = 91409 #  This is the indicator for percentage of adults who drink over 14 units per week
# Chosen_area = 102 # This is the code for upper tier LA, 101 is for lower tier LA, 7 for GPs, 6 for region
# Chosen_parent_area = 6 
# 
# LAPE_over14  <- read_csv(url(paste0("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=", Chosen_Ind,"&child_area_type_id=", Chosen_area ,"&parent_area_type_id=",Chosen_parent_area,"&profile_id=", Chosen_Profile)))

#  Note: if you use fingertips_data, there are no spaces in the colnames returned (e.g. AreaName) but if you use the read_csv command there are spaces in colnames, so you have to run the next line and that will remove any spaces.

# You also get some quirky field types as R guesses what a field is from the first few rows so it thinks the values for 99% CIs are parsing failures

# LAPE_over14  <- read_csv(url(paste0("https://fingertips.phe.org.uk/api/all_data/csv/by_indicator_id?indicator_ids=", Chosen_Ind,"&child_area_type_id=", Chosen_area ,"&parent_area_type_id=",Chosen_parent_area,"&profile_id=", Chosen_Profile)),
#                          col_types = cols(
#                            .default = col_character(),
#                            `Indicator ID` = col_character(),
#                            Value = col_double(),
#                            `Lower CI 95.0 limit` = col_double(),
#                            `Upper CI 95.0 limit` = col_double(),
#                            `Lower CI 99.8 limit` = col_double(),
#                            `Upper CI 99.8 limit` = col_double(),
#                            Count = col_double(),
#                            Denominator = col_double(),
#                            `Recent Trend` = col_character(),
#                            `Time period Sortable` = col_double(),
#                            `New data` = col_character(),
#                            `Compared to goal` = col_character()
#                          ))


# QOF itself ####

# download.file('https://files.digital.nhs.uk/E2/BF16AA/QOF_1920.zip', '~/GP_profile_work/QOF_2020.zip', mode = 'wb')
# unzip('~/GP_profile_work/QOF_2020.zip', exdir = '~/GP_profile_work')

QOF_register_codes <- data.frame(Condition = 'Atrial Fibrilation', Code = 'AF', Age = 'All ages') %>% 
  add_row(Condition = 'Asthma', Code = 'AST', Age = 'All ages') %>% 
  add_row(Condition = 'Cancer', Code = 'CAN', Age = 'All ages') %>% 
  add_row(Condition = 'Coronary Heart Disease', Code = 'CHD', Age = 'All ages') %>% 
  add_row(Condition = 'Chronic Kidney Disease', Code = 'CKD', Age = '18+') %>% 
  add_row(Condition = 'Chronic Obstructive Pulmonary Disease', Code = 'COPD', Age = 'All ages') %>% 
  add_row(Condition = 'Dementia', Code = 'DEM', Age = 'All ages') %>% 
  add_row(Condition = 'Depression', Code = 'DEP', Age = '18+') %>% 
  add_row(Condition = 'Diabetes', Code = 'DM', Age = '17+') %>% 
  add_row(Condition = 'Epilepsy', Code = 'EP', Age = '18+') %>% 
  add_row(Condition = 'Heart Failure', Code = 'HF', Age = 'All ages') %>% 
  add_row(Condition = 'Hypertension', Code = 'HYP', Age = 'All ages') %>% 
  add_row(Condition = 'Mental Health', Code = 'MH', Age = 'All ages') %>%  # This is possibly recorded differently between artemus and QOF
  add_row(Condition = 'Osteoporosis', Code = 'OST', Age = '50+') %>% 
  add_row(Condition = 'Rheumatoid Arthritis', Code = 'RA', Age = '16+') %>% 
  add_row(Condition = 'Stroke and Transient Ischaemic Attack', Code = 'STIA', Age = 'All ages')

Registers <- read_csv('~/GP_profile_work/PREVALENCE_1920.csv') %>%
  filter(PRACTICE_CODE %in% SID_primary_care_orgs$ODS_Code) %>%
  filter(GROUP_CODE %in% c('AF', 'AST', 'CAN', 'CHD', 'CKD', 'COPD', 'DEM', 'DEP', 'DM', 'EP', 'HF', 'HYP', 'MH', 'OST', 'RA', 'STIA')) %>%
  rename(Code = GROUP_CODE) %>%
  left_join(QOF_register_codes, by = 'Code') %>%
  rename(GP_Code = PRACTICE_CODE,
         Patients_on_QOF_register = REGISTER,
         Denominator = PATIENT_LIST_SIZE) %>% 
  mutate(Prevalence = Patients_on_QOF_register / Denominator)

# Prescribing ####

# https://openprescribing.net/


# GP workforce information ####
# https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services
