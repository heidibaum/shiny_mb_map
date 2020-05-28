# Rodrigo Dal Ben
# 27/05/2020, last update: 28/05/2020
# adapted from: https://github.com/rladies/rshinylady

##### 
# To-do:
# rename all variables consistenly across this script and the app (see indications on code);
# check if website urls are functional;
# get US and Canada under "North America" & edit the app
# check the arrangement on dfs
# rename Autralia/Oceania to Oceania

# add studies separation AND link to app;

#####

# load library
library(rvest)
library(tidyverse)
library(here)
library(googlesheets4) # read database from drive
library(countrycode) # continents from countries

#library(meetupr) # data... delete? test first

# load database 
members <- googlesheets4::read_sheet("1Q9hQZ-Nop6Vqq3Ezre8waVU_qDsGhnJLsWYPh5nAih8")

## exclude digits and punctuations
## check abbreviations; add more if necessary
## differentiate "Americas"
rladies_groups <- ## RENAME
  members %>% 
  dplyr::mutate(
    country = gsub("[[:digit:]]+", "", country),
    country = gsub("[[:punct:]]+", "", country),
    country = case_when(
      country == "US" | country == "USA" ~ "United States",
      country == "UK" ~ "United Kingdom",
      TRUE ~ country
      ),
    continent = countrycode::countrycode(country, "country.name", "continent"),
    continent = case_when( 
      country %in% c("United States", "Canada") ~ "North America",
      continent == "Americas" ~ "Latin America", ## "Americas" except US and Canada;
      TRUE ~ continent
      )
    ) #%>% 
  #dplyr::select(-full_address) # consider deleting some columns, like address and institution, lab, 
                                # keep only institution_lab

## get continent next to country DO THIS WITH SELECT (PREVIOUS STEP)
rladies_groups <- rladies_groups[, c(1:8, 12, 9:11)] 

## RENAME VARIABLES ; REPLACE URLS to MB WEBSITE
# Add extra columns to set up popup content in the app
rladies_groups$fullurl <- paste0("https://rodrigodalben.github.io/", rladies_groups$studies, "/")
rladies_groups$url <- paste0("<a href='", rladies_groups$fullurl, "'>", rladies_groups$studies, "</a>")


# Separate by continent
## KEEP US AND CANADA SEPARATE FOR NOW; FIX LATER: TOGETHER UNDER "NORTH AMERICA"
# arranged by studies Z-A (most to less recent), then by institutions name (A-Z)

# United States
created_usa <- 
  rladies_groups %>% 
  filter(country == "United States") %>% 
  arrange(desc(studies), institution)

# Canada
created_canada <- 
  rladies_groups %>% 
  filter(country == "Canada") %>% 
  arrange(desc(studies), institution) 

# Latin America 
created_latam <- 
  rladies_groups %>% 
  filter(continent == "Latin America") %>% 
  arrange(desc(studies), institution) 

# Europe
created_europe <- 
  rladies_groups %>% 
  filter(continent == "Europe") %>% 
  arrange(desc(studies), institution) 

# Africa
created_africa <- 
  rladies_groups %>% 
  filter(continent == "Africa") %>% 
  arrange(desc(studies), institution) 

# Asia
created_asia <- 
  rladies_groups %>% 
  filter(continent == "Asia") %>% 
  arrange(desc(studies), institution) 

## RENAME TO OCEANIA! 
#  Australia/Oceania
groups_australia <- 
  rladies_groups %>% 
  filter(continent == "Oceania") %>% 
  arrange(desc(studies), institution) 


