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

# Notes:
# keep variables name lowercase
# MB3N is empty for now

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
collaborators <- googlesheets4::read_sheet("1Q9hQZ-Nop6Vqq3Ezre8waVU_qDsGhnJLsWYPh5nAih8")

## exclude digits and punctuations
## check abbreviations; add more if necessary
## differentiate "Americas"
mb_collaborators <- 
  collaborators %>% 
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
mb_collaborators <- mb_collaborators[, c(1:8, 12, 9:11)] 

## RENAME VARIABLES ; REPLACE URLS to MB WEBSITE
# Add extra columns to set up popup content in the app
mb_collaborators$fullurl <- paste0("https://rodrigodalben.github.io/", mb_collaborators$studies, "/")
mb_collaborators$url <- paste0("<a href='", mb_collaborators$fullurl, "'>", mb_collaborators$studies, "</a>")

a <- "a"
a <- "b"

# Separate by continent
# arranged by studies Z-A (most to less recent), then by institutions name (A-Z)

## KEEP US AND CANADA SEPARATE FOR NOW; FIX LATER: TOGETHER UNDER "NORTH AMERICA"

# United States
created_usa <- 
  mb_collaborators %>% 
  filter(country == "United States") %>% 
  arrange(desc(studies), institution)

# Canada
created_canada <- 
  mb_collaborators %>% 
  filter(country == "Canada") %>% 
  arrange(desc(studies), institution) 

# Latin America 
created_latam <- 
  mb_collaborators %>% 
  filter(continent == "Latin America") %>% 
  arrange(desc(studies), institution) 

# Europe
created_europe <- 
  mb_collaborators %>% 
  filter(continent == "Europe") %>% 
  arrange(desc(studies), institution) 

# Africa
created_africa <- 
  mb_collaborators %>% 
  filter(continent == "Africa") %>% 
  arrange(desc(studies), institution) 

# Asia
created_asia <- 
  mb_collaborators %>% 
  filter(continent == "Asia") %>% 
  arrange(desc(studies), institution) 

## RENAME TO OCEANIA! 
#  Australia/Oceania
created_australia <- 
  mb_collaborators %>% 
  filter(continent == "Oceania") %>% 
  arrange(desc(studies), institution) 


# Separate by study
# Main projects
# MB1
created_mb1 <- 
  mb_collaborators %>% 
  filter(studies == "MB1") %>% 
  arrange(institution) 

# MB2
created_mb2 <- 
  mb_collaborators %>% 
  filter(studies == "MB2") %>% 
  arrange(institution) 

# MB3
created_mb3 <- 
  mb_collaborators %>% 
  filter(studies == "MB3") %>% 
  arrange(institution) 

# MB4
created_mb4 <- 
  mb_collaborators %>% 
  filter(studies == "MB4") %>% 
  arrange(institution) 

# MB5
created_mb5 <- 
  mb_collaborators %>% 
  filter(studies == "MB5") %>% 
  arrange(institution) 

# MB-AtHome
created_mb_athome <- 
  mb_collaborators %>% 
  filter(studies == "MB-AtHome") %>% 
  arrange(institution) 

# Spin-offs
# MB1A
created_mb1a <- 
  mb_collaborators %>% 
  filter(studies == "MB1A") %>% 
  arrange(institution) 

# MB1B
created_mb1b <- 
  mb_collaborators %>% 
  filter(studies == "MB1B") %>% 
  arrange(institution) 

# MB1L
created_mb1l <- 
  mb_collaborators %>% 
  filter(studies == "MB1L") %>% 
  arrange(institution) 

# MB1N
created_mb1n <- 
  mb_collaborators %>% 
  filter(studies == "MB1N") %>% 
  arrange(institution) 

# MB1T
created_mb1t <- 
  mb_collaborators %>% 
  filter(studies == "MB1T") %>% 
  arrange(institution) 

# MB3N - empty for now
created_mb3n <- 
  mb_collaborators %>% 
  filter(studies == "MB3N") %>% 
  arrange(institution) 
