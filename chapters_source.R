# Rodrigo Dal Ben
# 27/05/2020
# adapted from: https://github.com/rladies/rshinylady

# load library
library(rvest)
library(tidyverse)
library(here)
library(googlesheets4) # read database from drive
library(countrycode) # continents from countries

library(meetupr) # data... might not be necessary

# load database 
members <- googlesheets4::read_sheet("1Q9hQZ-Nop6Vqq3Ezre8waVU_qDsGhnJLsWYPh5nAih8")

## RENAME 
# exclude any punctuations
# check abbreviations; add more if necessary
# differentiate "Americas"
rladies_groups <- 
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
      continent == "Americas" ~ "Latin America",
      TRUE ~ continent
      )
    ) #%>% 
  #dplyr::select(-full_address) # consider deleting some columns, like address and institution, lab, 
                                # keep only institution_lab

## get continent next to country DO THIS WITH SELECT (PRECIOUS STEP)
rladies_groups <- rladies_groups[, c(1:8, 12, 9:11)] 

##### DELETE IN THE FUTURE

# meetup groups ----------------------------------------------------------
api_key <- Sys.getenv("meetup_key")
# this is needed when you deploy the shiny app
# api_key <- readRDS("meetup_key.RDS")
all_rladies_groups <- find_groups(text = "r-ladies", api_key = api_key)

# Cleanup
rladies_groups <- all_rladies_groups[grep(pattern = "rladies|r-ladies|r ladies", 
                                          x = all_rladies_groups$name,
                                          ignore.case = TRUE), ]

##### 

## RENAME VARIABLES ; REPLACE URLS
# Add extra columns to set up popup content in the app
rladies_groups$fullurl <- paste0("https://rodrigodalben.github.io/", rladies_groups$studies, "/")
rladies_groups$url <- paste0("<a href='", rladies_groups$fullurl, "'>", rladies_groups$studies, "</a>")


## Each continent - KEEP US AND CANADA SEPARATE FOR NOW, FIX LATER - TOGETHER UNDER "NORTH AMERICA"
# arranged by studies Z-A (most to less recent), then by institutions name ?? 
created_usa <- 
  rladies_groups %>% 
  filter(country == "United States") %>% 
  arrange(desc(studies), institution)


##### 
##Not doing this now
  #groups_usa %>% 
  #mutate(dt_created = substr(created, 1, 10)) %>% 
  #arrange(desc(dt_created)) %>% 
  #select("city", "state", "country", "dt_created", "members")

#####

## If we do not have "country" as an input on member sign-up form (which I think we should have)
## we can get countries from Timezones as described below:

# For subsetting, the follwing countries, I'm using the time zone as a proxy
# of where the chapter is located
# unique(rladies_groups$timezone)
# unique(sub(pattern = "\\/.*", "", unique(rladies_groups$timezone)))
# [1] "Europe"    "America"   "Australia" "Africa"    
#  "Canada"    "Asia"      "US"        "Pacific" 
# Please note that Auckland has TZ "Pacific/Auckland"


# Canada

#canada <- sort(unique(rladies_groups[grep("Canada", rladies_groups$timezone),]$country))

created_canada <- rladies_groups %>% 
  filter(country == "Canada") %>% 
  arrange(desc(studies), institution) 

#created_canada <- groups_canada %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")


# Latin America (all Americas countries except US and Canada)

#latam <- sort(unique(rladies_groups[grep("America", rladies_groups$timezone),]$country))

created_latam <- rladies_groups %>% 
  filter(continent == "Latin America") %>% 
  arrange(desc(studies), institution) 

#created_latam <- groups_latam %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

# Europe

#europe <- sort(unique(rladies_groups[grep("Europe", rladies_groups$timezone),]$country))
created_europe <- rladies_groups %>% 
  filter(continent == "Europe") %>% 
  arrange(desc(studies), institution) 
# Add Tbilisi to the Europe group
# for some reason Tbilisi uses the timezone `Asia/Tbilisi` therefore
# it got categorized as Asia. However, Tbilisi is located in Europe
#groups_europe <- rbind(groups_europe,
#                       rladies_groups %>% filter(city == "Tbilisi"))

#created_europe <- groups_europe %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

# Africa

#africa <- sort(unique(rladies_groups[grep("Africa", rladies_groups$timezone),]$country))
created_africa <- rladies_groups %>% 
  filter(continent == "Africa") %>% 
  arrange(desc(studies), institution) 

#created_africa <- groups_africa %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

# Asia

#asia <- sort(unique(rladies_groups[grep("Asia", rladies_groups$timezone),]$country))
created_asia <- rladies_groups %>% 
  filter(continent == "Asia") %>% 
  arrange(desc(studies), institution) 

# %>% 
#  filter(city != "Tbilisi") # Remove Tbilisi from the Asia group
#created_asia <- groups_asia %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")

## RENAME TO OCEANIA! 
#  Australia/Oceania

#australia <- sort(unique(rladies_groups[grep("Australia|Pacific/Auckland", rladies_groups$timezone),]$country))
groups_australia <- rladies_groups %>% 
  filter(continent == "Oceania") %>% 
  arrange(desc(studies), institution) 

#created_australia <- groups_australia %>% 
#  mutate(dt_created = substr(created, 1, 10)) %>% 
#  arrange(desc(dt_created)) %>% 
#  select("city", "state", "country", "dt_created", "members")
