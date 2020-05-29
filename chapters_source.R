# Rodrigo Dal Ben
# 27/05/2020, last update: 28/05/2020
# adapted from: https://github.com/rladies/rshinylady

##### 
# To-do:
# rename all variables consistenly across this script and the app (see indications on code);
# check if website urls are functional;
# check the arrangement on dfs

# Notes:
# keep variables name lowercase
# MB3N is empty for now
# dfs arranged by studies Z-A (most to less recent), then by institutions name (A-Z)
# dfs:
#   1. summary_ = pop-ups; map & stats
#   2. collab_ = collaborators info 
#   extra = created_ serves as a filter/arrange pre-step.

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
      ),
    researcher = ifelse(is.na(researcher), 
                        paste0("anonymous-", 1:length(researcher[is.na(researcher)])), 
                        researcher)
    )

mb_collaborators <- mb_collaborators[, c(1:8, 12, 9:11)] # column reorder

## RENAME VARIABLES ; REPLACE URLS to MB WEBSITE
# Add extra columns to set up popup content in the app
mb_collaborators$fullurl <- paste0("https://rodrigodalben.github.io/", mb_collaborators$studies, "/")
mb_collaborators$url <- paste0("<a href='", mb_collaborators$fullurl, "'>", mb_collaborators$studies, "</a>")

# maps
# global
summary_global <- 
  mb_collaborators %>% 
  group_by(Latitude, Longitude) %>%
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
  ) %>% 
  ungroup()

# Separate by continent
# North America
created_nortam <- 
  mb_collaborators %>% 
  filter(continent == "North America") %>% 
  arrange(desc(studies), institution)

summary_nortam <- 
  created_nortam %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    ) 

collab_nortam <- 
  created_nortam %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
    ) %>% 
  arrange(researcher)

# Latin America 
created_latam <- 
  mb_collaborators %>% 
  filter(continent == "Latin America") %>% 
  arrange(desc(studies), institution) 

summary_latam <- 
  created_latam %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_latam <- 
  created_latam %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)

# Europe
created_europe <- 
  mb_collaborators %>% 
  filter(continent == "Europe") %>% 
  arrange(desc(studies), institution) 

summary_europe <- 
  created_europe %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_europe <- 
  created_europe %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)


# Africa
created_africa <- 
  mb_collaborators %>% 
  filter(continent == "Africa") %>% 
  arrange(desc(studies), institution) 

summary_africa <- 
  created_africa %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_africa <- 
  created_africa %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)

# Asia
created_asia <- 
  mb_collaborators %>% 
  filter(continent == "Asia") %>% 
  arrange(desc(studies), institution)

summary_asia <- 
  created_asia %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    ) 

collab_asia <- 
  created_asia %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)

# Oceania
created_oceania <- 
  mb_collaborators %>% 
  filter(continent == "Oceania") %>% 
  arrange(desc(studies), institution) 

summary_oceania <- 
  created_oceania %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_oceania <- 
  created_oceania %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)


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
