# Rodrigo Dal Ben
# 27/05/2020, last update: 01/06/2020
# adapted from: https://github.com/rladies/rshinylady

##### 
## To-do:
# update website URLs on all scripts after publishin MB new website

## Notes:
# if necessary, add more abbrevations when wrangling data;
# keep variables name lowercase;
# MB3N is empty for now;

# for region: 
#   1. summary_ = pop-ups; map & stats
#   2. collab_ = collaborators info 
#   extra = created_ serves as a filter/arrange pre-step.

# for studies:
#   1. tab = collaborators info for table;
#   2. summary = pop-ups; map & stats
#   extra = "map" filters based on studies

#####

# load library
library(tidyverse)
library(here)
library(googlesheets4) 
library(countrycode)

# load credentials
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = "youremail@email.com"
  )

#googlesheets4::gs4_auth()

# load database 
collaborators <- googlesheets4::read_sheet("1Q9hQZ-Nop6Vqq3Ezre8waVU_qDsGhnJLsWYPh5nAih8")

# clean databse 
mb_collaborators <- 
  collaborators %>% 
  dplyr::mutate(
    country = gsub("[[:digit:]]+", "", country), # exclude digits 
    country = gsub("[[:punct:]]+", "", country), # exclude punctuation
    country = case_when(
      country == "US" | country == "USA" ~ "United States", # standard abbreviation/name  
      country == "UK" ~ "United Kingdom",
      TRUE ~ country
      ),
    continent = countrycode::countrycode(country, "country.name", "continent"),
    continent = case_when( # differentiate "Americas" 
      country %in% c("United States", "Canada") ~ "North America",
      continent == "Americas" ~ "Latin America", 
      TRUE ~ continent
      ),
    researcher = ifelse(is.na(researcher), 
                        paste0("anonymous-", 1:length(researcher[is.na(researcher)])), 
                        researcher)
    )

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

collab_global <- 
  mb_collaborators %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)

# Separate by continent
# North America
filter_nortam <- 
  mb_collaborators %>% 
  filter(continent == "North America")

summary_nortam <- 
  filter_nortam %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    ) 

collab_nortam <- 
  filter_nortam %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
    ) %>% 
  arrange(researcher)

# Latin America 
filter_latam <- 
  mb_collaborators %>% 
  filter(continent == "Latin America")

summary_latam <- 
  filter_latam %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_latam <- 
  filter_latam %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)

# Europe
filter_europe <- 
  mb_collaborators %>% 
  filter(continent == "Europe")

summary_europe <- 
  filter_europe %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_europe <- 
  filter_europe %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)


# Africa
filter_africa <- 
  mb_collaborators %>% 
  filter(continent == "Africa") 

summary_africa <- 
  filter_africa %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_africa <- 
  filter_africa %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)

# Asia
filter_asia <- 
  mb_collaborators %>% 
  filter(continent == "Asia")

summary_asia <- 
  filter_asia %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    ) 

collab_asia <- 
  filter_asia %>% 
  group_by(researcher) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    studies = paste(unique(studies), collapse = ", "),
    country = paste(unique(country), collapse = ", ")
  ) %>% 
  arrange(researcher)

# Oceania
filter_oceania <- 
  mb_collaborators %>% 
  filter(continent == "Oceania")

summary_oceania <- 
  filter_oceania %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", "),
    studies = paste(unique(studies), collapse = ", ")
    )

collab_oceania <- 
  filter_oceania %>% 
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
filter_mb1 <- 
  mb_collaborators %>% 
  filter(studies == "MB1")

tab_mb1 <- 
  filter_mb1 %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb1 <- 
  filter_mb1 %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )
  

# MB2
filter_mb2 <- 
  mb_collaborators %>% 
  filter(studies == "MB2") %>% 
  arrange(institution) 

tab_mb2 <- 
  filter_mb2 %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb2 <- 
  filter_mb2 %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB3
filter_mb3 <- 
  mb_collaborators %>% 
  filter(studies == "MB3") %>% 
  arrange(institution) 

tab_mb3 <- 
  filter_mb3 %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb3 <- 
  filter_mb3 %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB4
filter_mb4 <- 
  mb_collaborators %>% 
  filter(studies == "MB4") %>% 
  arrange(institution) 

tab_mb4 <- 
  filter_mb4 %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb4 <- 
  filter_mb4 %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB5
filter_mb5 <- 
  mb_collaborators %>% 
  filter(studies == "MB5") %>% 
  arrange(institution) 

tab_mb5 <- 
  filter_mb5 %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb5 <- 
  filter_mb5 %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB-AtHome
filter_mb_athome <- 
  mb_collaborators %>% 
  filter(studies == "MB-AtHome") %>% 
  arrange(institution) 

tab_mb_athome <- 
  filter_mb_athome %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb_athome <- 
  filter_mb_athome %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# Spin-offs
# MB1A
filter_mb1a <- 
  mb_collaborators %>% 
  filter(studies == "MB1A") %>% 
  arrange(institution) 

tab_mb1a <- 
  filter_mb1a %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb1a <- 
  filter_mb1a %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB1B
filter_mb1b <- 
  mb_collaborators %>% 
  filter(studies == "MB1B") %>% 
  arrange(institution) 

tab_mb1b <- 
  filter_mb1b %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb1b <- 
  filter_mb1b %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB1L
filter_mb1l <- 
  mb_collaborators %>% 
  filter(studies == "MB1L") %>% 
  arrange(institution) 

tab_mb1l <- 
  filter_mb1l %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb1l <- 
  filter_mb1l %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB1N
filter_mb1n <- 
  mb_collaborators %>% 
  filter(studies == "MB1N") %>% 
  arrange(institution) 

tab_mb1n <- 
  filter_mb1n %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb1n <- 
  filter_mb1n %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB1T
filter_mb1t <- 
  mb_collaborators %>% 
  filter(studies == "MB1T") %>% 
  arrange(institution) 

tab_mb1t <- 
  filter_mb1t %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb1t <- 
  filter_mb1t %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )

# MB3N - empty for now
filter_mb3n <- 
  mb_collaborators %>% 
  filter(studies == "MB3N") %>% 
  arrange(institution) 

tab_mb3n <- 
  filter_mb3n %>% 
  select(researcher, institution, country, continent) %>% 
  arrange(researcher)

summary_mb3n <- 
  filter_mb3n %>% 
  group_by(Latitude, Longitude) %>% 
  summarise(
    institution = paste(unique(institution), collapse = ", "),
    researcher = paste(unique(researcher), collapse = ", ")
  )
