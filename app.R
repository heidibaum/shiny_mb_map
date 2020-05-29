source("chapters_source.R")
library(shinydashboard)
library(shiny)
library(leaflet)
library(htmltools)

#####
# To-do:
# add menuItem by studies: add other elements
# update webiste address
# change color
#
#####


## ui.R ##

## UI CONFIG

## Header
header <- dashboardHeader(title = "MB-collaborators")

# Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "ManyBabies", tabName = "manybabies", icon = icon("baby")), # test next 3 icons (FontAwesome)
  #  menuItem(text = "By Studies", tabName = "studies", icon = icon("graduation-cap")), # add other elements
    menuItem(text = "By Region", tabName = "region", icon = icon("globe")), 
    menuItem(text = "About", tabName = "about", icon = icon("heart"))
  )
)

## Body content
body <-   
  dashboardBody(
    tabItems(
      
      # Front Page
      
      # First sidebar tab - MB
      tabItem(tabName = "ManyBabies",
              selected = TRUE, 
        
        fluidRow(
          absolutePanel(style = "z-index: 2000", fixed = TRUE, draggable = TRUE,
                        top = 10, left = "auto", right = 20, width = "250px",
                        div(
                          tags$a(target="_blank", 
                                 href = "http://https://rodrigodalben.github.io/", # update
                                 tags$img(src="avatar-icon_cb.png", 
                                          height = "30px", id = "logo") 
                          )
                        )
          ),
          # A static valueBox
          valueBox(length(unique(mb_collaborators$studies)), "MB Studies", 
                   icon = icon("graduation-cap"), width = 3), # add color if necessary
          valueBox(length(unique(mb_collaborators$researcher)), "MB Collaborators", 
                   icon("users", lib = "font-awesome"), width = 3),
          valueBox(length(unique(mb_collaborators$institution)), "MB Institutions", 
                   icon = icon("university"), width = 3),
          valueBox(length(unique(mb_collaborators$country)), "MB Countries", 
                   icon = icon("map-o"), width = 3)
        ),
        leafletOutput('map', height = 700)
      ),
      
      
      ##### CONTINUE HERE! MERGE US AND CANADA! ; MAYBE NEED TO RECREATE SOME GROUPS AND INFO
      
      
      # ADD SECOND TAB: STUDIES!
      
      
      # Third sidebar tab - Region
      tabItem(tabName = "region",
              navbarPage(title = 'ManyBabies',
                         
                         tabPanel(title = 'North America',
                                  fluidRow(
                                    column(width = 4,
                                      # A static valueBox
                                      valueBox(nrow(collab_nortam), 
                                               "ManyBabies collaborators in North America", 
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_nortam"))
                                    ),
                                    column(width = 6,
                                      leafletOutput('map_nortam')
                                    )
                                  )
                         ),
                         tabPanel(title = 'Latin America',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(collab_latam),
                                               "ManyBabies collaborators in Latin America", 
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_latam")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_latam')
                                      )
                                  )
                         ),
                         tabPanel(title = 'Europe',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(collab_europe),
                                               "ManyBabies collaborators in Europe",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_europe")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_europe')
                                      )
                                  )
                         ),
                         tabPanel(title = 'Africa',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(collab_africa),
                                               "ManyBabies collaborators in Africa", 
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_africa")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_africa'))
                                  )
                         ),
                         tabPanel(title = 'Asia',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(collab_asia),
                                               "ManyBabies collaborators in Asia",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_asia")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_asia'))
                                  )
                         ),
                         tabPanel(title = 'Oceania',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(collab_oceania),
                                               "ManyBabies collaborators in Oceania",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_oceania")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_oceania'))
                                  )
                         )
              )
      ),
      tabItem(tabName = "about",
               
              
              fluidPage(
                h1(strong("About:")),
                p("This app was developed by ",
                  a("R-Ladies", href = "http://www.rladies.org"), 
                  "and adapted by",
                  a("ManyBabies", href = "https://rodrigodalben.github.io/"), # update website
                  "You can find the source code",
                  a("here.", href = "https://github.com/RodrigoDalBen/shiny_mb_map")), # update if moved
                  
                img(src = "avatar-icon_cb.png", height = 200, width = 200)
                
              )
      )
      
      ))




ui <- dashboardPage(skin = "black", header, sidebar, body) # color test

icons <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion", # check if this library will work... I was calling fa (fontawesome)
                      markerColor = "#F5F5F5") # color test


# Set up popup content for global and regional maps

### we want url to point to studies, but not region.
### global pop-up does not make sense.

global_popups <- paste0("<b>", "ManyBabies", "</b>", "<br/>", # ADD link here?
                        "Researchers: ", mb_collaborators$researcher, "<br/>",
                        "Studies: ", mb_collaborators$studies # original: members
)
nortam_popups <- paste0("<b>", summary_nortam$institution, "</b>", "<br/>",
                        "Researchers: ", summary_nortam$researcher, "<br/>",
                        "Studies: ", summary_nortam$studies
)
latam_popups <- paste0("<b>", summary_latam$institution, "</b>", "<br/>",
                       "Researchers: ", summary_latam$researcher, "<br/>",
                       "Studies: ", summary_latam$studies
)
europe_popups <- paste0("<b>", summary_europe$institution, "</b>", "<br/>",
                        "Researchers: ", summary_europe$researcher, "<br/>",
                        "Studies: ", summary_europe$studies
)
africa_popups <- paste0("<b>", summary_africa$institution, "</b>", "<br/>",
                        "Researchers: ", summary_africa$researcher, "<br/>",
                        "Studies: ", summary_africa$studies
)
asia_popups <- paste0("<b>", summary_asia$institution, "</b>", "<br/>",
                      "Researchers: ", summary_asia$researcher, "<br/>",
                      "Studies: ", summary_asia$studies
)
oceania_popups <- paste0("<b>", summary_oceania$institution, "</b>", "<br/>",
                         "Researchers: ", summary_oceania$researcher, "<br/>",
                         "Studies: ", summary_oceania$studies
)


# SERVER CONFIG

### for region (probability the same for studies)
## we need 3 dfs: 
#       1. Tables = researcher, studies, institution, country (summary; unique; )
#       2. pop-up = OK
#       3. map = OK created


server <- function(input, output) { 
  
  output$map <- renderLeaflet({               
    leaflet(data = mb_collaborators) %>% 
      addTiles() %>%
      addAwesomeMarkers(~Longitude, ~Latitute, popup = global_popups, icon = icons)
  })
  output$collab_nortam <- renderTable(collab_nortam, striped = TRUE, hover = TRUE) # df collab
  output$map_nortam <- renderLeaflet({
    leaflet(created_nortam) %>% # df map
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = nortam_popups) # df pop-ups
  })
  output$collab_latam <- renderTable(collab_latam, striped = TRUE, hover = TRUE)
  output$map_latam <- renderLeaflet({
    leaflet(created_latam) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = latam_popups) 
  })
  output$collab_europe <- renderTable(collab_europe, striped = TRUE, hover = TRUE)
  output$map_europe <- renderLeaflet({
    leaflet(created_europe) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = europe_popups)
  })
  output$collab_africa <- renderTable(collab_africa, striped = TRUE, hover = TRUE)
  output$map_africa <- renderLeaflet({
    leaflet(created_africa) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = africa_popups) 
  })
  output$collab_asia <- renderTable(collab_asia, striped = TRUE, hover = TRUE)
  output$map_asia <- renderLeaflet({
    leaflet(created_asia) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = asia_popups) 
  })
  output$collab_oceania <- renderTable(collab_oceania, striped = TRUE, hover = TRUE)
  output$map_oceania <- renderLeaflet({
    leaflet(created_oceania) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = oceania_popups) 
  })
  
  
}


shinyApp(ui, server)








