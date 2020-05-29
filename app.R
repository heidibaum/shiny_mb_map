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
header <- dashboardHeader(title = "ManyBabies")

# Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "ManyBabies", tabName = "manybabies", icon = icon("user-graduate")), 
  # menuItem(text = "By Studies", tabName = "studies", icon = icon("graduation-cap")), # add other elements
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
      tabItem(tabName = "manybabies",
              selected = TRUE, 
        
        fluidRow(
          absolutePanel(style = "z-index: 2000", fixed = TRUE, draggable = TRUE,
                        top = 10, left = "auto", right = 20, width = "250px",
                        div(
                          tags$a(target="_blank", 
                                 href = "http://rodrigodalben.github.io/", # update
                                 tags$img(src="avatar-icon_cb.png", 
                                          height = "35px", id = "logo") 
                          )
                        )
          ),
          # A static valueBox
          valueBox(length(unique(mb_collaborators$studies)), "MB Studies", 
                   icon = icon("graduation-cap", lib = "font-awesome"), width = 3), # add color if necessary
          valueBox(length(unique(mb_collaborators$researcher)), "MB Collaborators", 
                   icon("users", lib = "font-awesome"), width = 3),
          valueBox(length(unique(mb_collaborators$institution)), "MB Institutions", 
                   icon = icon("university", lib = "font-awesome"), width = 3),
          valueBox(length(unique(mb_collaborators$country)), "MB Countries", 
                   icon = icon("map-o", lib = "font-awesome"), width = 3)
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
                                      box("Created at", width = 18, tableOutput("collab_nortam"))
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
                                      box("Created at", width = 18, tableOutput("collab_latam")
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
                                      box("Created at", width = 18, tableOutput("collab_europe")
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
                                      box("Created at", width = 18, tableOutput("collab_africa")
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
                                      box("Created at", width = 18, tableOutput("collab_asia")
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
                                      box("Created at", width = 18, tableOutput("collab_oceania")
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




ui <- dashboardPage(skin = "black", header, sidebar, body)

icons <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion", 
                      markerColor = "blue") 


# Set up popup content for global and regional maps
### we want url to point to studies, but not region. FIX

global_popups <- paste0("<b>", summary_global$institution, "</b>", "<br/>", 
                        "Researchers: ", summary_global$researcher, "<br/>", 
                        "Studies: ", summary_global$studies  
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

server <- function(input, output) { 
  
  output$map <- renderLeaflet({               
    leaflet(summary_global) %>% # test
      addTiles() %>%
      addAwesomeMarkers(~Longitude, ~Latitude, popup = global_popups, icon = icons)
  })
  output$collab_nortam <- renderTable(collab_nortam, striped = TRUE, hover = TRUE) # df collab
  output$map_nortam <- renderLeaflet({
    leaflet(summary_nortam) %>% # df map
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = nortam_popups) # df pop-ups
  })
  output$collab_latam <- renderTable(collab_latam, striped = TRUE, hover = TRUE)
  output$map_latam <- renderLeaflet({
    leaflet(summary_latam) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = latam_popups) 
  })
  output$collab_europe <- renderTable(collab_europe, striped = TRUE, hover = TRUE)
  output$map_europe <- renderLeaflet({
    leaflet(summary_europe) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = europe_popups)
  })
  output$collab_africa <- renderTable(collab_africa, striped = TRUE, hover = TRUE)
  output$map_africa <- renderLeaflet({
    leaflet(summary_africa) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = africa_popups) 
  })
  output$collab_asia <- renderTable(collab_asia, striped = TRUE, hover = TRUE)
  output$map_asia <- renderLeaflet({
    leaflet(summary_asia) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = asia_popups) 
  })
  output$collab_oceania <- renderTable(collab_oceania, striped = TRUE, hover = TRUE)
  output$map_oceania <- renderLeaflet({
    leaflet(summary_oceania) %>% 
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = oceania_popups) 
  })
  
  
}


shinyApp(ui, server)








