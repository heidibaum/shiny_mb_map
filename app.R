source("mb_source.R")
library(shinydashboard)
library(shiny)
library(leaflet)
library(htmltools)
library(DT)

#####
# To-do:
# update webiste address
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
    menuItem(text = "By Studies", tabName = "studies", icon = icon("graduation-cap")), # add other elements
    menuItem(text = "By Region", tabName = "region", icon = icon("globe")), 
    menuItem(text = "About", tabName = "about", icon = icon("heart"))
  )
)

## Body content
body <-   
  dashboardBody(
    tabItems(
      
      # Front Page
      
      # First sidebar tab - ManyBabies
      tabItem(tabName = "manybabies",
              selected = TRUE, 
        
        fluidRow(
          absolutePanel(style = "z-index: 2000", fixed = TRUE, draggable = TRUE,
                        top = 10, left = "auto", right = 20, width = "250px",
                        div(
                          tags$a(target="_blank", 
                                 href = "http://rodrigodalben.github.io/", # update
                                 tags$img(src = "avatar-icon_cb.png", 
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
      
      
      # region tab
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
                                      box("Created at", width = 18, DT::dataTableOutput("collab_nortam"))
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
                                      box("Created at", width = 18, DT::dataTableOutput("collab_latam")
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
                                      box("Created at", width = 18, DT::dataTableOutput("collab_europe")
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
                                      box("Created at", width = 18, DT::dataTableOutput("collab_africa")
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
                                      box("Created at", width = 18, DT::dataTableOutput("collab_asia")
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
                                      box("Created at", width = 18, DT::dataTableOutput("collab_oceania")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_oceania'))
                                  )
                         )
      )),
      
      # studies tab
      tabItem(tabName = "studies",
              navbarPage(title = 'ManyBabies',
                         
                         tabPanel(title = 'MB1',
                                  fluidRow(
                                    column(width = 5,
                                           # A static valueBox
                                           valueBox(nrow(tab_mb1), 
                                                    "ManyBabies collaborators in MB1", 
                                                    icon = icon("glyphicon-blackboard"), width = 18
                                           ),
                                           box("Created at", width = 18, DT::dataTableOutput("tab_mb1")) # table output - CREATE ON NEXT CHUNK
                                    ),
                                    column(width = 6,
                                           leafletOutput('map_mb1') # map output - CREATE ON NEXT CHUNK
                                    )
                                  )
                         ),
                         tabPanel(title = 'MB2',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb2),
                                               "ManyBabies collaborators in MB2", 
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb2")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb2')
                                    )
                                  )
                         ),
                         tabPanel(title = 'MB3',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb3),
                                               "ManyBabies collaborators in MB3",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb3")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb3')
                                    )
                                  )
                         ),
                         tabPanel(title = 'MB4',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb4),
                                               "ManyBabies collaborators in MB4", 
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb4")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb4'))
                                  )
                         ),
                         tabPanel(title = 'MB5',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb5),
                                               "ManyBabies collaborators in MB5",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb5")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb5'))
                                  )
                         ),
                         tabPanel(title = 'MB-AtHome',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb_athome),
                                               "ManyBabies collaborators in MB-AtHome",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb_athome")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb_athome'))
                                  )
                         ),
                         tabPanel(title = 'MB1A',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1a),
                                               "ManyBabies collaborators in MB1A",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1a")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb1a'))
                                  )
                         ),
                         tabPanel(title = 'MB1B',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1b),
                                               "ManyBabies collaborators in MB1B",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1b")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb1b'))
                                  )
                         ),
                         tabPanel(title = 'MB1L',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1l),
                                               "ManyBabies collaborators in MB1L",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1l")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb1l'))
                                  )
                         ),
                         tabPanel(title = 'MB1N',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1n),
                                               "ManyBabies collaborators in MB1N",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1n")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb1n'))
                                  )
                         ),
                         tabPanel(title = 'MB1T',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1t),
                                               "ManyBabies collaborators in MB1T",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1t")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb1t'))
                                  )
                         ),
                         tabPanel(title = 'MB3N',
                                  fluidRow(
                                    column(
                                      width = 5,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb3n),
                                               "ManyBabies collaborators in MB3N",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb3n")
                                      )
                                    ),
                                    column(
                                      width = 6,
                                      leafletOutput('map_mb3n'))
                                  )
                         )
      )),
      
      # about tab
      tabItem(tabName = "about",
              fluidPage(
                h1(strong("About:")),
                p("Learn more about",
                  a("ManyBabies projects", href = "https://rodrigodalben.github.io/projects"),
                  "and how to",
                  a("get involved.", href = "https://rodrigodalben.github.io/get_involved")
                  ), 
                p("This app was developed by ",
                  a("R-Ladies", href = "http://www.rladies.org"), 
                  "and adapted by",
                  a("ManyBabies", href = "https://rodrigodalben.github.io/"), # update website
                  "You can find the source code",
                  a("here.", href = "https://github.com/RodrigoDalBen/shiny_mb_map") # update if moved
                  ), 
                img(src = "avatar-icon_cb.png", height = 200, width = 200)
                )
      )))

# ui aesthetics
ui <- dashboardPage(skin = "black", header, sidebar, body)

icons <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion", 
                      markerColor = "blue") 

# Pop-ups
# global
global_popups <- paste0("<b>", summary_global$institution, "</b>", "<br/>", 
                        "Researchers: ", summary_global$researcher, "<br/>", 
                        "Studies: ", summary_global$studies  
)

# region
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

# studies
mb1_popups <- paste0("<b>", summary_mb1$institution, "</b>", "<br/>",
                         "Researchers: ", summary_mb1$researcher 
)
mb2_popups <- paste0("<b>", summary_mb2$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb2$researcher 
)
mb3_popups <- paste0("<b>", summary_mb3$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb3$researcher 
)
mb4_popups <- paste0("<b>", summary_mb4$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb4$researcher 
)
mb5_popups <- paste0("<b>", summary_mb5$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb5$researcher 
)
mb_athome_popups <- paste0("<b>", summary_mb_athome$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb_athome$researcher 
)
mb1a_popups <- paste0("<b>", summary_mb1a$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb1a$researcher 
)
mb1b_popups <- paste0("<b>", summary_mb1b$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb1b$researcher 
)
mb1l_popups <- paste0("<b>", summary_mb1l$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb1l$researcher 
)
mb1n_popups <- paste0("<b>", summary_mb1n$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb1n$researcher 
)
mb1t_popups <- paste0("<b>", summary_mb1t$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb1t$researcher 
)
mb3n_popups <- paste0("<b>", summary_mb3n$institution, "</b>", "<br/>",
                     "Researchers: ", summary_mb3n$researcher 
)


# SERVER CONFIG
server <- function(input, output) { 
  
  # keep leaflet cluster "Green" regardless of size (looks better and avoid noise in the map)
  green_clusters = JS("function (cluster) {    
                      var childCount = cluster.getChildCount(); 
                      var c = ' marker-cluster-';  
                      c += 'small';
                      return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
                      }")
    
  
  # global
  output$map <- renderLeaflet({               
    leaflet(summary_global) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = global_popups, 
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))
  })
  
  # regions
  output$collab_nortam <- DT::renderDataTable({collab_nortam})
  output$map_nortam <- renderLeaflet({
    leaflet(summary_nortam) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = nortam_popups,
                 clusterOptions =
                   markerClusterOptions(maxClusterRadius = 30,
                                        iconCreateFunction = green_clusters
                                        ))
  })
  output$collab_latam <- DT::renderDataTable({collab_latam})
  output$map_latam <- renderLeaflet({
    leaflet(summary_latam) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = latam_popups,
                 clusterOptions = 
                   markerClusterOptions(maxClusterRadius = 30,
                                        iconCreateFunction = green_clusters
                                              ))
  })
  output$collab_europe <- DT::renderDataTable({collab_europe})
  output$map_europe <- renderLeaflet({
    leaflet(summary_europe) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = europe_popups,
                       clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                             iconCreateFunction = green_clusters
                                                             ))
  })
  output$collab_africa <- DT::renderDataTable({collab_africa})
  output$map_africa <- renderLeaflet({
    leaflet(summary_africa) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = africa_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$collab_asia <- DT::renderDataTable({collab_asia})
  output$map_asia <- renderLeaflet({
    leaflet(summary_asia) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = asia_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$collab_oceania <- DT::renderDataTable({collab_oceania})
  output$map_oceania <- renderLeaflet({
    leaflet(summary_oceania) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = oceania_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  
  # studies
  output$tab_mb1 <- DT::renderDataTable({tab_mb1})
  output$map_mb1 <- renderLeaflet({
    leaflet(summary_mb1) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb1_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 ))  
  })
  output$tab_mb2 <- DT::renderDataTable({tab_mb2})
  output$map_mb2 <- renderLeaflet({
    leaflet(summary_mb2) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb2_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 ))  
  })
  output$tab_mb3 <- DT::renderDataTable({tab_mb3})
  output$map_mb3 <- renderLeaflet({
    leaflet(summary_mb3) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb3_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$tab_mb4 <- DT::renderDataTable({tab_mb4})
  output$map_mb4 <- renderLeaflet({
    leaflet(summary_mb4) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb4_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$tab_mb5 <- DT::renderDataTable({tab_mb5})
  output$map_mb5 <- renderLeaflet({
    leaflet(summary_mb5) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb5_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 ))  
  })
  output$tab_mb_athome <- DT::renderDataTable({tab_mb_athome})
  output$map_mb_athome <- renderLeaflet({
    leaflet(summary_mb_athome) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb_athome_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$tab_mb1a <- DT::renderDataTable({tab_mb1a})
  output$map_mb1a <- renderLeaflet({
    leaflet(summary_mb1a) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb1a_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$tab_mb1b <- DT::renderDataTable({tab_mb1b})
  output$map_mb1b <- renderLeaflet({
    leaflet(summary_mb1b) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb1b_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$tab_mb1l <- DT::renderDataTable({tab_mb1l})
  output$map_mb1l <- renderLeaflet({
    leaflet(summary_mb1l) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb1l_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$tab_mb1n <- DT::renderDataTable({tab_mb1n})
  output$map_mb1n <- renderLeaflet({
    leaflet(summary_mb1n) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb1n_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 ))  
  })
  output$tab_mb1t <- DT::renderDataTable({tab_mb1t})
  output$map_mb1t <- renderLeaflet({
    leaflet(summary_mb1t) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb1t_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  output$tab_mb3n <- DT::renderDataTable({tab_mb3n})
  output$map_mb3n <- renderLeaflet({
    leaflet(summary_mb3n) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, popup = mb3n_popups,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 30,
                                                       iconCreateFunction = green_clusters
                 )) 
  })
  
  }

shinyApp(ui, server)








