source("mb_source.R")
library(shinydashboard)
library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(shinydashboardPlus)
library(shinyjs)

#####
# To-do:
#
# Notes:
# `setView(lat, lng, zoom)` in Latin America & Oceania is a quick-fix; delete as numbers of collaborators increase
# 
# Known issues:
#   - logo is slighty misaligned in collapsed bar
#   - navbar overlays sidbar popups
#   - Data table and map overlaps on small screens
# 
#####


## ui.R ##

## UI CONFIG

## Header
header <- dashboardHeaderPlus(title = tags$a(href = "http://manybabies.github.io/", 
                                             tags$img(src = "avatar-icon_cb.png", height = "32px"),
                                             "ManyBabies", width = 180)) 
                                
# Sidebar content
sidebar <- dashboardSidebar(
  width = 135,
  sidebarMenu(
    menuItem(text = "Overview", tabName = "overview", icon = icon("binoculars")), 
    menuItem(text = "By Study", tabName = "studies", icon = icon("graduation-cap")),
    menuItem(text = "By Region", tabName = "region", icon = icon("globe")),
    menuItem(text = "People", tabName = "people", icon = icon("users")),
    menuItem(text = "About", tabName = "about", icon = icon("heart"))
  )
)

## Body content
body <-   
  dashboardBody(
    
    # link internal tabs
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    
    # custom color-blind friendly colors (as MB logo)
    # masking: green, blue, orange, fuchsia
    tags$style(".small-box.bg-green { background-color: #009E73 !important; color: #FFFFFF !important; }"),
    tags$style(".small-box.bg-blue { background-color: #56B4E9 !important; color: #FFFFFF !important; }"),
    tags$style(".small-box.bg-orange { background-color: #E69600 !important; color: #FFFFFF !important; }"),
    tags$style(".small-box.bg-fuchsia { background-color: #CC79A7 !important; color: #FFFFFF !important; }"),
    
    tabItems(
      # Front Page & first sidebar tab - ManyBabies
      tabItem(tabName = "overview",
              selected = TRUE, 
              
              fluidRow(
              # dynamic valuebox
              valueBoxOutput("vb_studies", width = 3), 
              valueBoxOutput("vb_collaborators", width = 3), 
              valueBoxOutput("vb_institutions", width = 3), 
              valueBoxOutput("vb_countries", width = 3)
              ),

              # global map
              leafletOutput('map', height = 700)
      ),
      
      
      # region tab
      tabItem(tabName = "region",
              navbarPage(title = 'ManyBabies',
                         
                         tabPanel(title = 'North America',
                                  fluidRow(
                                    column(width = 6,
                                      # A static valueBox
                                      valueBox(nrow(collab_nortam), 
                                               "ManyBabies collaborators in North America", 
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "green"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("collab_nortam")
                                          )
                                    ),
                                    column(width = 5,
                                      leafletOutput('map_nortam')
                                    )
                                  )
                         ),
                         tabPanel(title = 'Latin America',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(collab_latam),
                                               "ManyBabies collaborators in Latin America", 
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "green"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("collab_latam")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_latam')
                                      )
                                  )
                         ),
                         tabPanel(title = 'Europe',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(collab_europe),
                                               "ManyBabies collaborators in Europe",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "green"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("collab_europe")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_europe')
                                      )
                                  )
                         ),
                         tabPanel(title = 'Africa',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(collab_africa),
                                               "ManyBabies collaborators in Africa", 
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "green"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("collab_africa")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_africa'))
                                  )
                         ),
                         tabPanel(title = 'Asia',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(collab_asia),
                                               "ManyBabies collaborators in Asia",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "green"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("collab_asia")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_asia'))
                                  )
                         ),
                         tabPanel(title = 'Oceania',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(collab_oceania),
                                               "ManyBabies collaborators in Oceania",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "green"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("collab_oceania")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_oceania'))
                                  )
                         )
                         
      )),
      
      # study tab
      tabItem(tabName = "studies",
              navbarPage(title = 'ManyBabies',
                         
                         tabPanel(title = 'MB1',
                                  fluidRow(
                                    column(width = 6,
                                           # A static valueBox
                                           valueBox(nrow(tab_mb1), 
                                                    "ManyBabies collaborators in MB1", 
                                                    icon = icon("glyphicon-blackboard"), width = 18,
                                                    color = "blue"
                                           ),
                                           box("Created at", width = 18, DT::dataTableOutput("tab_mb1"))
                                    ),
                                    column(width = 5,
                                           leafletOutput('map_mb1')
                                    )
                                  )
                         ),
                         tabPanel(title = 'MB2',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb2),
                                               "ManyBabies collaborators in MB2", 
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb2")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb2')
                                    )
                                  )
                         ),
                         tabPanel(title = 'MB3',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb3),
                                               "ManyBabies collaborators in MB3",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb3")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb3')
                                    )
                                  )
                         ),
                         tabPanel(title = 'MB4',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb4),
                                               "ManyBabies collaborators in MB4", 
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb4")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb4'))
                                  )
                         ),
                         tabPanel(title = 'MB5',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb5),
                                               "ManyBabies collaborators in MB5",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb5")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb5'))
                                  )
                         ),
                         tabPanel(title = 'MB-AtHome',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb_athome),
                                               "ManyBabies collaborators in MB-AtHome",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb_athome")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb_athome'))
                                  )
                         ),
                         tabPanel(title = 'MB1A',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1a),
                                               "ManyBabies collaborators in MB1A",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1a")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb1a'))
                                  )
                         ),
                         tabPanel(title = 'MB1B',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1b),
                                               "ManyBabies collaborators in MB1B",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1b")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb1b'))
                                  )
                         ),
                         tabPanel(title = 'MB1G',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1g),
                                               "ManyBabies collaborators in MB1G",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1g")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb1g'))
                                  )
                         ),
                         tabPanel(title = 'MB1L',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1l),
                                               "ManyBabies collaborators in MB1L",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1l")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb1l'))
                                  )
                         ),
                         tabPanel(title = 'MB1N',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1n),
                                               "ManyBabies collaborators in MB1N",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1n")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb1n'))
                                  )
                         ),
                         tabPanel(title = 'MB1T',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb1t),
                                               "ManyBabies collaborators in MB1T",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb1t")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb1t'))
                                  )
                         ),
                         tabPanel(title = 'MB3N',
                                  fluidRow(
                                    column(
                                      width = 6,
                                      # A static valueBox
                                      valueBox(nrow(tab_mb3n),
                                               "ManyBabies collaborators in MB3N",
                                               icon = icon("glyphicon-blackboard"), width = 18,
                                               color = "blue"
                                      ),
                                      box("Created at", width = 18, DT::dataTableOutput("tab_mb3n")
                                      )
                                    ),
                                    column(
                                      width = 5,
                                      leafletOutput('map_mb3n'))
                                  )
                         )
      )),
      
      # people tab
      tabItem(tabName = "people",
              fluidPage(
                box(width = 10, DT::dataTableOutput("collab_global")
                    )
               )
     ),
     
      # about tab
      tabItem(tabName = "about",
              fluidPage(
                h1(strong("About:")),
                p("Learn more about the",
                  a("ManyBabies projects", href = "https://manybabies.github.io/projects"),
                  "and how to",
                  a("get involved.", href = "https://manybabies.github.io/get_involved")
                  ), 
                p("This app was developed by ",
                  a("R-Ladies", href = "http://www.rladies.org"), 
                  "and adapted by",
                  a("Rodrigo Dal Ben", href = "https://github.com/RodDalBen"), 
                  "and the",
                  a("ManyBabies team.", href = "https://manybabies.github.io/"),
                  "You can find the source code",
                  a("here.", href = "https://github.com/RodDalBen/shiny_mb_map") # update if moved
                  ), 
                img(src = "avatar-icon_cb.png", height = 200, width = 200)
                )
      )))

# ui
ui <- dashboardPagePlus(skin = "black", header, sidebar, body, useShinyjs())

# custom cluster color
green_clusters <- JS("function (cluster) {    
                      var childCount = cluster.getChildCount(); 
                      var c = ' marker-cluster-';  
                      c += 'small';
                      return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
                      }")

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

# study
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
mb1g_popups <- paste0("<b>", summary_mb1g$institution, "</b>", "<br/>",
                      "Researchers: ", summary_mb1g$researcher 
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

  # dynamic value box (color-blind friendly; like MB logo)
  output$vb_studies <- renderValueBox({
    valueBox(length(unique(mb_collaborators$studies)),
             HTML("<a style=color:white; onclick = openTab('studies'); 
                  href= \"#\">MB Studies</a>"),
             icon = icon("graduation-cap", lib = "font-awesome"), width = 3,
             color = "blue"
             )
  })
  output$vb_collaborators <- renderValueBox({
    valueBox(length(unique(mb_collaborators$researcher)),
             HTML("<a style=color:white; onclick = openTab('people'); 
                  href= \"#\">MB Collaborators</a>"),
             icon = icon("users", lib = "font-awesome"), width = 3,
             color = "orange"
             )
  })
  output$vb_institutions <- renderValueBox({
    valueBox(length(unique(mb_collaborators$institution)),
             HTML("<a style=color:white; onclick = openTab('people'); 
                  href= \"#\">MB Institutions</a>"),
             icon = icon("university", lib = "font-awesome"), width = 3,
             color = "fuchsia"
             )
  })
  output$vb_countries <- renderValueBox({
    valueBox(length(unique(mb_collaborators$country)),
             HTML("<a style=color:white; onclick = openTab('region'); 
                  href= \"#\">MB Countries</a>"),
             icon = icon("map-o", lib = "font-awesome"), width = 3,
             color = "green"
             )
  })

  # collapse sidebar
  addClass(selector = "body", class = "sidebar-collapse")
  
  # global
  output$map <- renderLeaflet({               
    leaflet(summary_global,
            options = leafletOptions(zoomControl = FALSE)
            ) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(global_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39", 
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))
  })
  
  # collab_global
  output$collab_global <- DT::renderDataTable(collab_global, options = list(pageLength = 50)) 
  
  # region
  output$collab_nortam <- DT::renderDataTable({collab_nortam})
  output$map_nortam <- renderLeaflet({
    leaflet(summary_nortam) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(nortam_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions =
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))
  })
  output$collab_latam <- DT::renderDataTable({collab_latam})
  output$map_latam <- renderLeaflet({
    leaflet(summary_latam) %>% 
      addTiles() %>%
      setView(lat = 6.664608, lng = -93.889068, zoom = 3) %>% 
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(latam_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))
  })
  output$collab_europe <- DT::renderDataTable({collab_europe})
  output$map_europe <- renderLeaflet({
    leaflet(summary_europe) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(europe_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))
  })
  output$collab_africa <- DT::renderDataTable({collab_africa})
  output$map_africa <- renderLeaflet({
    leaflet(summary_africa) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       label = purrr::map(africa_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$collab_asia <- DT::renderDataTable({collab_asia})
  output$map_asia <- renderLeaflet({
    leaflet(summary_asia) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       label = purrr::map(asia_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$collab_oceania <- DT::renderDataTable({collab_oceania})
  output$map_oceania <- renderLeaflet({
    leaflet(summary_oceania) %>% 
      addTiles() %>%
      setView(lat = -33.431441, lng = 148.313572, zoom = 3) %>% 
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(oceania_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  
  # study
  output$tab_mb1 <- DT::renderDataTable({tab_mb1})
  output$map_mb1 <- renderLeaflet({
    leaflet(summary_mb1) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb1_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = markerClusterOptions(
                         maxClusterRadius = 30,
                         iconCreateFunction = green_clusters
                         ))  
  })
  output$tab_mb2 <- DT::renderDataTable({tab_mb2})
  output$map_mb2 <- renderLeaflet({
    leaflet(summary_mb2) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       label = purrr::map(mb2_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))  
  })
  output$tab_mb3 <- DT::renderDataTable({tab_mb3})
  output$map_mb3 <- renderLeaflet({
    leaflet(summary_mb3) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       label = purrr::map(mb3_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$tab_mb4 <- DT::renderDataTable({tab_mb4})
  output$map_mb4 <- renderLeaflet({
    leaflet(summary_mb4) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       label = purrr::map(mb4_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$tab_mb5 <- DT::renderDataTable({tab_mb5})
  output$map_mb5 <- renderLeaflet({
    leaflet(summary_mb5) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb5_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))  
  })
  output$tab_mb_athome <- DT::renderDataTable({tab_mb_athome})
  output$map_mb_athome <- renderLeaflet({
    leaflet(summary_mb_athome) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb_athome_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$tab_mb1a <- DT::renderDataTable({tab_mb1a})
  output$map_mb1a <- renderLeaflet({
    leaflet(summary_mb1a) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb1a_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$tab_mb1b <- DT::renderDataTable({tab_mb1b})
  output$map_mb1b <- renderLeaflet({
    leaflet(summary_mb1b) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb1b_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$tab_mb1g <- DT::renderDataTable({tab_mb1g})
  output$map_mb1g <- renderLeaflet({
    leaflet(summary_mb1g) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb1g_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                         )) 
  })
  output$tab_mb1l <- DT::renderDataTable({tab_mb1l})
  output$map_mb1l <- renderLeaflet({
    leaflet(summary_mb1l) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb1l_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$tab_mb1n <- DT::renderDataTable({tab_mb1n})
  output$map_mb1n <- renderLeaflet({
    leaflet(summary_mb1n) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb1n_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              ))  
  })
  output$tab_mb1t <- DT::renderDataTable({tab_mb1t})
  output$map_mb1t <- renderLeaflet({
    leaflet(summary_mb1t) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, 
                       label = purrr::map(mb1t_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  output$tab_mb3n <- DT::renderDataTable({tab_mb3n})
  output$map_mb3n <- renderLeaflet({
    leaflet(summary_mb3n) %>% 
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       label = purrr::map(mb3n_popups, htmltools::HTML),
                       labelOptions = labelOptions(textsize = "12px"),
                       color = ~ "#6ecc39",
                       radius = 7,
                       fillOpacity = 0.8,
                       clusterOptions = 
                         markerClusterOptions(maxClusterRadius = 30,
                                              iconCreateFunction = green_clusters
                                              )) 
  })
  
  }

shinyApp(ui, server)
