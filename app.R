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
          valueBox(lenght(mb_collaborators$institution), "MB Institutions", 
                   icon = icon("university"), width = 3),
          valueBox(length(unique(mb_collaborators$country)), "MB Countries", 
                   icon = icon("map-o"), width = 3)
        ),
        leafletOutput('map', height = 700)
      ),
      
      
      ##### CONTINUE HERE! MERGE US AND CANADA! ; MAYBE NEED TO RECREATE SOME GROUPS AND INFO
      
      
      # Second sidebar tab - Region
      tabItem(tabName = "region",
              navbarPage(title = 'R-Ladies',
                         
                         tabPanel(title = 'USA',
                                  fluidRow(
                                    column(width = 4,
                                      # A static valueBox
                                      valueBox(nrow(groups_usa), "R-Ladies groups in the US", 
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_usa"))
                                    ),
                                    column(width = 6,
                                      leafletOutput('map_usa')
                                    )
                                  )
                         ),
                         tabPanel(title = 'Canada',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(groups_canada), "R-Ladies groups in Canada", 
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_canada")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_canada')
                                    )
                                  )
                         ),
                         tabPanel(title = 'Latin America',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(groups_latam), "R-Ladies groups in Latin America", 
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
                                      valueBox(nrow(groups_europe), "R-Ladies groups in Europe",
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
                                      valueBox(nrow(groups_africa), "R-Ladies groups in Africa", 
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
                                      valueBox(nrow(groups_asia), "R-Ladies groups in Asia",
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
                         tabPanel(title = 'Australia/Oceania',
                                  fluidRow(
                                    column(
                                      width = 4,
                                      # A static valueBox
                                      valueBox(nrow(groups_australia), "R-Ladies groups in Australia",
                                               icon = icon("glyphicon-blackboard"), width = 18
                                      ),
                                      box("Created at", width = 18, tableOutput("created_australia")
                                      )
                                    ),
                                    column(
                                      width = 8,
                                      leafletOutput('map_australia'))
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
                  a("here.", href = "https://github.com/RodrigoDalBen/shiny_mb_map")),
                  
                img(src = "avatar-icon_cb.png", height = 200, width = 200)
                
              )
      )
      
      ))




ui <- dashboardPage(skin = "purple", header, sidebar, body)

icons <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion",
                      markerColor = "purple")


# Set up popup content for global and regional maps
global_popups <- paste0("<b>", mb_collaborators$url, "</b>", "<br/>",
                       "Created: ", as.Date(mb_collaborators$created), "<br/>",
                       "collaborators: ", mb_collaborators$collaborators
)
usa_popups <- paste0("<b>", groups_usa$url, "</b>", "<br/>",
                               "Created: ", as.Date(groups_usa$created), "<br/>",
                               "collaborators: ", groups_usa$collaborators
)
canada_popups <- paste0("<b>", groups_canada$url, "</b>", "<br/>",
                     "Created: ", as.Date(groups_canada$created), "<br/>",
                     "collaborators: ", groups_canada$collaborators
)
latam_popups <- paste0("<b>", groups_latam$url, "</b>", "<br/>",
                     "Created: ", as.Date(groups_latam$created), "<br/>",
                     "collaborators: ", groups_latam$collaborators
)
europe_popups <- paste0("<b>", groups_europe$url, "</b>", "<br/>",
                     "Created: ", as.Date(groups_europe$created), "<br/>",
                     "collaborators: ", groups_europe$collaborators
)
africa_popups <- paste0("<b>", groups_africa$url, "</b>", "<br/>",
                     "Created: ", as.Date(groups_africa$created), "<br/>",
                     "collaborators: ", groups_africa$collaborators
)
asia_popups <- paste0("<b>", groups_asia$url, "</b>", "<br/>",
                     "Created: ", as.Date(groups_asia$created), "<br/>",
                     "collaborators: ", groups_asia$collaborators
)
australia_popups <- paste0("<b>", groups_australia$url, "</b>", "<br/>",
                      "Created: ", as.Date(groups_australia$created), "<br/>",
                      "collaborators: ", groups_australia$collaborators
)


# # Set up popup content

# mb_collaborators$fullurl <- paste0("https://www.meetup.com/", mb_collaborators$urlname, "/")
# mb_collaborators$url <- paste0("<a href='", mb_collaborators$fullurl, "'>", mb_collaborators$name, "</a>")

# popup_content <- paste0("<b>", mb_collaborators$url, "</b>", "<br/>",
#                        "Created: ", as.Date(mb_collaborators$created), "<br/>",
#                        "collaborators: ", mb_collaborators$collaborators
# )


server <- function(input, output) { 
  
  output$map <- renderLeaflet({
    leaflet(data = mb_collaborators) %>% 
      addTiles() %>%
      addAwesomeMarkers(~lon, ~lat, popup = global_popups, icon = icons)
  })
  output$created_usa <- renderTable(created_usa, striped = TRUE, hover = TRUE)
  output$map_usa <- renderLeaflet({
    leaflet(groups_usa) %>% 
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = usa_popups) 
  })
  output$created_canada <- renderTable(created_canada, striped = TRUE, hover = TRUE)
  output$map_canada <- renderLeaflet({
    leaflet(groups_canada) %>% 
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = canada_popups) 
  })
  output$created_latam <- renderTable(created_latam, striped = TRUE, hover = TRUE)
  output$map_latam <- renderLeaflet({
    leaflet(groups_latam) %>% 
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = latam_popups) 
  })
  output$created_europe <- renderTable(created_europe, striped = TRUE, hover = TRUE)
  output$map_europe <- renderLeaflet({
    leaflet(groups_europe) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = europe_popups)
  })
  output$created_africa <- renderTable(created_africa, striped = TRUE, hover = TRUE)
  output$map_africa <- renderLeaflet({
    leaflet(groups_africa) %>% 
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = africa_popups) 
  })
  output$created_asia <- renderTable(created_asia, striped = TRUE, hover = TRUE)
  output$map_asia <- renderLeaflet({
    leaflet(groups_asia) %>% 
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = asia_popups) 
  })
  output$created_australia <- renderTable(created_australia, striped = TRUE, hover = TRUE)
  output$map_australia <- renderLeaflet({
    leaflet(groups_australia) %>% 
      addTiles() %>%
      addMarkers(~lon, ~lat, popup = australia_popups) 
  })
  
  
}


shinyApp(ui, server)








