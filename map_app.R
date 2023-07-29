## Load required libraries
library(shiny)
library(leaflet)
source("src/constants.R")

## Define the UI for the Shiny App
ui <- fluidPage(
    ## Application title
    titlePanel(TITLE_APP),
    
    ## Leaflet map output
    leafletOutput("map")
)

## Define the server for the Shiny App
server <- function(input, output) {
    ## Function to create a basic leaflet map
    output$map <- renderLeaflet({
        ## Initial map center and zoom level
        initial_lat <- ALMTE_BROWN_LAT 
        initial_lon <- ALMTE_BROWN_LNG 
        initial_zoom <- MAP_ZOOM_INIT ## Initial zoom level (higher number = closer view)
        
        ## Create the map object
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
                             )  %>%
             ## Set the initial map view
            setView(lng = initial_lon, lat = initial_lat, zoom = initial_zoom) %>%
            ## Add a simple marker to the map
            addMarkers(lng = initial_lon, lat = initial_lat, popup = "This is a marker!")
    })
}

## Run the Shiny App
shinyApp(ui, server)
