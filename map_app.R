library(shiny)
library(dplyr)
library(tmap)

source("src/constants.R")
source("src/helpers.R")

## Load and process data
data = load_data() %>% process_data()

## Define the UI for the Shiny App
ui <- fluidPage(
    titlePanel(TITLE_APP),
    sidebarLayout(
        sidebarPanel(textOutput("panel"), width=2),
        mainPanel(
            tabsetPanel(
                tabPanel("Mapa principal",
                         tmapOutput("map", height = "95vh")),
                tabPanel("Mapa hexágonos",
                        tmapOutput("map_hex", height = "95vh")),
            )
        )
    )
)

## Define the server for the Shiny App
server <- function(input, output) {
    output$map <- renderTmap({
        cortes = generar_cortes(
            data$radiosIVS_filt$denspobl_menor_a_10, 7
        )
        ## Plot localidades
        tm_shape(data$localidades, name="Localidades") +
            tm_polygons(border.col = 1, lwd = 2, col ="black", alpha = 0) +
            ## Plot population dataframe
            tm_shape(data$radiosIVS_filt) +
            tm_polygons("pobl_menor_a_10", breaks = cortes, alpha = 0.3) +
            ## Municipios vecinos
            tm_shape(data$municipios) +
            tm_polygons(border.col = 3, lwd = 3, alpha = 0, breaks = cortes) +
            tm_text("NAM") +
            ## Merenderos y efectores
            plot_hex(data$merenderos, "merenderos", RESOLUCION) +
            plot_hex(data$efectores, "efectores", RESOLUCION)
    })
    
    output$map_hex <- renderTmap({
        ## Plot hexagons
            tm_shape(data$localidades, name="Localidades") +
                tm_polygons(border.col = 1, lwd = 2, col ="black", alpha = 0) +
                plot_hex(data$hex_municipios$geometry, "local", RESOLUCION) + 
   })
}

## Run the Shiny App
shinyApp(ui, server)
