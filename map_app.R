library(shiny)
library(dplyr)
library(tmap)

## Constants project-wide    
source("src/constants.R")

## Helpers for plotting data
source("src/helpers.R")

## Helper functions for loading, processing
source("src/leer_municipios_vecinos.R")
source("src/main.R")

data = load_data() %>% process_data()

## Define the UI for the Shiny App
ui <- fluidPage(
    titlePanel(TITLE_APP),
    sidebarLayout(
        sidebarPanel(textOutput("panel"), width=2),
        mainPanel(
            tabsetPanel(
                tabPanel("Mapa principal", tmapOutput("map", height = "95vh")),
                tabPanel("Histogramas",
                         plotOutput("histogramas", height = "95vh")),
                tabPanel("Hacinados hacinados",
                         tmapOutput("hacinados", height = "95vh")),
                tabPanel("Hacinados", tmapOutput("hacinados25", height = "95vh")),
                tabPanel("Población menor a 10",
                         tmapOutput("poblacion_menor_10", height = "95vh")),
                tabPanel("Más de 30 chicos + hacinados",
                         tmapOutput("chicos_hacinados", height = "95vh")),
                tabPanel("Hacinados - extra",
                         tmapOutput("hacinados_extra", height = "95vh")
                         )
            )
        )
    )
)

## Define the server for the Shiny App
server <- function(input, output) {
    output$map <- renderTmap({
        ## Initial map center and zoom level
        cortes = generar_cortes(
            as.numeric(data$radios_censalesIVS_f2$denspobl_menor_a_10), 7
        )
        ## Create the map object
        tm_shape(data$radios_censalesIVS_f2) +
            tm_polygons("total_pobl_menor_a_10", breaks = cortes, alpha = 0.3) +
            tm_shape(data$municipios_vecinos) +
            tm_polygons(border.col = 3, lwd = 3, alpha = 0, breaks = cortes)+
            tm_text("NAM") +
            tm_shape(data$localidades_alte_brown) +
            tm_polygons(border.col = 1, lwd = 2, alpha = 0)+
            tm_text("Name") +
            plot_hex(data$merenderos_lng_lat, RESOLUCION) +
            plot_hex(data$efectores_lng_lat, RESOLUCION) +
            plot_hex_in_hex(data$hex_municipios, data$hex_alte_brown)
    })
    output$histogramas <- renderPlot({
        ## Nota:
        ##   estos numeros son para Alte. Brown
        ##   - cada hexagono con resolucion 9 es del orden de 7.28+/- 0.01 hectáreas
        ##   - cada hexagono con resolucion 9 tiene longitud de 168.4+/- 0.2m
        par(mfrow=c(2, 2))
        hist(data$hex_alte_brown_pop$areash)
        hist(data$hex_alte_brown_pop$perimetro / 6)
        hist(data$radios_censalesIVS_f2$densidad_hogares_hacinados)
        hist(data$hex_alte_brown_pop$densidad_hogares_hacinados)
    })
    output$hacinados <- renderTmap({
        ## Densidad hogares hacinados por censo y por hexagono (para
        ## controlar que esta bien la transferencia)
        ## tm_shape(hex_alte_brown_pop)+  # FIXME: revisar!
        tm_shape(data$radios_censalesIVS_f2) +
            tm_polygons("densidad_hogares_hacinados",border.col = 1,
                        breaks =        generar_cortes(as.numeric(data$hex_alte_brown_pop$densidad_hogares_hacinados), 7),
                        alpha = 0.5)+
            tm_polygons("densidad_hogares_hacinados", border.col = 1,
                        breaks =            generar_cortes(as.numeric(data$radios_censalesIVS_f2$densidad_hogares_hacinados), 7),
                        alpha = 0.5)
    })
    output$hacinados25 <- renderTmap({
        cortes = generar_cortes(
    as.numeric(data$hex_alte_brown_pop$densidad_hogares_hacinados), 7
    )
        ## Hexágonos con el 25% más necesitado.
        tm_shape(data$hex_alte_brown_pop_25) +
            tm_polygons("densidad_hogares_hacinados",
                        border.col = 1,
                        breaks = cortes,
                        alpha = 0.5) +
            tm_shape(data$radios_censalesIVS_f2) +
            tm_polygons("densidad_hogares_hacinados",
                        breaks = cortes,
                        alpha = 0.5)
    })
    output$poblacion_menor_10 <- renderTmap({
        mybreaks=generar_cortes(as.numeric(data$hex_alte_brown_pop$denspobl_menor_a_10), 10)
        tm_shape(data$hex_alte_brown_pop, alpha = 0.3)+
            tm_polygons("denspobl_menor_a_10",
                        border.col = 1,
                        breaks = mybreaks,
                        alpha = 0.3) +
            tm_shape(data$radios_censalesIVS_f2) +
            tm_polygons("denspobl_menor_a_10",
                        alpha = 0.3,
                        breaks = mybreaks)
    })
    output$chicos_hacinados <- renderTmap({ 
        ## Más de 30 chicos en la unidad y alto hacinamiento.
        ## estos lugares serían prioritarios. Todos los hexagonos tienen
        ## * mas de 50 menores a 10
        ## * estan dentro del 25 % de los mas hacinados
        tm_shape(data$hex_alte_brown2, alpha = 0.3) +
            tm_polygons("index_geom",
                        border.col = 1,
                        breaks = generar_cortes(as.numeric(data$hex_alte_brown2$index_geom), 10),
                        alpha = 0.3)
    })
    output$hacinados_extra <- renderTmap({
        tm_shape(data$hex_alte_brown_pop %>%
                 rename(`número de hogares hacinados por celda`=hac_est)) +
            tm_polygons("número de hogares hacinados por celda", border.col = 1,
                        breaks =   generar_cortes(round(as.numeric(data$hex_alte_brown_pop$hac_est)),7)[-1],
                        alpha = 0.5)
    })   
}

## Run the Shiny App
shinyApp(ui, server)
