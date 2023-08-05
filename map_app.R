## Load required libraries
library(shiny)
library(h3) # uber 
library(sf) # working with geographic data
library(dplyr) # data wrangling
library(ggplot2) # plotting
library(mapview) # plotting and exploring geographic data
library(tmap) # view the data interactively

source("src/constants.R")
source("src/helpers.R")
source("src/leer_municipios_vecinos.R")
source("src/main.R")

data = load_data() %>% process_data()
      
## MAPA
ALMTE_BROWN_LAT = -34.8272 # latitud
ALMTE_BROWN_LNG = -58.3772 # longitud
MAP_ZOOM_INIT = 12 # zoom inicial (higher number = closer view)
TITLE_APP = "Merenderos"

## Define the UI for the Shiny App
ui <- fillPage(
    ## Application title
    titlePanel(TITLE_APP),
    
    ## tmap map output
     tabsetPanel(
         tabPanel("Mapa 1", fillRow(tmapOutput("map"))),
         tabPanel("Merenderos", tmapOutput("merenderos")),
         tabPanel("Efectores", tmapOutput("efectores")),
         tabPanel("Hexágonos", tmapOutput("hex")),
         tabPanel("Histogramas", plotOutput("histogramas")),
         tabPanel("Hacinados hacinados", tmapOutput("hacinados")),
         tabPanel("Hacinados", tmapOutput("hacinados25")),
         tabPanel("Población menor a 10", tmapOutput("poblacion_menor_10")),
         tabPanel("Más de 30 chicos + hacinados", tmapOutput("chicos_hacinados")),
         tabPanel("Hacinados - extra", tmapOutput("hacinados_extra")),  
    ))

## Define the server for the Shiny App
server <- function(input, output) {
    ## Function to create a basic tmap map
    output$map <- renderTmap({
        ## Initial map center and zoom level
        initial_lat <- ALMTE_BROWN_LAT 
        initial_lon <- ALMTE_BROWN_LNG 
        initial_zoom <- MAP_ZOOM_INIT
        
        ## Create the map object
        plot_cortes(
            radios = data$radios_censalesIVS_f2,
            cortes = generar_cortes(
                as.numeric(data$radios_censalesIVS_f2$denspobl_menor_a_10), 7
            ),
            municipios_df = data$municipios_vecinos,
            localidades = data$localidades_alte_brown,
            columna = "total_pobl_menor_a_10"
        )
    })
    output$merenderos <- renderTmap({
        mostrar_mapa_vecinos(data$municipios_vecinos) +
            plot_hexagons(get_hexagons_from_df(data$merenderos_lng_lat, RESOLUCION))}
        )
    output$efectores <- renderTmap({
        mostrar_mapa_vecinos(municipios_vecinos) +
            plot_hexagons(get_hexagons_from_df(data$efectores_lng_lat, RESOLUCION))
    })
    output$hex <- renderTmap({
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
     ## Hexágonos con el 25% más necesitado.
     tm_shape(data$hex_alte_brown_pop_25) +
         tm_polygons("densidad_hogares_hacinados",
                     border.col = 1,
                     breaks =   generar_cortes(as.numeric(data$hex_alte_brown_pop$densidad_hogares_hacinados), 7),
                     alpha = 0.5) +
         tm_shape(data$radios_censalesIVS_f2) +
         tm_polygons("densidad_hogares_hacinados",
                     breaks =   generar_cortes(as.numeric(data$hex_alte_brown_pop$densidad_hogares_hacinados), 7),
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
