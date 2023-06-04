source("src/main.R")

RESOLUCION <- 10
lat_lng <- st_coordinates(merenderos_lng_lat$geometry,res=RESOLUCION)[,2:1]
h3_index_merenderos <- geo_to_h3(lat_lng, res = RESOLUCION)
hexagons1 <- h3_to_geo_boundary_sf(h3_index_merenderos)

tm_shape(hexagons1)+
    tm_polygons(border.col = 3)

##
almte <- municipios_vecinos[municipios_vecinos$NAM=="Almirante Brown", ]
h3_index_almte <- polyfill(polygon = almte$geometry,res = RESOLUCION)
hexagonsalte <- h3_to_geo_boundary_sf(h3_index_almte)
tm_shape(hexagonsalte)+tm_polygons(border.col = 3)

## ejemplo de distancia
distancia1 <- h3_distance(h3_index_almte[1], h3_index_merenderos)

## esto da las 41 distancias del primer hexagono a todos los efectores
distancia1 <- sapply(h3_index_almte,
         function(t){
             h3::edge_length(RESOLUCION, unit = "m") * min(h3_distance(t, h3_index_merenderos))
         })

distancia1

hexagons_distancia <- h3_to_geo_boundary_sf(h3_index_almte) %>%
  mutate(distance = distancia1,
         distancia_estimada_en_cuadras = round(sqrt(2) * distance / 100, 1))

mybreaks <- cortes(hexagons_distancia$distancia_estimada_en_cuadras, 10)
mybreaks <- c(0, 2, 4, 6, 8, 10, 30)

load("salida_indicadores_solamente.RData")

hexagonsalte3 <- hex_alte_brown2 %>%
    mutate(index_geom = round(index_geom, 2)) %>%
    rename(indice = index_geom)

tm_shape(municipios_vecinos) +
    tm_polygons(border.col = 3, lwd = 3, alpha = 0) +
    tm_text("NAM") +
    tm_shape(merenderos) +
    tm_dots() +
    tm_shape(hexagons_distancia) +
    tm_polygons(border.col = 4, lwd = 0.5, alpha = 0) +
    tm_shape(hexagons_distancia) +
    tm_polygons("distancia_estimada_en_cuadras",
                alpha=0.3,
                palette = rev(viridisLite::magma(10)),
                breaks = mybreaks,
                lwd = 0) +
    tm_shape(hexagonsalte3) +
    tm_polygons("indice",
                border.col = 2,
                breaks = generar_cortes(as.numeric(hex_alte_brown2$index_geom), 10),
                alpha = 0)
