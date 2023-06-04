library(h3)
library(sf) # working with geographic data
library(dplyr) # data wrangling
library(ggplot2) # plotting
library(mapview) # plotting and exploring geographic data
library(tmap) # view the data interactively
source("src/constantes.R")
source("src/utils.R")
source("src/leer_municipios_vecinos.R")

## ---------------------------------------------------------------------------------
## Carga de datos
## ---------------------------------------------------------------------------------

## Datos municipios
municipios_vecinos <- cargar_municipios(MUNICIPIOS_SHP_PATH) %>%
    filtrar_municipios()

## Localidades de Almirante Brown
localidades_alte_brown <- st_read(BROWN_KML_PATH, "Localidades de Almirante Brown")

## Merenderos
merenderos <- st_read(COMEDORES_KML_PATH, "Merenderos")
merenderos_lng_lat <- st_as_sf(merenderos)

## Efectores y hospitales
efectores_y_hospitales <- rbind(
    st_read(BROWN_KML_PATH, "Efectores de salud"),
    st_read(BROWN_KML_PATH, "HOSPITALES PUBLICOS")
)
efectores_lng_lat <- st_as_sf(efectores_y_hospitales)

## Radios censales
radios_censales <- sf::read_sf(file.path(RADIOS_CENSALES_SHP_PATH))  %>%
    mutate(cod_prov_partido = substr(link, 1, PARTIDO_CODE_NCHAR))

## Radios censales con datos de vulnerabilidad
radios_censalesIVS <- sf::read_sf(file.path(IVS_SHP_PATH)) %>%
    mutate(cod_prov_partido = substr(link, 1, PARTIDO_CODE_NCHAR))

## ---------------------------------------------------------------------------------
## Cuentas
## ---------------------------------------------------------------------------------

## Radios censales filtrados por población menor a 15
radios_censales_f1 <- radios_censales %>% filter(cod_prov_partido %in% CODIGO_ALTE_BROWN)
radios_censalesIVS_f1 <- radios_censalesIVS %>%
    filter(cod_prov_partido %in% CODIGO_ALTE_BROWN) %>%
    mutate(total_pobl_menor_a_10 = edad_de_10 + edad_de_5_ + edad_de_0_)

## Hexagonos
hex_municipios <- get_hexagons_from_df(efectores_lng_lat, res = RESOLUCION)
hex_alte_brown <- get_inner_hexagons_from_df(municipios_vecinos)

## crs_target es el sistema de h3.
crs_target <- st_crs(hex_alte_brown)
radios_censalesIVS_f2 <- st_transform(radios_censalesIVS_f1, crs = crs_target)
radios_censalesIVS_f2 <- radios_censalesIVS_f2 %>%
    mutate(
        denspobl_menor_a_10 = total_pobl_menor_a_10 / units::set_units(st_area(geometry), hm^2),
        hogares_hacinados = hogares * porc_hogar,
        densidad_hogares_hacinados = hogares_hacinados / units::set_units(st_area(geometry),hm^2)
    )

##
crs_target <- st_crs(hex_alte_brown)
hexagon <- hex_alte_brown$geometry[100]
reference_crs <- st_crs(hex_alte_brown$geometry[1:30])
radios_censalesIVS_f2_st <- st_transform(radios_censalesIVS_f2, crs_target)

## FIXME: wtf
## Atencion estos numeros son para el hemisf. norte.
## cuando uno cambia a hemisferio sur no se sabe muy bien
unidad_area <- units::set_units(hex_area(RESOLUCION, "m2"), hm^2)
longitud_hexagono <- edge_length(res = RESOLUCION, unit = "m")
area <- 3 * sqrt(3) / 2 * (longitud_hexagono / 100)^2

## En el futuro usar parlapply en vez del for pedorro que puse abajo
pop_alte_brown <- calcular_poblacion_h3(hex_alte_brown$geometry, radios_censalesIVS_f2)
pop_h3 <- pop_alte_brown$pop
hac_est_h3 <- pop_alte_brown$hac_est

##
hex_alte_brown_pop <- st_as_sf(hex_alte_brown) %>%
    mutate(pobl_menor_a_10 = pop_h3,
           hac_est = hac_est_h3,
           areash = st_area(geometry),
           areash = units::set_units(areash,hm^2),
           denspobl_menor_a_10 = pobl_menor_a_10/areash,
           densidad_hogares_hacinados = hac_est/areash,
           perimetro = st_length(st_cast(geometry,"MULTILINESTRING")),
           perimetro = units::set_units(perimetro,m),
           )

##
hex_alte_brown_pop_25 <- hex_alte_brown_pop %>%
    filter(hac_est > quantile(hac_est, 0.75))

##
quantil_hac = quantile(hex_alte_brown_pop$hac_est, 0.75)
hex_alte_brown2 = hex_alte_brown_pop %>%
    filter(pobl_menor_a_10>50,hac_est>quantil_hac) %>%
    mutate(
        index_geom = sqrt(pobl_menor_a_10/max(pobl_menor_a_10)*hac_est/max(hac_est))
    )


## -----------------------------------------------------
## PLOTS
## -----------------------------------------------------
plot_cortes(radios = radios_censalesIVS_f1,
            cortes = c(0, 10, 100, 500, 1000, 5000),
            municipios_df = municipios_vecinos,
            localidades = localidades_alte_brown,
            columna = "total_pobl_menor_a_10"
            )

plot_cortes(radios = radios_censalesIVS_f2,
            cortes = generar_cortes(as.numeric(radios_censalesIVS_f2$denspobl_menor_a_10), 7),
            municipios_df = municipios_vecinos,
            localidades = localidades_alte_brown,
            columna = "total_pobl_menor_a_10"
            )

## Merenderos
mostrar_mapa_vecinos(municipios_vecinos) + plot_hexagons(get_hexagons_from_df(merenderos_lng_lat, RESOLUCION))

## Efectores
mostrar_mapa_vecinos(municipios_vecinos) + plot_hexagons(get_hexagons_from_df(efectores_lng_lat, RESOLUCION))

plot_hex_in_hex(hex_municipios, hex_alte_brown)

## Nota:
##   estos numeros son para Alte. Brown
##   - cada hexagono con resolucion 9 es del orden de 7.28+/- 0.01 hectáreas
##   - cada hexagono con resolucion 9 tiene longitud de 168.4+/- 0.2m
hist(hex_alte_brown_pop$areash)
hist(hex_alte_brown_pop$perimetro / 6)
hist(radios_censalesIVS_f2$densidad_hogares_hacinados)
hist(hex_alte_brown_pop$densidad_hogares_hacinados)

## Densidad hogares hacinados por censo y por hexagono
## (para controlar que esta bien la transferencia)
tm_shape(hex_alte_brown_pop)+
    tm_shape(radios_censalesIVS_f2) +
    tm_polygons("densidad_hogares_hacinados",border.col = 1,
                breaks = generar_cortes(as.numeric(hex_alte_brown_pop$densidad_hogares_hacinados), 7),
                alpha = 0.5)+
    tm_polygons("densidad_hogares_hacinados", border.col = 1,
                breaks = generar_cortes(as.numeric(radios_censalesIVS_f2$densidad_hogares_hacinados), 7),
                alpha = 0.5)

##
tm_shape(hex_alte_brown_pop %>%
         rename(`número de hogares hacinados por celda`=hac_est)) +
    tm_polygons("número de hogares hacinados por celda", border.col = 1,
                breaks = generar_cortes(round(as.numeric(hex_alte_brown_pop$hac_est)),7)[-1],
                alpha = 0.5)

## Hexágonos con el 25% más necesitado.
tm_shape(hex_alte_brown_pop_25) +
    tm_polygons("densidad_hogares_hacinados",
                border.col = 1,
                breaks = generar_cortes(as.numeric(hex_alte_brown_pop$densidad_hogares_hacinados), 7),
                alpha = 0.5) +
    tm_shape(radios_censalesIVS_f2) +
    tm_polygons("densidad_hogares_hacinados",
                breaks = generar_cortes(as.numeric(hex_alte_brown_pop$densidad_hogares_hacinados), 7),
                alpha = 0.5)

##
mybreaks=generar_cortes(as.numeric(hex_alte_brown_pop$denspobl_menor_a_10), 10)
tm_shape(hex_alte_brown_pop, alpha = 0.3)+
    tm_polygons("denspobl_menor_a_10",
                border.col = 1,
                breaks = mybreaks,
                alpha = 0.3) +
    tm_shape(radios_censalesIVS_f2) +
    tm_polygons("denspobl_menor_a_10",
                alpha = 0.3,
                breaks = mybreaks)

## Más de 30 chicos en la unidad y alto hacinamiento.
## estos lugares serían prioritarios. Todos los hexagonos tienen
## * mas de 50 menores a 10
## * estan dentro del 25 % de los mas hacinados
tm_shape(hex_alte_brown2, alpha = 0.3) +
    tm_polygons("index_geom",
                border.col = 1,
                breaks = generar_cortes(as.numeric(hex_alte_brown2$index_geom), 10),
                alpha = 0.3)
