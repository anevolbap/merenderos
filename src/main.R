## Load and process data needed for the maps to be displayed.

load_data = function(){
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

    return(list(
        municipios_vecinos=municipios_vecinos,
        localidades_alte_brown=localidades_alte_brown,
        merenderos=merenderos,
        merenderos_lng_lat=merenderos_lng_lat,
        efectores_y_hospitales=efectores_y_hospitales,
        efectores_lng_lat=efectores_lng_lat,
        radios_censales=radios_censales,
        radios_censalesIVS=radios_censalesIVS
                ))
}


process_data = function(data) {
    ## Radios censales filtrados por población menor a 15
    radios_censales_f1 <- data$radios_censales %>% filter(cod_prov_partido %in% CODIGO_ALTE_BROWN)
    radios_censalesIVS_f1 <- data$radios_censalesIVS %>%
        filter(cod_prov_partido %in% CODIGO_ALTE_BROWN) %>%
        mutate(total_pobl_menor_a_10 = edad_de_10 + edad_de_5_ + edad_de_0_)

    ## Hexagonos
    hex_municipios <- get_hexagons_from_df(data$efectores_lng_lat, res = RESOLUCION)
    hex_alte_brown <- get_inner_hexagons_from_df(data$municipios_vecinos)

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

    return(c(
        data,
        list(radios_censales_f1 = radios_censales_f1,
             radios_censalesIVS_f2 = radios_censalesIVS_f2,
             hex_municipios = hex_municipios,
             hex_alte_brown = hex_alte_brown, 
             hex_alte_brown_pop = hex_alte_brown_pop,
             hex_alte_brown_pop_25 = hex_alte_brown_pop_25,
             hex_alte_brown2 = hex_alte_brown2)
    )
           )
}
