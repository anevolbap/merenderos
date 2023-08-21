library(sf)

## Load data
load_data <- function() {
    ## Datos municipios
    municipios <- st_read(dsn = MUNICIPIOS_SHP_PATH,
                          layer = "ign_municipio")
    municipios <- municipios[municipios$FNA %in% MUNICIPIOS, ]
    
    ## Localidades de Almirante Brown
    localidades <- st_read(dsn = BROWN_KML_PATH,
                           layer = "Localidades de Almirante Brown")

    ## Efectores
    efectores <- st_read(dsn = BROWN_KML_PATH,
                         layer = "Efectores de salud")

    ## Hospitales
    hospitales <- st_read(dsn = BROWN_KML_PATH,
                          layer = "HOSPITALES PUBLICOS")   

    ## Merenderos
    merenderos <- st_read(dsn = COMEDORES_KML_PATH,
                          layer = "Merenderos")

    ## Radios censales (procesar código de partido)
    radios <- read_sf(dsn = RADIOS_SHP_PATH)
    radios["cod_prov"] <- substr(radios$link, 1, MUNICIPIO_CODE_NCHAR)

    ## Radios censales con datos de vulnerabilidad (IVS)
    radiosIVS <- read_sf(dsn = RADIOSIVS_SHP_PATH)
    radiosIVS["cod_prov"] <- substr(radiosIVS$link, 1, MUNICIPIO_CODE_NCHAR)

    return(
        list(
            municipios=municipios,
            localidades=localidades,
            merenderos=merenderos,
            efectores=efectores,
            hospitales=hospitales,
            radios=radios,
            radiosIVS=radiosIVS
        ))
}

## Process data required for the maps
process_data <- function(data) {  
    ## Radios censales filtrados por población menor a 15
    radios_filt <- data$radios %>% filter(cod_prov == COD_ALTE_BROWN)
    radiosIVS_filt <- data$radiosIVS %>%
        filter(cod_prov == COD_ALTE_BROWN) %>%
        mutate(pobl_menor_a_10 = edad_de_10 + edad_de_5_ + edad_de_0_)

    ## Hexagonos
    hex_municipios <- get_hexagons_from_df(data$efectores, res = RESOLUCION)
    hex_alte_brown <- get_inner_hexagons_from_df(data$municipios, res = RESOLUCION)

    ## FIXME: why filtering?
    hexagon <- hex_alte_brown$geometry #[100]
    reference_crs <- st_crs(hex_alte_brown$geometry) #[1:30])

    crs_target <- st_crs(hex_alte_brown)
    radiosIVS_filt_2 <- st_transform(radiosIVS_filt, crs = crs_target)
    radiosIVS_filt_2 <- radiosIVS_filt_2 %>%
        mutate(area = units::set_units(st_area(geometry), hm^2)) %>%
        mutate(denspobl_menor_a_10 = pobl_menor_a_10 / area,
               hogares_hacinados = hogares * porc_hogar,
               densidad_hogares_hacinados = hogares_hacinados / area)

    ## FIXME: refactor this function!!! 
    pop_alte_brown <- calcular_poblacion_h3(
        hex_alte_brown$geometry,
        radios_filt,
        radiosIVS_filt_2,
        reference_crs
    )
    pop_h3 <- pop_alte_brown$pop
    hac_est_h3 <- pop_alte_brown$hac_est

    ## FIXME: should leave only one df and filter by UI.
    
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

    return(
        c(
            data,
            list(radios_f1 = radios_f1,
                 radiosIVS_f2 = radiosIVS_f2,
                 reference_crs = reference_crs,
                 hex_municipios = hex_municipios,
                 hex_alte_brown = hex_alte_brown, 
                 hex_alte_brown_pop = hex_alte_brown_pop,
                 hex_alte_brown_pop_25 = hex_alte_brown_pop_25,
                 hex_alte_brown2 = hex_alte_brown2)
        )
    )
}
