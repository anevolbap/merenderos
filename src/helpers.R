library(sf)
library(h3)

#' Load all required data
#' 
#' @returns A list of sf dataframes.
load_data <- function() {  
    ## Define a function to read spatial data
    read_spatial_data <- function(file_path, layer_name) {
        data <- st_read(dsn = file_path, layer = layer_name)
        return(data)
    }

    ## Define data file paths and layer names as a named list
    data_files <- list(
        municipios = list(file_path = MUNICIPIOS_SHP_PATH, layer_name = MUNICIPIOS_LAYER),
        localidades = list(file_path = BROWN_KML_PATH, layer_name = LOCALIDADES_LAYER),
        efectores = list(file_path = BROWN_KML_PATH, layer_name = EFECTORES_LAYER),
        hospitales = list(file_path = BROWN_KML_PATH, layer_name = HOSPITALES_LAYER),
        merenderos = list(file_path = COMEDORES_KML_PATH, layer_name = MERENDEROS_LAYER),
        radios = list(file_path = RADIOS_SHP_PATH, layer_name = RADIOS_LAYER),
        radiosIVS = list(file_path = RADIOSIVS_SHP_PATH, layer_name = RADIOSIVS_LAYER)
    )

    ## Use lapply to read spatial data for each data file
    spatial_data_list <- lapply(data_files, function(file_info) {
        read_spatial_data(file_info$file_path, file_info$layer_name)
    })

    return(spatial_data_list)
}

#' Process data required for the maps
#'
#' @params data A list of sf dataframes.
#' 
#' @returns An extended list with processed data.
process_data <- function(data) {

    ## FIXME: revisit this substr!
    ## Some cleaning
    data$radiosIVS["cod_prov"] <- substr(data$radiosIVS$link, 1, MUNICIPIO_CODE_NCHAR)
    data$municipios <- data$municipios[data$municipios$FNA %in% MUNICIPIOS, ]
    data$radios["cod_prov"] <- substr(data$radios$link, 1, MUNICIPIO_CODE_NCHAR)

    ## FIXME: avoid loop and move to helper!
    ## Get hexagons
    hex_municipios <- list()
    for (localidad in 1:nrow(data$localidades)) {
        hex_municipios = c(
            hex_municipios,
            polyfill(
                data$localidades$geometry[localidad],
                res = RESOLUCION
            )
        )
    }
    hex_municipios <- h3_to_geo_boundary_sf(unlist(hex_municipios))
    hex_alte_brown <- list()
    for (localidad in 1:nrow(data$municipios)) {
        hex_alte_brown = c(
            hex_alte_brown,
            polyfill(data$municipios$geometry[localidad], res = RESOLUCION)
        )
    }
    hex_alte_brown <- h3_to_geo_boundary_sf(unlist(hex_alte_brown))
    
    ## Filtered districts
    radiosIVS_filt <- data$radiosIVS %>%
        filter(cod_prov == COD_ALTE_BROWN) %>%
        mutate(pobl_menor_a_10 = edad_de_10 + edad_de_5_ + edad_de_0_) %>%
        mutate(area = units::set_units(st_area(geometry), hm^2)) %>%
        mutate(denspobl_menor_a_10 = pobl_menor_a_10 / area,
               hogares_hacinados = hogares * porc_hogar,
               densidad_hogares_hacinados = hogares_hacinados / area)  %>%
        st_transform(crs = st_crs(hex_alte_brown)) # FIXME: needed?
    
    ## FIXME: pending!
    ## Calcular la poblacion total y hacinados en un poligono.
    ##
    
    return(c(data, list(radiosIVS_filt = radiosIVS_filt,
                        hex_municipios = hex_municipios,
                        hex_alte_brown = hex_alte_brown)))
}

## FIXME: check docs!

#' Cut by quantiles
generar_cortes <- function(x, n){
    x <- as.numeric(x)
    points <- (2:n-1) / n
    ret <- c(min(x) - 1e-8,
             quantile(x, points, na.rm = TRUE),
             max(x) + 1e-8)
    return(ret)
}

#' Parse geometry into hexagons
#'
#' @params geometry A sf dataframe 
#' @params res An integer for h3 map resolution.
#'
#' @returns List of hexagon indices 
get_hexagons_from_df <- function(geometry, res = RESOLUCION){
    lat_lng <- st_coordinates(geometry, res = res)[ , 2:1]
    h3_index <- geo_to_h3(lat_lng, res = res)
    hexagons <- h3_to_geo_boundary_sf(h3_index)
    return(hexagons)
}

#' Plot a geometry as hexagons
#'
#' @params hex_df
#' @params name
#' @params resolucion
plot_hex <- function(hex_df, name, resolucion) {
    tm_shape(
        get_hexagons_from_df(hex_df, resolucion),
        alpha = 0.2,
        name=name) +
        tm_polygons(border.col = 1, alpha = 0.2) 
}

## FIXME: se puede borrar?
plot_histogramas <- function(data) {
    #' Nota:
    #'   estos numeros son para Alte. Brown
    #'   - cada hexagono con resolucion 9 es del orden de 7.28+/- 0.01 hectáreas
    #'   - cada hexagono con resolucion 9 tiene longitud de 168.4+/- 0.2m
    par(mfrow=c(2, 2))
    hist(data$hex_alte_brown_pop$areash)
    hist(data$hex_alte_brown_pop$perimetro / 6)
    hist(data$radiosIVS$densidad_hogares_hacinados)
    hist(data$hex_alte_brown_pop$densidad_hogares_hacinados)
}


## FIXME: arreglar esto!
calcular_poblacion_h3 <- function(hexagon, radios) {
    #' funcion que recibe una lista de hexagonos y luego transfiere
    #consistentemente ' la cantidad asociada al radio censal a cada
    #hexagono ' pone la variable equivalente de cada hexagono.
    indices <- unlist(st_intersects(hexagon, radios$geometry))    
    filtered_radios <- radios[indices,] %>%
        mutate(areas = st_intersection(hexagon, .$geometry) %>%
                   st_area() %>%
                   units::set_units(hm^2),
               pop_est = round(sum(areas * denspobl_menor_a_10), 3),
               hac_est = round(sum(areas * densidad_hogares_hacinados), 5)) %>%
        select(pop_est, hac_est)
    return(filtered_radios)
}
