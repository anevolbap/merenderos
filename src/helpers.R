library(h3) # uber 

## Utilidades para procesar datos y graficar

#' Generar cortes según cuantiles
generar_cortes <- function(x, n){
    x <- as.numeric(x)
    points <- (2:n-1) / n
    ret <- c(min(x) - 1e-8,
            quantile(x, points, na.rm = TRUE),
            max(x) + 1e-8)
    return(ret)
}

#' Calcular hexágonos
get_hexagons_from_df <- function(df, res = 9){
    lat_lng <- st_coordinates(df$geometry, res = res)[,2:1]
    h3_index <- geo_to_h3(lat_lng, res = res)
    hexagons <- h3_to_geo_boundary_sf(h3_index)
    return(hexagons)
}

#' Particion interior del municipio como hexagonos
get_inner_hexagons_from_df <- function(df, municipio = NOMBRE_ALTE_BROWN, res = 9){
    df <- df[df$NAM == municipio, ]
    h3_index <- polyfill(polygon = df$geometry, res = res)
    hexagons <- h3_to_geo_boundary_sf(h3_index)
}

calcular_poblacion_h3_single <- function(hexagon, radios_censales_f2){
    crs_target <- reference_crs
    crs_input <- st_crs(radios_censalesIVS_f2)
    if(crs_target != crs_input){
        radios_censalesIVS_f2 = st_transform(radios_censales_f2, crs_target)
    }
    indices1 <- st_intersects(hexagon, radios_censalesIVS_f2$geometry);
    indices <- unlist(indices1)
    densidad <- radios_censalesIVS_f2[indices, ]$denspobl_menor_a_10
    densidad_hogares_hacinados <- radios_censalesIVS_f2[indices,]$densidad_hogares_hacinados
    areas <- st_area(st_intersection(hexagon, radios_censalesIVS_f2[indices, ]$geometry))
    areas <- units::set_units(areas, hm^2)
    pop_est <- round(sum(areas * densidad), 3)
    ## revisar lo de nbi
    ## nbi_est=round(sum(areas*densporc_hogar))
    hac_est1 <- round(sum(areas * densidad_hogares_hacinados), 5)
    return(list(pop = pop_est, hac_est = hac_est1))
}

calcular_poblacion_h3 <- function(hexagon_list, radios_censales_f2){
    #' funcion que recibe una lista de hexagonos y luego transfiere consistentemente
    #' la cantidad asociada al radio censal a cada hexagono
    #' pone la variable equivalente de cada hexagono.
    crs_target <- st_crs(hexagon_list)
    radios_censales_f2 <- st_transform(radios_censales_f2,
                                      crs_target)
    N <- length(hexagon_list)
    pop <- numeric(N)
    hac_est <- numeric(N)
    ## lleno cada hexagonito punto a punto, (un poco malo hacerlo asi,
    ## pero lapply no funco)
    for (j in 1:N) {
        result <- calcular_poblacion_h3_single(hexagon_list[j],
                                               radios_censalesIVS_f2)
        pop[j] <- result$pop
        hac_est[j] <- result$hac_est
    }
    return(list(pop = pop, hac_est = hac_est))
}

## Utilidades para graficar los mapas
#' Mostrar mapa de hexágonos
plot_hexagons <- function(hexagons_df){
    tm_shape(hexagons_df, alpha = 0.2) +
        tm_polygons(border.col = 1, alpha = 0.2)
}

#'
plot_hex <- function(hex, resolucion) {
    tm_shape(get_hexagons_from_df(hex, resolucion), alpha = 0.2) +
        tm_polygons(border.col = 1, alpha = 0.2) 
}

#'
plot_hex_in_hex <- function(hex_out, hex_in) {
    tm_shape(hex_out) +
        tm_polygons(border.col = 4, alpha = 0.4) +
        tm_shape(hex_in, alpha = 0.2) +
        tm_polygons(border.col = 3,alpha = 0.2)
}

#'
plot_cortes <- function(radios, cortes, municipios_df, localidades, columna){
    tm_shape(radios) +
        tm_polygons(columna, breaks = cortes, alpha = 0.3) +
        tm_shape(municipios_df) +
        tm_polygons(border.col = 3, lwd = 3, alpha = 0, breaks = cortes)+
        tm_text("NAM") +
        tm_shape(localidades) +
        tm_polygons(border.col = 1, lwd = 2, alpha = 0)+
        tm_text("Name")
}

plot_municipios <- function(municipios_vecinos, cortes) {
    tm_shape(municipios_vecinos) +
        tm_polygons(border.col = 3, lwd = 3, alpha = 0, breaks = cortes) +
        tm_text("NAM")
    }

plot_localidades <- function(localidades) {
    tm_shape(localidades) +
        tm_polygons(border.col = 1, lwd = 2, alpha = 0) +
        tm_text("Name")
    }

plot_histogramas <- function(data) {
    #' Nota:
    #'   estos numeros son para Alte. Brown
    #'   - cada hexagono con resolucion 9 es del orden de 7.28+/- 0.01 hectáreas
    #'   - cada hexagono con resolucion 9 tiene longitud de 168.4+/- 0.2m
    par(mfrow=c(2, 2))
    hist(data$hex_alte_brown_pop$areash)
    hist(data$hex_alte_brown_pop$perimetro / 6)
    hist(data$radios_censalesIVS_f2$densidad_hogares_hacinados)
    hist(data$hex_alte_brown_pop$densidad_hogares_hacinados)
}
