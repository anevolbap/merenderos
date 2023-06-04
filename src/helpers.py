## Utilidades para graficar los mapas

#' Mostrar mapa de hexágonos
plot_hexagons <- function(hexagons_df){
    tm_shape(hexagons_df, alpha = 0.2) +
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
