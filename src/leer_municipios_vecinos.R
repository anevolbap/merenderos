#' Leer municipios a partir del shape file
#'
#' Fuente: https://datos.gob.ar/dataset/ign-unidades-territoriales/archivo/ign_01.04.02
cargar_municipios = function(path, download=FALSE){
    ## TODO: descargar si no esta local
    ## if (download) descargar_municipios()
    ## remove_z_coord(filename, paste(filename,"post", sep = "_")){

    return(sf::read_sf(path))
}

#' Filtra los municipios vecinos a partir del shape file
#'
#' Nota: Lomas de Zamora, al este con """Quilmes" y "Florencio Varela",
#' al sur con Presidente Perón, y al oeste con Esteban Echeverría.
#' @param map_show logical. Muestra el mapa
#' @return data.frame. Municipios
filtrar_municipios = function(df){
    df %>%
        filter(grepl(paste(LISTA_VECINOS, collapse = "|"), FNA)) %>%
        filter(NAM != "Quilmes y los  Sueldos")
}


#' Mostrar mapa
#'
#' @param df data.frame. Datos de los municipios vecinos.
#' @param etiqueta character. Etiqueta.
#' @return . Mapa
mostrar_mapa_vecinos = function(df, label="NAM"){
    tm_shape(df) +
        tm_polygons() +
        tm_text(label) +
        tm_legend()
}

#' Elimina la coordenada z
#'
#' @param df data.frame. Datos de los municipios vecinos.
#' @param etiqueta character. Etiqueta.
#' @return . Mapa
remove_z_coord = function(source, destination){
    gdal_utils(util = "vectortranslate",
               source = source,
               destination = destination,
               options = c("-dim", "XY"))
}
