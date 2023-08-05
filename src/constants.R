LISTA_VECINOS = c("Municipio Almirante Brown",
                  "Lomas de Zamora",
                  "Quilmes",
                  "Florencio Varela",
                  "Presidente Perón",
                  "Esteban Echeverría")

PARTIDO_CODE_NCHAR = 5

## Archivos
MUNICIPIOS_SHP_PATH = "data/ign_municipio.shp"
BROWN_KML_PATH = "data/ALMIRANTE_BROWN_2020.kml"
COMEDORES_KML_PATH = "data/comedores_merenderos.kml"
RADIOS_CENSALES_SHP_PATH = "data/Buenos_Aires_con_datos.shp"
IVS_SHP_PATH = "data/Indice_de_Vulnerabilidad_por_Radio_Censal.shp"

## TODO: revisar!
#' Manipulacion para que las cosas peguen bien
#' NOTA: para que los datos del Indec se acoplen con IGN; le tuve que borrar un cero:
CODIGO_ALTE_BROWN = "06028"
NOMBRE_ALTE_BROWN = "Almirante Brown"

## CODIGOS_VECINOS <- c("060274", "060658", "060260", "060028", "060648", "060490")
## referencia; c(varela, quilmes, E. Etcheverria, Almte. Brown)
CODIGOS_VECINOS <- c("06274", "06658", "06260", "06028", "06648", "06490")

RESOLUCION = 9

## MAPA
ALMTE_BROWN_LAT = -34.8272 # latitud
ALMTE_BROWN_LNG = -58.3772 # longitud
MAP_ZOOM_INIT = 12 # zoom inicial (higher number = closer view)
TITLE_APP = "Merenderos"
