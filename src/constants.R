## Data filenames
MUNICIPIOS_SHP_PATH <- "data/ign_municipio.shp"
RADIOS_SHP_PATH <- "data/Buenos_Aires_con_datos.shp"
RADIOSIVS_SHP_PATH <- "data/Indice_de_Vulnerabilidad_por_Radio_Censal.shp"
BROWN_KML_PATH <- "data/ALMIRANTE_BROWN_2020.kml"
COMEDORES_KML_PATH <- "data/comedores_merenderos.kml"
MUNICIPIOS_LAYER <- "ign_municipio"
LOCALIDADES_LAYER <- "Localidades de Almirante Brown"
EFECTORES_LAYER <- "Efectores de salud"
HOSPITALES_LAYER <- "HOSPITALES PUBLICOS"
MERENDEROS_LAYER <- "Merenderos"
RADIOS_LAYER <- "Buenos_Aires_con_datos"
RADIOSIVS_LAYER <- "Indice_de_Vulnerabilidad_por_Radio_Censal"

## Municipios
MUNICIPIOS <- list("Municipio Almirante Brown",
                   "Municipio Esteban Echeverría",
                   "Municipio Florencio Varela",
                   "Municipio Lomas de Zamora",
                   "Municipio Presidente Perón",
                   "Municipio Quilmes"
                   )
MUNICIPIO_CODE_NCHAR <- 5
COD_ALTE_BROWN <- "06028"

## Map
ALMTE_BROWN_LAT <- -34.8272 # latitud
ALMTE_BROWN_LNG <- -58.3772 # longitud
MAP_ZOOM_INIT <- 12 # initial zoom (higher number <- closer view)

## Hexagons h3
RESOLUCION <- 9

## App
TITLE_APP <- "Merenderos"

## Colores
PALETA_HEXAGONOS = c("yellow", "darkgreen")

## Cache
PROCESSED_DATA_PATH = "data/merenderos_data.rdata"
