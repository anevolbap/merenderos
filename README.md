# Merenderos

He aquí el mapa interactivo de merenderos del municipio de Almirante
Brown.

## TODO:

- Improve loading data functions / cache.
- Brush up the readme.
- Add linter/actions.
- Set up renv.

## Instalación 

Clonar el repositorio o descargar el código fuente:
``` bash
git clone git@github.com:anevolbap/merenderos.git
```

Para instalar las dependencias necesarias se sugiere usar la librería
[`renv`](https://rstudio.github.io/renv/articles/renv.html "renv").
``` rdoc
# install.packages("renv")
renv::restore()
```

Para abrir la aplicación en el navegador:
``` rdoc
library(shiny)
run_app("mapa_app.R")
```

### Dependencias 

Algunas librerías pueden requerir dependencias previas:
- units: libudunits2-dev

## Datos

Los datos pueden descargarse del siguiente link.

(Nota: bajo el directorio `/data/` deben estar los datos.)

### Municipios
- Fuente: https://datos.gob.ar/dataset/ign-unidades-territoriales/archivo/ign_01.04.02

### Indicadores sociales:

- Mapa radio censal, datos Indec.
- IVS: índice vulnerabilidad por radio censal.
