# Merenderos

Mapa interactivo de merenderos del municipio de Almirante Brown.

## Instalación 

Clonar el repositorio o descargar su código fuente.
``` bash
git clone git@github.com:anevolbap/merenderos.git
```

Instalar las dependencias necesarias manualmente o con la librería [`renv`](https://rstudio.github.io/renv/articles/renv.html "renv").
``` rdoc
# install.packages("renv")
renv::restore()
```

Por último, desde la consola de R, abrir la aplicación de Shiny en el navegador.
``` rdoc
shiny::run_app("mapa_app.R")
```

## Datos

El mapa busca los datos en el directorio `/data/` que **no
está incluido** en este repositorio. Los datos pueden descargarse del
siguiente [link](pendiente "pendiente") (pendiente).

### Fuentes originales 
- Municipios: https://datos.gob.ar/dataset/ign-unidades-territoriales/archivo/ign_01.04.02

## Colaboración

Abrir un [issue](https://docs.github.com/es/issues/tracking-your-work-with-issues/creating-an-issue "lo qué?") aquí en Github.

## Links y referencias.
- [https://r-spatial.github.io/sf/]([url](https://r-spatial.github.io/sf/))
