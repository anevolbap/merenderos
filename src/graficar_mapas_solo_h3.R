library(sf) # working with geographic data
library(dplyr) # data wrangling
library(ggplot2) # plotting
library(mapview) # plotting and exploring geographic data
library(tmap) # view the data interactively
load("salida_indicadores_solamente.RData")

plot_h3(hex_df = hexagonsalte1,
        col = hac_est,
        nbreaks = 7,
        municipios_df = municipios_vecinos,
        localidades = localidades_almte_brown,
        label = "número de hogares hacinados por hexágono")

## FIXME: terminar
plot_h3 = function(hex_df, col, nbreaks, municipios_df, localidades, label) {
    breaks = cortes(round(as.numeric(df[, col])),nbreaks)[-1]

    tm_shape(municipios_df) +
        tm_polygons(border.col = 3, lwd = 5, alpha = 0, breaks = breaks) +
        tm_text("NAM") +
        tm_shape(localidades) +
        tm_polygons(fill = NULL,border.col = 1,lwd = 3, alpha = 0)+
        tm_text("Name", size = 1.4, fontface = "bold")+
        tm_shape(
            hex_df %>% mutate(col = floor(col))%>%
            rename(label = col)
        ) +
        tm_polygons(label,
                    border.col = 1,
                    breaks = breaks,
                    alpha = 0.3)
}

## Map 1: Hogares hacinados
mybreaks_hogares_hacinados_hex=cortes(round(as.numeric(hexagonsalte1$hac_est)),7)
mybreaks_hogares_hacinados_hex=mybreaks_hogares_hacinados_hex[-1]
tm_shape(municipios_vecinos) +
    tm_polygons(border.col = 3,lwd=5,alpha=0,breaks=mybreaks)+
    tm_text("NAM") +
    tm_shape(localidades_almte_brown) +
    tm_polygons(fill=NULL,border.col = 1,lwd=3,alpha=0)+
    tm_text("Name",size=1.4,fontface = "bold")+
    tm_shape(
        hexagonsalte1%>%mutate(hac_est=floor(hac_est))%>%
        rename(`número de hogares hacinados por hexágono`=hac_est))+
    tm_polygons("número de hogares hacinados por hexágono",
                border.col = 1,
                breaks=mybreaks_hogares_hacinados_hex,
                alpha=0.3)

## Map 2:
## Cantidad de niños menores que 10
mybreaks_pobl_menor_a_10_hex=cortes(round(as.numeric(hexagonsalte1$pobl_menor_a_10)),7)
mybreaks_pobl_menor_a_10_hex=mybreaks_pobl_menor_a_10_hex[-1]
mybreaks_pobl_menor_a_10_hex=c(0,20,60,80,100,120,200,400,600,800);
tm_shape(municipios_vecinos) +
    tm_polygons(border.col = 3,lwd=5,alpha=0,breaks=mybreaks)+
    tm_text("NAM") +
    tm_shape(localidades_almte_brown) +
    tm_polygons(fill=NULL,border.col = 1,lwd=3,alpha=0)+
    tm_text("Name",size=1.4,fontface = "bold")+
    tm_shape(
        hexagonsalte1%>%mutate(hac_est=floor(hac_est))%>%
        rename(`número de niños menores a 10 años por hexágono`=pobl_menor_a_10))+
    tm_polygons("número de niños menores a 10 años por hexágono",
                border.col = 1,
                breaks=mybreaks_pobl_menor_a_10_hex,
                alpha=0.3)
## Map 3: INDICE NO FILTRADO
hexagonsalte2 = hexagonsalte1%>%
    mutate(index_geom=sqrt(pobl_menor_a_10/max(pobl_menor_a_10)*hac_est/max(hac_est)))

mybreaks_index_geom_hex=cortes(round(as.numeric(hexagonsalte2$index_geom),3),7)
mybreaks_index_geom_hex=mybreaks_index_geom_hex[-1]
tm_shape(municipios_vecinos) +
    tm_polygons(border.col = 3,lwd=5,alpha=0,breaks=mybreaks)+
    tm_text("NAM") +
    tm_shape(localidades_almte_brown) +
    tm_polygons(fill=NULL,border.col = 1,lwd=3,alpha=0)+
    tm_text("Name",size=1.4,fontface = "bold")+
    tm_shape(
        hexagonsalte2%>%mutate(index_geom=round(index_geom,2))%>%
        rename(indice=index_geom))+
    tm_polygons("indice",
                border.col = 1,
                breaks=mybreaks_index_geom_hex,
                alpha=0.3)

## Map 4: INDICE FILTRADO
hexagonsalte2=hexagonsalte1%>%filter(pobl_menor_a_10>50,hac_est>quantil_hac)%>%
    mutate(index_geom=sqrt(pobl_menor_a_10/max(pobl_menor_a_10)*hac_est/max(hac_est)))
mybreaks_index_geom_hex=cortes(round(as.numeric(hexagonsalte2$index_geom),3),7)
mybreaks_index_geom_hex=mybreaks_index_geom_hex[-1]
tm_shape(municipios_vecinos) +
    tm_polygons(border.col = 3,lwd=5,alpha=0,breaks=mybreaks)+
    tm_text("NAM") +
    tm_shape(localidades_almte_brown) +
    tm_polygons(fill=NULL,border.col = 1,lwd=3,alpha=0)+
    tm_text("Name",size=1.4,fontface = "bold")+
    tm_shape(
        hexagonsalte2%>%mutate(index_geom=round(index_geom,2))%>%
        rename(indice=index_geom))+
    tm_polygons("indice",
                border.col = 1,
                breaks=mybreaks_index_geom_hex,
                alpha=0.3)
