# Universidad de los Andes 
# Taller R
# Elaborado por: Juan David Vélez Pérez
# Código:       201923348

require(pacman) # require pacman
p_load(tidyverse,rio,sf,leaflet,rvest, # require and/or install packages
       osmdata, 
       xml2,  
       ggsn,    
       XML,modelsummary, stargazer,ggplot2,margins, coefplot) 

#                   REGRESIONES
data_regresiones <- readRDS("~/Semestre #6/Taller R/pset-3/input/data_regresiones.rds")

base = readRDS("~/Semestre #6/Taller R/pset-3/input/data_regresiones.rds")
variable.names(base)

model1= lm(price ~ rooms + bathrooms + property_type, data = base) # Modelo #1
model2= lm(price ~ rooms + bathrooms + dist_cole + dist_park, data = base) #MOdelo #2
model3= lm(price ~ rooms + bathrooms + property_type, data = base) #Modelo #3

grafo = modelplot(ols) + coord_flip()

coefplot (model1)
multiplot= multiplot ( model1, model2, model3, single = TRUE)
capture.output(multiplot , file = "Output/plot_regresiones.png")

resultados = stargazer(model1 , model2 , model3,  type = "text")
capture.output(resultados , file = "Output/resultados_regresiones.xlsx ")

#                     DATOS ESPACIALES
## configuracion inicial 
rm(list = ls()) # limpia el entorno de R

###Descargar datos:

getbb("Bogotá Colombia")

osm = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "parks") %>%
  osmdata_sf()
osm %>% class()
osm

amenities = osm$osm_points %>% select(osm_id,amenity)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data=amenities , weight=1 , col="green")

### Eduard duré mucho tiempo intentando y no me corre.

#                   WEB-SCRAPING Y PROCESAMIENTO DE TEXTO

myurl="https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
myhtml = read_html(myurl)
class(myhtml)
titulo = myhtml %>% html_nodes("h1") %>% html_text() 
parse = read_html(myurl) %>% htmlParse()
tablas = parse %>% readHTMLTable(header = T)
tablas[[4]] %>% class()
tablas[[4]]
Dpts_col = tablas[[4]]








