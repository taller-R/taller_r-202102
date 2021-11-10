#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 31-08-2021
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
rm(list=ls())
require(pacman) # llamar pacman
p_load(tidyverse,sf,osmdata,sp,ncdf4,raster,stars,viridis,leaflet,png,grid) # llamar y/o instalar librerias

# raster: raster(), stack() 
# stars: read_stars()
# png, grid: leer graficos

#=================#
# [1.] Motivacion #
#=================#

# Night lights and economic growth
dev.off()
grid.raster(readPNG("pics/Indonesia.png")) # Tomado de: Measuring Economic Growth from Outer Space
grid.raster(readPNG("pics/Rwanda.png")) # Tomado de: Measuring Economic Growth from Outer Space
browseURL("https://www.aeaweb.org/articles?id=10.1257/aer.102.2.994") # Ir a: Measuring Economic Growth from Outer Space

# Night lights and Covid-19
dev.off()
grid.raster(readPNG("pics/covid.png")) 
browseURL("https://economia.uniandes.edu.co/sites/default/files/observatorio/Resultados-luminosidad.pdf") # Ir a: COVID19 y actividad económica: demanda de energía y luminosidad ante la emergencia

#==================================#
# [2.] Introduccion a datos raster #
#==================================#

## qué es un raster?
dev.off()
grid.raster(readPNG("pics/raster.png")) # fuente: https://www.neonscience.org

## resolucion
dev.off()
grid.raster(readPNG("pics/rasterize.png")) # poner fuente

## bandas de un raster
dev.off()
grid.raster(readPNG("pics/rgb_raster.png")) # Imagen tomada de https://www.neonscience.org

## importar raster de luces
luces_r = raster('data_13/input/colombia_202003.tif')
luces_r
luces_s = read_stars("data_13/input/colombia_202003.tif")
luces_s
0.00416667*111000 # resolucion

## geometria
st_bbox(luces_s)
st_crs(luces_s)
st_dimensions(luces_s)

## atributos
names(luces_s)
names(luces_s) = "date_202003"

## valores del raster
luces_s[[1]] %>% max(na.rm = T)
luces_s[[1]] %>% min(na.rm = T)
luces_s[[1]] %>% as.vector() %>% summary() 
luces_s[[1]][is.na(luces_s[[1]])==T] # Reemplazar NA's
luces_s[[1]][2000:2010,2000:2010]
luces_s[[1]][2000:2010,2000:2010] %>% table() # Sustraer una parte de la matriz

## puedo reproyectar un raster?
st_crs(luces_s)
luces_new_crs = st_transform(luces_s,crs=4126)
luces_s[[1]][2000:2010,2000:2010] # no se alteran las geometrias
luces_new_crs[[1]][2000:2010,2000:2010] # no se alteran las geometrias

## hacer clip a un raster
medellin = opq(bbox = getbb("Medellín Colombia")) %>%
           add_osm_feature(key = "boundary", value = "administrative") %>% osmdata_sf()
medellin = medellin$osm_multipolygons
medellin = medellin  %>% dplyr::filter(admin_level==7) %>%  dplyr::filter(name=="Zona Urbana Medellín")
ggplot() + geom_sf(data=medellin , col="red") + theme_bw() 

luces_medellin_1 = st_crop(luces_s,medellin) # crop luces de Colombia con polygono de Medellin

ggplot() + geom_stars(data=luces_medellin_1 , aes(y=y,x=x,fill=date_202003)) + # plot raster
scale_fill_viridis(option="A" , na.value='white') +
geom_sf(data=medellin , fill=NA , col="green") + theme_bw() 

#==========================#
# [3.] Extraer informacion #
#==========================#

## load data
luces_medellin_0 = read_stars("data_13/input/colombia_202002.tif") %>% 
                   st_crop(.,medellin)
names(luces_medellin_0) = "date_202002"

ggplot() + geom_stars(data=luces_medellin_0 , aes(y=y,x=x,fill=date_202002)) + # plot raster
scale_fill_viridis(option="A" , na.value='white') +
geom_sf(data=medellin , fill=NA , col="green") + theme_bw() 

## apilar raster
luces_medellin = c(luces_medellin_0,luces_medellin_1)
luces_medellin

## get building data
house = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "house") %>% 
        osmdata_sf() %>% .$osm_polygons %>% .[,c("osm_id")] %>% mutate(building="house")

commercial = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "commercial") %>% 
             osmdata_sf() %>%  .$osm_polygons %>% .[,c("osm_id")]  %>% mutate(building="commercial")

hotel = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "hotel") %>% 
        osmdata_sf() %>%  .$osm_polygons %>% .[,c("osm_id")]  %>% mutate(building="hotel")

construction = opq(bbox = getbb("Medellín Colombia")) %>% add_osm_feature(key = "building", value = "construction") %>% 
               osmdata_sf() %>%  .$osm_polygons %>% .[,c("osm_id")]  %>% mutate(building="construction")

building = rbind(house,commercial) %>% rbind(.,hotel) %>% rbind(.,construction) %>% .[medellin,]
saveRDS(building,"data_13/output/building.rds")

building = readRDS("data_13/output/building.rds")

building %>% head()

ggplot() + geom_sf(data = building , aes(fill=building) , col=NA)

## extraer informacion de un raster (opcion 1)
luces_building = st_extract(x = luces_medellin, at = building) %>% st_as_sf()
luces_building %>% head()

## extraer informacion de un raster (opcion 2)
luces_medellin_sf = st_as_sf(x = luces_medellin, as_points = T, na.rm = T) # raster to sf (points)
luces_medellin_sf

ggplot() + geom_sf(data = luces_medellin_sf , aes(col=date_202002))  + 
scale_fill_viridis(option="A" , na.value='white') +
geom_sf(data=medellin , fill=NA , col="green") + theme_bw() 

luces_medellin_sf2 = st_as_sf(x = luces_medellin, as_points = F, na.rm = T) # raster to sf (polygons)

ggplot() + geom_sf(data = luces_medellin_sf2 , aes(fill=date_202002),col=NA)  + 
scale_fill_viridis(option="A" , na.value='white') +
geom_sf(data=medellin , fill=NA , col="green") + theme_bw()  + 
geom_sf(data = building , col=NA , fill="green")

luces_building_2 = st_join(x=building , y=luces_medellin_sf2 , largest=F) # join layers

## variacion promedio 
df = luces_building_2

st_geometry(df) = NULL

df %>% group_by(building) %>% 
summarise(pre=mean(date_202002,na.rm=T) , post=mean(date_202003,na.rm=T))

#====================#
# [4.] Download Data #
#====================#

## Datos de Luces Nocturnas
browseURL("https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html") # 1992-2013
browseURL("https://payneinstitute.mines.edu/eog/") # Datos VIIRS mensuales 201204-2021...
cat("Ver códigos")

browseURL("https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/2") # 1992-2018
cat("Ver paper")

## Datos de la NASA
browseURL("https://disc.gsfc.nasa.gov")
cat("Ver código")








