#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 27-10-2021
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino está instalada)
require(pacman) # llamar pacman
p_load(tidyverse,viridis,sf,leaflet,raster,maps) # llamar y/o instalar librerias
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

#============================================#
# [4.] Aplicación: Importar datos espaciales #
#============================================#

# source
browseURL("https://github.com/r-spatial")

# read shape
points = st_read("data_11/input/points_barranquilla.shp")
points
points %>% class() # get class

# plot points (leaflet() %>% addTiles()  = ggplot() +)
leaflet() %>% addTiles() %>% addCircleMarkers(data = points[100:110,])
ggplot() + geom_sf(data =  points[100:110,] , col="red") + theme_bw()

# geometry
points %>% crs() # get CRS

points %>% st_crs() # get CRS

points %>% st_bbox() # get bbox

points %>% st_geometry() # get vertices

# attributes
points %>% colnames() # get column names

points$MPIO_CCDGO %>% table() # frequency table

# add columns 
data = data.frame(MPIO_CCDGO= unlist(points$MPIO_CCDGO) %>% unique(),
                  rate = rnorm(n=23 , mean=1000 , sd=10) %>% round())

points = left_join(points,data,"MPIO_CCDGO") # add variable

points %>% head() # view

#========================================#
# [5.] Aplicación: Georreferenciar datos #
#========================================#

# Ejercicio en clase

# load data
df = storms
class(df)

# df to sf
sf_df = st_as_sf(x = df, coords = c("long" , "lat"), crs = "+proj=longlat +datum=WGS84")

# visualizar datos
leaflet() %>% addTiles() %>% addCircleMarkers(data = sf_df[sf_df$name=="Katrina",])

# cargar datos del mundo
world = st_as_sf(map("world", plot = FALSE, fill = TRUE))

# Pintar el recorrido de Katrina
ggplot() + 
#geom_sf(data = world , fill = "grey80", col = "grey40", lwd = 0.3) +
geom_sf(data = sf_df[sf_df$name=="Katrina",] , aes(size=hu_diameter) , color="red" , alpha=0.5)


# Otro ejemplo
andes = st_as_sf(x = read.table(text="-74.0683221  4.6014581"),
                 coords = c(1,2) , crs = "+proj=longlat +datum=WGS84")

leaflet() %>% addTiles() %>% addCircleMarkers(data = andes)













