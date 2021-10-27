#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 27-10-2021
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
if (!require("pacman")) install.packages("pacman") # Isntalar pacman (sino está instalada)
require(pacman) # llamar pacman
p_load(tidyverse,viridis,sf,leaflet,raster,png,grid) # llamar y/o instalar librerias
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

#============================================#
# [4.] Aplicación: Importar datos espaciales #
#============================================#

# source
browseURL("https://github.com/r-spatial")

# read shape
points = st_read("clase_2/input/points_barranquilla.shp")
points
points %>% class() # get class

# plot points
leaflet() %>% addTiles() %>% addCircleMarkers(data = points[,])

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

# df to sf
sf_df = st_as_sf(x = df, coords = c("lat","long"), crs = "+proj=longlat +datum=WGS84")







