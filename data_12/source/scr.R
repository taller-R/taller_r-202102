#==========================================#
# Elaborado por: Eduard F Martinez-Gonzalez
# Ultima modificacion: 03-11-2021
# R version 4.1.1 (2021-08-10)
#==========================================#

# initial configuration
rm(list=ls())
require(pacman) # llamar pacman
p_load(tidyverse,viridis,sf,leaflet,osmdata,ggsn,skimr,ggmap) # llamar y/o instalar librerias
if(sessionInfo()$loadedOnly$Rcpp$Version!="1.0.7") update.packages("Rcpp") # Para la librería OSM necesitamos la versión 1.0.7 de Rcpp 
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

# para esta clase revisar
# Cap. 5 de 

#=========================#
# [1.] OpenStreetMap Data #
#=========================#

# view keys
browseURL("https://wiki.openstreetmap.org/wiki/Map_features")

# get avaliables values for amenity
available_tags("amenity")

# get bbox
getbb("Quito Ecuador")

# get amenity data
ameni_osm = opq(bbox = getbb("Quito Ecuador")) %>%
            add_osm_feature(key = "amenity", value = "bar") %>%
            add_osm_feature(key = "amenity", value = "pub") %>%
            add_osm_feature(key = "building", value = "hotel") %>%
            osmdata_sf()
ameni_osm %>% class()
ameni_osm

# get points
ameni_point = ameni_osm$osm_points

# select variables
amenitys = ameni_point %>% select(osm_id,amenity) %>%
           subset(is.na(amenity)==F)
amenitys %>% class()

# plot polygons and points
leaflet() %>% addTiles() %>% 
addCircleMarkers(data=ameni_point , weight=1 , col="green")

#==================#
# [2.] Making maps #
#==================#

# get street
street = opq(bbox = getbb("Quito Ecuador")) %>%
         add_osm_feature(key = "highway") %>%
         osmdata_sf()
street = street$osm_lines

# get boundary
quito = opq(bbox = getbb("Quito Ecuador")) %>%
        add_osm_feature(key = "boundary", value = "administrative") %>% osmdata_sf()
quito = quito$osm_multipolygons %>% subset(admin_level==9)

# plot basic map
p = ggplot() + geom_sf(data=quito, col="orange" , fill=NA , size=0.3) + 
    geom_sf(data=street, col="black" , size=0.05)  + 
    geom_sf(data=amenitys,size=0.8,shape=5,aes(col=amenity)) + theme_bw()
p 

# add scalebar and north symbol
p = p + north(data=quito , location="topleft") + 
    scalebar(data=quito , dist=5 , dist_unit="km" , transform=T , model="WGS84")
p

# make zoom
p = p + coord_sf(xlim=c(-78.615,-78.44),ylim=c(-0.36,-0.05))
p

# remove axis-labels
p = p + labs(x="",y="","Amenitys")
p

# save plot
ggsave(plot=p , filename="data_12/output/quito_map.pdf" , width=6.5 , height=8)

#=============================#
# [3.] Making maps with ggmap #
#=============================#

# get api key google maps
browseURL("http://api.census.gov/data/key_signup.html")

# access to Google Maps
register_google(key = "YOUR GOOGLE MAPS API HERE")

# plot basic map 
ggmap(get_googlemap(center = c(lon = -78.47524493549128, lat = -0.17743936182761244),
                    zoom = 14, scale = 2, maptype ='satellite', color = 'color')) 

# add data 
ggmap(get_googlemap(center = c(lon = -78.47524493549128, lat = -0.17743936182761244),
                    zoom = 11, scale = 2, maptype ='satellite', color = 'color')) +
geom_sf(data=quito, col="orange" , fill=NA , size=0.3 , inherit.aes = F) + # Con los objetos sf se debe agregar la opción (inherit.aes = F) 
geom_sf(data=amenitys,size=0.8,shape=5,aes(col=amenity) , inherit.aes = F) +
coord_sf(xlim=c(-78.615,-78.44),ylim=c(-0.36,-0.05))

#====================================#
# [3.] Modelo de equilibrio espacial #
#====================================#

# load packages with census data
p_load(tidycensus) 

# get access to the Census API
browseURL("http://api.census.gov/data/key_signup.html")

# interface with the US Census Bureau data
census_api_key("YOUR CENSUS API HERE") # 

# get Median Housing Values Cook county
chicago = get_acs(geography="block group",variables="B25077_001E",state ="IL",county="Cook County",year=2016,geometry=T)

# get Median Housing Values Suffolk County
boston = get_acs(geography="block group",variables="B25077_001E",state ="MA",county="Suffolk County",year=2016,geometry = T)

# plot layers
ggplot() + geom_sf(data=boston , col="black" , fill=NA) + theme_bw()

# another plot
leaflet(boston) %>% addTiles() %>% addPolygons(color="green",fill=NA,weight=2)

# create city centers  
chicago_cbd = st_as_sf(x = read.table(text="-87.627800  41.881998"),
                       coords = c(1,2), crs = "+proj=longlat +datum=WGS84")

boston_cbd = st_as_sf(x = read.table(text="-71.057083  42.361145"),
                      coords = c(1,2),
                      crs = "+proj=longlat +datum=WGS84")

# put everything in the same projection
chicago_cbd = chicago_cbd %>% st_transform(st_crs(chicago))

boston_cbd = boston_cbd %>% st_transform(st_crs(boston))

# view data
leaflet(boston) %>% addTiles() %>% 
addPolygons(color="green",fill=NA,weight=2) %>% addCircleMarkers(data=boston_cbd,col="red",weight=3)

# create distances
chicago$dist_CBD = st_distance(chicago,chicago_cbd) 
boston$dist_CBD = st_distance(boston,boston_cbd) 

# change units to miles
chicago$dist_CBD = as.numeric(chicago$dist_CBD)*0.000621371 
boston$dist_CBD = as.numeric(boston$dist_CBD)*0.000621371

# plot map
ggplot() + geom_sf(data=boston , col="black" , aes(fill=dist_CBD)) + 
scale_fill_viridis(option="A" , alpha=0.9 , direction=-1 , name="Dist. CBD (miles)") +
geom_sf(data=boston_cbd , col = "green" , size = 5) + theme_bw()
ggsave("clase_2/output/map_distance_boston_cbd.pdf")

# prepare data
boston$City="Boston" # create name city
chicago$City="Chicago" # create name city
chicago=chicago %>% filter(dist_CBD<=10) # keep block groups in Cook County that are within 10 miles of the city center
st_geometry(chicago)=NULL
st_geometry(boston)=NULL
dta=rbind(chicago,boston) # stack data

# scaterplot
ggplot(dta, aes(x=dist_CBD, y=estimate, color=City)) +
geom_point(shape=1) + geom_smooth(method=lm) + xlab("Distance to CBD (miles)") +
ylab("Median Housing Prices ($)") + theme_bw()
ggsave("data_12/output/figure_2.1.pdf")

