#==============================================================================#
# Autor(es): Eduard Martínez
# Colaboradores: 
# Fecha creación: 
# Fecha última modificación: 
# Versión de R: 4.0.3.
#==============================================================================#

#--- limpiar entorno y cargar paquetes ---#
rm(list=ls())
require(pacman)
p_load(here , tidyverse , sf , raster , rvest , xml2 , maps)
path = here('','')

# rutas a los datos
dmsp = "https://www.ngdc.noaa.gov/eog/dmsp/downloadV4composites.html"

# notas sobre datos de luces
cat('Los datos de DSMP están anuales desde 1992 a 2013')

# layer colombia
country = st_as_sf(map("world", plot = F, fill = T)) %>% subset(ID=="Ecuador")

# obtener URL de los raster a descargar
lista_link = read_html(dmsp) %>% html_nodes('a') %>% 
             html_attr('href') %>% as.character() %>% .[grep('v4composites',.)] %>%
             paste0("https://www.ngdc.noaa.gov",.) %>% gsub("da\nta","data",.)

# select year a descargar
years = 1992:2013 # subset years
files_years = lapply(years , function(x) grep(paste0(x,".v4"),lista_link)) %>% unlist()
lista_link = lista_link[files_years]

# funcion que descarga y corta los raster para Colombia
download_raster <- function(url){
                   # Descargando el raster 
                   download.file(url,destfile = 'clase_5/input/temp/temporal_dmsp.tgz',method = "curl")
  
                   # Descomprimiendo el .tgz
                   file <- untar(paste0(path,'clase_5/input/temp/temporal_dmsp.tgz'),list=TRUE) %>% .[grep('stable_lights.avg_vis.tif.gz',.)]
                   untar(paste0(path,'clase_5/input/temp/temporal_dmsp.tgz'), files=file , exdir=paste0(path,'clase_5/input/temp/'))
                   R.utils::gunzip(paste0(path,'clase_5/input/temp/',file))
                   
                   # Leyendo el raster
                   file = gsub(".gz","",file)
                   raster_completo = raster(x = paste0(path,'clase_5/input/temp/',file))
                   country = st_transform(country,crs = crs(raster_completo))
                   raster_country = crop(raster_completo,country) %>% mask(country)
                   
                   # Exportar raster
                   date = substr(file,1,7) 
                   writeRaster(x = raster_country , filename = paste0(path,'clase_5/output/colombia_dmsp_',date,'.tif') , overwrite = TRUE )
                   
                   # Remover archivos
                   file.remove(paste0(path,'clase_5/input/temp/temporal_dmsp.tgz'))
                   file.remove(paste0(path,'clase_5/input/temp/',file))
}
lapply(lista_link,download_raster)  

print("Listo!")

