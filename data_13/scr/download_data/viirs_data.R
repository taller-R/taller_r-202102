#==============================================================================#
# Autor(es): Eduard Martínez
# Colaboradores: 
# Fecha creación: 02/03/2021
# Fecha última modificación: 02/03/2021
# Versión de R: 4.0.3.
#==============================================================================#

#--- limpiar entorno y cargar paquetes ---#
rm(list=ls())
require(pacman)
p_load(here , tidyverse , sf , raster , rvest , xml2, maps)
path = here('','')

# rutas a los datos
viirs = "https://www.ngdc.noaa.gov/eog/viirs/download_dnb_composites_iframe.html" # datos hasta 2019 (no requiere autenticación)
viirs2 = 'https://eogdata.mines.edu/pages/download_dnb_composites_iframe.html' # datos hasta 2020 (requiere autenticación)
viirs3 = "https://eogdata.mines.edu/nighttime_light/monthly/v10/" # Datos actualizados (requiere autenticación)

# notas sobre datos de luces
cat('En la serie de datos mensuales se utilizan dos configuraciones diferentes. 
En la primera se excluye cualquier dato afectado por luz parásita (vcm). 
En la segunda se incluye estos datos  si los valles de radiancia se han sometido al procedimiento de corrección de luz parásita (vcmsl).
Estas dos configuraciones se indican en los nombres de archivo como "vcm" y "vcmsl" respectivamente.
La versión "vcmsl", que incluye los datos corregidos por luz parásita, tendrá más cobertura de datos.
hacia los polos, pero será de calidad reducida. Depende de los usuarios determinar qué
set es mejor para sus aplicaciones. Las versiones anuales solo se realizan con el
Versión "vcm", excluyendo cualquier dato afectado por luz parásita.')

# layer colombia
country = st_as_sf(map("world", plot = F, fill = T)) %>% subset(ID=="Ecuador")

# obtener URL de los raster a descargar
lista_link = read_html(viirs) %>% html_nodes('a') %>% 
              html_attr('href') %>% as.character() %>% .[grep('vcmcfg',.)] %>% .[grep('180W00N',.)] # Ojo dejar las de 180

# select year a descargar
years = 2012:2019 
files_years = lapply(years , function(x) grep(paste0('npp_',x),lista_link)) %>% unlist()
lista_link = lista_link[files_years]

# funcion que descarga y corta los raster para Colombia
download_raster = function(url){
                   # Descargando el raster 
                   download.file(url,destfile = 'clase_5/input/temp/temporal.tgz',method = "curl")
  
                   # Descomprimiendo el .tgz
                   file = untar(paste0(path,'clase_5/input/temp/temporal.tgz'),list=TRUE) %>% .[grep('avg_rade9h.tif',.)]
                   untar(paste0(path,'clase_5/input/temp/temporal.tgz'), files=file)
                   
                   # Leyendo el raster
                   raster_completo = raster(x = file)
                   country = st_transform(country,crs = crs(raster_completo))
                   raster_country = crop(raster_completo,country) %>% mask(country)
                   
                   # Exportar raster
                   date = substr(file,11,16)
                   writeRaster(x = raster_country , filename = paste0(path,'clase_5/output/colombia_viirs_',date,'.tif') , overwrite = TRUE )
                   
                   # Remover archivos
                   file.remove(paste0(path,'clase_5/input/temp/temporal.tgz'))
                   file.remove(file)
}
lapply(lista_link,download_raster)  

#---------------------------#
# Para los años 2020 y 2021 #
#---------------------------#

# initial confguration
rm(list=ls())
path = here('','')

# load files
viirs3 = "https://eogdata.mines.edu/nighttime_light/monthly/v10/" # Datos actualizados (requiere autenticación)
years = 2020:2021
meses = 1:12
meses[1:9] = paste0("0",meses[1:9])
files = lapply(years, function(x) paste0(x,"/",x,meses,"/vcmcfg/")) %>% unlist()
files = lapply(files, function(x) paste0(viirs3,x)) %>% unlist()
files = files[1:16]

# obtener URL de los raster a descargar
lista_link = lapply(files, function(x) read_html(x) %>% html_nodes('a') %>% 
                     html_attr('href') %>% as.character() %>% .[grep('75N180W',.)])
lista_link = lista_link %>% unlist() %>% unique() %>% paste0(files,.)

# load shape
country = readRDS(paste0(path,'1_download/input/layers/colombia disolve (sin sa).rds'))

# list untar
list_untar = list.files(path=paste0(path,"1_download/input/temp/"),pattern=".tar",full.names=T)

# funcion que descarga y corta los raster para Colombia
download_raster = function(x){
                  # Descomprimiendo el .tgz
                  file = untar(x,list=TRUE) %>% .[grep('avg_rade9h.tif',.)]
                  untar(x, files=file)
                  
                  # Leyendo el raster
                  raster_completo = raster(x = file)
                  country = st_transform(country,crs = crs(raster_completo))
                  raster_country = crop(raster_completo,country) %>% mask(country)
                  
                  # Exportar raster
                  date = substr(file,11,16)
                  writeRaster(x = raster_country , filename = paste0(path,'1_download/output/viirs/colombia_',date,'.tif') , overwrite = TRUE )
                  
                  # Remover archivos
                  file.remove(x)
                  file.remove(file)
}
lapply(list_untar,download_raster)

# verificar que todos los archivos peguen
all_files = list.files(paste0(path,'1_download/output/viirs'),full.names = T)
list_raster = lapply(all_files, function(x) raster(x=x)) 
names(list_raster) = lapply(list_raster, function(x) names(x)) %>% unlist()
names(list_raster)
lapply(1:length(list_raster),function(x) isTRUE(all.equal(st_bbox(list_raster[[39]]),st_bbox(list_raster[[x]]), check.attributes=F))) %>%
grep("FALSE",.)


