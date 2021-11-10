#==============================================================================#
# Autor(es): Eduard Martínez
# Colaboradores: 
# Fecha creación: 02/03/2021
# Fecha última modificación: 31/08/2021
# Versión de R: 4.0.3.
#==============================================================================#

#--- limpiar entorno y cargar paquetes ---#
rm(list=ls())
require(pacman)
p_load(here , tidyverse , sf , raster , rvest , xml2 , maps)
path = here('','')

# rutas a los datos
harmonized = "https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827/2"

# notas sobre datos de luces
cat('Estos datos fueron procesados en el paper: https://www.nature.com/articles/s41597-020-0510-y')

# layer colombia
country = st_as_sf(map("world", plot = F, fill = T)) %>% subset(ID=="Colombia")

# obtener URL de los raster a descargar
lista_link = read_html(harmonized) %>% html_nodes('a') %>% 
             html_attr('href') %>% as.character() %>% .[grep('9828827',.)]

# Descargar y descomprimir files
download.file(lista_link,destfile = '1_download/input/temp/temporal.tgz',method = "curl")
untar(paste0(path,'1_download/input/temp/temporal.tgz'), exdir = paste0(path,"1_download/output/harmonized/"))
file.remove(paste0(path,'1_download/input/temp/temporal.tgz'))

# Ler y cortar los raster
files = list.files(path = paste0(path,"1_download/output/harmonized/"),full.names = T) %>% .[grep('Harmonized_DN_NTL',.)]
list_raster = lapply(files, function(y) raster(x = y) %>% crop(country) %>% mask(country)) 
saveRDS(list_raster, paste0(path,'1_download/output/harmonized/lista_rasters_harmonized_1992_2018.rds'))








