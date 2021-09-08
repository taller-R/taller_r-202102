# Elaborado por: Eduard Martinez
# Colaboradores: 
# Fecha de elaboracion: 04/09/2021
# Ultima modificacion: 07/09/2021

# configuracion inicial 
rm(list = ls()) # limpia el entorno de R
require(pacman)
p_load(rio,skimr,tidyverse,png,grid) # Llamar y/o instalar las librerías de la clase
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

# rio:
# skimr:
# tidyverse:

# Limpiar datos antes de esta clase...
dev.off()
grid.raster(readPNG("data_5/input/pics/before.png"))

#------------------------------------------------------------------------------#
#------------- Importar datos de la gran encuesta integrada (geih) ------------#
#------------------------------------------------------------------------------#

# Enero
c_gen1 = import(file = "data_5/input/01.Cabecera - Caracteristicas generales (Personas).csv")
ocu1 = import(file = "data_5/input/01.Cabecera - Ocupados.csv")

#febrero
c_gen2 = import(file = "data_5/input/02.Cabecera - Características generales (Personas).csv")
ocu2 = import(file = "data_5/input/02.Cabecera - Ocupados.csv")


#------------------------------------------------------------------------------#
#------------------ filtrar & seleccionar filas o columnas --------------------#
#------------------------------------------------------------------------------#

#----------------------------#
#---- informacion extra -----#
#----------------------------#

#---- variables 
browseURL("http://microdatos.dane.gov.co/index.php/catalog/599/datafile/F344/V18089", browser = getOption("browser")) # microdatos de geigh 2019

# por propósitos de simplificacion solo trabajemos con las siguientes columnas:

#-- caracteristicas_generales
# directorio
# orden
# secuencia_p
# mes
# p6020: sexo 
# p6040: ¿cuantos years  tiene...?
# p6160: ¿ sabe leer y escribir?


#-- ocupados 
# directorio
# orden
# secuencia_p
# mes
# p6500: Antes de descuentos ¿cuánto gano ... El mes pasado en este empleo?
# p6460s1: ¿su contrato laboral tiene una duracion de cuantos meses?
# p6426: ¿cuanto tiempo lleva trabajando en esta empresa (dias)?

# paginas web
browseURL("https://www.marsja.se/select-columns-in-r-by-name-index-letters-certain-words-with-dplyr/", browser = getOption("browser")) # Informacion de select
browseURL("https://dplyr.tidyverse.org/reference/select.html", browser = getOption("browser")) # Informacion de select
browseURL("https://dplyr.tidyverse.org/reference/filter.html", browser = getOption("browser"))# Informacion de filter


#----------------------------#
#--------- select -----------#
#----------------------------#

# Seleccionar variables
dev.off()
grid.raster(readPNG("data_5/input/pics/maybe.png"))

# llamar el numero de columnas
colnames(c_gen1)
c_gen1 %>% select(c(2:10)) %>% head() #colnames nos permite ver el nombre de las columnas, solo para simplificacion. 
c_gen1 %>% select(c(2,3,4,5,9,10)) %>% head()

# Select con parte de una palabra
c_gen1 %>%  select(starts_with("p60")) %>% head()
c_gen1 %>%  select(ends_with(c("70","60","50"))) %>% head()
data = c_gen1 %>%  select(contains("60")) 
c_gen1 %>%  select(contains("60")) %>% head()

# por  tipo de dato
c_gen1 %>%  select_if(is.character) %>% head()
c_gen1 %>%  select_if(is.numeric) %>% head()

# Eliminar columnas
c_gen1 %>%  select(!starts_with(c("p","c"))) %>% head()

# minuscula con select_all
c_gen1 %>% select_all(tolower) %>% head()

# selecionar con un vector
vector_c = c("directorio","secuencia_p","orden","p6020", "p6040", "mes", "p6160", "p6040")
vector_o = c("directorio","secuencia_p","orden", "p6500", "p6460s1", "p6426", "mes")

c_gen1 = c_gen1 %>% select_all(tolower) %>% select(vector_c) 
c_gen1 %>% head()

c_gen2 = c_gen2 %>% select_all(tolower) %>% select(vector_c)
c_gen1 %>% head()

ocu1 = ocu1 %>% select_all(tolower) %>% select(vector_o)
ocu1 %>% head()

ocu2 = ocu2 %>% select_all(tolower) %>% select(vector_o)
ocu2 %>% head()

c_gen3 = c_gen1 %>% select_all(tolower) %>% select(directorio,orden,p6020) 
c_gen3 %>% head()

#----------------------------#
#--------- subset -----------#
#----------------------------#

cat("Poner cuadro con condicionales")

# mayor de 40
c_gen2 %>% subset(p6040 >= 40) %>% head()

# entre 40 y 60 
c_gen2 %>% subset(p6040 >= 40 & p6040 <= 60) %>% head()
c_gen2 %>% subset(p6040 <= 10 | p6040 >= 60) %>% head()

# combinarlo con una una fuuncion similar a select
c_gen2 %>% subset(p6040 >= 40, select = c(p6020,p6040)) %>% head()

# drop.na para quitar las filas que contengan valores NA
is.na(ocu1$p6500) %>% table() # valores que son na = True
ocu1_na = ocu1  %>% drop_na(p6500) 
is.na(ocu1_na$p6500) %>% table()

# quitar todas las filas que tengan na
is.na(c_gen1) %>% table() 
c_gen1_na = c_gen1 %>% drop_na()
is.na(c_gen1_na) %>% table() 


#----------------------------#
#--------- filter -----------#
#----------------------------#

tidyverse_conflicts()

# similar a subset, pero hace parte de tidyverse
c_gen2 %>% dplyr::filter(p6040 >= 40) %>% select(c(p6020,p6040)) %>% head() 

# podemos filtrar y quedarnos con los valores NA
ocu1 %>% dplyr::filter(is.na(p6500)) %>% head()

# podemos filtrar para quitar los NA, agrupar los valores ens los cuales estamos interesados y sacar el total
c_gen2 %>% 
  dplyr::filter(!is.na(p6160)) %>% 
  group_by(p6020,p6160) %>% 
  summarise(total = sum(p6160)) %>% 
  mutate(p6020 = ifelse(p6020 == 1, "Hombre","Mujer"),
         p6160 = ifelse(p6160 == 1, "Lee","No Lee"))


#------------------------------------------------------------------------------#
#------------------------------- Unir bases de datos --------------------------#
#------------------------------------------------------------------------------#

#----------------------------#
#---- informacion extra -----#
#----------------------------#

# paginas web
browseURL("https://dplyr.tidyverse.org/reference/bind.html", browser = getOption("browser"))
browseURL("https://dplyr.tidyverse.org/reference/mutate-joins.html?q=full", browser = getOption("browser"))


#----------------------------#
#--------- joint_ -----------#
#----------------------------#

# Seleccionar variables
dev.off()
grid.raster(readPNG("data_5/input/pics/Types of join.png"))

#----------------- joint con el numero correcto de variables ------------------#

# left_join
dev.off()
grid.raster(readPNG("data_5/input/pics/Left join.png"))
x_left = left_join(x = c_gen1, y = ocu1, by = c("directorio","secuencia_p","orden"), suffix = c("_c", "_o"))
nrow(x_left)
nrow(c_gen1)
cat("mantuvo el numero de filas de caracteristicas general ya que es un left join")

# right_join
dev.off()
grid.raster(readPNG("data_5/input/pics/right join.png"))
x_right = right_join(c_gen1, ocu1, by = c("directorio","secuencia_p","orden"), suffix = c("", ""))
nrow(x_right)
nrow(ocu1)
cat("mantuvo el numero de filas de ocupado ya que es un right join")

# inner_join
dev.off()
grid.raster(readPNG("data_5/input/pics/inner join.png"))
x_inner = inner_join(c_gen2, ocu2, by = c("directorio","secuencia_p","orden"), suffix = c("", ""))
nrow(x_inner)
nrow(ocu2)
cat("mantuvo el numero de filas de ocupado ya que el el inner join mantiene las filas que estas tengan en comun")

# full_join
dev.off()
grid.raster(readPNG("data_5/input/pics/full join.png"))
x_full = full_join(c_gen1, ocu1, by = c("directorio","secuencia_p","orden"), suffix = c("", ""))
cat("Full join mantendra todas las columnas de ambas bases de datos")
warning("full join puede causar que datos dentro de las columnas se eliminen")


#--------------- joint sin el numero correcto de variables --------------------#

# Seleccionar variables
dev.off()
grid.raster(readPNG("data_5/input/pics/bad join.png"))

# Si no se pone las variables id correctas puedes tener filas duplicadas. 

# una variable de id
duplicated(c_gen1$directorio) %>% table() # duplicate busca dentro si hay duplicados de fillas, table se usa para resumir

# dos variables de id
duplicated(c_gen1[,c("directorio","secuencia_p")]) %>% table()

# Tres variables de id
duplicated(c_gen1[,c("directorio","secuencia_p","orden")]) %>% table()

# Ejemplo de un bad joint
mal_joint = left_join(c_gen1, ocu1, by = c("directorio","secuencia_p"), suffix = c("", ""))
new_joint = left_join(c_gen1, ocu1, by = c("directorio","secuencia_p","orden"), suffix = c("", ""))
nrow(c_gen1)
nrow(mal_joint)
cat("las filas se duplicaron")


#----------------------------#
#----- bind_cols & rows -----#
#----------------------------#

# podemos pegar los meses 1 y 2 con la funcion bind rows. 
c_gen = bind_rows(c_gen1, c_gen2)
nrow(c_gen)
nrow(c_gen1) + nrow(c_gen2)

# Podemos pegar columna con la funcion bind cols
p1 = ocupados_1_2 %>% select(1:4)
p2 = ocupados_1_2 %>% select(5:9)

junto = bind_cols(p1, p2)

#----------------------------#
#------- Export data --------#
#----------------------------#
export(ocupados_1_2, "data_5/output/ocupados mes 1 & 2.RDS")

# Limpiar datos después de esta clase...
dev.off()
grid.raster(readPNG("data_5/input/pics/after.png"))

# Verificar si existe un elmento de un vector dentro de otro vector
ocu1$directorio %in% c_gen1$directorio %>% table()
c_gen1 = c_gen1 %>% mutate(ocupado = ifelse(directorio %in% ocu1$directorio,1,0))

