# Elaborado por: Eduard Martinez
# Colaboradores: 
# Fecha de elaboracion: 10/09/2021
# Ultima modificacion: 10/09/2021

# configuracion inicial 
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(rio,tidyverse,png,grid) # Llamar y/o instalar las librerías de la clase
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

#-------------------------------------#
#---------------- Datos --------------#
#-------------------------------------#

# utilizaremos los datos de ocupados de la clase pasada

ocu_1_2 = import(file = "data_6/input/ocupados mes 1 & 2.RDS")

skimr::skim(ocu_1_2) # Un resumen de los datos

#---- recordatorio de las columnas elegidas en la anterior clase
browseURL("http://microdatos.dane.gov.co/index.php/catalog/599/datafile/F344/V18089", browser = getOption("browser")) # microdatos de geigh 2019

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


#------------------------------------------------------------------------------#
#--------------------------- agrupar bases de datos ---------------------------#
#------------------------------------------------------------------------------#

#-------------------------------------#
#---------- Informcaion extra --------#
#-------------------------------------#
browseURL("https://fhernanb.github.io/Manual-de-R/tablas.html", browser = getOption("browser")) # Tabla de frecuencia
browseURL("https://dplyr.tidyverse.org/reference/group_by.html", browser = getOption("browser")) # group_by
browseURL("https://dplyr.tidyverse.org/reference/summarise.html", browser = getOption("browser")) # summarise

# " summarize == summarise "  son sinónimos, la diferente manera de deletrear proviene de Inglaterra (summarize) y  américa (summarise). Esta función mantiene la estructura de los datos en estilo dataframe.

#-------------------------------------#
#------------- summarise -------------#
#-------------------------------------#

# Media
mean(ocu_1_2$p6500, na.rm = TRUE)
ocu_1_2 %>% summarise( media = mean(p6500, na.rm = TRUE)) 

# Mediana
median(ocu_1_2$p6500, na.rm = TRUE)
ocu_1_2 %>% summarise( mediana = median(p6500, na.rm = TRUE)) 

# frecuencia
table(ocu_1_2$p6020) 
ocu_1_2 %>%summarise(total = table(p6020)) 

# Quartiles
quantile(ocu_1_2$p6500, na.rm = TRUE)
ocu_1_2 %>% summarise(quartiles = quantile(p6500, na.rm = TRUE)) 


#-------------------------------------#
#------ group_by() + summarise() -----#
#-------------------------------------#

# 1 variable
dev.off()
grid.raster(readPNG("data_6/input/pics/one_var.png"))
ocu_1_2 %>% group_by(p6020) %>% summarise(total = table(p6160)) # cantidad de hombres y mujeres ocupados que leen. 

ocu_1_2 %>% group_by(p6020) %>% summarise(ingresos_promedio = mean(p6500, na.rm = TRUE)) # ingresos promedios de los hombres y mujeres

# 2 variables
dev.off()
grid.raster(readPNG("data_6/input/pics/two_var.png"))
ocu_1_2 %>%  group_by(p6020, p6160)  %>% summarise(total = table(p6020),  ingresos_promedio = mean(p6500, na.rm = TRUE))  # cantidad de hombres y mujeres ocupados que saben escribir y sus ingresos promedios

#-------------------------------------#
#- group_by() + mutate() + summarize -#
#-------------------------------------#

#---------------------------------- 1 variable --------------------------------#

# Total de hombres y mujeres
poblacion_sexo = ocu_1_2 %>% 
                  group_by(p6020) %>%
                  mutate(p6020 = ifelse(test = p6020 == 1, "hombre", "mujer")) %>% 
                  summarise(total = table(p6020))

poblacion_sexo

#---------------------------------- 2 variables -------------------------------#

# total de  hombres y mujeres divididos por rangos de edad
rango_sexo = ocu_1_2 %>% 
              mutate(rango = ifelse(test = p6040 <= 25, "joven",
                                    ifelse(test = p6040 >= 25 & p6040 <= 50, "adulto",
                                            ifelse(test = p6040 >= 50 & p6040 <= 65, "alta_edad", "retirado"))),
                     p6020 = ifelse(test = p6020 == 1, "hombre", "mujer")) %>% 
              group_by(rango, p6020) %>% 
              summarise(total = table(p6020))

rango_sexo

#---------------------------------- 3 variables -------------------------------#

# tasa de actividad dividida por sexo, analfabetism, mes, y sus ingresos promedios
mes_sexo_leen_ingreso_promedios = ocu_1_2 %>% 
                                    mutate(mes = ifelse(test = mes == 1, "enero", "febrero"),
                                           p6020 = ifelse(test = p6020 == 1, "hombre", "mujer"),
                                           p6160 = ifelse(test = p6160 == 1, "lee", "no_lee")) %>% 
                                    group_by(mes, p6020, p6160) %>% 
                                    summarise(total = table(p6020),
                                              ingreso_promedio =  median(p6500, na.rm = TRUE))

mes_sexo_leen_ingreso_promedios

# rango de edad de hombres y mujeres que leen y sus ingresos promedios 
rango_sexo_leen_ingreso_promedios = ocu_1_2 %>% 
                                      mutate(rango = ifelse(test = p6040 <= 25, "joven",
                                                            ifelse(test = p6040 >= 25 & p6040 <= 50, "adulto",
                                                                   ifelse(test = p6040 >= 50 & p6040 <= 65, "alta_edad", "retirado"))),
                                              p6020 = ifelse(test = p6020 == 1, "hombre", "mujer"),
                                              p6160 = ifelse(test = p6160 == 1, "lee", "no_lee")) %>% 
                                       group_by(rango, p6020, p6160) %>% 
                                       summarise(total = table(p6020),
                                       ingreso_promedio =  median(p6500, na.rm = TRUE))

rango_sexo_leen_ingreso_promedios


#------------------------------------------------------------------------------#
#-------------------- Pivotear o transponer bases de datos --------------------#
#------------------------------------------------------------------------------#

#-------------------------------------#
#---------- Informcaion extra --------#
#-------------------------------------#
browseURL("https://tidyr.tidyverse.org/articles/pivot.html", browser = getOption("browser")) # pivot
browseURL("https://www.garrickadenbuie.com/project/tidyexplain/", browser = getOption("browser")) # Pivot theory

#----------------------------#
#------- pivot_wider --------#
#----------------------------#

# disminuye el numero de filas y aumenta el numero de columnas.
browseURL("https://lectures-r.gitlab.io/lecture_6/#/pivot",browser = getOption("browser"))

# 2 columnas -> 2 columnas
poblacion_sexo

wide_poblacion_sexo= poblacion_sexo %>% pivot_wider(names_from = p6020, 
                                                    values_from = total )  
wide_poblacion_sexo

# 3 columnas -> 3 columnas pero menos filas
rango_sexo

wide_rango_sexo = rango_sexo %>% pivot_wider(id_cols= c(rango,p6020), 
                                             names_from = p6020, 
                                             values_from = total )  
wide_rango_sexo

# 4 columnas - 5 columnas pero menos filas
mes_sexo_leen_ingreso_promedios

wide_mes_sexo_leen_ingreso_promedios = mes_sexo_leen_ingreso_promedios %>%  pivot_wider(id_cols= c(mes, p6020, p6160), 
                                                                                        names_from = p6160, 
                                                                                        values_from = c(total,ingreso_promedio))
wide_mes_sexo_leen_ingreso_promedios

# 5 columnas 

rango_sexo_leen_ingreso_promedios

wide_rango_sexo_leen_ingreso_promedios = rango_sexo_leen_ingreso_promedios %>% pivot_wider(id_cols= c(rango, p6020, p6160), 
                                                                                           names_from = c(p6160,rango), 
                                                                                           values_from = c(total,ingreso_promedio))
view(wide_rango_sexo_leen_ingreso_promedios)


#----------------------------#
#------- pivot_longer -------#
#----------------------------#

# disminuye el numero de columnas y aumenta el numero de filas.

# poblacion sexo
wide_poblacion_sexo

long_poblacion_sexo = wide_poblacion_sexo %>% pivot_longer(cols = c(hombre,mujer), 
                                                           names_to = "sexo")
long_poblacion_sexo

# población dividida en rangos
wide_rango_sexo

long_rango_sexo = wide_rango_sexo %>% pivot_longer( cols = c(hombre,mujer), names_to = "total")

long_rango_sexo


#----------------------------#
#---------- Export ----------#
#----------------------------#
export(wide_poblacion_sexo, file = "data_6/output/poblacion_sexo.rds")
export(wide_rango_sexo, file = "data_6/output/rango_sexo.rds")
export(wide_mes_sexo_leen_ingreso_promedios, file = "data_6/output/mes_sexo_leen_ingreso_promedios.rds")
export(wide_rango_sexo_leen_ingreso_promedios, file = "data_6/output/rango_sexo_leen_ingreso_promedios.rds")

