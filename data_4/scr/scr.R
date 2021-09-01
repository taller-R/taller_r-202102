# Elaborado por: Eduard Martinez
# Colaboradores: 
# Fecha de elaboracion: 31/08/2021
# Ultima modificacion: 31/08/2021

# configuracion inicial 
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(rio,readxl,haven,skimr,WriteXLS,tidyverse) # Llamar y/o instalar las librerías de la clase
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # Encoding UTF-8

# skimr:
# rio:
# readxl:
# haven:
# skimr:
# WriteXLS:
# tidyverse:

#------------------------------------------------------------------------------#
#-------------------- 1. importar & exportar base de datos --------------------#
#------------------------------------------------------------------------------#


#--------------------------#
# 1.1 readxl, haven, write #         
#--------------------------#


#---- 1.1.1 Import --------#
# informacion extra
browseURL("https://readxl.tidyverse.org/" , browser = getOption("browser"))
browseURL("https://haven.tidyverse.org/" , browser = getOption("browser"))
?read.csv
?read_excel
?read_dta
?readRDS 
?load

# Importar bases de datos en formato .csv
data_csv = read.csv(file = "data_4/input/censo 2018.csv" ,sep = ",", header = T, stringsAsFactors = F, skip = 5) 
head(data_csv) # Ver primeras observaciones
str(data_csv) # Inspeccionar las variables del dataframe

# Importar bases de datos en formato .xls y .xlsx 
cat("Importar base de datos: hurto-personas-2020_0.xlsx")
data_xls = read_excel(path = "" , sheet = "Sheet1" , col_names = TRUE, skip = 9) 
head(data_xls)
str(data_xls) 

# Importar bases de datos en formato .dta
cat("Importar base de datos: Area - Caracteristicas generales (Personas).dta")
data_dta = read_dta(file = "") 
head(data_dta)
str(data_dta)

# Importar bases de datos en formato .rds
cat("Importar base de datos: proyecciones DANE 2005-2020.rds")
data_rds = readRDS() 
head(data_dta)
str(data_rds) 

# Importar bases de datos en formato .Rdata
load(file = "data_4/input/Homicidios 2020.Rdata")
head(data_rdata)
str(data_rdata)


#---- 1.1.2 Export --------#
# informacion extra
?write.csv
?WriteXLS
?write_dta
?saveRDS
?save

# Exportar bases de datos en formato .csv
write.csv(x = data_csv , file = "data_4/output/censo 2018.csv")

# Exportar bases de datos en formato .xls y .xlsx 
WriteXLS(x = "data_xls", ExcelFileName = "data_4/output/Hurtos 2020.xlsx" , SheetNames =  "Hurtos") 

# Exportar bases de datos en formato .dta
write_dta(data = data_dta ,path = "data_4/output/Area - Caracteristicas generales (Personas).dta") 

# Exportar bases de datos en formato .rds
saveRDS(object = data_rds, file = "data_4/output/proyecciones DANE 2005-2020.rds")

# xportar varias bases de datos en formato .Rdata 
save(data_rds,data_dta,data_xls,data_csv,file = "data_4/output/Datos.Rdata")

  
#-------------------------#
#         1.2 rio         #
#-------------------------#
rm(list = ls())# clean enviroment

# Informacion extra
?rio


#------ 1.2.1 Import -----#
# Informacion extra
?rio::import

# Importar bases de datos en formato .csv
data_csv = import(file = "data_4/input/censo 2018.csv" ,sep = ",", header = T, stringsAsFactors = F, skip = 6) 

# Importar bases de datos en formato .xls y .xlsx 
data_xls = import(file= "data_4/input/hurto-personas-2020_0.xlsx" , sheet = "Sheet1" , col_names = TRUE, skip = 9) 

# Importar bases de datos en formato .dta
data_dta = import(file = "data_4/input/Area - Caracteristicas generales (Personas).dta")

# Importar bases de datos en formato .rds
data_rds = import(file = "data_4/input/proyecciones DANE 2005-2020.rds") 

# Importar bases de datos en formato .Rdata
data_rdata = import(file = "data_4/input/Homicidios 2020.Rdata")


#---- 1.2.2 Import -------#
# Informacion extra
?rio::export

# exportar bases de datos en formato .csv
export(data_csv, "data_4/output/censo 2018.csv" ) 

# exportar bases de datos en formato .xls y .xlsx 
export(data_xls,"data_4/output/hurto-personas-2020_0.xlsx",overwrite = T)

# exportar bases de datos en formato .dta
export(data_dta,"data_4/output/Area - Caracteristicas generales (Personas).dta")

# exportar bases de datos en formato .rds
export(data_rds,"data_4/output/proyecciones DANE 2005-2020.rds") 

# exportar base de datos en formato .Rdata
export(data_rdata,"data_4/output/Homicidios 2020.Rdata")

 
#----- 1.2.3 convert -----#
# Informacion extra
?rio::convert

# Convertir a formato .csv
convert("data_4/input/Homicidios 2020.Rdata" ,"data_4/output/Homicidios 2020.csv")

# Convertir a formato .rds
convert("data_4/input/hurto-personas-2020_0.xlsx" ,"data_4/output/hurto-personas-2020_0.rds")

# Convertir a formato .dta
convert("data_4/input/proyecciones DANE 2005-2020.rds" ,"data_4/input/proyecciones DANE 2005-2020.dta")


#-------------------------#
#       1.3  skimr        #
#-------------------------#
# Informacion extra
?skimr::skim()

# 1.3.1 resumen del data -#
skim(data_csv)
skim(data_dta)
skim(data_rdata)
skim(data_rds)
skim(data_xls)


#------------------------------------------------------------------------------#
#------------------------- 2. operador pipe (%>%) -----------------------------#
#------------------------------------------------------------------------------#

# Informacion extra
browseURL("https://rsanchezs.gitbooks.io/rprogramming/content/chapter9/pipeline.html" , browser = getOption("browser"))
browseURL("https://rsanchezs.gitbooks.io/rprogramming/content/chapter9/dplyr.html" , browser = getOption("browser"))

#-------------------------#
#      2.1 sin pipe       #
#-------------------------#
# ejemplo 1
head(data_xls)

data_xls = rename_with(data_xls, tolower) # cambiar nombres a minúscula. 
data_xls = select(data_xls, c(`armas medios`,`descripcion conducta`, `codigo dane`)) #seleccionar columnas. si los nombres tienen un espacio utilizar ``

head(data_xls)

# ejemplo 2
head(mtcars)

mtcars = slice(mtcars, 1:3) # selecionamos filas 1:3
mtcars = arrange(mtcars, desc(disp)) # mayor a menor 
mtcars = filter(mtcars, disp == 160) ## disp = 160

head(mtcars)


#-------------------------#
#      2.2 con pipe       #
#-------------------------#
data_xls = import(file= "data_4/input/hurto-personas-2020_0.xlsx" , sheet = "Sheet1" , col_names = TRUE, skip = 9) #resetear a base de datos original
data(mtcars)  #resetear a base de datos original

# ejemplo 1
head(data_xls)

data_xls = data_xls %>% rename_with(tolower) %>% select(c(`armas medios`,`descripcion conducta`, `codigo dane`))

head(data_xls)

# ejemplo 2

head(mtcars)

mtcars = mtcars %>% slice( 1:3) %>% arrange(desc(disp)) %>% filter(disp == 160)

head(data_xls)

#------------------------------------------------------------------------------#
#--------------------------- 3. crear variables -------------------------------#
#------------------------------------------------------------------------------#
data(iris)
data(mtcars)


#-------------------------#
#      3.1 data$var       #
#-------------------------#
mtcars$codigo = paste(mtcars$vs,mtcars$am,mtcars$gear,mtcars$carb) # agregamos columnas

mtcars$vs = NULL # Eliminamos columnas
mtcars$am = NULL
mtcars$gear = NULL
mtcars$carb = NULL

head(mtcars)


#--------------------------#
#3.2 mutate | condicionales#
#--------------------------#

# creador de numeros aleatorios
ran = round(rnorm(nrow(iris), mean = 10, sd = 5), digits = 1)

# filtrar variables con filter 
iris2 = iris %>% filter(Sepal.Width >= 3 & Sepal.Length <= 5.5)
        head(iris2)

# numero aleatorios mas filtrar las species virginica y setosa
iris3 = iris %>% 
        mutate(r_num = ran) %>%
        filter(Species == "virginica" | Species == "setosa" & r_num >= 10 & r_num <= 15 ) 
head(iris3)

# elegir species que no sean virginica
iris4 = iris[iris$Species != "virginica",]
head(iris4)


#-------------------------#
#3.3 mutate & condiconales#
#-------------------------#
# podemos usar condicionales para crear una nueva columna
iris5 = iris %>% 
        mutate(setosa.petal.width = ifelse(test = Species =="setosa", yes = Petal.Width, no = NA)) %>%
        mutate(virginica.petal.width = ifelse(test = Species !="setosa" & Species !="versicolor", yes = Petal.Width, no = NA)) %>% 
        mutate(versicolor_setosa.sepal.width = ifelse(test = Species =="versicolor" | Species =="setosa" , yes = Sepal.Width, no = NA))

head(iris5)  


#-------------------------#
#- 3.4 mutate & substr() -#
#-------------------------#

# 3.4.1 rellena la variable name_codigo
data_rds = data_rds %>% fill(name_codigo, .direction = "down") 

#  la funcion substr() te permite extraer una parte de un character
substr(x = 'Hola' , start = 2, stop = 4)
substr(x = 'Hola' , start = 1, stop = 3)

# te indica en que donde comienza y donde termina el caracter que tienes en el pattern.
str_locate(string = "Hola - todos" ,pattern = "-")
str_locate(string = "Hola - todos" ,pattern = " -")
str_locate(string = "Hola - todos" ,pattern = "- ")

# 3.4.2 separar name & codigo

# columna nombre
data_rds = data_rds %>% 
  mutate(nombre = 
           substr(x = name_codigo,
                  start = 1,
                  stop = str_locate(string = name_codigo, pattern = '-')-1))

# columna codigo
data_rds = data_rds %>% 
  mutate(codigo = 
           substr(x = name_codigo, 
                  start = str_locate(string = name_codigo, pattern = '-') + 1, 
                  stop =  str_locate(string = name_codigo, pattern = '-') + 5))

#eliminar columna name_codigo
data_rds = data_rds %>% select(!name_codigo) %>% select(nombre, codigo, everything())

head(data_rds,2)
