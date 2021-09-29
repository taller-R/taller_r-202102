# Elaborado por: Eduard Martinez
# Colaboradores: 
# Fecha de elaboracion: 19/09/2021
# Ultima modificacion: 19/09/2021


# configuracion inicial 
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(rio, tidyverse) # Llamar y/o instalar las librerías de la clase

#------------------------------------------------------------------------------#
#---------------------------- Bucles o ciclo ----------------------------------#
#------------------------------------------------------------------------------#

#--------------------------------#
#------ informacion extra -------#
#--------------------------------#

browseURL(url = "https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r", browser = getOption("browser")) # Datacamp
browseURL(url = "https://bookdown.org/jboscomendoza/r-principiantes4/estructuras-de-control.html", browser = getOption("browser")) # R para principiantes
browseURL(url = "https://intro2r.com/loops.html#for-loop", browser = getOption("browser")) # intro2r
browseURL(url = "https://dplyr.tidyverse.org/reference/storms.html", browser = getOption("browser")) # datasests packages in tidyverse

View(storms) # utilizaremos la base datos storms que provine del package de tidyverse

#--------------------------------#
#------- 1.1 Usando for ---------#
#--------------------------------#

# Usando "for" podemos imprimir el cuadrado de 1:5
vector =  c(1:8)

for (i in vector){ # no tiene que ser i, puede ser x, r, hola...
  i = i*i  # Sobreescribe i como el resultado de i*i
  print(i) # Pinta el resultado sobre la consola
}

# imprimamos las descriptivas de la base de datos 
for(i in colnames(storms)){
  print(i) %>% summary(storms[,i]) %>% print()# podemos utilizar pipe
}

#--------------------------------#
#------- 1.2 Usando while  ------#
#--------------------------------#

# imprimamos las tormentas de 
j = 1

while (j <= 10) { # condición
  print(j) 
  j = j+1 # sobrescribir j como j + 1, observemos que sucede si retiramos el 1. 
}

# numero de filas hasta 1979 en la base de datos
i = 1
while (storms[i:nrow(storms),"year"] <= 1979) { # condición
  print(i)
  i = i + 1
}

#--------------------------------#
#------- 1.3 Usando repeat ------#
#--------------------------------#
set.seed(0117) # fijar semilla

# generando números aleatorios mayores a 8
repeat{
  m = rnorm(n=1 , mean=10 , sd=2) # generar un número aleatorio (media 10 , sd 2) 
  print(m) # pintar el número sobre la consola
  if (m <= 8){ # condicional que se activa si número es menor o igual a 8
    break # detener el loop si m es menor o igual a 8 
  } 
}

#------------------------------------------------------------------------------#
#---------------------------- Controles de flujo ------------------------------#
#------------------------------------------------------------------------------#

#--------------------------------#
#------- informacion extra ------#
#--------------------------------#

browseURL(url = "https://mauricioanderson.com/curso-r-estructuras-control/", browser = getOption("browser")) # estructura de control

#--------------------------------#
#-------- 2.1. if y else --------#
#--------------------------------#

abc = letters
vocal = c("a","e","i","o","u")

#Usando solo if
for (j in abc) {
  print(j)
  if (j %in% vocal){
    print(paste0(j," - es una vocal"))
  }
}

#Usando if y else
for (letra in abc) {
  if (letra %in% vocal){
    print(paste(letra," - Es una vocal"))
  }
  else {
    print(paste(letra," - NO es una vocal")) 
  } 
}

#--------------------------------#
#---------- 2.2. Next -----------#
#--------------------------------#

# imprimimos los números impares
for(i in 1:20) {
  if(i %% 2 == 0) next # Se usa next dentro de un condicional para indicar al loop que debe saltar el siguiente elemento
  print(i)
}


# podemos encontrar la ubicacion de la tormenta durante los años 1970:179
storms_location = as.data.frame(storms[1,1:7])
r = 1
repeat{
  r = r + 1
  
  if (storms[n ,2] >= 1980  ){next
    break
  }
  
  else{
    storms_location = rbind(storms_location, storms[n ,1:7])
  }
}

#--------------------------------#
#----------  2.3. breack --------#
#--------------------------------#

# Podemos encontrar los nombres de tormentas de 1975:1980
storms_names = as.data.frame(storms[1,1])
n = 1

repeat{
  n = n + 1
  storms_names = rbind(storms_names, storms[n ,1]) # juntamos filas
  
  if (storms[n ,2] == 1980  ){ # condicional que se activa si year es igual a 1980
    
    storms_names = data.frame(table(storms_names)) %>% select(-Freq)
    break
  } 
}

#------------------------------------------------------------------------------#
#------------------------- applicacion con la geih ----------------------------#  
#------------------------------------------------------------------------------#

#--------------------------------#
#------- Importar la geih -------#
#--------------------------------#

#---- extraer los nombres de los archivos
file_names = list()

for (x in c("enero.csv/","febrero.csv/","marzo.csv/","abril.csv/")) {
  file_names[x] = as.data.frame(list.files(paste0("Data_8/Input/",x),full.names = T))
}

file_names = file_names %>% unlist()

#---- import list

# utilizando la función grep podemos encortar la posicion de los elementos que cumplan con cierta característica
grep(3,c(1,3,4,2,3,7))

grep("a" ,c("a","b","c","a","d","a"))

# con los files names 
grep("generales", file_names) # posición de los nombres que tengan caracterizaras generales

file_names[grep("generales", file_names)] # URL completo de los archivos característica general

# importamos archivos
cgen = import_list(file_names[grep("generales", file_names)], rbind = TRUE) %>% rename_with(tolower)   # importamos todos los archivos característica general
des = import_list(file_names[grep("Desocupados", file_names)], rbind = TRUE) %>% rename_with(tolower) # importamos todos los archivos de desempleados
f_tra = import_list(file_names[grep("trabajo", file_names)], rbind = TRUE) %>% rename_with(tolower)  # importamos todos los archivos de fuerza de trabajo

#---- unir archivos
geih = left_join(x = cgen, y = f_tra, by = c("directorio", "secuencia_p", "orden"), suffix = c("","")) %>% 
       left_join(x = .   , y = des, by = c("directorio", "secuencia_p", "orden"), suffix = c("",""))

 duplicated(geih[,c("directorio","secuencia_p", "orden")]) %>% table()
 
#--------------------------------#
#------------ ejemplo 2 ---------#
#--------------------------------#
 
# buscamos si una casa fue vistiada mas que 3 veces 
 total = 0
 repeat{
   total = total + 1
  
   if (geih[total, "orden"] >= 6){
     geih[total, "orden"] = paste0( geih[total, "orden"], "- sobre encuestada")
     
   }
   if (total == nrow(geih)){
     break
   }
 }

 