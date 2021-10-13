# Elaborado por: Eduard Martinez
# Colaboradores: 
# Fecha de elaboracion: 19/09/2021
# Ultima modificacion: 19/09/2021


# configuracion inicial 
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(tidyverse) # Llamar y/o instalar las librerias de la clase

#------------------------------------------------------------------------------#
#--------------------------------- funciones ----------------------------------#
#------------------------------------------------------------------------------#

#--------------------------------#
#------ informacion extra -------#
#--------------------------------#
browseURL(url = "https://fhernanb.github.io/Manual-de-R/creafun.html", browser = getOption("browser")) # partes de una función
browseURL(url = "https://es.r4ds.hadley.nz/funciones.html", browser = getOption("browser")) # cuando se debería escribir una función
browseURL(url = "https://www.datamentor.io/r-programming/function/", browser = getOption("browser")) # everything about a function
browseURL(url = "https://www.r-bloggers.com/2016/02/functions-exercises/", browser = getOption("browser")) # ejercicios extras

#--------------------------------#
#-------- función básica --------#
#--------------------------------#

# función que pega dos palabras y las convierte en mayúscula 

unidor = function(x, y){
         palabra = paste(x ,y) %>% toupper()
         return(palabra)
}

unidor(x = "hola", y = "mundo")

#--------------------------------#
#-------- usando un loop --------#
#--------------------------------#

# funcione que extrae las vocales dentro de una frase
vocales = function(frase) {
          finaly = list()
          for (i in 1:nchar(frase)) { 
            
            letra = substr(frase,i,i) %>% tolower()
              
            if(letra %in% c("a","e","i","o","u")){
              
                finaly[i] = letra
            }
          }
           finaly = finaly %>% unlist() %>% unique()
            return(finaly)
}

vocales(frase = "hola clase")

#------------------------------------------------------------------------------#
#----------------------------- Message, warnings, errors ----------------------#
#------------------------------------------------------------------------------#

#--------------------------------#
#------ informacion extra -------#
#--------------------------------#
browseURL(url = "https://mauricioanderson.com/curso-r-debugging/", browser = getOption("browser")) # debuging
browseURL(url = "https://adv-r.hadley.nz/conditions.html", browser = getOption("browser")) # errors, messages and warnings

#--------------------------------#
#----------- message ------------#
#--------------------------------#

# funcion que divide un numero hasta que este sea un numero impar
num_2 = function(x){
        c = x*x
return(c)
}

num_2(x = 4)
num_2(x = "A")


# funcion que divide un numero hasta que este sea un numero impar
num_2 = function(numero){
  
        # si es un numero
        if (is.numeric(numero)){
            c = numero*numero
            return(c)
        }
  
        # si no es un numero
        if (is.numeric(numero)==F){
            warning(paste0(numero," no es un número"))
        }
}
num_2(numero = 10)
num_2(numero = "hola")
num_2(numero = "10")
  
#--------------------------------#
#------------- stop -------------#
#--------------------------------#

# funcion que divide un numero hasta que este sea un numero impar
num_2 = function(numero){
  
        # si es un numero
        if (is.numeric(numero)){
            c = numero*numero
        return(c)
        }
        
        # si no es un numero
        if (is.character(numero)){
            new_num = as.numeric(numero)
            
            if (is.na(new_num)==F){
                c = new_num*new_num
                warning(paste0(numero," fue convertido a número"))
                return(c)
            }
             
            if (is.na(new_num)==T){
               stop(paste0(numero," no pudo ser convertido a número"))
            } 
        }
}
num_2(numero = 10)
num_2(numero = "5")
num_2(numero = "hola")


#------------------------------------------------------------------------------#
#---------------------------- apply, lapply & sapply --------------------------#
#------------------------------------------------------------------------------#

#--------------------------------#
#------ informacion extra -------#
#--------------------------------#  

browseURL(url = "https://www.guru99.com/r-apply-sapply-tapply.html", browser = getOption("browser")) 
browseURL(url = "http://adv-r.had.co.nz/Functionals.html", browser = getOption("browser")) 
browseURL(url = "https://www.datacamp.com/community/tutorials/r-tutorial-apply-family", browser = getOption("browser"))

#--------------------------------#
#----------- apply() ------------#
#--------------------------------#

# Operaciones por columnas
apply(X = mtcars, MARGIN = 2, FUN = summary) # margin 1 se realiza en por columnas, el dos funciona por medio de filas

# esto es equivalente a la linea 150
for (i in 1:ncol(mtcars)){
     summary(mtcars[,i]) %>% print()
}

apply(X = mtcars, MARGIN = 2, function(x) num_2(numero = x )) 

#  Operaciones por filas
apply(X = mtcars, MARGIN = 1, function(x)  min(x))

#--------------------------------#
#----------- sapply() -----------#
#--------------------------------#
descritivas_s = sapply(X = mtcars, summary) # retorna un vector o matrix, funciona por columnas 

descritivas_s

#--------------------------------#
#----------- lapply() -----------#
#--------------------------------#
descritivas_l = lapply(iris, function(x) summary(x)) # retorna siempre en estilo fila
descritivas_l %>% print()

descritivas_l[[1]] # llamamos por el numero de la lista

descritivas_l[["Sepal.Width"]] # llamamos por el nombre de la lista
descritivas_l$Sepal.Length # llamamos por el nombre de la lista


# otro ejemplo de lapply
storms
lapply(storms , function(x) table(x))

lapply(storms , function(x) is.na(x)  %>% table())

# replace NA
df = storms
replace_na = function(var){  var = ifelse(is.na(var)==T,0,var) }
df = lapply(df, function(x) replace_na(var = x)) %>% data.frame()



#------------------------------------------------------------------------------#
#----------------------------------- ejemplo ----------------------------------#
#------------------------------------------------------------------------------#

# función que extrae un resumen de datos numeric y character
resumen = function(database){

          # si la variable es tipo character realizara los siguientes funciones 
          if (is_integer(database) | is.character(database)) {
            exit = data.frame(elementos = length(database),
                              elementos_unicos = nest(tibble(elementos = unique(database))))
            return(exit)
          }
          # si las variables son numericas realizara las siguientes descriptivas
          if(is.numeric(database)) {
          exit = data.frame(type = typeof(database), 
                            minimo = min(database), 
                            q2 = quantile(group(database), prob=c(.25), na.rm = TRUE),
                            media = round(mean(database),2), 
                            q3 = quantile(database, prob=c(.75), na.rm = TRUE),
                            maximo = max(database))
          row.names(exit) = ""
          return(exit)
          }
}

resumen1 = lapply(iris, function(x) resumen(x))
resumen2 = lapply(storms, function(x) resumen(x))

#------------------------------------------------------------------------------#
#----------------------------- repaso en casa ---------------------------------#
#------------------------------------------------------------------------------#

# Funcion que devueleve un vector con 'N' caracteres aleatorios de una palabra 
caracter_vector = function(Palabra,Numero){
  
                  # Generamos un vector tan largo como 'Numero' que contiene numeros aleatorios entre 1 y el numero maximo de caracteres de 'Palabra' 
                  X = round(runif(Numero, min=1, max=nchar(Palabra)),0)
                  
                  # Generamos un vector que va a alojar las palabra y es tan largo como el vector 'X'
                  vector_final = vector(mode="character", length=length(X))
                  
                  # Si la palabra tienes mas caracteres que el numero de caracteres que le pedimos 
                  if (Numero < nchar(Palabra)) {
                    for(i in 1:length(X)) {
                      vector_final[i] = substr(Palabra,X[i],X[i])
                    } 
                    return(vector_final)  
                  }    
                  
                  # la palabra tienes el mismo numero de caracteres que el numero de caracteres que le pedimos  
                  else if (Numero == nchar(Palabra)) {
                    warning(paste0("Advertencia! El numero ", Numero," es igual al numero de caracteres de la palabra '",Palabra,"'"))
                    for(j in 1:length(X)) {
                      vector_final[j] = substr(Palabra,X[j],X[j])
                    } 
                    return(vector_final)
                  }
                  
                  # la palabra tienes menos caracteres que el numero de caracteres que le pedimos          
                  else {
                    stop(paste0("Error! El numero ", Numero," es mayor al numero de caracteres de la palabra '",Palabra,"'")) 
                  }
}

sapply(c("que bonito perro", "veamos la grabacion"), function(x) caracter_vector(x, Numero = 8 ))


#--------------------------------#
#--------------------------------#
#--------------------------------#

# Funcion para extraer los caracteres no numericos de una variable
extrac_nonumeric_vector = function(variable){
  
                          X = unique(variable) %>% as.data.frame(.) 
                          
                          # función que encuentra y remplaza los números
                          for(i in 0:9){
                            X[,1] = gsub(as.character(i),"",X[,1])  
                          }
                          
                          Y = X[,1] %>% .[!. %in% c("",NA)] 
                          return(Y)
}

lapply(c("roj0o", "h1ol2a", "p7el3icula"), function(x) extrac_nonumeric_vector(x))


