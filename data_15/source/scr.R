#==============================================================================#
# Author: Eduard Martinez
# Update: 12/05/2021
# R version 4.1.1 (2021-08-10)
#==============================================================================#

# configuración inicial
rm(list = ls()) # limpia el entorno de R

# instalar y/o llamar librerías
require(pacman)
p_load(tidyverse,rio,data.table,XML,rvest,xml2,
       textcat,stringi,tm,cluster,wordcloud) # cargar y/o instalar paquetes a usar

#===== Hoy veremos =====#

# 1. Obtener abcstrac Revista Desarrollo y Sociedad

# 2. Analisis de textos

#========================================# 
# Abcstrac Revista Desarrollo y Sociedad #
#========================================# 

# URL de las paginas con documentos CEDE en repec
url = "https://ideas.repec.org/s/col/000090.html" %>% 
       read_html() %>% 
       html_nodes(xpath = '//*[@id="content-block"]/div[2]') %>% 
       html_nodes("a") %>% 
       html_attr('href') %>% unique() %>% .[!is.na(.)]

url = paste0("https://ideas.repec.org/s/col/",url)

# extraer abstrac de los papers
get_abstrac = function(path){

              # get the abstrac URL
              url_abstrac = read_html(path) %>% 
                            html_nodes(xpath = '//*[@id="content"]') %>% 
                            html_nodes("a") %>%  
                            html_attr('href') %>% 
                            .[grepl("/a/col/",.)==T] %>% 
                            paste0("https://ideas.repec.org",.)

              # extrac abstrac
              extrac = function(link){
                       print(link)
                       df = tibble(url=rep(NA,1) , titulo=rep(NA,1) , autores=rep(NA,1) , abstrac=rep(NA,1))
                       html_doc = read_html(link)
                      
                       df$url = link 
                       
                       df$titulo = html_doc %>% 
                                   html_nodes(xpath = '//*[@id="title"]/h1') %>% 
                                   html_text()
                     
                       df$autores = html_doc %>%
                                    html_nodes("#authorlist") %>% 
                                    html_text() %>% 
                                    str_replace_all(string=. , pattern="\n" , replacement=" ; ")
                     
                       df$abstrac = html_doc %>% 
                                    html_nodes("#abstract-body") %>% 
                                    html_text()
              return(df)}
          
              # get tibble
              db = lapply(url_abstrac,extrac) %>% rbindlist(use.names=T , fill=T)
return(db)} 

# obtener los abstrac de todos los papers
df = lapply(url,get_abstrac) %>% 
     rbindlist(.,use.names = T,fill = T) %>% as_tibble()
export(df , "data_15/output/abstrac_desarrollo_sociedad.rds")

#====================# 
# Analisis de textos #
#====================# 

# librería para trabajar con textos
browseURL("https://stringi.gagolewski.com") # stringi
browseURL("https://github.com/rstudio/cheatsheets/blob/main/strings.pdf") # string 

# importar bases de datos
df = import("data_15/output/abstrac_desarrollo_sociedad.rds") %>% 
     subset(textcat(titulo)=="english")

# Convertir los abstrac en un vector de los caracteres
abstrac = df$abstrac
abstrac %>% head()

#========== Limpiar el texto ==========# 

# detectar codificacion
stri_enc_mark(abstrac)
stri_enc_isascii(df$abstrac)
stri_enc_isutf8(df$abstrac)

# recodificar
stri_enc_mark("Ábcdêãçoàúü") 
iconv("Ábcdêãçoàúü", from = 'UTF-8', to = 'ASCII//TRANSLIT')

stri_encode(abstrac,"UTF-8", "ASCII") %>% head()

stri_trans_general(abstrac, "Latin-ASCII") %>% head() 

abstrac[5]
Encoding(abstrac) = "ASCII"
abstrac[5]

# Vamos a limpiar nuestros caracteres
"todos los caracteres a minusculas"
abstrac[5]
abstrac = tolower(abstrac)
abstrac[5]

## Eliminar carcateres especiales
str_remove_all(string="Washington (D.C.)" , pattern="[^[:alnum:] ]")
str_replace_all(string="Washington (D.C.)" , pattern="[^[:alnum:] ]" , replacement="")
abstrac = str_remove_all(string=abstrac , pattern="[^[:alnum:] ]")

## Eliminar  acentos
stri_trans_general("Ahí está la Economía", "Latin-ASCII")
stri_trans_general("Colombia in the late 1990âs", "Latin-ASCII")
abstrac = stri_trans_general(abstrac, "Latin-ASCII")

## Remover puntuacion
removePunctuation("- Hola como estas?")
abstrac = removePunctuation(abstrac)

## Remover numeros
removeNumbers("Desde de 1950 existe")
abstrac = removeNumbers(abstrac)

## Remover preposiciones y/o conectores
stopwords("spanish") 
stopwords("en")
removeWords("En la luna existen algunas evidencias",stopwords("spanish"))
abstrac = removeWords(abstrac,stopwords("en"))

## Remover otras cadena de caracteres
abstrac = removeWords(abstrac,c("et","abstrac","documento", "paper"))

## Remover exceso de espacios
stripWhitespace("Hola    como   estas ")
abstrac = stripWhitespace(abstrac) 

## Remover espacios iniaciales y finales
trimws(" Hola como estas ")
abstrac = trimws(abstrac)

## Verificar cuantos caracteres se eliminaron
abstrac[10] %>% nchar()
df$abstrac[10] %>% trimws() %>% nchar()

abstrac[10]
df$abstrac[10] %>% trimws()

#========== De texto a corpus ==========# 

# vector de caracteres a corpues
abstrac_corpus = Corpus(VectorSource(abstrac)) # formato de texto
class(abstrac_corpus)

# matriz con terminos
tdm_abstrac = TermDocumentMatrix(abstrac_corpus)
class(tdm_abstrac)  

# en columnas están los papers y en las filas el numero de palabras
dim(tdm_abstrac)

# frecuencia de palabras (se repiten almenos 20 veces)
findFreqTerms(tdm_abstrac, lowfreq = 20)
frecuentes = findFreqTerms(tdm_abstrac, lowfreq = 20)

# palabras con las que mas se asocian las primeras 5 palabras del vector frecuentes
findAssocs(tdm_abstrac, frecuentes[1:5], rep(x = 0.45, rep = 50))

# Convirtamos el objeto tdm_abstrac en una matriz con frecuencias
matrix_abstrac = as.matrix(tdm_abstrac) #lo vuelve una matriz
dim(matrix_abstrac)
view(matrix_abstrac)

# Sumemos la frecuencia de cada palabra
frec_words = sort(rowSums(matrix_abstrac),decreasing=TRUE) 
class(frec_words)
df_words = data.frame(word = names(frec_words) , n = frec_words)

# Histograma con frecuencias
barplot(df_words[1:10,]$n, las = 2, names.arg = df_words[1:10,]$word,
        col ="orange", main = "Palabras frecuentes" , ylab = "Frecuencia de palabras")
  
# Grafiquemos la nube de palabras
wordcloud(words = df_words$word, freq = df_words$n, min.freq = 6,
          max.words = 250 , random.order = T , rot.per = 0.35 , scale = c(10,1))
  

wordcloud(words = df_words$word, freq = df_words$n, min.freq = 1,
          max.words = 300 , random.order = T ,colors = brewer.pal(10, "Dark2"))

