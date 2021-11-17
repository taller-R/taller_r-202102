#===========================================================================#
# Autor(es): Eduard Martinez
# Colaboradores: 
# Fecha creacion: 12/05/2021
# Fecha modificacion: 12/05/2021
# Version de R: 4.0.3.
#==============================================================================#

# intial configuration
rm(list = ls()) # limpia el entorno de R
require(pacman)
p_load(tidyverse,data.table,plyr,XML,rvest,xml2) # cargar y/o instalar paquetes a usar

#---------------------------------------------#
# 1. Atributos del elemento (libreria rvest)  # 
#---------------------------------------------#

### 1.1. Ver pagina
browseURL(url = 'https://es.wikipedia.org/robots.txt',browser = getOption('browser'))
browseURL(url = 'https://es.wikipedia.org/wiki/Organización_para_la_Cooperación_y_el_Desarrollo_Económicos',browser = getOption('browser'))

### 1.2. Leer HTML
"read_html lee el HTML de la pagina y lo convierte en un objeto del tipo 'xml_document' y 'xml_node'"
myurl = "https://es.wikipedia.org/wiki/Organización_para_la_Cooperación_y_el_Desarrollo_Económicos"
myhtml = read_html(myurl)
class(myhtml)

### 1.3. Usando el xpath

### 1.3.1. Extraer el primer parrafo de la pagina
myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/p[1]')

myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/p[1]') %>% class()

texto = myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div/p[1]') %>% html_text() # Convertir en texto
texto

### 1.3.2. Usando los atributos del elemento
myhtml %>% html_nodes(css = ".toctext") %>% html_text() # Extraemos los subtitulos de la pagina

myhtml %>% html_nodes(".toctext") %>% html_text() # Si no le indicamos que es un css, R reconoce que es un css

myhtml %>% html_nodes(xpath = ".toctext") %>% html_text() # Pero si usamos el xpath comete un error

### 1.3.3. html_node() vs html_nodes()
myhtml %>% html_nodes("a") # html_nodes() retorna el tipo de objeto y los 874 link que hay en la pagina
myhtml %>% html_nodes("a") %>% length()

myhtml %>% html_node("a") # html_node() retorna el tipo de objeto y el primer link de la pagina
myhtml %>% html_node("a") %>% length()

### 1.4. Extraer los link de las referencias
link = myhtml %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/div[10]') # xtraer el xml_nodeset de la seccion de referencias
link

link = html_nodes(link,"a") # Extraer elementos que contienen un link (los que tienen la etiqueta a)

link = html_attr(link,'href') %>% as.data.frame() %>% setNames("link") # Extraer solo el link (atributo ref del elemento)

link = link %>% dplyr::filter(substr(.$link,1,4)=="http") # Filtrar solo los enlaces
View(link) 

#------------------------------------#
#  2. Aplicacion (extraer abstract)  # 
#------------------------------------#

### 2. Link a Cuadernos de Economia
banrep = "https://ideas.repec.org/s/bdr/cheedt.html"

### 2.1. Obteniendo el html_document
html_banrep = read_html(banrep)

### 2.2. Extrayendo el html_node de que contiene los link a los documentos
link = html_banrep %>% html_nodes(xpath = '//*[@id="content"]') %>% html_nodes("a") 

"Veamos los atributos de los elementos"
link %>% html_attrs()

"extraemos solo el atributo href de cada elemento"
link = link %>% html_attr('href') %>% as.data.frame() %>% setNames("link")

"agregamos la ruta hasta la pagina"
link$link = paste0("https://ideas.repec.org",link$link)

### 2.3. Extraer informacion de un documento
"definiendo url"
url_i = link[52,1]

"leyendo html"
html_i = read_html(url_i)

"Extrayendo el titulo del documento"
html_i %>% html_nodes(xpath = '//*[@id="title"]/h1') %>% html_text()

"Extrayendo autores del documento"
html_i %>% html_nodes("#authorlist") %>% html_text() %>% gsub("\n"," ; ",.)

"Extrayendo autores del documento"
html_i %>% html_nodes("#abstract-body") %>% html_text()

### 2.4. Programemos esto dentro de un loop
"Creemos un dataframe para almacenar la informacion"
df_documentos = data.frame(titulo = rep(NA,nrow(link)),
                            autores = rep(NA,nrow(link)),
                            abstrac = rep(NA,nrow(link)))

"hagamos el loop"
for (i in 42:52){
    
    "definiendo url"
    url_i = link[i,1]
        
    "leyendo html"
    html_i = read_html(url_i)
    
    "Extrayendo el titulo del documento"
    df_documentos[i,1] = html_i %>% html_nodes(xpath = '//*[@id="title"]/h1') %>% html_text()
        
    "Extrayendo autores del documento"
    df_documentos[i,2] = html_i %>% html_nodes("#authorlist") %>% html_text() %>% gsub("\n"," ; ",.)
    
    "Extrayendo autores del documento"
    df_documentos[i,3] = html_i %>% html_nodes("#abstract-body") %>% html_text()
}

"veamos el resultado"
View(df_documentos)
df_documentos[47:52,3]

"verifiquemos"
link[50,1]

#----------------------------------#
#  3. Aplicacion (extraer tablas)  # 
#----------------------------------#
rm(list=ls())

### 3.1. Extraer tablas de un HTML usando el paquete rvest
"primero vamos a leer el HTML de la pagina y convertirlo en un objeto del tipo 'xml_document' y 'xml_node'"
myurl = "https://es.wikipedia.org/wiki/Copa_Mundial_de_Fútbol"
myhtml = read_html(myurl)

"Vamos a extraer el xml_nodeset con las tablas usando la etiqueta 'table' "
tabla = myhtml %>% html_nodes('table') 
tabla

"Veamos algunos de los 12 elementos que obtienen la etiqueta 'table'"
tabla[1] %>% html_table(header = T,fill=T)
tabla[5] %>% html_table(header = T,fill=T) 
tabla[12] %>% html_table(header = T,fill=T)

"creando dataframes con la informacion"
confedereacion = tabla[5] %>% html_table(header = T,fill=T)  %>% as.data.frame()
mundiales = tabla[3] %>% html_table(header = T,fill=T)  %>% as.data.frame()

### 3.2. Extraer tablas de un HTML usando el paquete XML
"Primero vamos a leer el HTML de la pagina y convertirlo en un objeto del tipo 'HTMLInternalDocument' y 'XMLInternalDocument'"
rm(list=ls())
myurl = "https://es.wikipedia.org/wiki/Copa_Mundial_de_Fútbol"
parse = read_html(myurl) %>% htmlParse()

"vamos a extraer todas las tablas"
tablas = parse %>% readHTMLTable(header = T)

"Esta funcion nos devuelve directamente un dataframe"
tablas[[4]] %>% class()
tablas[[4]]
campeones = tablas[[4]]

#------------------------------#
#  4. Aplicacion (web-driver)  # 
#------------------------------#

"Esta es una tecnica mas avanzada, pero con lo visto en clases ustedes podrian seguir el siguiente ejemplo:"

### RSelenium
browseURL(url = "https://callumgwtaylor.github.io/post/using-rselenium-and-docker-to-webscrape-in-r-using-the-who-snake-database/")
    
"Si tienen un MAC, deberan hacer esto tambien"
mac = browseURL(url = "https://www.raynergobran.com/2017/01/rselenium-mac-update/")


