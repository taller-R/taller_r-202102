# Elaborado por: Eduard Martinez
# Colaboradores: 
# Fecha de elaboracion: 19/09/2021
# Ultima modificacion: 19/09/2021

# configuracion inicial 
rm(list = ls()) # limpia el entorno de R
if(!require(pacman)) install.packages("pacman") ; require(pacman) # Instalar la librería pacman
p_load(tidyverse , rio , skimr , RColorBrewer , ggthemes , hrbrthemes , igraph) # Llamar y/o instalar las librerías de la clase

#------------------------------------------------------------------------------#
#------------- 1.0 Gratificos con el paquete base & dividir pantalla ----------#
#------------------------------------------------------------------------------#

#----------------------------#
#---- informacion extra -----#
#----------------------------#
browseURL("https://r-coder.com/plot-r/", getOption("browser"))

# crear data.frame 
df = storms
skim(df)

#----------------------------#
#-- 1.1 gráficos con r base -#
#----------------------------#

#---- r en sus funciones bases tiene un paquete que permite graficar 

# Plot graph
plot(df$wind,df$ts_diameter)
plot(df$month,df$ts_diameter)

# density plot
density(df$month)
plot(density(df$month))

#histograma
hist(df$year)

#----------------------------#
#--- 1.2 dividir pantalla ---#
#----------------------------#

#---- Layout permite dividir el área de graficar en sectores

# divida en 3 partes
layout(matrix(c(1:3,3), nrow=2, byrow= FALSE))
layout.show(3)

# divida en 4  partes
layout(matrix(c(1:4), nrow=2, byrow= FALSE))
layout.show(4)

#---- Graficas
plot(df$wind,df$ts_diameter)

plot(df$month,df$ts_diameter)

plot(density(df$month))

hist(df$year)

#------------------------------------------------------------------------------#
#------------------------------- 2. ggplot ------------------------------------#
#------------------------------------------------------------------------------#


#----------------------------#
#---- informacion extra -----#
#----------------------------#
browseURL("https://www.data-to-viz.com",getOption("browser")) # ayuda para escoger una grafica
browseURL("https://www.r-graph-gallery.com/",getOption("browser")) # tipos de graficas

browseURL("https://ggplot2.tidyverse.org/reference/scale_brewer.html",getOption("browser")) # scale brewer function
browseURL("https://colorbrewer2.org/#type=qualitative&scheme=Set3&n=9",getOption("browser")) # manual color picker
browseURL("https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html",getOption("browser")) # palletas disponibles
browseURL(url="http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html" , browser = getOption("browser")) # + palletas disponibles
display.brewer.all() # + + palletas disponibles

browseURL("https://mran.microsoft.com/snapshot/2017-02-04/web/packages/ggthemes/vignettes/ggthemes.html",getOption("browser")) # ggtheme package
browseURL("https://ggplot2.tidyverse.org/reference/theme.html",getOption("browser")) # ggtheme package

iris %>% head() # utilizaremos la base de iris, incluida en el paqete base

#----------------------------#
#---- 2.1 ggplot: basics ----#
#----------------------------#


ggplot(iris) # ggplot funciona por medio de capas cada "+" es una capa diferente

#------ pintamos sobre la primera capa
ggplot() + geom_point(data = iris, show.legend = T , col = "red" , 
                      aes(x=Sepal.Length, y=Sepal.Width)) 

#------ pintamos sobre la segunda capa
ggplot(data = iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width)) 

#------ para mantener cambiar los ejes
ggplot(iris) + geom_point(aes(y=Sepal.Length, x=Sepal.Width)) 

ggplot(data = iris) + 
  geom_point(aes(x=Sepal.Length, y=Sepal.Width, shape = Species)) + 
  coord_flip() + coord_equal()

#------ podemos incluir colores
ggplot(data = iris) + 
geom_point(aes(x=Sepal.Length, y=Sepal.Width, colour = Species , shape = Species)) + 
coord_equal()

#----------------------------#
#----- 2.1 ggplot:advance ---#
#----------------------------#

# para simplificar pondremos este código lo pondremos dentro de una variable de tal manera que cuando llamemos a la variable nos de la gráfica
graph_1 = ggplot(iris) + 
          geom_point(aes(x=Sepal.Length, y=Sepal.Width, colour = Species)) + 
          coord_equal()
graph_1

#------ scale colour

# podemos definir los colores que queremos utilizar con las funciones scale_colour_brewer & scale_colour_manual

# scale_fill_manual
graph_1 + scale_colour_manual(values  = c("blue","grey","black"))


# scale_fill_brewer
graph_2 = graph_1 + scale_colour_brewer(palette = "Blues") # reescribimos variable para simplificar

graph_2

#------ agregar temas 

# podemos agregar diferentes temas

# Tema solarized
graph_2 + theme_solarized(light = FALSE) 
graph_2 + theme_few()
graph_2 + theme_pander() + scale_colour_pander()

# Tema stata
graph_3 = graph_2 + theme_stata() + scale_colour_stata() # el anterior scale colour fue reemplazado por este color de scale_colour_stata

graph_3
#------ Labels
 graph_4 = graph_3 + labs(title = "Grafica Iris", 
                     subtitle = "Sepal",
                     caption = "Fuente: Iris",
                     y = "Ancho del sepalo", 
                     x = "Largo del sepalo", 
                     colour = "Especie")
 graph_4
 
 
#--------------------------------#
#-- ejemplos de otras gráficas --#
#--------------------------------#

# utilizaremos las siguientes dos base de datos

base_1 = import("data_7/input/rango_sexo.rds")
base_2 = import("data_7/input/desempleados_1_6.rds") 

#--------- Grafica de barras
# ggplot funciona con columnas, por ende tenemos que cambiarlo a formato long
base_1 = pivot_longer(base_1, cols = c(hombre,mujer), names_to = "sexo", values_to = "total" ) 


# grafica
graph_5 = ggplot(data = base_1 , aes(x = rango , y = total , fill = sexo)) + # en ves de poner colour ponemos fill
          geom_bar(stat = "identity", position = "dodge") +  
          geom_text(aes(label = total),   # fill para poner la cantidad encima
                    position=position_dodge(width=0.9), #  ajusta la posicion 
                    vjust=-0.25,
                    size = 3.5) +
          scale_fill_brewer(palette = "Paired")+ 
          labs(title = "Genero dividio por grupos",
               caption = "Informacion obtenida de la GEIH",
               x = "Rango de edad", 
               y = "Frequencia", 
               fill = "Genero") + theme_bw()
graph_5

#--------- Grafica de lineas  
# el factor de expansion estan en un formato diferente a numérico, arreglaremos eso.
is.numeric(base_2$fex_c_2011)

base_2 = base_2 %>% mutate(fex_c_2011 = gsub(",","\\.",base_2$fex_c_2011 ) %>% as.numeric())

is.numeric(base_2$fex_c_2011)

# realizaremos summarise para obtener el total de desempleado por mes

base_3 = base_2 %>%
          group_by(mes, p6020) %>% 
          summarise(total = sum(fex_c_2011)) %>%  
          mutate(p6020 = ifelse(test = p6020 == 1, yes = "Hombre", no = "Mujer"))

# graficamos 
graph_6 = base_3 %>%
            ggplot( aes(x=as.character(mes), y=total, group=p6020, color=p6020)) +
            geom_line(size=1) +
            labs(title = "Desempleados",
                 caption = "Informacion obtenida de la GEIH/2019",
                 x = "Mes", 
                 y = "Total",
                 colour = "") +
            theme_light() + theme(plot.title = element_text(hjust = 0.5), legend.position="bottom") +
            scale_color_canva()
graph_6

#--------- Grafica circular
# arreglamos nuestra base de datos
base_4 = base_2 %>%
  group_by( p6020) %>% 
  summarise(total = sum(fex_c_2011)) %>% 
  mutate(p6020 = ifelse(test = p6020 == 1, yes = "Hombre", no = "Mujer"), 
         porcentaje = total / sum(total)) 

# calculamos la posicion de nuestro text
base_4 = base_4 %>% 
  arrange(desc(p6020)) %>%
  mutate(prop = total / sum(base_3$total) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )  

# graficamos
graph_7 = ggplot(base_4, aes(x="", y= prop, fill= p6020)) +
          geom_bar(stat="identity", width=1) +
          coord_polar("y", start=0)+
          theme_void() + 
          geom_text(aes(y = ypos, 
                        label = paste0(round(porcentaje*100,0),"%")), 
                    color = "black", size=6)  +
          scale_fill_brewer(palette="Set1") +
          geom_line(size=1, linetype= 3) +
          labs(title = "Desempleados por genero",
               caption = "Informacion obtenida de la GEIH/2019",
               fill = "") +
          theme(plot.title = element_text(hjust = 0.5), 
                legend.position="bottom")
graph_7

#--------------------------------#
#---- guardamos las graficas ----#
#--------------------------------#

### Como jpeg
ggsave(plot=graph_1, file = "data_7/output/ejemplo_1.jpeg")
ggsave(plot=graph_2, file = "data_7/output/ejemplo_2.png")
ggsave(plot=graph_3, file = "data_7/output/ejemplo_3.pdf")
ggsave(plot=graph_4, file = "data_7/output/ejemplo_4.tiff")
ggsave(plot=graph_5, file = "data_7/output/ejemplo_5.jpeg")
ggsave(plot=graph_6, file = "data_7/output/ejemplo_6.jpeg")
ggsave(plot=graph_7, file = "data_7/output/ejemplo_7.jpeg")



#-------------------------- extra task ----------------------------------------#
print("Encuente la manera de dividir he hacer una grafica compartida") 


