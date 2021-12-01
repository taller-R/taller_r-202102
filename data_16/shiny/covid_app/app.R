
### Cargar librerias
library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(ggsn)
library(ggnewscale)
library(viridis)

### Cargar bases de datos
shape = readRDS(file = 'data/shape.rds')
data = readRDS('data/Casos por upz.rds') %>% 
       dplyr::select(cod_upz,casos_03,casos_04,casos_05,casos_06,casos_07,casos_08,casos_09)

shape = merge(shape,data,'cod_upz',all.x=T)


#---------------------------------#
# Definir la interfaz del usuario #
#---------------------------------#
ui <- fluidPage(theme = shinytheme("united"),
      titlePanel("Seleccione un mes:"),
      sidebarLayout(
                    sidebarPanel("",
                                 selectInput(inputId = "mes", label = "", 
                                 choices = c('Marzo'='casos_03','Abril'='casos_04','Mayo'='casos_05',
                                             'Junio'='casos_06','Julio'='casos_07','Agosto'='casos_08','Septiembre'='casos_09'),
                                 selected = "")),
                    plotOutput(outputId = "mapa",width = 1400, height = 650 )
                   )
      
)
                        

#------------------#
# Definir servidor #
#------------------#
server <- function(input, output) {
          
          # Plot mapa
          output$mapa <- renderPlot({
                         shape[,'value'] = shape[,input$mes]
                         ggplot() + geom_sf(data = shape , color = 'gray', aes(fill=value), size = 0.5 ,) + 
                         scale_fill_viridis(name="Casos",na.value = "white",direction = 1,option = 'inferno') +
                         ggsn::north(data = shape , location = "topleft") + 
                         ggsn::scalebar(data = shape, dist = 5, dist_unit = "km",transform = T, model = "WGS84",location = "bottomleft") +
                         ggtitle('Casos de Covid-19 por UPZ') + xlab('') + ylab('') + 
                         theme_bw()
          })
}


#--------------------#
# Correr application #
#--------------------#
shinyApp(ui = ui, server = server)
