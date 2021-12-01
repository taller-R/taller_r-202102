library(shiny)
library(tidyverse)

# User interface
ui = fluidPage(
      h1("Este es un ejemplo para la clase de R de 2021-1"),
      numericInput(inputId = "tamano", "Sample size", value = 250), 
      textOutput("texto"),
      plotOutput(outputId = "figura")
)

# Server
server = function(input, output) {
  
output$texto = renderText(
               paste0("Gráfico de dispersión para ",input$tamano," observaciones") 
)
  
output$figura = renderPlot({
                            datos = data.frame(x = rnorm(n = input$tamano , mean = 0, sd = 10),
                                               y = rnorm(n = input$tamano , mean = 0, sd = 10))
                            p = ggplot() + geom_point(data = datos , aes(x=x,y=y)) + theme_bw()
                            p
                })
}

# Run the application 
shinyApp(ui = ui, server = server)
