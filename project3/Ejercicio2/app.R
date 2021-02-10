#Ejercicios previos al examen - Técnicas de Visualización 
#Ejercicio 2
#Diego Senso González
#4 febrero 2021

#Objetivo: crear un shiny con el que seleccionar una muestra de vuelos filtrada según un air_time o tiempo
#de vuelo máximo. De esta muestra, se representan dos histogramas de las variables "dep_delay" y "arr_delay" respectivamente.
#Al cambiar alguno de los dos campos numéricos, cambian los histogramas.

#Se cargan las librerías y datos necesarios.
#El dataset "flights" contiene información sobre un total de 336.776 vuelos distintos.
library(shiny)
library(ggplot2)
library(nycflights13)
library(dplyr)
data(flights)

##### PARTE DE UI #####
ui <- fluidPage(
  
  #Título de la app.  
  titlePanel("Ejercicio 2 previo al examen Final - Diego Senso González"),
  
  sidebarLayout(
    #Se añaden diferentes inputs en el menú lateral de la izquierda.
    sidebarPanel(
      
      #Se genera un primer selector con una única opción que seleccionará la variable dep_delay,
      #que será la representada en el primer histograma.
      selectInput("departure", label = "Variable del primer histograma:", 
                  choices = list("Retraso en la salida (departure delay)" = "dep_delay")),
      
      #Se utiliza un segundo selector con una única opción que seleccionará la variable arr_delay,
      #que será la representada en el segundo histograma.
      selectInput("arrival", label = "Variable del segundo histograma:", 
                  choices = list("Retraso en la llegada (arrival delay)" = "arr_delay")),
      
      #Se incluye un selector numérico para que el usuario establezca el tamaño de la submuestra que desea hacer
      #sobre el dataset original.
      numericInput("num", label = h3("Tamaño de la muestra deseada:"), 
                   value = 5, 
                   min = 1,
                   max = nrow(flights)),
      
      #Se realiza un nuevo selector numérico para fijar el máximo de tiempo de vuelo (air_time) que deberán
      #tener los vuelos que entren en la muestra. Los vuelos que estén en la muestra deberán
      #cumplir esta condición.
      numericInput("variable", label = h3("Air_time máximo:"), 
                   value = 100, 
                   min = 1,
                   max = max(flights$air_time)),
    ),
    
    ## MAIN-PANEL ##    
    #En el panel principal se hacen figurar los dos histogramas que se visualizarán en la app (2 plotOutputs).
    # Hist1 será para la variable "dep_delay".
    # Hist2 será para la variable "arr_delay"
    mainPanel(
      plotOutput(outputId = "hist1"),
      plotOutput(outputId = "hist2")
    )
  )
)

##### PARTE DE SERVER #####

server <- function(input, output) {
  #Se genera una muestra a partir de un reactivo.
  #Esta muestra se realizará con un filtro sobre "flights" que filtrará solo aquellas observaciones que 
  #cumplan la condición de que el "air_time" (flights$air_time) sea menor que el valor que el usuario haya seleccionado en el selector
  #numérico de "air_time máximo" (input$variable). Por último, se escogerá la cantidad de muestra que el usuario haya fijado
  #en el primer selector numérico (input$num).
  muestra <- reactive({
    flights[sample(nrow(filter(flights, 
                               flights$air_time < input$variable)), input$num),]
  })
  
  #Se genera el output del primero de los histogramas.
  #Se realiza el histograma sobre la muestra() y de la variable fijada en el primer selector (dep_delay).
  #Se añaden opciones adicionales como las etiquetas de los ejes o el color.
  output$hist1 <- renderPlot({
    hist(muestra()[[input$departure]], main = ("Histograma del Departure Delay"), 
         xlab = "Minutos de retraso en la salida", 
         ylab = "Número de vuelos", 
         col = "deeppink")
  })
  
  #Se crea el segundo histograma.
  #Se sigue un proceso similar. Se realiza el histograma sobre la muestra() y de la variable fijada, 
  #esta vez en el segundo selector (arr_delay).
  #Se añaden opciones adicionales como la etiqueta de los ejes o el color.
  output$hist2 <- renderPlot({
    hist(muestra()[[input$arrival]], main = ("Histograma del Arrival Delay"), 
         xlab = "Minutos de retraso en la llegada", 
         ylab = "Número de vuelos", 
         col = "blueviolet")
  })
}
# Run the app.
shinyApp(ui = ui, server = server)

