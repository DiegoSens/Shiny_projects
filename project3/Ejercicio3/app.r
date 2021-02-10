#Ejercicios previos al examen - Técnicas de Visualización 
#Ejercicio 3
#Diego Senso González
#4 febrero 2021

#Objetivo: crear un gráfico de dispersión con tantos datos aleatorios como cantidad de muestra se seleccione.
#En cualquier momento, el usuario podrá añadir un punto aleatorio nuevo a la gráfica existente pulsando un botón.
#También se reiniciará la muestra si el usuario desea generar una nueva, pulsado otro botón.

#Se cargan las librerías necesarias.
library(shiny)
library(ggplot2)

##### PARTE DE UI #####
ui <- fluidPage(
  
  #Título de la app.
  titlePanel("Ejercicio 3 - Examen Final Shiny"),
  sidebarLayout(
    
    #Se añade un input y dos botones en el menú lateral de la izquierda.
    sidebarPanel(
      
      #Se incluye un input numérico para que el usuario introduzca la cantidad de puntos a representar. 10 es el valor por defecto.
      numericInput("num", label = "Tamaño de la muestra:",
                   min = 1, max = 100000, value = 10),
      
      #Se añade un botón "muestra" que generará una muestra aleatoria cada vez que se pulse.
      actionButton("muestra", label = "Generar muestra"),
      
      #Se crea otro botón, "nuevo", que añadirá un nuevo punto aleatorio a la gráfica ya existente.
      actionButton("nuevo", label = "Añadir punto nuevo")
    ),
    
    ## MAIN-PANEL ##
    mainPanel(
      #En el panel principal se hace figurar el plot de dispersión que aparecerá en la app.
      plotOutput("dispersion")
    )
  )
)


##### PARTE DE SERVER #####
server <- function(input, output) {
  
  #Se configura un valor inicial de 10, ya que es el valor por defecto del numeric input del UI.
  valor_inicial_muestra <- 10
  
  #Se crea un dataset a partir de "reactiveValues" para que pueda ser actualizado al pulsar el segundo botón.
  #El dataset será la muestra inicial, creado a partir de 10 puntos aleatorios (con x e y) creados con rnorm.
  dataset <- reactiveValues(
    muestra_inicial = data.frame(x = rnorm(valor_inicial_muestra), 
                                 y = rnorm(valor_inicial_muestra))
    )
  
  #Se incluye un observeEvent que reaccionará ante un evento. Este evento será que se presione el botón que genera la muestra.
  #Lo que hará es generar un dataset de puntos aleatorios (con rnorm) donde el número de puntos será igual a la cantidad fijada en el numeric input inicial.
  #Servirá para generar una nueva muestra y eliminar la existente.
  observeEvent(input$muestra, {
    dataset$muestra_inicial <- data.frame(x = rnorm(input$num), 
                                          y = rnorm(input$num))
  })
  
  #Se incluye la salida del gráfico de dispersión. El ggplot se originará de cero cuando se pulse el botón que genera la muestra.
  #El gráfico será sobre el dataset generado por el reactive_Values, con sus ejes x e y.
  #También se actualizará cuando se añada un punto. Se han añadido opciones de estilo.
  output$dispersion <- renderPlot({
    input$muestra
    ggplot(dataset$muestra_inicial, aes(x = dataset$muestra_inicial$x, 
                                        y = dataset$muestra_inicial$y, 
                                        size = 5)) + 
                                        geom_point(show.legend = FALSE, 
                                                   alpha = 0.5, 
                                                   color  = 'chocolate',
                                                   shape = 19) +
                                        labs(title = "Gráfico de dispersión: Puntos aleatorios",
                                             subtitle = "Puede seleccionar un número de muestra y luego añadir puntos sobre la gráfica creada.",
                                             x = "Eje X", 
                                             y = "Eje Y")
  })
  
  #Se introduce un segundo observeEvent que servirá para añadir un punto nuevo aleatorio.
  #Al pulsar, se generará un nuevo punto aleatorio con rnorm y se añadirá con "rbind" al "dataset_muestra_inicial",
  #que contenía los datos de la muestra que existía en ese momento y que estaba representada en el gráfico.
  observeEvent(input$nuevo, {
    dataset$muestra_inicial <- rbind(dataset$muestra_inicial, 
                                     rnorm(input$nuevo))
  })
}

#Run the app.
shinyApp(ui = ui, server = server)

