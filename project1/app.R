
#Título: "Ejercicio 1 - Shiny"
#Autor: "Diego Senso González"
#Fecha: "14 Noviembre 2020"

#Se ha procedido a crear la aplicación siguiendo las instrucciones dadas.
#La creación de esta app se realizará en un solo fichero, que es el presente.

#Carga de librerías
library(shiny)
library(ggplot2)
library(tidyr)

#Estas líneas no se van a ejecutar, pero las dejo debido a que durante algún punto de 
#la realización de la práctica, R no encontraba el archivo "mpg". Por si volviera a ocurrir se adjunta el dataset mpg dentro del proyecto.
#En ellas, exportaba del exterior el archivo y eliminaba las columnas que no eran numéricas y no iba a utilizar.
#mpg <- read.csv("mpg.csv", sep = ";")
#mpg <- mpg[,-1]
#mpg <- mpg[,c(-1,-2,-6,-7,-10,-11)]


#####PARTE DE UI#####
ui <- fluidPage(

#Título de la app.  
  titlePanel("Ejercicio 1 Shiny - Diego Senso González"),
  
  
  sidebarLayout(
    sidebarPanel(
      
#Se crean dos selectInput, uno para cada variable. Se especifica el texto a mostrar como etiqueta o label.
#Además, se crean las opciones, en este caso serán las cuatro variables numéricas con las que cuenta el dataset.
#A la derecha se introuduce el nombre de la variable, a la izquierda el texto que se quiere mostrar al usuario.
      selectInput("selector1", label = "Seleccionar Variable 1:", 
                  choices = list("Cilindrada (en litros)" = "displ", 
                                 "Número de cilindros" = "cyl",
                                 "Distancia en ciudad (millas por galón)" = "cty",
                                 "Distancia en carretera (millas por galón)" = "hwy")),
      
      selectInput("selector2", label = "Seleccionar Variable 2:", 
                  choices = list("Cilindrada (en litros)" = "displ", 
                                 "Número de cilindros" = "cyl",
                                 "Distancia en ciudad (millas por galón)" = "cty",
                                 "Distancia en carretera (millas por galón)" = "hwy"), 
                  selected = "cyl"), #Se especifica la opción "selected" para que por defecto salga esta variable al iniciar la aplicación.

#Se crea un input numérico, que recogerá la cantidad de observaciones que queremos extraer como muestra.
#Especificamos el valor por defecto, el mínimo a elegir y el máximo, que coincidirá con el número de observaciones del dataset.
      numericInput("num", label = h3("Tamaño de la submuestra"), 
                   value = 10, 
                   min = 2,
                   max = nrow(mpg)),
      
#Creación de un botón llamado "Actualizar", que servirá para actualizar gráficos, summaries y correlación.
#Se identificará con la etiqueta "button".
      actionButton("button", label = "Actualizar")
    ),
    
#####MAIN-PANEL#####    
#En el main panel se fijan los outputs necesarios. Dos histogramas, dos summaries, un gráfico de dispersión y la correlación en formato texto.
#Entre comillas se identifican con sus nombres para posteriormente llamarlos en la parte del server.
    mainPanel(
      plotOutput(outputId = "hist1"),
      plotOutput(outputId = "hist2"),
      verbatimTextOutput("summary1"),
      verbatimTextOutput("summary2"),
      plotOutput("dispersion"),
      textOutput("correlacion")
    )
  )
)

#####PARTE DE SERVER#####
server <- function(input, output) {

#Un reactivo bajo la denominación "muestra". Este se actualizará al pulsar el botón, y lo que hará es generar una
#muestra de mpg, donde seleccionará tantas filas u observaciones como le digamos en input$num, que es el campo numérico que se ha definido en UI.
#Utilizamos isolate para que esa muestra se renueve solo cuando accionemos el botón.
    muestra <- reactive({
    input$button
    isolate(mpg[sample(nrow(mpg), input$num),])
  })
  
#Se crea el primer histograma. Será un renderPlot por estar creando un gráfico.
#Se define "var1" como la variable seleccionada en el primer selector
#Crea el histograma del tamaño de muestra seleccionada, pero cogiendo esa var1 concreta, utilizando el doble corchete para selccionarla.
#Se añaden otros parámetros al histograma como el color, título o nombre de los ejes.
  output$hist1 <- renderPlot({
    var1 <- isolate(input$selector1)
    hist(muestra()[[var1]], main = ("Histograma de la Variable 1"), xlab = "Valores de la variable 1", ylab = "Frecuencia", col = "green")
    
  })

#Se crea el primer summary con un renderPrint. Se define "var1" como la variable elegida en el primer selector.
#Luego se aplica la función summary sobre la cantidad de muestra seleccionada y se accede a la var1 usando el doble corchete.
  output$summary1 <- renderPrint({
    var1 <- isolate(input$selector1)
    summary(muestra()[[var1]])
    
  })

#Mismo procedimiento que al crear el primer histograma. Aquí se define "var2" como la variable escogida en el segundo selector.
#Se crea el histograma con la función "hist" sobre la cantidad de muestra de esa var2. Se añaden parámetros al gráfico.  
  output$hist2 <- renderPlot({
    var2 <- isolate(input$selector2)
    hist(muestra()[[var2]], main = ("Histograma de la Variable 2"), 
                            xlab = "Valores de la variable 2", 
                            ylab = "Frecuencia", 
                            col = "purple")
    
  })

#Creación del segundo summary definiendo "var2" como la variable que sale del segundo selector.
#Luego se aplica la función summary de la misma forma que en el summary1.
  output$summary2 <- renderPrint({
    var2 <- isolate(input$selector2)
    summary(muestra()[[var2]])
   
  })

#Se crea el gráfico de dispersión mediante un renderPlot. Se definen tanto var1 (variable que sale del selector 1) como var2 (variable que sale del selector2).
  output$dispersion <- renderPlot({
    var1 <- isolate(input$selector1)
    var2 <- isolate(input$selector2)
#Se crea un ggplot de la muestra de los aesthetics var1 y var2.
#Se hace que el gráfico sea de puntos, añadiendo color y tamaño del punto. Tema oscuro.
    ggplot(muestra(), aes_string(var1, var2)) +
      geom_point(colour = 'tomato1', 
                 size = 10) +
      ggtitle("Diagrama de dispersión entre las variables seleccionadas") +
      xlab("Variable 1") +
      ylab("Variable 2") +
      theme_dark()
  })
  
#Por último, se calcula la correlación, que será un output en formato texto.
#Se vuelven a definir var1 y var2 que salen de los selectores.
#Se crea un texto que precede al resultado de lo que sale de la función de correlación.
#"Cor" selecciona las obs dadas de la variable1, y mide la correlación con lo mismo pero de la variable2. 
  output$correlacion <- renderText({
    var1 <- isolate(input$selector1)
    var2 <- isolate(input$selector2)
    print(paste0('Correlación entre las dos variables seleccionadas: ',
                 round(cor(muestra()[[var1]], muestra()[[var2]]),4))) #Redondeamos los decimales del resultado
  })

}
# Run the app.
shinyApp(ui = ui, server = server)

#FIN

