#Ejercicio 2 Shiny - Técnicas de visualización
#Alumno: Diego Senso González
#Fecha: 2/12/2020

#"Crea un shiny que muestre una grafica de dispersión en la que al pasar el ratón por encima aparezca
#una tabla con los DOS puntos más cercanos que estén como mucho a una distancia de 5. Cuando el
#usuario no esté pasando el ratón sobre la gráfica la tabla estará vacía.

#El dataset que se muestra tiene que ser generado aleatoriamente con rnorm. El tamaño del dataset
#debe ser de al menos de 30 puntos. Existirán dos columnas: x e y que respectivamente se usarán
#para estas coordenadas".

#Se cargan las dos librerías necesarias
library(shiny)
library(ggplot2)

#UI 
ui <- basicPage(
    
#Se crea un gráfico en la pantalla con la etiqueta "dispersion", que reaccionará solamente al hover (pasar por encima el cursor del ratón).
#Se crea una tabla con la etiqueta "nearpoints" donde se comprenderán los puntos cercanos bajo unas condiciones (se programa en el UI).
    plotOutput("dispersion", 
               hover = "hover"),
    dataTableOutput("nearpoints")
)


#SERVER
server <- function(input, output) {

#Se genera un dataset que comprenderá los valores de x e y especificados aquí. La generación será de forma aleatoria con rnorm.    
    dataset <- reactiveVal({ 
        data.frame(x = rnorm(200), y = rnorm(200))
    })

#Se crea la salida de un gráfico con la etiqueta "dispersion". Tipo ggplot y de puntos, que se generará a partir del dataset generado en el UI.
    output$dispersion <- renderPlot({
        ggplot(dataset(), aes(x = x, y = y)) + geom_point()
    })

#Se genera la salida de una tabla bajo la etiqueta "nearpoints".
#Mostrará los puntos del dataset que estén cercanos al input creado a partir del hover (poner el cursor encima de la pantalla).
#El máximo de puntos (maxpoints) que mostrará es 2, la distancia máxima (threshold) es 5, y se añade la opción de que muestra la distancia para comprobar que se cumple.
    output$nearpoints <-  renderDataTable({
      nearPoints(dataset(), input$hover, maxpoints = 2, threshold = 5, addDist = TRUE)
    })
}

shinyApp(ui, server)