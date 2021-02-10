# Título: "Práctica Grupal Shiny - Mapa del COVID-19"
# Autores: "Daniel Corral Ruiz, Antonio Pascual Hernández, Antonio Romero Martínez-Eiroa,  Diego Senso González, Luis Vaciero"
# Fecha: "23/12/2020"
# Asignatura: Técnicas de Visualización. Máster en Data Science para Finanzas

# Carga de librerías
library(readxl)
library(tidyverse)
library(shiny)
library(ggplot2)
library(tidyr)
library(Hmisc)
library(dygraphs)
library(sf)
library(geojsonio)
library(shinyHeatmaply)
library(leaflet)
library(viridis)
library(RColorBrewer)
library(shinydashboard)
library(rgdal)
library(memisc)

# Carga del dataset y tratamiento de variables
covid <- read.csv("paises.csv" , sep = ";")
covid$mortalidad <- covid$mortalidad * 100
covid$incidencia_100.000 <- round(covid$incidencia_100.000, 0)
covid$muertes_100.000 <- round(covid$muertes_100.000, 0)

# Elección de variables para el análisis clúster
vars <- setdiff(names(covid[,c(1,3,4,5,7,8)]), "country")

# Carga del archivo geo.json con el que se construirá el mapa
geo <- geojson_read("countries.geo.json", what = "sp")

# Fijamos valor inicial de cero para las variables
geo@data$casos <- 0
geo@data$muertes <- 0
geo@data$poblacion <- 0
geo@data$mortalidad <- 0
geo@data$incidencia_100.000 <- 0
geo@data$muertes_100.000 <- 0

# Se genera una función auxiliar
'%!in%' <- function(x,y)!('%in%'(x,y))

# Tratamiento para que el geo.json reconozca los países por la columna "code" de nuestro dataset
for (i in 1:nrow(geo@data)) {
    id = geo@data$id[i]
    if (id %!in% covid$code) next()
    geo@data$casos[i] <- covid$casos[covid$code == id]
    geo@data$muertes[i] <- covid$muertes[covid$code == id]
    geo@data$poblacion[i] <- covid$poblacion[covid$code == id]
    geo@data$mortalidad[i] <- covid$mortalidad[covid$code == id]
    geo@data$incidencia_100.000[i] <- covid$incidencia_100.000[covid$code == id]
    geo@data$muertes_100.000[i] <- covid$muertes_100.000[covid$code == id]

}

# Uso de función para dar color al mapa
colores <- colorBin(
    domain = geo@data$casos, 
    bins = 20, 
    palette = heat.colors(20),   
    pretty = T 
)

colors <- colores(geo@data$casos)


#UI
ui <- dashboardPage(skin = "red",
    
    # Título
    dashboardHeader(title = "COVID-19"),
    
    # Barra lateral
    dashboardSidebar(
      
      sidebarMenu(
        # Creación de las seis pestañas diferentes de la app
        menuItem("Inicio", tabName = "inicio", icon = icon("home")),
        menuItem("Mapa del Mundo", tabName = "map", icon = icon("globe-europe")),
        # Introducción de un desplegable con las opciones del mapa
        menuItem("Opciones del Mapa", tabName = "options", icon = icon("filter"),
                 selectInput("variable", "Escoja una variable para visualizar en el mapa:", 
                             c("Casos" = "casos",
                               "Muertes" = "muertes", 
                               "Población" = "poblacion",
                               "Mortalidad" = "mortalidad",
                               "Contagios por 100.000 hab" = "incidencia_100.000",
                               "Muertes por 100.000 hab" = "muertes_100.000")
                 ),
                 selectInput("variable2", "Escoja una variable para ver la correlación con la primera:", 
                             c("Casos" = "casos",
                               "Muertes" = "muertes", 
                               "Población" = "poblacion",
                               "Mortalidad" = "mortalidad",
                               "Contagios por 100.000 hab" = "incidencia_100.000",
                               "Muertes por 100.000 hab" = "muertes_100.000"),
                             selected = "muertes"
                 ),

                 radioButtons("palette", label = h3("Escoja la paleta de colores:"),
                              choices = list("Rojo" = "Red", "Verde" = "Green", "Azul" = "Blue", "Naranja" = "Orange"), 
                              selected = "Red")
        ),
        
        # Pestañas restantes
        menuItem("Análisis Clúster", tabName = "cluster", icon = icon("object-group")),
        menuItem("Higher or Lower", tabName = "juego", icon = icon("gamepad")),
        menuItem("Información en tiempo real", tabName = "video", icon = icon("youtube")))),
      
        
    
    # Main Body
    dashboardBody(
      # Introducción del botón musical
      h4("Si lo desea, pulsando el 'Play' puede añadir música mientras visualiza los datos:"),
      HTML('<iframe src="https://drive.google.com/file/d/1FZHlPYx-4C7K4qOkseYgwxCkWbb8Ix4a/preview" width="500" height="70"></iframe>'),
      
            
        tabItems(
          #  Pestaña que aparece al abrir la app. Muestra un texto de bienvenida y una imagen. 
          tabItem(
            tabName = "inicio",
            h1("Bienvenido a la aplicación"),
            h3("Seleccione una de las pestañas en el panel de la izquierda para comenzar a visualizar los datos. Para un mejor uso, recomendamos seleccionar la opción superior 'Open in Browser'."),
            tags$img(src = "https://cdn.discordapp.com/attachments/788710509742260224/790160532976042004/covid19.jpg",
                     width = "900",
                     height = "350")
            ),
          
          # Pestaña con el mapa. Cuenta con un título, explicación de la variable elegida, el mapa interactivo, un selector de países, cálculos de correlación, dos histogramas y una tabla de datos.
          tabItem(
            tabName = "map",
            h1("COVID-19 en el Mundo"),
            fluidRow(textOutput("diccionario"),
                      h3("Mapa del Mundo"),
                      leafletOutput("map"),
                      h3("Histogramas y correlación para las variables y países escogidos."),
                      selectInput("value", label = h3("Escoja los países para visualizar en los histogramas y la tabla inferior:"), 
                                 choices = covid[,1], 
                                 multiple = TRUE,
                                 selected = "Spain"),
                     
                      textOutput("Correlacion"),
                     
                      box(plotOutput(outputId = "hist1")),
                      box(plotOutput(outputId = "hist2")),
                      h3("Tabla con todos los indicadores de los países escogidos:"),
                      dataTableOutput("tabla"),
                      fluidRow(verbatimTextOutput("map_shape_click")))
          ),
          
          # Pestaña de Análisis clúster. Incluye la elección de dos variables y número de grupos, el gráfico y una tabla de los países de cada grupo.
          tabItem(
            tabName = "cluster",
            h1("Análisis Clúster"),
            h4("Para realizar la agrupación de países por clústers, seleccione las dos variables por las cuales agrupar y el número de grupos que desea. En el gráfico inferior, cada punto representará un país y cada color será un grupo diferente."),
            selectInput('xcol', 'Escoja la primera variable:', vars, selected = "casos"),
            selectInput('ycol', 'Escoja la segunda variable:', vars, selected = "muertes"),
            numericInput('clusters', 'Escoja el número de grupos a realizar:', 3, min = 1, max = 10),
            plotOutput("plotcluster"),
            h3("En la tabla siguiente, puede observar a qué grupo pertenece cada país en función de la configuración escogida:"),
            dataTableOutput("tablacluster")
          ),
          
          # Pestaña con un minijuego donde se eligen dos países y muestra cuál cuenta con un indicador más elevado en cada variable, más una tabla de los países.
          tabItem(
            tabName = "juego",
            h1("Higher or Lower: COVID-19"),
            h3("Seleccione dos países y podrá observar si los indicadores del primero son mayores, menores o iguales a los del segundo."),
            selectInput("juegopais", label = h3("Escoja el primer país:"), 
                        choices = covid[,1], 
                        multiple = FALSE,
                        selected = "Spain"),
            selectInput("juegopais2", label = h3("Escoja el segundo país:"), 
                        choices = covid[,1], 
                        multiple = FALSE,
                        selected = "Portugal"),
            textOutput("juegocasos"),
            textOutput("juegomuertes"),
            textOutput("juegopoblacion"),
            textOutput("juegomortalidad"),
            textOutput("juegoincidencia_100.000"),
            textOutput("juegomuertes_100.000"),
            
            h3("A continuación, se ofrecen los indicadores numéricos de los dos países escogidos:"),
            dataTableOutput("tablajuego1"),
            dataTableOutput("tablajuego2")
            
          ),
          
          
          # Pestaña con un live stream insertado sobre datos del COVID en tiempo real.
          tabItem(
            tabName = "video",
            h1("Información en tiempo real"),
            h3("Si lo desea, en el siguiente vídeo propiedad de Roylab Stats, puede consultar información referente al COVID-19 por país y en tiempo real (se recomienda abrir la aplicación en navegador):"),
            HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/NMre6IAAAiU" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
          )
        )
    )
)

#SERVER
server <- function(input, output){
  
  # Reacción del mapa
  value <- reactiveValues(click_shape = NULL)
  
  observeEvent(input$reset, { 
    value$click_shape <- NULL
  })
  
  observeEvent(input$map_shape_click, {
    value$click_shape <- input$map_shape_click$id
  })
  
  # Texto explicativo de la variable seleccionada
    output$diccionario <- renderText({   
    
      variable1 <- input$variable
    
        if (variable1 == "casos") {print("*CASOS* es el número de infectados confirmados por COVID-19.")} 
        else if (variable1 == "muertes") {print("*MUERTES* es el número de fallecidos confirmados por COVID-19.")} 
        else if (variable1 == "poblacion") {print("*POBLACIÓN* es el número de habitantes de cada país.")} 
        else if (variable1 == "mortalidad") {print("*MORTALIDAD* es el porcentaje de infectados que ha fallecido por COVID-19.")} 
        else if (variable1 == "incidencia_100.000") {print("*CONTAGIOS POR 100.000 HAB* es el número de infectados confirmados por COVID-19 por cada 100.000 personas.")}
        else if (variable1 == "muertes_100.000") {print("*MUERTES POR 100.000 HAB* es el número de fallecidos por COVID-19 por cada 100.000 personas.")}
        else {print("No variable")}
  })
    
    # Función de colores
    colores <- reactive({
        colorBin(
            if(input$palette == "Blue"){palette = brewer.pal(3, "Blues")}
            else if(input$palette == "Red"){palette = brewer.pal(3, "Reds")}
            else if(input$palette == "Orange"){palette = rev(magma(587))}
            else palette = rev(viridis(587)),
            domain = geo@data[,input$variable],
            bins = 10,
            pretty = T
        )
    })
    # Representacion del mapa
    observe({
        colors <- colores()
        map <- leaflet(geo) %>%
            addTiles() %>%
            addPolygons(
                stroke = F,
                smoothFactor = 0.1,
                fillOpacity = .95,
                color = ~colors(geo@data[,input$variable]),
                highlight = highlightOptions(
                  weight = 5,
                  color = "#663",
                  fillOpacity = 0.2,
                  bringToFront = TRUE),
                  label = paste(sep = ": ", geo@data$name, geo@data[,input$variable]),
                  layerId =  paste(sep = ": ", geo@data$name, geo@data[,input$variable]),
                  labelOptions =  labelOptions(
                  style = list("font-weight" = "bold-italic", padding = "5px 8px"),
                  textsize = "15px",
                  direction = "auto")
                
            ) %>%
          # Leyenda del mapa en función de la variable escogida
          addLegend("topright", pal = colors, values = input$variable,
                    title = "Leyenda variable seleccionada" ,
                    opacity = 1
          ) %>%
            setView(lng = 40,
                    lat = 40,
                    zoom = 1.25)
            
    output$map <- renderLeaflet(map)
    
    })
    
    # Histograma de la variable 1
    output$hist1 <- renderPlot({
      var1 <- isolate(input$variable)
      hist(dataset()[[var1]], main = paste("Histograma de la variable:",toupper(input$variable)), 
           xlab = paste("Valores de la variable:",toupper(input$variable)), 
           ylab = "Frecuencia", 
           col = input$palette)
      
    })
    # Histograma de la variable 2
    output$hist2 <- renderPlot({
      var2 <- isolate(input$variable2)
      hist(dataset()[[var2]], main = paste("Histograma de la variable:",toupper(input$variable2)), 
           xlab = paste("Valores de la variable:",toupper(input$variable2)), 
           ylab = "Frecuencia", 
           col = input$palette)
      
    })
    
    # Representación de la correlación
    output$Correlacion <- renderText({
      
      variable1 <- isolate(input$variable)
      variable2 <- isolate(input$variable2)
      
      print(paste0("La correlación entre las variables ", capitalize(variable1), " y ", capitalize(variable2), " para los países seleccionados es: ", 
                   round(cor(dataset()[[variable1]], dataset()[[variable2]]),2)))
    })
    
    # Creación de un reactive para introducir en la tabla los datos de los países seleccionados
    dataset <- reactive({ 
      print(input$value)
      data.frame(covid[covid$country %in% input$value,])
    })
    
    # Mostramos la tabla
    output$tabla <- renderDataTable({dataset()})
    
   
  # Clusters
    
     # Selección de las variables para realizar el análisis cluster
    selectedData <- reactive({
      covid[, c(input$xcol, input$ycol)]
    })
    
    # Agrupación por K-medias
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    clusterdata <- reactive({data.frame(clusters())})
    
    # Representación del análisis cluster en gráfico y con colores
    output$plotcluster <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)})
    
    # Creación de dataframe con cada país y el clúster al que pertenece
    cluster_country <- reactive({
      data.frame(País = covid$country, 
                 Grupo = clusters()$cluster)
    })
    
    # Representación de la tabla con cada país y su número de clúster
    output$tablacluster <- renderDataTable({cluster_country()})
    
  # Higher or lower
    
    # Creación de dataframe con los datos de los dos paises elegidos
    datasetjuego <- reactive({ 
      data.frame(covid[covid$country %in% input$juegopais,])
    })
    
    datasetjuego2 <- reactive({ 
      data.frame(covid[covid$country %in% input$juegopais2,])
    })
    
    # Creación del output de las comparaciones entre los paises
    output$juegocasos <- renderText({
      
      if (datasetjuego()$casos > datasetjuego2()$casos) {print("Casos: HIGHER")} 
      else if (datasetjuego()$casos < datasetjuego2()$casos) {print("Casos: LOWER")}
      else {print("Casos: EQUAL")}
    })
    
    output$juegomuertes <- renderText({
      
      if (datasetjuego()$muertes > datasetjuego2()$muertes) {print("Muertes: HIGHER")} 
      else if (datasetjuego()$muertes < datasetjuego2()$muertes) {print("Muertes: LOWER")}
      else {print("Muertes: EQUAL")}
    })
    
    output$juegopoblacion <- renderText({
      
      if (datasetjuego()$poblacion > datasetjuego2()$poblacion) {print("Población: HIGHER")} 
      else if (datasetjuego()$poblacion < datasetjuego2()$poblacion) {print("Población: LOWER")}
      else {print("Población: EQUAL")}
    })
    
    output$juegomortalidad <- renderText({
    
      if (datasetjuego()$mortalidad > datasetjuego2()$mortalidad) {print("Mortalidad: HIGHER")} 
      else if (datasetjuego()$mortalidad < datasetjuego2()$mortalidad) {print("Mortalidad: LOWER")}
      else {print("Mortalidad: EQUAL")}
    })
    
    output$juegoincidencia_100.000 <- renderText({ 
      
      if (datasetjuego()$incidencia_100.000 > datasetjuego2()$incidencia_100.000) {print("Casos por 100.000 habitantes: HIGHER")} 
      else if (datasetjuego()$incidencia_100.000 < datasetjuego2()$incidencia_100.000) {print("Casos por 100.000 habitantes: LOWER")}
      else {print("Casos por 100.000 habitantes: EQUAL")}
    })
    
    output$juegomuertes_100.000 <- renderText({  
      
      if (datasetjuego()$muertes_100.000 > datasetjuego2()$muertes_100.000) {print("Muertes por cada 100.000 habitantes: HIGHER")} 
      else if (datasetjuego()$muertes_100.000 < datasetjuego2()$muertes_100.000) {print("Muertes por cada 100.000 habitantes: LOWER")}
      else {print("Muertes por cada 100.000 habitantes: EQUAL")}
    })
    
    # Creación de una tabla para mostrar los datos de cada país seleccionado
    output$tablajuego1 <- renderDataTable({datasetjuego()})
    output$tablajuego2 <- renderDataTable({datasetjuego2()})
    
}
# run shiny
shinyApp(ui,server)

