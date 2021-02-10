#Ejercicios previos al examen - Técnicas de Visualización 
#Ejercicio 1
#Diego Senso González
#4 febrero 2021

#Objetivo: realizar los 3 gráficos propuestos.

#Se cargan las librerías necesarias y los datos.
#El dataset "flights" contiene información sobre un total de 336.776 vuelos distintos.
#El dataset "airports" contiene información sobre 1458 aeropuertos diferentes de EEUU.
library(ggplot2)
library(nycflights13)
data(flights)
data(airports)

#Se procede a representar los gráficos solicitados:

#Figura A (Flights)
#Se realiza un gráfico de barras con ggplot de la variable "mes". Representa el número de vuelos por cada mes.
ggplot(flights, aes(x = month)) + geom_bar()

#Figura B (Flights)
#Se realiza un gráfico que representa la relación entre las variables "dep_delay" y "arr_delay".
ggplot(flights, aes(x=dep_delay, y=arr_delay)) +
  geom_density_2d()

#Figura C (Airports)
#Se crea un gráfico de dispersión de las variables "longitud" y  "latitud", donde el tamaño de los puntos los determina la altitud del aeropuerto.
#Se define un valor de alpha de 0.05, que determina la transparencia de los puntos.
ggplot(airports, aes(x = lon, y = lat, size = alt)) + 
  geom_point(alpha = 0.05, show.legend = TRUE)

