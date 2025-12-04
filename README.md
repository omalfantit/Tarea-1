# Tarea-1
Tarea 1 del curso de Datos de la gran profesora Soledad Araya

# Ranking de películas de IMDB por género y ganadoras del Oscar
Instalación y carga de librería de los siguientes paquetes:  
library(tidyverse)  
library(highcharter)  
library(htmlwidgets)

# Limpieza de base de datos
#Elimina las filas donde la columna genres está vacía (igual a "")  
df2 <- df[-which(df$genres == ""), ]  
#Elimina varias columnas por índice.  
df2 <- df2[, -c(1:3,5,7,10:16)]  
#Elimina filas duplicadas  
df2 <- df2[!duplicated(df2), ]  
#Revisa si existe al menos un valor NA en toda la BD  
any(is.na(df2))  
#Elimina filas con NA en la columna genres (no solo vacías, como en la primera línea)  
df2 <- df2[!is.na(df2$genres),]  
#Elimina filas con NA en la columna averageRating  
df2 <- df2[!is.na(df2$averageRating),]  
#Muestra una vista compacta del dataframe final (df2), incluyendo tipo de dato y ejemplos de valores.  
#Es parte del paquete dplyr.  
glimpse(df2)  
#eliminar segunda palabra en columna (después de la ,)  
df2$genres <- gsub(",.*","",df2$genres)  
#promedios de una columna  
df3 <- df2 %>% group_by(genres) %>% summarize(averageRating = round(mean(averageRating),2))

# plot
#Usa el data frame df3  
#Eje X: el campo genres  
#Eje Y: averageRating  
#Tipo de gráfico: línea  
p <- hchart(df3, type = 'line',  
            hcaes(x = 'genres', y = 'averageRating')) %>%  
#Color de la línea  
  hc_plotOptions(line = list(color = "black")) %>%  
#Esto convierte la línea en un gráfico circular estilo radar chart  
  hc_chart(polar = TRUE) %>%  
#Ajusta ejes  
  hc_yAxis(max = 7.5, title = FALSE) %>%  
  hc_xAxis(title = FALSE) %>%  
#Títulos del gráfico  
#Títulos alineados a la izquierda  
#Fuente en negrita  
#Tamaños personalizados  
  hc_title(text = 'Rating de películas en IMDB', style = list(fontWeight = 'bold', fontSize = '30px'),  
           align = 'left') %>%  
  hc_subtitle(text = 'películas en IMDB ranqueadas por género', style = list(fontWeight = 'bold', fontSize = '15px'),  
              align = 'left') %>%  
#Tooltip personalizado  
#Al pasar el mouse:  
#Muestra el género en azul.  
#Muestra el rating en cursiva.  
  hc_tooltip(useHTML = TRUE, headerFormat = '',  
             pointFormat = '<b> <span style = "color: blue"> {point.genres} </span> <br> <em> Average Rating: {point.y} <em>') %>%  
#Créditos personalizados  
  hc_credits(enabled = TRUE, text = 'Por Osvaldo Malfanti basado en Antonela Tamagnin <br> Source: IMDB Website') %>%  
#Aplica un tema visual  
#Agrega un estilo preconfigurado (colores suaves, fondo claro)  
  hc_add_theme(hc_theme_gridlight())  
p  
#Guarda el gráfico como html  
#Esto genera un archivo HTML completamente interactivo, independiente (puede abrirse en cualquier navegador)  
saveWidget(widget = p, file = 'plot.html')  
