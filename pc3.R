# Cargar librerías necesarias
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(cluster)
library(factoextra)
library(mltools)
library(data.table)
library(gridExtra)
library(factoextra)

# 1. Descomprimir el fichero y cargar los datos
unzip("C:/final/data.zip", exdir = "C:/final/data")
csv_file <- list.files("C:/final/data", pattern = "*.csv", full.names = TRUE)

# Leer el archivo CSV usando read_table
logs <- read_table(csv_file, col_names = FALSE)

# 2. Asignar nombres de columna
colnames(logs) <- c("Requester", "Time", "Method", "Page", "Protocol", "Status", "Size")

# Limpieza y transformación de datos
logs <- logs %>%
  mutate(
    Method = gsub('"', '', Method),
    Page = gsub('"', '', Page),
    Protocol = gsub('"', '', Protocol),
    #Status = as.character(Status),
    Size = as.numeric(Size)
  ) 

# 3. Convertir a datos elegantes
logs <- logs %>%
  mutate(
    Requester = as.factor(Requester),
    Method = as.factor(Method),
    Status = as.factor(Status),
    #Page = as.character(Page)
  )

# Extraer la hora de la columna Time
logs <- logs %>%
  mutate(
    Hour = as.numeric(gsub("^.*:(\\d+):(\\d+):\\d+\\]$", "\\1", Time))  # Extraer las horas utilizando expresiones regulares
  )


# 4. Número único de usuarios según errores
error_logs <- logs %>%
  filter(Status >= 400)
unique_users_error <- length(unique(error_logs$Requester))
unique_users_no_error <- length(unique(logs$Requester)) - unique_users_error

# Descripción de los tipos de errores
error_summary <- error_logs %>%
  group_by(Status) %>%
  summarise(UniqueUsers = n_distinct(Requester))

# 5. Análisis de tipos de peticiones HTTP
http_methods <- logs %>%
  group_by(Method) %>%
  summarise(Frequency = n())

# Filtrar peticiones de imágenes
image_requests <- logs %>%
  filter(grepl("\\.(jpg|jpeg|png|gif|bmp|html)$", Page)) %>%
  group_by(Method) %>%
  summarise(Frequency = n())

# 6. Visualización de resultados
# Gráfico 1: Frecuencia de métodos HTTP
grafico0 <- ggplot(http_methods, aes(x = Method, y = Frequency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Frecuencia de Métodos HTTP")

# Gráfico 2: Usuarios únicos por tipo de error
grafico1 <- ggplot(error_summary, aes(x = Status, y = UniqueUsers)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Usuarios Únicos por Tipo de Error")

# 7. Gráfico de peticiones servidas a lo largo del tiempo
time_series <- logs %>%
  #mutate(Hour = hour(Time)) %>%
  group_by(Hour) %>%
  summarise(Requests = n())
 

grafico2 <- ggplot(time_series, aes(x = Hour, y = Requests)) +
  geom_line() +
  theme_minimal() +
  ggtitle("Número de Peticiones Servidas a lo Largo del Tiempo")

# 8. Clustering de datos
# Añadir longitud de la URL como característica numérica
logs <- logs %>%
  mutate(URL_Length = nchar(Page))

# Eliminar filas con NA
logs <- na.omit(logs)

#Normalizar el tamaño de la solicitud
#logs$SizeNorma <- scale(logs$Size)
logs$SizeNorma <- scale(logs$Size, center = FALSE)

# Convertir factores a variables numéricas
logs_one_hot <- one_hot(as.data.table(logs[, c("Method", "Status", "SizeNorma", "Hour", "URL_Length"), drop = FALSE]), sparsifyNAs = TRUE)
#logs_one_hot <- one_hot(as.data.table(logs[, c("SizeNorma","Hour"), drop = FALSE]), sparsifyNAs = TRUE)

# K-means clustering
set.seed(1234)
k2 <- kmeans(logs_one_hot, centers = 2, nstart = 25)
k3 <- kmeans(logs_one_hot, centers = 3, nstart = 25)
logs$cluster2 <- as.factor(k2$cluster)
logs$cluster3 <- as.factor(k3$cluster)


# 9. Visualización de clustering
#grafico3 <- fviz_cluster(k2, data = logs_one_hot) + ggtitle("Clustering con k = 2")
#grafico4 <- fviz_cluster(k3, data = logs_one_hot) + ggtitle("Clustering con k = 3")

# Visualización de clustering con k = 2
ggplot() + geom_point(aes(x = "Method", y = "Status", color = cluster2), data = logs, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = k2$centers[, 1], y = k2$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 2 / K-Medios') + 
  xlab('X') + ylab('Y')

# Visualización de clustering con k = 3
#grafico4 <- fviz_cluster(k3, geom = "point", data = logs_one_hot, label = "none") + 
#  ggtitle("Clustering con k = 3") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  labs(x = dimension_names[1], y = dimension_names[2])  # Etiquetas para las dimensiones


# graficar
#grid.arrange(grafico0,grafico1, grafico2 ,grafico3, grafico4, nrow = 5)
#grid.arrange( grafico3, nrow = 5)

