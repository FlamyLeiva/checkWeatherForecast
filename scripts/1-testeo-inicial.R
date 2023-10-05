
pacman::p_load(httr,
               jsonlite, 
               mongolite, 
               tidyverse)
library(lubridate)


# predios vilab (desde API Vilab)  ------------------------------------------------

GET('https://api.vilab.cl/index.php/api/predios/key/7df5d2f73a99ed699a1955c87050ea7d') -> prediosVilab

prediosVilab$content |> 
  rawToChar() |> 
  fromJSON() |> 
  pluck(1) |> 
  select(Nombre, Id, 
         Estacion_institucion, Estacion_nombre,
         Estacion_id, Estacion_lat, Estacion_long) |> 
  
  rename(id_Analytics = Nombre, 
         id_Vilab = Id) -> prediosVilab





# descriptionOrchard (Analytics) ------------------------------------------
mongo(url = 'mongodb+srv://ti-analytics:oS11dxE6qv3T6dYQ@productioncluster.bllew.mongodb.net/', 
      db = 'db-general',
      collection = 'DescriptionOrchard') -> DescriptionOrchard
a<-3


DescriptionOrchard$find(
  
  fields = '{"clientValue": 1, "value": 1, "_id": 1, "location" : 1, "stationId" : 1, "dataSource" : 1}' 
    
  ) -> DescriptionOrchard
  

# std names, selección columnas
DescriptionOrchard |> 
  unnest_wider(location) |> 
  
  rename(orchard = value,
         id_Analytics = `_id`,
         client = clientValue) |> 
  
  select(client, orchard, 
         id_Analytics, 
         lat, lng,
         stationId, dataSource)  |> 
  arrange(client, orchard) -> DescriptionOrchard





# Descargar predicciones ------------------------------------------------

# Definir huerto a consultar

#lista de huertos 


Desctipction_Vilab<- subset(DescriptionOrchard, dataSource == "Vilab")

#tabla de dupla nombre de orchard y id asignado
orchard_station <- Desctipction_Vilab %>%
  select(orchard, stationId) %>%
  distinct() %>%
  drop_na() %>%
  collect()

#lista
get_data_for_orchard_station <- function (stationId) {
  url <- paste0('https://api.vilab.cl/index.php/api/clima_pro/',
                'key/7df5d2f73a99ed699a1955c87050ea7d/',
                'id/',
                stationId)
  response <- GET(url)
  return(response)
}

#get_data_for_orchard_station(5918)
library(dplyr)
library(lubridate)

all_forecasts <- data.frame()

# Iterar sobre la lista de pares OrchardName y StationID
for (i in 1:nrow(orchard_station)) {
  orchard <- orchard_station[i, "orchard"]
  stationId <- orchard_station[i, "stationId"]
  response <- get_data_for_orchard_station(stationId)
  
  forecast2 <- response$content |>
    rawToChar() |>
    fromJSON() |>
    pluck(1) |>
    as_tibble() |>
    rename(datetimePredict = 'Fecha',
           tempMean = '1',
           precipitation = '2',
           relativeHumidityMean = '3',
           windSpeed = '4') 
  
  # Agregar las columnas orchard e idstation como las primeras en forecast2
  forecast2 <- forecast2 %>%
    mutate(orchard = orchard, 
           idstation = stationId,
           datetimeGenerate = force_tz(Sys.time(), tz = 'America/Santiago'),
           predictIndex = seq(1, nrow(forecast2), 1))
  
  # Cambiar el orden de las columnas en forecast2
  forecast2 <- forecast2 %>%
    select(orchard, datetimeGenerate, datetimePredict, predictIndex, everything())
  
  # Agregar las filas de forecast2 a la tabla all_forecasts
  all_forecasts <- rbind(all_forecasts, forecast2)
}

# Verificar la tabla resultante
print(all_forecasts)




#lista para todos los orchards
all_forecasts <- data.frame()

# Iterar sobre la lista de pares OrchardName y StationID
for (i in 1:nrow(orchard_station)) {
  orchard <- orchard_station[i, "orchard"]
  stationId <- orchard_station[i, "stationId"]
  response <- get_data_for_orchard_station(stationId)
  
  forecast2 <- response$content |>
    rawToChar() |>
    fromJSON() |>
    pluck(1) |>
    as_tibble() |>
    rename(datetimePredict = 'Fecha',
           tempMean = '1',
           precipitation = '2',
           relativeHumidityMean = '3',
           windSpeed = '4') 
  
  # Agregar las columnas orchard e idstation como las primeras en forecast2
  forecast2 <- forecast2 %>%
    mutate(orchard = orchard, idstation = stationId)
  
  # Agregar las columnas adicionales a forecast2
  forecast2 <- forecast2 %>%
    mutate(datetimeGenerate =force_tz(Sys.time(), tz = 'America/Santiago'),
           predictIndex = seq(1, nrow(forecast2), 1))
  
  # Agregar las filas de forecast2 a la tabla all_forecasts
  all_forecasts <- rbind(all_forecasts, forecast2)
}
# Verificar la tabla resultante
print(all_forecasts)

# Subir los datos a mongo -------------------------------------
mongo(url = 'mongodb+srv://ti-analytics:pO3xLskbi0vJz4nE@prototypecluster.4cmnn9u.mongodb.net/', 
      db = 'forecastWeather',
      collection = 'test') -> forecastWeather
gl
forecastWeather$insert(forecast)
# Por resolver (por ahora)

# Todos los huertos vilab
# Considerar actualización continua de la data (sin perder la predicción generada a la hora anterior)
# Revisar timezones local/mongo
# Revisar mejor tipo de database en mongo para albergar la data (timeseries y/n)









