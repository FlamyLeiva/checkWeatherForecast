
pacman::p_load(httr,
               jsonlite, 
               mongolite, 
               tidyverse)



Sys.getenv('PREDIOS_VLAB') -> PREDIOS_VLAB
#print('Hola', PREDIOS_VLAB)

# predios vilab (desde API Vilab)  ------------------------------------------------

GET(PREDIOS_VLAB) -> prediosVilab

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
mongo(url = Sys.getenv('MDB_PROD'), 
      db = 'db-general',
      collection = 'DescriptionOrchard') -> DescriptionOrchard
cat('busca', Sys.getenv('MDB_PROD'))

DescriptionOrchard$find(
  
  fields = '{"clientValue": 1, "value": 1, "_id": 1, "location" : 1, "stationId" : 1, "dataSource" : 1}' 
  
) -> DescriptionOrchard

print(paste('Buscar',colnames(DescriptionOrchard)))



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


#Cruce con predios 
DescriptionOrchard |> 
  left_join(prediosVilab) -> infoOrchards_Analytics

# Filtrar filas donde "mi_columna" no es NA
infoOrchards_Analytics <- subset(infoOrchards_Analytics, !is.na(id_Vilab))


# Descargar predicciones ------------------------------------------------


#tabla de dupla nombre de orchard y id asignado
orchard_station <- infoOrchards_Analytics %>%
  select(orchard, id_Vilab) %>%
  distinct() %>%
  drop_na() %>%
  collect()

#funcion para acceder a la data de una estacion
get_data_for_orchard_station <- function (stationId) {
  url <- paste0('https://api.vilab.cl/index.php/api/clima_pro/',
                Sys.getenv('KEY_API'),
                'id/',
                stationId)
  response <- GET(url)
  return(response)
}

#get_data_for_orchard_station(5918)

#tabla que alojara todos los huertos
all_forecasts <- data.frame()

# Iterar sobre la lista de pares OrchardName y StationID
for (i in 1:nrow(orchard_station)) {
  orchard <- orchard_station[i, "orchard"]
  stationId <- orchard_station[i, "id_Vilab"]
  response <- get_data_for_orchard_station(stationId)
  
  forecast2 <- response$content |>
    rawToChar() |>
    fromJSON() |>
    pluck(1) |>
    as_tibble() |>
    rename(datetimePredict = 'Fecha',
           Predict_tempMean = '1',
           Predict_precipitation = '2',
           Predict_relativeHumidityMean = '3',
           Predict_windSpeed = '4') 
  
  # Agregar las columnas orchard e idstation como las primeras en forecast2
  forecast2 <- forecast2 %>%
    mutate(orchard = orchard, 
           stationId = stationId,  # Cambiamos id_Vilab a stationId
           datetime = as.POSIXct(Sys.time(), tz = 'America/Santiago'),
           datetimePredict = ymd_hms(datetimePredict),
           datetime = ymd_hms(datetime),
           predictIndex = seq(1, nrow(forecast2), 1)) %>%
    unnest(c(orchard, stationId))
  
  # Cambia el nombre de la columna id_Vilab a stationId
  colnames(forecast2)[colnames(forecast2) == "id_Vilab"] <- "stationId"
  
  # Cambia el orden de las columnas en forecast2 (por defecto se agregaban al final)
  forecast2 <- forecast2 %>%
    select(orchard, datetime, datetimePredict, predictIndex, everything())
  
  colnames(forecast2)[colnames(forecast2) == "id_Vilab"] <- "stationId"
  
  # Agrega las filas de forecast2 a la tabla consoidad all_forecasts
  all_forecasts <- rbind(all_forecasts, forecast2)
}

# Verificar la tabla resultante
print(all_forecasts)


# Subir los datos a mongo -------------------------------------
mongo(url = Sys.getenv('MDB_PROT'), 
      db = 'forecastWeather',
      collection = 'test6') -> forecastWeather

forecastWeather$insert(all_forecasts)


# Todos los huertos vilab OK
# Considerar actualización continua de la data (sin perder la predicción generada a la hora anterior)
# Revisar