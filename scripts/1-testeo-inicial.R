
pacman::p_load(httr,
               jsonlite, 
               mongolite, 
               tidyverse)

# Descargar descriptionOrchard ------------------------------------------
mongo(url = 'mongodb+srv://ti-analytics:oS11dxE6qv3T6dYQ@productioncluster.bllew.mongodb.net/', 
      db = 'db-general',
      collection = 'DescriptionOrchard') -> DescriptionOrchard



DescriptionOrchard$find(
  
  fields = '{"clientValue": 1, "value": 1, "_id": 1, "location" : 1, "stationId" : 1, "dataSource" : 1}' 
    
  ) -> DescriptionOrchard
  

# predios vilab 

GET('https://api.vilab.cl/index.php/api/predios/key/7df5d2f73a99ed699a1955c87050ea7d') -> prediosVilab

# fromJSON(rawToChar(prediosVilab$content))[['predios']] 

prediosVilab$content |> 
  rawToChar() |> 
  fromJSON() |> 
  pluck('predios') |> 
  select(Nombre, Id, 
         Estacion_institucion, Estacion_nombre,
         Estacion_id, Estacion_lat, Estacion_long) |> 
  
  rename(id_Analytics = Nombre, 
         id_Vilab = Id) -> prediosVilab

  


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




# Definir huerto 

huerto <- 'Canadilla'


GET(paste0('https://api.vilab.cl/index.php/api/clima_pro/',
           'key/7df5d2f73a99ed699a1955c87050ea7d/',
           'id/', 
           DescriptionOrchard |> 
             filter(orchard == huerto) |> 
             select(stationId))) -> res
 
res$content |> 
  rawToChar() |> 
  fromJSON() |> 
  pluck(1) |> 
  as_tibble() |> 
  
  rename(datetimePredict = 'Fecha',
         tempMean = '1',
         precipitation = '2',
         relativeHumidityMean = '3',
         windSpeed = '4') -> forecast
  


# Agregar nuevas columnas 

forecast |> 
  
  mutate(orchard = huerto,
         .before = datetimePredict) |> 

  mutate(datetimeGenerate = Sys.time(), 
         .before = datetimePredict) |> 
  
  mutate(predictIndex = seq(1, nrow(forecast), 1),
         .after = datetimePredict) -> forecast




# Agregar los datos a mongo 
mongo(url = 'mongodb+srv://ti-analytics:pO3xLskbi0vJz4nE@prototypecluster.4cmnn9u.mongodb.net/', 
      db = 'forecastWeather',
      collection = 'test') -> forecastWeather


# Todos los huertos vilab
# Este punto se debe mejorar, considerando actualización de la data
# Revisar timezone 
# revisar timezones local/mongo

forecastWeather$insert(forecast)









