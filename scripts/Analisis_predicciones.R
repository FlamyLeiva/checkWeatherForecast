pacman::p_load(httr,
               jsonlite, 
               mongolite, 
               tidyverse)


mongo(url = 'mongodb+srv://ti-analytics:pO3xLskbi0vJz4nE@prototypecluster.4cmnn9u.mongodb.net/',
      db = 'forecastWeather',
      collection = 'test6') -> predicciones_mongo

mongo(url = 'mongodb+srv://ti-analytics:oS11dxE6qv3T6dYQ@productioncluster.bllew.mongodb.net/',
      db = 'db-general',
      collection = 'DescriptionOrchard') -> descriptionOrchard_mongo

mongo(url = 'mongodb+srv://ti-analytics:oS11dxE6qv3T6dYQ@productioncluster.bllew.mongodb.net/',
      db = 'db-general',
      collection = 'WeatherDailyByHours') -> WeatherDaily_mongo


descriptionOrchard_mongo$find() %>%
  as_tibble()  -> descriptionOrchard

descriptionOrchard %>% 
  filter(region=="Libertador General Bernardo O'Higgins") %>%
  filter(clientValue=="Garces_Fruit")-> Orchards_Garces_6ta

predicciones_mongo$find() %>%
  as_tibble()  -> predicciones

#Weatherdaily

# Crear un filtro BSON para el operador $in
filter_bson <- paste0('{"orchard": {"$in": ', toJSON(Orchards_Garces_6ta$value), '}}')

# Filtrar directamente en la base de datos y cargar el resultado como tibble
WeatherDaily_Garces <- WeatherDaily_mongo$find(filter_bson) %>%
  as_tibble()


# WeatherDaily_Garces <- WeatherDaily %>%
#   filter(orchard %in% Orchards_Garces_6ta$value)

#filtrar predicciones 

predicciones_Garces <- predicciones %>%
  filter(orchard %in% Orchards_Garces_6ta$value)

#write.csv(predicciones_Garces, "oredicciones_Garces.csv")

#write.csv(WeatherDaily_Garces, "WeatherDaily_Garces.csv")

#filtros de fecha 
fecha_inicio_prediccion <- min(predicciones_Garces$datetimePredict)

WeatherDaily_Garces <- WeatherDaily_Garces %>%
  filter(datetime >= fecha_inicio_prediccion)

# cambiar fecha 
WeatherDaily_Garces <- WeatherDaily_Garces %>%
  mutate(datetime = with_tz(datetime, tz = "UTC")) %>% 
  mutate(datetime = force_tz(datetime, tz = "America/Santiago"))


#Comparar predicciones

# Columna para la diferencia en horas desde que se predice hasta la hora de predicción 

predicciones_Garces <- predicciones_Garces %>%
  mutate(diferencia_horas = round(as.numeric(difftime(datetimePredict, datetime, units = "hours")), 1))  # Redondear a enteros


# Redondear las fechas a la hora más cercana para luego comparar 

predicciones_Garces_2 <- predicciones_Garces %>%
  mutate(datetimePredict = as.POSIXct(cut(datetimePredict, breaks = "hour")))

WeatherDaily_Garces_2 <- WeatherDaily_Garces %>%
  mutate(datetime = round_date(datetime, unit = "hour"))

#juntar dataframes para comparar 

df_comparacion <- predicciones_Garces_2 %>%
  inner_join(WeatherDaily_Garces_2, by = c("datetimePredict" = "datetime"))


# Calcular la diferencia de temperatura
df_comparacion <- df_comparacion %>%
  mutate(diferencia_tempMean = Predict_tempMean - tempMean)

resumen_diferencias <- df_comparacion %>%
  group_by(diferencia_horas) %>%
  summarize(
    diferencia_promedio = mean(diferencia_tempMean),
    desviacion_estandar = sd(diferencia_tempMean)
  )

resumen_diferencias<- resumen_diferencias %>%
  arrange(abs(diferencia_promedio))

#grafico
library(ggplot2)

# Supongamos que tienes una columna de fechas en tu dataframe llamada 'fecha_prediccion'
# y una columna de diferencias de horas llamada 'diferencia_horas'
# y una columna de diferencias de temperatura llamada 'diferencia_temperatura'

# Crear un gráfico de series temporales
ggplot(resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio)) +
  geom_line() +
  geom_ribbon(aes(ymin = diferencia_promedio - desviacion_estandar, ymax = diferencia_promedio + desviacion_estandar), alpha = 0.3) +
  labs(title = "Relación entre Diferencia de Horas y Exactitud de Predicciones",
       x = "Diferencia de Horas",
       y = "Diferencia Promedio de Temperatura") +
  theme_minimal()

graf <- ggplot(resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio)) +
  geom_line() +
  geom_ribbon(aes(ymin = diferencia_promedio - desviacion_estandar, ymax = diferencia_promedio + desviacion_estandar), alpha = 0.3) +
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio), color = "red") +
  labs(title = "Relación entre Diferencia de Horas y Exactitud de Predicciones",
       x = "Diferencia de Horas",
       y = "Diferencia Promedio de Temperatura") +
  theme_minimal()

#AHORA CON PRECIPITACION

# Calcular la diferencia de precipitacion
df_comparacion <- df_comparacion %>%
  mutate(diferencia_precipitation = Predict_precipitation - precipitation)

resumen_diferencias_preci <- df_comparacion %>%
  group_by(diferencia_horas) %>%
  summarize(
    diferencia_promedio = mean(diferencia_precipitation),
    desviacion_estandar = sd(diferencia_precipitation)
  )

resumen_diferencias_preci<- resumen_diferencias_preci %>%
  arrange(abs(diferencia_promedio))

graf_preci <- ggplot(resumen_diferencias_preci, aes(x = diferencia_horas, y = diferencia_promedio)) +
  geom_line() +
  geom_ribbon(aes(ymin = diferencia_promedio - desviacion_estandar, ymax = diferencia_promedio + desviacion_estandar), alpha = 0.3) +
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio), color = "red") +
  labs(title = "Relación entre Diferencia de Horas y Exactitud de Predicciones",
       x = "Diferencia de Horas",
       y = "Diferencia Promedio de precipitacion") +
  theme_minimal()

