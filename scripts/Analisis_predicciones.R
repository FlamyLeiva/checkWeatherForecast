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

clima_horario<- read.csv("clima_horario.csv")
clima_raw<- read.csv("clima_raw.csv")



descriptionOrchard_mongo$find() %>%
  as_tibble()  -> descriptionOrchard

# descriptionOrchard %>%
#   filter(region=="Libertador General Bernardo O'Higgins") %>%
#   filter(clientValue=="Garces_Fruit")-> Orchards_Garces_6ta

descriptionOrchard %>%
  filter(region %in% c("Libertador General Bernardo O'Higgins", "Maule")) -> Orchards_6_7


predicciones_mongo$find() %>%
  as_tibble()  -> predicciones

#Weatherdaily

# Crear un filtro BSON para el operador $in
#filter_bson <- paste0('{"orchard": {"$in": ', toJSON(Orchards_6_7$value), '}}')

# Filtrar directamente en la base de datos y cargar el resultado como tibble
# WeatherDaily_Garces <- WeatherDaily_mongo$find(filter_bson) %>%
#   as_tibble()
clima_horario %>% 
  filter(orchard %in% Orchards_6_7$value) -> clima_horario_6_7

# WeatherDaily_Garces <- WeatherDaily %>%
#   filter(orchard %in% Orchards_Garces_6ta$value)

#filtrar predicciones 

predicciones_6_7 <- predicciones %>%
  filter(orchard %in% Orchards_6_7$value)

#write.csv(predicciones_Garces, "oredicciones_Garces.csv")

#write.csv(WeatherDaily_Garces, "WeatherDaily_Garces.csv")

#filtros de fecha 
fecha_inicio_prediccion <- min(predicciones_6_7$datetimePredict)

WeatherDaily_6_7<- clima_horario_6_7%>%
  filter(datetime >= fecha_inicio_prediccion)

# cambiar fecha 
WeatherDaily_6_7 <- WeatherDaily_6_7 %>%
  mutate(datetime = with_tz(datetime, tz = "UTC")) %>% 
  mutate(datetime = force_tz(datetime, tz = "America/Santiago"))


#Comparar predicciones

# Columna para la diferencia en horas desde que se predice hasta la hora de predicción 

predicciones_6_7 <- predicciones_6_7 %>%
  mutate(  # Redondear a enteros


# Redondear las fechas a la hora más cercana para luego comparar 

predicciones_6_7 <- predicciones_6_7%>%
  mutate(datetimePredict = as.POSIXct(cut(datetimePredict, breaks = "hour")))

# WeatherDaily_6_7 <- WeatherDaily_Garces %>%
#   mutate(datetime = round_date(datetime, unit = "hour"))

#juntar dataframes para comparar 

df_comparacion <- predicciones_6_7 %>%
  inner_join(clima_horario_6_7, by = c("datetimePredict" = "datetime"))


# Calcular la diferencia de temperatura
df_comparacion <- df_comparacion %>%
  mutate(diferencia_tempMean = Predict_tempMean - tempMean)

resumen_diferencias <- df_comparacion %>%
  group_by(diferencia_horas,orchard.x) %>%
  summarize(
    diferencia_promedio = mean(diferencia_tempMean),
    desviacion_estandar = sd(diferencia_tempMean),
    mediana= median(diferencia_tempMean)
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
  geom_ribbon(aes(ymin = diferencia_promedio - desviacion_estandar, ymax = diferencia_promedio + desviacion_estandar), alpha = 0.2) +
  labs(title = "Relación entre Diferencia de Horas y Exactitud de Predicciones",
       x = "Diferencia de Horas",
       y = "Diferencia Promedio de Temperatura") +
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = mediana), color = "green")+
  theme_minimal()


graf <- ggplot(resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio)) +
  geom_line() +
  geom_ribbon(aes(ymin = diferencia_promedio - desviacion_estandar, ymax = diferencia_promedio + desviacion_estandar), alpha = 0.1) +
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio), color = "red") +
  labs(title = "Relación entre Diferencia de Horas y Exactitud de Predicciones",
       x = "Diferencia de Horas",
       y = "Diferencia Promedio de Temperatura") +
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = mediana), color = "green")+
  theme_minimal()

#AHORA CON PRECIPITACION

# Calcular la diferencia de precipitacion
df_comparacion <- df_comparacion %>%
  mutate(diferencia_precipitation = Predict_precipitation - precipitation)

resumen_diferencias_preci <- df_comparacion %>%
  group_by(diferencia_horas) %>%
  summarize(
    diferencia_promedio = mean(diferencia_precipitation),
    desviacion_estandar = sd(diferencia_precipitation),
    mediana= median(diferencia_precipitation)
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
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = mediana), color = "green")+
  theme_minimal()

ggplot(resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio)) +
  geom_line() +
  geom_ribbon(aes(ymin = diferencia_promedio - desviacion_estandar, ymax = diferencia_promedio + desviacion_estandar), alpha = 0.3) +
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = diferencia_promedio), color = "red") +
  labs(title = "Relación entre Diferencia de Horas y Exactitud de Predicciones",
       x = "Diferencia de Horas",
       y = "Diferencia Promedio de Temperatura") +
  geom_point(data = resumen_diferencias, aes(x = diferencia_horas, y = mediana), color = "green")+
  theme_minimal()

diferencias_medianas<- resumen_diferencias%>%
  mutate(medianas)


#por orchard printear datetime versus temperatura

Los_Gomeros<-clima_horario_6_7%>%
  filter(orchard=="Los_Gomeros")

Gomeros_predicciones<- predicciones_6_7 %>%
  filter(orchard=="Los_Gomeros")

a <- ggplot(Gomeros_predicciones, aes(x = datetime, y =Predict_tempMean)) +
  geom_point() +  # Elige el tipo de geometría que deseas, por ejemplo, geom_point()
  labs(title = "Título del Gráfico", x = "Etiqueta del Eje X", y = "Etiqueta del Eje Y")


