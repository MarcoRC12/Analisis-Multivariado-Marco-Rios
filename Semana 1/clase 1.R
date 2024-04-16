resultados2014 = read_delim( "C:/Users/marco/Desktop/7 mo ciclo/Analisis multiariado/Clase 1 - 03 22/resultados2014.csv", ",",
                            escape_double = FALSE, trim_ws=TRUE)

resultados2018 = read_delim( "C:/Users/marco/Desktop/7 mo ciclo/Analisis multiariado/Clase 1 - 03 22/resultados2018.csv", ",",
                            escape_double = FALSE, trim_ws=TRUE)

head(resultados2014,5)

head(resultados2018,5)

#//CUANTAS FILAS Y COLUMNAS TIENEN
dim()

#//TIPO DE DATOS , double, string
str(resultados2018)

#//Nombres tienen las variables
names()

#//Datos descriptivos de las variables
summary(resultados2018)


names(resultados2018)
names(resultados2014)


#//SI queremos cambiar los nombres de las variables uno por 1
pase18 = names(resultados2018)[names(resultados2018) == 'votos1']

partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18',
                    'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')

#//Si queremos cambiar varias variables de una sola
pase18 = names(resultados2018)[names(resultados2018) == 'votos1']

partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18',
                    'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')


cam_nombre = function(dataframe)
  
{
  
  for (i in 1:length(partidos_nombre))
    
  {
    names(dataframe)[names(dataframe)
                     ==
                       paste0('votos', i)] = partidos_nombre[i]
    
  }
  
  return(dataframe)
  
}

resultados2018=cam_nombre(resultados2018)

#//CALCULAR PORCENTAJE

votos_porcentaje= function(dataframe){
  
  x=dataframe%>%
    
    group_by(codigo)%>%
    
    mutate_all(funs((. / votos_validos)*100))%>%
    
    select(-votos_validos)
  
  return(x)
  
}

#//Ejecutar CALCULAR PORCENTAJE
por_resultados2014 = votos_porcentaje(resultados2014)

por_resultados2018 = votos_porcentaje(resultados2018)

#//PARTIDO GANADOR POR ZONA

winner = function(dataframe, periodo){
  
  x = dataframe%>%
    
    gather(partido, votos, -codigo) %>%
    
    group_by(codigo)%>%
    
    filter(votos==max(votos))%>%
    
    separate(partido, c(paste0("partido", periodo)),
             sep="1")%>%
    
    select(-votos)
  
  return(x)
  
}

#//ejecutar partido ganador por zona
winner2014=winner(por_resultados2014, 14)

winner2018 =winner(por_resultados2018, 18)


#¿Cómo cambió la distribución de los cantones ganados por cada
#partido político en comparación con las elecciones de 2014?
  
  cambio = winner2018%>%
  
  left_join(winner2014, by="codigo")%>%
  
  mutate(cambio=ifelse(partido18==partido14,"sin
cambio", "cambio"),
         
         robo=ifelse(cambio=="cambio",
                     paste(partido18, partido14, sep=" al "), "sin
cambio"))

table(cambio$cambio)
