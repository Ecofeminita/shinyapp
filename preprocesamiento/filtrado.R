#¿Qué variables filtro?

#levanto una sola como toy
base <- get_microdata(year = 2019, 
                       trimester = 4,
                       type =  c('individual'))


nombres <- names(base)


tt <- readLines("preprocesamiento/prueba_texto.txt")

tt <- toString(tt)

class(tt)


filtro <- sapply(nombres, grepl, tt)


nombres_filtrar <- c(nombres[filtro],"PP04B_COD", "REGION")



prueba <- base %>% select(nombres_filtrar)


prueba <- limpieza_individuos(prueba)


creador_tablas(prueba,tareas_domesticas = F)

###hogar

base <- get_microdata(year = 2019, 
                      trimester = 4,
                      type =  c('hogar'))

b_p <- get_microdata(year = 2019, 
                     trimester = 4,
                     type =  c('individual'))

b_p <- limpieza_individuos(b_p)

nombres <- names(base)

filtro <- sapply(nombres, grepl, tt)

nombres_filtrar_hogar <- c(nombres[filtro])

prueba <- base %>% select(nombres_filtrar_hogar)

tareas_domesticas_sexo(b_p,prueba)


write.xlsx(list("personas" = nombres_filtrar,
                "hogares" = nombres_filtrar_hogar),
           file = "preprocesamiento/nombres_filtro.xlsx")
