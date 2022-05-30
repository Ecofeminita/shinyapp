

#Hola! Acá levantamos dfs que vayamos a usar en varios módulos o funciones que necesitemos en varios módulos.
#De esta forma, los levanta una sola vez cuando empieza a correr la app


#Objects defined in global.R are similar to those defined in app.R outside of the server function definition, with one important difference: they are loaded into the global environment of the R session; all R code in a Shiny app is run in the global environment or a child of it.



tabla_resultados <- readRDS("www/tabla_resultados.RDS")