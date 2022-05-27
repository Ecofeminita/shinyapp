library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gt)
library(markdown)

# load('data.Rdata')



about_ui <- tabPanel(title = 'about',
                     includeMarkdown('README.md'))


main_ui <- {
  
  tabla_resultados <- readRDS("www/tabla_resultados.RDS")
  
  navbarPage('titulo',
             #sample_plot_ui('ejemplo'),
             about_ui,
             
             navbarMenu(title = 'Mercado de Trabajo',
             tasas_sexo_ui('tasas_sexo'),
             tasas_edad_ui('tasas_edad'),
             tipo_insercion_ui('jerarquias'),
             ramas_ui('ramas')
             ),
             
             navbarMenu(title = 'Ingresos',
             brechas_ui('brechas_general'),
             brechas_desag_ui('brechas_desag')

             )
             
             )
}
