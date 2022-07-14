library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gt)
library(markdown)

# load('data.Rdata')

tabla_resultados <- readRDS("www/tabla_resultados.RDS")

about_ui <- tabPanel(title = 'Inicio',
                     includeMarkdown('README.md'))


main_ui <- {
  
  
  
  navbarPage('ECOFEMIDATA',
             #sample_plot_ui('ejemplo'),
             about_ui,
             #tab de inicio (rmd): con ilustración, accesos directos, bienvenida, objetivos, como usar plotly
             
             navbarMenu(title = 'Mercado de Trabajo',
             tasas_sexo_ui('tasas_sexo'),
             tasas_edad_ui('tasas_edad'),
             tipo_insercion_ui('jerarquias'),
             ramas_ui('ramas')
             ),
             
             navbarMenu(title = 'Ingresos',
             brechas_ui('brechas_general'),
             brechas_desag_ui('brechas_desag'),
             deciles_ui('deciles')

             ),
             
             navbarMenu(title = 'Uso del tiempo',
                        #remunerado
                        horas_remunerado_ui('horas_remuneradas'),
                        #no remunerado
                        horas_no_remunerado_ui('horas_no_rem')
                       
             ),
             
             navbarMenu(title = 'Trabajadoras de Casas Particulares',
                        #principales indicadores del informe
                        serv_dom_ocupadas_ui('s_d_ocup'),
                        serv_dom_ing_ui('s_d_ing'),
                        serv_dom_derechos_ui('s_d_derechos')
                        
                        
             )
             
             )
}
