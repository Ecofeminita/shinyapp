library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(DT)
library(plotly)
library(gridExtra)
library(shinyjs)
library(shinydashboard)

#tabla_resultados <- readRDS("www/tabla_resultados.RDS")



ui <- fluidPage( 
    theme = shinytheme("journal"),
    
    #define fakeClick for buttons
    (tags$head(tags$script(HTML('var fakeClick = function(tabName) {
                                                         var dropdownList = document.getElementsByTagName("a");
                                                         for (var i = 0; i < dropdownList.length; i++) {
                                                         var link = dropdownList[i];
                                                         if(link.getAttribute("data-value") == tabName) {
                                                         link.click();
                                                         };
                                                         }
                                                         };
                                                         '))) ),

    uiOutput(outputId = "main_ui")
)


##### server #####



server <- function (input, output,session) {
    
    
    
     #render UI 
    output$main_ui <- renderUI({
        main_ui})
    
    ########## 
    
    # Output modules ----------------------------------------------------------
    #sample_plot_server('ejemplo')
    tasas_sexo_server('tasas_sexo')
    tasas_edad_server('tasas_edad')
    tipo_insercion_server('jerarquias')
    ramas_server('ramas')
    
    brechas_server('brechas_general')
    brechas_desag_server('brechas_desag')
    deciles_server('deciles')
    
    horas_remunerado_server('horas_remuneradas')
    horas_no_remunerado_server('horas_no_rem')
    
    
    serv_dom_ocupadas_server('s_d_ocup')
    serv_dom_ing_server('s_d_ing')
    serv_dom_derechos_server('s_d_derechos')

}


##### RUN #####

shinyApp(ui, server)

