library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(DT)
library(plotly)
library(gridExtra)
library(shinyjs)
library(shinydashboard)
library(shinyalert)


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
    
    
    chooseSliderSkin("Flat", color = "#e5616e"),
    
    tags$style(type = 'text/css', 
               HTML(".container-fluid > .nav > li > 
                        a[data-value='Metodología'] {background-color: #D3D3D3; color:black}
                      
                    ")),
    
    #y agregar esto?
    # .container-fluid > .nav > li > 
    #   a[data-value='Mercado de Trabajo'] {background-color: #e2616e; color:black}
    #       .container-fluid > .nav > li > 
    #       a[data-value='Ingresos'] {background-color: #687aad; color:black}
    #           .container-fluid > .nav > li > 
    #           a[data-value='Uso del tiempo'] {background-color: #e7bfce; color:black}
    #               .container-fluid > .nav > li > 
    #               a[data-value='Trabajadoras de Casas Particulares'] {background-color: #8adbd1; color:black}
    

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

