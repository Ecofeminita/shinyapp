library(shiny)
library(shinythemes)
library(shinycssloaders)
library(glue)
library(DT)
library(plotly)
library(gridExtra)
library(shinyjs)
library(shinydashboard)



ui <- fluidPage( 
    theme = shinytheme("journal"),

    uiOutput(outputId = "main_ui")
)


##### server #####

server <- function (input, output,session) {
    
     #render UI 
    output$main_ui <- renderUI({
        main_ui})
    
    ########## 
    
    # Output modules ----------------------------------------------------------
    sample_plot_server('ejemplo')
    tasas_sexo_server('tasas_sexo')

}


##### RUN #####

shinyApp(ui, server)

