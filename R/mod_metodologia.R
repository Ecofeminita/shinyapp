metodologia_ui <- tabPanel(title = 'Metodología',
         #includeMarkdown('README.md')
         
         titlePanel('Metodología'),
         
         fluidRow(
           column(12,
                  column(9,
         
         tags$div(   id = "intro_metod", style="text-align: justify;",
                   tags$p('Para realizar los cálculos se utilizan bases de datos publicadas por la Encuesta Permanente de Hogares del INDEC. Esta encuesta releva características demográficas, sociales, económicas, con especial atención a las formas de participación en el mercado laboral, entre hogares y personas de 31 aglomerados urbanos (en aquellos casos en los que los indicadores no refieren al total de la muestra, el universo se aclara en el subtitulo del gráfico o tabla). Para la elaboración de las series a precios constantes, se utiliza el Índice de Precios al Consumidor elaborado por el mismo organismo.'),
                   
                   tags$p('Respecto del impacto de la pandemia por COVID-19, tanto sobre los resultados como sobre la calidad de los datos, cabe aclarar que la encuesta pasó de la modalidad presencial a la telefónica para contactar y realizar la entrevista a los hogares. El propio INDEC advierte que ciertos datos no cuentan con la calidad con la que habitualmente la EPH presenta sus resultados.'),
                   
                   
                   tags$p('Los indicadores presentados se encuentran desagregados según la variable disponible, que indica el sexo declarado por las personas respondentes de la encuesta, de acuerdo a una distinción binaria. Bregamos por la incorporación definitiva de una pregunta que nos permita tener información sobre la identidad de género de las personas en las estadísticas públicas de manera general (además de la referida al sexo asignado al nacer). La población trans-travesti históricamente ha sido excluida mediante violencia física, simbólica, psicológica, sexual y económica del sistema educativo, de salud y del trabajo formal. Dada la situación de vulnerabilidad a la que muches se enfrentan, resulta fundamental que estén presentes en las estadísticas oficiales.')),
         
         tags$div(style="display: inline-flex;", id = "codigo_git",
                   tags$p('La app se realizó utilizando el lenguaje de programación R. Para les curioses, todo el código se encuentra disponible para su '),
                   HTML('&nbsp;'),
                   tags$a(" descarga.", id = "descarga_cod",
                          
                          href="https://github.com/Ecofeminita/shinyapp"
                          
                   )),
         
         tags$div(   id = "invita",
                     tags$p('Obviamente, estás invitade a utilizar estos datos en notas, reflexiones, trabajos, infografías, etc.')
                     ),
                   
         tags$div( style="display: inline-flex;",id = "link_informes",
                   tags$p(' También podés consultar los'),
                   
                   HTML('&nbsp;'),
                   tags$a(" informes", id = "informes_ecofemidata",
                          
                          href="https://ecofeminita.com/ecofemidata/"
                          
                   ),
                   HTML('&nbsp;'),
                   tags$p('elaborados por el equipo de ecofemidata en base a los datos publicados cada trimestre.'))
                   
                   #acá cierra el fluid
                  )))
  
         
         #esto cierra el tab
)