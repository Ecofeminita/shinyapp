library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(gt)
library(markdown)



about_ui <- tabPanel(title = 'Inicio',
                     #includeMarkdown('README.md')
                     
                     titlePanel('EcofemiData'),
                     
                     br(),
                   
                     fluidRow(
                       column(12,
                              tags$div(style = "display:flex",
                             
                             
                                     tags$div( class="panel panel-primary",
                                               style = "border-color: #e2616e;width:50%",
                                               
                                               a(tags$div(class= "panel-heading",
                                                        style="background:#e2616e;border-color: #e2616e",
                                                        h3('Mercado de Trabajo')),
                                                 onclick="fakeClick('Tasas básicas')",
                                                 style ="text-decoration: none !important;"
                                               )
                                               ,
                                               
                                               tags$div(class="panel-body", style="display: flex;",
                                                        
                                                        a(img(height = 100, width = 100,src = "img/Iconosapp-01.png", style="flex: 0 0 15%;")
                                                          ,  onclick="fakeClick('Tasas básicas')")
                                                          ,
                                                        
                                                        a(p('En esta sección presentamos las tasas básicas del mercado de trabajo desagregadas por sexo, así como también por grupo etario. Por otro lado, estudiamos las condiciones de inserción laboral de las mujeres y los varones a partir de su jerarquía y rama de actividad.', style="text-align: left;font-size:18px;")
                                                          ,  onclick="fakeClick('Tasas básicas')",
                                                          style ="text-decoration: none !important;color: black !important;")
                                                        
                                                   
                                               ),tags$div(class="panel-body", style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                 tags$a("Tasas básicas",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;font-size:14px"),
                                                        onclick="fakeClick('Tasas básicas')",
                                                        class="btn btn-primary btn-s")),
                                               
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                 tags$a("Tasas básicas por grupos de edad",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;font-size:14px"),
                                                        onclick="fakeClick('Por grupos de edad')",
                                                        class="btn btn-primary btn-s"))
                                                 
                                               ,tags$div(class="panel-body", style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                 
                                                 tags$a("Tipo de inserción laboral",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;font-size:14px"),
                                                        onclick="fakeClick('Tipo de inserción laboral')",
                                                        class="btn btn-primary btn-s")),
                                               
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                 tags$a("Ramas de la actividad",style=paste0(btn_style,"background:#e2616e;border-color: #e2616e;font-size:14px"),
                                                        onclick="fakeClick('Ramas de la actividad')",
                                                        class="btn btn-primary btn-s")
                                                 
                                                 
                                               )
                                               
                                   
                                    
                              ),  
                              
                              #espacio entre cajas
                              HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              HTML('&nbsp;'),
                              
                       
                                     
                                     tags$div( class="panel panel-primary",
                                               style = "border-color: #687aad;width:50%",
                                               
                                               
                                               a(tags$div( class= "panel-heading",
                                                         style="background:#687aad;border-color: #687aad",
                                                         h3('Ingresos')),
                                                 onclick="fakeClick('Brechas de ingresos - general')",
                                                 style ="text-decoration: none !important;"
                                               )
                                               ,
                                               
                                               tags$div(class="panel-body",style="display: flex;",
                                                        a(img(height = 100, width = 100,src = "img/Iconosapp-02.png", style="flex: 0 0 15%;"),
                                                          onclick="fakeClick('Brechas de ingresos - general')"
                                                          ),
                                                        
                                                        a(p('En esta sección estudiamos las brechas de ingresos entre mujeres y varones, así como también la distribución de la población de cada sexo entre los distintos deciles de ingreso.', style="text-align: left;font-size:18px")
                                                          ,  onclick="fakeClick('Brechas de ingresos - general')",
                                                          style ="text-decoration: none !important;color: black !important;")
                                                        
                                               ),tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Brechas de ingresos - general", style=paste0(btn_style,"background:#687aad;border-color: #687aad;font-size:14px"),
                                                               onclick="fakeClick('Brechas de ingresos - general')",
                                                               class="btn btn-primary btn-s")),
                                               
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Brechas de ingresos - desagregado", style=paste0(btn_style,"background:#687aad;border-color: #687aad;font-size:14px"),
                                                               onclick="fakeClick('Brechas de ingresos - desagregado')",
                                                               class="btn btn-primary btn-s")
                                               ),
                                               
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Deciles de ingreso", style=paste0(btn_style,"background:#687aad;border-color: #687aad;font-size:14px"),
                                                               onclick="fakeClick('Deciles de ingreso')",
                                                               class="btn btn-primary btn-s")
                                               )
                                     )
                            #  )
                             
                              
                       )
                       )
                     ),
                     
                     
                     fluidRow(
                       column(12,
                              tags$div(style = "display:flex",
                             
                                     tags$div( class="panel panel-primary",
                                               style = "border-color: #e7bfce;width:50%",
                                               
                                               a(tags$div(class= "panel-heading",
                                                        style="background:#e7bfce;border-color: #e7bfce",
                                                        h3('Uso del tiempo')),
                                                 onclick="fakeClick('Trabajo remunerado')",
                                                 style ="text-decoration: none !important;"
                                               )
                                                 ,
                                               
                                               
                                               tags$div(class="panel-body",style="display: flex;",
                                                        
                                                        
                                                        a(img(height = 100, width = 100,src = "img/Iconosapp-03.png", style="flex: 0 0 15%;"),
                                                          onclick="fakeClick('Trabajo remunerado')"
                                                          ),                                                        
                                                       
                                                         a(p('En esta sección exploramos el uso del tiempo de las personas de cada sexo, ¿cuánto tiempo se dedican al trabajo remunerado? ¿y al trabajo no remunerado?', style="text-align: left;font-size:18px;")
                                                           ,  onclick="fakeClick('Trabajo remunerado')",
                                                           style ="text-decoration: none !important;color: black !important;")
                                                        
                                               ),tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Trabajo remunerado",style=paste0(btn_style,"background:#e7bfce;border-color: #e7bfce;color: black;font-size:14px"),
                                                               onclick="fakeClick('Trabajo remunerado')",
                                                               class="btn btn-primary btn-s")),
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Trabajo no remunerado",style=paste0(btn_style,"background:#e7bfce;border-color: #e7bfce;color: black;font-size:14px"),
                                                               onclick="fakeClick('Trabajo no remunerado')",
                                                               class="btn btn-primary btn-s")
                                               )
                                    
                              ),
                              #espacio entre cajas
                             HTML('&nbsp;'),
                             HTML('&nbsp;'),
                             HTML('&nbsp;'),
                            
                                     tags$div( class="panel panel-primary",
                                               style = "border-color: #8adbd1;width:50%",
                                               
                                               
                                               a(tags$div( class= "panel-heading",
                                                         style="background:#8adbd1;border-color: #8adbd1",
                                                         h3('Trabajadoras de Casas Particulares')),
                                                 onclick="fakeClick('Ocupadas en el servicio doméstico')",
                                                 style ="text-decoration: none !important;"
                                               )
                                               ,
                                               
                                               tags$div(class="panel-body",style="display: flex;",
                                                        a(img(height = 100, width = 100,src = "img/Iconosapp-04.png", style="flex: 0 0 15%;"),
                                                          onclick="fakeClick('Ocupadas en el servicio doméstico')"
                                                          ),
                                                        a(p('En esta sección exploramos las principales características de las condiciones de inserción laboral de las trabajadoras de Casas Particulares.', style="text-align: left;font-size:18px;")
                                                          ,  onclick="fakeClick('Ocupadas en el servicio doméstico')",
                                                          style ="text-decoration: none !important;color: black !important;")
                                                        
                                               ),tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Ocupadas en el servicio doméstico", style=paste0(btn_style,"background:#8adbd1;border-color: #8adbd1;color: black;font-size:14px"),
                                                               onclick="fakeClick('Ocupadas en el servicio doméstico')",
                                                               class="btn btn-primary btn-s")),
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Ingresos y tasa de feminización", style=paste0(btn_style,"background:#8adbd1;border-color: #8adbd1;color: black;font-size:14px"),
                                                               onclick="fakeClick('Ingresos y tasa de feminización')",
                                                               class="btn btn-primary btn-s")
                                               ),
                                               tags$div(class="panel-body",  style = "display: flex;padding: 1px 0;justify-content: flex-end",
                                                        tags$a("Derechos laborales", style=paste0(btn_style,"background:#8adbd1;border-color: #8adbd1;color: black;font-size:14px"),
                                                               onclick="fakeClick('Derechos laborales')",
                                                               class="btn btn-primary btn-s")
                                               )
                                     )
                              )
                      # )
                       
                       )
                      
                     )
                    
                     ,
                     br(),
                     br(),
                     tags$div( style="display: inline-flex;",  id = "intro_app",
                               tags$p('Los datos presentados en esta app pertenecen a '),
                               
                               HTML('&nbsp;'),
                               tags$a(" ecofeminita", id = "pagina_lnk",
                                      
                                      
                                      href="https://ecofeminita.com/"
                                      
                               )
                     )
                     
                     
                     
                     
                     
                     
                     
                     
                     
                     )




main_ui <- {
  
 
  
  navbarPage('ECOFEMIDATA',
             
            
             about_ui,
            
          
             
             navbarMenu(title = 'Mercado de Trabajo',
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
             tasas_sexo_ui('tasas_sexo'),
             tasas_edad_ui('tasas_edad'),
             tipo_insercion_ui('jerarquias'),
             ramas_ui('ramas')
             ),
             
             
           
             
             
             navbarMenu(title = 'Ingresos',
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
             brechas_ui('brechas_general'),
             brechas_desag_ui('brechas_desag'),
             deciles_ui('deciles')

             ),
             
            
             
             navbarMenu(title = 'Uso del tiempo',
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        #remunerado
                        horas_remunerado_ui('horas_remuneradas'),
                        #no remunerado
                        horas_no_remunerado_ui('horas_no_rem')

             ),
             
       
             navbarMenu(title = 'Trabajadoras de Casas Particulares',
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        #principales indicadores del informe
                        serv_dom_ocupadas_ui('s_d_ocup'),
                        serv_dom_ing_ui('s_d_ing'),
                        serv_dom_derechos_ui('s_d_derechos')


             ),


             
             metodologia_ui,
             
             # footer=tags$a("Volver", style=btn_style, id = "a_ecofem",
             #        href="https://ecofeminita.com/",
             #        class="btn btn-primary btn-s"
             #        
             # )
             
             )
}
