# Tablero de información actualizada de Ecofeminita


Podemos crear todos los archivos .md que querramos para redactar texto. Luego, con `tabPanel(title = 'about', includeMarkdown('README.md'))` creamos un tab de shiny donde se muestra el contenido renderizado. 

Mientras estamos desarrollando la app, podemos aprovechar este espacio para tomar nota de algunas cosas.


La shiny esta distribuida en varias carpetas y archivos. 

- app.R
- prepare_data.R
- data.Rdata
- R/
  - ui_main.R
  - modulos.R

`app.R` es el archivo principal, de dónde se ejecuta la app. Tiene el `ui` y el `server`. Por lo general no necesitamos modificarlo, excepto cuando queremos agregar un nuevo modulo al server.

`prepare_data.R` sirve para agregar todo el preprocesamiento de datos. Es un archivo que no corre la shiny app, sino que lo corremos nosotres offline, cuando diseñamos. El output es `data.Rdata`, que contiene las tablas de resultados que utilizamos para graficar. A tener en cuenta:
1. Cuanto más preprocesamiento hagamos en este archivo, más liviana va a ser `data.Rdata`, y más rápida va a ser la app. 
2. Cuando corramos este archivo, es muy importante **reiniciar R**, porque todo, incluyendo los paquetes que tengamos cargados, se guarda en el Rdata. 

`R/ui_main` es el archivo dónde cargamos la parte de UI de cada módulo.

e `R/` tambié agregamos un script por cada módulo que queremos sumar. Cada módulo esta compuesto de un ui y un server. La parte de ui se carga en ui_main, y la parte de server se carga en app.R. Ambas partes se conectan mediante u *namespace*

Agregar un módulo suele implicar las siguientes etapas:

1. Agregar el preprocesamiento en `prepare_data.R` y correr el script
2. agregar el script del módulo en R/. Lo mejor es copiar y pegar algún módulo anterior (podemos conservar module_example para eso)
3. Agregar la parte UI en ui_main.R
4. Agregar la parte del server en app.R
