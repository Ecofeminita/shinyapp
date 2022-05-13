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
  
  navbarPage('titulo',
             sample_plot_ui('ejemplo'),
             about_ui
             )
}
