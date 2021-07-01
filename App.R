

# Leitura dos Pacotes ----------------------------------------------------------

rm(list=ls())
options(OutDec = ',')

library(tidyverse)
library(readxl)
library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(gt)
library(highcharter)
library(DT)



# Carregando Modulos -----------------------------------------------------------

source('modulos/Analises2.R', encoding = 'UTF-8')
source('modulos/Modelo.R', encoding = 'UTF-8')



# Carregando os dados ----------------------------------------------------------


header <- uiOutput('')

sidebar <- uiOutput('main_sidebar')

body <- dashboardBody(
  uiOutput('main_body')
)


ui <- dashboardPage(
  dashboardHeader(title = 'Dashboard Analytics'),
  sidebar,
  body
)


# Servidor ####################################################################

server <- function(input, output, session) {
  
  output$main_sidebar <- renderUI({
    
    dashboardSidebar(
      
      sidebarUserPanel(name = a(href = 'http://estatidados.com.br/', 'EstaTiDados'),
                       # O arquivo de imagem deve estar em www/
                       image = 'EstaTiDados.jpg'
      ),
      
      sidebarMenu(
        id = 'tabs',
        menuItem("Sobre os Dados", tabName = "sobre_dados", icon = icon("align-justify")),
        menuItem('Machine Learning', tabName = 'modelo', icon = icon('chart-bar')),
        menuItem('Gráficos Interativos', tabName = 'analise2', icon = icon('user-friends'))
      )
    )
  })
    
  
  
  
# Body -------------------------------------------------------------------------
  
  output$main_body <- renderUI({
    tabItems(
      tabItem(
        tabName = 'sobre_dados',  
        class = 'active',
        h1(class = 'title-header', 'Sobre o banco de dados'),
        h4('Vou utilizar o banco de dados encontrado no', a('kaggle', href = 'https://www.kaggle.com/andrewmvd/heart-failure-clinical-data'),
          'que contém 12 características clínicas para previção de mortalidade por insuficiência cardíaca.'),
        h3('O banco de dados está composto pelas variáveis:'),
        p(strong('age'), '= Idade', 
          br(), strong('anaemia'), ' = Se possui anemia', 
          br(), strong('creatinine_phosphokinase'), ' = Nível da enzima CPK no sangue (mcg / L)', 
          br(), strong('diabetes'), ' = Se possui diabetes', 
          br(), strong('ejection_fraction'), ' = Porcentagem de sangue saindo do coração a cada contração (porcentagem)', 
          br(), strong('high_blood_pressure'), ' = Se possui pressão alta', 
          br(), strong('platelets'), ' = Plaquetas no sangue (quiloplacas / mL)', 
          br(), strong('serum_creatinine'), ' = Nível de creatinina sérica no sangue (mg / dL)', 
          br(), strong('serum_sodium'), ' = Nível de sódio sérico no sangue (mEq / L)', 
          br(), strong('sex'), ' = Sexo - 1 = Masculino e 0 - Feminino', 
          br(), strong('smoking'), ' = Se é fumante', 
          br(), strong('DEATH_EVENT'), ' = Se morreu'),
        br(),
        img(src = 'tags.png', align = "center")
      ),
      modelo_ui(id = 'modelo'),
      analise2_ui(id = 'analise2')
    )
  })
  

  
  
  ## Modulos -------------------------------------------------------------------
  
  callModule(
    module = modelo_server,
    id = 'modelo'
  )
  
  callModule(
    module = analise2_server,
    id = 'analise2'
  )

}


shinyApp(ui = ui, server = server)
