


options(OutDec = ',')

##### Pacotes ------------------------------------------------------------------

if(!require(tidyverse)){install.packages('tidyverse'); require(tidyverse)}
if(!require(highcharter)){install.packages(highcharter);require(highcharter)}
if(!require(plotly)){install.packages('plotly'); require(plotly)}



##### Leitura e manipulação dos Dados ------------------------------------------


Dados2 <- reactive({
  
  df <- mtcars %>% 
    mutate(
      Cambio = case_when(
        am == 1 ~ 'Automático',
        am == 0 ~ 'Manual',
        
      ))
  
  return(df)
  
})

# isolate(Dados2())


##### shiny --------------------------------------------------------------------

analise2_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = 'analise2',
    h2(
      style='color: #1E2733; font-size: 25px;',
      align = 'center',
      'Análises com Highchart e Plotly'
    ),
    br(),
    fluidRow(
      box(width = 4,
          status = 'primary',
          selectInput(inputId = ns('cambio'),
                      label = 'Selecione a câmbio', 
                      choices = c('Automático',   'Manual', 'Todos'))
      ),
      box(width = 8,
          status = 'primary',
          title = 'Gráfico da média de potência por número de marchas', 
          highchartOutput(ns('cambio_plot'))
      ),
    ),
    fluidRow(
      box(width = 6,
          status = 'primary',
          radioButtons(
            inputId = ns('cilindros'),
            label = 'Selecione o número de cilindros',
            choices = c(Dados2() %>%
                          pull(cyl) %>%
                          unique(), 'Todos')),
          title = 'Gráfico da média de milhas/galão por número de marchas',
          plotlyOutput(ns('cilindros_plotly'))
      ),
      box(width = 6,
          status = 'primary',
          title = 'Gráfico da média de milhas/galão por número de marchas', 
          plotlyOutput(ns('cilindros_ggplot'))
      ),
    )
  )
}


analise2_server <- function(input, output, session) {
  
  tabela_plot <- reactive({
    
    req(input$cambio)
    req(input$cilindros)
    
    if (input$cambio == 'Todos') {
      tabela_plot1 <- df %>%
        group_by(gear) %>% 
        summarise(Media = mean(mpg)) %>% 
        ungroup()
      
    } else {
      tabela_plot1 <- Dados2() %>% 
        filter(Cambio == input$cambio) %>% 
        group_by(gear) %>% 
        summarise(Media = mean(mpg)) %>% 
        ungroup()
    }
    
    
    if (input$cilindros == 'Todos') {
      tabela_plot2 <- Dados2() %>%
        group_by(gear) %>% 
        summarise(Media = mean(hp)) %>% 
        ungroup()
      
    } else {
      tabela_plot2 <- Dados2() %>% 
        filter(cyl == input$cilindros) %>% 
        group_by(gear) %>% 
        summarise(Media = mean(hp)) %>% 
        ungroup()
    }
    
    list(tabela_plot1 = tabela_plot1,
         tabela_plot2 = tabela_plot2)
    
  })
 
  
  output$cambio_plot <- renderHighchart({
    
    dados <- tabela_plot()[['tabela_plot1']] %>% 
      mutate(Media = round(Media, 2))
    
      highchart() %>% 
      hc_add_series(data = dados, 
                    type = 'column', 
                    hcaes(x = gear, 
                          y = Media)) %>% 
      hc_xAxis(title = list(text = 'Número de marchas')) %>% 
      hc_yAxis(title = list(text = 'Média da Potência')) %>% 
      hc_tooltip(pointFormat = "Número de marchas: <b> {point.x}</b><br>
                 Média da Potência: <b> {point.y}</b>") %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>% 
      hc_legend(NULL)
    
  })
  
  
  
  output$cilindros_plotly <- renderPlotly({
    
    dados2 <- tabela_plot1 %>% 
      mutate(Media = round(Media, 2),
             gear = as.factor(gear))
    
    fig <- plot_ly(dados2, 
                   x = ~gear, y = ~Media, 
                   type = 'bar', 
                   name = '')
    
    fig <- fig %>% layout(title = '',
                          xaxis = list(title = 'Número de marchas'),
                          yaxis = list(title = 'Milhas/galão'),
                          margin = list(b = 100),
                          barmode = 'group'
                          )
    
    fig
    
    
  })
  
  
  output$cilindros_ggplot <- renderPlotly({
    
    dados3 <- tabela_plot()[['tabela_plot2']] %>% 
      mutate(Media = round(Media, 2))
    
    ggplotly(ggplot(data = dados3,
                    aes(x = gear,
                        y = Media)) +
               geom_bar(fill = 'skyblue3', 
                        stat = 'identity') +
               xlab('Número de marchas') + ylab('Milhas/galão'))
    
  })

}