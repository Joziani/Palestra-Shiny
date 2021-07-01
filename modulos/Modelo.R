


options(OutDec = ',')

##### Pacotes ------------------------------------------------------------------

if(!require(tidyverse)){install.packages('tidyverse'); require(tidyverse)}
if(!require(gtsummary)){install.packages('gtsummary'); require(gtsummary)}
if(!require(caret)){install.packages('caret'); require(caret)} 
if(!require(ROCR)){install.packages('ROCR'); require(ROCR)}


##### Funções ------------------------------------------------------------------

basic <- function(x, more=F) {
  stats <- list()
  
  stats$N <- length(x)
  stats$Media <- round(mean(x),2)
  stats$D.P <- round(sd(x),2)
  
  t1 <- unlist(stats)
  names(t1) <- c("N", "Média", "D.P.")
  t1
}

metricas <- function(mod, mod.class, mod.probs, Teste){
  
  #predicting the test data
  mod.labels <- as.factor(Teste$DEATH_EVENT)
  mod.confusion <- confusionMatrix(mod.labels, mod.class)
  mod.accuracy <- round(as.numeric(mod.confusion$overall[1]),3)
  mod.sensitivity <- round(as.numeric(mod.confusion$byClass[1]),3)
  mod.specificity <- round(as.numeric(mod.confusion$byClass[2]),3)
  
  #roc analysis for test data
  mod.prediction <- prediction(mod.probs, mod.labels)
  mod.performance <- performance(mod.prediction,"tpr","fpr")
  mod.auc <- round(performance(mod.prediction,"auc")@y.values[[1]],3)
  
  tabela <- data.frame(Acurácia = mod.accuracy,
                       Sensibilidade  = mod.sensitivity,
                       Especificidade = mod.specificity,
                       AUC = mod.auc)
  
  return(list(medida=tabela, performace=mod.performance))
}



##### Leitura e manipulação dos Dados ------------------------------------------


Dados <- reactive({
  
  df <- readxl::read_xlsx('dados/Dados 1.xlsx')
  
  return(df)
  
})



##### shiny --------------------------------------------------------------------

modelo_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = 'modelo',
    h2(
      style="color: #1E2733; font-size: 25px;",
      align = 'center',
      'Análises de Machine Learning'
    ),
    fluidRow(
      box(
        width = 5,
        status = "primary",
        title = 'Descrição de variáveis categóricas', 
        gt::gt_output(ns('tabela_cat'))
      ),
      box(
        width = 7,
        status = "primary",
        title = 'Descrição de variáveis numéricas', 
        DT::dataTableOutput(ns('tabela_num'))
      )
    ),
    fluidRow(
      box(
        width = 12,
        status = "primary",
        title = 'Correlação entre variáveis', 
        plotOutput(ns('correlacao'))
      )
    ),
    fluidRow(
      box(
        status = "primary",
        title = 'Informações sobre os dados de treino e teste', 
        numericInput(
          ns('porc_treino'),
          'Porcentagem para treino',
          value = 70,
          step = 1,
          min = 70,
          max = 80,
          width = '100%'
        ),
        valueBoxOutput(ns('dim_treino_teste'), width = 12)
      ),
      box(
        width = 6,
        status = "primary",
        DT::dataTableOutput(ns('info_dados'))
      )
    ),
    fluidRow(
      box(
        width = 8,
        status = "primary",
        title = 'Tabela de comparação entre modelos',
        DT::dataTableOutput(ns('tabela_comp'))
      )
    ),
    fluidRow(
      box(
        width = 8,
        status = "primary",
        title = 'Curva ROC para os 3 modelos',
        plotOutput(ns('curva_roc'))
      )
    )
  )
}



modelo_server <- function(input, output, session) {
  
  
  treino_teste <- reactive({
    
    req(input$porc_treino)
    porc_treino <- as.numeric(input$porc_treino)
    
    set.seed(123)
    dim_treino <- round(porc_treino/100*(dim(Dados())[1]), 0)
    id_sample <-  1:(dim(Dados())[1]) %>% 
      sample(dim_treino, replace = F)
    
    Treino <- data.frame(Dados()[id_sample,]) 
    Teste <- data.frame(Dados()[-id_sample,]) 
    
    list(Treino = Treino,
         Teste = Teste)
    
  })
  
  output$tabela_cat <- gt::render_gt({
    
    tabela <- Dados() %>% 
      dplyr::select(DEATH_EVENT,
                    anaemia,
                    diabetes,
                    high_blood_pressure,
                    sex,
                    smoking) %>% 
      dplyr::mutate(
        DEATH_EVENT = case_when(
          DEATH_EVENT == 0 ~ 'Não',
          DEATH_EVENT == 1 ~ 'Sim'
        ),
        anaemia = case_when(
          anaemia == 0 ~ 'Não',
          anaemia == 1 ~ 'Sim'
        ),
        diabetes = case_when(
          diabetes == 0 ~ 'Não',
          diabetes == 1 ~ 'Sim'
        ),
        high_blood_pressure = case_when(
          high_blood_pressure == 0 ~ 'Não',
          high_blood_pressure == 1 ~ 'Sim'
        ),
        sex = case_when(
          sex == 0 ~ 'Feminino',
          sex == 1 ~ 'Masculino'
        ),
        smoking = case_when(
          smoking == 0 ~ 'Não',
          smoking == 1 ~ 'Sim'
        )
      )
    
    colnames(tabela) <- c('Se morreu',
                          'Anemia',
                          'Diabetes',
                          'Se possui pressão alta',
                          'Sexo',
                          'Se é fumante')
    
    tabela %>% 
      tbl_summary(
        type = all_categorical() ~ "categorical") %>% 
      modify_header(update =list(label  ~  "**Variáveis**"))%>% 
      bold_labels()  %>% as_gt()
    
  })
  
  output$tabela_num <- DT::renderDataTable({
    
    tabela <- t(apply(Dados() %>% dplyr::select(age,
                                           creatinine_phosphokinase,
                                           ejection_fraction,
                                           platelets,
                                           serum_creatinine,
                                           serum_sodium), 2, basic)) %>% 
      data.frame() %>% 
      transmute(Variáveis = c('Idade',
                              'Nível da enzima CPK no sangue',
                              'Porcentagem de sangue saindo do coração a cada contração',
                              'Plaquetas no sangue',
                              'Nível de creatinina sérica no sangue',
                              'Nível de sódio sérico no sangue'),
                N,
                Média,
                D.P.)
    
    tabela %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    escape = FALSE,
                    extensions = c('Buttons'), #, 'Scroller'),
                    options = 
                      list(pageLength = 20,
                           dom = 'Bdt',
                           buttons = c('excel', 'pdf'),
                           #scrollX = TRUE,
                           #scroller = TRUE,
                           autoWidth = TRUE,
                           #columnDefs = list(list(width = '170px', targets = "_all")),
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#1E2733', 'color': '#fff'});",
                             "}")
                      )
      )
  })
  
  
  
  output$correlacao <- renderPlot({
    
    corelacao <- cor(as.matrix(Dados()), method = 'spearman')
    p.mat <- ggcorrplot::cor_pmat(as.matrix(Dados()))
    
    ggcorrplot::ggcorrplot(corelacao, hc.order = TRUE, 
                           type = 'lower', 
                           lab = TRUE, 
                           lab_size = 3, 
                           method = 'circle', 
                           colors = c('tomato2', 'white', 'springgreen3'), 
                           title= 'Correlograma entre variáveis', 
                           p.mat = p.mat,
                           ggtheme = theme_bw)
    
  })
  
  
  output$info_dados <- DT::renderDataTable({
    
    var_zero_treino <- length(nearZeroVar(treino_teste()[['Treino']]))
    var_zero_teste <- length(nearZeroVar(treino_teste()[['Teste']]))
    
    comb_lineares_treino <- findLinearCombos(treino_teste()[['Treino']] %>% 
                                               dplyr::select(-DEATH_EVENT) %>% 
                                               data.matrix)
    comb_lineares_teste <- findLinearCombos(treino_teste()[['Teste']] %>% 
                                              dplyr::select(-DEATH_EVENT) %>% 
                                              data.matrix)
    
    tabela <- data.frame(Tipo = c('Preditores com variância zero - Treino',
                                  'Preditores com variância zero - Teste',
                                  'Quantidade de combinações lineares - Treino',
                                  'Quantidade de combinações lineares - Teste',
                                  'Variáveis com combinações lineares a serem retiradas - Treino',
                                  'Variáveis com combinações lineares a serem retiradas - Teste'),
                         Valores = c(var_zero_treino,
                                     var_zero_teste,
                                     length(comb_lineares_treino$remove),
                                     length(comb_lineares_teste$remove),
                                     paste0(unlist(comb_lineares_treino$linearCombos), 
                                            collapse = ','),
                                     paste0(unlist(comb_lineares_teste$linearCombos), 
                                            collapse = ',')))
    
    tabela %>% 
      DT::datatable(.,
                    rownames = FALSE,
                    escape = FALSE,
                    options = 
                      list(pageLength = 20,
                           dom = 'Bdt',
                           autoWidth = TRUE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#1E2733', 'color': '#fff'});",
                             "}")
                      )
      )
    
  })
  
  
  output$dim_treino_teste <- renderValueBox({
    
    dim_treino <-  dim(treino_teste()[['Treino']])[1]
    dim_teste <- dim(treino_teste()[['Teste']])[1]
    
    valueBox(value = paste0('Treino = ', dim_treino, '; ',
                            'Teste = ', dim_teste), 
             subtitle = 'Tamanho dos dados', 
             icon = icon("file-alt"), 
             color = "aqua")
    
  })
  
  
  regressao <- reactive({
    
    Treino <- treino_teste()[['Treino']]
    Teste <- treino_teste()[['Teste']]
    
    mod_reg <- glm(DEATH_EVENT ~ age + 
                     anaemia + 
                     creatinine_phosphokinase + 
                     diabetes + 
                     ejection_fraction + 
                     high_blood_pressure + 
                     platelets + 
                     serum_creatinine + 
                     serum_sodium + 
                     sex + 
                     smoking, 
                   family = binomial, 
                   data = Treino)
    
    cutoff_reg <- lapply(as.list(seq(0.3,0.7,0.01)),
                         function(x){
                           table(ifelse(predict(mod_reg, Treino, 
                                                type = 'response') >= x, 1, 0), 
                                 Treino$DEATH_EVENT) %>% 
                             prop.table() %>% 
                             diag() %>% 
                             sum()})
    
    ponto_corte_reg <- seq(0.3,0.7,0.01)[which.max(cutoff_reg)]
    
    mod.probs_reg <- predict(mod_reg, Teste, type = 'response')
    
    mod.class_reg <- as.factor(ifelse(mod.probs_reg >= ponto_corte_reg, 1, 0))
    
    medidas_reg <- metricas(mod_reg, mod.class_reg, mod.probs_reg, Teste)$medida
    performace_reg <- metricas(mod_reg, mod.class_reg, mod.probs_reg, Teste)$performace
    
    list(medidas_reg = medidas_reg,
         performace_reg = performace_reg)
    
  })
  
  
  random_forest <- reactive({
    
    Treino <- treino_teste()[['Treino']]
    Teste <- treino_teste()[['Teste']]
    
    set.seed(123)
    mod_rf <- randomForest::randomForest(DEATH_EVENT ~ age + 
                                           anaemia + 
                                           creatinine_phosphokinase + 
                                           diabetes + 
                                           ejection_fraction + 
                                           high_blood_pressure + 
                                           platelets + 
                                           serum_creatinine + 
                                           serum_sodium + 
                                           sex + 
                                           smoking, 
                                         data = Treino,
                                         importance = TRUE)
    
    cutoff_rf <-  lapply(as.list(seq(0.3,0.7,0.01)),
                         function(x){
                           table(ifelse(predict(mod_rf, Treino, 
                                                type = 'response')>=x, 1, 0), 
                                 Treino$DEATH_EVENT) %>% 
                             prop.table() %>% 
                             diag() %>% 
                             sum()})
    
    ponto_corte_rf <-  seq(0.3,0.7,0.01)[which.max(cutoff_rf)]
    
    mod.probs_rf <- predict(mod_rf, Teste, type = 'response')
    
    mod.class_rf <- as.factor(ifelse(mod.probs_rf >= ponto_corte_rf, 1, 0))
    
    medidas_rf <- metricas(mod_rf, mod.class_rf, mod.probs_rf, Teste)$medida
    performace_rf <- metricas(mod_rf, mod.class_rf, mod.probs_rf, Teste)$performace
    
    list(medidas_rf = medidas_rf,
         performace_rf = performace_rf)
    
  })
  
  
  svm <- reactive({
    
    Treino <- treino_teste()[['Treino']]
    Teste <- treino_teste()[['Teste']]
    
    mod_svm <-  e1071::svm(DEATH_EVENT ~ age + 
                             anaemia + 
                             creatinine_phosphokinase + 
                             diabetes + 
                             ejection_fraction + 
                             high_blood_pressure + 
                             platelets + 
                             serum_creatinine + 
                             serum_sodium + 
                             sex + 
                             smoking, 
                           data = Treino,
                           kernel = 'radial', 
                           type = 'C-classification',
                           cost = 10, gamma = 0.01, coef0 = 1,
                           probability = TRUE)
    
    
    mod.class_svm <- predict(mod_svm, Teste)
    mod.probs_svm <- attr(predict(mod_svm, Teste, probability = TRUE), 
                          'probabilities')[,2]
    
    medidas_svm <- metricas(mod_svm, mod.class_svm, mod.probs_svm, Teste)$medida
    performace_svm <- metricas(mod_svm, mod.class_svm, mod.probs_svm, Teste)$performace
    
    list(medidas_svm = medidas_svm,
         performace_svm = performace_svm)
    
  })
  
  
  
  output$tabela_comp <- DT::renderDataTable({

    medidas_tab <- data.frame(rbind(regressao()[['medidas_reg']],
                                    random_forest()[['medidas_rf']],
                                    svm()[['medidas_svm']]))
    
    rownames(medidas_tab) <- c('Regressão',
                               'Random Forest',
                               'SVM')

    medidas_tab = t(medidas_tab)

    medidas_tab %>%
      DT::datatable(.,
                    escape = FALSE,
                    extensions = c('Buttons'),
                    options =
                      list(language =
                             list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'),
                           pageLength = 20,
                           dom = 'Bdt',
                           buttons = c('excel', 'pdf'),
                           autoWidth = TRUE,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#1E2733', 'color': '#fff'});",
                             "}")
                      )
      )

  })
  
  
  output$curva_roc <- renderPlot({
    
    plot(regressao()[['performace_reg']], col = 'red', lwd = 2, 
         xlab = 'Taxa de falso positivo',
         ylab = 'Taxa de verdadeiro positivo')
    plot(random_forest()[['performace_rf']], add = TRUE, col = 'blue', lwd = 2)
    plot(svm()[['performace_svm']], add = TRUE, col = 'green', lwd = 2)
    title(main = '', font.main = 4)
    plot_range <- range(0,0.5,0.5)
    legend(0.5, plot_range[2], 
           c('Regressão Logística', 'Random Forest', 'SVM'), 
           cex = 0.8, col = c('red','green','blue'), pch = 21:22, lty = 1:2)
    
  })
  
}
