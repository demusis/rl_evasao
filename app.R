library(shiny)
library(bestglm)
library(boot)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(caret)
library(shinybusy)
library(pROC)
library(purrr)
library(DT)

# Interface do usuário
ui <- fluidPage(
  titlePanel("Análise de regressão logística com seleção stepwise e validação bootstrap"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Escolha o arquivo XLSX", accept = c(".xlsx")),
      checkboxInput("include_bootstrap", "Efetuar validação bootstrap", value = FALSE),
      radioButtons("criterio", "Critério de seleção Stepwise", choices = c("AIC", "BIC"), selected = "AIC"),
      radioButtons("direcao", "Direção", choices = c("both", "backward", "forward"), selected = "both"),
      sliderInput("proporcao_treino", "Particionemento para treino", min = 50, max = 95, value = 80),
      sliderInput("slider1", "Repetições Bootstrap", min = 5, max = 10000, value = 100),
      actionButton("analyze", "Analisar")
    ),
    
    mainPanel(
      add_busy_spinner(spin = "fading-circle"),
      tabsetPanel(
        tabPanel("Modelos Stepwise (ass.)", verbatimTextOutput("stepwise")),
        tabPanel(
          "LogLik e Critérios de Informação (ass.)",
          verbatimTextOutput("criteria")
        ),
        
        tabPanel(
          "Gráficos (ass.)",
          plotOutput("residualsPlot"),
          plotOutput("qqPlot"),
          plotOutput("scaleLocationPlot"),
          plotOutput("cooksDistancePlot"),
          plotOutput("rocPlot"),
          h3("Gráfico de Tornado"),
          plotOutput("tornadoPlot")
        ),
        
        tabPanel("Bootstrap", 
                 h4("Estatísticas"),
                 DTOutput("boot"),
                 h4("Odds ratio"),
                 DTOutput("bootOR"))
        
      )
    )
  )
)

# Função do servidor
server <- function(input, output) {
  observeEvent(input$analyze, {
    req(input$file1)
    
    show_modal_spinner()
    
    # Carregar a planilha
    caminho_arquivo <- input$file1$datapath
    dados <- as.data.frame(read_excel(caminho_arquivo))
    dados[, 1:24] <- scale(dados[, 1:24])
    # Selecionar apenas as colunas numéricas
    dados_numericos <- dados %>% select_if(is.numeric)
    
    # Substituir NA por zero
    dados_numericos <- dados_numericos %>% mutate_all( ~ replace(., is.na(.), 0))
    # Certifique-se de que esta coluna seja binária
    variavel_resposta <- dados_numericos[, "evadido"]
    
    # Modelo com todas as covariáveis
    ajuste <- glm(evadido ~ ., data = dados_numericos, family = "binomial")
    
    # Modelo nulo (apenas com o intercepto)
    ajuste_nulo <- glm(evadido ~ 1, data = dados_numericos, family = "binomial")
    
    
    if (input$criterio=="AIC") {
      sfcriterio <- step(ajuste_nulo,
                         scope = formula(ajuste),
                         direction = input$direcao)
      fcriterio <<- sfcriterio$formula
      modelo_final_criterio <- formula(sfcriterio)
      vf_criterio <- all.vars(modelo_final_criterio)[-1]
    } else {
      
      sfcriterio <- step(
        ajuste_nulo,
        scope = formula(ajuste),
        direction = input$direcao,
        k = log(nrow(dados_numericos))
      )
      fcriterio <<- sfcriterio$formula
      modelo_final_criterio <- formula(sfcriterio)
      vf_criterio <- all.vars(modelo_final_criterio)[-1]
    }
    
    # --------------------------------------------------------------------------		
    
    calcular_ic <- function(df) {
      
      # Função auxiliar para calcular as estatísticas desejadas
      calcular_valores <- function(coluna) {
        # Remover valores NA
        coluna <- na.omit(coluna)
        
        # Calcula o primeiro e terceiro quartis
        Q1 <- quantile(coluna, 0.25)
        Q3 <- quantile(coluna, 0.75)
        
        # Calcula o intervalo interquartil (IQR)
        IQR <- Q3 - Q1
        
        # Define os limites para determinar outliers
        limite_inferior <- Q1 - 1.5 * IQR
        limite_superior <- Q3 + 1.5 * IQR
        
        # Filtra o dataframe para remover os outliers
        df_limpo <- coluna[coluna >= limite_inferior & coluna <= limite_superior]
        coluna <- df_limpo
        
        
        # Calcular as estatísticas
        media <- round(mean(coluna), digits = 3)
        q2.5 <- round(quantile(coluna, 0.025), digits = 3)
        q97.5 <- round(quantile(coluna, 0.975), digits = 3)
        n_validos <- length(coluna)
        
        # Retornar um data frame com as estatísticas
        return(data.frame(Media = media, `2.5%` = q2.5, `97.5%` = q97.5, `N válidos` = n_validos))
      }
      
      # Aplicar a função auxiliar a cada coluna do dataframe e combinar os resultados
      resultado <- map_dfr(df, calcular_valores, .id = "Variável")
      
      return(resultado)
    }
    
    
    
    # --------------------------------------------------------------------------		
    
    # Função para calcular as métricas de avaliação
    calcular_metricas <- function(modelo, dados, resposta) {
      pred_prob <- predict(modelo, type = "response")
      pred <- ifelse(pred_prob > 0.5, 1, 0)
      
      cm <- confusionMatrix(factor(pred), factor(resposta))
      
      metrics <- data.frame(
        Accuracy = cm$overall["Accuracy"],
        Sensitivity = cm$byClass["Sensitivity"],
        Specificity = cm$byClass["Specificity"],
        VPP = cm$byClass["Pos Pred Value"],
        VPN = cm$byClass["Neg Pred Value"]
      )
      
      return(metrics)
    }
    
    
    # Função para ajustar modelos e calcular métricas de avaliação
    ajustar_modelos <- function(dados_treino, dados_teste) {
      # Ajustar o modelo
      sfcriterio <- glm(fcriterio, data = dados_treino, family = "binomial")
      
      # Avaliação do modelo no conjunto de teste
      # Predições
      predicoes <- predict(sfcriterio, newdata = dados_teste, type = "response")
      pred_class <- ifelse(predicoes > 0.5, 1, 0)
      conf_matrix <- confusionMatrix(factor(pred_class), factor(dados_teste$evadido))
      
      # Coletar coeficientes e odds ratios
      coef <- coef(sfcriterio)
      odds_ratios <- exp(coef)
      
      # Métricas de avaliação
      metrics <- data.frame(
        Accuracy = conf_matrix$overall['Accuracy'],
        Sensitivity = conf_matrix$byClass['Sensitivity'],
        Specificity = conf_matrix$byClass['Specificity']
      )
      
      list(
        metrics = metrics,
        coef = coef,
        odds_ratios = odds_ratios
      )
    }
    
    # Função para aplicar o bootstrap
    bootstrap_modelos <- function(dados, n_bootstrap) {
      resultados <- replicate(n_bootstrap, {
        num_individuos <- nrow(dados)
        
        # Número de indivíduos no conjunto de treino
        num_treino <- round(input$proporcao_treino * num_individuos/100)
        
        # Amostra aleatória, sem reposição, de índices para o conjunto de treino
        indices_treino <- sample(num_individuos, num_treino, replace = FALSE)
        
        # Indivíduos restantes para o conjunto de teste
        indices_teste <- setdiff(1:num_individuos, indices_treino)
        
        # Amostra bootstrap
        indices_bootstrap_treino <- sample(num_treino, num_treino, replace = TRUE)
        indices_bootstrap_teste <- sample(num_individuos-num_treino, 
                                          num_individuos-num_treino, 
                                          replace = TRUE)
        
        dados_treino <- dados[indices_treino[indices_bootstrap_treino], ]
        dados_teste <- dados[indices_teste[indices_bootstrap_teste], ]
        
        # Ajustar modelos e calcular métricas
        ajustar_modelos(dados_treino, dados_teste)
      }, simplify = FALSE)
      
      # Coletar resultados
      metrics <- do.call(rbind, lapply(resultados, function(x)
        x$metrics))
      coef <- do.call(rbind, lapply(resultados, function(x)
        x$coef))
      odds_ratios <- do.call(rbind, lapply(resultados, function(x)
        x$odds_ratios))
      
      list(
        metrics = metrics,
        coef = coef,
        odds_ratios = odds_ratios
      )
    }
    
    if (input$include_bootstrap) {
      resultados_bootstrap <<- bootstrap_modelos(dados_numericos, n_bootstrap = input$slider1)
    } 
    
    
    # --------------------------------------------------------------------------
    
    # Exibir os resumos dos modelos
    output$boot <- renderDT({
      if (input$include_bootstrap) {
        
        dfBoot <- resultados_bootstrap$metrics
        rownames(dfBoot) <- NULL
        dfBoot <- as.data.frame(dfBoot)
        datatable(calcular_ic(dfBoot), rownames = FALSE, 
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                    )
                  )
        )}
    })
    
    output$bootOR <- renderDT({
      if (input$include_bootstrap) {
        
        
        dfBootOR <- resultados_bootstrap$odds_ratios
        rownames(dfBootOR) <- NULL
        dfBootOR <- as.data.frame(dfBootOR)
        auxc <- calcular_ic(dfBootOR)
        datatable(auxc, rownames = FALSE, 
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    language = list(
                      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                    )
                  )
        )}
    })
    
    
    
    
    # --------------------------------------------------------------------------
    
    # Exibir o resumo do modelo
    output$summary <- renderPrint({
      summary(sfcriterio)
    })
    
    # Salvar gráficos
    output$residualsPlot <- renderPlot({
      plot(sfcriterio, which = 1)
    })
    output$qqPlot <- renderPlot({
      plot(sfcriterio, which = 2)
    })
    output$scaleLocationPlot <- renderPlot({
      plot(sfcriterio, which = 3)
    })
    output$cooksDistancePlot <- renderPlot({
      plot(sfcriterio, which = 4)
    })
    
    output$tornadoPlot <- renderPlot({
      coeficientes <- summary(sfcriterio)$coefficients
      coeficientes_df <- as.data.frame(coeficientes)
      coeficientes_df <- coeficientes_df[-1, ]
      coeficientes_df$Variable <- rownames(coeficientes_df)
      coeficientes_df <- coeficientes_df %>%
        arrange(Estimate) %>%
        mutate(Variable = factor(Variable, levels = Variable))
      
      ggplot(coeficientes_df, aes(x = Variable, y = Estimate)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Gráfico de tornado", x = "Variável", y = "Estimativa dos coeficientes")
    })
    
    # Salvar e exibir a curva ROC
    output$rocPlot <- renderPlot({
      prob <- predict(sfcriterio, type = "response")
      roc <- roc(variavel_resposta, prob)
      plot(roc, main = "Curva ROC")
    })
    
    
    # --------------------------------------------------------------------------
    
    # Exibir LogLik e Critérios de Informação
    output$criteria <- renderPrint({
      list(
        LogLik_Stepwise = logLik(sfcriterio),
        AIC_Stepwise = AIC(sfcriterio),
        BIC_Stepwise = BIC(sfcriterio),
        
        LogLik_Nulo = logLik(ajuste_nulo),
        AIC_Nulo = AIC(ajuste_nulo),
        BIC_Nulo = BIC(ajuste_nulo)
      )
    })
    
    # --------------------------------------------------------------------------
    
    # Exibir Modelos Stepwise
    output$stepwise <- renderPrint({
      fcriterio
    })
    
    
    remove_modal_spinner()
  })
}

# Executar a aplicação
shinyApp(ui = ui, server = server)