# Carregar as bibliotecas necessárias
library(bestglm)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(caret) # Para funções de avaliação de modelo

setwd("~/rl")

# Carregar a planilha
caminho_arquivo <- "dados-dummy.xlsx" # Substitua pelo caminho correto do arquivo
dados <- as.data.frame(read_excel(caminho_arquivo))

# Selecionar apenas as colunas numéricas
dados_numericos <- dados %>% select_if(is.numeric)

# Substituir NA por zero
dados_numericos <- dados_numericos %>% mutate_all(~replace(., is.na(.), 0))

# Retirar variáveis desnecessárias
dados_numericos <- dados_numericos[,c(-1, -2)]

# Certifique-se de que esta coluna seja binária
variavel_resposta <- dados_numericos[,113]
unique(variavel_resposta)

# Modelo com todas as covariáveis
ajuste <- glm(Evadiu ~ ., data=dados_numericos, family="binomial")

summary(ajuste)
logLik(ajuste)
AIC(ajuste)
BIC(ajuste)

# Modelo nulo (apenas com o intercepto)
ajuste_nulo <- glm(Evadiu ~ 1, data=dados_numericos, family="binomial")

logLik(ajuste_nulo)
AIC(ajuste_nulo)
BIC(ajuste_nulo)

sfaic <- step(ajuste_nulo, scope=formula(ajuste), direction="forward")
sfbic <- step(ajuste_nulo, scope=formula(ajuste), direction="forward", k=log(nrow(dados_numericos)))
summary(sfbic)

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

# Função para salvar o resumo do modelo em arquivos Excel
salvar_resumo_modelo <- function(modelo, nome_arquivo, metodo, dados, resposta) {
  coeficientes <- summary(modelo)$coefficients
  variaveis_significativas <- coeficientes[coeficientes[,4] < 0.05,]
  
  # Calcular odds ratios e intervalos de confiança
  odds_ratios <- exp(coeficientes[,1])
  conf_int <- exp(confint(modelo))
  
  # Criar um data frame para odds ratios e intervalos de confiança
  odds_ratios_df <- data.frame(
    Estimate = coeficientes[,1],
    OR = odds_ratios,
    CI_lower = conf_int[,1],
    CI_upper = conf_int[,2]
  )
  rownames(odds_ratios_df) <- rownames(coeficientes)
  
  # Calcular métricas de avaliação
  metrics <- calcular_metricas(modelo, dados, resposta)
  
  # Criar um data frame para as estatísticas do modelo
  estatisticas <- data.frame(
    LogLikelihood = as.numeric(logLik(modelo)),
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    Metodo = metodo
  )
  estatisticas <- cbind(estatisticas, metrics)
  
  # Criar um workbook e adicionar as folhas
  wb <- createWorkbook()
  addWorksheet(wb, "Estatisticas")
  addWorksheet(wb, "Coeficientes")
  addWorksheet(wb, "Significativas")
  addWorksheet(wb, "Odds Ratios")
  
  # Escrever os data frames nas folhas correspondentes
  writeData(wb, sheet = "Estatisticas", estatisticas)
  writeData(wb, sheet = "Coeficientes", coeficientes, rowNames = TRUE)
  writeData(wb, sheet = "Significativas", variaveis_significativas, rowNames = TRUE)
  writeData(wb, sheet = "Odds Ratios", odds_ratios_df, rowNames = TRUE)
  
  # Salvar o arquivo Excel
  saveWorkbook(wb, paste0(nome_arquivo, ".xlsx"), overwrite = TRUE)
}

# Função para salvar os gráficos padrão do modelo
salvar_graficos_padrao <- function(modelo, nome_base) {
  png(paste0(nome_base, "_residuals_vs_fitted.png"))
  plot(modelo, which = 1)
  dev.off()
  
  png(paste0(nome_base, "_qq_plot.png"))
  plot(modelo, which = 2)
  dev.off()
  
  png(paste0(nome_base, "_scale_location.png"))
  plot(modelo, which = 3)
  dev.off()
  
  png(paste0(nome_base, "_cooks_distance.png"))
  plot(modelo, which = 4)
  dev.off()
}

# Função para criar gráfico de tornado
criar_grafico_tornado <- function(modelo, nome_grafico) {
  coeficientes <- summary(modelo)$coefficients
  coeficientes_df <- as.data.frame(coeficientes)
  coeficientes_df$Variable <- rownames(coeficientes_df)
  coeficientes_df <- coeficientes_df %>% 
    arrange(Estimate) %>%
    mutate(Variable = factor(Variable, levels = Variable))
  
  ggplot(coeficientes_df, aes(x = Variable, y = Estimate)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Tornado Plot for", nome_grafico), 
         x = "Variable", 
         y = "Coefficient Estimate")
}

# Salvar os modelos sfaic e sfbic
salvar_resumo_modelo(sfaic, "sfaic_model", "AIC", dados_numericos, variavel_resposta)
salvar_resumo_modelo(sfbic, "sfbic_model", "BIC", dados_numericos, variavel_resposta)

# Criar e salvar os gráficos de tornado
tornado_sfaic <- criar_grafico_tornado(sfaic, "SFAIC Model")
tornado_sfbic <- criar_grafico_tornado(sfbic, "SFBIC Model")

ggsave("tornado_sfaic.png", plot = tornado_sfaic, width = 30, height = 8)
ggsave("tornado_sfbic.png", plot = tornado_sfbic, width = 30, height = 8)

# Salvar os gráficos padrão do modelo
salvar_graficos_padrao(sfaic, "sfaic_model")
salvar_graficos_padrao(sfbic, "sfbic_model")
