# Carregar as bibliotecas necessárias
library(bestglm)
library(boot)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(caret) # Para funções de avaliação de modelo

setwd("~/rl")
nome_arquivo <- "regressao_logistica"

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
faic <- sfaic$formula
sfbic <- step(ajuste_nulo, scope=formula(ajuste), direction="forward", k=log(nrow(dados_numericos)))
fbic <- sfbic$formula


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

# Função para ajustar modelos e calcular métricas de avaliação
ajustar_modelos <- function(dados_treino, dados_teste) {
  # Ajustar o modelo 
  sfaic <- glm(faic, data=dados_treino, family="binomial")
  sfbic <- glm(fbic, data=dados_treino, family="binomial")
  
  # Avaliação do modelo no conjunto de teste
  # Predições usando o modelo AIC
  predicoes_aic <- predict(sfaic, newdata = dados_teste, type="response")
  pred_class_aic <- ifelse(predicoes_aic > 0.5, 1, 0)
  conf_matrix_aic <- confusionMatrix(factor(pred_class_aic), factor(dados_teste$Evadiu))
  
  # Predições usando o modelo BIC
  predicoes_bic <- predict(sfbic, newdata = dados_teste, type="response")
  pred_class_bic <- ifelse(predicoes_bic > 0.5, 1, 0)
  conf_matrix_bic <- confusionMatrix(factor(pred_class_bic), factor(dados_teste$Evadiu))
  
  # Coletar coeficientes e odds ratios
  coef_aic <- coef(sfaic)
  coef_bic <- coef(sfbic)
  odds_ratios_aic <- exp(coef_aic)
  odds_ratios_bic <- exp(coef_bic)
  
  # Métricas de avaliação
  metrics_aic <- data.frame(
    Accuracy = conf_matrix_aic$overall['Accuracy'],
    Sensitivity = conf_matrix_aic$byClass['Sensitivity'],
    Specificity = conf_matrix_aic$byClass['Specificity']
  )
  
  metrics_bic <- data.frame(
    Accuracy = conf_matrix_bic$overall['Accuracy'],
    Sensitivity = conf_matrix_bic$byClass['Sensitivity'],
    Specificity = conf_matrix_bic$byClass['Specificity']
  )
  
  list(
    metrics_aic = metrics_aic, 
    metrics_bic = metrics_bic, 
    coef_aic = coef_aic, 
    coef_bic = coef_bic, 
    odds_ratios_aic = odds_ratios_aic, 
    odds_ratios_bic = odds_ratios_bic
  )
}

# Função para aplicar o bootstrap
bootstrap_modelos <- function(dados, n_bootstrap = 100) {
  resultados <- replicate(n_bootstrap, {
    # Amostra bootstrap
    indices_bootstrap <- sample(nrow(dados), replace = TRUE)
    dados_treino <- dados[indices_bootstrap, ]
    dados_teste <- dados[-unique(indices_bootstrap), ]
    
    # Ajustar modelos e calcular métricas
    ajustar_modelos(dados_treino, dados_teste)
  }, simplify = FALSE)
  
  # Coletar resultados
  metrics_aic <- do.call(rbind, lapply(resultados, function(x) x$metrics_aic))
  metrics_bic <- do.call(rbind, lapply(resultados, function(x) x$metrics_bic))
  coef_aic <- do.call(rbind, lapply(resultados, function(x) x$coef_aic))
  coef_bic <- do.call(rbind, lapply(resultados, function(x) x$coef_bic))
  odds_ratios_aic <- do.call(rbind, lapply(resultados, function(x) x$odds_ratios_aic))
  odds_ratios_bic <- do.call(rbind, lapply(resultados, function(x) x$odds_ratios_bic))
  
  list(
    metrics_aic = metrics_aic, 
    metrics_bic = metrics_bic, 
    coef_aic = coef_aic, 
    coef_bic = coef_bic, 
    odds_ratios_aic = odds_ratios_aic, 
    odds_ratios_bic = odds_ratios_bic
  )
}

# Medir o tempo de execução
tempo_execucao <- system.time({
  # Aplicar bootstrap nos modelos
  resultados_bootstrap <- bootstrap_modelos(dados_numericos, 
                                            n_bootstrap = 10)
})

# Calcular estatísticas de resumo
summary_metrics_aic <- summary(resultados_bootstrap$metrics_aic)
summary_metrics_bic <- summary(resultados_bootstrap$metrics_bic)

# Calcular intervalos de confiança não paramétricos
ci_nonparametric <- function(data, level = 0.95) {
  alpha <- 1 - level
  lower <- apply(data, 2, quantile, probs = alpha / 2, na.rm = TRUE)
  upper <- apply(data, 2, quantile, probs = 1 - alpha / 2, na.rm = TRUE)
  data.frame(lower = lower, upper = upper)
}

ci_coef_aic <- ci_nonparametric(resultados_bootstrap$coef_aic)
ci_coef_bic <- ci_nonparametric(resultados_bootstrap$coef_bic)
ci_odds_ratios_aic <- ci_nonparametric(resultados_bootstrap$odds_ratios_aic)
ci_odds_ratios_bic <- ci_nonparametric(resultados_bootstrap$odds_ratios_bic)

print("Resumo das métricas de avaliação para o modelo AIC:")
print(summary_metrics_aic)

print("Resumo das métricas de avaliação para o modelo BIC:")
print(summary_metrics_bic)

print("Intervalos de confiança para os coeficientes do modelo AIC:")
print(ci_coef_aic)

print("Intervalos de confiança para os coeficientes do modelo BIC:")
print(ci_coef_bic)

print("Intervalos de confiança para os odds ratios do modelo AIC:")
print(ci_odds_ratios_aic)

print("Intervalos de confiança para os odds ratios do modelo BIC:")
print(ci_odds_ratios_bic)

# Salvar os resultados em arquivos Excel
write.xlsx(list(
  SummaryMetrics_AIC = as.data.frame(summary_metrics_aic),
  SummaryMetrics_BIC = as.data.frame(summary_metrics_bic),
  Coef_CI_AIC = ci_coef_aic,
  Coef_CI_BIC = ci_coef_bic,
  OddsRatios_CI_AIC = ci_odds_ratios_aic,
  OddsRatios_CI_BIC = ci_odds_ratios_bic
), file = "resultados_modelos_bootstrap.xlsx")

# Imprimir o tempo de processamento
print("Tempo de processamento:")
print(tempo_execucao)

