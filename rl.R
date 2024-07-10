# Carregar as bibliotecas necessárias
library(bestglm)
library(readxl)
library(dplyr)
library(tidyr)

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

# Função para salvar o resumo do modelo em arquivos CSV
salvar_resumo_modelo <- function(modelo, nome_arquivo, metodo) {
  coeficientes <- summary(modelo)$coefficients
  variaveis_significativas <- coeficientes[coeficientes[,4] < 0.05,]
  
  # Criar um data frame para as estatísticas do modelo
  estatisticas <- data.frame(
    LogLikelihood = as.numeric(logLik(modelo)),
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    Metodo = metodo
  )
  
  # Salvar estatísticas do modelo
  write.csv(estatisticas, paste0(nome_arquivo, "_estatisticas.csv"), row.names = FALSE)
  
  # Salvar todos os coeficientes
  write.csv(coeficientes, paste0(nome_arquivo, "_coeficientes.csv"))
  
  # Salvar apenas as variáveis significativas
  write.csv(variaveis_significativas, paste0(nome_arquivo, "_significativas.csv"))
}

# Salvar os modelos sfaic e sfbic
salvar_resumo_modelo(sfaic, "sfaic_model", "AIC")
salvar_resumo_modelo(sfbic, "sfbic_model", "BIC")

cat("Modelos salvos com sucesso nos arquivos sfaic_model_coeficientes.csv, sfaic_model_significativas.csv, sfaic_model_estatisticas.csv, sfbic_model_coeficientes.csv, sfbic_model_significativas.csv e sfbic_model_estatisticas.csv")
