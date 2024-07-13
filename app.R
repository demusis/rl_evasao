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
					h3("AIC"),
					plotOutput("residualsPlot"),
					plotOutput("qqPlot"),
					plotOutput("scaleLocationPlot"),
					plotOutput("cooksDistancePlot"),
					plotOutput("rocPlotAIC"),
					# Adicionando plot para a curva ROC AIC
					h3("Gráfico de Tornado"),
					plotOutput("tornadoPlot"),
					h3("BIC"),
					plotOutput("residualsPlotBIC"),
					plotOutput("qqPlotBIC"),
					plotOutput("scaleLocationPlotBIC"),
					plotOutput("cooksDistancePlotBIC"),
					plotOutput("rocPlotBIC"),
					# Adicionando plot para a curva ROC BIC
					plotOutput("tornadoPlotBIC")
				),
				tabPanel(
					"Cruzamentos",
					# Adicionando aba de Cruzamentos
					h3("AIC"),
					uiOutput("crossTabsAIC"),
					uiOutput("crossPlotsAIC"),
					h3("BIC"),
					uiOutput("crossTabsBIC"),
					uiOutput("crossPlotsBIC")
				),
				tabPanel("Bootstrap", 
								 h3("AIC"),
								 h4("Estatísticas"),
								  DTOutput("bootAIC"),
								 h4("Coeficientes"),
								  DTOutput("bootCoefAIC"),
								 h4("Odds ratio"),
								  DTOutput("bootORAIC"),
				         h3("BIC"),
								 h4("Estatísticas"),
								  DTOutput("bootBIC"),
								 h4("Coeficientes"),
								  DTOutput("bootCoefBIC"),
								 h4("Odds ratio"),
								  DTOutput("bootORBIC")),
				
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
		
		sfaic <- step(ajuste_nulo,
									scope = formula(ajuste),
									direction = "forward")
		faic <<- sfaic$formula
		modelo_final_faic <- formula(sfaic)
		vfaic <- all.vars(modelo_final_faic)[-1]
		
		sfbic <- step(
			ajuste_nulo,
			scope = formula(ajuste),
			direction = "forward",
			k = log(nrow(dados_numericos))
		)
		fbic <<- sfbic$formula
		modelo_final_fbic <- formula(sfaic)
		vfbic <- all.vars(modelo_final_faic)[-1]
		
		# --------------------------------------------------------------------------		
		
		calcular_ic <- function(df) {
			
			# Função auxiliar para calcular as estatísticas desejadas
			calcular_valores <- function(coluna) {
				# Remover valores NA
				coluna <- na.omit(coluna)
				
				# Calcular as estatísticas
				media <- mean(coluna)
				q2.5 <- quantile(coluna, 0.025)
				q97.5 <- quantile(coluna, 0.975)
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
			sfaic <- glm(faic, data = dados_treino, family = "binomial")
			sfbic <- glm(fbic, data = dados_treino, family = "binomial")
			
			# Avaliação do modelo no conjunto de teste
			# Predições usando o modelo AIC
			predicoes_aic <- predict(sfaic, newdata = dados_teste, type = "response")
			pred_class_aic <- ifelse(predicoes_aic > 0.5, 1, 0)
			conf_matrix_aic <- confusionMatrix(factor(pred_class_aic), factor(dados_teste$evadido))
			
			# Predições usando o modelo BIC
			predicoes_bic <- predict(sfbic, newdata = dados_teste, type = "response")
			pred_class_bic <- ifelse(predicoes_bic > 0.5, 1, 0)
			conf_matrix_bic <- confusionMatrix(factor(pred_class_bic), factor(dados_teste$evadido))
			
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
			metrics_aic <- do.call(rbind, lapply(resultados, function(x)
				x$metrics_aic))
			metrics_bic <- do.call(rbind, lapply(resultados, function(x)
				x$metrics_bic))
			coef_aic <- do.call(rbind, lapply(resultados, function(x)
				x$coef_aic))
			coef_bic <- do.call(rbind, lapply(resultados, function(x)
				x$coef_bic))
			odds_ratios_aic <- do.call(rbind, lapply(resultados, function(x)
				x$odds_ratios_aic))
			odds_ratios_bic <- do.call(rbind, lapply(resultados, function(x)
				x$odds_ratios_bic))
			
			
			list(
				metrics_aic = metrics_aic,
				metrics_bic = metrics_bic,
				coef_aic = coef_aic,
				coef_bic = coef_bic,
				odds_ratios_aic = odds_ratios_aic,
				odds_ratios_bic = odds_ratios_bic
			)
		}

		if (input$include_bootstrap) {
			resultados_bootstrap <<- bootstrap_modelos(dados_numericos, n_bootstrap = input$slider1)
		} 
		
		
		# --------------------------------------------------------------------------

		# Exibir os resumos dos modelos
		output$bootAIC <- renderDT({
			if (input$include_bootstrap) {
				
			dfBootAIC <- resultados_bootstrap$metrics_aic
			rownames(dfBootAIC) <- NULL
			dfBootAIC <- as.data.frame(dfBootAIC)
			datatable(calcular_ic(dfBootAIC), rownames = FALSE, 
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

		output$bootCoefAIC <- renderDT({
			if (input$include_bootstrap) {
				
			dfBootCoefAIC <- resultados_bootstrap$coef_aic
			rownames(dfBootCoefAIC) <- NULL
			dfBootCoefAIC <- as.data.frame(dfBootCoefAIC)
			auxc <- calcular_ic(dfBootCoefAIC)
			datatable(
				auxc, 
				rownames = FALSE,
				extensions = 'Buttons',
				options = list(
					dom = 'Bfrtip',
					buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
					language = list(
						url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
					)
				)
			)
			}
		})

		output$bootORAIC <- renderDT({
			if (input$include_bootstrap) {
				
			
			dfBootORAIC <- resultados_bootstrap$odds_ratios_aic
			rownames(dfBootORAIC) <- NULL
			dfBootORAIC <- as.data.frame(dfBootORAIC)
			auxc <- calcular_ic(dfBootORAIC)
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
		
		
		
		output$bootBIC <- renderDT({
			if (input$include_bootstrap) {
			dfBootBIC <- resultados_bootstrap$metrics_bic
			rownames(dfBootBIC) <- NULL
			dfBootBIC <- as.data.frame(dfBootBIC)
			datatable(calcular_ic(dfBootBIC), rownames = FALSE, 
								extensions = 'Buttons',
								options = list(
									dom = 'Bfrtip',
									buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
									language = list(
										url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
									)
								)
			)
			}
		})
		
		output$bootCoefBIC <- renderDT({
			if (input$include_bootstrap) {
				
			dfBootCoefBIC <- resultados_bootstrap$coef_bic
			rownames(dfBootCoefBIC) <- NULL
			dfBootCoefBIC <- as.data.frame(dfBootCoefBIC)
			auxc <- calcular_ic(dfBootCoefBIC)
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
		
		output$bootORBIC <- renderDT({
			if (input$include_bootstrap) {
				
			dfBootORBIC <- resultados_bootstrap$odds_ratios_bic
			rownames(dfBootORBIC) <- NULL
			dfBootORBIC <- as.data.frame(dfBootORBIC)
			auxc <- calcular_ic(dfBootORBIC)
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
			summary(sfaic)
		})
		
		# Salvar gráficos AIC
		output$residualsPlot <- renderPlot({
			plot(sfaic, which = 1)
		})
		output$qqPlot <- renderPlot({
			plot(sfaic, which = 2)
		})
		output$scaleLocationPlot <- renderPlot({
			plot(sfaic, which = 3)
		})
		output$cooksDistancePlot <- renderPlot({
			plot(sfaic, which = 4)
		})
		
		output$tornadoPlot <- renderPlot({
			coeficientes <- summary(sfaic)$coefficients
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
		
		# Salvar gráficos BIC
		output$residualsPlotBIC <- renderPlot({
			plot(sfbic, which = 1)
		})
		output$qqPlotBIC <- renderPlot({
			plot(sfbic, which = 2)
		})
		output$scaleLocationPlotBIC <- renderPlot({
			plot(sfbic, which = 3)
		})
		output$cooksDistancePlotBIC <- renderPlot({
			plot(sfbic, which = 4)
		})
		
		output$tornadoPlotBIC <- renderPlot({
			coeficientes <- summary(sfbic)$coefficients
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
		
		# Salvar e exibir a curva ROC para o modelo AIC
		output$rocPlotAIC <- renderPlot({
			prob_AIC <- predict(sfaic, type = "response")
			roc_AIC <- roc(variavel_resposta, prob_AIC)
			plot(roc_AIC, main = "Curva ROC - Modelo AIC")
		})
		
		# Salvar e exibir a curva ROC para o modelo BIC
		output$rocPlotBIC <- renderPlot({
			prob_BIC <- predict(sfbic, type = "response")
			roc_BIC <- roc(variavel_resposta, prob_BIC)
			plot(roc_BIC, main = "Curva ROC - Modelo BIC")
		})
		
		# --------------------------------------------------------------------------
		
		# Exibir LogLik e Critérios de Informação
		output$criteria <- renderPrint({
			list(
				LogLik_Stepwise_AIC = logLik(sfaic),
				AIC_Stepwise_AIC = AIC(sfaic),
				BIC_Stepwise_AIC = BIC(sfaic),
				
				LogLik_Stepwise_BIC = logLik(sfbic),
				AIC_Stepwise_BIC = AIC(sfbic),
				BIC_Stepwise_BIC = BIC(sfbic),
				
				LogLik_Nulo = logLik(ajuste_nulo),
				AIC_Nulo = AIC(ajuste_nulo)
			)
		})
		
		# --------------------------------------------------------------------------
		
		# Exibir Modelos Stepwise
		output$stepwise <- renderPrint({
			list(Modelo_AIC = faic, Modelo_BIC = fbic)
		})
		
		# Função para gerar tabelas e gráficos de cruzamento em percentuais
		generateCrossOutput <- function(model,
																		dados_numericos,
																		variavel_resposta,
																		prefix) {
			selected_vars <- all.vars(model)[-1] # Excluindo a variável resposta
			
			output[[paste0("crossTabs", prefix)]] <- renderUI({
				tabs <- lapply(selected_vars, function(var) {
					table_id <- paste0("table_", var, "_", prefix)
					plot_id <- paste0("plot_", var, "_", prefix)
					
					tabPanel(title = var, tableOutput(table_id), plotOutput(plot_id))
				})
				do.call(tabsetPanel, tabs)
			})
			
			lapply(selected_vars, function(var) {
				table_id <- paste0("table_", var, "_", prefix)
				plot_id <- paste0("plot_", var, "_", prefix)
				
				output[[table_id]] <- renderTable({
					cross_tab <- prop.table(table(dados_numericos[[var]], variavel_resposta), 1) * 100
					cross_tab
				})
				
				output[[plot_id]] <- renderPlot({
					cross_tab <- as.data.frame(prop.table(table(
						dados_numericos[[var]], variavel_resposta
					), 1) * 100)
					colnames(cross_tab) <- c("Var", "Evadido", "Percent")
					ggplot(cross_tab, aes(
						x = Var,
						y = Percent,
						fill = Evadido
					)) +
						geom_bar(stat = "identity", position = "dodge") +
						labs(
							title = paste("Cruzamento entre \"", var, "e \"Evadido\" (%)"),
							x = var,
							y = "Percentual"
						) +
						theme_minimal()
				})
			})
		}
		
		# Gerar cruzamentos para AIC
		generateCrossOutput(faic, dados_numericos, variavel_resposta, "AIC")
		
		# Gerar cruzamentos para BIC
		generateCrossOutput(fbic, dados_numericos, variavel_resposta, "BIC")
		
		remove_modal_spinner()
	})
}

# Executar a aplicação
shinyApp(ui = ui, server = server)