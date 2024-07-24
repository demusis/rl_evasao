### Código desenvolvido para a dissertação de Adriana Amorim De Musis.

---

# Análise de Regressão Logística com Seleção Stepwise e Validação Bootstrap

Este repositório contém um aplicativo Shiny para realizar análises de regressão logística, utilizando seleção stepwise e validação bootstrap. O aplicativo permite a importação de arquivos XLSX, configuração de parâmetros de validação e visualização de resultados através de várias interfaces gráficas e tabelas.

## Funcionalidades

### Interface do Usuário (UI)
A interface é composta por diversos componentes para interação e visualização:

- **Carregamento de Arquivo**: Permite ao usuário selecionar um arquivo XLSX para análise.
- **Validação Bootstrap**: Opção para incluir a validação bootstrap.
- **Configuração de Particionamento**: Slider para definir a proporção de dados para treino.
- **Configuração de Repetições Bootstrap**: Slider para definir o número de repetições bootstrap.
- **Botão de Análise**: Inicia a análise dos dados carregados.

### Painel Principal

#### Tabset Panels
O painel principal possui várias abas para exibir diferentes tipos de resultados:

1. **Modelos Stepwise (ass.)**: Exibe os resultados da seleção stepwise.
2. **LogLik e Critérios de Informação (ass.)**: Mostra os valores de LogLik e critérios de informação.
3. **Gráficos (ass.)**: Contém diversos gráficos, incluindo:
   - Gráficos de Resíduos
   - QQ Plot
   - Scale-Location Plot
   - Cook’s Distance Plot
   - Curvas ROC (AIC e BIC)
   - Gráficos de Tornado
4. **Bootstrap**: Exibe estatísticas e coeficientes para os modelos AIC e BIC com validação bootstrap.

### Server
O servidor processa os dados e gera os resultados conforme as opções selecionadas pelo usuário.

- **Leitura de Dados**: Leitura do arquivo XLSX carregado pelo usuário.
- **Seleção Stepwise**: Implementação do algoritmo de seleção stepwise.
- **Validação Bootstrap**: Execução da validação bootstrap conforme configurado pelo usuário.
- **Geração de Gráficos**: Criação de gráficos para análise visual dos modelos.
- **Cálculo de Métricas**: Cálculo de métricas como AIC, BIC e curvas ROC.

## Dependências

O aplicativo utiliza diversas bibliotecas do R, incluindo:

- `shiny`
- `bestglm`
- `boot`
- `ggplot2`
- `readxl`
- `dplyr`
- `tidyr`
- `openxlsx`
- `caret`
- `shinybusy`
- `pROC`
- `purrr`
- `DT`

Certifique-se de que todas essas bibliotecas estejam instaladas antes de rodar o aplicativo.

## Instruções de Uso

1. **Clone o repositório**:
    ```bash
    git clone https://github.com/seu-usuario/seu-repositorio.git
    ```
2. **Navegue até o diretório do projeto**:
    ```bash
    cd seu-repositorio
    ```
3. **Execute o aplicativo Shiny**:
    ```R
    library(shiny)
    runApp("app.R")
    ```

## Licença

Este projeto está licenciado sob a Licença MIT - veja o arquivo LICENSE para mais detalhes.

---

### Observações Finais

Esse aplicativo foi projetado para facilitar a análise de regressão logística com um enfoque especial em seleção de modelos e validação, oferecendo uma interface amigável para usuários que desejam explorar essas técnicas de forma interativa.
