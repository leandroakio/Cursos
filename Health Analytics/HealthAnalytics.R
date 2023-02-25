# Health Analytics

# Projeto - Podemos Prever o Tempo de Sobrevivência dos Pacientes 1 Ano Após Receberem um Transplante?

# Leia a definição do projeto e detalhes sobre a fonte de dados no manual em pdf no Capítulo 9.

# Diretório de trabalho
setwd("C:/FCD/Business_Analytics/09_Health Analytics")
getwd()

# Pacotes
library(dplyr)
library(ggcorrplot)
library(forecast)
library(nnet)
library(neuralnet)

# Carregando os dados
dados <- read.csv("dados/dataset.csv", header = TRUE, na.strings = c(""))
dim(dados)

# Análise Exploratória, Limpeza, Transformação e Manipulação de Dados (Data Wrangling)

# Visualizando os dados
View(dados)

# Tipos dos dados
str(dados)

# Explorando os dados das variáveis numéricas
hist(dados$AGE)
hist(dados$AGE_DON)
hist(dados$PTIME)
hist(dados$DAYSWAIT_CHRON)
hist(dados$FINAL_MELD_SCORE)

# Explorando os dados das variáveis categóricas
dados$DIAB <- as.factor(dados$DIAB)
table(dados$DIAB)

dados$PSTATUS <- as.factor(dados$PSTATUS)
table(dados$PSTATUS)

dados$GENDER <- as.factor(dados$GENDER)
dados$GENDER_DON <- as.factor(dados$GENDER_DON)
table(dados$GENDER)
table(dados$GENDER_DON)

dados$REGION <- as.factor(dados$REGION)
table(dados$REGION)

dados$TX_Year <- as.factor(dados$TX_Year)
table(dados$TX_Year)

dados$MALIG <- as.factor(dados$MALIG)
table(dados$MALIG)

dados$HIST_CANCER_DON <- as.factor(dados$HIST_CANCER_DON)
table(dados$HIST_CANCER_DON)

# Considerando apenas os pacientes que sobreviveram ao primeiro ano de cirurgia
dados1 <- dados %>%
  filter(PTIME > 365) %>%
  mutate(PTIME = (PTIME - 365))

dim(dados1)

# Dos pacientes que sobreviveram ao primeiro ano da cirurgia,
# filtramos os que permaneceram vivos por até três anos depois da cirurgia.
dados2 <- dados1 %>%
  filter(PTIME <= 1095)

dim(dados2)
View(dados2)

# Vamos separar variáveis numéricas e categóricas
dados_num <- dados2[,unlist(lapply(dados2, is.numeric))]
dim(dados_num)

dados_fator <- dados2[,unlist(lapply(dados2, is.factor))]
dim(dados_fator)

# Correlação entre as variáveis numéricas
# Para variáveis categóricas usamos associação
df_corr <- round(cor(dados_num, use = "complete.obs"), 2)
?ggcorrplot
ggcorrplot(df_corr)

# Padronização das variáveis numéricas e combinação em um novo dataframe 
# com as variáveis categóricas

# Padronização
dados_num_norm <- scale(dados_num)
dados_final <- cbind(dados_num_norm, dados_fator)
dim(dados_final)
View(dados_final)

# Divisão dos dados em treino e teste
set.seed(1)
index <- sample(1:nrow(dados_final), dim(dados_final)[1]*.7)
dados_treino <- dados_final[index,]
dados_teste <- dados_final[-index,]

# Remove os registros dos anos de 2001 e 2002 (pois foram os primeiros anos da coleta de dados)
dados_treino <- dados_treino %>%
  filter(TX_Year != 2001) %>%
  filter(TX_Year != 2002)

dados_teste <- dados_teste %>%
  filter(TX_Year != 2001) %>%
  filter(TX_Year != 2002)

# Modelagem Preditiva com Modelo de Regressão

# Vamos trabalhar apenas com algumas variáveis mais significativas para o problema.
# Isso também reduz o tempo total de treinamento.
?lm
modelo_v1 <- lm(PTIME ~ FINAL_MELD_SCORE + 
                  REGION + 
                  LiverSize + 
                  LiverSizeDon + 
                  #ALCOHOL_HEAVY_DON + 
                  MALIG + 
                  TX_Year,
                data = dados_treino)

summary(modelo_v1)

# Avaliação do modelo

# Com dados de treino
modelo_v1_pred_1 = predict(modelo_v1, newdata = dados_treino)
?accuracy
accuracy(modelo_v1_pred_1, dados_treino$PTIME)

# Com dados de teste
modelo_v1_pred_2 = predict(modelo_v1, newdata = dados_teste)
accuracy(modelo_v1_pred_2, dados_teste$PTIME)

# Distribuição do erro de validação
par(mfrow = c(1,1))
residuos <- dados_teste$PTIME - modelo_v1_pred_2
hist(residuos, xlab = "Resíduos", main = "Sobreviventes de 1 a 3 Anos")

# Agora vamos padronizar os dados de treino e teste de forma separada.
# Executamos o procedimento anterior, mas de forma separada em cada subset.
set.seed(1)
index <- sample(1:nrow(dados2), dim(dados2)[1]*.7)
dados_treino <- dados2[index,]
dados_teste <- dados2[-index,]

# Vamos separar variáveis numéricas e categóricas (treino)
dados_treino_num <- dados_treino[,unlist(lapply(dados_treino, is.numeric))]
dim(dados_treino_num)

dados_treino_fator <- dados_treino[,unlist(lapply(dados_treino, is.factor))]
dim(dados_treino_fator)

# Vamos separar variáveis numéricas e categóricas (teste)
dados_teste_num <- dados_teste[,unlist(lapply(dados_teste, is.numeric))]
dim(dados_teste_num)

dados_teste_fator <- dados_teste[,unlist(lapply(dados_teste, is.factor))]
dim(dados_teste_fator)

# Padronização
dados_treino_num_norm <- scale(dados_treino_num)
dados_treino_final <- cbind(dados_treino_num_norm, dados_treino_fator)
dim(dados_treino_final)

# Padronização
dados_teste_num_norm <- scale(dados_teste_num)
dados_teste_final <- cbind(dados_teste_num_norm, dados_teste_fator)
dim(dados_teste_final)

# Filtra os anos de 2001 e 2002
dados_treino_final <- dados_treino_final %>%
  filter(TX_Year != 2001) %>%
  filter(TX_Year != 2002)

dados_teste_final <- dados_teste_final %>%
  filter(TX_Year != 2001) %>%
  filter(TX_Year != 2002)

# Cria novamente o modelo agora com o outro dataset de treino
modelo_v1 <- lm(PTIME ~ FINAL_MELD_SCORE + 
                  REGION + 
                  LiverSize + 
                  LiverSizeDon + 
                  #ALCOHOL_HEAVY_DON + 
                  MALIG + 
                  TX_Year,
                data = dados_treino_final)

summary(modelo_v1)

# Avaliação do modelo

# Com dados de treino
modelo_v1_pred_1 = predict(modelo_v1, newdata = dados_treino_final)
accuracy(modelo_v1_pred_1, dados_treino_final$PTIME)

# Com dados de teste
modelo_v1_pred_2 = predict(modelo_v1, newdata = dados_teste_final)
accuracy(modelo_v1_pred_2, dados_teste_final$PTIME)

# Distribuição do erro de validação
par(mfrow = c(1,1))
residuos <- dados_teste_final$PTIME - modelo_v1_pred_2
hist(residuos, xlab = "Resíduos", main = "Sobreviventes de 1 a 3 Anos")

# Vamos desfazer a escala dos dados
variaveis_amostra <- c("PTIME",
                       "FINAL_MELD_SCORE",
                       "REGION",
                       "LiverSize",
                       "LiverSizeDon",
                       "MALIG",
                       "TX_Year")

# Removemos valores NA das variáveis que usaremos para aplicar o unscale
dados_unscale <- na.omit(dados2[,variaveis_amostra])

# Retorna os dados unscale
dados_final_unscale <- dados_unscale[-index,] %>%
  filter(TX_Year!= 2001) %>%
  filter(TX_Year!= 2002)

# Histograma dos dados sem escala (formato original)
previsoes = predict(modelo_v1, newdata = dados_final_unscale)
hist(previsoes)
accuracy(previsoes, dados_final_unscale$PTIME)

# Modelagem Preditiva com Modelo de Rede Neural

# Preparação dos dados
dados_final2 <- na.omit(dados_final[,variaveis_amostra])
dim(dados_final2)
str(dados_final2)

# Retorna somente as variáveis que não são do tipo fator
variaveis_numericas <- unlist(lapply(dados_final2, is.numeric))
View(variaveis_numericas)

# Retorna o nome das variáveis numéricas
variaveis_numericas_nomes <- names(dados_final2[,unlist(lapply(dados_final2, is.numeric))])
View(variaveis_numericas_nomes)

# Gera o dataframe final com variáveis dummy
?class.ind
df_final = cbind(dados_final2[,variaveis_numericas],
                 class.ind(dados_final2$REGION),
                 class.ind(dados_final2$MALIG),
                 class.ind(dados_final2$TX_Year))

dim(df_final)
View(df_final)

# Nomes das variáveis
names(df_final) = c(variaveis_numericas_nomes,
                    paste("REGION", c(1:11),sep = ""),
  
                    paste("MALIG", c(1:3), sep = ""),
                    paste("LISTYR", c(01:18), sep = ""))

dim(df_final)
View(df_final)

# Divisão em dados de treino e teste
index2 <- sample(1:nrow(df_final), dim(df_final)[1]*.70)
dados_treino2 <- df_final[index2,]
dados_teste2 <- df_final[-index2,]
print(dados_teste2[1,])

# Modelo
# www.deeplearningbook.com.br
?neuralnet
modelo_v2 <- neuralnet::neuralnet(PTIME ~ ., 
                                  data = dados_treino2, 
                                  linear.output = TRUE,
                                  hidden = 2,
                                  stepmax = 1e7)

# Plot
plot(modelo_v2,
     col.entry.synapse = "red", 
     col.entry = "brown",
     col.hidden = "green", 
     col.hidden.synapse = "black",
     col.out = "yellow", 
     col.out.synapse = "purple",
     col.intercept = "green", 
     fontsize = 10,
     show.weights = TRUE ,
     rep = "best")

# Avaliação do modelo

# Com dados de treino
modelo_v2_pred_1 <- compute(modelo_v2, dados_treino2)
accuracy(unlist(modelo_v2_pred_1), dados_treino2$PTIME)

# Com dados de teste
modelo_v2_pred_2 <- compute(modelo_v2, dados_teste2)
accuracy(unlist(modelo_v2_pred_2), dados_teste2$PTIME)

# Conclusão: O modelo de regressão linear apresentou uma taxa de erro menor e, portanto, 
# deve ser usado como versão final.

# Finaliza-se assim a previsão do tempo de sobrevivência dos pacientes 1 ano após receberem um transplante.






