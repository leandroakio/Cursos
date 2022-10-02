# Definindo o diretório de trabalho
getwd()
setwd("C:/Users/leand/OneDrive/Documents/GitHub/Cursos/R")

# O objetivo deste esudo é identificar o risco de crédito de um cliente
credit <- read.csv("dados/credito.csv")
str(credit)
View(credit)

#verificando o saldo da conta corrente e da conta poupança do dataset
table(credit$checking_balance)
table(credit$savings_balance)

# Verificando os tipos de crédito solicitados no dataset
summary(credit$months_loan_duration)
summary(credit$amount)

# Verificando a qtd de créditos concedidos e recusados (variável target)
table(credit$default)

# Usando sample para construir os dados de treino e de teste
set.seed(123)
train_sample <- sample(1000, 900)

# Split dos dataframes em treino e teste
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# Verificando a proporção da variável target. permanece a mesma proporção da amostra
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Construindo o modelo preditivo C5.0 (tipo decision tree)
install.packages("C50")
library(C50)

# Convertendo a coluna 'default' como tipo fcator para passar para o modelo
credit_train$default<-as.factor(credit_train$default)
str(credit_train$default)

# Criando e visualizando o modelo (informando quais colunas são de treino e target)
# Removendo a variável da coluna 17, que corresponde a resposta se concede ou recusa o crédito, para definir as colunas para treinar o modelo.
# A coluna 17, com nome 'default', é a coluna target.
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

# Informações detalhadas sobre a árvore
summary(credit_model)
#O saldo do cliente teve 100% de participação nas decisões

# Avaliando a performance do modelo
credit_pred <- predict(credit_model, credit_test)

# Confusion Matrix para comparar valores observados e valores previstos
install.packages("gmodels")
library(gmodels)

# Criando a Confusion Matrix para analisar os erros e acertos do modelo
?CrossTable
CrossTable(credit_test$default, 
           credit_pred,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Observado', 'Previsto'))

# Melhorando a performance do modelo

# Aumentando a precisão com incremento de 10 tentativas de treino
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)

# Score do modelo
credit_boost_pred10 <- predict(credit_boost10, credit_test)

# Confusion Matrix
CrossTable(credit_test$default, 
           credit_boost_pred10,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Observado', 'Previsto'))

# Dando pesos aos erros

# Criando uma matriz de dimensões de custo
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("Previsto", "Observado")
matrix_dimensions

# Construindo a matriz
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# Aplicando a matriz a árvore
?C5.0
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)

# Score do modelo
credit_cost_pred <- predict(credit_cost, credit_test)

# Confusion Matrix
CrossTable(credit_test$default, 
           credit_cost_pred,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Observado', 'Previsto'))