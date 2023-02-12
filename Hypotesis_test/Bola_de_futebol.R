# Teste de Hipóteses

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/Users/leand/OneDrive/Documents/GitHub/Cursos/Hypotesis_test")
getwd()


# Pacotes
install.packages("readxl")
library(readxl)
library(e1071)

# Carregando o Dataset
dados <- read_excel("dados/Bola_Futebol.xlsx")
View(dados)
dim(dados)
str(dados)

##### Análise Exploratória ##### 

# Verificando valores missing
colSums(is.na(dados))

# Calculando as estatísticas
summary(dados)

# Vetor com o nome das estatísticas
nomes_stats <- c("Média", "Desvio Padrão", "Variância", "Tipo de Bola")

# Calculando as estatísticas para a Bola com Revestimento Atual
dados_stats_atual <- c(round(mean(dados$Atual), digits = 2), 
                       round(sd(dados$Atual), digits = 2), 
                       round(var(dados$Atual), digits = 2),
                       "Bola com Revestimento Atual")

# Calculando as estatísticas para a Bola com Revestimento Novo
dados_stats_novo <- c(round(mean(dados$Novo), digits = 2), 
                      round(sd(dados$Novo), digits = 2), 
                      round(var(dados$Novo), digits = 2),
                      "Bola com Revestimento Novo")

# Combina os resultados para comparação
dados_stats_combined <- rbind(nomes_stats, dados_stats_atual, dados_stats_novo)
View(dados_stats_combined)

# Análise Univariada

# Range
range_atual <- max(dados$Atual) - min(dados$Atual)
range_atual

range_novo <- max(dados$Novo) - min(dados$Novo)
range_novo

# Intervalo Interquartil
summary(dados)

IQR_atual <- IQR(dados$Atual)
IQR_atual

IQR_novo <- IQR(dados$Novo)
IQR_novo

# O intervalo interquartil de ambas as variáveis é equivalente aproximadamente a 12, ou seja, não
# há diferença entre os valores do primeiro ao terceiro quartil.

# Da média, pode-se dizer que as bolas do modelo de revestimento atual cobrem uma distância maior 
# que as bolas do novo modelo (em média), mas o desvio padrão é muito parecido. 
# Portanto, podemos dizer que não há muita diferença (significativa) nos dois modelos de revestimento da bola.

# Representação Gráfica

# A distribuição normal é a distribuição mais fácil de trabalhar, a fim de obter um entendimento 
# sobre estatísticas. As distribuições da vida real geralmente são distorcidas. Muita assimetria 
# e muitas técnicas estatísticas não funcionam. Como resultado, são utilizadas técnicas matemáticas 
# avançadas, incluindo logaritmos e técnicas de regressão quantílica. 
# http://www.econ.uiuc.edu/~roger/research/rq/rq.html

# Ajusta a área de plotagem
par(mfrow = c(2,2))

# Histograma
hist(dados$Atual, 
     main = "Distância - Bola Com Revestimento Atual", 
     xlab = "Distância (metros)", 
     ylab = "Número de Bolas", 
     col = "Blue")

hist(dados$Novo, 
     main = "Distância - Bola Com Revestimento Novo", 
     xlab = "Distância (metros)", 
     ylab = "Número de Bolas", 
     col = "Green")

# Um histograma é inclinado para a direita se o pico do histograma virar para a esquerda. 
# Portanto, a cauda do histograma tem uma inclinação positiva para a direita. 
# O raciocínio inverso é o mesmo.

# Boxplot
boxplot(dados$Atual, 
        main = "Distância - Bola Com Revestimento Atual", 
        xlab = "Distância (metros)", 
        ylab = "Número de Bolas", 
        col = "Blue", 
        horizontal = TRUE)

boxplot(dados$Novo, 
        main = "Distância - Bola Com Revestimento Novo", 
        xlab = "Distância (metros)", 
        ylab = "Número de Bolas", 
        col = "Green", 
        horizontal = TRUE)

# Boxplot assimétrico à esquerda
# Se a maior parte das observações estiver na extremidade superior da escala, um boxplot 
# será assimétrico à esquerda. 
# Consequentemente, o "bigode" esquerdo é mais longo que o "bigode" direito.
# Como resultado, a média é menor que a mediana.

# Boxplot assimétrico à direita
# Se um boxplot estiver assimétrico à direita, a caixa mudará para a esquerda e o 
# "bigode" direito ficará mais longo. Como resultado, a média é maior que a mediana.

# Portanto, a variável com revestimento atual é assimétrica à direita, pois a maioria dos 
# dados está localizada à direita do "pico". A média é maior que a mediana.

# A variável revestimento novo também é inclinada (assimétrica) à direita, pois a maioria dos dados 
# está localizada à direita do gráfico. A média é maior que a mediana.

summary(dados$Atual)
summary(dados$Novo)

# Assimetria

# Se uma cauda é mais longa que a outra, a distribuição é inclinada ou assimétrica. 
# Às vezes, essas distribuições são chamadas de distribuições assimétricas, pois não mostram 
# nenhum tipo de simetria. Simetria significa que metade da distribuição é uma imagem espelhada 
# da outra metade. Por exemplo, a distribuição normal é uma distribuição simétrica sem inclinação. 
# As caudas são exatamente as mesmas.

# Uma distribuição inclinada para a esquerda tem uma longa cauda esquerda. 
# As distribuições inclinadas para a esquerda também são chamadas de distribuições inclinadas 
# negativamente. Isso ocorre porque há uma cauda longa na direção negativa na linha numérica. 
# A média está à esquerda do pico.

# Uma distribuição inclinada à direita tem uma longa cauda direita. 
# Distribuições com inclinação à direita também são chamadas de distribuições com inclinação positiva. 
# Isso ocorre porque há uma cauda longa na direção positiva na linha numérica. 
# A média está à direita do pico.

# Em uma distribuição normal, a média e a mediana são o mesmo número, enquanto a média e a mediana 
# em uma distribuição assimétrica se tornam números diferentes. Podemos então calcular o 
# coeficiente de assimetria.

# Se a assimetria é menor que -1 ou maior que 1, a distribuição é altamente distorcida.
# Se a assimetria é entre -1 e -0,5 ou entre 0,5 e 1, a distribuição é enviesada (assimétrica) moderadamente.
# Se a assimetria é entre -0,5 e 0,5, a distribuição é aproximadamente simétrica.

# A medida positiva indicaria que a média dos valores dos dados é maior do que a mediana e 
# a distribuição dos dados é inclinada para a direita.

# Uma medida negativa indica que a média dos valores dos dados é menor que a mediana e a 
# distribuição dos dados é inclinada para a esquerda. 

skewness(dados$Atual)
summary(dados$Atual)
# nota-se que a média é maior que a mediana


skewness(dados$Novo)
summary(dados$Novo)


# Curtose

# A curtose informa a altura e a nitidez do pico central, em relação a uma curva de sino padrão.
# A distribuição normal tem kurtosis igual a zero.

# Uma curtose negativa significa que sua distribuição é mais plana que uma curva normal com a 
# mesma média e desvio padrão. O raciocínio inverso é o mesmo.
kurtosis(dados$Atual)
kurtosis(dados$Novo)

# Se você estiver executando um teste estatístico paramétrico em seus dados (por exemplo, uma ANOVA), 
# o uso de dados altamente inclinados para a direita ou esquerda pode levar a resultados enganosos. 
# Portanto, se você deseja executar um teste nesse tipo de dados, execute uma transformação de log e 
# execute o teste nos números transformados.

# Teste de Normalidade - Shapiro Test
# Hipótese Nula (H0): Os dados são normalmente distribuídos. 
# Hipótese Alternativa (H1): Os dados não são normalmente distribuídos. 

# Se o valor-p for maior que 0.05 não rejeitamos a hipótese nula e podemos assumir a normalidade dos dados.
# Se o valor-p for menor que 0.05 rejeitamos a hipótese nula e não podemos assumir a normalidade dos dados.
?shapiro.test
shapiro.test(dados$Atual)
shapiro.test(dados$Novo)

# Conclusão da Análise Univariada

# A partir do boxplot e demais análises, pode-se dizer que não há discrepâncias. 
# A média do modelo de revestimentpo atual é 270,3 e a média do novo modelo é 267,5 e o desvio padrão 
# do atual é 8,7529 e o desvio padrão do novo é 9,8969. Não parece haver diferença significativa.

# Análise Bivariada

# Ajusta a área de plotagem
par(mfrow = c(1,1))

# Scatter Plot
plot(dados$Atual, dados$Novo)

# Correlação
cor(dados$Atual, dados$Novo)


# Teste de Hipóteses

# O Teste de Hipóteses é uma forma de inferência estatística que usa dados de uma amostra para tirar 
# conclusões sobre um parâmetro populacional ou uma distribuição de probabilidade populacional. 

# Primeiro, é feita uma suposição provisória sobre o parâmetro ou distribuição. Essa suposição é chamada 
# de hipótese nula e é indicada por H0. Uma hipótese alternativa (denotada H1), é o oposto do que é 
# declarado na hipótese nula. O procedimento de teste de hipóteses envolve o uso de dados 
# de amostra para determinar se H0 pode ou não ser rejeitada. 

# Se H0 for rejeitada, a conclusão estatística é que a hipótese alternativa H1 é verdadeira.

# Formulação da Hipótese

# Hipótese Nula (H0) – Não há diferença significativa entre a distância percorrida das bolas de futebol 
# com revestimento atual e novo.
# H0: muAtual - muNovo igual a 0 (zero)

# Hipótese Alternativa (H1) – Há diferença significativa entre a distância percorridad as bolas de futebol 
# com revestimento atual e novo.
# H1: muAtual - muNovo diferente de 0 (zero)

# A condição preliminar para se aplicar o teste t é a existência de distribuição normal dos dados 
# em ambos grupos de dados. 

# Todavia, existe um impasse! Há três tipos de test t: 
# teste t de uma amostra
# teste t de amostras independentes 
# teste t de amostras relacionadas (pareadas)

# Usamos o teste t de uma amostra para verificar os valores da variável em relação a média conhecida 
# de uma população.

# Para realizarmos os testes de igualdade de variâncias e os testes de médias, precisamos que as 
# duas populações sejam independentes. Esse é um teste de amostras independentes. Por isso paired = F. 

?t.test
#alternative = t indica teste bi caudal
teste_hipo <- t.test(dados$Atual, dados$Novo, paired = F, conf.level = 0.95, alternative = "t") 
teste_hipo

# O teste t pareado é útil para analisar o mesmo conjunto de itens que foram medidos sob duas 
# condições diferentes, as diferenças nas medições feitas sobre o mesmo assunto antes e depois 
# de um tratamento, ou diferenças entre dois tratamentos dados ao mesmo assunto.

# Interpretação do nosso resultado:

# Falhamos em rejeitar a hipótese nula, pois valor-p é maior que o limite de 0.05.
# Significa dizer que há uma probabilidade alta de não haver diferença significativa entre 
# os tipos de revestimento das bolas de futebol.

# Valor-p

# Probabiblidade de que a estatística do teste assuma um valor extremo em relação ao valor 
# observado quando H0 é verdadeira.

# Lembre-se disso (considerando oo valor-p de 0.05):

# Valor-p Baixo: Forte evidência empírica contra H0.
# Valor-p Alto: Pouca ou nenhuma evidência empírica contra H0

# Determinando a Força do Teste e o Tamanho o Ideal de Amostra

# Diferença das médias
delta_mean <- mean(dados$Atual) - mean(dados$Novo)
delta_mean

# Desvio padrão da diferença entre os dados
delta_desvio <- sd(dados$Atual - dados$Novo)
delta_desvio

# Size Effect
size_effect = delta_mean/delta_desvio
size_effect

# Power Test - Força do Teste
library(pwr)
?pwr.t.test
dim(dados)
power_teste <- pwr.t.test(n = 40, d = size_effect, sig.level = 0.05, alternative = "t")
power_teste

# Tamanho ideal da amostra
tamanho_amostra <- pwr.t.test(power = .95, d = 0.5, type = "t", alternative = "t", sig.level = .05)
tamanho_amostra

# a conclusão é de que não existe diferença significativa entre as bolas com tipos de revestimentos diferentes.
#o ideal seria ter 105 amostras (o dobro do que foi utilizado neste estudo)
