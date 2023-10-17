Chamando o pacote
library(readr)
library(caTools)
library(matlib)
library(sandwich)
library(lmtest)
#Criando a pasta de diretório
setwd("/Users/luanaferraz/Desktop/2º\ semestre/Econometria\ 1/Tarefas\ R\ -\ Luana/")
#Importando a base de dados
data2 <- read.csv("auto.csv", header=TRUE)
#Rodando o modelo de regressão linear (price é Var Dependente e mpg é a Var Independente)
model<-lm(price~mpg, data=data2)
#Obtendo o resultado da regressão
summary(model)
#calculando a matriz de Variância e Covariância
matriz_de_covariancia <- vcov(summary(model))
#ver resultado matriz
View(matriz_de_covariancia)

#calculando por matrizes
#definindo X e Y
X = as.matrix(cbind(1,data2$mpg))
Y = as.matrix(data2$price)
#calculando o beta
beta = solve(t(X)%*%X)%*%t(X)%*%Y
beta

#Matriz de variância e covariância por matrizes
#Calculando as matrizes M e P
#transposta de X
tX <- t(X)
# Calculando a inversa de X'X
InvtXX <- solve(tX %*% X)
#Calculando a Matriz P
P <- X %*% InvtXX %*% tX

PY <- P %*% Y
#Calculando o erro
e <- Y - PY
#Transposta do erro
te <- t(e)
# e` * e
tee <- te %*% e

#Calculando a matriz M
#Matriz Identidade
I <- diag(rep(1,74))
M <- I - P

#Vamos Calcular o S²
#sum(diag(MATRIZ)) é a forma de se calcular o traço da matriz no R
n <- sum(diag(I))
print(n)
k <- sum(diag(P))
print (k)
S12 <- tee / (n-k)
#alternativamente
tr(M)
S122 <- tee / (tr(M))
#transformando S12 e S122 em um escalar
S2 <- as.numeric(S12)
print(S2)
S22 <- as.numeric(S122)
print(S22)
#Vamos calcular a var(b|x)
#Est Var(b|x) = S²*(X'X)^(-1)
Var <- S2 * InvtXX
View(Var)
Var2 <- S22 * InvtXX
View(Var2)


#Tarefa 02 - Calcular a matriz de var-cov de White
#Relembrando o modelo
summary(model)

#Com comando simples
vcovHC(model, type="HC")
vcovHC(model, type="HC0")

#OBS: " 'HC' (ou equivalentemente 'HC0') fornece o estimador...
#...de White, os outros estimadores são refinamentos deste."

#Calculando passo a passo por matrizes

#V(b) = (X´X)^-1 * X´ * Ω * X * (X´X)^-1
#onde Ω é uma matriz diagonal com elemento da diagonal igual a σi^2

equad <- ((e)^2)

#Matriz quadrada (num linhas = num colunas) do erro ao quadrado
Matriz_equad <- matrix (equad, n, n)

#Matriz com os erros quadrados na diagonal principal
equad.diag <- diag(diag(Matriz_equad))
View(equad.diag)

#Então, calculando (X´X)^-1 * X´ * Ω * X * (X´X)^-1
var.white = InvtXX %*% tX %*% equad.diag %*% X %*% InvtXX
print(var.white)

#Relembrando
vcovHC(model, type="HC")
vcovHC(model, type="HC0")

#Mas, se quisermos incluir o escalar de ponderação do Slide, daria uma diferença
#Escalar da ponderação
escalar <- ((n)/(n-k))
var.white2 = ( InvtXX %*% tX %*% equad.diag %*% X %*% InvtXX )* escalar
print(var.white2)

#Por quê? A matriz adaptada de White que inclui ponderação (n)/(n-k) é a HC1
vcovHC(model, type="HC1") 