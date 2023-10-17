
#estimacao da variancia e  da covariancia por matrizes em um modelo de regressao linear 

#instalando os pacotes 
# install.packages ("caTools")
# install.packages("readr")

#Chamando o pacote
# library(readr)
# library(caTools)

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
#duvida: b = ((t(X)%*%X)ˆ(-1))%*%t(X)%*%Y nao?
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
# e * e'
tee <- e %*% te

#Calculando a matriz M
#Matriz Identidade
I <- diag(rep(1,74))
M <- I - P

#Vamos Calcular o S²
#Primeiro vamos calcular e'e
MY <- M %*% Y
tMY <- t(Y) %*% M
ee <- tMY %*% MY
n <- sum(diag(I))
print(n)
k <- sum(diag(MatrizP))
print (k)
Squad <- ee / (n-k)
Squadrado <- as.numeric(Squad)
View(Squadrado)

#Vamos calcular a var(b|x)
#Est Var(b|x) = S²*(X'X)^(-1)
Variancia <- Squadrado * Invertermo1
View(Variancia)


#VAR(BETA)
varBeta = InvtXX %*% tX %*% tee %*% X %*% InvtXX
varBeta
