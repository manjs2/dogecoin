#Aula 16 - Metodologia Box- Jenkings

remove.packages("readxl")
install.packages("readxl", dependencies = T)
remove.packages("aTSA")
install.packages("aTSA", dependencies = T)
remove.packages("tseries")
install.packages("tseries", dependencies = T)

library(readxl)
library(aTSA)
library(tseries)
library("urca") 
DOGECOIN <- na.omit(read_excel("C:/Econometria/Dogecoin.xls"))

Dogecoin <-  ts(log(DOGECOIN$Close), start = 2014, frequency = 365)

plot(Dogecoin, type="l", main="Logaritmos do Preço do Dogecoin", ylab="Log Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")


#Criar FAC  e FACP

acf(log(DOGECOIN$Close),lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(log(DOGECOIN$Close),lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
summary(ur.df(Dogecoin, "none", lags = 1))

#Teste Philips-Perron
pp.test(Dogecoin)

#Teste KPSS
kpss.test(Dogecoin)

#Se não for estacionária, diferenciar a série

IntOrdem1 <- diff(log(DOGECOIN$Close))
IntegradaOrdem1 <- ts(IntOrdem1, start = 2014, frequency = 365)

plot(IntegradaOrdem1, type="l", main="Primeira Diferança dos Logs do Dogecoin - LogReturn", ylab="Log Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")

#FAC e FACP

acf(IntOrdem1,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted") 

pacf(IntOrdem1,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted") 

#Teste ADF
ur.df(IntegradaOrdem1, "none", lags = 1)

#Teste Philips-Perron
pp.test(IntegradaOrdem1)

#Teste KPSS
kpss.test(IntegradaOrdem1)

#Estimando Regressões e Tabelando Resultados
AR1 <- arima(IntegradaOrdem1, order = c(1,1,0))   #Estima o AR1 e salva os resultados como AR1
AR2 <- arima(IntegradaOrdem1, order = c(2,1,0))   #Estima o AR2 e salva os resultados como AR2



MA1 <- arima(IntegradaOrdem1, order = c(0,1,1))
MA2 <- arima(IntegradaOrdem1, order = c(0,1,2))
MA3 <- arima(IntegradaOrdem1, order = c(0,1,3))
MA4 <- arima(IntegradaOrdem1, order = c(0,1,4))



estimacoes <- list(AR1, AR2, MA1, MA2, MA3, MA4)      #Cria uma lista com os estimadores
sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

AIC <- sapply(estimacoes, AIC)      #Cria Coluna com resultados AIC
BIC <- sapply(estimacoes, BIC)      #Cria Coluna com resultados BIC
Modelo <- c("AR1", "AR2","MA1","MA2","MA3",
            "MA4")   #cria coluna com nome dos modelos

Resultados <- data.frame(Modelo, AIC, BIC)  #Junta as três colunas acima num único resultado
View(Resultados)

#Efetuar teste ARCH-LM para o melhor modelo

MA3 <- arima(IntegradaOrdem1, order = c(0,1,3))


#Previsões

predict(MA3,15)

library(forecast)
forecast(MA3,15)

plot(forecast(MA3,5), type="o", xlim=c(2018.75,2018.85), ylim=c(-0.03,0.06))
grid(col = "black", lty = "dotted")

arch.test(MA3)

#Modelando a Variância

residuos <- MA3$residuals
plot(residuos, type="o", main="Residuos do MA3")
grid(col = "black", lty = "dotted")

#FAC  e FACP  dos Residuos

acf(residuos,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(residuos,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")


GARCH202=garch(residuos,c(20,2),trace=F)
