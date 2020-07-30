##=============================================================================
## Título: Medidas estatísticas
## Autor : José Cláudio Faria
## Data  : 20/07/2016 09:48:14
## Versão: v2
## Objetivo: Cálculo das principais medidas estatísticas
##=============================================================================
#! - Tendência central
##  - Média
##  - Mediana
##  - Moda

#! - Posição
##  - Quartis
##  - Decis
##  - Percentis

#! - Dispersão
##  - Amplitude total
##  - Amplitude inter-quartílica
##  - Desvio médio
##  - Desvio quadrático médio
##  - Variância
##  - Desvio parão
##  - Desvio padrão relativo
##  - Coeficiente de variação

#! - Associação
##  - Covariância
##  - Correlação linear simples

##=============================================================================
## Observações:
## - As funções de reconstrução de apresentações tabulares (make.fdt)
##   e respectivas medidas estatísticas são dependentes do pacote 'fdth'.
##   Adicionalmente o pacote implementa a função 'mfv' que pode ser usada
##   para vetores e tabelas.
## - Algumas funções simples são implementadas para medidas pouco usadas,
##   como: amplitude total, desvio médio, desvio quadrático médio, ...
## - O pacote 'psych' implementa uma série de funções úteis relacionadas
##   a medidas estatísticas
##=============================================================================

## Carregando os pacotes necessários
library(fdth)
library(psych)

#! Tendência central
#--------------------------------------------------#
# Função           Medida             Pacote
#--------------------------------------------------#
# mean             média aritmética   base
# mean_g           média geométrica   implementada*
# mean_h           média harmônica    implementada*
# geometric.mean   média geométrica   psych
# harmonic.mean    média harmônica    psych
# median           mediana            base, fdth
# mfv              moda               fdth
#--------------------------------------------------#

# Implementação de funções (para vetores)
# média geométrica
mean_g <- function(x) {
  prod(x)^(1/length(x))
}

# média harmônica
mean_h <- function(x) {
  length(x)/(sum(1/x))
}

##--------------------------------
## Média
##--------------------------------
# Vetor - pag. 33 apostila CET018
y <- c(3, 7, 8, 10, 11)
mean(y)

# Dados previamente agrupados - pag. 34 apostila CET018
y <- 1:4
f <- c(1, 3, 5, 1)
sum(y*f)/sum(f)

# Reconstituindo uma fdt - pag. 34 apostila CET018
(tb1 <- make.fdt(f=c(5, 10, 14, 8, 3),
                 start=2,
                 end=12))
mean(tb1)

# Média geral - pag. 34 apostila CET018
n1 <- 5; y1 <- 6
n2 <- 3; y2 <- 2
n3 <- 5; y3 <- 11
sum(n1*y1 + n2*y2 + n3*y3)/sum(n1, n2, n3)

# Média geométrica - pag. 35 apostila CET018
y <- c(3, 6, 12, 24, 48)
geometric.mean(y)
mean_g(y)

# Média harmônica - pag. 35 apostila CET018
y <- c(2, 5, 8)
harmonic.mean(y)
mean_h(y)

##--------------------------------
## Mediana
##--------------------------------
# Vetor: impar - pag. 36 apostila CET018
y <- c(1,
       rep(2, 3),
       rep(3, 5),
       rep(4, 2))
length(y)
median(y)

# Vetor: par - pag. 36 apostila CET018
y <- c(rep(82, 5),
       rep(85, 10),
       rep(87, 15),
       rep(89, 8),
       rep(90, 4))
length(y)
median(y)

# Dados previamente agrupados - pag. 37 apostila CET018
tb2 <- make.fdt(f=c(5, 12, 18, 14, 6, 3),
                start=35,
                end=95)
tb2
median(tb2)

##--------------------------------
## Moda
##--------------------------------
# Vetor: impar - pag. 38 apostila CET018
y <- c(rep(243, 7),
       rep(245, 17),
       rep(248, 23),
       rep(251, 20),
       rep(307, 8))
mfv(y)

# Dados previamente agrupados - pag. 38 apostila CET018
tb3 <- make.fdt(f=c(3, 10, 17, 8, 5),
                start=0,
                end=5)
tb3
mfv(tb3)


#! Posição ou separatrizes
#--------------------------------------------------#
# Função           Medida             Pacote
#--------------------------------------------------#
# quantile         quantis            base, fdth
#--------------------------------------------------#

# Vetor
y <- c(rep(0, 3),
       rep(1, 10),
       rep(2, 17),
       rep(3, 8),
       rep(4, 5))

quantile(y)[2:4]                     # quartis

quantile(y, p=seq(0, 1, .1))[2:10]   # decis

quantile(y, p=seq(0, 1, .01))[2:100] # percentis


# Dados previamente agrupados
quantile(tb3,                        # quartil 2
         i=2)

quantile(tb3,                        # decil 5
         i=5,
         probs=seq(0, 1, .1))

quantile(tb3,                        # percentil 50
         i=50,
         probs=seq(0, 1, .01))

# Verificação
median(tb3)                          # igual a todos acima


#! Dispersão
#------------------------------------------------------------#
# Função           Medida                       Pacote
#------------------------------------------------------------#
# at               Amplitude total              implementada*
# IQR              Amplitude inter-quartílica   stats
# dm               Desvio médio                 implementada*
# dqm              Desvio quadrático médio      implementada*
# var              Variância                    stats, fdth
# sd               Desvio padrão                stats, fdth
# dpr              Desvio padrão relativo       implementada*
# cv               Coeficiente de variação      implementada*
#------------------------------------------------------------#

# Implementação de funções (para vetores)
# Amplitude total
at <- function(x) {
  diff(range(x))
}

# Desvio médio
dm <- function(x) {
  sum(abs(x-mean(x)))/length(x)
}

# Desvio quadrático médio
dqm <- function(x) {
  sum((x-mean(x))^2)/length(x)
}

# Desvio padrão relativo
dpr <- function(x) {
  sd(x)/mean(x)
}

# Coeficiente de variação
cv <- function(x) {
  100 * sd(x)/mean(x)
}

#------------------------------------------------------
# Reproduzindo duas amostras- pag. 47 apostila CET018
#------------------------------------------------------
A <- c(2, 1.2, 2.1, 1.6, 0.9, 2.2, 1.8)    # Altura, m
B <- c(1.8, 1.7, 1.7, 1.7, 1.5, 1.7, 1.5)  # Altura, m

#------------------------------------------------------
# Visualização gráfica das amostras
#------------------------------------------------------
par(mfrow=c(2,1),
    bty='l')

# A
plot(A,
     type='h',
     ylim=c(0, 2.5),
     xlab='A',
     ylab='')

## Média de A
abline(h=mean(A),
       col='red',
       lw=2)

# B
plot(B,
     type='h',
     ylim=c(0, 2.5),
     xlab='B',
     ylab='Altura, m')

# Média de B
abline(h=mean(B),
       col='red',
       lw=2)

# Amplitude total
at(A)
at(B)

# Desvio médio
dm(A)
dm(B)

# Desvio quadrático médio
dqm(A)
dqm(B)

# Variância
var(A)
var(B)

# Desvio padrão
sd(A)
sd(B)

# Desvio padrão relativo
dpr(A)
dpr(B)

# Coeficiente de variação
cv(A)
cv(B)

# Verificando a influência da escala nas medidas
A100 <- 100 * A  # (Altura, cm)

var(A) == var(A100)
sd(A)  == sd(A100)
dpr(A) == dpr(A100)
cv(A)  == cv(A100)

# Curiosidade
# Testando a igualdade
var(100 + A) == var(A)

# Testando a igualdade variando o número de casas decimais
for(ndec in 1:17)
  print(round(var(100 + A),
              ndec) == round(var(A),
                             ndec))

# Dados previamente agrupados
tb3
var(tb3)
sd(tb3)

#! Associação
#--------------------------------------------------#
# Função        Medida                      Pacote
#--------------------------------------------------#
# cov           covariância                 stats
# cor           correlação liner simples    stats
#--------------------------------------------------#

# Criando os vetores
Y1  <- 1:12

Y2  <- 10 * Y1

Y3  <- -10 * Y1

Y4  <- c(10, 24, 28, 40, 55, 62,
         65, 80, 94, 95, 112, 116)

Y5  <- -1 * Y4

Y6  <- runif(n = 20, min=1, max=15)

Y7  <- Y6 + rnorm(20, mean=2, sd=5)

Y8  <- -1 * Y6 + rnorm(20, m=2, sd=5)

Y9  <- c(0.03, 0.62, 0.07, 0.75, 0.88, 0.59,
         0.93, 0.15, 0.45, 0.61, 0.33, 0.70)

Y10 <- c(0.78, 0.39, 0.40, 0.38, 0.68, 0.63,
         0.66, 0.62, 0.19, 0.98, 0.75, 0.56)

# Definindo parâmetros gráficos
par(mfrow=c(2,2),
    cex=1,
    bty='l',
    pch=19,
    col='blue')

# Visualização gráfica das variáveis aleatórias em um diagrama de dispersão
# Y1 vs. Y2
plot(Y2 ~ Y1,
     main='Perfeita e positiva',
     sub=paste('cov(Y1, Y2)',
               round(cov(Y1, Y2),
                     2),
               sep='='))

# Y1 vs. Y3
plot(Y3 ~ Y1,
     main='Perfeita e negativa',
     sub=paste('cov(Y1, Y3)',
               round(cov(Y1, Y3),
                     2),
               sep='='))

# Y1 vs. Y4
plot(Y4 ~ Y1,
     main='Forte e positiva',
     sub=paste('cov(Y1, Y4)',
               round(cov(Y1, Y4),
                     2),
               sep='='))

# Y1 vs. Y5
plot(Y5 ~ Y1,
     main='Forte e negativa',
     sub=paste('cov(Y1, Y5)',
               round(cov(Y1, Y5),
                     2),
               sep='='))

# Y6 vs. Y7
plot(Y7 ~ Y6,
     main='Fraca e positiva',
     sub=paste('cov(Y6, Y7)',
               round(cov(Y6, Y7),
                     2),
               sep='='))

# Y6 vs. Y8
plot(Y8 ~ Y6,
     main='Fraca e negativa',
     sub=paste('cov(Y6, Y8)',
               round(cov(Y6, Y8),
                     2),
               sep='='))
# Y10 vs. Y9
plot(Y10 ~ Y9,
     main='Nula',
     sub=paste('cov(Y9, Y10)',
               round(cov(Y9, Y10),
                     2),
               sep='='))

# Covariância
cov(Y1, Y2)   # Perfeita positiva
cov(Y1, Y3)   # Perfeita negativa
cov(Y1, Y4)   # Forte positiva
cov(Y1, Y5)   # Forte negativa
cov(Y6, Y7)   # Fraca e positiva
cov(Y7, Y8)   # Fraca e negativa
cov(Y9, Y10)  # Nula

# Correlação: observar as vantagens em relação a covariância!
cor(Y1, Y2)   # Perfeita positiva
cor(Y1, Y3)   # Perfeita negativa
cor(Y1, Y4)   # Forte positiva
cor(Y1, Y5)   # Forte negativa
cor(Y6, Y7)   # Fraca e positiva
cor(Y7, Y8)   # Fraca e negativa
cor(Y9, Y10)  # Nula

# Verificando a influência da escala
# Covariância
cov(Y1, Y4)
cov(100 * Y1, 100 * Y4)

# Correlação
cor(Y1, Y4)
cor(100 * Y1, 100 * Y4)


#! Uma função (psych) com várias medidas estatísticas univariadas
describe(iris)


#! Observações para correção da apostila CET108
# Amplitude total - pag. 43
y <- c(1, 0, 1, 2, 2, 0, 2, 2, 2, 5, 3, 3, 3, 8)
at(y)  # Observar erro na apostila = 28

# Desvio médio - pag. 44
y <- c(2, 1.2, 2.1, 1.6, 0.9, 2.2, 1.8)
dm(y)

# Desvio qudrático médio - pag. 46
y <- c(2, 1.2, 2.1, 1.6, 0.9, 2.2, 1.8)
dqm(y)  # Observar erro na apostila = 1.41
