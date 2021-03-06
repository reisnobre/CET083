##=============================================================================
## T�tulo: Medidas estat�sticas
## Autor : Jos� Cl�udio Faria
## Data  : 20/07/2016 09:48:14
## Vers�o: v2
## Objetivo: C�lculo das principais medidas estat�sticas
##=============================================================================
#! - Tend�ncia central
##  - M�dia
##  - Mediana
##  - Moda

#! - Posi��o
##  - Quartis
##  - Decis
##  - Percentis

#! - Dispers�o
##  - Amplitude total
##  - Amplitude inter-quart�lica
##  - Desvio m�dio
##  - Desvio quadr�tico m�dio
##  - Vari�ncia
##  - Desvio par�o
##  - Desvio padr�o relativo
##  - Coeficiente de varia��o

#! - Associa��o
##  - Covari�ncia
##  - Correla��o linear simples

##=============================================================================
## Observa��es:
## - As fun��es de reconstru��o de apresenta��es tabulares (make.fdt)
##   e respectivas medidas estat�sticas s�o dependentes do pacote 'fdth'.
##   Adicionalmente o pacote implementa a fun��o 'mfv' que pode ser usada
##   para vetores e tabelas.
## - Algumas fun��es simples s�o implementadas para medidas pouco usadas,
##   como: amplitude total, desvio m�dio, desvio quadr�tico m�dio, ...
## - O pacote 'psych' implementa uma s�rie de fun��es �teis relacionadas
##   a medidas estat�sticas
##=============================================================================

## Carregando os pacotes necess�rios
library(fdth)
library(psych)

#! Tend�ncia central
#--------------------------------------------------#
# Fun��o           Medida             Pacote
#--------------------------------------------------#
# mean             m�dia aritm�tica   base
# mean_g           m�dia geom�trica   implementada*
# mean_h           m�dia harm�nica    implementada*
# geometric.mean   m�dia geom�trica   psych
# harmonic.mean    m�dia harm�nica    psych
# median           mediana            base, fdth
# mfv              moda               fdth
#--------------------------------------------------#

# Implementa��o de fun��es (para vetores)
# m�dia geom�trica
mean_g <- function(x) {
  prod(x)^(1/length(x))
}

# m�dia harm�nica
mean_h <- function(x) {
  length(x)/(sum(1/x))
}

##--------------------------------
## M�dia
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

# M�dia geral - pag. 34 apostila CET018
n1 <- 5; y1 <- 6
n2 <- 3; y2 <- 2
n3 <- 5; y3 <- 11
sum(n1*y1 + n2*y2 + n3*y3)/sum(n1, n2, n3)

# M�dia geom�trica - pag. 35 apostila CET018
y <- c(3, 6, 12, 24, 48)
geometric.mean(y)
mean_g(y)

# M�dia harm�nica - pag. 35 apostila CET018
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


#! Posi��o ou separatrizes
#--------------------------------------------------#
# Fun��o           Medida             Pacote
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

# Verifica��o
median(tb3)                          # igual a todos acima


#! Dispers�o
#------------------------------------------------------------#
# Fun��o           Medida                       Pacote
#------------------------------------------------------------#
# at               Amplitude total              implementada*
# IQR              Amplitude inter-quart�lica   stats
# dm               Desvio m�dio                 implementada*
# dqm              Desvio quadr�tico m�dio      implementada*
# var              Vari�ncia                    stats, fdth
# sd               Desvio padr�o                stats, fdth
# dpr              Desvio padr�o relativo       implementada*
# cv               Coeficiente de varia��o      implementada*
#------------------------------------------------------------#

# Implementa��o de fun��es (para vetores)
# Amplitude total
at <- function(x) {
  diff(range(x))
}

# Desvio m�dio
dm <- function(x) {
  sum(abs(x-mean(x)))/length(x)
}

# Desvio quadr�tico m�dio
dqm <- function(x) {
  sum((x-mean(x))^2)/length(x)
}

# Desvio padr�o relativo
dpr <- function(x) {
  sd(x)/mean(x)
}

# Coeficiente de varia��o
cv <- function(x) {
  100 * sd(x)/mean(x)
}

#------------------------------------------------------
# Reproduzindo duas amostras- pag. 47 apostila CET018
#------------------------------------------------------
A <- c(2, 1.2, 2.1, 1.6, 0.9, 2.2, 1.8)    # Altura, m
B <- c(1.8, 1.7, 1.7, 1.7, 1.5, 1.7, 1.5)  # Altura, m

#------------------------------------------------------
# Visualiza��o gr�fica das amostras
#------------------------------------------------------
par(mfrow=c(2,1),
    bty='l')

# A
plot(A,
     type='h',
     ylim=c(0, 2.5),
     xlab='A',
     ylab='')

## M�dia de A
abline(h=mean(A),
       col='red',
       lw=2)

# B
plot(B,
     type='h',
     ylim=c(0, 2.5),
     xlab='B',
     ylab='Altura, m')

# M�dia de B
abline(h=mean(B),
       col='red',
       lw=2)

# Amplitude total
at(A)
at(B)

# Desvio m�dio
dm(A)
dm(B)

# Desvio quadr�tico m�dio
dqm(A)
dqm(B)

# Vari�ncia
var(A)
var(B)

# Desvio padr�o
sd(A)
sd(B)

# Desvio padr�o relativo
dpr(A)
dpr(B)

# Coeficiente de varia��o
cv(A)
cv(B)

# Verificando a influ�ncia da escala nas medidas
A100 <- 100 * A  # (Altura, cm)

var(A) == var(A100)
sd(A)  == sd(A100)
dpr(A) == dpr(A100)
cv(A)  == cv(A100)

# Curiosidade
# Testando a igualdade
var(100 + A) == var(A)

# Testando a igualdade variando o n�mero de casas decimais
for(ndec in 1:17)
  print(round(var(100 + A),
              ndec) == round(var(A),
                             ndec))

# Dados previamente agrupados
tb3
var(tb3)
sd(tb3)

#! Associa��o
#--------------------------------------------------#
# Fun��o        Medida                      Pacote
#--------------------------------------------------#
# cov           covari�ncia                 stats
# cor           correla��o liner simples    stats
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

# Definindo par�metros gr�ficos
par(mfrow=c(2,2),
    cex=1,
    bty='l',
    pch=19,
    col='blue')

# Visualiza��o gr�fica das vari�veis aleat�rias em um diagrama de dispers�o
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

# Covari�ncia
cov(Y1, Y2)   # Perfeita positiva
cov(Y1, Y3)   # Perfeita negativa
cov(Y1, Y4)   # Forte positiva
cov(Y1, Y5)   # Forte negativa
cov(Y6, Y7)   # Fraca e positiva
cov(Y7, Y8)   # Fraca e negativa
cov(Y9, Y10)  # Nula

# Correla��o: observar as vantagens em rela��o a covari�ncia!
cor(Y1, Y2)   # Perfeita positiva
cor(Y1, Y3)   # Perfeita negativa
cor(Y1, Y4)   # Forte positiva
cor(Y1, Y5)   # Forte negativa
cor(Y6, Y7)   # Fraca e positiva
cor(Y7, Y8)   # Fraca e negativa
cor(Y9, Y10)  # Nula

# Verificando a influ�ncia da escala
# Covari�ncia
cov(Y1, Y4)
cov(100 * Y1, 100 * Y4)

# Correla��o
cor(Y1, Y4)
cor(100 * Y1, 100 * Y4)


#! Uma fun��o (psych) com v�rias medidas estat�sticas univariadas
describe(iris)


#! Observa��es para corre��o da apostila CET108
# Amplitude total - pag. 43
y <- c(1, 0, 1, 2, 2, 0, 2, 2, 2, 5, 3, 3, 3, 8)
at(y)  # Observar erro na apostila = 28

# Desvio m�dio - pag. 44
y <- c(2, 1.2, 2.1, 1.6, 0.9, 2.2, 1.8)
dm(y)

# Desvio qudr�tico m�dio - pag. 46
y <- c(2, 1.2, 2.1, 1.6, 0.9, 2.2, 1.8)
dqm(y)  # Observar erro na apostila = 1.41
