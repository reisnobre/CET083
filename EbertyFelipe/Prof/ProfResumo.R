##=============================================================================
## Título: Medidas estatísticas
## Autor : José Cláudio Faria
## Adaptadção: Eberty Alves e Felipe Cardoso
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
(tb1 <- make.fdt(f=c(5, 10, 14, 8, 3), start=2, end=12))
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
y <- c(1, rep(2, 3), rep(3, 5), rep(4, 2))
length(y)
median(y)

# Vetor: par - pag. 36 apostila CET018
y <- c(rep(82, 5), rep(85, 10), rep(87, 15), rep(89, 8), rep(90, 4))
length(y)
median(y)

# Dados previamente agrupados - pag. 37 apostila CET018
tb2 <- make.fdt(f=c(5, 12, 18, 14, 6, 3), start=35, end=95)
tb2
median(tb2)

##--------------------------------
## Moda
##--------------------------------
# Vetor: impar - pag. 38 apostila CET018
y <- c(rep(243, 7), rep(245, 17), rep(248, 23), rep(251, 20), rep(307, 8))
mfv(y)

# Dados previamente agrupados - pag. 38 apostila CET018
tb3 <- make.fdt(f=c(3, 10, 17, 8, 5), start=0, end=5)
tb3
mfv(tb3)


#! Posição ou separatrizes
#--------------------------------------------------#
# Função           Medida             Pacote
#--------------------------------------------------#
# quantile         quantis            base, fdth
#--------------------------------------------------#

# Vetor
y <- c(rep(0, 3), rep(1, 10), rep(2, 17), rep(3, 8), rep(4, 5))

quantile(y)[2:4]                     # quartis

quantile(y, p=seq(0, 1, .1))[2:10]   # decis

quantile(y, p=seq(0, 1, .01))[2:100] # percentis


# Dados previamente agrupados
quantile(tb3, i=2)                        # quartil 2

quantile(tb3,i=5, probs=seq(0, 1, .1))    # decil 5

quantile(tb3, i=50, probs=seq(0, 1, .01)) # percentil 50

# Verificação
median(tb3)                          # igual a todos acima


#------------------------------------------------------
# Visualização gráfica das amostras
#------------------------------------------------------
par(mfrow=c(2,1), bty='l')

# A
plot(A, type='h', ylim=c(0, 2.5), xlab='A', ylab='')

## Média de A
abline(h=mean(A), col='red', lw=2)

# B
plot(B, type='h', ylim=c(0, 2.5), xlab='B', ylab='Altura, m')

# Média de B
abline(h=mean(B), col='red', lw=2)
