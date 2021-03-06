##=============================================================================
## T�tulo : Medidas Estat�sticas - Tend�ncia central, Posi��o (central)
## Autores: Gabriel Figueiredo, Matheus Almeida e Ra� Bizerra
## Ano	  : 2017.2
## Objetivo: C�lculo das principais medidas estat�sticas
##=============================================================================

#! - Tend�ncia central
##  - M�dia
##  - Mediana
##  - Moda

#! - Posi��o ou separatrizes
##  - Quartis
##  - Decis
##  - Percentis

#! Carregando os pacotes necess�rios
install.packages('fdth')
install.packages('psych')
library(fdth)
library(psych)


#--------------------------------------------------#
# Fun��o           Medida             Pacote
#--------------------------------------------------#
# mean             m�dia aritm�tica   base, fdth
# geometric.mean   m�dia geom�trica   psych
# harmonic.mean    m�dia harm�nica    psych
# median           mediana            base, fdth
# mfv              moda               fdth
# quantile         quantis            base, fdth
#--------------------------------------------------#


##=============================================================================

#! ----------- Media aritim�tica ----------- #
	#N�o agrupado
		(y <- c(3, 7, 8, 10, 11))
		mean(y)
   
	#Dados previamente agrupados
    #1� Forma
      (y <- 1:4)
      (f <- c(1, 3, 5, 1))
      sum(y*f)/sum(f)
   
   #2� Forma
     (y<-c(1,2,2,2,3,3,3,3,3,4))
     table(y)
     mean(y)
   
  #Agrupado Classes - Reconstituindo uma fdt
    (tb1 <- make.fdt(f=c(5, 10, 14, 8, 3), start=2, end=12))
    methods(mean)
    mean(tb1)  #nesse casso � da mean.fdt 
    

#! ----------- Media Geral ----------- #
	(y1 <- 4:8)
  (y2 <- 1:3)
	(y3 <- 9:13)
     
	medGeral<-function(y1,y2,y3){
		(length(y1)*mean(y1) + length(y2)*mean(y2) + length(y3)*mean(y3)) / 
    (length(y1) + length(y2) + length(y3))
	}
    
	medGeral(y1,y2,y3)
    

#! ----------- Media Geom�trica ----------- #
	y <- c(3, 6, 12, 24, 48)
	geometric.mean(y) #library(psych)
	(prod(y)^(1/length(y))) #formula m�dia geom�trica

	
#! ----------- Media Harm�nica ----------- #
	y <- c(2, 5, 8)
	harmonic.mean(y) #library(psych)
	length(y)/(sum(1/y)) #formula M�dia harm�nica


#! ----------- Mediana ----------- #
#C�lculo da mediana para vari�vel discreta
	#Vetor: impar
  	(y <- c(1, rep(2, 3), rep(3, 5), rep(4, 2)))
  	length(y)
  	median(y)

	#Vetor: par
  	(y <- c(rep(82, 5), rep(85, 10), rep(87, 15), rep(89, 8), rep(90, 4)))
  	length(y)
  	median(y)
 
#C�lculo da mediana para vari�vel cont�nua
    (tb2 <- make.fdt(f=c(5, 12, 18, 14, 6, 3), start=35, end=95))
    methods(median)
    median(tb2)  #median.fdt

#! ----------- Moda ----------- #
#Distribui��o sem agrupamento de classes:
  	(y <- c(rep(243, 7), rep(245, 17), rep(248, 23), rep(251, 20), rep(307, 8)))
    mfv(y) #library(fdth)

#Distribui��o com agrupamento de classes
    (tb3 <- make.fdt(f=c(3, 10, 17, 8, 5), start=0, end=5))
    mfv(tb3) #library(fdth)


#! ----------- Medidas de posi��o ou separatrizes ----------- #
# Vetor
(y <- c(rep(0, 3), rep(1, 10), rep(2, 17), rep(3, 8), rep(4, 5)))

quantile(y)[2:4]                     # quartis

quantile(y, p=seq(0, 1, .1))[2:10]   # decis

quantile(y, p=seq(0, 1, .01))[2:100] # percentis


# Dados previamente agrupados
quantile(tb3, i=2)                        # quartil 2

quantile(tb3,i=5, probs=seq(0, 1, .1))    # decil 5

quantile(tb3, i=50, probs=seq(0, 1, .01)) # percentil 50

# Verifica��o
median(tb3)                          # igual a todos acima

	
#!Exemplo 1
	(y <- sample(1:10, 10, replace=T))
	quantile(y)

	#Quartis
		quantile(y, seq(.25, .75, by=0.25) )
	#Decis
		quantile(y, seq(0, 1, by=0.1))
	#Percentis
		quantile(y, seq(0, 1, by=0.01))

	boxplot(y)
	median(y)
 
#!Exemplo 2
  (tb = make.fdt(f=c(4, 9, 11, 8, 5, 3), start=50, end=74))
  mean(tb)
  quantile(tb, i=1)
  quantile(tb, i=3, probs=seq(0, 1, 0.1))
  quantile(tb, i=8, probs=seq(0, 1, 0.01))
  methods(quantile)