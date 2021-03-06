## ===========================================================================
## Nome    : func�o geradora de dados: gerar_dados
## Autor   : Jos� Cl�udio Faria/DCET/UESC
## Data    : 18/07/2016 11:31:41
## Vers�o  : v8
## Objetivo: Gerar dados (data.frame, com estrutura de covari�ncia) para as
##           avalia��es pr�ticas de an�lise explorat�ria de dados dos cursos 
##           de introdu��o � estat�stica
## Require : Matrix, mvtnorm
## email   : <<<joseclaudio.faria@gmail.com>>>
## ===========================================================================
## Observa��es:
## 1- Muito cuidado ao informar as matr�culas: a gera��o dos dados para
## an�lise (e subsequente corre��o) dependem dessa informa��o correta.
## As matr�culas informadas devem ser obrigatoriamente as do grupo.
## 
## 2- Grupos com menos que 3 alunos: repetir a(s) �ltima(s) matr�cula(s)
## para a(s) matr�cula(s) restante(s).
## 
## 3- Se encontrar algum problema com as matr�clas informadas, tente alterar
## alguma das matr�culas. Nesse caso n�o esquecer de comunicar ao professor
## (por escrito) no corpo da prova.
## 
## 4- A fun��o est� intencionalmente mal documentada.
## ===========================================================================
#install.packages('fdth')
#install.packages('Matrix')
#install.packages('mvtnorm')
#install.packages('xtable')
library(fdth)
library(Matrix)
library(mvtnorm)
library(xtable)
## Fun��o que gera 2e3 dados de acordo com as matr�culas informadas
gerar_dados <- function(mat_1=NULL,
                        mat_2=NULL,
                        mat_3=NULL,
                        n=2e3)
{
  #Verifica se as matriculas informadas s�o n�meros
  stopifnot(is.numeric(mat_1) &
            is.numeric(mat_2) &
            is.numeric(mat_3))
  
  #Semente para gera��o aleat�ria de dados
  set.seed(mat_1 +
           mat_2 +
           mat_3)
  
  m_1 <- runif(1,
               min=20,
               max=40)

  m_2 <- runif(1,             
               min=20,
               max=40)

  ## Categ�rica                            
  n_cat_1 <- n/10 * sample(4:8,
                           1)

  ## Matriz de vari�ncias e covari�ncias
  sigma_1 <- matrix(c(m_1,
                      m_1 / 1.1,
                      m_1 / 1.1,
                      m_2),
                    ncol=2)

  sigma_2 <- matrix(c(m_1,
                      -1 * (m_2 / 1.2),
                      -1 * (m_2 / 1.2),
                      m_2),
                    ncol=2)

  near_1 <- nearPD(sigma_1)

  near_2 <- nearPD(sigma_2)

  sigma_1 <- matrix(near_1[['mat']]@x,
                    nc=ncol(sigma_1))

  sigma_2 <- matrix(near_2[['mat']]@x,
                    nc=ncol(sigma_2))

  v_pro_1 <- round(rmvnorm(n=n_cat_1,
                           mean=c(m_1,
                                  m_2), 
                           sigma=sigma_1),
                   2)

  v_pro_2 <- round(rmvnorm(n=(n - n_cat_1),
                           mean=c(m_1,
                                  m_2),
                           sigma=sigma_2),
                   2)

  ## Escala categ�rica
  cat_1 <- rep('M',
               n_cat_1)

  cat_2 <- rep('F',
               n - n_cat_1)

  v_pro <- c('v_pro_1',
             'v_pro_2')

  v_cat <- c('cat_1',
             'cat_2')

  ord <- sample(1:2,
                2)

  sexo <- c(eval(parse(text=v_cat[ord[1]])),
            eval(parse(text=v_cat[ord[2]])))

  
  ## Frame de dados
  res <- as.data.frame(rbind(eval(parse(text=v_pro[ord[1]])),
                             eval(parse(text=v_pro[ord[2]]))))

  res <- cbind(res,
               sexo)

  colnames(res) <- c('Y1',
                     'Y2',
                     'Sexo')

  ## Outlier v_pro_1
  n_out_v1 <- sample(10:20,
                     1)

  out_v1 <- sample(1:length(res[, 1]),
                   n_out_v1)

  res[, 1][out_v1] <- sample(730:999,
                             n_out_v1)

  ## Outlier v_pro_2
  n_out_v2 <- sample(10:30,
                     1)

  out_v2 <- sample(1:length(res[, 2]),
                   n_out_v2)

  res[, 2][out_v2] <- sample(200:300,
                             n_out_v2)

  ## NAs
  res[, 1][sample(1:n, 
                  sample(10:20, 
                         1))] <- NA

  res[, 2][sample(1:n, 
                   sample(10:20, 
                          1))] <- NA

  res[, 3][sample(1:n, 
                   sample(10:20, 
                          1))] <- NA

  ## Negativos
   res[, 1][sample(1:n, 
                   sample(10:20, 
                          1))] <- -999

   res[, 2][sample(1:n, 
                   sample(10:20, 
                          1))] <- -999

  invisible(res)
}

gerar_dados_rl <- function(mat_1=NULL,
                           mat_2=NULL,
                           mat_3=NULL,
                           n=10)
{
  stopifnot(is.numeric(mat_1) &
            is.numeric(mat_2) &
            is.numeric(mat_3))

  set.seed(sum(mat_1,
               mat_2,
               mat_3))

  X <- seq(0, 10, length=n)
  Y <- 1 + 2*X + -.08*X^2 + rnorm(n)

  res <- data.frame(X,
                    Y)

  invisible(res)
}


remove_outliers <- function(x, na.rm = TRUE, ...) {
  AIQY1 <- (quantile(x$Y1, na.rm = TRUE)[4] - quantile(x$Y1, na.rm = TRUE)[2])
  AIQY1 <- AIQY1*1.5
  
  AIQY2 <-(quantile(x$Y2, na.rm = TRUE)[4] - quantile(x$Y2, na.rm = TRUE)[2])
  AIQY2 <- AIQY2*1.5

  dad <-  subset(x,
          Y1>= quantile(Y1, na.rm = TRUE)[2]-AIQY1 &
          Y1<= quantile(Y1, na.rm = TRUE)[4]+AIQY1 & Y1 >= 0 &
          Y2>= quantile(Y2, na.rm = TRUE)[2]-AIQY2 &
          Y2<= quantile(Y2, na.rm = TRUE)[4]+AIQY2  & Y2>=0)
          
  dad
}

## Exemplo de uso
 dad <- gerar_dados(mat_1=201610658, mat_2=201620028, mat_3=201610329)
 dad_rl <- gerar_dados_rl(mat_1=201511463, mat_2=201420373, mat_3=201420378)
# 1.Apresenta��es Tabulares e Gr�ficas
# 1.1.Diagrama de caixa (boxplot) para Y1 e Y2
# 1.1.1.Antes e Ap�s elimina��o de poss�veis outliers - sem distin��o de sexo:

# Defini��o do layout do plot (matriz com 2 colunas)
 layout(matrix(1:2, ncol=2))

# Diagrama de caixa de Y1 e Y2 antes da elimina��o de outliers
 boxplot(dad$Y1, dad$Y2, names=c("Y1", "Y2"), main="Antes")

# Remo��o de outliers e valores reais negativos
 for (i in 1:3){
     dad <- remove_outliers(dad)  
 }                 
## Diagrama de caixa de Y1 e Y2 depois da elimina��o de outliers
 boxplot(dad$Y1, dad$Y2, names=c("Y1", "Y2"), main="Ap�s")                  
#
# 1.1.2.Y1 e Y2: ap�s elimina��o de poss�veis outliers - com distin��o de sexo:
#
## Diagrama de caixa de Y1 e Y2 ap�s elimina��o de outliers, com distin��o de sexo  
 masc <- subset(dad, Sexo == "M", select = c("Y1", "Y2"))
 fem <- subset(dad, Sexo == "F", select = c("Y1", "Y2"))
 boxplot(masc$Y1, masc$Y2, names=c("Y1", "Y2"), main="Masculino")
 boxplot(fem$Y1, fem$Y2, names=c("Y1", "Y2"), main="Feminino")
#
# 1.2.Y1
# 1.2.1.Apresenta��es Tabulares
#
## Tabela de distribui��o de frequ�ncia de Y1 (un) (sexo masculino)
 fdtY1masc <- fdt(masc$Y1)
 fdtY2masc <- fdt(masc$Y2)
 fdtY1mascLaTeX <- latex.fdt(fdtY1masc, col=c(1, 2, 4, 6))
#
## Tabela de distribui��o de frequ�ncia de Y1 (un) (sexo masculino)
 fdtY1fem <- fdt(fem$Y1)
 fdtY2fem <- fdt(fem$Y2)
 fdtY1femLaTeX <- latex.fdt(fdtY1fem, col=c(1, 2, 4, 6))

### 1.2.2.Histograma e pol�gono de frequ�ncia acumulada:
#
## Histograma e pol�gono de frequ�ncia acumulada de Y1 (un) (sexo masculino)
 plot(fdtY1masc, xlab="Y1", ylab="Frequ�ncia")
 plot(fdtY1masc, xlab="Y1", type="cfp", ylab="Frequ�ncia acumulada", col="black")
## Histograma e pol�gono de frequ�ncia acumulada de Y1 (un) (sexo feminino)
 plot(fdtY1fem, xlab="Y1", ylab="Frequ�ncia")
 plot(fdtY1fem, xlab="Y1", type="cfp", ylab="Frequ�ncia acumulada", col="black")
 
### 2.AED: Medidas estat�sticas b�sicas
# 2.1.Tend�ncia Central: M�dia e Mediana

mean(fdtY1masc)
median(fdtY1masc)
mean(fdtY2masc)
median(fdtY2masc)
mean(fdtY1fem)
median(fdtY1fem)
mean(fdtY2fem)
median(fdtY2fem)

# 2.2.Posi��o: Quartis e Decis
quantile(masc$Y1, seq(.25,.75, by=0.25))
quantile(masc$Y2, seq(.25,.75, by=0.25))
quantile(fem$Y1, seq(.25,.75, by=0.25))
quantile(fem$Y2, seq(.25,.75, by=0.25))

quantile(masc$Y1, seq(0, .9, by=0.1))
quantile(masc$Y2, seq(0, .9, by=0.1))
quantile(fem$Y1, seq(0, .9, by=0.1))
quantile(fem$Y2, seq(0, .9, by=0.1))


# 2.3.Dispers�o: Amplitude total, vari�ncia, desvio padr�o e coeficiente de varia��o

diff(range(masc$Y1))
diff(range(masc$Y2))
diff(range(fem$Y1))
diff(range(fem$Y2))

var(masc$Y1)
var(masc$Y2)
var(fem$Y1)
var(fem$Y2)

#Desvio Padr�o

sd(masc$Y1)
sd(masc$Y2)
sd(fem$Y1)
sd(fem$Y2)

#Coeficiente de Varia��o

(sd(masc$Y1))/mean(masc$Y1)*100
(sd(masc$Y2))/mean(masc$Y2)*100

(sd(fem$Y1))/mean(fem$Y1)*100
(sd(fem$Y2))/mean(fem$Y2)*100


#         3 . AED: Medidas estat�sticas de associa��o e regress�o linear

#     3.1 Associa��o (1.5)
#			3.1.1.Estimativas: covari�ncia e correla��o linerar simples

#Matriz de Covari�ncia Masculina
cov(masc$Y1, masc$Y1)
cov(masc$Y1, masc$Y2)
cov(masc$Y2, masc$Y1)
cov(masc$Y2, masc$Y2)

#Matriz de Covari�ncia Feminina
cov(fem$Y1, fem$Y1)
cov(fem$Y1, fem$Y2)
cov(fem$Y2, fem$Y1)
cov(fem$Y2, fem$Y2)

#Matriz de Correla��es lineares Masculina
cor(masc$Y1, masc$Y1)
cor(masc$Y1, masc$Y2)
cor(masc$Y2, masc$Y1)
cor(masc$Y2, masc$Y2)

#Matriz de Correla��es lineares Feminina
cor(fem$Y1, fem$Y1)
cor(fem$Y1, fem$Y2)
cor(fem$Y2, fem$Y1)
cor(fem$Y2, fem$Y2)

# 3.1.2.Diagrama de dispers�o dos dados
plot(masc, main= 'Masculino')
plot(fem, main= 'Feminino')

#	3.1.3.Compara��o de estudos semelhantes
# Para comparar associa��es entre as vari�veis de ambos os estudos seria recomendada a medida de CORRELA��O, porque essa n�o � influenciada por unidade de medida.

# 3.2.Regress�o linear
X <- dad_rl$X                                           
Y <- dad_rl$Y

# 3.2.1.Ajustamento
summary(lm(Y ~ X))
xtable(lm(Y ~ X))
summary(lm(Y ~ X + I(X^2)))
xtable(lm(Y ~ X + I(X^2)))

# 3.2.2.Verificando o gr�fico de todos os modelos
layout(matrix(1:1, ncol=1))
plot(Y ~ X, xlab="X, un", ylab="Y, un", pch=19, col='blue', lwd=4)

# Modelo linear de 1 grau: com intercepto
lines(fitted(lm(Y ~ X)) ~ X,
      col='red', lwd=2)

# Modelo linear de 2 grau: com intercepto
lines(fitted(lm(Y ~ X + I(X^2))) ~ X,
      col='darkgreen', lwd=2)

text(x=7.5, y=7.5, 'Y = 3.3261 + 1.1140X', col='red')
text(x=6.07, y=7.8, '^', col='red')
text(x=7.5, y=6.8, paste('r�',' = 0.8972', sep=''), col='red')

text(x=7.5, y=5.5, 'Y = 1.811 + 2.1367 - 0.1023X', col='darkgreen')
text(x=5.52, y=5.8, '^', col='darkgreen')
text(x=7.5, y=4.8, paste('r�',' = 0.9569', sep=''), col='darkgreen')
