## ===========================================================================
## Nome    : funcão geradora de dados: gerar_dados
## Autor   : José Cláudio Faria/DCET/UESC
## Data    : 18/07/2016 11:31:41
## Versão  : v8
## Objetivo: Gerar dados (data.frame, com estrutura de covariância) para as
##           avaliações práticas de análise exploratória de dados dos cursos 
##           de introdução à estatística
## Require : Matrix, mvtnorm
## email   : <<<joseclaudio.faria@gmail.com>>>
## ===========================================================================
## Observações:
## 1- Muito cuidado ao informar as matrículas: a geração dos dados para
## análise (e subsequente correção) dependem dessa informação correta.
## As matrículas informadas devem ser obrigatoriamente as do grupo.
## 
## 2- Grupos com menos que 3 alunos: repetir a(s) última(s) matrícula(s)
## para a(s) matrícula(s) restante(s).
## 
## 3- Se encontrar algum problema com as matríclas informadas, tente alterar
## alguma das matrículas. Nesse caso não esquecer de comunicar ao professor
## (por escrito) no corpo da prova.
## 
## 4- A função está intencionalmente mal documentada.
## ===========================================================================
#install.packages('fdth')
#install.packages('Matrix')
#install.packages('mvtnorm')
#install.packages('xtable')
library(fdth)
library(Matrix)
library(mvtnorm)
library(xtable)
## Função que gera 2e3 dados de acordo com as matrículas informadas
gerar_dados <- function(mat_1=NULL,
                        mat_2=NULL,
                        mat_3=NULL,
                        n=2e3)
{
  #Verifica se as matriculas informadas são números
  stopifnot(is.numeric(mat_1) &
            is.numeric(mat_2) &
            is.numeric(mat_3))
  
  #Semente para geração aleatória de dados
  set.seed(mat_1 +
           mat_2 +
           mat_3)
  
  m_1 <- runif(1,
               min=20,
               max=40)

  m_2 <- runif(1,             
               min=20,
               max=40)

  ## Categórica                            
  n_cat_1 <- n/10 * sample(4:8,
                           1)

  ## Matriz de variâncias e covariâncias
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

  ## Escala categórica
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
# 1.Apresentações Tabulares e Gráficas
# 1.1.Diagrama de caixa (boxplot) para Y1 e Y2
# 1.1.1.Antes e Após eliminação de possíveis outliers - sem distinção de sexo:

# Definição do layout do plot (matriz com 2 colunas)
 layout(matrix(1:2, ncol=2))

# Diagrama de caixa de Y1 e Y2 antes da eliminação de outliers
 boxplot(dad$Y1, dad$Y2, names=c("Y1", "Y2"), main="Antes")

# Remoção de outliers e valores reais negativos
 for (i in 1:3){
     dad <- remove_outliers(dad)  
 }                 
## Diagrama de caixa de Y1 e Y2 depois da eliminação de outliers
 boxplot(dad$Y1, dad$Y2, names=c("Y1", "Y2"), main="Após")                  
#
# 1.1.2.Y1 e Y2: após eliminação de possíveis outliers - com distinção de sexo:
#
## Diagrama de caixa de Y1 e Y2 após eliminação de outliers, com distinção de sexo  
 masc <- subset(dad, Sexo == "M", select = c("Y1", "Y2"))
 fem <- subset(dad, Sexo == "F", select = c("Y1", "Y2"))
 boxplot(masc$Y1, masc$Y2, names=c("Y1", "Y2"), main="Masculino")
 boxplot(fem$Y1, fem$Y2, names=c("Y1", "Y2"), main="Feminino")
#
# 1.2.Y1
# 1.2.1.Apresentações Tabulares
#
## Tabela de distribuição de frequência de Y1 (un) (sexo masculino)
 fdtY1masc <- fdt(masc$Y1)
 fdtY2masc <- fdt(masc$Y2)
 fdtY1mascLaTeX <- latex.fdt(fdtY1masc, col=c(1, 2, 4, 6))
#
## Tabela de distribuição de frequência de Y1 (un) (sexo masculino)
 fdtY1fem <- fdt(fem$Y1)
 fdtY2fem <- fdt(fem$Y2)
 fdtY1femLaTeX <- latex.fdt(fdtY1fem, col=c(1, 2, 4, 6))

### 1.2.2.Histograma e polígono de frequência acumulada:
#
## Histograma e polígono de frequência acumulada de Y1 (un) (sexo masculino)
 plot(fdtY1masc, xlab="Y1", ylab="Frequência")
 plot(fdtY1masc, xlab="Y1", type="cfp", ylab="Frequência acumulada", col="black")
## Histograma e polígono de frequência acumulada de Y1 (un) (sexo feminino)
 plot(fdtY1fem, xlab="Y1", ylab="Frequência")
 plot(fdtY1fem, xlab="Y1", type="cfp", ylab="Frequência acumulada", col="black")
 
### 2.AED: Medidas estatísticas básicas
# 2.1.Tendência Central: Média e Mediana

mean(fdtY1masc)
median(fdtY1masc)
mean(fdtY2masc)
median(fdtY2masc)
mean(fdtY1fem)
median(fdtY1fem)
mean(fdtY2fem)
median(fdtY2fem)

# 2.2.Posição: Quartis e Decis
quantile(masc$Y1, seq(.25,.75, by=0.25))
quantile(masc$Y2, seq(.25,.75, by=0.25))
quantile(fem$Y1, seq(.25,.75, by=0.25))
quantile(fem$Y2, seq(.25,.75, by=0.25))

quantile(masc$Y1, seq(0, .9, by=0.1))
quantile(masc$Y2, seq(0, .9, by=0.1))
quantile(fem$Y1, seq(0, .9, by=0.1))
quantile(fem$Y2, seq(0, .9, by=0.1))


# 2.3.Dispersão: Amplitude total, variância, desvio padrão e coeficiente de variação

diff(range(masc$Y1))
diff(range(masc$Y2))
diff(range(fem$Y1))
diff(range(fem$Y2))

var(masc$Y1)
var(masc$Y2)
var(fem$Y1)
var(fem$Y2)

#Desvio Padrão

sd(masc$Y1)
sd(masc$Y2)
sd(fem$Y1)
sd(fem$Y2)

#Coeficiente de Variação

(sd(masc$Y1))/mean(masc$Y1)*100
(sd(masc$Y2))/mean(masc$Y2)*100

(sd(fem$Y1))/mean(fem$Y1)*100
(sd(fem$Y2))/mean(fem$Y2)*100


#         3 . AED: Medidas estatísticas de associação e regressão linear

#     3.1 Associação (1.5)
#			3.1.1.Estimativas: covariância e correlação linerar simples

#Matriz de Covariância Masculina
cov(masc$Y1, masc$Y1)
cov(masc$Y1, masc$Y2)
cov(masc$Y2, masc$Y1)
cov(masc$Y2, masc$Y2)

#Matriz de Covariância Feminina
cov(fem$Y1, fem$Y1)
cov(fem$Y1, fem$Y2)
cov(fem$Y2, fem$Y1)
cov(fem$Y2, fem$Y2)

#Matriz de Correlações lineares Masculina
cor(masc$Y1, masc$Y1)
cor(masc$Y1, masc$Y2)
cor(masc$Y2, masc$Y1)
cor(masc$Y2, masc$Y2)

#Matriz de Correlações lineares Feminina
cor(fem$Y1, fem$Y1)
cor(fem$Y1, fem$Y2)
cor(fem$Y2, fem$Y1)
cor(fem$Y2, fem$Y2)

# 3.1.2.Diagrama de dispersão dos dados
plot(masc, main= 'Masculino')
plot(fem, main= 'Feminino')

#	3.1.3.Comparação de estudos semelhantes
# Para comparar associações entre as variáveis de ambos os estudos seria recomendada a medida de CORRELAÇÃO, porque essa não é influenciada por unidade de medida.

# 3.2.Regressão linear
X <- dad_rl$X                                           
Y <- dad_rl$Y

# 3.2.1.Ajustamento
summary(lm(Y ~ X))
xtable(lm(Y ~ X))
summary(lm(Y ~ X + I(X^2)))
xtable(lm(Y ~ X + I(X^2)))

# 3.2.2.Verificando o gráfico de todos os modelos
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
text(x=7.5, y=6.8, paste('r²',' = 0.8972', sep=''), col='red')

text(x=7.5, y=5.5, 'Y = 1.811 + 2.1367 - 0.1023X', col='darkgreen')
text(x=5.52, y=5.8, '^', col='darkgreen')
text(x=7.5, y=4.8, paste('r²',' = 0.9569', sep=''), col='darkgreen')
