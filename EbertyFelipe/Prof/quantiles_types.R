##===============================================================================
## Título: Comparação entre os tipos da função quantile
## Autor : José Cláudio Faria
## Data  : 2013/04/28 - 11:36:42
## Versão: v1
## Objetivos: Comparar os tipos (types) da função quantile
##===============================================================================
## 1- Detalhar o argumento type da função quantile
## 2- Comparar os tipos com outras medidas de posição disponíveis no R
##===============================================================================

## Criando vetores
v.par <- 1:4  # par
v.imp <- 1:5  # impar


##--------------------------------------------- 
## quantile vs. fivenum
##--------------------------------------------- 
## Vetor par
for (i in 1:9) {
  if (i == 1)
    cat('\n',
        'fivenum =',
        fivenum(v.par),
        '\n')

  cat('\n',
      'type =',
      i,
      '\n')

  cat(quantile(v.par,
               type=i),
      '\n')

  cat(all(fivenum(v.par) == quantile(v.par,
                                     type=i)),
      '\n')
}
## Observa-se que os tipos 2 e 5 são iguais!

## Vetor impar
for (i in 1:9) {
  if (i == 1)
    cat('\n',
        'fivenum =',
        fivenum(v.imp),
        '\n')

  cat('\n',
      'type =',
      i,
      '\n')

  cat(quantile(v.imp,
               type=i),
      '\n')

  cat(all(fivenum(v.imp) == quantile(v.imp,
                                     type=i)),
      '\n')
}
## Observa-se que os tipos 1, 2 e 7 são iguais!
## Portanto, o tipo 2, para os casos comuns, bate com a teoria
## clássica dos quantis ensinadas em aulas.


##--------------------------------------------- 
## quantile vs. boxplot.stats
##--------------------------------------------- 
## Vetor par
for (i in 1:9) {
  if (i == 1)
    cat('\n',
        'quantile do boxplot =',
        boxplot.stats(v.imp)$stats[2:4],
        '\n')

  cat('\n',
      'type =',
      i,
      '\n')

  cat(quantile(v.par,
               type=i)[2:4],
      '\n')

  cat(all(boxplot.stats(v.par)$stats[2:4] == quantile(v.par,
                                                      type=i)[2:4]),
      '\n')
}
## Observa-se que os tipos 2 e 5 são iguais!


## Vetor impar
for (i in 1:9) {
  if (i == 1)
    cat('\n',
        'quantile do boxplot =',
        boxplot.stats(v.imp)$stats[2:4],
        '\n')

  cat('\n',
      'type =',
      i,
      '\n')

  cat(quantile(v.imp,
               type=i)[2:4],
      '\n')

  cat(all(boxplot.stats(v.imp)$stats[2:4] == quantile(v.imp,
                                                      type=i)[2:4]),
      '\n')
}
## Observa-se que os tipos 1, 2 e 7 são iguais!
## Portanto, o tipo 2, para os casos comuns, bate com a teoria
## clássica dos quantis ensinadas em aulas.

## Conclusão: para padronizar, usaremos type=2 para a função quantile
## no decorrer da disciplina!!!

##--------------------------------------------- 
## quantile vs. grafico boxplot
##--------------------------------------------- 
## Vetor par
qts <- quantile(v.par,
                type=2)[2:4]

boxplot(v.par,
        main='Vetor par',
        ylim=c(0, 5),
        col='darkgreen')

text(x=.6,
     y=qts,
     qts,
     col='red',
     cex=2)

for (i in 1:length(qts))
  abline(qts[i],
         0,
         col='red',
         lty=2)

## Vetor impar
qts <- quantile(v.imp,
                type=2)[2:4]

boxplot(v.imp,
        main='Vetor impar',
        ylim=c(0, 6),
        col='darkgreen')

text(x=.6,
     y=qts,
     qts,
     col='red',
     cex=2)

for (i in 1:length(qts))
  abline(qts[i],
         0,
         col='red',
         lty=2)


##------------------------------------------------- 
## Distância inter quartílica via quantile vs. IQR
##-------------------------------------------------
## Vetor par
diq <- as.numeric(quantile(v.par,
                           type=2)[4] - quantile(v.par,
                                                 type=2)[2])

diq == IQR(v.par,
           type=2)

## Vetor impar
diq <- as.numeric(quantile(v.imp,
                           type=2)[4] - quantile(v.imp,
                                                 type=2)[2])

diq == IQR(v.imp,
           type=2)
