# Regresión: Abalone
# autor: Ricardo Ignacio Shepstone Aramburu 77553533V

# Importamos kknn
require(kknn)

# cargamos dataset 
abalone.reg <-read.csv("Input/abalone/abalone.dat", comment.char="@", header = FALSE)
# Cambiamos nombres de las variables
n <- length(names(abalone.reg)) - 1
names(abalone.reg)[1:n] <- paste ("X", 1:n, sep="")
names(abalone.reg)[n+1] <- "Y"

str(abalone)


# 1. Obtención de modelos lineales simples

# X2 (Length):
fitX2=lm(Y~X2,data=abalone.reg)
summary(fitX2)
# 0.3099, 0.3098

# X3 (Diameter):
fitX3=lm(Y~X3,data=abalone.reg)
summary(fitX3)
#0.3302, 0.3301

# X4 (Height):
fitX4=lm(Y~X4,data=abalone.reg)
summary(fitX4)
#0.3108, 0.3106

# X5 (Whole_weight):
fitX5=lm(Y~X5,data=abalone.reg)
summary(fitX5)
#0.292, 0.2912

# X8 (Shell_weight):
fitX8=lm(Y~X8,data=abalone.reg)
summary(fitX8)
#0.3938,0.3937

# 2. Obtención del modelo lineal múltiple

# Con todas las variables:
fit1=lm(Y~., abalone.reg)
summary(fit1)


# Pruebo descartando la variable X2 que tiene un p-valor bajo
fit2=lm(Y~.-X2, abalone.reg)
summary(fit2)

# R cuadrado y R cuadrado ajustado han bajado un poco, aquí las variables ya tienen un p-valor bastante bajo
# probamos quitar X4 que es la que tiene el valor más alto de todas
fit3=lm(Y~.-X2-X4, abalone.reg)
summary(fit3)
# R cuadrado y el ajustado bajan poco. Probamos introducir téminos de interacción
fit4=lm(Y~.-X2-X4+X1*X3, abalone.reg)
summary(fit4)


# Diameter con Shell_weight
fit5=lm(Y~.-X2-X4+X3*X8, abalone.reg)
summary(fit5)


# Comprobamos términos de interacción entre las variables de peso
fit6=lm(Y~.-X2-X4+X3*X8+X5*X6, abalone.reg)
summary(fit6)

# Mejora notablemente  

fit7=lm(Y~.-X2-X4+X3*X8+I(X5/X6), abalone.reg)
summary(fit7)
# Mejora más que el anterior

# Añadimos términos no lineales
fit8=lm(Y~.-X2-X4+X3*X8+I(X5/X6)+I(sqrt(X8)), abalone.reg)
summary(fit8)
# No mejora mucho


fit9=lm(Y~.-X2-X4+X3*X8+I(X5/X6)+I(log(X6)), abalone.reg)
summary(fit9)
# mejora poco




# 3. Aplicar k-nn y el mejor modelo obtenido con validación cruzada

nombre <- "Input/abalone/abalone"

#------------- 5-fold cross-validation KNN todas las variables

run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))

# Mostramos resultados
print('Resultados modelo knn con todas las variables')
print(knnMSEtrain)
print(knnMSEtest)




#------------- 5-fold cross-validation LM de las variables seleccionadas

run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=lm(Y~.-X2-X4+X3*X8+I(X5/X6),x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

# Mostramos resultados
print('Resultados modelo lineal con las variables seleccionadas')
print(lmMSEtrain)
print(lmMSEtest)


# 4. Comparativa de algoritmos de regresión múltiple

# Lo primero es obtener los resultados para el algoritmo de regresión múltiple con todas las variables

#------------- 5-fold cross-validation LM todas las variables

run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain2<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest2<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

# Mostramos resultados
print('Resultados modelo lineal con todas las variables')
print(lmMSEtrain2)
print(lmMSEtest2)




#leemos la tabla con los errores medios de test
resultados <- read.csv("input/Tablas/regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]

#leemos la tabla con los errores medios de entrenamiento
resultados <- read.csv("input/Tablas/regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) <- resultados[,1]


# sustituimos valores obtenidos
# en train
tablatra['abalone','out_train_lm'] <- lmMSEtrain2
tablatra['abalone','out_train_kknn'] <- knnMSEtrain
# en test
tablatst['abalone','out_test_lm'] <- lmMSEtest2
tablatst['abalone','out_test_kknn'] <- knnMSEtest


# Normalizamos las tablas con el código propuesto
##TABLA NORMALIZADA - lm (other) vs knn (ref) para WILCOXON
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
# train
difs.tra <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2.tra <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, 	abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2.tra) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2.tra)


# Test
difs.tst <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2.tst <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, 	abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2.tst) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2.tst)

# Aplicamos test de WILCOXON entre knn y lm
#Aplicación del test de WILCOXON
# subconjunto de train
LMvsKNNtra <- wilcox.test(wilc_1_2.tra[,1], wilc_1_2.tra[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtra$statistic
pvalue <- LMvsKNNtra$p.value
LMvsKNNtra <- wilcox.test(wilc_1_2.tra[,2], wilc_1_2.tra[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtra$statistic
Rmas
Rmenos
pvalue


# subconjunto de test
LMvsKNNtst <- wilcox.test(wilc_1_2.tst[,1], wilc_1_2.tst[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2.tst[,2], wilc_1_2.tst[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue


# Aplicamos test de Friedman para comparar los tres algoritmos.
#Aplicación del test de Friedman
# Para train
test_friedman.tra <- friedman.test(as.matrix(tablatra))
test_friedman.tra
# para test
test_friedman.tst <- friedman.test(as.matrix(tablatst))
test_friedman.tst


#Aplicación del test post-hoc de HOLM
# train
tam.tra <- dim(tablatra)
groups.tra <- rep(1:tam.tra[2], each=tam.tra[1])
pairwise.wilcox.test(as.matrix(tablatra), groups.tra, p.adjust = "holm", paired = TRUE)

# test
tam.tst <- dim(tablatst)
groups.tst <- rep(1:tam.tst[2], each=tam.tst[1])
pairwise.wilcox.test(as.matrix(tablatst), groups.tst, p.adjust = "holm", paired = TRUE)
