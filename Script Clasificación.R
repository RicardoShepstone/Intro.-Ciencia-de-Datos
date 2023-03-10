# Clasificación: Vehicle
# autor: Ricardo Ignacio Shepstone Aramburu 77553533V

require(tidyverse)
require(readr)
require(caret)
require(ggplot2)

# Cargamos dataset

vehicle.raw <- read.csv("Input/vehicle/vehicle.dat", comment.char="@", header=FALSE)
vehicle <- vehicle.raw

# renombramos las variables para trabajar con ellas
colnames(vehicle) <- c('Compactness','Circularity','Distance_circularity','Radius_ratio',
                       'Praxis_aspect_ratio', 'Max_length_aspect_ratio', 'Scatter_ratio',
                       'Elongatedness', 'Praxis_rectangular', 'Length_rectangular',
                       'Major_variance', 'Minor_variance', 'Gyration_radius',
                       'Major_skewness', 'Minor_skewness', 'Minor_kurtosis', 
                       'Major_kurtosis', 'Hollows_ratio', 'Class')

vehicle[,'Class'] <- str_remove_all(vehicle$Class," ") 
head(vehicle)

nombre <- "Input/vehicle/vehicle"


# Aplicamos knn con validación cruzada para distintos valores de k

#------------- 10-fold cross-validation KNN todas las variables
run_knn_k_fold <- function(kmax, nfolds, x){
  set.seed(1)
  accuracy.df <- data.frame('k'=1:kmax, 'train'=1:kmax, 'test'=1:kmax)
  for (k in 1:kmax){
    accuracy.train <- 1:nfolds
    accuracy.test <- 1:nfolds
    for (i in 1:nfolds){
      file <- paste(x, "-10-", i, "tra.dat", sep="")
      x_tra <- read.csv(file, comment.char="@", header=FALSE)
      file <- paste(x, "-10-", i, "tst.dat", sep="")
      x_tst <- read.csv(file, comment.char="@", header=FALSE)
      In <- length(names(x_tra)) - 1
      names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
      names(x_tra)[In+1] <- "Y"
      names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
      names(x_tst)[In+1] <- "Y"
  
  
      knnModel <- train(x=x_tra %>% select(-Y), y = x_tra[,"Y"],
                          method = "knn", preProc = c("center", "scale"), 
                          metric="Accuracy", tuneGrid = data.frame(k=k))
    
      knnPred <- predict(knnModel, newdata = x_tst %>% select(-Y))
      cfm <- table(knnPred, x_tst[,'Y'])
      accuracy.train[i] <- knnModel$results$Accuracy
      accuracy.test[i] <- sum(diag(cfm))/length(x_tst[,'Y'])
    }
    accuracy.df[k,'train']<- mean(accuracy.train)
    accuracy.df[k,'test']<- mean(accuracy.test)
  }
accuracy.df
}

# llamamos a la función
resultados <- run_knn_k_fold(20, 10, nombre)
rewsultados2 <- run_knn_k_fold(20, 10, nombre)


# Mostramos los resultados
colors <- c('train'='red', 'test'='blue')
ggplot(resultados, aes(x=k))+ 
  geom_line(aes(y=train, color="train")) +
  geom_line(aes(y=test, color="test")) +
  labs(title="Estudio de knn", y="Accuracy",  color = "Legend") +
  scale_x_continuous(breaks=seq(0, 21, 1)) +
  scale_y_continuous(breaks=seq(0, 1, 0.01)) 
  
# Mejores resultados 
resultados[which.max(resultados$train),]
resultados[which.max(resultados$test),]


# Realizamos test de shapiro por categorías a cada variable

# Compactness
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Compactness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Compactness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Compactness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Compactness)
shapiro.test(p1[,1])

# Circularity
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Circularity)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Circularity)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Circularity)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Circularity)
shapiro.test(p1[,1])

# Distance_circularity
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Distance_circularity)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Distance_circularity)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Distance_circularity)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Distance_circularity)
shapiro.test(p1[,1])

# Radius_ratio
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Radius_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Radius_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Radius_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Radius_ratio)
shapiro.test(p1[,1])

# Praxis_aspect_ratio
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Praxis_aspect_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Praxis_aspect_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Praxis_aspect_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Praxis_aspect_ratio)
shapiro.test(p1[,1])


# Max_length_aspect_ratio
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Max_length_aspect_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Max_length_aspect_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Max_length_aspect_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Max_length_aspect_ratio)
shapiro.test(p1[,1])


# Scatter_ratio
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Scatter_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Scatter_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Scatter_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Scatter_ratio)
shapiro.test(p1[,1])


# Elongatedness
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Elongatedness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Elongatedness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Elongatedness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Elongatedness)
shapiro.test(p1[,1])



# Praxis_rectangular
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Praxis_rectangular)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Praxis_rectangular)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Praxis_rectangular)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Praxis_rectangular)
shapiro.test(p1[,1])




# Length_rectangular
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Length_rectangular)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Length_rectangular)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Length_rectangular)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Length_rectangular)
shapiro.test(p1[,1])




# Major_variance
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Major_variance)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Major_variance)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Major_variance)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Major_variance)
shapiro.test(p1[,1])




# Minor_variance
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Minor_variance)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Minor_variance)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Minor_variance)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Minor_variance)
shapiro.test(p1[,1])




# Gyration_radius
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Gyration_radius)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Gyration_radius)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Gyration_radius)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Gyration_radius)
shapiro.test(p1[,1])




# Major_skewness
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Major_skewness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Major_skewness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Major_skewness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Major_skewness)
shapiro.test(p1[,1])




# Minor_skewness
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Minor_skewness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Minor_skewness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Minor_skewness)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Minor_skewness)
shapiro.test(p1[,1])




# Minor_kurtosis
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Minor_kurtosis)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Minor_kurtosis)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Minor_kurtosis)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Minor_kurtosis)
shapiro.test(p1[,1])




# Major_kurtosis
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Major_kurtosis)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Major_kurtosis)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Major_kurtosis)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Major_kurtosis)
shapiro.test(p1[,1])




# Hollows_ratio
p1 <- vehicle %>% filter(Class == "bus") %>% dplyr::select(Hollows_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "opel") %>% dplyr::select(Hollows_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "saab") %>% dplyr::select(Hollows_ratio)
shapiro.test(p1[,1])

p1 <- vehicle %>% filter(Class == "van") %>% dplyr::select(Hollows_ratio)
shapiro.test(p1[,1])



# Comprobamos las varianzas
var(vehicle %>% filter(Class == "bus") %>% dplyr::select(-19))
var(vehicle %>% filter(Class == "opel") %>% dplyr::select(-19))
var(vehicle %>% filter(Class == "saab") %>% dplyr::select(-19))
var(vehicle %>% filter(Class == "van") %>% dplyr::select(-19))


# Aplicamos tests para estudiar homogeneidad
leveneTest(Compactness ~ Class, vehicle)
leveneTest(Circularity ~ Class, vehicle)
leveneTest(Distance_circularity ~ Class, vehicle)
leveneTest(Radius_ratio ~ Class, vehicle)
bartlett.test(Praxis_aspect_ratio ~ Class, vehicle)
leveneTest(Max_length_aspect_ratio ~ Class, vehicle)
leveneTest(Scatter_ratio ~ Class, vehicle)
leveneTest(Elongatedness ~ Class, vehicle)
leveneTest(Praxis_rectangular ~ Class, vehicle)
leveneTest(Length_rectangular ~ Class, vehicle)
leveneTest(Major_variance ~ Class, vehicle)
leveneTest(Minor_variance ~ Class, vehicle)
leveneTest(Gyration_radius ~ Class, vehicle)
leveneTest(Major_skewness ~ Class, vehicle)
leveneTest(Minor_skewness ~ Class, vehicle)
leveneTest(Minor_kurtosis ~ Class, vehicle)
leveneTest(Major_kurtosis ~ Class, vehicle)
leveneTest(Hollows_ratio ~ Class, vehicle)



# # función para LDA
run_lda_fold <- function(nfolds, x){
set.seed(1)
accuracy.df <- data.frame('fold'=1:nfolds, 'train'=1:nfolds, 'test'=1:nfolds)
for (i in 1:10){
  file <- paste(x, "-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  file <- paste(x, "-10-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  
  TrainData <- x_tra %>% select(-Y)
  TrainClasses <- x_tra %>% pull(Y)
  
  ldaModel <- train(TrainData, TrainClasses,
                    method = "lda", preProc = c("center", "scale"), 
                    metric="Accuracy", tuneLength = 10)
  
  ldaPred <- predict(ldaModel, newdata = x_tst %>% select(-Y))
  cfm <- table(ldaPred, x_tst[,'Y'])

  
  accuracy.df[i,'train']<- ldaModel$results$Accuracy
  accuracy.df[i,'test']<- sum(diag(cfm))/length(x_tst[,'Y'])
}
accuracy.df
}

# resultados
results.LDA <- run_lda_fold(10, nombre)

# para calcular la media 

LDA.train <- mean(results.LDA$train)
LDA.test <- mean(results.LDA$test)





# # función para QDA
run_qda_fold <- function(nfolds, x){
  set.seed(1)
  accuracy.df <- data.frame('fold'=1:nfolds, 'train'=1:nfolds, 'test'=1:nfolds)
  for (i in 1:10){
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    
    TrainData <- x_tra %>% select(-Y)
    TrainClasses <- x_tra %>% pull(Y)
    
    qdaModel <- train(TrainData, TrainClasses,
                      method = "qda", preProc = c("center", "scale"), 
                      metric="Accuracy", tuneLength = 10)
    
    qdaPred <- predict(qdaModel, newdata = x_tst %>% select(-Y))
    cfm <- table(qdaPred, x_tst[,'Y'])
    
    
    accuracy.df[i,'train']<- qdaModel$results$Accuracy
    accuracy.df[i,'test']<- sum(diag(cfm))/length(x_tst[,'Y'])
  }
  accuracy.df
}


results.QDA <- run_qda_fold(10, nombre)


# para calcular la media 
QDA.train <- mean(results.QDA$train)
QDA.test <- mean(results.QDA$test)



#leemos la tabla con la precisión media de test
resultados <- read.csv("input/Tablas/clasif_test_alumos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]

#leemos la tabla con la precisión media de entrenamiento
resultados <- read.csv("input/Tablas/clasif_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) <- resultados[,1]



# Normalizamos las tablas con el código propuesto
##TABLA NORMALIZADA - para WILCOXON
# + 0.1 porque wilcox R falla para valores == 0 en la tabla
# train
difs <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2.tra <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, 	abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2.tra) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2.tra)


# Test
difs.tst <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2.tst <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, 	abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2.tst) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2.tst)


#Aplicación del test de WILCOXON
# subconjunto de train
LDAvsKNNtra <- wilcox.test(wilc_1_2.tra[,1], wilc_1_2.tra[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LDAvsKNNtra$statistic
pvalue <- LDAvsKNNtra$p.value
LDAvsKNNtra <- wilcox.test(wilc_1_2.tra[,2], wilc_1_2.tra[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LDAvsKNNtra$statistic
Rmas
Rmenos
pvalue


# subconjunto de test
LDAvsKNNtst <- wilcox.test(wilc_1_2.tst[,1], wilc_1_2.tst[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LDAvsKNNtst$statistic
pvalue <- LDAvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2.tst[,2], wilc_1_2.tst[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LDAvsKNNtst$statistic
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
