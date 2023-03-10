# EDA Regresión: Abalone
# autor: Ricardo Ignacio Shepstone Aramburu 77553533V

require(tidyverse)
require(readr)
require(moments)
require(car)
require(corrplot)
require(fastDummies)
require(caret)
library(FactoMineR)
library(factoextra)
## Procesamiento de los datos

# Cargamos dataset
#abalone.raw <- read_csv("Input/abalone/abalone.dat", col_names = FALSE, skip = 13)

abalone.raw <- read.csv("Input/abalone/abalone.dat", comment.char="@", header=FALSE)

abalone <- abalone.raw

#comprobamos número de columnas, filas y si se han cargado bien los datos, la estructura de estos
nrow(abalone)
ncol(abalone)
str(abalone)
summary(abalone)


colnames(abalone)<-c('Sex', 'Length', 'Diameter', 'Height', 'Whole_weight', 'Shucked_weight', 'Viscera_weight', 'Shell_weight', 'Rings')
#abalone <-  mutate_at(abalone, vars(Sex, Rings), as.integer)
abalone <- mutate_if(abalone, is.double, funs(.*200))


# factores en Sex
abalone <- abalone %>% mutate(Sex=factor(Sex, levels = c(1,2,3), labels = c('M', 'F', 'I')))

# comprobamos
head(abalone)

# Así es como quedan las variables:
str(abalone)


# Comprobación de missing values
sum(is.na(abalone))
sum(is.null(abalone))
# El min de Height muestra un valor extraño (estos dos pueden ser missing values)
abalone %>% filter(Height==0)
# podemos eliminar estos dos valores, ya que tienen valores de loingitud y diámetro, pero no de altura
abalone <- abalone[-which(abalone$Height==0),]

summary(abalone)

# comprobamos si hay duplicados
sum(duplicated(abalone))

# Outliers en una dimensión
# Calculamos rango intercuartílico de todas las variables
abalone.IQR <- abalone %>% select(-Sex) %>% apply(2, IQR)
abalone.Quartiles <- abalone %>% select(-Sex) %>% apply(2, quantile,c(0.25,0.75))
Upper.limit <- abalone.Quartiles[2,]+1.5*abalone.IQR
Upper.limit
Lower.limit <- abalone.Quartiles[1,]-1.5*abalone.IQR
Lower.limit

# Obtenemos los outliers para cada variable y calculamos sus posiciones 
# Length
Length.outliers <- which(abalone$Length>Upper.limit['Length'] | abalone$Length<Lower.limit['Length'])
# Diameter
Diameter.outliers <- which(abalone$Diameter>Upper.limit['Diameter'] | abalone$Diameter<Lower.limit['Diameter'])
# Height
Height.outliers <- which(abalone$Height>Upper.limit['Height'] | abalone$Height<Lower.limit['Height'])
# Whole_weight
Whole_weight.outliers <- which(abalone$Whole_weight>Upper.limit['Whole_weight'] | abalone$Whole_weight<Lower.limit['Whole_weight'])
# Shucked_weight
Shucked_weight.outliers <- which(abalone$Shucked_weight>Upper.limit['Shucked_weight'] | abalone$Shucked_weight<Lower.limit['Shucked_weight'])
# Viscera_weight
Viscera_weight.outliers <- which(abalone$Viscera_weight>Upper.limit['Viscera_weight'] | abalone$Viscera_weight<Lower.limit['Viscera_weight'])
# Shell_weight
Shell_weight.outliers <- which(abalone$Shell_weight>Upper.limit['Shell_weight'] | abalone$Shell_weight<Lower.limit['Shell_weight'])
# Rings
Rings.outliers <- which(abalone$Rings>Upper.limit['Rings'] | abalone$Rings<Lower.limit['Rings'])

# Comprobamos cantidad de outliers 

abalone.outliers <- unique(c(Length.outliers, Diameter.outliers, Height.outliers, Whole_weight.outliers, Shucked_weight.outliers, Viscera_weight.outliers, Shell_weight.outliers, Rings.outliers))
length(abalone.outliers)


#########################################################################################################################

# Ignorar esta sección, es una prueba

# Vemos los outliers para cada variable
# Length
#Lenght.outliers2 <- abalone %>% filter(Length>Upper.limit['Length'] | Length<Lower.limit['Length'])
# Diameter
#Diameter.outliers2 <- abalone %>% filter(Diameter>Upper.limit['Diameter'] | Diameter<Lower.limit['Diameter'])
# Height
#Height.outliers2 <- abalone %>% filter(Height>Upper.limit['Height'] | Height<Lower.limit['Height'])
# Whole_weight
#Whole_weight.outliers2 <- abalone %>% filter(Whole_weight>Upper.limit['Whole_weight'] | Whole_weight<Lower.limit['Whole_weight'])
# Shucked_weight
#Shucked_weight.outliers2 <- abalone %>% filter(Shucked_weight>Upper.limit['Shucked_weight'] | Shucked_weight<Lower.limit['Shucked_weight'])
# Viscera_weight
#Viscera_weight.outliers2 <- abalone %>% filter(Viscera_weight>Upper.limit['Viscera_weight'] | Viscera_weight<Lower.limit['Viscera_weight'])
# Shell_weight
#Shell_weight.outliers2 <- abalone %>% filter(Shell_weight>Upper.limit['Shell_weight'] | Shell_weight<Lower.limit['Shell_weight'])
# Rings
#Rings.outliers2 <- abalone %>% filter(Rings>Upper.limit['Rings'] | Rings<Lower.limit['Rings'])

#prueba <- unique(rbind(Lenght.outliers, Diameter.outliers, Height.outliers, Whole_weight.outliers, Shucked_weight.outliers, Viscera_weight.outliers, Shell_weight.outliers, Rings.outliers))

#abalone[Length.outliers,]==Lenght.outliers2

############################################################################################################################


# valores extraños en infants
abalone %>% group_by(Sex) %>% summarise(min.Rings=min(Rings), max.rings=max(Rings))
# comentar


## Resumen de los datos
# Medidas de tendencia central

# Con summary() podemos obtener la media y cuartiles
summary(abalone)

# Para obtener estos valores:
# como lista:
abalone.mean.median <- abalone %>% select(-Sex) %>% apply(2,function(x){list(media=mean(x), mediana=median(x))})

# Como vector
abalone.mean <- abalone %>% select(-Sex) %>% map_dbl(mean)
abalone.median <- abalone %>% select(-Sex) %>% map_dbl(median)

abalone.mean
abalone.median

# Medidas de dispersión
# Para el Rango, máximo y mínimo:
abalone.min.max <- abalone %>% select(-Sex) %>% apply(2,range)
abalone.range <- abalone.min.max[2,]-abalone.min.max[1,]
abalone.min.max.range <- rbind(abalone.min.max, abalone.range)
rownames(abalone.min.max.range) <- c('min', 'max', 'range')
abalone.min.max.range             

# primer cuartil, tercer cuartil y rango intercuartílico.
abalone.Quartiles
abalone.IQR

# varianza, desviación típica y desviación absoluta de la mediana
abalone.var <- abalone %>% select(-Sex) %>% map_dbl(var)
abalone.sd <- abalone %>% select(-Sex) %>% map_dbl(sd)
abalone.mad <- abalone %>% select(-Sex) %>% map_dbl(mad)

abalone.var
abalone.sd 
abalone.mad

# Medidas de forma
abalone.skewness <- abalone %>% select(-Sex) %>% map_dbl(skewness)
abalone.kurtosis <- abalone %>% select(-Sex) %>% map_dbl(kurtosis)

abalone.skewness
abalone.kurtosis

abalone.skewness2 <- abalone[-abalone.outliers,] %>% select(-Sex) %>% map_dbl(skewness)
abalone.skewness2

# Comprobamos normalidad con shapiro

abalone.shapiro <- apply(abalone[-abalone.outliers,-1],2,shapiro.test)
abalone.shapiro

# visualización de los datos

# Para la variable Sex
counts.sex <- table(abalone$Sex)
barplot(counts.sex, main='Distribución por sexo', xlab='sexo',ylim = c(0,2000))


# Para Length
ggplot(abalone, aes(x=Length)) + 
  geom_histogram(bins=25, color='blue',fill='blue')+
  labs(x='Length', title = 'Histograma de la variable Length')+
  scale_y_continuous(breaks = seq(from=0, to=500, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=170, by=10))


ggplot(abalone, aes(y=Length)) + 
  geom_boxplot(fill='blue',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Length', title = 'Diagrama de cajas de la variable Length')+
  scale_y_continuous(breaks = seq(from=0, to=170, by=10))


ggplot(data=abalone, aes(x=Length, fill="blue"))+
  geom_density(stat="density", alpha=I(0.2), color= 'blue', fill='blue') +
  xlab("Length") + ylab("Density") + 
  ggtitle("Curva de densidad de Length")



# Para Diameter
ggplot(abalone, aes(x=Diameter)) + 
  geom_histogram(bins=25, color='blue',fill='blue')+
  labs(x='Diameter', title = 'Histograma de la variable Diameter')+
  scale_y_continuous(breaks = seq(from=0, to=500, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=170, by=10))


ggplot(abalone, aes(y=Diameter)) + 
  geom_boxplot(fill='blue',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Diameter', title = 'Diagrama de cajas de la variable Diameter')+
  scale_y_continuous(breaks = seq(from=0, to=170, by=10))


ggplot(data=abalone, aes(x=Diameter, fill="blue"))+
  geom_density(stat="density", alpha=I(0.2), color= 'blue', fill='blue') +
  xlab("Diameter") + ylab("Density") + 
  ggtitle("Curva de densidad de Diameter")


# Para Height
ggplot(abalone, aes(x=Height)) + 
  geom_histogram(bins=50, color='blue',fill='blue')+
  labs(x='Height', title = 'Histograma de la variable Height', subtitle = 'Con los outliers de Height')+
  scale_y_continuous(breaks = seq(from=0, to=1200, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=240, by=10))
# comentar que se ha tenido que coger un mayor número de bins por el outlier

# Sin outliers:
ggplot(abalone[-Height.outliers,], aes(x=Height)) + 
  geom_histogram(bins=20, color='blue',fill='blue')+
  labs(x='Height', title = 'Histograma de la variable Height', subtitle = 'Sin los outliers de Height')+
  scale_y_continuous(breaks = seq(from=0, to=500, by=20))+
  scale_x_continuous(breaks = seq(from=0, to=60, by=5))


# boxplot con outliers
ggplot(abalone, aes(y=Height)) + 
  geom_boxplot(fill='blue',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Height', title = 'Diagrama de cajas de la variable Height', subtitle = 'Con los outliers de Height')+
  scale_y_continuous(breaks = seq(from=0, to=250, by=10))
# boxplot sin outliers (quitar los calculados)
ggplot(abalone[-Height.outliers,], aes(y=Height)) + 
  geom_boxplot(fill='blue')+
  labs(y='Height', title = 'Diagrama de cajas de la variable Height', subtitle = 'Sin los outliers de Height')+
  scale_y_continuous(breaks = seq(from=0, to=250, by=10))

# curva densidad
ggplot(data=abalone, aes(x=Height, fill="blue"))+
  geom_density(stat="density", alpha=I(0.2), color= 'blue', fill='blue', subtitle = 'Con los outliers de Height') +
  xlab("Height") + ylab("Density") + 
  ggtitle("Curva de densidad de Height")

# Sin outliers
ggplot(data=abalone[-Height.outliers,], aes(x=Height, fill="blue"))+
  geom_density(stat="density", alpha=I(0.2), color= 'blue', fill='blue') +
  xlab("Height") + ylab("Density") + 
  ggtitle("Curva de densidad de Height", subtitle = 'Sin los outliers de Height')




# Whole_weight
ggplot(abalone, aes(x=Whole_weight)) + 
  geom_histogram(bins=24, color='green',fill='green')+
  labs(x='Whole_weight', title = 'Histograma de la variable Whole_weight')+
  scale_y_continuous(breaks = seq(from=0, to=400, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=600, by=50))


ggplot(abalone, aes(y=Whole_weight)) + 
  geom_boxplot(fill='green',outlier.colour = 'green', outlier.shape = 3)+
  labs(y='Whole_weight', title = 'Diagrama de cajas de la variable Whole_weight')+
  scale_y_continuous(breaks = seq(from=0, to=600, by=50))


ggplot(data=abalone, aes(x=Whole_weight, fill="green"))+
  geom_density(stat="density", alpha=I(0.2), color= 'green', fill='green') +
  xlab("Whole_weight") + ylab("Density") + 
  ggtitle("Curva de densidad de Whole_weight")

# Shucked_weight
ggplot(abalone, aes(x=Shucked_weight)) + 
  geom_histogram(bins=25, color='green',fill='green')+
  labs(x='Shucked_weight', title = 'Histograma de la variable Shucked_weight')+
  scale_y_continuous(breaks = seq(from=0, to=500, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=300, by=25))


ggplot(abalone, aes(y=Shucked_weight)) + 
  geom_boxplot(fill='green',outlier.colour = 'green', outlier.shape = 3)+
  labs(y='Shucked_weight', title = 'Diagrama de cajas de la variable Shucked_weight')+
  scale_y_continuous(breaks = seq(from=0, to=300, by=20))


ggplot(data=abalone, aes(x=Shucked_weight, fill="green"))+
  geom_density(stat="density", alpha=I(0.2), color= 'green', fill='green') +
  xlab("Shucked_weight") + ylab("Density") + 
  ggtitle("Curva de densidad de Shucked_weight")


# Viscera_weight
ggplot(abalone, aes(x=Viscera_weight)) + 
  geom_histogram(bins=25, color='green',fill='green')+
  labs(x='Viscera_weight', title = 'Histograma de la variable Viscera_weight')+
  scale_y_continuous(breaks = seq(from=0, to=450, by=25))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))


ggplot(abalone, aes(y=Viscera_weight)) + 
  geom_boxplot(fill='green',outlier.colour = 'green', outlier.shape = 3)+
  labs(y='Viscera_weight', title = 'Diagrama de cajas de la variable Viscera_weight')+
  scale_y_continuous(breaks = seq(from=0, to=160, by=10))


ggplot(data=abalone, aes(x=Viscera_weight, fill="green"))+
  geom_density(stat="density", alpha=I(0.2), color= 'green', fill='green') +
  xlab("Viscera_weight") + ylab("Density") + 
  ggtitle("Curva de densidad de Viscera_weight")


# Shell_weight
ggplot(abalone, aes(x=Shell_weight)) + 
  geom_histogram(bins=25, color='green',fill='green')+
  labs(x='Shell_weight', title = 'Histograma de la variable Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=500, by=25))+
  scale_x_continuous(breaks = seq(from=0, to=210, by=10))


ggplot(abalone, aes(y=Shell_weight)) + 
  geom_boxplot(fill='green',outlier.colour = 'green', outlier.shape = 3)+
  labs(y='Shell_weight', title = 'Diagrama de cajas de la variable Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=210, by=10))


ggplot(data=abalone, aes(x=Shell_weight, fill="green"))+
  geom_density(stat="density", alpha=I(0.2), color= 'green', fill='green') +
  xlab("Shell_weight") + ylab("Density") + 
  ggtitle("Curva de densidad de Shell_weight")

# Rings (variable de salida)
ggplot(abalone, aes(x=Rings)) + 
  geom_histogram(bins=length(unique(abalone$Rings)), color='Red',fill='Red')+
  labs(x='Rings', title = 'Histograma de la variable Rings')+
  scale_y_continuous(breaks = seq(from=0, to=700, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=30, by=1))


ggplot(abalone, aes(y=Rings)) + 
  geom_boxplot(fill='Red',outlier.colour = 'Red', outlier.shape = 3)+
  labs(y='Rings', title = 'Diagrama de cajas de la variable Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))


ggplot(data=abalone, aes(x=Rings, fill="Red"))+
  geom_density(stat="density", alpha=I(0.2), color= 'Red', fill='Red') +
  xlab("Rings") + ylab("Density") + 
  ggtitle("Curva de densidad de Rings")


# plots bivariables

# analizamos la variable Sex con respecto al resto, al ser esta variable la única categórica
# sex-Length

ggplot(abalone, aes(x=Length, fill=Sex)) + 
  geom_histogram(bins=30,  color='black', alpha=0.5, position='identity')+
  labs(x='Length', title = 'Histograma de Length en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=240, by=3))+
  scale_x_continuous(breaks = seq(from=0, to=170, by=10))

ggplot(abalone, aes(y=Length, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Length', title = 'Diagrama de cajas de la variable Length por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=170, by=10))


#sex-Diameter

ggplot(abalone, aes(x=Diameter, fill=Sex)) + 
  geom_histogram(bins=30,  color='black', alpha=0.5, position='identity')+
  labs(x='Diameter', title = 'Histograma de Diameter en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=170, by=10))

ggplot(abalone, aes(y=Diameter, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Diameter', title = 'Diagrama de cajas de la variable Diameter por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=170, by=10))


#sex-Height

#quitar outliers
ggplot(abalone[-Height.outliers,], aes(x=Height, fill=Sex)) + 
  geom_histogram(bins=21,  color='black', alpha=0.5, position='identity')+
  labs(x='Height', title = 'Histograma de Height en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=1500, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=250, by=20))

ggplot(abalone[-Height.outliers,], aes(y=Height, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Height', title = 'Diagrama de cajas de la variable Height por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=170, by=10))


#sex-Whole Weight

ggplot(abalone, aes(x=Whole_weight, fill=Sex)) + 
  geom_histogram(bins=30,  color='black', alpha=0.5, position='identity')+
  labs(x='Whole_weight', title = 'Histograma de Whole_weight en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=400, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=600, by=50))

ggplot(abalone, aes(y=Whole_weight, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Whole_weight', title = 'Diagrama de cajas de la variable Whole_weight por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=600, by=50))


#sex-Shucked_weight

ggplot(abalone, aes(x=Shucked_weight, fill=Sex)) + 
  geom_histogram(bins=30,  color='black', alpha=0.5, position='identity')+
  labs(x='Shucked_weight', title = 'Histograma de Shucked_weight en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=250, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=300, by=20))

ggplot(abalone, aes(y=Shucked_weight, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Shucked_weight', title = 'Diagrama de cajas de la variable Shucked_weight por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=300, by=20))

#Probar verlo sin outliers


#sex-Viscera_weight

ggplot(abalone, aes(x=Viscera_weight, fill=Sex)) + 
  geom_histogram(bins=30,  color='black', alpha=0.5, position='identity')+
  labs(x='Viscera_weight', title = 'Histograma de Viscera_weight en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=260, by=20))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))

ggplot(abalone, aes(y=Viscera_weight, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Viscera_weight', title = 'Diagrama de cajas de la variable Viscera_weight por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=170, by=10))



#sex-Shell_weight

ggplot(abalone, aes(x=Shell_weight, fill=Sex)) + 
  geom_histogram(bins=30,  color='black', alpha=0.5, position='identity')+
  labs(x='Shell_weight', title = 'Histograma de Shell_weight en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=250, by=20))+
  scale_x_continuous(breaks = seq(from=0, to=200, by=10))

ggplot(abalone, aes(y=Shell_weight, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Shell_weight', title = 'Diagrama de cajas de la variable Shell_weight por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))


#sex-Rings

ggplot(abalone, aes(x=Rings, fill=Sex)) + 
  geom_histogram(bins=length(unique(abalone$Rings)),  color='black', alpha=0.5, position='identity')+
  labs(x='Rings', title = 'Histograma de Rings en función del sexo')+
  scale_y_continuous(breaks = seq(from=0, to=300, by=20))+
  scale_x_continuous(breaks = seq(from=0, to=30, by=1))

ggplot(abalone, aes(y=Rings, x=Sex)) + 
  geom_boxplot(aes(fill=Sex),outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Rings', title = 'Diagrama de cajas de la variable Rings por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))


# Variables de tamaño

# Length-Diameter

ggplot(abalone[-abalone.outliers,],aes(x=Length, y=Diameter)) + geom_point(color='Blue')+  geom_smooth(method=lm, color='Red')+
labs(title = 'Diagrama de dispersión entre Length y Diameter')+
  scale_y_continuous(breaks = seq(from=0, to=150, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))
# Hay un outlier que podemos estudiar
# bastante homocedástico


# Por sexo se pueden distinguir, aunque este gráfico satura mucho, igual es interesante para otras variables
ggplot(abalone,aes(x=Length, y=Diameter, color=Sex)) + geom_point()+  geom_smooth(method=lm)+
labs(title = 'Diagrama de dispersión entre Length y Diameter')+
  scale_y_continuous(breaks = seq(from=0, to=150, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))

# Length-Height
ggplot(abalone,aes(x=Length, y=Height)) + geom_point(color='Blue')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Length y Height')+
  scale_y_continuous(breaks = seq(from=0, to=150, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))


ggplot(abalone[-abalone.outliers,],aes(x=Length, y=Height)) + geom_point(color='Blue')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Length y Height')+
  scale_y_continuous(breaks = seq(from=0, to=50, by=2))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))


# Diameter-Height
ggplot(abalone[-Height.outliers,],aes(x=Diameter, y=Height)) + geom_point(color='Blue')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Diamneter y Height', subtitle = 'Sin los outliers de Height')+
  scale_y_continuous(breaks = seq(from=0, to=150, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))

ggplot(abalone[-Height.outliers,],aes(x=Diameter, y=Height, color=Sex)) + geom_point()+  geom_smooth(method=lm)+
  labs(title = 'Diagrama de dispersión entre Length y Diameter por sexo')+
  scale_y_continuous(breaks = seq(from=0, to=150, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))+
  facet_wrap(~Sex)
  

# Variables de peso

# Whole_weight-Shucked_weight
ggplot(abalone[-abalone.outliers,],aes(x=Whole_weight, y=Shucked_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Whole_weight y Shucked_weight')+
  scale_y_continuous(breaks = seq(from=0, to=300, by=20))+
  scale_x_continuous(breaks = seq(from=0, to=600, by=50))
# Estudiar outliers multivariantes, ya que pueden afectar a la recta de regresión, especialmente a la hora de modelar
# e intentar predecir los valores más pequeños de ambas variables. Estos dos outliers aparecen en todas las gráficas
# en las que se use Whole_weight.

# Whole_weight-Viscera_weight
ggplot(abalone[-abalone.outliers,],aes(x=Whole_weight, y=Viscera_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Whole_weight y Viscera_weight')+
  scale_y_continuous(breaks = seq(from=0, to=150, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=600, by=50))

# Whole_weight-Shell_weight
ggplot(abalone,aes(x=Whole_weight, y=Shell_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Whole_weight y Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=600, by=50))

# Si estudiamos sin outliers univariados
ggplot(abalone[-c(Whole_weight.outliers,Shell_weight.outliers),],aes(x=Whole_weight, y=Shell_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Whole_weight y Shell_weight', subtitle = 'Sin outliers univariados')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=600, by=50))


# Shucked_weight-Viscera_Weight
ggplot(abalone[-abalone.outliers,],aes(x=Shucked_weight, y=Viscera_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Shucked_weight y Viscera_weight')+
  scale_y_continuous(breaks = seq(from=0, to=150, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=300, by=20))

# Shucked_weight-Shell_weight
ggplot(abalone,aes(x=Shucked_weight, y=Shell_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Shucked_weight y Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=300, by=20))

# sin outliers univariados
ggplot(abalone[-c(Shucked_weight.outliers,Shell_weight.outliers),],aes(x=Shucked_weight, y=Shell_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Shucked_weight y Shell_weight', subtitle = 'Sin outliers univariados')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=200, by=10))

# Viscera_weight-Shell_weight
ggplot(abalone,aes(x=Viscera_weight, y=Shell_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Viscera_weight y Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))

# Sin outliers  univariados
ggplot(abalone[-c(Viscera_weight.outliers,Shell_weight.outliers),],aes(x=Viscera_weight, y=Shell_weight)) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Viscera_weight y Shell_weight', subtitle = 'Sin outliers univariados')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=10))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))
# Bastante heterodasticidad
# Si sumamos las de peso:
ggplot(abalone [-abalone.outliers,],aes(x=Whole_weight, y=(Shell_weight+Viscera_weight+Shucked_weight))) + geom_point(color='Green')+  geom_smooth(method=lm, color='Red')+
labs(title = 'Diagrama de dispersión entre Whole_weight y Shell_weight')+
scale_y_continuous(breaks = seq(from=0, to=400, by=50))+
scale_x_continuous(breaks = seq(from=0, to=500, by=50))



# Analizar biplots entre variables de tamaño y de peso

ggplot(abalone [-abalone.outliers,],aes(x=Length, y=Whole_weight)) + geom_point(color='Cyan')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre Length y Whole_weight')+
  scale_y_continuous(breaks = seq(from=0, to=400, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))


ggplot(abalone [-abalone.outliers,],aes(x=Length*Diameter*Height, y=Whole_weight)) + 
  geom_point(color='Cyan')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre el producto de las variables de tamaño
       y Whole_weight')+
  scale_y_continuous(breaks = seq(from=0, to=400, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=800000, by=100000))


ggplot(abalone [-abalone.outliers,],aes(x=Length*Diameter*Height, y=Shucked_weight+Shell_weight+Viscera_weight)) + 
  geom_point(color='Cyan')+  geom_smooth(method=lm, color='Red')+
  labs(title = 'Diagrama de dispersión entre el producto de las variables de tamaño
       y Whole_weight')+
  scale_y_continuous(breaks = seq(from=0, to=400, by=50))+
  scale_x_continuous(breaks = seq(from=0, to=800000, by=100000))


# Resumen
#scatterplotMatrix(abalone[-Height.outliers,] %>% select(-Sex,-Rings))

#explicar un poco lo que vemos con rasgos generales

# Analizamos las variables cuantitativas con respecto a la variable de salida (Rings)

# Length-Rings
ggplot(abalone,aes(x=Length, y=log(Rings))) + geom_point(color='Red')+  geom_smooth(method=lm, color='Blue')+
  labs(title = 'Diagrama de dispersión entre Length y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))

# eliminando outliers univariados de Rings y length (no merece la pena, nos quedamos con el gráfico anterior)
ggplot(abalone[-c(Length.outliers, Rings.outliers),],aes(x=Length, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Blue')+
  labs(title = 'Diagrama de dispersión entre Length y Rings', subtitle = 'Sin outliers univariados')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))
# Bastante heterocedasticidad en ambos casos

ggplot(abalone,aes(x=Length, y=log(Rings))) + 
  geom_point(color='Red')+  geom_smooth(method=lm, color='Blue')+
  labs(title = 'Diagrama de dispersión entre Length y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))



# Diameter-Rings
ggplot(abalone,aes(x=Diameter, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Blue')+
  labs(title = 'Diagrama de dispersión entre Diameter y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))

# eliminando outliers univariados de Rings y Diameter (no merece la pena, nos quedamos con el gráfico anterior)
ggplot(abalone[-c(Diameter.outliers, Rings.outliers),],aes(x=Length, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Blue')+
  labs(title = 'Diagrama de dispersión entre Length y Rings', subtitle = 'Sin outliers univariados')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))
# Bastante heterocedasticidad en ambos casos con esta variable también


# Height-Rings
ggplot(abalone,aes(x=Height, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Blue')+
  labs(title = 'Diagrama de dispersión entre Height y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))

# En este caso si es necesario eliminar los outliers de Height
ggplot(abalone[-c(Height.outliers),],aes(x=Height, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Blue')+
  labs(title = 'Diagrama de dispersión entre Height y Rings', subtitle = 'Sin outliers de Height')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=50, by=2))

# Whole_weight-Rings
ggplot(abalone,aes(x=Whole_weight, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre Whole_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=500, by=50))
# Parece que hay una relación logarítmica o cuadrática inversa (incluir en apartado de Transformación de los datos)

############################
# Con log
ggplot(abalone,aes(x=log(Whole_weight), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre el log de Whole_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=500, by=50))
# Relación cuadrática inversa
ggplot(abalone,aes(x=sqrt(Whole_weight), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre la raíz de Whole_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=500, by=50))
##########################


# Shucked_weight-Rings
ggplot(abalone,aes(x=Shucked_weight, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre Shucked_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=300, by=20))

# Lo mismo
############################
# Con log
ggplot(abalone,aes(x=log(Shucked_weight), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre el log de Shucked_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=500, by=50))
# Relación cuadrática inversa
ggplot(abalone[-Shucked_weight.outliers,],aes(x=(Shucked_weight^2), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre la raiz de Shucked_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=500, by=50))
##########################


# Viscera_weight-Rings
ggplot(abalone,aes(x=Viscera_weight, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre Viscera_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))

# Lo mismo
############################
# Con log
ggplot(abalone,aes(x=log(Viscera_weight), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre el log de Viscera_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))
# Relación cuadrática inversa
ggplot(abalone,aes(x=sqrt(Viscera_weight), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre la raiz de Viscera_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))
##########################

# Shell_weight-Rings
ggplot(abalone,aes(x=Shell_weight, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre Shell_weight y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=200, by=10))

# Sin los outliers de Shell_weight se ve mejor
ggplot(abalone[-Shell_weight.outliers,],aes(x=Shell_weight, y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre Shell_weight y Rings', subtitle = 'Sin outliers de Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=130, by=10))

# Lo mismo
############################
# Con log
ggplot(abalone[-Shell_weight.outliers,],aes(x=log(Shell_weight), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre el log de Shell_weight y Rings', subtitle = 'Sin outliers de Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))
# Relación cuadrática inversa
ggplot(abalone,aes(x=(Shell_weight)^(1/3), y=Rings)) + geom_point(color='Red')+  geom_smooth(method=lm, color='Green')+
  labs(title = 'Diagrama de dispersión entre la raiz de Shell_weight y Rings', subtitle = 'Sin outliers de Shell_weight')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=150, by=10))
##########################

ggplot(abalone,aes(x=Length, y=Rings,color=Sex)) + geom_point()+  geom_smooth(method=lm)+
  labs(title = 'Diagrama de dispersión entre Length y Rings')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))+
  scale_x_continuous(breaks = seq(from=0, to=160, by=10))+
  facet_wrap(~Sex)




# Descomposición de datos en sus partes

# Pasamos la variable categórica a valores binarios
abalone.dummy <- dummy_cols(abalone,remove_selected_columns = TRUE)
# Explicar que si se utilizan los valores de sex tal y como vienen, implica meter una noción de orden


# Búsqueda de datos redundantes

# Estudiamos la correlación entre las variables mediante su matriz de correlacción
# Con outliers
cor(abalone[,-1])
# Sin outliers
cor(abalone[-c(abalone.outliers),-1])
cov(abalone[-c(abalone.outliers),-1])
# Sin outliers
abalone.CorMatrix <- cor(abalone[-c(abalone.outliers),-1])
corrplot(abalone.CorMatrix, method = "number")

# PCA

abalone.pca <- PCA(abalone[-abalone.outliers,-c(1,9)], scale.unit = TRUE, ncp = 3, graph = FALSE)
get_eigenvalue(abalone.pca)
abalone.pca$var$cos2
fviz_pca_var(abalone.pca, 
             axes = c(1, 2),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", 
                               "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_contrib(abalone.pca, 
             choice = "var", 
             axes = 1:2,
             xtickslab.rt = 90,
             top=10)
# Comentar

# Las variables de peso y medidas están muy correladas, Lenght y Diameter están muy correladas por lo que podemos quitar una de las dos
# shucked_weight igual con las variables de peso, y menos correlada con la salida Rings, lo mismo que Viscera_weight
abalone.CorMatrix2 <- cor(abalone[-c(abalone.outliers),-c(1,2,6,7)])
corrplot(abalone.CorMatrix2, method = "number")

# Comentar que los atributos correlados interfieren en el modelo de regresión lineal

## Transformación de los datos

# Normalidad
# explicar lo de la transformación con la raiz cuadrada de las variables de peso, y para knn la normalización de las variables


chisq.test(abalone$Whole_weight,abalone$Rings)
