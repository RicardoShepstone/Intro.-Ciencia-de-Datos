# EDA Clasificación: Vehicle
# autor: Ricardo Ignacio Shepstone Aramburu 77553533V

require(tidyverse)
require(readr)
require(moments)
require(car)
require(corrplot)
require(fastDummies)
require(stringr)

# Cargamos dataset

vehicle.raw <- read.csv("Input/vehicle/vehicle.dat", comment.char="@", header=FALSE)
vehicle <- vehicle.raw

# renombramos las variables para trabajar con ellas
colnames(vehicle) <- c('Compactness','Circularity','Distance_circularity','Radius_ratio',
                       'Praxis_aspect_ratio', 'Max_length_aspect_ratio', 'Scatter_ratio', 'Elongatedness',
                       'Praxis_rectangular', 'Length_rectangular', 'Major_variance', 'Minor_variance', 'Gyration_radius',
                       'Major_skewness', 'Minor_skewness', 'Minor_kurtosis', 'Major_kurtosis', 'Hollows_ratio', 'Class')



#comprobamos número de columnas, filas y si se han cargado bien los datos, la estructura de estos
nrow(vehicle)
ncol(vehicle)
str(vehicle)
summary(vehicle)

# eliminamos espacios al principio y final de cada string de class
vehicle[,'Class'] <- str_remove_all(vehicle$Class," ") 

# Comprobación de missing values
sum(is.na(vehicle))
sum(is.null(vehicle))

# datos duplicados

sum(duplicated(vehicle))


# Outliers en una dimensión
# Calculamos rango intercuartílico de todas las variables
vehicle.IQR <- vehicle %>% select(-Class) %>% apply(2, IQR)
vehicle.Quartiles <- vehicle %>% select(-Class) %>% apply(2, quantile,c(0.25,0.75))
Upper.limit <- vehicle.Quartiles[2,]+1.5*vehicle.IQR
Upper.limit
Lower.limit <- vehicle.Quartiles[1,]-1.5*vehicle.IQR
Lower.limit

# Obtenemos los outliers para cada variable y calculamos sus posiciones 
# Compactness
Compactness.outliers <- which(vehicle$Compactness>Upper.limit['Compactness'] | vehicle$Compactness<Lower.limit['Compactness'])
# Circularity
Circularity.outliers <- which(vehicle$Circularity>Upper.limit['Circularity'] | vehicle$Circularity<Lower.limit['Circularity'])
# Distance_circularity
Distance_circularity.outliers <- which(vehicle$Distance_circularity>Upper.limit['Distance_circularity'] | vehicle$Distance_circularity<Lower.limit['Distance_circularity'])
# Radius_ratio
Radius_ratio.outliers <- which(vehicle$Radius_ratio>Upper.limit['Radius_ratio'] | vehicle$Radius_ratio<Lower.limit['Radius_ratio'])
# Praxis_aspect_ratio
Praxis_aspect_ratio.outliers <- which(vehicle$Praxis_aspect_ratio>Upper.limit['Praxis_aspect_ratio'] | vehicle$Praxis_aspect_ratio<Lower.limit['Praxis_aspect_ratio'])
# Max_length_aspect_ratio
Max_length_aspect_ratio.outliers <- which(vehicle$Max_length_aspect_ratio>Upper.limit['Max_length_aspect_ratio'] | vehicle$Max_length_aspect_ratio<Lower.limit['Max_length_aspect_ratio'])
# Scatter_ratio
Scatter_ratio.outliers <- which(vehicle$Scatter_ratio>Upper.limit['Scatter_ratio'] | vehicle$Scatter_ratio<Lower.limit['Scatter_ratio'])
# Elongatedness
Elongatedness.outliers <- which(vehicle$Elongatedness>Upper.limit['Elongatedness'] | vehicle$Elongatedness<Lower.limit['Elongatedness'])
# Praxis_rectangular
Praxis_rectangular.outliers <- which(vehicle$Praxis_rectangular>Upper.limit['Praxis_rectangular'] | vehicle$Praxis_rectangular<Lower.limit['Praxis_rectangular'])
# Length_rectangular
Length_rectangular.outliers <- which(vehicle$Length_rectangular>Upper.limit['Length_rectangular'] | vehicle$Length_rectangular<Lower.limit['Length_rectangular'])
# Major_variance
Major_variance.outliers <- which(vehicle$Major_variance>Upper.limit['Major_variance'] | vehicle$Major_variance<Lower.limit['Major_variance'])
# Minor_variance
Minor_variance.outliers <- which(vehicle$Minor_variance>Upper.limit['Minor_variance'] | vehicle$Minor_variance<Lower.limit['Minor_variance'])
# Gyration_radius 
Gyration_radius.outliers <- which(vehicle$Gyration_radius>Upper.limit['Gyration_radius '] | vehicle$Gyration_radius<Lower.limit['Gyration_radius'])
# Major_skewness
Major_skewness.outliers <- which(vehicle$Major_skewness>Upper.limit['Major_skewness'] | vehicle$Major_skewness<Lower.limit['Major_skewness'])
# Minor_skewness
Minor_skewness.outliers <- which(vehicle$Minor_skewness>Upper.limit['Minor_skewness'] | vehicle$Minor_skewness<Lower.limit['Minor_skewness'])
# Minor_kurtosis
Minor_kurtosis.outliers <- which(vehicle$Minor_kurtosis>Upper.limit['Minor_kurtosis'] | vehicle$Minor_kurtosis<Lower.limit['Minor_kurtosis'])
# Major_kurtosis
Major_kurtosis.outliers <- which(vehicle$Major_kurtosis>Upper.limit['Major_kurtosis'] | vehicle$Major_kurtosis<Lower.limit['Major_kurtosis'])
# Hollows_ratio
Hollows_ratio.outliers <- which(vehicle$Hollows_ratio>Upper.limit['Hollows_ratio'] | vehicle$Hollows_ratio<Lower.limit['Hollows_ratio'])


# Comprobamos cantidad de outliers 

vehicle.outliers <- unique(c(Compactness.outliers, Circularity.outliers, 
                             Distance_circularity.outliers, Radius_ratio.outliers, 
                             Praxis_aspect_ratio.outliers, Max_length_aspect_ratio.outliers,
                             Scatter_ratio.outliers, Elongatedness.outliers,
                             Praxis_rectangular.outliers, Length_rectangular.outliers,
                             Major_variance.outliers, Minor_variance.outliers,
                             Gyration_radius.outliers, Major_skewness.outliers,
                             Minor_skewness.outliers, Minor_kurtosis.outliers,
                             Major_kurtosis.outliers, Hollows_ratio.outliers))

length(vehicle.outliers)

table(vehicle[vehicle.outliers,"Class"])

# calculamos número de elementos por clase
counts.Class <- table(vehicle$Class)
counts.Class

## Resumen de los datos
# Medidas de tendencia central

# Con summary() podemos obtener la media y cuartiles
summary(vehicle)

# Para obtener estos valores:
vehicle.mean <- vehicle %>% select(-Class) %>% map_dbl(mean)
vehicle.median <- vehicle %>% select(-Class) %>% map_dbl(median)

vehicle.mean
vehicle.median

# Medidas de dispersión
# Para el Rango, máximo y mínimo:
vehicle.min.max <- vehicle %>% select(-Class) %>% apply(2,range)
vehicle.range <- vehicle.min.max[2,]-vehicle.min.max[1,]
vehicle.min.max.range <- rbind(vehicle.min.max, vehicle.range)
rownames(vehicle.min.max.range) <- c('min', 'max', 'range')
vehicle.min.max.range             

# primer cuartil, tercer cuartil y rango intercuartílico.
vehicle.Quartiles
vehicle.IQR

# varianza, desviación típica y desviación absoluta de la mediana
vehicle.var <- vehicle %>% select(-Class) %>% map_dbl(var)
vehicle.sd <- vehicle %>% select(-Class) %>% map_dbl(sd)
vehicle.mad <- vehicle %>% select(-Class) %>% map_dbl(mad)

vehicle.var
vehicle.sd 
vehicle.mad

# Medidas de forma
vehicle.skewness <- vehicle %>% select(-Class) %>% map_dbl(skewness)
vehicle.kurtosis <- vehicle %>% select(-Class) %>% map_dbl(kurtosis)

vehicle.skewness
vehicle.kurtosis

# Comprobamos normalidad con shapiro

vehicle.shapiro <- apply(vehicle[,-19],2,shapiro.test)
vehicle.shapiro

# visualización de los datos

# Para la variable de salida Class
barplot(counts.Class, main='Distribución por clase', xlab='clase',ylim = c(0,250))


# Para Compactness
ggplot(vehicle, aes(x=Compactness)) + 
  geom_histogram(aes(y=stat(density)),bins=24, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Compactness Density', title = 'Histograma y densidad de la variable Compactness')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=120, by=5))


ggplot(vehicle, aes(y=Compactness)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Compactness', title = 'Diagrama de cajas de la variable Compactness')+
  scale_y_continuous(breaks = seq(from=0, to=120, by=5))

# Para Circularity
ggplot(vehicle, aes(x=Circularity)) + 
  geom_histogram(aes(y=stat(density)),bins=27, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Circularity Density', title = 'Histograma y densidad de la variable Circularity')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=60, by=2))


ggplot(vehicle, aes(y=Circularity)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Circularity', title = 'Diagrama de cajas de la variable Circularity')+
  scale_y_continuous(breaks = seq(from=0, to=60, by=2))


# Para Distance_circularity
ggplot(vehicle, aes(x=Distance_circularity)) + 
  geom_histogram(aes(y=stat(density)),bins=27, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Distance_circularity Density', title = 'Histograma y densidad de la variable Distance_circularity')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=110, by=5))


ggplot(vehicle, aes(y=Distance_circularity)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Distance_circularity', title = 'Diagrama de cajas de la variable Distance_circularity')+
  scale_y_continuous(breaks = seq(from=0, to=110, by=5))



# Para Radius_ratio
ggplot(vehicle, aes(x=Radius_ratio)) + 
  geom_histogram(aes(y=stat(density)),bins=24, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Radius_ratio Density', title = 'Histograma y densidad de la variable Radius_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=400, by=10))


ggplot(vehicle, aes(y=Radius_ratio)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Radius_ratio', title = 'Diagrama de cajas de la variable Radius_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=400, by=10))



# Para Praxis_aspect_ratio
ggplot(vehicle, aes(x=Praxis_aspect_ratio)) + 
  geom_histogram(aes(y=stat(density)),bins=24, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Praxis_aspect_ratio Density', title = 'Histograma y densidad de la variable Praxis_aspect_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=140, by=5))


ggplot(vehicle, aes(y=Praxis_aspect_ratio)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Praxis_aspect_ratio', title = 'Diagrama de cajas de la variable Praxis_aspect_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=140, by=5))




# Para Max_length_aspect_ratio
ggplot(vehicle, aes(x=Max_length_aspect_ratio)) + 
  geom_histogram(aes(y=stat(density)),bins=24, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Max_length_aspect_ratio', title = 'Histograma y densidad de la variable Max_length_aspect_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=60, by=2))


ggplot(vehicle, aes(y=Max_length_aspect_ratio)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Max_length_aspect_ratio', title = 'Diagrama de cajas de la variable Max_length_aspect_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=60, by=2))



# Para Scatter_ratio
ggplot(vehicle, aes(x=Scatter_ratio)) + 
  geom_histogram(aes(y=stat(density)),bins=27, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Scatter_ratio Density', title = 'Histograma y densidad de la variable Scatter_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=300, by=10))


ggplot(vehicle, aes(y=Scatter_ratio)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Scatter_ratio', title = 'Diagrama de cajas de la variable Scatter_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=300, by=10))



# Para Elongatedness
ggplot(vehicle, aes(x=Elongatedness)) + 
  geom_histogram(aes(y=stat(density)),bins=35, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Elongatedness Density', title = 'Histograma y densidad de la variable Elongatedness')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=60, by=2))


ggplot(vehicle, aes(y=Elongatedness)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Elongatedness', title = 'Diagrama de cajas de la variable Elongatedness')+
  scale_y_continuous(breaks = seq(from=0, to=60, by=2))




# Para Praxis_rectangular
ggplot(vehicle, aes(x=Praxis_rectangular)) + 
  geom_histogram(aes(y=stat(density)),bins=12, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Praxis_rectangular Density', title = 'Histograma y densidad de la variable Praxis_rectangular')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=30, by=1))


ggplot(vehicle, aes(y=Praxis_rectangular)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Praxis_rectangular', title = 'Diagrama de cajas de la variable Praxis_rectangular')+
  scale_y_continuous(breaks = seq(from=0, to=30, by=1))




# Para Length_rectangular
ggplot(vehicle, aes(x=Length_rectangular)) + 
  geom_histogram(aes(y=stat(density)),bins=26, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Length_rectangular Density', title = 'Histograma y densidad de la variable Length_rectangular')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=200, by=5))


ggplot(vehicle, aes(y=Length_rectangular)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Length_rectangular', title = 'Diagrama de cajas de la variable Length_rectangular')+
  scale_y_continuous(breaks = seq(from=0, to=200, by=5))




# Para Major_variance
ggplot(vehicle, aes(x=Major_variance)) + 
  geom_histogram(aes(y=stat(density)),bins=24, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Major_variance Density', title = 'Histograma y densidad de la variable Major_variance')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=350, by=10))


ggplot(vehicle, aes(y=Major_variance)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Major_variance', title = 'Diagrama de cajas de la variable Major_variance')+
  scale_y_continuous(breaks = seq(from=0, to=350, by=10))



# Para Minor_variance
ggplot(vehicle, aes(x=Minor_variance)) + 
  geom_histogram(aes(y=stat(density)),bins=24, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Minor_variance Density', title = 'Histograma y densidad de la variable Minor_variance')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=1000, by=50))


ggplot(vehicle, aes(y=Minor_variance)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Minor_variance', title = 'Diagrama de cajas de la variable Minor_variance')+
  scale_y_continuous(breaks = seq(from=0, to=1000, by=50))


# Para Gyration_radius
ggplot(vehicle, aes(x=Gyration_radius)) + 
  geom_histogram(aes(y=stat(density)),bins=28, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Gyration_radius Density', title = 'Histograma y densidad de la variable Gyration_radius')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=300, by=10))


ggplot(vehicle, aes(y=Gyration_radius)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Gyration_radius', title = 'Diagrama de cajas de la variable Gyration_radius')+
  scale_y_continuous(breaks = seq(from=0, to=300, by=10))



# Para Major_skewness
ggplot(vehicle, aes(x=Major_skewness)) + 
  geom_histogram(aes(y=stat(density)),bins=24, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Major_skewness Density', title = 'Histograma y densidad de la variable Major_skewness')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=140, by=5))


ggplot(vehicle, aes(y=Major_skewness)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Major_skewness', title = 'Diagrama de cajas de la variable Major_skewness')+
  scale_y_continuous(breaks = seq(from=0, to=140, by=5))



# Para Minor_skewness
ggplot(vehicle, aes(x=Minor_skewness)) + 
  geom_histogram(aes(y=stat(density)),bins=23, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Minor_skewness Density', title = 'Histograma y densidad de la variable Minor_skewness')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=25, by=1))


ggplot(vehicle, aes(y=Minor_skewness)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Minor_skewness', title = 'Diagrama de cajas de la variable Minor_skewness')+
  scale_y_continuous(breaks = seq(from=0, to=25, by=1))



# Para Minor_kurtosis
ggplot(vehicle, aes(x=Minor_kurtosis)) + 
  geom_histogram(aes(y=stat(density)),bins=21, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Minor_kurtosis Density', title = 'Histograma y densidad de la variable Minor_kurtosis')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=40, by=2))


ggplot(vehicle, aes(y=Minor_kurtosis)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Minor_kurtosis', title = 'Diagrama de cajas de la variable Minor_kurtosis')+
  scale_y_continuous(breaks = seq(from=0, to=48, by=2))




# Para Major_kurtosis
ggplot(vehicle, aes(x=Major_kurtosis)) + 
  geom_histogram(aes(y=stat(density)),bins=30, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Major_kurtosis Density', title = 'Histograma y densidad de la variable Major_kurtosis')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=175, to=210, by=2))


ggplot(vehicle, aes(y=Major_kurtosis)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Major_kurtosis', title = 'Diagrama de cajas de la variable Major_kurtosis')+
  scale_y_continuous(breaks = seq(from=175, to=210, by=2))



# Para Hollows_ratio
ggplot(vehicle, aes(x=Hollows_ratio)) + 
  geom_histogram(aes(y=stat(density)),bins=30, color='Blue',fill='Cyan')+
  geom_density(lwd = 1.2,linetype = 1,colour = 6)+
  labs(y='Hollows_ratio Density', title = 'Histograma y densidad de la variable Hollows_ratio')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=180, to=210, by=2))


ggplot(vehicle, aes(y=Hollows_ratio)) + 
  geom_boxplot(fill='Cyan',outlier.colour = 'red', outlier.shape = 3)+
  labs(y='Hollows_ratio', title = 'Diagrama de cajas de la variable Hollows_ratio')+
  scale_y_continuous(breaks = seq(from=180, to=210, by=2))

library(GGally)

ggpairs(data=vehicle, columns=1:18, title="vehicle data")

ggpairs(data=vehicle[-vehicle.outliers,], columns=1:18, title="vehicle data")





# Plots bivariables con la salida
# Compactness-Class

vehicle %>%
ggplot(aes(x=Compactness, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Compactness,
                          mean = tapply(Compactness, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Compactness, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Compactness',y='Density', 
       title = 'Distribuciones por clase de la variable Compactness', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=120, by=5))


# Circularity-Class

vehicle %>%
  ggplot(aes(x=Circularity, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Circularity,
                          mean = tapply(Circularity, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Circularity, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Circularity',y='Density', 
       title = 'Distribuciones por clase de la variable Circularity', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=60, by=2))



# Distance_circularity-Class

vehicle %>%
  ggplot(aes(x=Distance_circularity, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Distance_circularity,
                          mean = tapply(Distance_circularity, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Distance_circularity, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Distance_circularity',y='Density', 
       title = 'Distribuciones por clase de la variable Distance_circularity', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=120, by=5))




# Radius_ratio-Class

vehicle %>%
  ggplot(aes(x=Radius_ratio, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Radius_ratio,
                          mean = tapply(Radius_ratio, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Radius_ratio, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Radius_ratio',y='Density', 
       title = 'Distribuciones por clase de la variable Radius_ratio', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=350, by=20))




# Praxis_aspect_ratio-Class

vehicle[-Praxis_aspect_ratio.outliers,] %>%
  ggplot(aes(x=Praxis_aspect_ratio, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Praxis_aspect_ratio,
                          mean = tapply(Praxis_aspect_ratio, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Praxis_aspect_ratio, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Praxis_aspect_ratio',y='Density', 
       title = 'Distribuciones por clase de la variable Praxis_aspect_ratio', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=75, by=2))




# Max_length_aspect_ratio-Class

vehicle[-Max_length_aspect_ratio.outliers,] %>%
  ggplot(aes(x=Max_length_aspect_ratio, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Max_length_aspect_ratio,
                          mean = tapply(Max_length_aspect_ratio, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Max_length_aspect_ratio, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Max_length_aspect_ratio',y='Density', 
       title = 'Distribuciones por clase de la variable Max_length_aspect_ratio', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.05))+
  scale_x_continuous(breaks = seq(from=0, to=15, by=1))




# Scatter_ratio-Class

vehicle %>%
  ggplot(aes(x=Scatter_ratio, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Scatter_ratio,
                          mean = tapply(Scatter_ratio, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Scatter_ratio, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Scatter_ratio',y='Density', 
       title = 'Distribuciones por clase de la variable Scatter_ratio', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=120, to=250, by=20))




# Elongatedness-Class

vehicle %>%
  ggplot(aes(x=Elongatedness, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Elongatedness,
                          mean = tapply(Elongatedness, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Elongatedness, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Elongatedness',y='Density', 
       title = 'Distribuciones por clase de la variable Elongatedness', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=25, to=60, by=2))




# Praxis_rectangular-Class

vehicle %>%
  ggplot(aes(x=Praxis_rectangular, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Praxis_rectangular,
                          mean = tapply(Praxis_rectangular, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Praxis_rectangular, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Praxis_rectangular',y='Density', 
       title = 'Distribuciones por clase de la variable Praxis_rectangular', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.05))+
  scale_x_continuous(breaks = seq(from=0, to=30, by=1))




# Length_rectangular-Class

vehicle %>%
  ggplot(aes(x=Length_rectangular, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Length_rectangular,
                          mean = tapply(Length_rectangular, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Length_rectangular, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Length_rectangular',y='Density', 
       title = 'Distribuciones por clase de la variable Length_rectangular', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=115, to=200, by=10))




# Major_variance-Class

vehicle[-Major_variance.outliers,] %>%
  ggplot(aes(x=Major_variance, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Major_variance,
                          mean = tapply(Major_variance, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Major_variance, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Major_variance',y='Density', 
       title = 'Distribuciones por clase de la variable Major_variance', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=350, by=20))




# Minor_variance-Class

vehicle %>%
  ggplot(aes(x=Minor_variance, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Minor_variance,
                          mean = tapply(Minor_variance, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Minor_variance, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Minor_variance',y='Density', 
       title = 'Distribuciones por clase de la variable Minor_variance', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=200, to=1000, by=100))




# Gyration_radius-Class

vehicle %>%
  ggplot(aes(x=Gyration_radius, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Gyration_radius,
                          mean = tapply(Gyration_radius, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Gyration_radius, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Gyration_radius',y='Density', 
       title = 'Distribuciones por clase de la variable Gyration_radius', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=120, to=260, by=20))




# Major_skewness-Class

vehicle[-Major_skewness.outliers,] %>%
  ggplot(aes(x=Major_skewness, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Major_skewness,
                          mean = tapply(Major_skewness, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Major_skewness, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Major_skewness',y='Density', 
       title = 'Distribuciones por clase de la variable Major_skewness', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=120, by=5))




# Minor_skewness-Class

vehicle %>%
  ggplot(aes(x=Minor_skewness, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Minor_skewness,
                          mean = tapply(Minor_skewness, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Minor_skewness, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Minor_skewness',y='Density', 
       title = 'Distribuciones por clase de la variable Minor_skewness', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=25, by=2))




# Minor_kurtosis-Class

vehicle %>%
  ggplot(aes(x=Minor_kurtosis, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Minor_kurtosis,
                          mean = tapply(Minor_kurtosis, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Minor_kurtosis, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Minor_kurtosis',y='Density', 
       title = 'Distribuciones por clase de la variable Minor_kurtosis', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=0, to=40, by=5))




# Major_kurtosis-Class

vehicle %>%
  ggplot(aes(x=Major_kurtosis, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Major_kurtosis,
                          mean = tapply(Major_kurtosis, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Major_kurtosis, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Major_kurtosis',y='Density', 
       title = 'Distribuciones por clase de la variable Major_kurtosis', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=175, to=210, by=5))





# Hollows_ratio-Class

vehicle %>%
  ggplot(aes(x=Hollows_ratio, fill=Class)) + 
  geom_density(linetype = 1)+
  facet_wrap(~Class)+
  geom_line(aes(y = dnorm(Hollows_ratio,
                          mean = tapply(Hollows_ratio, Class, mean, na.rm = TRUE)[PANEL],
                          sd = tapply(Hollows_ratio, Class, sd, na.rm = TRUE)[PANEL])),
            color =1, lwd=1.1,linetype = 3)+
  labs(x='Hollows_ratio',y='Density', 
       title = 'Distribuciones por clase de la variable Hollows_ratio', 
       subtitle ='Y distribuciones normales ideales por clase')+
  scale_y_continuous(breaks = seq(from=0, to=1, by=0.01))+
  scale_x_continuous(breaks = seq(from=180, to=210, by=5))


## Descomposición de atributos complicados


# Pasamos la variable categórica a valores binarios
vehicle.dummy <- dummy_cols(vehicle,remove_selected_columns = TRUE)
head(vehicle.dummy)

# Matriz correlación

# Estudiamos la correlación entre las variables mediante su matriz de correlacción
vehicle.CorMatrix <- cor(vehicle[,-19])
corrplot(vehicle.CorMatrix,method = "ellipse", type = "lower")

# Escalado 
vehicle.scaled <- vehicle.dummy %>% mutate_if(is.numeric, scale, center = TRUE, scale = TRUE)
head(vehicle.scaled)
