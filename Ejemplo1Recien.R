# profile -> install packages

#RecienNacidos3<- read.csv("Practica R/RecienNacidos.csv")

#RecienNacidos2<- read.csv("Practica R/RecienNacidos.csv", ",",head=TRUE)    # comas o punto y coma?


library(readr)
RecienNacidos <- read_csv("Practica R/RecienNacidos.csv")
View(RecienNacidos)

attach(RecienNacidos)

names(RecienNacidos)

mean(RecienNacidos$PESO_BEBE)
mean(PESO_BEBE)

#-------Ignorando valores nulos-------

mean(PESO_BEBE,na.rm = TRUE)
mean(RecienNacidos$PESO_BEBE,na.rm=TRUE)

#-------Veamos casos completos------
RecienN = RecienNacidos[ complete.cases(RecienNacidos), ]

attach(RecienN)  #cambiemos predeterminado a nuevo dataset

mean(PESO_BEBE,na.rm = TRUE)    # No es necesario ya al haber casos completos
mean(PESO_BEBE)

hist(PESO_BEBE)
median(PESO_BEBE)

boxplot(PESO_BEBE)
boxplot(PESO_BEBE ~ GENERO)  # varia el peso segun genero del bebe?

plot(EDAD_MADRE~ PESO_BEBE)

#------Analisis edad de madre-----

mean(EDAD_MADRE)
median((EDAD_MADRE))

hist(EDAD_MADRE)
plot(RecienN$EDAD_MADRE)
boxplot(EDAD_MADRE)

# Existen indicios de diferencia significativa debido a  estado civil?

boxplot(PESO_BEBE ~ CASADA, main = "Baby weight by civil status (Married) ",
        xlab = "Married mother",
        ylab = "Weight (pounds)",
        col="blue"
)

# --------- Aumenta significativamente de peso en la mujer?------

#variable PESO_GANADO  -> diferencia de peso
DIFERENCIA = PESO_DESPUES_EMB - PESO_ANTES_EMB
mean(DIFERENCIA)

boxplot(DIFERENCIA)


shapiro.test(DIFERENCIA)  # se rechaza hipotesis nula?

# nuestra muestra es suficientemente grande
t.test(DIFERENCIA, mu=0)    ## prueba de hipotesis,  es significativo el cambio de peso?
wilcox.test(DIFERENCIA)   #sin asumir normalidad o 

# ------ otros analisis -------

mean(SEMANA_GESTACION)
hist(SEMANA_GESTACION)
boxplot(SEMANA_GESTACION)

boxplot(PESO_BEBE ~ EDUC_MADRE)
boxplot(PESO_BEBE ~ REGION_RES_MADRE)
