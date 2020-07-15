##  -------     EJEMPLO 1 ---------


#    Tabla de contingencia
#                 Divorciado NoDivorciado
#Fumadores            73           12
#No fumadores         43           39


# Ho: No existe asociacion estadistica entre las variables categoricas
# H1: Existe una asociacion significativa  ( Dependencia)

Divorciado<-c(73,43)


NoDivorciado<-c(12,39)


cuadro<-cbind(Divorciado,NoDivorciado)  # funciona tambien data.frame(  )


rownames(cuadro)<-c("Fumadores","No fumadores")

prop.table(cuadro)

barplot( prop.table(cuadro), main = "Comparison",
         legend = c("Fumadores", "No Fumadores"),
         xlab = " Estado Civil", beside = TRUE)


chisq = chisq.test(cuadro)  # 2x2 tables use continuity correction

chisq

#Expected counts
chisq$expected

# Most contributing cells ?

round( chisq$residuals, 3)

### ---------- Ejemplo 2 ---------

Ho: House tasks are equally distributed
H1: They aren't'

filepath= "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"

TareasEnCasa<- read.delim(filepath, row.names = 1)

head(TareasEnCasa)

# Example Expected:

sum(TareasEnCasa$Wife)  # 156+ 124+..+ 0 

sum(TareasEnCasa[1,])   # first row,  156+ 14+ .. .+ 4 

sum(TareasEnCasa)    # sum of all rows

# Expected  Wife*Laundry  60.55

600*176 / 1744 = ??

chisq = chisq.test(TareasEnCasa)  # 4 col * 13 rows

chisq

# df
chisq$parameter  #  (nrow -1 )*(ncol -1)

#Expected counts
chisq$expected


## Most contributing cells or correlation
round( chisq$residuals, 3)


### --------------- Ejemplo 3: Bondad de ajuste -----------

Ho:  Equally distribution of persons per zodiac signs
H1:  Not equally distributed

# 265 persons

# 12 zodiac sign

## Observed:


 ## Aries, Taurus, Gemini, ..., Capricorn, Aquarius, Pisces
observed<- c(29, 24, 22, 19,  21, 18, 19, 20, 23, 18, 20, 23)


#Expected: equally distribution of persons per sign?

prop <- 256 /12

p1<- prop/ 256      # for each zodiac sign must be persons...

12 - 1    ##  degree of freedom

p <- rep( p1, 12 )  # vector of probabilities

chiq<- chisq.test(observed, p = p )

chiq

## expected values 

chiq$expected
