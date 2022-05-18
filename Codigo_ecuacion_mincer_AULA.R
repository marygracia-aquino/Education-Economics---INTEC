################################################################################
'
Proyecto: Economia de la Educacion - INTEC
Que hace: Estima ecuacion mincer
Autora: Marygracia Aquino
Ultima actualizacion: 12 mayo 2022
'
################################################################################

#Importar y limpiar la base de datos

##establece carpeta de trabajo
## cambiar \ a / para computadoras windows
setwd(dir = "C:/Users/maryg/OneDrive/3_Jobs/8_intec/s2-c4/Ecuacion_mincer")

## importar base de datos
library(readxl)
bd1 <- read_excel("ENCFT_20203_Mincer.xlsx")

## visualizar las primeras variables y filas
head(bd1)

## estadistica descriptiva
summary(bd1)

##tabla de frecuencia para una variable
table(bd1$GRUPO_EMPLEO)/nrow(bd1)

################################################################################

#Crear variables

##ingreso

### crear variable de ingreso laboral
names(bd1) #enlista variables de una base de datos
bd1$ingreso <- rowSums(bd1[,48:51], na.rm = T)

###histograma con valor absoluto
options(scipen=999)
jpeg(filename = "histogram_ingreso_absoluto.jpeg")
hist(bd1$ingreso, main = "", ylab = "Frecuencia", xlab = "Ingreso mensual",
     col = "firebrick")
dev.off()

### eliminar personas que ganan cero

### crear logaritmo natural de ingreso
bd1$ln_ingreso <- log(bd1$ingreso)
bd1$ln_ingreso[bd1$ingreso<1] <- NA

### histogram del ln ingreso
jpeg(filename = "histogram_ingreso_ln.jpeg")
hist(bd1$ln_ingreso, main = "", ylab = "Frecuencia", xlab = "Ln(ingreso mensual)",
     col = "firebrick")
dev.off()

## anos educativos

### tabla de frecuencia del nivel educativo maximo alcazando y ultimo ano
### alcanzado en ese nivel educativo
table(bd1$NIVEL_ULTIMO_ANO_APROBADO,bd1$ULTIMO_ANO_APROBADO)

### creacion de variable ano educativos

#### computando obervacion vacia hay para cada variable (TRUE)
table(is.na(bd1$NIVEL_ULTIMO_ANO_APROBADO))
table(is.na(bd1$ULTIMO_ANO_APROBADO))

#### sacar todos los individuos que no tenga informacion de educacion
library(dplyr)
bd2 = filter(bd1,!is.na(NIVEL_ULTIMO_ANO_APROBADO))

table(is.na(bd2$ULTIMO_ANO_APROBADO)) #verificar que ULTIMO_ANO_APROBADO no haya vacios

#### crear anos educativos
bd2$educ <- NA
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==1] <- 0 #preescolar
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==10] <- 0 #quisqueya aprende contigo
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==9] <- 0 #ninguno
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==2] <- bd2[bd2$NIVEL_ULTIMO_ANO_APROBADO==2,]$ULTIMO_ANO_APROBADO #primaria
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==3] <- bd2[bd2$NIVEL_ULTIMO_ANO_APROBADO==3,]$ULTIMO_ANO_APROBADO + 8 #secundaria
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==4] <- bd2[bd2$NIVEL_ULTIMO_ANO_APROBADO==4,]$ULTIMO_ANO_APROBADO + 8 #secundaria tecnica
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==5] <- bd2[bd2$NIVEL_ULTIMO_ANO_APROBADO==5,]$ULTIMO_ANO_APROBADO + 12 #unversitario
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==6] <- bd2[bd2$NIVEL_ULTIMO_ANO_APROBADO==6,]$ULTIMO_ANO_APROBADO + 16 #maestria
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==7] <- bd2[bd2$NIVEL_ULTIMO_ANO_APROBADO==7,]$ULTIMO_ANO_APROBADO + 16 #postgrado
bd2$educ[bd2$NIVEL_ULTIMO_ANO_APROBADO==8] <- bd2[bd2$NIVEL_ULTIMO_ANO_APROBADO==8,]$ULTIMO_ANO_APROBADO + 18 #doctorado

bd2$educ_2 <- bd2$educ**2

table(bd2$educ)

## anos de experiencia laboral
bd2$exp <- bd2$EDAD - bd2$educ - 6
bd2$exp_2 <-bd2$exp**2 #experiencia cuadratica

################################################################################
#regresion

##regresion
summary(lm(ln_ingreso ~ educ + exp + exp_2, data = bd2))

## guardar regresion
m1 <- lm(ln_ingreso ~ educ + exp + exp_2, data = bd2)
m2 <- lm(ln_ingreso ~ educ + exp + exp_2 + educ_2, data = bd2)

##importar regresion a html
library(stargazer)
stargazer(m1,m2, type = "html", out= "Regresion_Mincer.html",
          dep.var.labels = "Ln(ingreso laboral)",
          covariate.labels = c("Años educativos","Experiencia laboral", 
          "Experiencia laboral 2", "Años educativos 2"))