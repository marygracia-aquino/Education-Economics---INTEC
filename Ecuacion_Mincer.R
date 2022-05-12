################################################################################
"Proyecto: Economia de la Educacion - INTEC
Que hace: prepara bd y estima ecuacion de Mincer
Autora: Marygracia Aquino
Actualizacion: 11 de mayo de 2022"
################################################################################

#Limpieza de datos

##establecer carpeta de trabajo
setwd(dir = "C:/Users/maryg/OneDrive/3_Jobs/8_intec/s2-c4")

##Cargar ENCFT 2021
library(readxl)
bd1 <- read_excel("Base ENCFT 20201 - 20204.xlsx", sheet = 3)

##ver listado de variables para quedarse con las importantes
names(bd1)

##quedarse con las variables importantes
bd2 <- bd1[,c(1,5,10:13,17,19,21,114,119:139,147,148,195,216,521:539,555,558,562)]
bd2 <- bd2[,-c(4,5,44:47)]

##quedarse con el ultimo trimestre
library(dplyr)
bd2 <- filter(bd2, TRIMESTRE == 20203)

##estadisticas de la bd
head(bd2)

library(Hmisc)
describe(bd2)

## exportat datos
library(writexl)
write_xlsx(bd2,"ENCFT_20203_Mincer.xlsx")

################################################################################

#estadisticas descriptivas 
summary(bd2)

#Crear variables importantes

##ingreso

###crear variable
names(bd2)
bd2$ingreso_laboral <- rowSums(bd2[,48:51])

#grafico
options(scipen=999) #quitar notacion cientifica
jpeg(file="hist_ingreso_abs.jpeg")
hist(bd2$ingreso_laboral,main = "", ylab = "Frecuencia",
     xlab = "Ingreso laboral mensual", col = "darkred")
dev.off()

### convertir ln
bd2$ln_ingreso_laboral <- log(bd2$ingreso_laboral)
bd2$ln_ingreso_laboral[bd2$ingreso_laboral<1] <- NA #convertir ingreso laborales menores que 1 en NA
jpeg(file="hist_ingreso_ln.jpeg")
hist(bd2$ln_ingreso_laboral,main = "", ylab = "Frecuencia",
     xlab = "Ln ingreso laboral mensual", col = "darkred")
dev.off()

##anos educativos completados
table(bd2$NIVEL_ULTIMO_ANO_APROBADO,bd2$ULTIMO_ANO_APROBADO)
table(is.na(bd2$NIVEL_ULTIMO_ANO_APROBADO),is.na(bd2$ULTIMO_ANO_APROBADO))

bd2$nivel_educ <- ifelse(is.na(bd2$NIVEL_ULTIMO_ANO_APROBADO),0,
                           bd2$NIVEL_ULTIMO_ANO_APROBADO)
table(bd2$nivel_educ,bd2$NIVEL_ULTIMO_ANO_APROBADO)
bd2$ultimo_ano <- ifelse(is.na(bd2$ULTIMO_ANO_APROBADO),0,bd2$ULTIMO_ANO_APROBADO)
table(bd2$ultimo_ano,bd2$ULTIMO_ANO_APROBADO)

bd2$educ <- NA
bd2$educ[bd2$nivel_educ==1] <- 0  #preescolar
bd2$educ[bd2$nivel_educ==10] <- 0 #quisqueya aprende contigo
bd2$educ[bd2$nivel_educ==9] <- 0 #niguno
bd2[bd2$nivel_educ==2,]$educ <- bd2[bd2$nivel_educ==2,]$ultimo_ano #primario
bd2[bd2$nivel_educ==3,]$educ <- bd2[bd2$nivel_educ==3,]$ultimo_ano + 8 #secundario
bd2[bd2$nivel_educ==4,]$educ <- bd2[bd2$nivel_educ==4,]$ultimo_ano + 8 #secundario tecnico
bd2[bd2$nivel_educ==5,]$educ <- bd2[bd2$nivel_educ==5,]$ultimo_ano + 12 #universitario
bd2[bd2$nivel_educ==6,]$educ <- bd2[bd2$nivel_educ==6,]$ultimo_ano + 16 #postgrado
bd2[bd2$nivel_educ==7,]$educ <- bd2[bd2$nivel_educ==7,]$ultimo_ano + 16 #maestria
bd2[bd2$nivel_educ==8,]$educ <- bd2[bd2$nivel_educ==8,]$ultimo_ano + 18 #postgrado
table(bd2$educ)

###cuadratica
bd2$educ_2 <- bd2$educ**2

##anos de experiencia laboral

###absoluto
bd2$exp <- bd2$EDAD - bd2$educ - 6

###cuadratico
bd2$exp_2 <- bd2$exp**2

################################################################################

#Estimacion de ecuacion Mincer
m1 <- lm(ln_ingreso_laboral ~ educ + exp + exp_2, data = bd2)
m2 <- lm(ln_ingreso_laboral ~ educ + exp + exp_2 + educ_2, data = bd2)

## guardar modelos en una lista
library(stargazer)
stargazer(m1,m2, type = "html", out = "Regresion.html",
          dep.var.labels=c("Ln(ingreso)"),
          covariate.labels=c("Años educativos","Experiencia laboral", 
                             "Experiencia laboral 2", "Años educativos 2"))