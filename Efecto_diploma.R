################################################################################
'
Proyecto: Economia de la Educacion - INTEC
Que hace: Estima educacion como señalizacion
Autora: Marygracia Aquino
Ultima actualizacion: 26 mayo 2022
'
################################################################################

#Importar y limpiar la base de datos

##establece carpeta de trabajo
## cambiar \ a / para computadoras windows
setwd(dir = "C:/Users/maryg/OneDrive/3_Jobs/8_intec/s4-c8")

## importar base de datos
library(readxl)
modulo.educ <- read_excel("Base de datos Módulo Educación, Expectativas y Percepciones.xlsx")
miembros <- read_excel("Base ENFT Abril 2015.xls", sheet = 3)
ingreso <- read_excel("Base ENFT Abril 2015.xls", sheet = 7)

## unir base de datos
library(dplyr)
bd1 <- full_join(miembros,ingreso,by=c("EFT_VIVIENDA","EFT_HOGAR","EFT_MIEMBRO"))
bd1 <- full_join(bd1,modulo.educ,by=c("EFT_VIVIENDA","EFT_HOGAR","EFT_MIEMBRO"))

################################################################################

#Ecuacion Mincer

##ingreso

###histograma con valor absoluto
options(scipen=999)
jpeg(filename = "histogram_ingreso_absoluto.jpeg")
hist(bd1$EFT_INGRESO_MENSUAL, main = "", ylab = "Frecuencia", xlab = "Ingreso mensual",
     col = "firebrick")
dev.off()

### crear logaritmo natural de ingreso
bd1$ln_ingreso <- log(bd1$EFT_INGRESO_MENSUAL)

### personas que gana cero NA
bd1$ln_ingreso[bd1$EFT_INGRESO_MENSUAL<1] <- NA

### histogram del ln ingreso
jpeg(filename = "histogram_ingreso_ln.jpeg")
hist(bd1$ln_ingreso, main = "", ylab = "Frecuencia", xlab = "Ln(ingreso mensual)",
     col = "firebrick")
dev.off()

## anos educativos

### tabla de frecuencia del nivel educativo maximo alcazando y ultimo ano alcanzado en ese nivel educativo
table(bd1$EFT_ULT_NIVEL_ALCANZADO,bd1$EFT_ULT_ANO_APROBADO)

### computando obervacion vacia hay para cada variable (TRUE)
table(is.na(bd1$EFT_ULT_NIVEL_ALCANZADO),is.na(bd1$EFT_ULT_ANO_APROBADO)) #1864 TRUE

#### sacar todos los individuos que no tenga informacion de educacion
bd2 = filter(bd1,!is.na(EFT_ULT_NIVEL_ALCANZADO))

### computando obervacion vacia hay para cada variable (TRUE)
table(is.na(bd2$EFT_ULT_NIVEL_ALCANZADO),is.na(bd2$EFT_ULT_ANO_APROBADO)) #0

### crear anos educativos
bd2$educ <- NA
bd2$educ[bd2$EFT_ULT_NIVEL_ALCANZADO==2] <- bd2[bd2$EFT_ULT_NIVEL_ALCANZADO==2,]$EFT_ULT_ANO_APROBADO #primaria
bd2$educ[bd2$EFT_ULT_NIVEL_ALCANZADO==3] <- bd2[bd2$EFT_ULT_NIVEL_ALCANZADO==3,]$EFT_ULT_ANO_APROBADO + 8 #secundaria
bd2$educ[bd2$EFT_ULT_NIVEL_ALCANZADO==4] <- bd2[bd2$EFT_ULT_NIVEL_ALCANZADO==4,]$EFT_ULT_ANO_APROBADO + 8 #vocaional
bd2$educ[bd2$EFT_ULT_NIVEL_ALCANZADO==5] <- bd2[bd2$EFT_ULT_NIVEL_ALCANZADO==5,]$EFT_ULT_ANO_APROBADO + 12 #unversitario
bd2$educ[bd2$EFT_ULT_NIVEL_ALCANZADO==6] <- bd2[bd2$EFT_ULT_NIVEL_ALCANZADO==6,]$EFT_ULT_ANO_APROBADO + 16 #postuniversitario
bd2$educ[bd2$EFT_ULT_NIVEL_ALCANZADO==7] <- 0 #ninguno
bd2$educ[bd2$EFT_ULT_NIVEL_ALCANZADO==1] <- 0 #ninguno

### tabla de frencuencia ano educacion
table(bd2$educ)

## anos de experiencia laboral

### creacion variables
bd2$exp <- bd2$EFT_EDAD - bd2$educ - 6
bd2$exp_2 <-bd2$exp**2 #experiencia cuadratica

### grafico 
hist(bd2$exp) 
plot(bd2$EFT_EDAD,bd2$EFT_EDAD)

################################################################################
#Estimacion ecuacion Mincer

##regresion
summary(lm(ln_ingreso ~ educ + exp + exp_2, data = bd2))
summary(lm(ln_ingreso ~ educ + exp + exp_2, data = bd2, weights = EFT_FACTOR_EXP_ANUAL)) #con factor de expansion

## guardar regresion
m1 <- lm(ln_ingreso ~ educ + exp + exp_2, data = bd2, weights = EFT_FACTOR_EXP_ANUAL)

##importar regresion a html
library(stargazer)
stargazer(m1, type = "html", out= "Regresion_Mincer.html",
          dep.var.labels = "Ln(ingreso laboral)",
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Constante"))

################################################################################
# Variables para efecto sheepskin o efecto diploma

## primaria
table(bd2$COMPLETO_EDUCACION_PRIMARIA)
table(is.na(bd2$COMPLETO_EDUCACION_PRIMARIA)) #7492 vacia
bd2$dip_primaria <- NA
bd2$dip_primaria[bd2$COMPLETO_EDUCACION_PRIMARIA==2] <- 0
bd2$dip_primaria[bd2$COMPLETO_EDUCACION_PRIMARIA==1] <- 1 #9114
table(bd2$dip_primaria)
sum(table(bd2$dip_primaria)) #17369

## secundaria
table(bd2$COMPLETO_EDUCACION_SECUNDARIA)
table(is.na(bd2$COMPLETO_EDUCACION_SECUNDARIA)) #15748 vacias
bd2$dip_secundaria <- NA
bd2$dip_secundaria[bd2$COMPLETO_EDUCACION_SECUNDARIA==2 | !is.na(bd2$dip_primaria)] <- 0
bd2$dip_secundaria[bd2$COMPLETO_EDUCACION_SECUNDARIA==1] <- 1 #5284
table(bd2$dip_secundaria)
  sum(table(bd2$dip_secundaria)) #17371

## tecnico superior
table(bd2$ESTA_INSCRITO_EN,bd2$COMPLETO_PROGRAMA_INSCRITO)
table(is.na(bd2$ESTA_INSCRITO_EN),is.na(bd2$COMPLETO_PROGRAMA_INSCRITO)) #19559 vacias
bd2$dip_tec_sup <- NA
bd2$dip_tec_sup[!is.na(bd2$dip_primaria)] <- 0
bd2$dip_tec_sup[bd2$ESTA_INSCRITO_EN==2 & bd2$COMPLETO_PROGRAMA_INSCRITO==1] <- 1
bd2$dip_tec_sup[bd2$ESTA_INSCRITO_EN==3 & bd2$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #100
table(bd2$dip_tec_sup)
sum(table(bd2$dip_tec_sup)) #17369

## carrera universitaria
bd2$dip_licenciatura <- NA
bd2$dip_licenciatura[!is.na(bd2$dip_primaria)] <- 0
bd2$dip_licenciatura[bd2$ESTA_INSCRITO_EN==1 & bd2$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #1293
table(bd2$dip_licenciatura)
sum(table(bd2$dip_licenciatura)) #17369

################################################################################
#Estimacion efecto sheepskin o efecto diploma

##regresion
summary(lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = bd2))
summary(lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = bd2, weights = EFT_FACTOR_EXP_ANUAL)) #con factor de expansion

## guardar regresion
m2 <- lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = bd2, weights = EFT_FACTOR_EXP_ANUAL)

##importar regresion a html
library(stargazer)
stargazer(m1,m2, type = "html", out= "Regresion_Sheepskin.html",
          dep.var.labels = "Ln(ingreso laboral)",
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Diploma primaria", 
                               "Diploma secundaria", "Diploma técnico superior", 
                               "Diploma licenciatura","Constante"))

################################################################################

#Exportar datos
bd3 <- bd2[,-c(31:34,65:98)]
library(writexl)
write_xlsx(bd3,"ENFT_abril2015_Mincer_sheepskin.xlsx")