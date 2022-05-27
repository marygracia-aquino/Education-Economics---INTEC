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
setwd(dir = "C:/Users/maryg/OneDrive/3_Jobs/8_intec/s4-c8/Sheepskin")

## importar base de datos
library(readxl)
bd1 <- read_excel("ENFT_abril2015_Mincer_sheepskin.xlsx")

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
table(is.na(bd1$EFT_ULT_NIVEL_ALCANZADO),is.na(bd1$EFT_ULT_ANO_APROBADO)) #0 vacios

### crear anos educativos
bd1$educ <- NA
bd1$educ[bd1$EFT_ULT_NIVEL_ALCANZADO==2] <- bd1[bd1$EFT_ULT_NIVEL_ALCANZADO==2,]$EFT_ULT_ANO_APROBADO #primaria
bd1$educ[bd1$EFT_ULT_NIVEL_ALCANZADO==3] <- bd1[bd1$EFT_ULT_NIVEL_ALCANZADO==3,]$EFT_ULT_ANO_APROBADO + 8 #secundaria
bd1$educ[bd1$EFT_ULT_NIVEL_ALCANZADO==4] <- bd1[bd1$EFT_ULT_NIVEL_ALCANZADO==4,]$EFT_ULT_ANO_APROBADO + 8 #vocacional
bd1$educ[bd1$EFT_ULT_NIVEL_ALCANZADO==5] <- bd1[bd1$EFT_ULT_NIVEL_ALCANZADO==5,]$EFT_ULT_ANO_APROBADO + 12 #unversitario
bd1$educ[bd1$EFT_ULT_NIVEL_ALCANZADO==6] <- bd1[bd1$EFT_ULT_NIVEL_ALCANZADO==6,]$EFT_ULT_ANO_APROBADO + 16 #postuniversitario
bd1$educ[bd1$EFT_ULT_NIVEL_ALCANZADO==7] <- 0 #ninguno
bd1$educ[bd1$EFT_ULT_NIVEL_ALCANZADO==1] <- 0 #ninguno

### tabla de frencuencia ano educacion
table(bd1$educ)

## anos de experiencia laboral

### creacion variables
bd1$exp <- bd1$EFT_EDAD - bd1$educ - 6
bd1$exp_2 <-bd1$exp**2 #experiencia cuadratica

### grafico 
hist(bd1$exp) 
plot(bd1$EFT_EDAD,bd1$exp)

################################################################################
#Estimacion ecuacion Mincer

##regresion
summary(lm(ln_ingreso ~ educ + exp + exp_2, data = bd1))
summary(lm(ln_ingreso ~ educ + exp + exp_2, data = bd1, weights = EFT_FACTOR_EXP_ANUAL))

## guardar regresion
m1 <- lm(ln_ingreso ~ educ + exp + exp_2, data = bd1, weights = EFT_FACTOR_EXP_ANUAL)

##visualizar regresion
library(stargazer)
stargazer(m1, type = "text", style = "all", digits = 3,
          dep.var.labels = "Ln(ingreso laboral)",
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Constante"))
## exportar regresion
stargazer(m1, type = "html", style = "all", digits = 3,
          out = "Regresion_Mincer.html",
          dep.var.labels = "Ln(ingreso laboral)",
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Constante"))

################################################################################
# Variables para efecto sheepskin o efecto diploma

## primaria
table(bd1$COMPLETO_EDUCACION_PRIMARIA)
table(is.na(bd1$COMPLETO_EDUCACION_PRIMARIA)) #7492 vacia
bd1$dip_primaria <- NA
bd1$dip_primaria[bd1$COMPLETO_EDUCACION_PRIMARIA==1] <- 1
bd1$dip_primaria[bd1$COMPLETO_EDUCACION_PRIMARIA==2] <- 0
table(bd1$dip_primaria)
sum(table(bd1$dip_primaria)) #17369

## secundaria
table(bd1$COMPLETO_EDUCACION_SECUNDARIA)
table(is.na(bd1$COMPLETO_EDUCACION_SECUNDARIA)) #15748 vacias
bd1$dip_secundaria <- NA
bd1$dip_secundaria[bd1$COMPLETO_EDUCACION_SECUNDARIA==2 | !is.na(bd1$dip_primaria)] <- 0
bd1$dip_secundaria[bd1$COMPLETO_EDUCACION_SECUNDARIA==1] <- 1 #5284
table(bd1$dip_secundaria)
sum(table(bd1$dip_secundaria)) #17371

## tecnico superior
table(bd1$ESTA_INSCRITO_EN,bd1$COMPLETO_PROGRAMA_INSCRITO)
table(is.na(bd1$ESTA_INSCRITO_EN),is.na(bd1$COMPLETO_PROGRAMA_INSCRITO)) #19559 vacias
bd1$dip_tec_sup <- NA
bd1$dip_tec_sup[!is.na(bd1$dip_primaria)] <- 0
bd1$dip_tec_sup[bd1$ESTA_INSCRITO_EN==2 & bd1$COMPLETO_PROGRAMA_INSCRITO==1] <- 1
bd1$dip_tec_sup[bd1$ESTA_INSCRITO_EN==3 & bd1$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #100
table(bd1$dip_tec_sup)
sum(table(bd1$dip_tec_sup)) #17369

## carrera universitaria
bd1$dip_licenciatura <- NA
bd1$dip_licenciatura[!is.na(bd1$dip_primaria)] <- 0
bd1$dip_licenciatura[bd1$ESTA_INSCRITO_EN==1 & bd1$COMPLETO_PROGRAMA_INSCRITO==1] <- 1  #1293
table(bd1$dip_licenciatura)
sum(table(bd1$dip_licenciatura)) #17369

################################################################################
#Estimacion efecto sheepskin o efecto diploma

##regresion
summary(lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = bd1))
summary(lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = bd1, weights = EFT_FACTOR_EXP_ANUAL)) #con factor de expansion

## guardar regresion
m2 <- lm(ln_ingreso ~ educ + exp + exp_2 + dip_primaria + dip_secundaria + dip_tec_sup + dip_licenciatura, data = bd1, weights = EFT_FACTOR_EXP_ANUAL)

##importar regresion a html
stargazer(m1,m2, type = "html", style = "all", digits = 3,
          out= "Regresion_Sheepskin.html",
          dep.var.labels = "Ln(ingreso laboral)",
          covariate.labels = c("Años educativos","Experiencia laboral", 
                               "Experiencia laboral 2", "Diploma primaria", 
                               "Diploma secundaria", "Diploma técnico superior", 
                               "Diploma licenciatura","Constante"))