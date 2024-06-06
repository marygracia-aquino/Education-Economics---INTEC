################################################################################
'
Proyecto: Economia de la Educacion - INTEC
Que hace: Estima modelo de crecimiento economico 
Autora: Marygracia Aquino
Ultima actualizacion: 15 junio 2022
'
################################################################################

#Importar y limpiar base de datos

##establece carpeta de trabajo
## cambiar \ a / para computadoras windows
setwd(dir = "C:/Users/maryg/OneDrive/3_Jobs/8_intec/s7-C12")

## importar base de datos
library(haven)
hw <- read_dta("hawo2012tabs.dta")

################################################################################

#Regresiones

## estadisticos
summary(lm(ypcgr ~ ypc60 + ed60, data = hw))
summary(lm(ypcgr ~ ypc60 + ed60 + tmeanmsagay, data = hw))
summary(lm(ypcgr ~ ypc60 + ed60 + tmeanmsagay + open + exprop, data = hw))

##guadar regresiones
m1 <- lm(ypcgr ~ ypc60 + ed60, data = hw)
m2 <- lm(ypcgr ~ ypc60 + ed60 + tmeanmsagay, data = hw)
m3 <- lm(ypcgr ~ ypc60 + ed60 + tmeanmsagay + open + exprop, data = hw)

##exportar
library(stargazer)
stargazer(m1,m2,m3, type = "html", digits = 3,
          out= "Crecimiento_economico_educacion.html",
          dep.var.labels = "Crecimiento promedio PIB per cápita 1960-2000",
          covariate.labels = c("PIB per cápita 1960","Años educativos promedios 1960", 
                               "Puntaje promedio", "Apertura comercial", 
                               "Protección contra la expropiación","Constante"))