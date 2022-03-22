# DATA PANEL WORK
library(ggplot2)
library(dplyr)
library(lubridate)

# HECHOS ESTILIZADOS

# TRASNFORMANDO LA DATA

# LA VARIABLE QUE CONTIENE ESTA DATA ES "Public spending on education", ESTO VIENE A SER EL VALUE

# VARIABLES: PRY_TRY -> primaria a terciaria

# PRY_NTRY-> primaria a post secundaria no terciaria

# TRY-> EDUCACIÓN TERCIARIA

# LA VARIABLES ES PORCENTAJE DE LA INVERSION PUBLICA DESTINADA A EDUCACIÓN

# LA EVALUACION la haremos sobre la educación terciaria (TRY)
# , es decir la que sigue de la educación secundaria, tal como universidad
data<-read.csv("C:\\Users\\User\\Downloads\\ocde2.csv")
colnames(data)[1]<-"PAIS"
# trabajamos solo con data de la educación terciaria
data<-data %>% filter(SUBJECT=="TRY")
# damos formato a la fecha
data$TIME<-as.Date.character(data$TIME,"%Y")
str(data)
# grafico de todos los paises a través de las dos ultimas decadas para educación terciaria
data %>% ggplot(mapping = aes(x=TIME,y=Value,color=PAIS))+geom_line()+labs(x="",y="% de inversion publica")
ggsave("totalcountries.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")

# dado que no es facilmente observable lo separaremos en sub grupos

# grupo sudamerica
sudamerica<-data %>% filter(PAIS %in% c("ARG","BRA","CHL","COL"))
sudamerica %>% ggplot(mapping = aes(x=TIME,y=Value,color=PAIS))+geom_line()+geom_point()+labs(x="",y="% de inversion publica")
ggsave("sudamerica.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")


# grupo europa
europa<-data %>% filter(PAIS %in% c("CZE","ESP","FIN","SWE"))
europa %>% ggplot(mapping = aes(x=TIME,y=Value,color=PAIS))+geom_line()+geom_point()+labs(x="",y="% de inversion publica")
ggsave("europa.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")

#grupo norteamerica
norteamerica<-data %>% filter(PAIS %in% c("MEX","USA","CAN"))
norteamerica %>% ggplot(mapping = aes(x=TIME,y=Value,color=PAIS))+geom_line()+geom_point()+labs(x="",y="% de inversion publica")
ggsave("norteamerica.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")

# dado que no todos los paises cuentan con data completa, escogeremos aquellos que contengan data completa por cada continente
# se seleciconaro dos por 
paisesdatacompelta<-data %>% filter(PAIS %in% c("CHL","BRA","FIN","ESP","CAN","MEX"))
# EVALUANDO DESDE EL AÑO 2000
paisesdatacompelta %>% filter(TIME>"1999-01-30") %>%
  ggplot(mapping = aes(x=TIME,y=Value,color=PAIS))+
  geom_line()+geom_point()+
  labs(x="",y="% de inversión pública",
       title = "Porcentaje de la inversión pública destinada al sector educación a nivel terciario")
ggsave("paisesdatacompleta.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")

# BOXPLOTS AND DENSITY FUNCTIONS
norteamerica %>% ggplot(mapping = aes(x=Value,color=PAIS))+geom_boxplot()+labs(x="% de inversion publica")+
  theme( axis.ticks.y=element_blank(),axis.text.y=element_blank())
ggsave("boxplotnorteamerica.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")

sudamerica %>% ggplot(mapping = aes(x=Value,color=PAIS))+geom_boxplot()+labs(x="% de inversion publica")+
  theme( axis.ticks.y=element_blank(),axis.text.y=element_blank())
ggsave("boxplotsudamerica.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")

europa %>% ggplot(mapping = aes(x=Value,color=PAIS))+geom_boxplot()+labs(x="% de inversion publica")+
  theme( axis.ticks.y=element_blank(),axis.text.y=element_blank())
ggsave("boxploteuropa.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")

paisesdatacompelta %>% ggplot(mapping = aes(x=Value,color=PAIS))+geom_density()+labs(x="% de inversión pública",
                                                                                     title = "Funciones de densidad de paises seleccionados por continente")
ggsave("boxplotpaisesdatacompleta.png",dpi = 500,
       path = "C:\\Users\\User\\OneDrive - Laboratorios Lansier\\Documentos\\GitHub\\Tesis_cientifica_agro\\imagenesdatapanelpaper")




#----------------------------------------------------------------------------------------------------------------#
#--------------------------------------- data total para data panel model ---------------------------------------#
#----------------------------------------------------------------------------------------------------------------#
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(tseries)
# install.packages("car")
library(car)
# install.packages("gplots")
library(gplots)
# install.packages("foreign")
library(foreign)
# install.packages("stargazer")
library(stargazer)
# install.packages("plm")
library(plm)

# DATA PANEL WORK : 31/01/2022
# la data viene con los decimales como comas y no como puntos pero R para esta pc lo toma como puntos
# asi que debo convertir
# la educación terciaria en esta ocasión esta como porcentaje del gasto en educación total, no del gasto público total
data<-read.csv("C:\\Users\\LBarrios\\Downloads\\datatotal.csv",dec = ",")
colnames(data)[3]<-"educacionporcentajedegastopublico"
colnames(data)[4]<-"educacionterciariaporcentajedegastopublico"
colnames(data)[5]<-"crecimientopbianual"

str(data)
summary(data)
# ahora vemos que solo tenemos problema con la data de educación terciaria
# la corregimos a dos digitos enteros y uno despues del punto
data$educacionterciariaporcentajedegastopublico
data$educacionterciariaporcentajedegastopublico<- gsub(".","", data$educacionterciariaporcentajedegastopublico,fixed = TRUE)
data$educacionterciariaporcentajedegastopublico<-paste0(substr(data$educacionterciariaporcentajedegastopublico,0,2),".",substr(data$educacionterciariaporcentajedegastopublico,3,3))
data$educacionterciariaporcentajedegastopublico<-as.numeric(data$educacionterciariaporcentajedegastopublico)
#convirtiendo a date la fecha a fin de poder trabajarlo mejor
data$TIME<-ymd(data$TIME,truncated = 2L)
# OBSERVANDO COMPORTAMIENDO POR VARIABLES ENTRE LOS DISTINTOS PAISES A EVALUAR
data %>% ggplot(mapping = aes(x=TIME,y=educacionporcentajedegastopublico,color=PAIS))+geom_line(size=2)
ggsave("educacionporcentajegastopublico.png",dpi = 500)
data %>% ggplot(mapping = aes(x=TIME,color=PAIS,y=educacionterciariaporcentajedegastopublico))+ geom_line(size=2)
ggsave("educacionterciariaporcentajedegastopublico.png",dpi = 500)
data %>% ggplot(mapping = aes(x=TIME,color=PAIS,y=crecimientopbianual))+ geom_line(size=1)
ggsave("crecimientopbianual.png",dpi = 500)

# verificando estacionariedad de las series
# cuando una serie o proceso tiene raíz unitaria, la serie no es estacionaria y los estimadores MCO no tienen distribución normal.
# las raíces unitarias son una causa de no estacionariedad.
# p-valor inferior a 0.05, la hipótesis nula se suele rechazar.
data %>% filter(PAIS=="ESP") %>% ggplot(aes(x=TIME,y=crecimientopbianual))+geom_line()
gdpgrowthesp<-data %>% filter(PAIS=="ESP") %>% select(crecimientopbianual)
gdpgrowthesp<-ts(gdpgrowthesp,start = c(2000),end = (2018))
gdpgrowthesp



coplot(crecimientopbianual ~ TIME|PAIS, type="l", data=data) 
ggsave("coplotcrecimientopbi.png",dpi = 400)

jpeg("heterogeneidadcrecimientoentrepaises.jpg", width = 350, height = 350)
plotmeans(crecimientopbianual ~ PAIS, main="Heterogeneidad a través de los países", data=data)
dev.off()

jpeg("heterogeneidadcrecimientoentreaños.jpg", width = 350, height = 350)
plotmeans(crecimientopbianual ~ TIME, main="Heterogeneidad a través de los años", data=data)
dev.off()

#----------------------------------------------------------------------------------------------------------#
#------------------------------------ ESTIMACION MODELOS ECONOMETRICOS ------------------------------------#
#----------------------------------------------------------------------------------------------------------#

# estimando el modelo de efectos fijos
efectosfijos<-plm(formula =  crecimientopbianual ~ educacionporcentajedegastopublico+educacionterciariaporcentajedegastopublico, 
                  data = data,
                  index = c("PAIS", "TIME"), 
                  model = "within",
                  effect = "twoways")

summary(efectosfijos)
stargazer(efectosfijos)


efectosaleatorios<-plm(formula =  crecimientopbianual ~ educacionporcentajedegastopublico+educacionterciariaporcentajedegastopublico, 
                       data = data,
                       index = c("PAIS", "TIME"), 
                       model = "random",
                       effect = "twoways")
summary(efectosaleatorios)
stargazer(efectosaleatorios)  


pooled<-plm(formula =  crecimientopbianual ~ educacionporcentajedegastopublico+educacionterciariaporcentajedegastopublico, 
            data = data,
            index = c("PAIS", "TIME"), 
            model = "pooling",
            effect = "twoways")
summary(pooled)
stargazer(pooled)  

# tes de hasuman
# eleccion entre modelos
phtest(efectosfijos, efectosaleatorios)
# si es menor a 0.05 usar efectos fijos, si es mayor usar efectos aleatorios en esta ocasion el p-value es mayor a 0.05, por lo tanto seguiremos con efectos aleatorios
# usamos efectos aleatorios


# test de breusch-pagan
plmtest(pooled  , type=c("bp"))
# como es mayor a 0.05, no hay diferencia significante entre los paises
# por lo tanto es apropiado utilizar el modelo pooled

# OBSERVANDO CORRELACIONES
data %>% ggplot(data = data,mapping = aes(x=educacionporcentajedegastopublico,y=crecimientopbianual))+geom_smooth()
ggsave("correlationterciariacrecimiento.png",dpi = 400)
data %>% ggplot(data = data,mapping = aes(x=educacionterciariaporcentajedegastopublico,y=crecimientopbianual))+geom_smooth()
ggsave("correlationeducacioncrecimiento.png",dpi = 400)

