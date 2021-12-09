#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#-------------------- AN罫ISIS PRELIMINAR DE DATOS -------------------------#
#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#

#--------------------------
# Librerias
#--------------------------

# install.packages("lmtest")
# install.packages("skedastic")
# install.packages("stargazer")
# install.packages("gplots")
# install.packages("plyr")
# install.packages("stringi")
# install.packages("foreign")
# install.packages("DescTools")
# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("sp")
# install.packages("tmap")
# install.packages("ozmaps") 
# install.packages("gtsummary")
# install.packages("psych")
library(psych)
library(gtsummary)
library(leaflet)
library(rgdal)
library(rgeos)
library(sp)
library(tmap)
library(foreign)
library(readxl)
library(lmtest)
library(skedastic)
library(stargazer)
library(gplots)
library(plm)
library(plyr)
library(tidyverse)
library(stringi)
library(foreign)
library(DescTools)
library(car)
library(kableExtra)
library(tidyr)
library(broom)
library(sf)

#----------------------- IMPORTANDO DATA NECESARIA PARA EL PAPER -----------------------#
peru_d <- st_read("C:/Users/LBarrios/Downloads/MAPA/dp.shp")
dataagro <- read_xlsx("excel1.xlsx")

#------- PROBANDO creacion del mapa-------------#
# 
# ggplot(data = peru_d) +
#   geom_sf()
# 
# ggplot(data = peru_d %>%
#          filter(NOMBDEP=="AREQUIPA")) +
#   geom_sf()

#-------------- agrupando data prodcafe y edusup por departamento -----------#
depascafe<-dataagro %>% group_by(depa) %>% summarise(prodcafe=sum(prodcafe))
depascafe$depa<-toupper( depascafe$depa )
names(depascafe)[1]="NOMBDEP"

peru_datos<-peru_d %>% 
  left_join(depascafe)
#
depaseducacion<-dataagro %>% group_by(depa) %>% summarise(edusup=sum(edusup))
depaseducacion$depa<-toupper(depaseducacion$depa)
names(depaseducacion)[1]="NOMBDEP"

peru_datos_educ<-peru_d %>% 
  left_join(depaseducacion)
#---------- GENERANDO EL MAPA ---------#

#--- PRODCAFE----#
ggplot(peru_datos) +
  geom_sf(aes(fill = prodcafe))+
  labs(title = "Producci贸n de caf茅 por departamentos 2007-2019",
       caption = "Fuente: SIRTOD (2020)
       Elaboraci贸n propia"
       )+
  scale_fill_continuous(low="white",
                        high="red" ,
                        guide_legend(title = "Producci贸n de caf茅"))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# COLORES BLANCOS DONDE PRODUCCI脫N CERCANA A CERO
# ROJO CUANDO PRODUCCI脫N ES MAYOR

#---- EDUSUP ----#
ggplot(peru_datos_educ) +
  geom_sf(aes(fill = edusup))+
  labs(title = "Educaci贸n superior por departamentos 2007-2019",
       caption = "Fuente: INEI (2020)
       Elaboraci贸n propia"
  )+
  scale_fill_continuous(low="white",
                        high="blue" ,
                        guide_legend(title = "Educaci贸n superior"))+
  theme(axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
# mas azul mas nivel de educaci贸n



#----------------- GRAFICOS QUE RELACIONAN LA PRODUCCION DE CAFE A TRAV脡S DE LOS A脩OS POR DEPARTAMENTO -----------------#

# coplot(prodcafe ~ year|depa, type="l", data=dataagro) # Lines # no paree muy util
# 
# scatterplot(prodcafe~year|depa,
#             boxplots=FALSE,
#             smooth=TRUE,
#             reg.line=FALSE,
#             data=dataagro) # se ve util aunque se podria escoger menos departamentos

# HETEROGENEIDAD A TRAVES DE LOS DEPARTAMENTOS

# plotmeans(prodcafe ~ depa,
#           main="Heterogeineity across departments",
#           data=dataagro,
#           connect = FALSE,
#           n.label = FALSE,
#           xlab = "Departamentos",
#           ylab = "Producci贸n de caf茅")

# heterogeneidad a trav茅s de los a帽os
# plotmeans(prodcafe ~ year,
#           main="Heterogeineity across years",
#           data=dataagro,
#           n.label = FALSE,
#           xlab = "A帽os",
#           ylab = "Producci贸n de caf茅")

# tendencia top 6 departamentos produccion de cafe
dataagro %>% filter(depa %in% c("San Martin","Junin","Cajamarca","Amazonas","Cusco","Pasco")) %>% 
  ggplot(aes(x = year, y = prodcafe)) +
  geom_path(aes(colour = as.factor(depa)),size=1,linetype=1) +
  labs(x = "A帽os",  y = "Producci贸n de caf茅",title = "Departamentos con mayor producci贸n de caf茅 ") +
  theme(legend.position = "bottom",legend.title =element_blank())

#Seleccionando el top 6 de departamentos con mayor producci贸n de caf茅

# heterogeneidad mediante boxplots por departamento
dataagro %>%
  group_by(depa) %>%
  summarise(inv_mean = mean(prodcafe)) %>%
  left_join(dataagro) %>%
  ggplot(data = ., 
         aes(x = reorder(as.character(depa), depa), y = prodcafe)) +
  geom_boxplot(color="darkblue") +
  geom_line(aes(x = depa, y = inv_mean)) +
  labs(x = "", y = "Producci贸n de caf茅",title = "Heterogeneidad por departamento")+coord_flip()

# tendencia de la produccion de cafe en el pa铆s a trav茅s de los a帽os de estudio
dataagro %>%
  group_by(year) %>% 
  summarise(produccion=mean(prodcafe)) %>% 
  ggplot(aes(x=year,y=produccion))+geom_smooth()+
  labs(x="A帽os",y="Producci贸n de caf茅",title = "Tendencia nacional de producci贸n de caf茅")

# ranking de producci贸n de caf茅 por departamento ordenado de mayor a menos
dataagro %>%
  group_by(depa) %>%
  summarise(produccion=sum(prodcafe)) %>%
  ggplot(aes(x=reorder(depa,produccion),y=produccion))+
  geom_bar(stat = "sum",fill="lightblue",show.legend = FALSE)+
  coord_flip()+labs(x="Producci贸n de caf茅",y="",title = "Ranking de producci贸n de caf茅 por departamento")
# tenemos que los mas altos son san martin , junin, cajamarca, amazonas, cusco, paco, puno

#---------- correlaciones -----------#
# correlacion entre produccion de cafe y pobreza
# se encontro una correlaci贸n peque帽a
# se observa que debilmente a medida que se incrementa la producci贸n de caf茅 disminuye la pobreza
dataagro %>% filter(prodcafe>5) %>% 
  ggplot( mapping = aes(x=prodcafe, y=pobre))+
  geom_smooth()+labs(x="Producci贸n de caf茅",y="Pobreza",title="Correlaci贸n entre pobreza y producci贸n de caf茅")

cor(dataagro$pobre,dataagro$prodcafe)


# correlacion entre educacion superior y pobreza
# se observa una correlaci贸n elevada
# a medida que se incrementa la educaci贸n superior, la pobreza en los individuos disminuye
ggplot(data = dataagro, mapping = aes(x=edusup, y=pobre))+
  geom_smooth()+labs(x="Educaci贸n superior",y="Pobreza",title="Correlaci贸n entre pobreza y educaci贸n superior")

cor(dataagro$pobre ,dataagro$edusup)


# grafico de estimaci贸n por variable
dataagro %>% filter(prodcafe>5) %>% 
ggplot(aes(x = prodcafe, y = pobre)) +
  geom_point(aes(shape = factor(depa, 
                                levels = c(1:24)))) +
  geom_smooth(method = "lm", se = F) +
  scale_shape_manual(values = 1:10) +
  labs(x = "Producci贸n de caf茅",
       y = "Pobreza",
       shape = "depa",title = "Estimaci贸n lineal entre pobreza y producci贸n de caf茅")

 
# grafico de estimaci贸n por variable
ggplot(data = dataagro,
       aes(x = edusup, y = pobre)) +
  geom_point(aes(shape = factor(depa, 
                                levels = c(1:10)))) +
  geom_smooth(method = "lm", se = F) +
  scale_shape_manual(values = 1:10) +
  labs(x = "edusup",
       y = "Pobreza",
       shape = "depa",title = "Estimaci贸n lineal entre pobreza y educaci贸n superior")

resumen<-summary(dataagro)
xtable(as.table(resumen), type = "latex")
install.packages("xtable")
library(xtable)

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
#----------------------------- AN罫ISIS EMP蚏ICO --------------------------------#
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# - - - - - - - -- - - - MODELAMIENTO ECONOM蒚RICO - - - - - - - - - - - - - - - #
# - - - - - - - -- - - - MODELAMIENTO ECONOM蒚RICO - - - - - - - - - - - - - - - #
# - - - - - - - -- - - - MODELAMIENTO ECONOM蒚RICO - - - - - - - - - - - - - - - #
# - - - - - - - -- - - - MODELAMIENTO ECONOM蒚RICO - - - - - - - - - - - - - - - #
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

# regresion lineal agro vs pobreza



#------- modelo de efectos fijos --------#

fixedmodel<-plm(formula = pobre~prodcafe + edusup,
    data = dataagro, model = "within",
    index = c("depa","year"),
    effects = "twoways" )

summary(fixedmodel)

#------- modelo de efectos aleatorios --------#

randommodel<-plm(formula = pobre~prodcafe + edusup,
    data = dataagro, model = "random",
    index = c("depa","year"),
    effects = "twoways" )

summary(randommodel)

#------- modelo pooling --------#

poolmodel<-plm(formula = pobre~prodcafe + edusup,
                data = dataagro, model = "pooling",
                index = c("depa","year"),
                effects = "twoways" )

summary(poolmodel)

# generando tabla en formato latex
stargazer(fixedmodel,randommodel,poolmodel,
          column.labels =c("Efectos Fijos","Efectos Aleatorios","Pooled"),
          title = "Comparaci贸n de modelos")
#----------------------------- ELECCION ENTRE AMBOS MODELOS ----------------------#

#--------- Test de Hausman-----------#
# Hip贸tesis nula igual a que el modelo preferido es el de efectos aleatorios
#   "         "   igual a que los erroes unicos no estan correlacionados con los regresores
testhausman<-phtest(fixedmodel, randommodel)
write.table(tidy(testhausman),"testhausman.txt")


# si es menor a 0.05 usar efectos fijos, si es mayor usar efectos aleatorios
# en esta ocasion el p-value es mayor a 0.05, por lo tanto seguiremos con efectos aleatorios


#---------- Breusch Pagan ------------#
breuschpagan<-plmtest(poolmodel, type=c("bp"))
write.table(tidy(breuschpagan),"breuschpagan.txt")

# como es menor a 0.05, si hay diferencia significante entre los departamentos
# por lo tanto es apropiado utilizar el modelo de efectos aleatorios










