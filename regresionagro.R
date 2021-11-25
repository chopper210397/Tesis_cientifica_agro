# regresion lineal agro vs pobreza
# parte 3 del proyecto
# install.packages("plm")
library(readxl)
library(plm)

dataagro<-read_xlsx("excel1.xlsx")

plm(formula = pobre~prodcafe + edusup, data = dataagro, model = "within", index = c("depa","year"), effects = "twoways" )





















