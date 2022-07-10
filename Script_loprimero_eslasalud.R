

#Realizacion  del trabajo primero es la salud

##Paquetes utilizados
library(ggplot2)
library(dplyr)
library(tdlyr)
library(ggplot2)
library(tidyverse)
library(lattice)
library(dplyr)
library (tidyr)

################################## Proyecto de investigacion  ########################################

##

library(readr)

Tasas_suicidio <- read_csv("Tasas_suicidio.csv") #base de datos
View(Tasas_suicidio)

##Elaborar grafico para las 23 observaciones de nuestra (tasa de suicidio)

ggplot(Tasas_suicidio, aes(aÃ±o, tasa_suicidio, color=Pais))+ #grafico de tasa de suicido
  geom_line()+ facet_wrap(~Pais, ncol = 6)+ theme_get()+ xlab("AÃ±o")+
  ylab("Tasa de suicidio")+ labs(title = "EvoluciÃ³n de la tasa de suicidios
para la regiÃ³n de AmÃ©rica: 2000 a 2019.")+ theme(legend.position = "none") #esto borra la leyenda de la funciÃ³n color (etiquetar paises)


summary(Tasas_suicidio)

#############################################################################


##################  Gasto en Salud y nÃºmero de psiquiatras  ########

library(readr)
Gasto_salud <- read_csv("Gasto_salud.csv")
View(Gasto_salud)
summary(Gasto_salud)

#################### Hacer Cloustering

# Paquetes
install.packages("stats")
library(stats)

install.packages("cluster")
library(cluster)

install.packages("mclust") 
library(mclust)

install.packages("factoextra")
library(factoextra)

install.packages("dendextend")
library(dendextend)

library(readr)

library(readr)

library(readr)
Mental <- read_csv("Mental.csv")

mental <- as.numeric(Mental)

head(Mental)
str(Mental)

###Para hacer clustering por manhatatm
set.seed(123)
pam_clusters <- pam(x = Mental, k = 4, metric = "manhattan")
pam_clusters


fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering paÃ­ses") +
  theme(legend.position = "none" )

############################### convertir en logaritmo a la variable educacion ############


Tasas_suicidio <-mutate(Tasas_suicidio, log_educa= log(Educa_))


###Hacer el modelo con los  terminos constitutivos

modelo <- lm(tasa_suicidio  ~ Gasto_salud* log_educa, Tasas_suicidio) 

library(stargazer)

stargazer(modelo, type = "text")



################  Modelo dos con variables de control #################

modelo2 <- lm(tasa_suicidio  ~ Gasto_salud* log_educa +desempleo+ 
                Creci_econ, Tasas_suicidio) 


summary(modelo2)  

Factorizando

y= B0+B1X1+(B2+B3X1)X

449.88620 - (74.29689*7.30)+ (-99.08602+16.6039*7.30)*4 #impacto en la tasa de suicidios

summary(Tasas_suicidio)

##########################  Efectos Marginales grafico #############3

library("interplot")
interplot(m = modelo, var1 = "log_educa", var2 = "Gasto_salud", hist = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed")+ xlab ("Gasto en Salud")+ 
  ylab("Impacto en la tasa de suicidios")+ labs(title = "GrÃ¡fico de efectos marginales con
        un intervalo a 95% de confianza")





