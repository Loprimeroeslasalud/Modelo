

##Realización del modelo

library(ggplot2)
library(dplyr)
library(tdlyr)
library(ggplot2)
library(tidyverse)
library(lattice)
library(dplyr)
library (tidyr)
library(readr)

############################  Hecer logaritmos la variable

Tasas_suicidio <-mutate(Tasas_suicidio, log_educa= log(Educa_), 
                        log_gasto= log(Gasto_salud), log_cre= log(Creci_econ))

View(Tasas_suicidio) 

summary(Tasas_suicidio)




 
############################  Modelo con efecto en la variable dependiente  ########

modelo3 <- lm(tasa_suicidio  ~ Gasto_salud* log_educa +desempleo+ 
                Creci_econ, Tasas_suicidio) 


summary(modelo3)       


modelo <- lm(tasa_suicidio  ~ Gasto_salud* log_educa, Tasas_suicidio) 


stargazer(modelo, type = "text")


library(stargazer)

stargazer(modelo3,type="text")


#Ecuacion

449.88620 - (74.29689*7) -99.08602*4.521+16.6039*7*4.521

summary(Tasas_suicidio)
Factorizando

y= B0+B1X1+(B2+B3X1)X

449.88620 - (74.29689*7.30)+ (-99.08602+16.6039*7.30)*4

summary(Tasas_suicidio)

##########################  Efectos Marginales

install.packages("margins")

library(margins)

modelo <- lm (tasa_suicidio ~ Gasto_salud*log_educa, Tasas_suicidio)

m <-  margins(modelo) #Sacar los efectos marginales

summary(m)

plot(m)

cplot(modelo, x="Gasto_salud",
      dx =  "log_educa", what = "effect", col = "blue" ) #Grafico de efectos marginales

me <- cplot(modelo, x="log_educa",
            dx =  "Gasto_salud", what = "effect", col = "blue" )
library(stargazer)

stargazer(me, type = "text")


persp(modelo, "log_educa", "Gasto_salud",col = "lightgreen",phi = 10, expand = 0.9,
      theta = 240, ltheta = 60, shade = .90, ticktype = "detailed" ) #grafico en 3D


head(me, n=15)
head(me, n=18) #valores de los efectos
head(me, n=25)


###################################################################################

Salud <-mutate(Salud, log_educa= log(Educa_), 
                        log_gasto= log(Gasto_salud), log_cre= log(Creci_econ))

View(Salud) 
