#===========================================#
# author: Andrés Felipe Silva Galvis y Juan Francisco Walteros
# code: 202014837 y 202022380
# version: 4.2.1
# last update: 26/04/2024
#===========================================#


# Preparar el entorno
rm(list=ls())

## load packages
require(pacman)



## usar la función p_load de pacman para instalar/llamar las librerías de la clase
p_load(tidyverse, #funciones para manipular/limpiar conjuntos de datos.
       rio, # función import/export: permite leer/escribir archivos desde diferentes formatos. 
       skimr, # función skim: describe un conjunto de datos
       janitor, # función tabyl: frecuencias relativas
       data.table)



# ================================= 1. Bucle ================================= #


# 1.1)


#help(list.files)
getwd()

carpetas <- list.files(path = "C:/Users/silva/OneDrive/Escritorio/Penúltimo Semestre/Taller de R/ProblemSet3/problem-set3",
                       recursive = T, pattern = "rds")

setwd("C:/Users/silva/OneDrive/Escritorio/Penúltimo Semestre/Taller de R/ProblemSet3/problem-set3")



c <- seq(1,50, by = 4)
carpetas <- carpetas[-c]

#view(c)
#view(carpetas)


# 1.2)

importable <- list()
#help(list)

for (i in 1:length(carpetas)) {
  
  importable[[i]] <- import(carpetas[i])
  
}


# Fuerza_Trabajo =========

a <- seq(1, length(importable) , by=3)
Fuerza_Trabajo <- list()

n=1
for (i in a) {
  
  n = n+1
  Fuerza_Trabajo[n] <- importable[i]
  
}

Fuerza_Trabajo <- Fuerza_Trabajo[-1]


# No_Ocupados =========

a <- seq(2, length(importable), by = 3)
no_ocupados <- list()

n=1
for (i in a) {
  
  n = n+1
  no_ocupados[n] <- importable[i]
  
}

no_ocupados <- no_ocupados[-1]


# Ocupados =============

a <- seq(3, length(importable), by = 3)
ocupados <- list()

n=1
for (i in a) {
  
  n = n+1
  ocupados[n] <- importable[i]
  
}

ocupados <- ocupados[-1]



# 1.3)

#Crear bases bindlist

ft <- rbindlist(Fuerza_Trabajo, use.names = TRUE,
                fill = TRUE ) # Base final Fuerza trabajo.

n_oc <- rbindlist(no_ocupados, use.names = TRUE,
                  fill = TRUE ) #Base final No ocupados.

oc <- rbindlist(ocupados, use.names = TRUE,
                fill = TRUE) #Base final ocupados. 


# ================================= 2. Preparacion ================================= #

# 2.1)
#1)
ft_mes_ft <- ft |> group_by(MES) |> summarise(fuerza_laboral=sum(FEX_C18[FT==1], na.rm=TRUE))

pet_mes <- ft |> group_by(MES) |> summarise(pet=sum(FEX_C18[PET==1], na.rm=TRUE))

ft_2 <- merge(ft_mes_ft,pet_mes, by="MES", all=TRUE) #uno ambas bases para tener una base para 2.1.1

#2)
ft_mes_oc <- oc |> group_by(MES) |> summarise(ocupados=sum(FEX_C18[FT==1], na.rm=TRUE))

#3)
dsi_mes <- n_oc |> group_by(MES) |> summarise(desempleados=sum(FEX_C18[DSI==1], na.rm=TRUE))

# 2.2) 
Output <- merge(ft_2,ft_mes_oc, by="MES", all=TRUE) #uno dos bases a la vez
Output <- merge(Output,dsi_mes, by="MES",all=TRUE) |> rename(mes=MES) 
#uno otra vez y cambio MES por mes para que todo este en minuscula

# 2.3)
Output <- Output |> mutate(tasa_desempleo = desempleados/fuerza_laboral,tasa_ocupacion=ocupados/fuerza_laboral)
#creo dos variables nuevas para la base Output.

export(Output,"output/Output.rds") #exporto la base final a la carpeta output


# ============================== 3. GGPLOT =================================== #


ggplot(Output, aes(x = as.integer(mes))) +
  geom_line(aes(y = tasa_ocupacion, color = "Tasa de Ocupación")) +
  geom_line(aes(y = tasa_desempleo, color = "Tasa de Desempleo")) +
  labs(x = "Mes", y = "Tasa (%)", color = "Leyenda") +
  scale_color_manual(values = c("Tasa de Ocupación" = "green", "Tasa de Desempleo" = "black")) +
  theme_minimal()
