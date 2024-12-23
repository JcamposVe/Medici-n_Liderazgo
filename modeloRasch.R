#MODELO DE ESCALA DE CALIFICACIÓN RASCH
#=======================================

#Correr la siguiente línea de comando al usa por primera vez el script
#install.packages(c("readr", "TAM", "plyr", "WrightMap", "eRm")) 

library(readr) # For import the data
library(TAM) # For running the Rating Scale Rasch Model
library(plyr) # For plot the Item characteristic curves
library(WrightMap)# For plot the variable map
library(eRm) # For another example

# Definir la ruta del archivo
archivo_excel <- ""
setwd("/Users/juancampos/Desktop/Juan Campos /MINEDU/10_Análisis_VAL-ED/10.1.Base")

# Importar la hoja "bd"
base_datos <- readxl::read_excel("Modelo_Rasch_VALED.xlsx")
base_datos <- as.data.frame(base_datos)

# Verificar la base de datos
head(base_datos)
str(base_datos)
colnames(base_datos)

#Eliminamos la variable Id
colnames(base_datos)
install.packages("dplyr")
library(dplyr)
base_datos <- base_datos %>% select(-Id)

#Vemos datos descriptivos
library(TAM)
library(psych)
nitems <- ncol(base_datos)
describe(base_datos)

library(psych)
#Buscamos frecuencia de respuesta
alpha(base_datos)$response.freq

# Instalar eRm si no está instalado
if (!requireNamespace("eRm", quietly = TRUE)) {
  install.packages("eRm")
}

# Agrupar los ítems por dimensión
# Cargar el paquete
library(eRm)
d1_items <- base_datos[, c("P1", "P2", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12")]
d2_items <- base_datos[, c("P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")]
d3_items <- base_datos[, c("P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35", "P36")]
d4_items <- base_datos[, c("P37", "P38", "P39", "P40", "P41", "P42", "P43", "P44", "P45", "P46", "P47", "P48")]

##Análisis del 1er Componente
##-----------------------------

# Ajustar el modelo Rasch de escala de calificación para el componente 1
modelo_d1 <- TAM::tam.mml(d1_items)

# Resumen del modelo ajustado
summary(modelo_d1)

# Verificar los índices de ajuste
summary(modelo_d1)

# Ver el ajuste específico por ítem
tam.fit(modelo_d1)

table(base_datos$P1) # Reemplaza P1 por cualquier ítem

##Identificamos los ìtems pronlemàticos
# Extraer los valores de ajuste del modelo
ajuste_items <- tam.fit(modelo_d1)$itemfit

# Filtrar ítems con problemas de Outfit
outfit_problemas <- ajuste_items[ajuste_items$Outfit > 1.3 | ajuste_items$Outfit < 0.7, ]

# Filtrar ítems con problemas de Infit
infit_problemas <- ajuste_items[ajuste_items$Infit > 1.3 | ajuste_items$Infit < 0.7, ]

# Ver los ítems problemáticos
outfit_problemas
infit_problemas

# Ver frecuencias de un ítem específico (por ejemplo, P1)
table(base_datos$P1)

# O de todos los ítems agrupados
apply(base_datos, 2, table)


##Análisis del 2do componente
##-----------------------------

# Ajustar el modelo Rasch de escala de calificación para el segundo componente
modelo_d2 <- TAM::tam.mml(d2_items)

# Resumen del modelo ajustado
summary(modelo_d2)

# Verificar los índices de ajuste
summary(modelo_d2)

# Ver el ajuste específico por ítem
tam.fit(modelo_d2)

table(base_datos$P13) # Reemplaza P1 por cualquier ítem

##Identificamos los ìtems problemàticos
# Extraer los valores de ajuste del modelo
ajuste_items <- tam.fit(modelo_d2)$itemfit

# Filtrar ítems con problemas de Outfit
outfit_problemas <- ajuste_items[ajuste_items$Outfit > 1.3 | ajuste_items$Outfit < 0.7, ]

# Filtrar ítems con problemas de Infit
infit_problemas <- ajuste_items[ajuste_items$Infit > 1.3 | ajuste_items$Infit < 0.7, ]

# Ver los ítems problemáticos
outfit_problemas
infit_problemas

# Ver frecuencias de un ítem específico (por ejemplo, P1)
table(base_datos$P15)
table(base_datos$P18)

##Análisis del tercer componente
##------------------------------------

# Ajustar el modelo Rasch de escala de calificación para el tercer componente
modelo_d3 <- TAM::tam.mml(d3_items)

# Resumen del modelo ajustado
summary(modelo_d3)

# Verificar los índices de ajuste
summary(modelo_d3)

# Ver el ajuste específico por ítem
tam.fit(modelo_d3)

table(base_datos$P30) # Reemplaza P30 por cualquier ítem

##Identificamos los ìtems problemàticos
# Extraer los valores de ajuste del modelo
ajuste_items <- tam.fit(modelo_d3)$itemfit

# Filtrar ítems con problemas de Outfit
outfit_problemas <- ajuste_items[ajuste_items$Outfit > 1.3 | ajuste_items$Outfit < 0.7, ]

# Filtrar ítems con problemas de Infit
infit_problemas <- ajuste_items[ajuste_items$Infit > 1.3 | ajuste_items$Infit < 0.7, ]

# Ver los ítems problemáticos
outfit_problemas
infit_problemas

# Ver frecuencias de un ítem específico (por ejemplo, P27)
table(base_datos$P27)

##Análisis del cuarto componente
##--------------------------------------

# Ajustar el modelo Rasch de escala de calificación para el cuarto componente
modelo_d4 <- TAM::tam.mml(d4_items)

# Resumen del modelo ajustado
summary(modelo_d4)

# Verificar los índices de ajuste
summary(modelo_d4)

# Ver el ajuste específico por ítem
tam.fit(modelo_d4)

table(base_datos$P37) # Reemplaza P37 por cualquier ítem

##Identificamos los ìtems problemàticos
# Extraer los valores de ajuste del modelo
ajuste_items <- tam.fit(modelo_d4)$itemfit

# Filtrar ítems con problemas de Outfit
outfit_problemas <- ajuste_items[ajuste_items$Outfit > 1.3 | ajuste_items$Outfit < 0.7, ]

# Filtrar ítems con problemas de Infit
infit_problemas <- ajuste_items[ajuste_items$Infit > 1.3 | ajuste_items$Infit < 0.7, ]

# Ver los ítems problemáticos
outfit_problemas
infit_problemas

##Anàlisis de dificultad de parámetros
#---------------------------------------

# Extraer parámetros de los ítems
params1 <- modelo_d1$item
params2 <- modelo_d2$item
params3 <- modelo_d3$item
params4 <- modelo_d4$item

# Ver los parámetros de un ítem específico, por ejemplo P1, P13, P25 y P37
params1[params1$item == "P1", ]
params2[params2$item == "P13", ]
params3[params3$item == "P25", ]
params4[params4$item == "P37", ]

# Revisar todos los parámetros
head(params1)

# Extraer y organizar los parámetros de umbrales
umbrales1 <- params1[, c("item", "AXsi_.Cat1", "AXsi_.Cat2", "AXsi_.Cat3", "AXsi_.Cat4")]
umbrales2 <- params2[, c("item", "AXsi_.Cat1", "AXsi_.Cat2", "AXsi_.Cat3", "AXsi_.Cat4")]
umbrales3 <- params3[, c("item", "AXsi_.Cat1", "AXsi_.Cat2", "AXsi_.Cat3", "AXsi_.Cat4")]
umbrales4 <- params4[, c("item", "AXsi_.Cat1", "AXsi_.Cat2", "AXsi_.Cat3", "AXsi_.Cat4")]

# Agregar nombres claros a las columnas
colnames(umbrales1) <- c("Item", "Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4")
colnames(umbrales2) <- c("Item", "Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4")
colnames(umbrales3) <- c("Item", "Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4")
colnames(umbrales4) <- c("Item", "Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4")

# Revisar los primeros ítems y detectar problemas
head(umbrales1)
head(umbrales2)
head(umbrales3)
head(umbrales4)

# Verificar si los umbrales están ordenados para cada ítem
umbrales1$Desorden <- with(umbrales1, Umbral_Cat1 > Umbral_Cat2 | Umbral_Cat2 > Umbral_Cat3 | Umbral_Cat3 > Umbral_Cat4)
umbrales2$Desorden <- with(umbrales2, Umbral_Cat1 > Umbral_Cat2 | Umbral_Cat2 > Umbral_Cat3 | Umbral_Cat3 > Umbral_Cat4)
umbrales3$Desorden <- with(umbrales3, Umbral_Cat1 > Umbral_Cat2 | Umbral_Cat2 > Umbral_Cat3 | Umbral_Cat3 > Umbral_Cat4)
umbrales4$Desorden <- with(umbrales4, Umbral_Cat1 > Umbral_Cat2 | Umbral_Cat2 > Umbral_Cat3 | Umbral_Cat3 > Umbral_Cat4)

# Ver los ítems con problemas de orden en los umbrales
umbrales1[umbrales1$Desorden == TRUE, ]
umbrales2[umbrales2$Desorden == TRUE, ]
umbrales3[umbrales3$Desorden == TRUE, ]
umbrales4[umbrales4$Desorden == TRUE, ]

##Visualizar patrones

library(ggplot2)

# Convertir los datos a formato largo para graficar: Componente 1
umbrales_long <- reshape2::melt(umbrales1, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Componente 1
ggplot(umbrales_long, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")

library(ggplot2)

# Convertir los datos a formato largo para graficar: Componente 2
umbrales_long2 <- reshape2::melt(umbrales2, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Componente 2
ggplot(umbrales_long2, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")


library(ggplot2)

# Convertir los datos a formato largo para graficar: Componente 3
umbrales_long3 <- reshape2::melt(umbrales3, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                 variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Componente 3
ggplot(umbrales_long3, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")

library(ggplot2)

# Convertir los datos a formato largo para graficar: Componente 4
umbrales_long4 <- reshape2::melt(umbrales4, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                 variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Componente 4
ggplot(umbrales_long4, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")

# Graficamos los ICC para los ítems de cada dimensión
#---------------------------------------------------------

items.to.plot<-c(1:3)
plot(modelo_d1, items = items.to.plot, type = "items")

items.to.plot<-c(1:10)
plot(modelo_d2, items = items.to.plot, type = "items")
title("ICC para el ítem P1")

items.to.plot<-c(1:10)
plot(modelo_d3, items = items.to.plot, type = "items")
title("ICC para el ítem P1")

items.to.plot<-c(1:10)
plot(modelo_d4, items = items.to.plot, type = "items")
title("ICC para el ítem P1")

##Calculamos los datos por dimensión con las etiquetas del Likert
#-------------------------------------------------------------------

# Calcular promedios por dimensión
promedio_d1 <- rowMeans(d1_items, na.rm = TRUE)
promedio_d2 <- rowMeans(d2_items, na.rm = TRUE)
promedio_d3 <- rowMeans(d3_items, na.rm = TRUE)
promedio_d4 <- rowMeans(d4_items, na.rm = TRUE)

# Crear un data frame con los promedios
promedios_dimensiones <- data.frame(
  Dim1 = promedio_d1,
  Dim2 = promedio_d2,
  Dim3 = promedio_d3,
  Dim4 = promedio_d4
)

# Ver los primeros valores
head(promedios_dimensiones)

# Promedio general por dimensión
promedios_globales <- colMeans(promedios_dimensiones, na.rm = TRUE)
promedios_globales

library(ggplot2)

# Crear un data frame para graficar
df_promedios <- data.frame(
  Dimension = c("Dim1", "Dim2", "Dim3", "Dim4"),
  Promedio = promedios_globales
)

# Gráfico de barras
ggplot(df_promedios, aes(x = Dimension, y = Promedio, fill = Dimension)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Promedios por Dimensión", x = "Dimensión", y = "Promedio") +
  theme(legend.position = "none")

##Calculamos los promedios a partir del modelo de Rasch
#---------------------------------------------------------

# Extraer las habilidades latentes (theta scores) del modelo Rasch para cada dimensión
habilidades_d1 <- modelo_d1$person$EAP
habilidades_d2 <- modelo_d2$person$EAP
habilidades_d3 <- modelo_d3$person$EAP
habilidades_d4 <- modelo_d4$person$EAP

# Crear un data frame con las habilidades por dimensión
habilidades_dimensiones <- data.frame(
  Dim1 = habilidades_d1,
  Dim2 = habilidades_d2,
  Dim3 = habilidades_d3,
  Dim4 = habilidades_d4
)

# Ver los primeros valores
head(habilidades_dimensiones)


# Calcular promedios por dimensión basados en habilidades latentes
promedio_habilidades_d1 <- mean(habilidades_d1, na.rm = TRUE)
promedio_habilidades_d2 <- mean(habilidades_d2, na.rm = TRUE)
promedio_habilidades_d3 <- mean(habilidades_d3, na.rm = TRUE)
promedio_habilidades_d4 <- mean(habilidades_d4, na.rm = TRUE)

# Crear un vector con los promedios por dimensión
promedios_habilidades <- c(promedio_habilidades_d1, promedio_habilidades_d2, promedio_habilidades_d3, promedio_habilidades_d4)
names(promedios_habilidades) <- c("Dim1", "Dim2", "Dim3", "Dim4")

# Ver los promedios
promedios_habilidades

# Crear un data frame para graficar
df_habilidades <- data.frame(
  Dimension = names(promedios_habilidades),
  Promedio = promedios_habilidades
)

# Gráfico de barras con promedios basados en habilidades latentes
library(ggplot2)
ggplot(df_habilidades, aes(x = Dimension, y = Promedio, fill = Dimension)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Promedios por Dimensión según Rasch", x = "Dimensión", y = "Promedio de Habilidad") +
  theme(legend.position = "none")

# Encontrar el mínimo y máximo de habilidades
min_habilidad <- min(habilidades_dimensiones, na.rm = TRUE)
max_habilidad <- max(habilidades_dimensiones, na.rm = TRUE)

# Transformar las habilidades a la escala 0-4
habilidades_transformadas <- 0 + (habilidades_dimensiones - min_habilidad) / (max_habilidad - min_habilidad) * 4

# Ver los resultados transformados
head(habilidades_transformadas)

# Calcular promedios por dimensión
promedio_dim1 <- mean(habilidades_transformadas$Dim1, na.rm = TRUE)
promedio_dim2 <- mean(habilidades_transformadas$Dim2, na.rm = TRUE)
promedio_dim3 <- mean(habilidades_transformadas$Dim3, na.rm = TRUE)
promedio_dim4 <- mean(habilidades_transformadas$Dim4, na.rm = TRUE)

# Crear un vector con los promedios
promedios_transformados <- c(Dim1 = promedio_dim1, 
                             Dim2 = promedio_dim2, 
                             Dim3 = promedio_dim3, 
                             Dim4 = promedio_dim4)

# Ver los resultados
print(promedios_transformados)


#Verificamoss la estimación
#---------------------------------

habilidades_wle <- TAM::tam.wle(modelo_d1)

# Extraer los errores estándar
errores_estandar <- habilidades_wle$error

# Verificar los primeros valores
head(errores_estandar)

# Calcular el promedio del error estándar
mean(errores_estandar, na.rm = TRUE)

IC_95_lower <- habilidades_wle$theta - 1.96 * errores_estandar
IC_95_upper <- habilidades_wle$theta + 1.96 * errores_estandar

# Crear un data frame con los resultados
IC_df <- data.frame(
  Habilidad = habilidades_wle$theta,
  IC_Lower = IC_95_lower,
  IC_Upper = IC_95_upper
)

# Visualizar los primeros resultados
head(IC_df)

# Crear histograma de habilidades estimadas
library(ggplot2)
ggplot(IC_df, aes(x = Habilidad)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribución de Habilidades Estimadas",
       x = "Habilidad Estimada (theta)",
       y = "Frecuencia")

#Verificación de los Mapas de Personas e Ítems (Wright Map)
library(WrightMap)
wrightMap(habilidades_wle$theta, modelo_d1$item$b)

#Evaluación de la Fiabilidad del Instrumento
# Calcular la varianza de las habilidades estimadas
var_theta <- var(habilidades_wle$theta, na.rm = TRUE)

# Calcular la varianza de los errores estándar
var_error <- mean(habilidades_wle$error^2, na.rm = TRUE)

# Calcular la fiabilidad WLE
fiabilidad_wle <- 1 - (var_error / var_theta)

# Mostrar el resultado
print(paste("Fiabilidad WLE:", round(fiabilidad_wle, 3)))

#CORRELACIONES 
habilidades_dim1 <- TAM::tam.wle(modelo_d1)$theta
habilidades_dim2 <- TAM::tam.wle(modelo_d2)$theta
habilidades_dim3 <- TAM::tam.wle(modelo_d3)$theta
habilidades_dim4 <- TAM::tam.wle(modelo_d4)$theta

habilidades_totales <- data.frame(
  Dim1 = habilidades_dim1,
  Dim2 = habilidades_dim2,
  Dim3 = habilidades_dim3,
  Dim4 = habilidades_dim4
)

cor(habilidades_totales)


#Verificamos Unidimensionalidad
#---------------------------------

# Ajustar el modelo
modelo_d1 <- TAM::tam.mml(d1_items)

# Extraer probabilidades esperadas del modelo
expected <- modelo_d1$EAP.rel # Probabilidades esperadas para cada respuesta

# Obtener respuestas observadas
observed <- d1_items

# Calcular residuos manualmente (observado - esperado)
residuos <- observed - expected

# Verificar los residuos
head(residuos)

# Realizar PCA
#---------------
pca_residuos <- prcomp(residuos, scale. = TRUE)

# Resumen de PCA
summary(pca_residuos)

# Ver los eigenvalues
eigenvalues <- pca_residuos$sdev^2
print(eigenvalues)

# Graficar el Scree Plot
plot(eigenvalues, type = "b", main = "Scree Plot de los residuos",
     xlab = "Componentes Principales", ylab = "Eigenvalues")




#########FIN