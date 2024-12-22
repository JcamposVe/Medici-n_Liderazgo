#MODELO DE ESCALA DE CALIFICACIÓN RASCH
#=======================================

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


# Agrupar los ítems por dimensión
# Instalar eRm si no está instalado
if (!requireNamespace("eRm", quietly = TRUE)) {
  install.packages("eRm")
}

# Cargar el paquete
library(eRm)
d1_items <- base_datos[, c("P1", "P2", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12")]
d2_items <- base_datos[, c("P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")]
d3_items <- base_datos[, c("P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35", "P36")]
d4_items <- base_datos[, c("P37", "P38", "P39", "P40", "P41", "P42", "P43", "P44", "P45", "P46", "P47", "P48")]

##ANÁLISIS COMPONENTE 1
##------------------------

# Ajustar el modelo Rasch de escala de calificación para la Dimensión 1
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

table(base_datos$P1) # Para P1


##ANÁLISIS COMPONENTE 2
##------------------------

# Ajustar el modelo Rasch de escala de calificación para la Dimensión 1
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

##ANÁLISIS COMPONENTE 3
##------------------------

# Ajustar el modelo Rasch de escala de calificación para la Dimensión 1
modelo_d3 <- TAM::tam.mml(d3_items)

# Resumen del modelo ajustado
summary(modelo_d3)

# Verificar los índices de ajuste
summary(modelo_d3)

# Ver el ajuste específico por ítem
tam.fit(modelo_d3)

table(base_datos$P30) # Reemplaza P1 por cualquier ítem

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

# Ver frecuencias de un ítem específico (por ejemplo, P1)
table(base_datos$P27)

##ANÁLISIS COMPONENTE 4
##------------------------

# Ajustar el modelo Rasch de escala de calificación para la Dimensión 1
modelo_d4 <- TAM::tam.mml(d4_items)

# Resumen del modelo ajustado
summary(modelo_d4)

# Verificar los índices de ajuste
summary(modelo_d4)

# Ver el ajuste específico por ítem
tam.fit(modelo_d4)

table(base_datos$P37) # Reemplaza P1 por cualquier ítem

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
# Extraer parámetros de los ítems
params1 <- modelo_d1$item
params2 <- modelo_d2$item
params3 <- modelo_d3$item
params4 <- modelo_d4$item

# Ver los parámetros de un ítem específico, por ejemplo, P1
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

# Convertir los datos a formato largo para graficar: Dimensión 1
umbrales_long <- reshape2::melt(umbrales1, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Dimensión 1
ggplot(umbrales_long, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")

library(ggplot2)

# Convertir los datos a formato largo para graficar: Dimensión 2
umbrales_long2 <- reshape2::melt(umbrales2, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Dimensión 2
ggplot(umbrales_long2, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")


library(ggplot2)

# Convertir los datos a formato largo para graficar: Dimensión 3
umbrales_long3 <- reshape2::melt(umbrales3, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                 variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Dimensión 3
ggplot(umbrales_long3, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")

library(ggplot2)

# Convertir los datos a formato largo para graficar: Dimensión 4
umbrales_long4 <- reshape2::melt(umbrales4, id.vars = "Item", measure.vars = c("Umbral_Cat1", "Umbral_Cat2", "Umbral_Cat3", "Umbral_Cat4"),
                                 variable.name = "Categoria", value.name = "Umbral")

# Graficar los umbrales por ítem: Dimensión 4
ggplot(umbrales_long4, aes(x = Categoria, y = Umbral, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Umbrales de los ítems", x = "Categoría", y = "Umbral") +
  theme(legend.position = "none")

########################## NO CORRER HASTA CORREGIR ##############################################
# Graficar la ICC para un ítem específico (por ejemplo, el primer ítem de la Dimensión 1)
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

# Crear un bucle para graficar todas las ICC de la Dimensión 1
for (i in 1:ncol(d1_items)) {
  plot(modelo_d1, items = i, type = "ICC", main = paste("ICC para el ítem", colnames(d1_items)[i]))
}

# Graficar todas las ICC juntas para la Dimensión 1
plot(modelo_d1, type = "ICC", main = "Curvas ICC para la Dimensión 1")

# Crear gráficos ICC y guardarlos en una carpeta
output_dir <- "ICC_Graficos" # Carpeta donde se guardarán las imágenes
dir.create(output_dir, showWarnings = FALSE)

for (i in 1:ncol(d1_items)) {
  png(filename = paste0(output_dir, "/ICC_", colnames(d1_items)[i], ".png"))
  plot(modelo_d1, items = i, type = "ICC", main = paste("ICC para el ítem", colnames(d1_items)[i]))
  dev.off()
}

####################################

##Calculamos los datos por dimensión

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


#######################

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


#### Verificamoss la estimación

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

##################################
#Verificamos Unidimensionalidad

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





# Extraer habilidades de los participantes
habilidades <- modelo_d1$person$EAP

# Obtener las habilidades de los participantes
habilidades <- modelo_d1$person$EAP

# Calcular el promedio por dimensión usando los ítems de cada dimensión
promedio_dimensiones_rasch <- data.frame(
  Dim1 = rowMeans(d1_items, na.rm = TRUE),
  Dim2 = rowMeans(d2_items, na.rm = TRUE),
  Dim3 = rowMeans(d3_items, na.rm = TRUE),
  Dim4 = rowMeans(d4_items, na.rm = TRUE)
)

# Verificar los primeros resultados
head(promedio_dimensiones_rasch)

# Obtener las habilidades de los participantes
habilidades <- modelo_d1$person$EAP

# Calcular el promedio por dimensión usando los ítems de cada dimensión
promedio_dimensiones_rasch <- data.frame(
  Dim1 = rowMeans(d1_items, na.rm = TRUE),
  Dim2 = rowMeans(d2_items, na.rm = TRUE),
  Dim3 = rowMeans(d3_items, na.rm = TRUE),
  Dim4 = rowMeans(d4_items, na.rm = TRUE)
)

# Verificar los primeros resultados
head(promedio_dimensiones_rasch)

# Dividir por dimensiones y calcular promedios
promedios_rasch <- data.frame(
  Dim1 = habilidades[1:nrow(d1_items)],
  Dim2 = habilidades[(nrow(d1_items) + 1):(nrow(d1_items) + nrow(d2_items))],
  Dim3 = habilidades[(nrow(d1_items) + nrow(d2_items) + 1):(nrow(d1_items) + nrow(d2_items) + nrow(d3_items))],
  Dim4 = habilidades[(nrow(d1_items) + nrow(d2_items) + nrow(d3_items) + 1):nrow(base_datos)]
)

# Ver los primeros valores transformados
head(promedios_rasch)

# Calcular promedio general por dimensión
promedios_globales_rasch <- colMeans(promedio_dimensiones_rasch, na.rm = TRUE)
promedios_globales_rasch

library(ggplot2)

# Crear un data frame con los promedios generales
df_promedios_globales <- data.frame(
  Dimension = names(promedios_globales_rasch),
  Promedio = promedios_globales_rasch
)

# Crear el gráfico
ggplot(df_promedios_globales, aes(x = Dimension, y = Promedio, fill = Dimension)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Promedios de habilidades por dimensión",
       x = "Dimensión",
       y = "Promedio de habilidades (\u03B8)") +
  theme(legend.position = "none")

#Código corregido para promedios
# Extraer las habilidades estimadas (EAP) para los participantes
habilidades <- modelo_d1$person$EAP

# Asociar las habilidades con las dimensiones
promedios_rasch_dimensiones <- data.frame(
  Dim1 = rowMeans(d1_items * habilidades, na.rm = TRUE),
  Dim2 = rowMeans(d2_items * habilidades, na.rm = TRUE),
  Dim3 = rowMeans(d3_items * habilidades, na.rm = TRUE),
  Dim4 = rowMeans(d4_items * habilidades, na.rm = TRUE)
)

# Calcular promedios globales por dimensión
promedios_globales_rasch <- colMeans(promedios_rasch_dimensiones, na.rm = TRUE)
promedios_globales_rasch

library(TAM)

# Ajustar el modelo de Rasch si no lo tienes ya ajustado
modelo_dim1 <- TAM::tam.mml(d1_items)

# Graficar las ICC para un ítem específico (por ejemplo, el primer ítem de la dimensión 1)
plot(modelo_dim1, items = 1, type = "ICC")

# Crear un bucle para graficar cada ítem de la Dimensión 1
for (i in 1:ncol(d1_items)) {
  plot(modelo_dim1, items = i, type = "ICC", main = paste("ICC para el ítem", colnames(d1_items)[i]))
}

getwd()

setwd("/Users/juancampos/Desktop/Juan Campos /MINEDU/10_Análisis_VAL-ED/10.1.Base")

# Crear la carpeta "Plots" si no existe
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

# Guardar los gráficos en formato PNG
for (i in 1:ncol(d1_items)) {
  png(filename = paste0("Plots/ICC_Item_", colnames(d1_items)[i], ".png"), width = 800, height = 600)
  plot(modelo_dim1, items = i, type = "ICC", main = paste("ICC para el ítem", colnames(d1_items)[i]))
  dev.off() # Cerramos el dispositivo gráfico
}



for (i in 1:ncol(d1_items)) {
  plot(modelo_dim1, items = i, type = "ICC", main = paste("ICC para el ítem", colnames(d1_items)[i]))
}

dir.create("Plots", showWarnings = FALSE) # Crea la carpeta si no existe
setwd("/Users/juancampos/Desktop/Juan Campos /MINEDU/10_Análisis_VAL-ED/10.1.Base")


for (i in 1:ncol(d1_items)) {
  png(filename = paste0("Plots/ICC_Item_", colnames(d1_items)[i], ".png"), width = 800, height = 600)
  plot(modelo_dim1, items = i, type = "ICC", main = paste("ICC para el ítem", colnames(d1_items)[i]))
  dev.off() # Cierra el dispositivo gráfico
}

for (i in 1:ncol(d1_items)) {
  plot(modelo_dim1, items = i, type = "ICC", main = paste("ICC para el ítem", colnames(d1_items)[i]))
}




















# Estimate and inspect the item parameters
library(TAM)
myTAM <- tam.mml(base_datos, 
                 irtmodel = "RSM")

myTAM$item_irt

# Plot the option characteristic curves
plot(myTAM, 
     type = "items", 
     export = FALSE, 
     package = "graphics", 
     observed = TRUE, 
     low = -3, 
     high = 3)


plot(myTAM, type = "summary")

str(myTAM)




install.packages("ggplot2")

class(base_datos)
str(base_datos)


library(ggplot2)

# Datos simulados con tu tabla
ggplot(data = base_datos, aes(x = beta)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribución de la dificultad de los ítems (beta)",
       x = "Dificultad (beta)",
       y = "Frecuencia")





# Cargar los paquetes
library(psych)
library(dplyr)
# Filtrar registros donde cargo es "Director/a"
base_directores <- base_datos %>%
  filter(cargo == "Director/a") %>%
  select(P1:P48)  # Ajusta el rango según las columnas de los ítems

# Definir el mapeo de texto a valores numéricos
conversion <- c("Ineficaz" = 1, 
                "Poco eficaz" = 2, 
                "Eficaz" = 3, 
                "Bastante eficaz" = 4, 
                "Muy eficaz" = 5)

# Aplicar la conversión a todas las columnas P1 a P48
base_datos <- base_datos %>%
  mutate(across(P1:P48, ~ conversion[.], .names = "converted_{col}"))

# Verificar el resultado
head(base_datos[, c("P1", "converted_P1")])  # Ejemplo para la columna P1

# Reemplazar directamente las columnas originales
base_datos <- base_datos %>%
  mutate(across(P1:P48, ~ conversion[.]))

# Eliminar todas las columnas cuyo nombre comienza con "converted"
base_datos <- base_datos %>%
  select(-starts_with("converted"))

# Verificar que las columnas fueron eliminadas
colnames(base_datos)





base_directores <- base_directores[, valid_items]


cor_matrix <- cor(base_directores)
high_cor <- which(abs(cor_matrix) > 0.95, arr.ind = TRUE)
high_cor <- high_cor[high_cor[, 1] != high_cor[, 2], ]
high_cor

apply(base_directores, 2, var)
base_directores <- base_directores[, apply(base_directores, 2, var) > 0]

#Ejecutamos modelo de escala de calificación
rs_model <- TAM::tam.mml(base_directores,irtmodel="RSM")
class(rs_model)
str(rs_model)

abilities <- rs_model$person$EAP

item_difficulties <- rs_model$item_irt$beta

library(WrightMap)
wrightMap(abilities, item_difficulties, show.thr.lab = TRUE)


# Plot the Variable Map
library(WrightMap)
graphics.off() # In case you can not run the plot correctly
wrightMap(rs_model,show.thr.lab=TRUE)

plot(rs_model,ask=FALSE)

#Curvas características de los artículos (pero ahora como umbrales)
graphics.off()
plot(rs_model,type = "items")

# We can use the similar code to achieve the item estimate as what we did for the Dichotomous Analysis
graphics.off()
rs_model$xsi # The first column is the item difficulty. In this case, is the rater's rating severity.

install.packages("TAM")
library(TAM)


rater_estimates <- rs_model$xsi
tam.fit(rs_model) 

# Note the last two rows also provides you the average fit statistics for category 1 and category 2. For this analysis, we are not focus on these data.
# We can also check the Rating Scale Thresholds
rs_threshold <- tam.threshold(rs_model)
rs_threshold # This provides the detail logit location for each categories for each rater.

# Use the tam.wle function to acheive the person ability
person_ability <- tam.wle(rs_model)

# Print out the person ability
head(person_ability$theta)# Person's fit statistics

rs_personfit <- tam.personfit(rs_model)
# Check the first 6 directores' person fit statistics
head(rs_personfit)


#OTRO MODELO
#================

ls()
install.packages("package_name")  # Replace with the actual package name
library(package_name)


### Load the example data:
data("base_directores")
# These data include 20 participants’ responses to six items that included four ordered categories (0, 1, 2, and 3).
summary(base_directores)

install.packages("eRm")
library(eRm)

apply(base_directores, 2, function(x) length(unique(x)))

base_directores <- as.data.frame(lapply(base_directores, function(x) x - 1))
summary(base_directores)

unique(base_directores$P3)  # Replace P1 with other items as needed

valid_items <- apply(base_directores, 2, function(x) length(unique(x)) == 5)
base_directores <- base_directores[, valid_items]


cor_matrix <- cor(base_directores)
high_cor <- which(abs(cor_matrix) > 0.95, arr.ind = TRUE)
high_cor <- high_cor[high_cor[, 1] != high_cor[, 2], ]
high_cor

apply(base_directores, 2, var)
base_directores <- base_directores[, apply(base_directores, 2, var) > 0]

apply(base_directores, 2, table)

low_var_items <- apply(base_directores, 2, function(x) {
  table(x)
}) %>% lapply(function(t) min(t)) %>% unlist()
names(low_var_items[low_var_items < 10])




library(eRm)
rs_model2 <- RSM(base_directores)
rs_model2 <- RSM(base_directores)

### Examine item difficulty values:
item.estimates <- thresholds(rs_model)
item.estimates



# Crear una lista con las frecuencias
frecuencias <- apply(base_directores, 2, table)

# Convertir la lista en un DataFrame
tabla_frecuencias <- do.call(cbind, lapply(frecuencias, as.data.frame))

# Renombrar las columnas para incluir los ítems
colnames(tabla_frecuencias) <- names(frecuencias)
