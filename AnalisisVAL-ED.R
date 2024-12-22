#########################################################
##            Análisis de la Encuesta VAL-ED           ##
## ----------------------------------------------------##                                                                  ##
#########################################################

#Instalamos los paquetes necesarios
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("polyco", repos="http://R-Forge.R-pproject.org")
install.packages("MVN")
install.packages("ggcorrplot")

library(readxl)
library(dplyr)
library(ggplot2)

#IMPORTAMOS LA BASE DE DATOS
#================================

# Definir la ruta del archivo
archivo_excel <- ""
#Correr para Mac:
#setwd("/Users/juancampos/Desktop/Juan Campos /MINEDU/10_Análisis_VAL-ED/10.1.Base")
#Correr para Windows
setwd("C:\Users\jcamp\iCloudDrive\Desktop\Juan Campos_\MINEDU\10_Análisis_VAL-ED\10.1.Base")

# Importar la hoja "bd"
base_datos <- readxl::read_excel("Asistencia Técnica 2024 - Instrumento autoevaluación.xlsx")
base_datos <- as.data.frame(base_datos)

# Verificar la base de datos
head(base_datos)
str(base_datos)
colnames(base_datos)

#ADECUAMOS BASE DE DATOS PARA EL DIAGNÓSTICO
#===============================================

#Sacamos algunos datos estadísticos para el diagnóstico de la encuesta
# Cargar el paquete
library(dplyr)
# Calcular frecuencias de la variable "cargo"
tabla_cargo <- base_datos %>%
  count(cargo, name = "Cantidad")

# Mostrar la tabla resultante
tabla_cargo

print(tabla_cargo)

# Determinamos la cantidad de IIEE
valores_distintos <- base_datos %>%
  summarise(ValoresUnicos = n_distinct(`Código de local`))

# Ver los resultados
valores_distintos

#Generamos un histograma para las variables relacionadas con experiencia
library(ggplot2)

# Crear un histograma para "año_exp_IE_actual"
ggplot(base_datos, aes(x = año_exp_IE_actual)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5, color = "black") + # Histograma
  stat_function(fun = dnorm, args = list(mean = mean(base_datos$año_exp_IE_actual, na.rm = TRUE), 
                                         sd = sd(base_datos$año_exp_IE_actual, na.rm = TRUE)), 
                color = "red", size = 1) + # Línea de distribución normal
  geom_vline(xintercept = quantile(base_datos$año_exp_IE_actual, probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 
             linetype = "dashed", color = "darkgreen", size = 1) + # Líneas de los cuartiles
  labs(title = "Histograma de años de experiencia en la IE actual con distribución normal y cuartiles",
       x = "Años de experiencia (actual)", 
       y = "Densidad") +
  theme_minimal()

#Revisamos los cuartiles de la variable año de experiencia

library(dplyr)

# Calcular cuartiles a nivel total
cuartiles_totales <- base_datos %>%
  summarise(
    Q1 = quantile(año_exp_IE_actual, 0.25, na.rm = TRUE),
    Mediana = quantile(año_exp_IE_actual, 0.5, na.rm = TRUE),
    Q3 = quantile(año_exp_IE_actual, 0.75, na.rm = TRUE)
  )

# Ver los cuartiles totales
cuartiles_totales

# Instalar dplyr (si no está instalado)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Cargar dplyr
library(dplyr)

# Calcular cuartiles por cargo
cuartiles_por_cargo <- base_datos %>%
  group_by(cargo) %>%
  summarise(
    Q1 = quantile(año_exp_IE_actual, 0.25, na.rm = TRUE),
    Mediana = quantile(año_exp_IE_actual, 0.5, na.rm = TRUE),
    Q3 = quantile(año_exp_IE_actual, 0.75, na.rm = TRUE),
    Total_Observaciones = n()
  )

# Ver los cuartiles por cargo
cuartiles_por_cargo


# Instalar y cargar ggplot2 (si no está instalado)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Calcular los datos de frecuencia para la variable "Sexo"
frecuencias_sexo <- base_datos %>%
  count(Sexo) %>%
  mutate(Porcentaje = n / sum(n) * 100)  # Calcular porcentajes

# Crear el gráfico de torta
ggplot(frecuencias_sexo, aes(x = "", y = Porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +  # Transformar en gráfico de torta
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold") +  # Etiquetas en blanco y negrita
  scale_fill_manual(values = c("Mujer" = "#D1495B", "Hombre" = "#2E294E")) +  # Colores personalizados
  theme_void() +  # Eliminar fondo y ejes
  theme(legend.position = "bottom") +  # Posicionar la leyenda abajo
  labs(title = "Distribución por Sexo",
       fill = "Sexo") +
  guides(fill = guide_legend(title = "Sexo"))  # Título de la leyenda

#ADECUAMOS LAS VARIABLES DE LA ENCUESTA PARA EL CONSTRUCTO
#============================================================

#Volvemos valores numéricos a las variables de la encuesta relacionadas con el constructo:

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

#REALIZAMOS UN ANÁLISIS DE CORRELACIÓN PARA LOS COMPONENTES BÁSICOS
#===================================================================

# Instalar dplyr (si no está instalado)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Cargar dplyr
library(dplyr)

#Correlación para items del COMPONENTE 1
#==========================================

# Seleccionar los ítems (sin usar `%>%`)
componente1 <- dplyr::select(base_datos, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)

# Calcular la matriz de correlación
matriz_correlacion1 <- cor(componente1, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion1, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#30638E"))(200))


#Correlación para items del COMPONENTE 2
#==========================================

# Seleccionar los ítems (sin usar `%>%`)
componente2 <- dplyr::select(base_datos, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22, P23, P24)

# Calcular la matriz de correlación
matriz_correlacion2 <- cor(componente2, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion2, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#30638E"))(200))

#Correlación para items del COMPONENTE 3
#==========================================

# Seleccionar los ítems (sin usar `%>%`)
componente3 <- dplyr::select(base_datos, P25, P26, P27, P28, P29, P30, P31, P32, P33, P34, P35, P36)

# Calcular la matriz de correlación
matriz_correlacion3 <- cor(componente3, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion3, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#30638E"))(200))

#Correlación para items del COMPONENTE 4
#==========================================

# Seleccionar los ítems (sin usar `%>%`)
componente4 <- dplyr::select(base_datos, P37, P38, P39, P40, P41, P42, P43, P44, P45, P46, P47, P48)

# Calcular la matriz de correlación
matriz_correlacion4 <- cor(componente4, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion4, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#30638E"))(200))

#Correlación para items del Proceso Clave Planificación
#========================================================

# Seleccionar los ítems (sin usar `%>%`)
Planificacion <- dplyr::select(base_datos, P1, P2, P13, P14, P25, P26, P37, P38)

# Calcular la matriz de correlación
matriz_correlacion5 <- cor(Planificacion, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion5, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#C17767"))(200))


#Correlación para items del Proceso Clave Desarrollo
#========================================================

# Seleccionar los ítems (sin usar `%>%`)
Desarrollo <- dplyr::select(base_datos, P3, P4, P15, P16, P27, P28, P39, P40)

# Calcular la matriz de correlación
matriz_correlacion6 <- cor(Desarrollo, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion6, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#C17767"))(200))

#Correlación para items del Proceso Clave Apoyo
#========================================================

# Seleccionar los ítems (sin usar `%>%`)
Apoyo <- dplyr::select(base_datos, P5, P6, P17, P18, P29, P30, P41, P42)

# Calcular la matriz de correlación
matriz_correlacion7 <- cor(Apoyo, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion7, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#C17767"))(200))

#Correlación para items del Proceso Clave Inclusión
#=====================================================

# Seleccionar los ítems (sin usar `%>%`)
Inclusión <- dplyr::select(base_datos, P7, P8, P19, P20, P31, P32, P43, P44)

# Calcular la matriz de correlación
matriz_correlacion8 <- cor(Inclusión, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion8, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#C17767"))(200))

#Correlación para items del Proceso Clave Comunicación
#=======================================================

# Seleccionar los ítems (sin usar `%>%`)
Comunicación <- dplyr::select(base_datos, P9, P10, P21, P22, P33, P34, P45, P46)

# Calcular la matriz de correlación
matriz_correlacion9 <- cor(Comunicación, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion9, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#C17767"))(200))


#Correlación para items del Proceso Clave Seguimiento
#=======================================================

# Seleccionar los ítems (sin usar `%>%`)
Seguimiento <- dplyr::select(base_datos, P11, P12, P23, P24, P35, P36, P47, P48)

# Calcular la matriz de correlación
matriz_correlacion10 <- cor(Seguimiento, use = "complete.obs")

# Instalar corrplot (si no está instalado)

# Cargar corrplot
library(corrplot)

# Crear el gráfico de correlación
corrplot(matriz_correlacion10, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "white", number.cex = 0.7,
         col = colorRampPalette(c("#DCE2C8", "white", "#C17767"))(200))


#CALCULAMOS LOS NIVELES DE ALPHA DE CRONBAUSH
#=============================================

#Para el caso de Directores
#============================

# Instalar paquetes si no están instalados
if (!requireNamespace("psych", quietly = TRUE)) {
  install.packages("psych")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Cargar los paquetes
library(psych)
library(dplyr)

# Filtrar registros donde cargo es "Director/a"
base_directores <- base_datos %>%
  filter(cargo == "Director/a") %>%
  select(P1:P48)  # Ajusta el rango según las columnas de los ítems

# Calcular el Alpha de Cronbach
resultado_alpha <- alpha(base_directores)

# Ver los resultados
print(resultado_alpha)

# Calcular el Alpha de Cronbach para el componente 1
resultado_alpha_Direc_comp1 <- alpha(componente1)

#REALIZAMOS UN ANÁLISIS FACTORIAL EXPLORATORIO PARA BASE DE DIRECTORES
#==========================================================================

## I. Pruebas para verificar aplicación de Análisis Factorial
###------------------------------------------------------------

# 1.1.Prueba de Esfericidad de Bartlett 

#Ho: Las variables son no correlacionadas 
#Ha: Las variables son correlacionadas

cortest.bartlett( cor( base_directores ) ,
                  n = dim(base_directores)[1])    #p.value= 0 (ambos)

# Conclusión de Prueba de Esfericidad de Bartlett: 
# Se rechaza porque el Pvalue es un valor muy pequeño. 
# Rechazamos Ho
# Existe evidencia suficiente para afirmar Ha
# Existe evidencia suficiente para afirmar que las variables son correlacionadas

# 1.2. Coeficiente KMO 

install.packages("rela")
library(rela)
descri = paf( as.matrix(base_directores) )
descri$KMO  #KMO=0.98, se puede realizar análisis factorial

## II. Extracción de factores
##----------------------------

## Selección del Número de Factores

scree(base_directores)

###Máxima verosimilitud

scree(base_directores, 
      factors=TRUE, pc=FALSE, 
      main="Gráfico de sedimentación", hline=1)

###Componentes principales

scree(base_directores, 
      factors=FALSE, pc=TRUE, 
      main="Gráfico de sedimentación", hline=1)

###Método de extracción de factores: Análisis Paralelo
# Calcular la matriz de correlación
mat_cor <- cor(base_directores, use = "complete.obs")
fa.parallel(mat_cor,n.obs=1670,fa="fa",fm="wls", main="Gráfico de sedimentación")

#### III. Método de rotación
###----------------------------

#### Usando la función fa() (otro comando dif al factanal) ####

#### VARIMAX (Rotacion & Factores no correlacionados) : 4 Factores

library(GPArotation)
fa1 <- fa(base_directores, nfactors=4, rotate = 'varimax')
fa1
fa1$loadings[,1:4]

#Factoring method: wls (weighted least squares=mínimos cuadrados ponderados)

fa1 <- fa(base_directores, nfactors=4, rotate = 'varimax',fm='wls')
fa1
fa1$loadings[,1:4]

#Graficar:
plot(fa(base_directores, nfactors=4, rotate = 'varimax',fm='wls'))
fa.diagram(fa(base_directores, nfactors=4, rotate = 'varimax',fm='wls'))
biplot(fa(base_directores,nfactors=4, rotate = 'varimax',fm='wls'),labels=rownames(base_directores))

#Factoring method: minres (mínimos residuos)

fa2 <- fa(base_directores, nfactors=4, rotate = 'varimax',fm='minres')
fa2
fa2$loadings[,1:4]

#Graficar:
plot(fa(base_directores, nfactors=4, rotate = 'varimax',fm='minres'))
fa.diagram(fa(base_directores, nfactors=4, rotate = 'varimax',fm='minres'))
biplot(fa(base_directores,nfactors=4, rotate = 'varimax',fm='minres'),labels=rownames(base_directores))

#### OBLIMIN (Rotacion & Factores correlacionados) : 2 Factores

library(GPArotation)
fa2 <-fa(base_directores, nfactors=4, rotate = 'oblimin')
fa2
fa2$loadings[,1:4]

#Factoring method: wls (weighted least squares=mínimos cuadrados ponderados)

fa2 <-fa(base_directores, nfactors=4, rotate = 'oblimin',fm='wls')
fa2
fa2$loadings[,1:4]

#Graficar:
plot(fa(base_directores, nfactors=4, rotate = 'oblimin',fm='wls'))
fa.diagram(fa(base_directores, nfactors=4, rotate = 'oblimin',fm='wls'))
biplot(fa(base_directores,nfactors=4, rotate = 'oblimin',fm='wls'),labels=rownames(base_directores))

#Factoring method: minres (mínimos residuos)

fa2 <-fa(base_directores, nfactors=4, rotate = 'oblimin',fm='minres')
fa2
fa2$loadings[,1:4]

#Graficar:
plot(fa(base_directores, nfactors=4, rotate = 'oblimin',fm='minres'))
fa.diagram(fa(base_directores, nfactors=4, rotate = 'oblimin',fm='minres'))
biplot(fa(base_directores,nfactors=4, rotate = 'oblimin',fm='minres'),labels=rownames(base_directores))


#ANÁLISIS FACTORIAL CONFIRMATORIO
#====================================

install.packages("lavaan", dependencies = TRUE)
install.packages("semPlot")

library(foreign)
library(lavaan)
library(semPlot)
library(diagram)

corrplot::colorlegend()
shape::colorlegend()
library(diagram)
library(corrplot)

f3 <- ' factor1 =~P38 + P37 + P44 + P39 + P43 + P47 + P42 + P40 + P46 + P41 + P45 + P48 + P26 + P31 + P27 + P20 + P34 + P33 + P32 + P29 + P28 + P5 + P30 + P23
factor2 =~P14 + P25 + P8 +P22 + P7 + P19 + P13 + P10 + P16+ P15 + P17
factor3 =~P36 + P24 + P11 + P35 + P12
factor4 =~P9 + P2 + P3 + P1 + P6 + P4 + P18 + P21

Liderazgo=~1*factor1 + 1*factor2 + 1*factor3 + 1*factor4
Liderazgo~~Liderazgo'
AFC_f3 <- cfa(f3, data=base_directores, estimator="ML")
summary(AFC_f3, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_f3, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))

semPaths(AFC_f3, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="blue",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)


f4 <- ' factor5 =~P1 + P2 + P3 + P4 + P5 + P6+ P7 + P8 + P9 + P10 + P11 + P12 
factor6 =~P13 + P14 + P15 +P16 + P17 + P18 + P19 + P20 + P21+ P22 + P23 + P24
factor7 =~P25 + P26 + P27 + P28 + P29 + P30 + P31 + P32 + P33 + P34 + P35 + P36
factor8 =~P37 + P38 + P39 + P40 + P41 + P42 + P43 + P44 + P45 + P46+ P47 + P48

Liderazgo2=~1*factor5 + 1*factor6 + 1*factor7 + 1*factor8
Liderazgo2~~Liderazgo2'
AFC_f4 <- cfa(f4, data=base_directores, estimator="ML")
summary(AFC_f4, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_f4, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))

semPaths(AFC_f4, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="blue",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)

f5 <- ' proceso1 =~P1 + P2 + P13 + P14 + P25 + P26+ P37 + P38 
proceso2 =~P3 + P4 + P15 +P16 + P27 + P28 + P39 + P40 
proceso3 =~P5 + P6 + P17 + P18 + P29 + P30 + P41 + P42 
proceso4 =~P7 + P8 + P19 + P20 + P31 + P32 + P43 + P44 
proceso5 =~P9 + P10 + P21 + P22 + P33 + P34 + P45 + P46
proceso6 =~P11 + P12 + P23 + P24 + P35 + P36 + P47 + P48

Liderazgo3=~1*proceso1 + 1*proceso2 + 1*proceso3 + 1*proceso4 + 1*proceso5 + 1*proceso6
Liderazgo3~~Liderazgo3'
AFC_f5 <- cfa(f5, data=base_directores, estimator="ML")
summary(AFC_f5, fit.measures=TRUE, standardized=T, rsquare=T)
fitMeasures(AFC_f5, fit.measures = c("chisq", "df","srmr", "rmsea", "tli", "cfi"))

semPaths(AFC_f5, nCharNodes = 0,intercepts = FALSE, edge.label.cex=1.3, optimizeLatRes = T, groups = "lat",pastel = T, sizeInt=5,edge.color ="blue",esize = 5, label.prop=0,sizeLat = 11,"std",layout="circle3", exoVar = F)

#MODELO DE RASCH
#===================

# Instalar eRm si no está instalado
if (!requireNamespace("eRm", quietly = TRUE)) {
  install.packages("eRm")
}

# Cargar el paquete
library(eRm)

# Agrupar los ítems por factor
factor5_items <- base_directores[, c("P1", "P2", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12")]
factor6_items <- base_directores[, c("P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")]
factor7_items <- base_directores[, c("P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35", "P36")]
factor8_items <- base_directores[, c("P37", "P38", "P39", "P40", "P41", "P42", "P43", "P44", "P45", "P46", "P47", "P48")]

# Restar 1 a todas las respuestas en los ítems del factor 5
factor5_items_shifted <- factor5_items - 1
factor6_items_shifted <- factor6_items - 1
factor7_items_shifted <- factor7_items - 1
factor8_items_shifted <- factor8_items - 1

# Ajustar el modelo de créditos parciales
rasch_factor5 <- PCM(factor5_items_shifted)
rasch_factor6 <- PCM(factor6_items_shifted)
rasch_factor7 <- PCM(factor7_items_shifted)
rasch_factor8 <- PCM(factor8_items_shifted)

# Estadísticos del modelo para factor5
summary(rasch_factor5)

# Índice de Separación de Personas (ISP) para factor5
person.parameter(rasch_factor5)

# Repetir para los otros factores
summary(rasch_factor6)
summary(rasch_factor7)
summary(rasch_factor8)

# ISP para otros factores
person.parameter(rasch_factor6)
person.parameter(rasch_factor7)
person.parameter(rasch_factor8)

# Graficar curvas características para factor5
plotICC(rasch_factor5)

# Graficar habilidades de las personas para factor5
plotPImap(rasch_factor5)

# Repetir para los otros factores
plotICC(rasch_factor6)
plotICC(rasch_factor7)
plotICC(rasch_factor8)

# Graficar habilidades de las personas para factor6
plotPImap(rasch_factor6)
plotPImap(rasch_factor7)
plotPImap(rasch_factor8)

# Chi-cuadrado global
summary(rasch_factor5)$global.test
summary(rasch_factor6)$global.test
summary(rasch_factor7)$global.test
summary(rasch_factor8)$global.test

# Recalcular residuos
residuos_factor5 <- residuals(rasch_factor5)

# Verificar los residuos
summary(residuos_factor5)


residuos_factor7 <- residuals(rasch_factor7)
summary(residuos_factor7)

# Calcular la media de los ítems para cada persona (directores) en cada factor
base_directores$Factor5 <- rowMeans(factor5_items, na.rm = TRUE)
base_directores$Factor6 <- rowMeans(factor6_items, na.rm = TRUE)
base_directores$Factor7 <- rowMeans(factor7_items, na.rm = TRUE)
base_directores$Factor8 <- rowMeans(factor8_items, na.rm = TRUE)

# Crear un resumen con medias y desviaciones estándar por factor
resultados_directores <- data.frame(
  Factor = c("Factor5", "Factor6", "Factor7", "Factor8"),
  Media = c(mean(base_directores$Factor5, na.rm = TRUE),
            mean(base_directores$Factor6, na.rm = TRUE),
            mean(base_directores$Factor7, na.rm = TRUE),
            mean(base_directores$Factor8, na.rm = TRUE)),
  DesviacionEstandar = c(sd(base_directores$Factor5, na.rm = TRUE),
                         sd(base_directores$Factor6, na.rm = TRUE),
                         sd(base_directores$Factor7, na.rm = TRUE),
                         sd(base_directores$Factor8, na.rm = TRUE))
)

# Combinar media y desviación estándar en un solo campo
resultados_directores$Media_DE <- paste0(
  round(resultados_directores$Media, 2), 
  " (", 
  round(resultados_directores$DesviacionEstandar, 2), 
  ")"
)

# Seleccionar solo las columnas relevantes
resultados_final <- resultados_directores[, c("Factor", "Media_DE")]

# Mostrar los resultados como una tabla básica
print(resultados_final)

install.packages("kableExtra")
# Usar kable para una tabla más presentable (opcional)
library(kableExtra)
resultados_final %>%
  kable(format = "html", caption = "Resultados de directores por factores") %>%
  kable_styling(full_width = FALSE)

#MODELO DE ESCALA DE CALIFICACIÓN RASCH
#=======================================

install.packages(c("readr", "TAM", "plyr", "WrightMap", "eRm"))

library(readr) # For import the data
library(TAM) # For running the Rating Scale Rasch Model
library(plyr) # For plot the Item characteristic curves
library(WrightMap)# For plot the variable map
library(eRm) # For another example