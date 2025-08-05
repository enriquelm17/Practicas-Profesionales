---
title: "MICE impute"
author: "Enrique Lopez Martinez"
date: "`r Sys.Date()`"
output: html_document
---

MICE (Multivariate imputation by chained equations) es un método de imputación múltiple que se utiliza para reemplazar los valores de datos que faltan en un conjunto de datos bajo ciertos supuestos sobre el mecanismo de falta de datos, el planteamiento de ecuaciones encadenadas es muy flexible y puede manejar variables de distintos tipos (por ejemplo, continuas o binarias).

Funciona creando múltiples versiones de un conjunto de datos, donde cada versión contiene diferentes imputaciones (valores estimados) para los datos faltantes. Luego, se realiza el análisis estadístico en cada conjunto de datos imputado y los resultados se combinan para obtener una estimación más robusta y precisa, para cada variable con datos faltantes, se crea un modelo predictivo utilizando las otras variables del conjunto de datos como predictores.

Para el presente proyecto se realizara este metodo para la base de datos Framinghan, la cual corresponde a una base pequeña del Framinghan Heart Study.

```{r Verificacion de NA,echo=TRUE,include=TRUE,message=FALSE,warning=FALSE}
fram<- read.csv("framingham_dataset.csv", header = TRUE)
#Para poder imputar los datos tomaremos la libreria "mice" de R
library(mice)

#Observamos los datos faltantes de nuestra base
library(VIM)
aggr(fram)

```

Hacemos la imputacion para toda la base, la desventaja de este metodo es que genera una base de datos nueva con los datos imputados y se genera una nueva por cada iteracion ocupando recursos computacionales, sin embargo, MICE permite trabajar con conjuntos de datos que tienen valores faltantes, lo que es común en muchos conjuntos de datos del mundo real.

```{r Iteracion con MICE, echo=TRUE,include=TRUE,message=FALSE,warning=FALSE}
fram_imp <- mice(fram) #Por defecto itera 5 veces y la documentacion recomienda de 5 a 10, en este caso lo dejaremos en 5

#completamos la data 

fram_comp <- complete(fram_imp)
aggr(fram_comp)
```

# Tabla 2 de Framingham con datos imputados

Usuaremos las mismas variables del ejercicio anterior (Tabla 2 Datos Framingham) pero ahora con la base de datos imputados:

```{r, echo=TRUE,include=TRUE,message=FALSE,warning=FALSE}
#definimos las variables:
age <- fram_comp$age 
sex <- factor(fram_comp$sex,levels = c(1,2) , labels = c("Male","Female"))
glucose <- fram_comp$glucose #mg/dL
diabetes <- factor(fram_comp$diabetes,levels = c(0,1) , labels = c("no","si"))
stroke <- factor(fram_comp$stroke,levels = c(0,1) , labels = c("no","si"))
cigarettes <- fram_comp$cigpday #cigarettes per day
hyperten <- factor(fram_comp$hyperten,levels = c(0,1) , labels = c("no","si"))
cholesterol <- fram_comp$totchol #Total cholesterol (mg,dL)
bmi <- fram_comp$bmi #Body mass index
rate <- fram_comp$heartrte # Heart rate (beats per minute)
```

Para ver la diferencia entre los datos tomaremos la variable de glucosa la cual tiene una de las variables con mas datos faltantes y la compararemos con la base de datos nueva

```{r, echo=TRUE,include=TRUE,message=FALSE,warning=FALSE}
plot(fram$age,fram$glucose,main = "Scatter plot: Age vs Glucose whit NA", xlab = "Age (Years)", ylab = "Glucose (mg/dL)")

plot(age,glucose,main = "Scatter plot: Age vs Cholesterol data complete", xlab = "Age (Years)", ylab = "Glucose (mg/dL)")



```

Observamos una grafica mas densa en los datos imputados que en la base original, ahora proseguimos con la tabla 2 con datos imputados

```{r Tabla 2, echo=TRUE,include=TRUE,message=FALSE,warning=FALSE}
library(tidyverse)
library(broom)
fchd <- factor(fram_comp$mi_fchd , levels = c(0,1) , labels = c("no","si"))
modelo <- glm(fchd ~ age + sex + glucose + diabetes + hyperten + cigarettes + stroke + rate + cholesterol, data = fram, family = binomial(link = "logit"))
summary(modelo)

```

### Comparacion de datos

![Tabla 2 datos originales](tablaf.png)

Viendo la comparacion de ambas tablas observamos que cambian los coeficientes y disminuyen los errores estandar, dandonos asi un nuevo modelo de prediccion probabilistico dandole a este mas precision

```{r , echo=TRUE,include=TRUE,message=FALSE,warning=FALSE}
tabla_2 <- tidy(modelo, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(across(c(estimate, conf.low, conf.high), ~round(., 2))) %>%
  mutate(p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))) %>%
  select(term, estimate, std.error, p.value, conf.low, conf.high)

knitr::kable(tabla_2, 
             col.names = c("Variable", "OR", "Error Estándar", "p-valor", "IC 95% (bajo)", "IC 95% (alto)"),
             caption = " Tabla 2: Modelo de regresión logística para predecir enfermedad de miocardio")
```

Las OR de los datos originales comparados con los datos imputados no difieren, solo en la variable hyperten que aumenta 3 milesimas, por lo tanto el nuevo modelo probabilistico quedaria de la siguiente forma:

$$ logit(L) = -4.312 +  0.027*age  -1.281*sex +  0.003*glucose + 0.792*diabetes + 0.682*hyperten + 0.009*cigpday$$

$$
+ 0.360*stroke + 0.002*rate +  0.007*cholesterol $$
