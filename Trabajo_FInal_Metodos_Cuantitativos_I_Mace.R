# PASO 1
#INSTALACION DE PAQUETE wooldridge
install.packages("wooldridge")

# PASO 2
# Cargar el paquete "wooldridge"
library(wooldridge)

# Cargar el conjunto de datos "wage2"
data(wage2)

# Ver la estructura de los datos
str(wage2)

# Obtener los nombres de las variables en el conjunto de datos
names(wage2)

# PASO 4
# Cargar la librería "dplyr"
install.packages("dplyr")
library(dplyr)

# Generar las variables adicionales
wage <- mutate(wage2, lw = log(wage/hours), exper2 = exper^2)

# PASO 5
# Eliminar las variables no deseadas
wage <- select(wage, -wage, -hours, -KWW, -age, -IQ)

# Mostrar los nombres de las variables en el nuevo marco de datos
names(wage)

# Mostrar la estructura del nuevo marco de datos
str(wage)

# PASO 6
# Revisar si hay datos ausentes
is.na(wage)

# Contar la cantidad de datos ausentes en cada variable
sum(is.na(wage))

# Limpiar el marco de datos eliminando filas con datos ausentes
wage <- na.omit(wage)

# Contar la cantidad de datos ausentes después de la limpieza
sum(is.na(wage))

#PASO 7
# Mostrar los nombres de las variables en el marco de datos actual
names(wage)

# Reordenar las columnas del marco de datos
wage <- select(wage, lw, educ, exper, exper2, tenure, sibs, brthord, married, black, south, feduc, meduc)

# Mostrar los nombres de las variables en el nuevo marco de datos
names(wage)

# PASO 8
# Cargar el paquete "stargazer"
install.packages("stargazer")
library(stargazer)

# Generar la tabla de estadísticas descriptivas
stargazer(wage, type = "text")

# PASO 9
# Cargar el paquete "apaTables"
install.packages("apaTables")

# Cargar el paquete "apaTables"
library(apaTables)

# Generar la tabla de correlaciones en formato APA
apa.cor.table(wage, filename = "Tabla1.doc", table.number = 1, landscape = T)
#cor_table <- apa_table(wage, show.conf.interval = FALSE)

# Imprimir la tabla en formato APA
#print(cor_table, format = "html", caption = "Tabla de correlaciones")

# PASO 10
# Cargar la librería "dplyr"
install.packages("dplyr")
library(dplyr)

# Generar el lote de datos excluyendo las variables dicotómicas
#wage2 <- select(wage, !married, !black, !south)
wage2 <- select(wage, -married, -black, -south)

# Verificar las variables en el nuevo lote de datos
names(wage2)

# PASO 11
# Cargar la librería "psych"
library(psych)

# Generar el gráfico múltiple de panel de comparación por pares de variables
pairs.panels(wage2)

# PASO 12
# Cargar la librería "GGally"
install.packages("GGally")
library(GGally)

# Generar el gráfico múltiple de panel de comparación por pares de variables
ggpairs(wage2)

# PASO 13
# Cargar el paquete lmtest
library(lmtest)

# Estimar la regresión múltiple (excluyendo feduc y meduc)
reg1 <- lm(lw ~ . - feduc - meduc, data = wage)

# PASO 14
# Cargar el paquete MASS
library(MASS)

# Seleccionar el mejor modelo usando el criterio AIC
best_model <- stepAIC(reg1, direction = "both")

# PASO 15
# Estimar la nueva regresión (reg2) con el modelo seleccionado
reg2 <- lm(lw ~ educ + exper + tenure + married + black + south, data = wage)

#PASO 16
# Cargar el paquete "apaTables"
library(apaTables)

# Generar los resultados del modelo reg2 en formato APA para Word
apa.reg.table(reg2, filename = "Resultados_Reg2.doc")

# PASO 17
# Cargar el paquete "stargazer"
library(stargazer)

# Generar la tabla de comparación de los resultados de las regresiones reg1 y reg2
stargazer(reg1, reg2, title = "Comparación de Regresiones",
          align = TRUE, type = "text", out = "Tabla_Comparacion.txt")

# PASO 18
# Cargar el paquete "car"
library(car)

# Calcular los factores de inflación de varianza (VIF) para el modelo reg2
vif_values <- vif(reg2)

#visualizacion
# Calcular los factores de inflación de varianza (VIF) para el modelo reg2
#vif_values <- car::vif(reg2)

# Crear un gráfico de barras para visualizar los VIF
barplot(vif_values, main = "Factores de Inflación de Varianza (VIF) para reg2",
        xlab = "Variables Explicativas", ylab = "VIF")
 #visualización con plotly
# Cargar el paquete "plotly"
install.packages("plotly")
library(plotly)

# Crear un gráfico interactivo de barras para visualizar los VIF
plot_ly(x = names(vif_values), y = vif_values, type = "bar") %>%
  layout(title = "Factores de Inflación de Varianza (VIF) para reg2",
         xaxis = list(title = "Variables Explicativas"),
         yaxis = list(title = "VIF"))

# PASO 19
# GRAFICO DE RESIDUOS
# Obtener los residuos del modelo reg2
residuos <- residuals(reg2)

# Crear un gráfico de dispersión de residuos frente a los valores ajustados
plot(predict(reg2), residuos, xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Gráfico de Residuos de reg2")
abline(h = 0, col = "red", lty = 2)  # Agregar línea horizontal en y = 0

#Prueba de Breusch-Pagan
# Cargar el paquete "lmtest"
library(lmtest)

# Realizar la prueba de Breusch-Pagan para evaluar la heterocedasticidad
bp_test <- bptest(reg2)

# Imprimir los resultados de la prueba
print(bp_test)

# PASO 20
#Prueba de Ljung-Box
# Realizar la prueba de Ljung-Box para evaluar la autocorrelación de orden 1 y 2 en los residuos
ljung_box_test <- stats::Box.test(residuos, lag = 2, type = "Ljung-Box")

# Imprimir los resultados de la prueba
print(ljung_box_test)

# PASO 21
# Cargar el paquete "AER" (para la función ivreg)
install.packages("AER")
library(AER)

# Estimar el modelo IV (iv2) con educ como variable endógena y meduc y feduc como instrumentos
iv2 <- ivreg(lw ~ educ | feduc + meduc , data = wage)

# Comparar los resultados de reg2 y iv2 usando stargazer
library(stargazer)

stargazer(reg2, iv2, title = "Comparación de Modelos",
          align = TRUE, type = "text", out = "Tabla_Comparacion2.txt")

# PASO 22
# Realizar los test de diagnóstico del modelo IV (iv2)
summary(iv2, vcov = sandwich, diagnostics = TRUE)

#PASO 23
cov1 <- vcovHC(reg1, type = "HC0")
se1 <- sqrt(diag(cov1))
cov2 <- vcovHC(reg2, type = "HC0")
se2 <- sqrt(diag(cov2))
cov_iv2 <- vcovHC(iv2, type = "HC0")
se_iv2 <- sqrt(diag(cov_iv2))

# PASO 24
stargazer(reg1, reg2, iv2, type = "text", se = list(se1, se2, se_iv2),
          title = "Tabla 3. Resultados de los modelos de Regresión")
