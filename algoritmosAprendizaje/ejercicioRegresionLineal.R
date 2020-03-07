

library(klaR)
library(caret)
library(e1071)

source("../lib/init.R")

dax <- stockGetdata("DAX")
x <- stockSubset(dax, "2011-01-01", yahoo.lastday())

Y30 = fwdReturns(x$Adj.Close, 30)
daxY = factor(Y30 > 0, labels = c("DOWN","UP"))


daxFeatures <- data.frame(rent10 = backReturns(dax$Adj.Close, 10),
                          rent30 = backReturns(dax$Adj.Close, 30),
                          y = daxY )
daxExamples <- na.omit(daxFeatures)

xgbGrid <- expand.grid(nrounds = c(1, 10),lambda = c(.01, .1),
                        alpha = c(.01, .1),eta = .3)

# se aplica el metodo de regresion lineal para realizar el entrenamiento
modelo_entrenado <- train(y ~ ., data = daxExamples,method = "xgbLinear",preProc = c("center", "scale"),tuneGrid = xgbGrid)

# se crean las instancia para ejecutar la prediccion
instancia_a_predecir1 <- data.frame(rent10 = c(-0.25), rent30 = c(-0.17),y = c(NA_real_))
instancia_a_predecir2 <- data.frame(rent10 = c(0.15), rent30 = c(-0.57),y = c(NA_real_))
instancia_a_predecir3 <- data.frame(rent10 = c(-0.92), rent30 = c(0.81),y = c(NA_real_))

# se utiliza el modelo entrenado para realizar la prediccion por instancia
prediction1 <- predict(modelo_entrenado, newdata = instancia_a_predecir1, type = "prob")
prediction2 <- predict(modelo_entrenado, newdata = instancia_a_predecir2, type = "prob")
prediction3 <- predict(modelo_entrenado, newdata = instancia_a_predecir3, type = "prob")
print(prediction1)
print(prediction2)
print(prediction3)
