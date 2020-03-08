library(caret)

source("../lib/init.R")

dax <- stockGetdata("DAX")
dax.df <- stockSubset(dax, "2011-01-01", yahoo.lastday())

Y50 = fwdReturns(dax.df$Adj.Close, 50)
y50C <- ifelse(Y50 >= -.08 & Y50 <= .08 , "NEUTRO", ifelse(Y50 > .08 ,"SUBE", ifelse(Y50 < -.08,"BAJA","NA")))
daxY <- factor(y50C)


daxFeatures <- data.frame(rent50 = backReturns(dax$Adj.Close, 50),
                          y = daxY )
daxExamples <- na.omit(daxFeatures)

longdaxExamples <- nrow(daxExamples)
# data de entrenamiento
trainingData <- na.omit(daxExamples[1:longdaxExamples*0.7,])
# data a predecir
toPredictData <- data.frame(rent50 = na.omit(daxExamples[longdaxExamples*0.7+1:longdaxExamples,1]))

xgbGrid <- expand.grid(nrounds = c(1, 10),lambda = c(.01, .1),
                        alpha = c(.01, .1),eta = .3)

# se aplica el metodo de regresion lineal para realizar el entrenamiento
modelo_entrenado_regresion <- train(y ~ .,
                                    data = trainingData,
                                    method = "xgbLinear",
                                    preProcess = c("center", "scale"),
                                    tuneGrid = xgbGrid
                                    )

# se utiliza el modelo entrenado para realizar la prediccion por instancia
# aca se predice directamente la rentabilidad
prediccionRegresion <- predict(modelo_entrenado_regresion, newdata = toPredictData, type = "prob")
# print("Clasificacion con regresion lineal para predecir rentabilidad")
# print(prediccionRegresion)


# -------------------------------Clasificacion
TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)
# Se entrena el modelo con el algritmo Support Vector Machine
# Train a model with above parameters. We will use C5.0 algorithm
modelo_entrenado <- train(y ~ ., data = trainingData,
                      method = "C5.0",
                      preProcess=c("scale","center"),
                      trControl= TrainingParameters,
                      na.action = na.omit
)
# se utiliza el modelo entrenado para realizar la prediccion por instancia
#aca se clasifica la instancia en las clases neutro, baja o sube basada en su rentabilidad futura
prediccionC50 <-predict(modelo_entrenado, toPredictData)

# print("Clasificacion con C5.0")
# print(prediccionC50)


# Se entrena el modelo con el algoritmo Naive Bayes
NaiveModel <- train(y ~ ., data = trainingData,
                    method = "nb",
                    preProcess=c("scale","center"),
                    trControl= TrainingParameters,
                    na.action = na.omit
)
NaivePredictions <-predict(NaiveModel, toPredictData, na.action = na.pass)
# print("Clasificacion con Naive")
# print(NaivePredictions)

write.table(prediccionRegresion, "entregables/practica2Resultados.csv", sep = ",", col.names = !file.exists("entregables/practica2Resultados.csv"), append = T)
write.table(prediccionC50, "entregables/practica2Resultados.csv", sep = ",", col.names = !file.exists("entregables/practica2Resultados.csv"), append = T)
write.table(NaivePredictions, "entregables/practica2Resultados.csv", sep = ",", col.names = !file.exists("entregables/practica2Resultados.csv"), append = T)

