source("init.R")

dax <- stockGetdata("DAX")
dax.df <- stockSubset(dax, "2011-01-01", yahoo.lastday())

Y30 = fwdReturns(dax.df$Adj.Close, 30)
daxY30 = factor(Y30 > 0, labels = c("DOWN","UP"))

numMonth <- as.integer( format(dax.df$Date,"%m"))
timestres <- c("Q1","Q2","Q3","Q4")
strQs <- ifelse(numMonth <= 3, "Q1", ifelse(numMonth<=6,"Q2", ifelse(numMonth<=9,"Q3","Q4")))
daxTrimestres <- factor(strQs)

#Features Dataset
daxFeatures <- data.frame(fecha = dax.df$Date,
                          trimes = daxTrimestres,
                          rent10 = backReturns(dax.df$Adj.Close, 10),
                          rent20 = backReturns(dax.df$Adj.Close, 20),
                          rent30 = backReturns(dax.df$Adj.Close, 30),
                          sma10 = SMA_dev(dax.df$Adj.Close, 10),
                          sma20 = SMA_dev(dax.df$Adj.Close, 20),
                          sma30 = SMA_dev(dax.df$Adj.Close, 30),
                          daxY = daxY30)


daxExamples <- na.omit(daxFeatures)


write.csv(daxExamples, "data/practica1Resultados.csv", row.names=FALSE)
