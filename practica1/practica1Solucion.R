source("../lib/init.R")

dax <- stockGetdata("DAX")
dax.df <- stockSubset(dax, "2011-01-01", yahoo.lastday())

Y30 = fwdReturns(dax.df$Adj.Close, 30)
daxY30 = factor(Y30 > 0, labels = c("DOWN","UP"))

numMonth <- as.integer( format(dax.df$Date,"%m"))
timestres <- c("Q1","Q2","Q3","Q4")
strQs <- ifelse(numMonth <= 3, "Q1", ifelse(numMonth<=6,"Q2", ifelse(numMonth<=9,"Q3","Q4")))
daxTrimestres <- factor(strQs)

rent10 <- backReturns(dax.df$Adj.Close, 10)
rent20 <- backReturns(dax.df$Adj.Close, 20)
rent30 <- backReturns(dax.df$Adj.Close, 30)
sma10 <- SMA_dev(dax.df$Adj.Close, 10)
sma20 <- SMA_dev(dax.df$Adj.Close, 20)
sma30 <- SMA_dev(dax.df$Adj.Close, 30)

#Features Dataset
daxFeatures <- data.frame(
                          trimes = daxTrimestres,
                          rent10 = rent10,
                          rent20 = rent20,
                          rent30 = rent30,
                          sma10 = sma10,
                          sma20 = sma20,
                          sma30 = sma30,
                          daxY = daxY30)

head(daxFeatures)
daxExamples <- na.omit(daxFeatures)

rentCorr <- data.frame(rent10 = rent10,sma10 = sma10,
                        rent20 = rent20,sma20 = sma20,
                        rent30 = rent30,sma30 = sma30
                      )

pairs(rentCorr,col = c("red","blue"))


write.csv(daxExamples, "data/practica1Resultados.csv", row.names=FALSE)
