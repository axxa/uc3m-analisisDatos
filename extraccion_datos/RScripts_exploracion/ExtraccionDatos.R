
source("init.R")

santander.df <- stockGetdata("SAN.MC")
#desarrollo
bbvaDF.df <- stockGetdata("BBVA")
mapfre.df <- stockGetdata("MAP.MC")
#-----------------------
lastday <- tail(santander.df$Date,1)

santander.df <- stockSubset(santander.df, "2011-01-01", lastday)
print(head(santander.df))
#imprimiendo data de bbva y mapfre
bbvaDF.df <- stockSubset(bbvaDF.df, "2011-01-01", lastday)
print(head(bbvaDF.df))
mapfre.df <- stockSubset(mapfre.df, "2011-01-01", lastday)
print(head(mapfre.df))
#----------------------------------

psan <- ggplot(santander.df, aes(x=Date, y=Adj.Close)) + geom_line()
print(psan)
#data para bbva y mapfre
psanBBVA <- ggplot(bbvaDF.df, aes(x=Date, y=Adj.Close)) + geom_line()
print(psanBBVA)
psanMapfre <- ggplot(mapfre.df, aes(x=Date, y=Adj.Close)) + geom_line()
print(psanMapfre)
#--------------------------

ibex <- stockGetdata("^IBEX")
ibex.df <- stockSubset(ibex, "2011-01-01", lastday)

compare.df <- data.frame(date = ibex.df$Date,
                         ibex = relativePrice(ibex.df$Adj.Close),
                         san = relativePrice(santander.df$Adj.Close),
                         bbva = relativePrice(bbvaDF.df$Adj.Close))

print(head(compare.df))

comparePlot.df <- melt(compare.df, id=c("date"))
str(comparePlot.df)

pcomp <- ggplot(comparePlot.df, aes(x=date, y=value, colour=variable)) + geom_line() + 
       scale_color_manual(values=c("black","red","blue"))
   
print(pcomp)




