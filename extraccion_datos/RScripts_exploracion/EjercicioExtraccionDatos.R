#3. Crear un script de R en el que generemos una gráfica para visualizar la correlación de los rendimientos diarios entre el IBEX35 y el Santander,
#   y el IBEX35 y Mapfre, ambos desde el 2011. Para esto es necesario:
  #i- Obtener y procesar las cotizaciones correspondientes y generar los dataframes con los datos desde 2011.
  #ii- Generar los vectores de rendimientos diarios con la función stepReturn programada en los scripts de R.
  #iii- Generar una gráfica de puntos con la librería ggplot2
source("init.R")

santander.df <- stockGetdata("SAN.MC")
mapfre.df <- stockGetdata("MAP.MC")
ibex <- stockGetdata("^IBEX")

lastday <- tail(santander.df$Date,1)

#i- Obtener y procesar las cotizaciones correspondientes y generar los dataframes con los datos desde 2011.
santander.df <- stockSubset(santander.df, "2011-01-01", lastday)
mapfre.df <- stockSubset(mapfre.df, "2011-01-01", lastday)
ibex.df <- stockSubset(ibex, "2011-01-01", lastday)
#-------------------------------------------------

#ii- Generar los vectores de rendimientos diarios con la función stepReturn
ndays <- length(ibex.df$Date)

compare.df <- data.frame(
  date = ibex.df$Date[2:ndays],
  ibex = stepReturn(ibex.df$Adj.Close),
  san = stepReturn(santander.df$Adj.Close),
  mapf = stepReturn(mapfre.df$Adj.Close)
)

print(head(compare.df))

comparePlot.df <- melt(compare.df, id=c("date"))
str(comparePlot.df)

#iii- Generar una gráfica de puntos con la librería ggplot2
pcomp <- ggplot(
  comparePlot.df, aes(x=date, y=value, colour=variable)
) + geom_line() + scale_color_manual(values=c("black","red","blue"))

print(pcomp)
