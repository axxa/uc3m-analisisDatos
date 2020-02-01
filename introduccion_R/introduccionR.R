#1. Implementar una función en R que reciba un vector de números (ej. Serie de precios), y devuelva un vector que vaya desde
#el valor máximo hasta el final de la serie.
printVecInDescOrder <- function(vec) {
    return(sort.int(vec,decreasing=TRUE))
}

#2. Implementar una función en R que calcule el suavizado exponencial de un vector de números, dado un parámetro de
#suavizado a (entre 0 y 1).
calcularSuavizadoExp <- function(vectorDemandaReal, alfa) {
    if( 0 <= alfa & alfa <= 1 ){
        n = length(vectorDemandaReal)
        for(i in 1:n){
            
            if(i == 1) {
                pronostico <- vectorDemandaReal[i]
            }
            else {
                pronostico <- pronostico + alfa * ( vectorDemandaReal[i - 1] - pronostico )
            }
        }
        return (pronostico)
    }
    else{
        print("alfa debe estar entre 0.0 y 1.0")
    }
}
#3. Hacer un script que cree un data frame con dos columnas de números. Añadir una tercera columna con el vector lógico que
#resulta de evaluar qué número es mayor en cada fila.
dataFrameNumeros <- data.frame(item=1:6,
    columna1=c(1,2,3,4,53,31355),
    columna2=c(4,3,2,1,112,3123),
    columna3=c(ifelse(dataFrameNumeros$columna1 > dataFrameNumeros$columna2, dataFrameNumeros$columna1, dataFrameNumeros$columna2))
    
)
