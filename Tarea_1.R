## Codigo apendice:

#Punto 1 y 2.
##Se crea un dataframe
##llamado data con dos columnas: CH (que contiene los números del 1 al 120) y 
##Y (que contiene una serie de valores 0 y 1).

CH <- 1:120
Y <- c(0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,1,1,0,1,1,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,1,1,1,0,1,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,1,0,0,0,1,0,0)
data <- data.frame(CH,Y)

##Se establece una semilla aleatoria para reproducibilidad. 
##Luego, se selecciona una muestra de tamaño 15 sin reemplazo 
##del conjunto de datos original.

set.seed(12345)
muestra <- sample(x=1:120,size=15,replace=F)
muestra

##Calcula la suma de los valores de la columna 
##Y correspondientes a los índices de la muestra.

sum(data[muestra,]$Y)

##Cálculos relacionados con la distribución hipergeométrica:
##Se definen dos funciones para calcular los límites inferior
##y superior del intervalo de confianza utilizando la distribución hipergeométrica. 
##Estas funciones son Li.conf.hiper.prop y Ls.conf.hiper.prop.

a <- 5
N <- 120
n <- 15
alfa <- 0.03 

Ls.conf.hiper.prop <-function (A, a, N, n, alfa) {
  phyper (a, A, N-A, n)-alfa*0.5}

Li.conf.hiper.prop<- function (A, a, N, n, alf){
  1-phyper (a-1, A, N-A, n)-alf/2}

##Gráficos de los intervalos de confianza usando distribución hipergeométrica:
##Se grafican las funciones definidas anteriormente para visualizar 
##los límites inferior y superior del intervalo de confianza utilizando 
##la distribución hipergeométrica.

par(mfrow=c(1,2))
curve(Li.conf.hiper.prop(x,a,N,n,alfa), from=0, to=41, xlab="LI",las=2 )
abline(h=0, col="blue")
curve(Ls.conf.hiper.prop(x,a,N,n,alfa), from=41, to=N, xlab="LS" )
abline(h=0, col="blue")

##Resolución de ecuaciones para los límites de confianza:
##Se utilizan las funciones uniroot.all del paquete rootSolve para encontrar
##las raíces de las ecuaciones y determinar los valores de LS1 y LI1,
##que corresponden a los límites inferior y superior del intervalo de confianza 
##utilizando la distribución hipergeométrica.
require(rootSolve)
fun_LS <- function(x) {Ls.conf.hiper.prop(x,a,N,n,alfa)}
LS1 <- uniroot.all(fun_LS, c(60,80) )
fun_LI <- function(x) {Li.conf.hiper.prop(x,a,N,n,alfa)}
LI1 <- uniroot.all(fun_LI, c(0,30) )

##Intervalos de confianza hallado mediante la distribucion hipergeometrica:
cat('IC Proporcion = (',c(LI1/N,LS1/N),')')
cat('IC Total = (',c(floor(LI1),ceiling(LS1)),')')

##Cálculos relacionados con la distribución binomial:
##Se definen funciones para calcular los límites inferior y superior del intervalo de confianza 
##utilizando la distribución binomial. Estas funciones son Li.conf.binom.prop y Ls.conf.binom.prop.
a <- 5 
n <- 15
alfa <- 0.03 

Ls.conf.binom.prop <-function (a, n, p,alfa) {
  pbinom (a,n,p)-alfa*0.5}

Li.conf.binom.prop<- function (a, n, p,alfa){
  1-pbinom (a,n,p)-alfa*0.5}

##Gráficos de los intervalos de confianza usando distribución binomial:
##Se grafican las funciones definidas anteriormente para visualizar 
##los límites inferior y superior del intervalo de confianza utilizando 
##la distribución binomial.

par(mfrow=c(1,2))
curve(Li.conf.binom.prop(a, n, x,alfa), from=0, to=a/n, xlab="LI",las=2 )
abline(h=0, col="blue")
curve(Ls.conf.binom.prop(a,n, x,alfa), from=a/n, to=1, xlab="LS",las=2 )
abline(h=0, col="blue")

##Resolución de ecuaciones para los límites de confianza:
##Se utilizan las funciones uniroot.all del paquete rootSolve para encontrar 
##las raíces de las ecuaciones y determinar los valores de LS2 y LI2, que corresponden
##a los límites inferior y superior del intervalo de confianza utilizando la distribución binomial.
require(rootSolve)
fun_LS <- function(x) {Ls.conf.binom.prop(a,n, x,alfa)}
LS2 <- uniroot.all(fun_LS, c(0.4,0.7) )
fun_LI <- function(x) {Li.conf.binom.prop(a, n, x,alfa)}
LI2 <- uniroot.all(fun_LI, c(0.10,0.20) )

##Intervalos de confianza hallado mediante la distribucion binomial:
cat('IC Proporcion = (',c(LI2,LS2),')')
cat('IC Total = (',c(floor(N*LI2),ceiling(N*LS2)),')')

#Punto 3.

# Usamos el codigo anterior y hallo el Intervalo del Total con:
##La aproximación binomial:

```{r,echo=FALSE}
cat('para calcular un intervalo con un nivel de confianza del 97% para la proporción de hogares con adultos maYores usando la distribucion binomial. basta con multiplicar por N (120) los limites del intervalo para la proporcion y quedaria de la siguiente forma (',floor(N*LI2),',',ceiling(N*LS2),')\n')
```
##La aproximación hipergeometrica:

```{r,echo=FALSE}
cat('para encontrar el intervalo de confianza para el total de casas con adultos mayores con la distribucion hipergeometrica basta con multiplicar por N los limites del intervalo de la proporcion  IC Total = (',floor(LI1),',',ceiling(LS18),')\n')
```
