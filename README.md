# Biplot-con-aditamentos
La técnica Exploratiria multivariada de biplots, es de uso muy frecuente pero si existe variables clasificatorias entre los individuos o entre las variable esto no es fácil de incluir. Por otra parte no todos los individuos ni todas las variables quedan igalmente representadas en los planos graficados.
En este trabajo mostramos como a través de tipo de punto o colores, podemos mostrar variables clasificatorias en los individuos o variables, y a traves de degrade de colores mostrar el grado de representacion que tiene cada individuo y/o variable en el plano graficado.

# Biplots con colores clasificando variables y/o individuos

# Gráficos Biplot
En muchos estudios diseñados u observacionales es usual registrar sobre un mismo conjuntos de individuos una serie de variables. Los gráficos biplot son una representación conjunta de los individuos y las variables medidas sobre ellos. Estos Gráficos fueron originalmente introducido por Gabriel (1971) y en este trabajo usaremos la notación presentada en ese trabajo. 

# Gráficos Biplot en colores distintos para grupos de variables y/o grupos de individuos.
En muchas aplicaciones las variables variables analizadas pueden agruparse por algún criterio, o bien los individuos pueden ser clasificados con algún criterio de interés. En cualquiera de los casos puede ser útil que en el gráfico biplot se pudieran identificar por color dichas categorías. 
Para ello se presentan a continuación tres funciones, que son simples modificaciones de las funciones biplot, biplot.princomp y biplot.prcomp de R, pero a las que se le agregan  4 nuevos argumentos:   

"colx" = vector de longitud igual a la cantidad de individuos (número de filas de la matriz a representar) y en cada coordenada indicará a que grupo pertenece ese individuo, por defecto es NULL,

"coly" = vector de longitud igual a la cantidad de variables (número de columnas de la matriz a representar) y en cada coordenada indicará a que grupo pertenece esa variable,  "angulo" =  valor numérico entre 0 y 180 que controla el ángulo de la cabeza de la flecha, por defecto es NULL, por defecto es 30, 

"pch.ind" =   indica que símbolo usar para representar los individuos, con la misma notación que el argumento pch en la función plot, por defecto usa el nombre de la fila.

para el resto de los argumentos ver ayuda de la función biplot (?biplot).
```r 
biplot.col <-
function (x, y, var.axes = TRUE, col, colx=NULL, coly=NULL, cex = rep(par("cex"), 2),
    xlabs = NULL, ylabs = NULL, expand = 1, xlim = NULL, ylim = NULL,
    arrow.len = 0.1, main = NULL, sub = NULL, xlab = NULL, ylab = NULL,angulo=30,
    pch.ind=NULL, ...)
{
    n <- nrow(x)
    p <- nrow(y)
    if (missing(xlabs)) {
        xlabs <- dimnames(x)[[1]]
        if (is.null(xlabs))
            xlabs <- 1:n
                          }
    xlabs <- as.character(xlabs)
    dimnames(x) <- list(xlabs, dimnames(x)[[2]])
    if (missing(ylabs)) {
        ylabs <- dimnames(y)[[1]]
        if (is.null(ylabs))
            ylabs <- paste("V", 1:p,sep="")
    }
    ylabs <- as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2]])
    if (length(cex) == 1)
        cex <- c(cex, cex)
    if (missing(col)) {
        col <- par("col")
        if (!is.numeric(col))
            col <- match(col, palette(), nomatch = 1)
        col <- c(col, col + 1)
    }
    else if (length(col) == 1)
        col <- c(col, col)
    unsigned.range <- function(x) c(-abs(min(x)), abs(max(x)))
    rangx1 <- unsigned.range(x[, 1])
    rangx2 <- unsigned.range(x[, 2])
    rangy1 <- unsigned.range(y[, 1])
    rangy2 <- unsigned.range(y[, 2])
    if (missing(xlim) && missing(ylim)) {
        xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)}  else {
        if (missing(xlim)){
             xlim <- rangx1}          else {
             if (missing(ylim)) {
                 ylim <- rangx2}}}
    ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
    on.exit(par(op))
    op <- par(pty = "s")
    if (!is.null(main))
        op <- c(op, par(mar = par("mar") + c(0, 0, 1, 0)))
    plot(x, type = "n", xlim = xlim, ylim = ylim, col = col[1],
        xlab = xlab, ylab = ylab, sub = sub, main = main,asp=1, ...)
    if (missing(colx)) {if(is.null(pch.ind))  text(x, xlabs, cex = cex[1], col = col[1], ...)
                       else points(x,pch=pch.ind,  cex = cex[1], col = col[1], ...)
                        }
    else {if(is.null(pch.ind)) text(x, xlabs, cex = cex[1], col = colx, ...)
         else points(x,pch=pch.ind,  cex = cex[1], col = colx, ...)
          }
    par(new = TRUE)
    plot(y, axes = FALSE, type = "n", xlim = xlim * ratio, ylim = ylim *
        ratio, xlab = "", ylab = "", col = col[1],asp=1, ...)
    axis(3, col = col[2])
    axis(4, col = col[2])
    box(col = col[1])
    if (missing(coly)) {
        text(y, labels = ylabs, cex = cex[2], col = col[2], ...)
        if (var.axes)
            arrows(0, 0, y[, 1] * 0.8, y[, 2] * 0.8, col = col[2],
                  length = arrow.len,angle=angulo)
                          }
    else {text(y, labels = ylabs, cex = cex[2], col = coly, ...)
    if (var.axes)
        arrows(0, 0, y[, 1] * 0.8, y[, 2] * 0.8, col = coly,
            length = arrow.len,angle=angulo)
          }
    invisible()
}



biplot.col.princomp <-
function (x, choices = 1:2, scale = 1, pc.biplot = FALSE, ...)
{
    if (length(choices) != 2)
        stop("length of choices must be 2")
    if (!length(scores <- x$scores))
        stop(gettextf("object '%s' has no scores", deparse(substitute(x))),
            domain = NA)
    lam <- x$sdev[choices]
    if (is.null(n <- x$n.obs))
        n <- 1
    lam <- lam * sqrt(n)
    if (scale < 0 || scale > 1)
        warning("'scale' is outside [0, 1]")
    if (scale != 0)
        lam <- lam^scale
    else lam <- 1
    if (pc.biplot)
        lam <- lam/sqrt(n)
    biplot.col(t(t(scores[, choices])/lam), t(t(x$loadings[,
        choices]) * lam), ...)
    invisible()
}



biplot.col.prcomp <-
function (x, choices = 1:2, scale = 1, pc.biplot = FALSE, ...)
{
    if (length(choices) != 2)
        stop("length of choices must be 2")
    if (!length(scores <- x$x))
        stop(gettextf("object '%s' has no scores", deparse(substitute(x))),
            domain = NA)
    if (is.complex(scores))
        stop("biplots are not defined for complex PCA")
    lam <- x$sdev[choices]
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    if (scale < 0 || scale > 1)
        warning("'scale' is outside [0, 1]")
    if (scale != 0)
        lam <- lam^scale
    else lam <- 1
    if (pc.biplot)
        lam <- lam/sqrt(n)
    biplot.col(t(t(scores[, choices])/lam), t(t(x$rotation[,
        choices]) * lam), ...)
    invisible()
}
```
Deberá correr estas tres funciones antes de utilizarlas. 

# Ejemplo:
# Análisis de componentes principales con la función “prcomp”  a los datos de “ardeche” del paquete “ade4”.
Para este ejemplo es necesario cargar los siguentes paquetes.
```r
library(ade4)
help(ardeche)
data(ardeche)
```

Vemos en la ayuda que “ardeche” es una lista con la cantidad de individuos identificados de varias especies de macroinvertebrados bentónicos en diferentes sitios y fechas. 
En esta lista tenemos 6 elementos: 
ardeche$tab es un archivo de datos con las 43 especies en las filas y 35 sitios y fechas en las columnas
ardeche$col.blocks Es un vector con cantidad de columnas que se midieron en cada fecha y la identificación de la fecha: julio 1982, agosto 1982, noviembre 1982, febrero 1983, abril 1983 y julio 1983.
ardeche$row.blocks es un vector con cuantas especies (filas hay en cada grupo ) y la identificación del grupo que define el orden de la especie.
ardeche$dat.fac es  un vector factor de longitud  igual a la cantidad de columnas identificando a que fecha corresponde cada columna.
ardeche$sta.fac es  un vector factor de longitud igual  a la cantidad de columnas identificando a que sitio corresponde cada columna.
ardeche$esp.fac es  un vector factor de longitud igual  a la cantidad de filas identificando a que Orden corresponde la especie de cada fila (Ephemeroptera, Plecoptera, Coleoptera, Trichoptera).

```r
ardeche.pca <- prcomp(ardeche$tab)
biplot(ardeche.pca)
biplot.col.prcomp(ardeche.pca)
```

En ambos casos obtenemos el mismo biplot (Figura Nº1). Pero como en este conjunto de datos las columnas pueden agruparse por fechas,  por ejemplo,  podemos usar el argumento “coly” de nuestra función para identificar con colores dichas fechas.

```r
biplot.col.prcomp(ardeche.pca, coly=as.numeric(ardeche$dat.fac))
legend(locator(n=1),names(ardeche$col.blocks),lty=1, lwd=1.3, pch=NULL,
       col= 1:nlevels(ardeche$dat.fac),title="Fecha",cex=0.7)  
```

Atención: La segunda sentencia coloca la leyenda en el gráfico mediante la función locator(), por lo cual al ejecutarla y pararse sobre la ventana de gráfico, el cursor cambia a una cruz, y al hacer click con el ratón (mouse) fijará en esa posición el extremo superior izquierdo de la leyenda, y no devolverá el control de R hasta que no se haya procedido a ubicar la leyenda. Si no queda bien ubicada la leyenda deberá re-intentar corriendo las dos sentencias nuevamente y/o modificando la longitud de los ejes con los argumentos xlim o ylim. 
Figura Nº 1: Biplot del análisis de componentes principales de los datos de ardeche$tab.
Figura Nº 2: Biplot del análisis de componentes principales de los datos de ardeche$tab, identificando con distintos colores, las fechas en que se realizaron las mediciones.
Como en este conjunto de datos las filas también pueden agruparse por orden de la especie, podemos usar el argumento “colx” de nuestra función

```r
biplot.col.prcomp(prcomp(ardeche$tab), colx=as.numeric(ardeche$esp.fac))
legend(locator(n=1),names(ardeche$row.blocks), pch=20,
       col= 1:nlevels(ardeche$esp.fac), title="Orden",cex=0.7)
```

Podemos usar ambos argumentos colx  y coly de nuestra función
```r
biplot.col.prcomp(ardeche.pca,
                  colx=as.numeric(ardeche$esp.fac),
                  coly=as.numeric(ardeche$dat.fac) )
legend(locator(n=1),names(ardeche$row.blocks),pch=20, 
       col= 1:nlevels(ardeche$esp.fac), title="Orden",cex=0.7)  
legend(locator(n=1),names(ardeche$col.blocks),lty=1, lwd=1.3, pch=NULL,
       col= 1:nlevels(ardeche$dat.fac), title="Fecha",cex=0.7)    

```
Figura Nº 3: Biplot del análisis de componentes principales de los datos de ardeche$tab, identificando con distintos colores, el orden de cada especie.

Figura Nº 4: Biplot del análisis de componentes principales de los datos de ardeche$tab, identificando con distintos colores, el orden de cada especie y la fecha de cada observación.
Al ser el nombre de la informativo del orden de la especie la información es redundante, podemos usar el argumento pch.ind para limpiar la imagen.
```r
biplot.col.prcomp(ardeche.pca, pch.ind=20,
                  colx=as.numeric(ardeche$esp.fac),
                  coly=as.numeric(ardeche$dat.fac))
legend(locator(n=1),names(ardeche$row.blocks), pch=20, 
       col= 1:nlevels(ardeche$esp.fac), title="Orden",cex=0.7)  
legend(locator(n=1),names(ardeche$col.blocks),lty=1, lwd=1.3, pch=NULL,
       col= 1:nlevels(ardeche$dat.fac), title="Fecha",cex=0.7)  
```

En la figura 5 las filas (especies) quedan representadas por puntos que resultan muy pequeños y  no es fácil verlos cuando quedan entre los vectores que representan las columnas # podemos usar el argumento “cex” para ampliar el tamaño del punto pero no el de las flechas.

```r
biplot.col.prcomp(ardeche.pca, pch.ind=20, cex=c(2,1),
                  colx=as.numeric(ardeche$esp.fac),
                  coly=as.numeric(ardeche$dat.fac))
legend(locator(n=1),names(ardeche$col.blocks),lty=1, pt.lwd=1.5, 
       col= 1:nlevels(ardeche$dat.fac),pch=NULL, title="Fecha",cex=0.7)  
legend(locator(n=1),names(ardeche$row.blocks),pch=20,
       col= 1:nlevels(ardeche$esp.fac), title="Orden",cex=0.7,pt.cex=2)  
abline(h=0, v=0, lty=2, col="gray")
```

Figura Nº 5: Biplot del análisis de componentes principales de los datos de ardeche$tab, identificando con distintos colores, el orden de cada especie y la fecha de cada observación, pero identificando las distintas especies de cada orden sólo por un punto.

Figura Nº 6: Biplot del análisis de componentes principales de los datos de ardeche$tab, identificando con distintos colores, el orden de cada especie y la fecha de cada observación, pero identificando las distintas especies de cada orden sólo por un punto de tamaño más adecuado e incorporando lineas de referencia en los ejes PC1=0 y PC2=0.

# Gráficos Biplot en degrade
La calidad de representación de cada una de las variables y de cada uno de los individuos, en un plano de componentes principales, puede ser muy distinta, y es de vital importancia conocerla a la hora de interpretar el gráfico obtenido. Lo que se propone en esta representación, es utilizar tanto para los individuos como para las variables, una gama de colores que salen desde el blanco en el caso de tener 0% de representación en el plano que se este visualizando, hasta el color puro (rojo para las variables y negro para los individuos) en el caso de tener 100% de representación. Esto permite que al interpretar el gráfico aquellos individuos y/o variables que no este bien representados en el plano que se este mirando,  queden representados por colores muy claros y sea fácil evitar su interpretación, mientras que aquellos individuos o variables bien representados resalten por su color fuerte y sean el centro de la interpretación.
La función biplot.deg se ejecuta directamente sobre la matriz de datos (con individuos en las filas y variables en las columnas.

```r
biplot.deg <- function(M,cent.=TRUE, scal.=TRUE,scale= 1,choices=c(1,2),
                       individuos="individuos", variables="variables", ...){

  ind.palette <- colorRampPalette(c("white","black"))
  vari.palette <- colorRampPalette(c("white","red"))
  leg.tex <- as.character(seq(0,100,by=20))
  MM <- scale(M, center=cent., scale=scal.)
  MM.svd <- svd(MM)
  indi <- MM.svd$u%*%diag(MM.svd$d^(1-scale))
  rownames(indi) <- rownames(MM)
  vari <- MM.svd$v%*%diag(MM.svd$d^scale)
  rownames(vari) <- colnames(MM)
  ejes <-round(100*MM.svd$d^2/sum(MM.svd$d^2),2)
  rep.indi_choices <- round(100*rowSums(indi[,choices]^2)/rowSums(indi^2),2)
  rep.vari_choices <- round(100*rowSums(vari[,choices]^2)/rowSums(vari^2),2)
  biplot.col(indi[,choices],vari[,choices],
             colx=ind.palette(100)[rep.indi_choices],
             coly=vari.palette(100)[rep.vari_choices],
             xlab=paste("Primera componente (",ejes[choices[1]],"%)"),
             ylab=paste("Segunda componente (",ejes[choices[2]],"%)"),...)
  abline(h=0,v=0,col="gray",lty=2)
  legend(locator(n=1),leg.tex,
         fill= ind.palette(100)[c(1,seq(20,100,by=20))],
         title=paste("% de rep. ", individuos),cex=0.7)   
  legend(locator(n=1),leg.tex,
         fill= vari.palette(100)[c(1,seq(20,100,by=20))],
         title=paste("% de rep. ", variables),cex=0.7)   
}
```
Los argumentos de esta función son:
M = Matriz de datos con individuos en las filas y variables en las columnas.
cent. = Valor lógico que indica si debe centrase a la matriz por columnas,
Matriz de datos con individuos en las filas y variables en las columnas.
scal. = Valor lógico que indica si deben estandarizarse las por columnas,
scale = Valor numérico entre 0 y 1, los individuos serán escalados por lamnda^(1-scale) y las variables por lamnda^scale. Donde lamnda son los valores singulares de M, es decir el desvío estándar explicado por cada eje principal. (Si Scale=1 se consigue la mejor representación de las variables, si scale=0 la mejor representación de los individuos, y si lamnda =0.5, se logra equilibrar la representación tanto de individuos como de variables)
choices = vector numérico de dos coordenadas con ejes que se desea graficar.
individuos, variables = Expresiones que indiquen que son los individuos y las variables respectivamente, en términos de la problemática en cuestión, por defecto usará “individuos” y “variables”. 
Atención esta función colocará legendas en el gráfico a través de la función locator(), por lo tanto luego de correrla, deberá hacer dos click en la ventana de gráficos en los lugares donde desee que aparezca la esquina superior izquierda de cada leyenda. Si las leyendas taparan una parte importante del gráfico, entonces deberá volver a correr el biplot agregando los argumentos xlim, o ylim, con valores adecuados para dejar lugar para las leyendas.

# Ejemplo: 
```r
biplot.deg(ardeche$tab[28:43,30:35],scal.=FALSE)  # con máxima 
                                       # representación de las variables
biplot.deg(ardeche$tab[28:43,30:35], scale=0)# con máxima representación                                                                      
                                             # de los individuos
biplot.deg(ardeche$tab[28:43,30:35], scale=0.5, 
           xlim=c(-2.2, 2.2),scal.=FALSE)    # con representación 
                                             # balanceada de variables        
                                             # e individuos (Fig.Nº 7).
```
Figura Nº 7: Biplot del los datos de ardeche$tab, correspondientes a las variables observadas en la fecha “Julio de 1983” y de las especies del orden “Trichoptera”;  identificando con distinta intensidad de color el grado de representación de las variables y los individuos (con representación balanceada entre ambos). 

Todos los análisis y gráficos de este documento fueron realizados en R, versión 3.1.0 (2014-04-10). R Core Team (2014). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.

