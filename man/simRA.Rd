\name{simRA}
\alias{simRA}

\title{
Simulating reproductive burden
}
\description{
Simulating and graphically illustrating the expected reproductive burden during pregnancy for a live-bearing species as a function of time and for a specific degree of superfetation, litter size, egg mass at fertilization, and offspring mass at birth.
}
\usage{
ReproductiveBurden(SF, LS, numberLitter, eggWeight, offspringWeight, lifespan = 5001 + (numberLitter - 1) * (round(5001/SF, digits = 0)), ymax = NULL, xmax = NULL, cex.axis = 0.8, col = "black", lwd = 1, margins = c(5, 6, 4, 2), mean = TRUE, colMean = "tomato", add = FALSE, at = -30, boxCol = "black", power = (3/2), plot = TRUE)
}

\arguments{
  \item{SF}{
Degree of superfetation; should be >=1
}
  \item{LS}{
Number of embryos in a litter
}
  \item{numberLitter}{
Number of litters to produce; note: numberLitter=life time reproductive success/litter size
}
  \item{eggWeight}{
Egg mass at fertilization
}
  \item{offspringWeight}{
Offspring mass at birth
}
  \item{lifespan}{
Lifespan in developmental stages of embryos (optional); 50 stages correspond to a complete gestation cycle and approximately 30 days in Poeciliidae; default is the time required to produce the specified number of litters with the specified degree of superfetation
}
  \item{ymax}{
Y-axis limit (optional); default=max(reproductive burden)
}
  \item{xmax}{
X-axis limit (optional); default=max(stages)
}
  \item{cex.axis}{
Axes label size (optional)
}
  \item{col}{
Curve color (optional); default="black"
}
  \item{lwd}{
Line width (optional); default=1
}
  \item{margins}{
Plot margins (optional); default=c(5, 6, 4, 2)
}
  \item{mean}{
If TRUE, the mean will be added to the boxplot (optional)
}
  \item{colMean}{
Color of the mean (optional); default="tomato"
}
  \item{add}{
If TRUE, an additional curve can be added to the plot (optional)
}
  \item{at}{
Position of boxplot (optional); default=-30
}
  \item{boxCol}{
Color of the boxplot (optional); default="black"
}
  \item{power}{
Power transformation coefficient for the developmental stage of embryos (optional); default=3/2, which is used for Poeciliopsis retropinna (Hagmayer et al. 2018); if power=1, no power-transformation will be applied (i.e. pure exponential relationship)
}
  \item{plot}{
Whether to show the graph (optional); default=TRUE
}
}
\details{

}
\value{

}
\references{
Hagmayer, A., Furness, A. I., Reznick, D. N., and Pollux, B. J. (2018). Maternal size and body condition predict the amount of post-fertilization maternal provisioning in matrotrophic fish. Ecology and Evolution, 8(24):12386-12396
}
\author{
Andres Hagmayer
}
\note{
This package was designed for matrotrophic species with a Matrotrophy Index (MI) of >1. There is no guarantee that meaningful results are obtained for species with a MI<1.
}


\seealso{

}
\examples{
## The function is currently defined as
function (SF, LS, numberLitter, eggWeight, offspringWeight, lifespan = 5001 + 
    (numberLitter - 1) * (round(5001/SF, digits = 0)), ymax = NULL, 
    xmax = NULL, cex.axis = 0.8, col = "black", lwd = 1, margins = c(5, 
        6, 4, 2), mean = TRUE, colMean = "tomato", add = FALSE, 
    at = -30, boxCol = "black", power = (3/2), plot = TRUE) 
{
    lambda <- round(5001/SF, digits = 0)
    seq_stage <- seq(0, 50, 0.01)
    M <- matrix(nrow = 5001 + (numberLitter - 1) * (lambda), 
        ncol = numberLitter, data = NA)
    for (c in 1:ncol(M)) {
        for (i in 1:5001) {
            M[i + (c - 1) * lambda, c] <- seq_stage[i]
        }
    }
    M <- M[1:lifespan, ]
    sub_hypothetical_function <- function(stage) {
        powerStage <- stage^power
        intercept <- log(eggWeight)
        beta <- (log(offspringWeight) - log(eggWeight))/(50^power)
        exp(intercept + beta * powerStage)
    }
    M2 <- sub_hypothetical_function(M)
    M3 <- M2 * LS
    data <- data.frame(cbind(seq(length.out = nrow(M3), from = 0, 
        by = 0.01), rowSums(M3, na.rm = T)))
    colnames(data) <- c("time", "burden")
    if (plot == TRUE) {
        if (is.null(ymax)) {
            ymax <- max(data$burden)
        }
        else {
            ymax <- ymax
        }
        if (is.null(xmax)) {
            xmax <- max(data$time)
        }
        else {
            xmax <- xmax
        }
        if (add == FALSE) {
            par(mar = margins)
            plot(data$burden ~ data$time, type = "l", cex.axis = cex.axis, 
                axes = F, xlab = "", ylab = "Reproductive burden", 
                col = col, xlim = c(-50, xmax), ylim = c(0, ymax), 
                lwd = 1)
            boxplot(data$burden, add = T, at = at, boxwex = 15, 
                border = boxCol, pch = 20, axes = FALSE)
            if (mean == TRUE) {
                segments(x0 = at - 3, x1 = at + 3, y0 = mean(data$burden), 
                  y1 = mean(data$burden), col = colMean, lwd = 3)
            }
            axis(side = 1, labels = round(seq(0, xmax, length.out = 5), 
                1), at = seq(0, xmax, length.out = 5), cex.axis = 0.8)
            axis(side = 2, labels = round(seq(0, ymax, length.out = 5), 
                1), at = seq(0, ymax, length.out = 5), cex.axis = 0.8)
            mtext(text = "Time [stages]", cex = 0.8, line = -2.5, 
                outer = T, side = 1, at = 0.65)
        }
        if (add == TRUE) {
            boxplot(data$burden, add = T, at = at, boxwex = 15, 
                border = boxCol, pch = 20, axes = FALSE)
            lines(data$burden ~ data$time, col = col, lwd = lwd)
            if (mean == TRUE) {
                segments(x0 = at - 3, x1 = at + 3, y0 = mean(data$burden), 
                  y1 = mean(data$burden), col = colMean, lwd = 3)
            }
        }
    }
    invisible(data)
  }
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
