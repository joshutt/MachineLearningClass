coinFlips <- function() {
    results <- rbinom(1000, 10, .5)
    c(results[1], sample(results,1), min(results)) / 10
}


prob12 <- function() {
    N <- 100000
    vRes <- colSums(t(replicate(N, coinFlips())))/N
}



generatePts <- function(N=100) {
    pts <- matrix(c(rep(1,N), runif(2*N, -1, 1)), ncol=3)
    lnPt <- matrix(runif(4, -1, 1), ncol=2)
    w <- as.numeric(vector(length=3))

    m <- matrix(c(rep(1,2), lnPt[,1]), ncol=2)
    eq <- solve(m, lnPt[,2])
    y <- sign(pts[,2] * eq[2] + eq[1] - pts[,3])

    return( list(pts=pts, res=y, target=eq))
}


calcError <- function(N=100, eout=1000, returnType="ein", dataSet=NULL) {
    if (is.null(dataSet)) {
        dataSet <- generatePts(N)
    }
    dataReg<- lm(res ~ pts[,2:3], data=dataSet)

    coefs <- dataReg$coefficients
    pts <- dataSet$pts

    lmresult <- sign(coefs[1] + coefs[2]*pts[,2] + coefs[3]*pts[,3])
    wrong <- sum(dataSet$res != lmresult)

    outSam <- matrix(c(rep(1, eout), runif(2*eout,-1,1)), ncol=3)
    realOutEr <- sign(outSam[,2] * dataSet$target[2] + dataSet$target[1] - outSam[,3])
    estOutEr <- sign(coefs[1] + coefs[2]*outSam[,2] + coefs[3]*outSam[,3])
    totOutEr <- sum(realOutEr != estOutEr)
    #return (list(einError=(wrong/N), eoutError=totOutEr/eout))
    if (returnType == "eout") {
        return (totOutEr/eout)
    } else {
        return (wrong/N)
    }
}


prob5 <- function() {
    N <- 100
    #return (mean(replicate(1000, calcError())[[1]]))
    return (mean(replicate(1000, calcError())))
}

prob6 <- function() {
    return (mean(replicate(1000, calcError(returnType="eout"))))
}

prob7 <- function() {
    source("wk1.R")
    result <- replicate(1000, runPla())
    rowSums(result)/1000
}

runPla <- function() {
    dataSet <- generatePts(10)
    dataReg<- lm(res ~ pts[,2:3], data=dataSet)
    pla(N=10, w=dataReg$coef, pts=dataSet$pts, eq=dataSet$eq)
}


generateNonLinearData <- function() {
    dataSet <- data.frame(x1=runif(1000, -1, 1), x2=runif(1000, -1, 1))
    y <- sign(data[,1]^2 + data[,2]^2 - 0.6)
    for (x in sample(y, 100)) {
        y[x] <- y[x] * -1
    }
    dataSet$y <- y
    return (dataSet)
}


calc8 <- function() {
    dataSet <- generateNonLinearData()
    model <- lm(y ~ x1 + x2, data=dataSet)
    coef <- model$coef

    ein <- sign(coef[1] + dataSet$x1 * coef[2] + dataSet$x2 * coef[3])
    mean(ein != dataSet$y)
}

prob8 <- function() {
    mean(replicate(1000, calc8()))
}


calc9 <- function() {
    dataSet <- generateNonLinearData()
    model <- lm(y ~ x1 + x1 + x1*x2 + I(x1^2) + I(x2^2), data=dataSet)

    revDataSet <- data.frame(rep(1,1000), x1=dataSet$x1, x2=dataSet$x2, x1x2=dataSet$x1*dataSet$x2, x1x1 = dataSet$x1^2, x2x2 = dataSet$x2^2) 
    vars <- c(-1, -0.05, 0.08, 0.13, 1.5, 1.5)
    ya <- sign(rowSums(vars*revDataSet))
    vars <- c(-1, -0.05, 0.08, 0.13, 1.5, 15)
    yb <- sign(rowSums(vars*revDataSet))
    vars <- c(-1, -0.05, 0.08, 0.13, 15, 1.5)
    yc <- sign(rowSums(vars*revDataSet))
    vars <- c(-1, -1.5, 0.08, 0.13, 0.05, 0.05)
    yd <- sign(rowSums(vars*revDataSet))
    vars <- c(-1, -0.05, 0.08, 1.5, .15, .15)
    ye <- sign(rowSums(vars*revDataSet))

    y <- sign(model$coef[1]*1 + model$coef[2] * dataSet$x1 + model$coef[3]*dataSet$x2 + model$coef[4]*dataSet$x1^2 + model$coef[5]*dataSet$x2^2 + model$coef[6]*dataSet$x1*dataSet$x2)

    return (c(sum(y!=ya), sum(y!=yb), sum(y!=yc), sum(y!=yd), sum(y!=ye)))
}


prob9 <- function() {
    rowMeans(replicate(1000, calc9()))
}

