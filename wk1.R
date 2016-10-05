
pla <- function(N=10, graph=FALSE, gfail=FALSE, hideReal=FALSE) {
    pts <- matrix(c(rep(1,N), runif(2*N, -1, 1)), ncol=3)
    lnPt <- matrix(runif(4, -1, 1), ncol=2)
    w <- as.numeric(vector(length=3))

    m <- matrix(c(rep(1,2), lnPt[,1]), ncol=2)
    eq <- solve(m, lnPt[,2])
    y <- sign(pts[,2] * eq[2] + eq[1] - pts[,3])

    count <- 0


    while (sum(sign(colSums(t(pts)*w)) != y) > 0){
    if (graph) {
        plot(pts[,2], pts[,3], xlim=c(-1,1), ylim=c(-1,1))
        if (!hideReal) {
            abline(eq, lwd=3, col="red")
        }
    }
        count <- count+1
        vals <- sample(nrow(pts))
        #print(count) 
        for (i in vals) {
            x <- pts[i,]
            if (sign(sum(x * w)) == y[i]) {
                next
            }
            w <- w + x * y[i]
            #w[1] <- 1
            break
        }

        if (graph) {
            abline(-w[1]/w[3], -w[2]/w[3], col="blue")
            Sys.sleep(1)
        }
    }

    if (hideReal) {
        line <- readline()
        abline(eq, lwd=3, col="red")
    }

    check <- matrix(c(rep(1,10000), runif(20000, -1, 1)), ncol=3)
    checkY <- sign(check[,2] * eq[2] + eq[1] - check[,3])
    checkW <- sign(colSums(t(check) * w))

    if (gfail) {
        passpass <- check[sign(check[,2]*eq[2]+eq[1]-check[,3]) > 0 & sign(check[,2]*-w[2]/w[3] + -w[1]/w[3] - check[,3]) > 0, ]
        failfail <- check[sign(check[,2]*eq[2]+eq[1]-check[,3]) < 0 & sign(check[,2]*-w[2]/w[3] + -w[1]/w[3] - check[,3]) < 0, ]
        passfail <- check[sign(check[,2]*eq[2]+eq[1]-check[,3]) > 0 & sign(check[,2]*-w[2]/w[3] + -w[1]/w[3] - check[,3]) < 0, ]
        failpass <- check[sign(check[,2]*eq[2]+eq[1]-check[,3]) < 0 & sign(check[,2]*-w[2]/w[3] + -w[1]/w[3] - check[,3]) > 0, ]
        points(passpass[,2], passpass[,3], pch=16, col="green")
        points(failfail[,2], failfail[,3], pch=16, col="blue")
        points(failpass[,2], failpass[,3], pch=16, col="red")
        points(passfail[,2], passfail[,3], pch=16, col="red")
        print( (nrow(passfail) + nrow(failpass)) / nrow(check))
    }

    return(c(count, sum(checkY != checkW) / nrow(check)))
}




