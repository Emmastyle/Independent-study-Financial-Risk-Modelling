install.packages("QRM")
library(QRM)
library(fGarch)
## Losses
data(EuStockMarkets)
loss <-  as.data.frame(na.omit(-1.0 * diff(log(EuStockMarkets)) *
                                 100.0))
## GARCH GARCH公式里的一项符合t分布
gfit <- lapply(loss, garchFit,  formula = ~ garch(1,1),
               cond.dist = "std", trace = FALSE)
#For each asset, the GARCH model is used to estimate volatility, assuming that the conditional distribution is a t distribution.

gprog <- unlist(lapply(gfit, function(x)
  predict(x, n.ahead = 1)[3]))
gshape <- unlist(lapply(gfit, function(x) x@fit$coef[5]))
gresid <- as.matrix(data.frame(lapply(gfit,
                                      function(x) x@residuals / sqrt(x@h.t))))
## Copula
U <- sapply(1:4, function(y) pt(gresid[, y], df = gshape[y]))
cop <- fit.tcopula(Udata = U, method = "Kendall")
#用Kendall秩相关的方法拟合t Copula，捕捉资产之间的依赖结构。

rcop <- rcopula.t(100000, df = cop$nu, Sigma = cop$P)
qcop <- sapply(1:4, function(x) qstd(rcop[, x], nu = gshape[x]))
ht.mat <- matrix(gprog, nrow = 100000, ncol = ncol(loss),
                 byrow = TRUE)
pf <- qcop * ht.mat #乘上预测波动率
## ES 95 percent
weights <- c(0.4, 0.2, 0.2, 0.2)
pfall <- (qcop * ht.mat) %*% weights
pfall.es95 <- median(tail(sort(pfall), 5000))
pfall.es95

#In the worst 5% scenario, the average portfolio loss is about 2.58%.