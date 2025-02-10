#	1.	计算 HYP（超双曲分布）的参数（如 chi 和 xi 系数）。
# 2.	拟合 HYP 分布到金融收益率数据。
# 3.	绘制形状三角形（Shape Triangle），可视化不同 HYP 分布形态的关系。

rd <- c(1, 5, 10, 20, 40) #选择不同的时间跨度（rd 代表计算收益率的不同时间间隔）。
yrets <- na.omit(matrix(unlist(lapply(rd,
                                      function(x) diff(log(y), lag = x))), ncol = 5))
#创建一个 金融收益率矩阵 yrets，用于后续的 HYP 拟合分析。

## Function for xi/chi coefficients
xichi <- function(x){
  param <- coef(x, type = "alpha.delta")
  rho <- param[["beta"]] / param[["alpha"]]
  zeta <- param[["delta"]] * sqrt(param[["alpha"]]^2 -
                                    param[["beta"]]^2)
  xi <- 1 / sqrt(1 + zeta)
  chi <- xi * rho
  result <- c(chi, xi)
  names(result) <- c("chi", "xi")
  return(result)
}
#该函数用于 将 HYP 分布参数转换成三角形坐标 (chi, xi)，用于可视化 HYP 分布的形状。

## HYP Fitting
hypfits <- apply(yrets, 2, fit.hypuv, symmetric = FALSE)
points <- matrix(unlist(lapply(hypfits, xichi)),
                 ncol = 2, byrow = TRUE)
#计算每个时间跨度 rd 对应的 HYP (chi, xi) 参数，并整理成矩阵 points，用于后续绘图。

## Shape triangle
col.def <- c("black", "blue", "red", "green", "orange")
leg.def <- paste(rd, rep("day return", 5))
plot(points, ylim = c(-0.2, 1.2), xlim = c(-1.2, 1.2),
     col = col.def, pch = 16, ylab = expression(xi),
     xlab = expression(chi))
lines(x = c(0, -1), y = c(0, 1))
lines(x = c(0, 1), y = c(0, 1))
lines(x = c(-1, 1), y = c(1, 1))
legend("bottomright", legend = leg.def, col = col.def, pch = 16)
text(x = 0.0, y = 1.05, label = "Laplace", srt = 0)
text(x = -1.0, y = 1.05, label = "Exponential", srt = 0)
text(x = 1.0, y = 1.05, label = "Exponential", srt = 0)
text(x = 0.0, y = -0.1, label = "Normal", srt = 0)
text(x = -0.6, y = 0.5, label = "Hyperbolic, left skewed",
     srt = 302)
text(x = 0.6, y = 0.5, label = "Hyperbolic, right skewed",
     srt = 57)