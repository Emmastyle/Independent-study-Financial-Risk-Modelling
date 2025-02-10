## 加载必要的库
library(lmomco)
library(FRAPO)

## 加载数据
data(SP500)
Idx <- SP500[, "QCOM"]

## 计算离散收益率并取相反数（表示损失）
L <- -1 * returnseries(Idx, method = "discrete", trim = TRUE)

## 设置滚动窗口参数
level <- 0.99
ep <- seq(104, length(L), by = 1)
sp <- seq(1, length(ep), by = 1)
VaR <- matrix(NA, ncol = 2, nrow = length(ep))

## 计算 VaR（基于 Normal 和 GLD 分布）
for (i in 1:length(sp)) {
  if (i > length(ep)) break  # 防止 i 超出范围
  
  x <- L[sp[i]:ep[i]]
  
  if (length(x) == 0 || any(is.na(x))) {
    next  # 跳过空数据或包含 NA 的情况
  }
  
  ## 计算 L-moments 并拟合 GLD 分布
  lmom <- lmom.ub(x)
  fit <- tryCatch(pargld(lmom), error = function(e) NULL)
  
  if (!is.null(fit)) {
    VaRGld <- quagld(level, fit)
  } else {
    VaRGld <- NA  # 避免错误导致的崩溃
  }
  
  ## 计算正态分布的 VaR
  VaRNor <- qnorm(level, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  
  ## 存储 VaR 结果
  VaR[i, ] <- c(VaRGld, VaRNor)
  
  ## 控制台输出（每 10 次输出一次）
  if (i %% 10 == 0) {
    print(paste("Iteration:", i, "Window End:", ep[i], "VaR GLD:", VaRGld, "VaR Normal:", VaRNor))
  }
}

## 结果整理
Res <- cbind(L[105:length(L)], VaR[-nrow(VaR), ])
colnames(Res) <- c("Loss", "VaRGld", "VaRNor")

## 绘制回测结果
plot(Res[, "Loss"], type = "p", xlab = "Time Index",
     ylab = "Losses in percent", pch = 19, cex = 0.5,
     ylim = c(-15, max(Res, na.rm = TRUE)))
abline(h = 0, col = "grey")  # 添加 0 轴参考线
lines(Res[, "VaRGld"], col = "blue", lwd = 2)
lines(Res[, "VaRNor"], col = "red", lwd = 2)
legend("bottomleft", legend = c("Losses", "VaR GLD", "VaR Normal"),
       col = c("black", "blue", "red"),
       lty = c(NA, 1, 1), pch = c(19, NA, NA), bty = "n")

#黑色散点离散程度变小