# 安装和加载必要的包
install.packages("zoo")
install.packages("AER")
install.packages("fGarch")
install.packages("timeSeries")

library(AER)
library(fGarch)
library(zoo)
library(timeSeries)

# 导入数据并转换
data(NYSESW)
NYSELOSS <- timeSeries(-1.0 * diff(log(NYSESW)) * 100, char.vec = time(NYSESW))

# 缩小样本量（前 2000 个观测值，进一步加速）
NYSELOSS_sub <- NYSELOSS[1:1000, ]

# 绘制图 8.2：NYSE 日损失时间序列图（小样本版）
plot(NYSELOSS_sub, main = "NYSE 日损失时间序列图（前 2000 样本）", ylab = "%", xlab = "时间")
mtext("图 8.2   NYSE 日损失时间序列图", side = 3, line = 1, adj = 1)

# 定义计算 t-GARCH(1,1) ES 的函数，并加上容错机制
ESgarch <- function(y, p = 0.99){
  result <- tryCatch({
    gfit <- garchFit(formula = ~garch(1, 1), data = y, cond.dist = "std", trace = FALSE)
    sigma <- predict(gfit, n.ahead = 1)[3]
    df <- coef(gfit)["shape"]
    ES <- sigma * (dt(qt(p, df), df) / (1 - p)) * ((df + (qt(p, df))^2) / (df - 1))
    ES
  }, error = function(e) NA)
  return(result)
}

# 转换为 zoo 对象
loss_vec <- as.numeric(NYSELOSS_sub)
loss_dates <- time(NYSELOSS_sub)
loss_zoo <- zoo(loss_vec, order.by = loss_dates)

# 将 rolling window 改成 300 且每10步计算一次，快速版
window_size <- 200

NYSEES <- rollapply(loss_zoo,
                    width = window_size,
                    FUN = function(x) ESgarch(x),
                    by = 10,  # 每10步计算一次，极大提速
                    align = "right",
                    fill = NA)

NYSEESL1 <- lag(NYSEES, k = -1)

if (exists("NYSEESL1")) {
  res <- na.omit(cbind(NYSELOSS_sub[index(NYSEESL1)], NYSEESL1))
  colnames(res) <- c("NYSELOSS", "ES99")
  
  # 绘制图 8.3，保持原配色
  plot(res[,"ES99"], col = "red", ylim = range(res), main = "NYSE: t-GARCH(1,1) ES 99%（快速版）", ylab = "%", xlab = "")
  points(res[,"NYSELOSS"], cex = 0.4, pch = 19, col = "blue")
  legend("topleft", legend = c("损失", "ES"), col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1))
  grid()
  mtext("图 8.3   NYSE 和 ES 日损失对比（快速版）", side = 3, line = 1, adj = 1)
} else {
  print("ES 计算失败，res 未生成。请检查窗口大小或数据问题。")
}
