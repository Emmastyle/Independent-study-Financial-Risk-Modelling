library(ghyp)
library(timeSeries)
library(fBasics)
## Return calculation
data(DowJones30) #道琼斯30只股票的时间序列数据
y <- timeSeries(DowJones30[, "HWP"], charvec =   
                  as.character(DowJones30[, 1]))    #让时间序列数据以日期字符格式存储
yret <- na.omit(diff(log(y)) * 100)                 ##diff(log(y)) 计算相邻数据的对数差，即对数收益率；na.omit() 删除 NA(not available) 值，以防止数据缺失影响计算
## Fitting
ef <- density(yret)          #计算经验密度估计，即数据的平滑直方图  
ghdfit <- fit.ghypuv(yret, symmetric = FALSE,
                     control = list(maxit = 1000))   #参数估计，最大迭代次数=1000
hypfit <- fit.hypuv(yret, symmetric = FALSE,
                    control = list(maxit = 1000))
nigfit <- fit.NIGuv(yret, symmetric = FALSE,
                    control = list(maxit = 1000))
## Densities
ghddens <- dghyp(ef$x, ghdfit)         #dghyp(x, model) 计算指定分布的概率密度函数（PDF）
hypdens <- dghyp(ef$x, hypfit)
nigdens <- dghyp(ef$x, nigfit)
nordens <- dnorm(ef$x, mean = mean(yret), sd = sd(c(yret[, 1])))      #dnorm(x, mean, sd) 计算正态分布（Normal Distribution） 的密度，这里是为了与拟合分布做对比
col.def <- c("black", "red", "blue", "green", "orange")
plot(ef, xlab = "", ylab = expression(f(x)), ylim = c(0, 0.25))
lines(ef$x, ghddens, col = "red")
lines(ef$x, hypdens, col = "blue")
lines(ef$x, nigdens, col = "green")
lines(ef$x, nordens, col = "orange")
legend("topleft",
       legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col = col.def, lty = 1)
## QQ-Plots            #QQ 图的作用：可视化比较 数据分布 vs. 假设分布。数据点落在对角线上 → 拟合较好。检验是否存在厚尾现象
qqghyp(ghdfit, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "", cex = 0.8)
qqghyp(hypfit, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
       gaussian = FALSE, line = FALSE, cex = 0.8)
qqghyp(nigfit, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
       gaussian = FALSE, line = FALSE, cex = 0.8)
legend("topleft", legend = c("GHD", "HYP", "NIG"),
       col = col.def[-c(1,5)], pch = 1:3)
## Diagnostics
AIC <- stepAIC.ghyp(yret, dist = c("ghyp", "hyp", "NIG"),         #计算Akaike Information Criterion：AIC = -2 \log(L) + 2k，用于模型选择的标准。 L是模型的最大似然估计值； k是模型的自由参数个数。AIC 越小，模型越优，因为它在权衡模型拟合度（Likelihood） 和复杂度（自由参数个数）
                    symmetric = FALSE,
                    control = list(maxit = 1000))
LRghdnig <- lik.ratio.test(ghdfit, nigfit)                        #似然比检验，LR = -2 (\log L_1 - \log L_2)， L_1是较简单模型（参数较少）， L_2是较复杂模型（参数较多）。如果 p 值小于 0.05，表示更复杂的模型 显著优于另一个模型。若 p 值较大（> 0.05），说明两个模型拟合差不多，可以选择更简单的那个。
LRghdhyp <- lik.ratio.test(ghdfit, hypfit)

print(AIC)

library(ggplot2)
# 创建 AIC 值的 DataFrame
AIC_values <- data.frame(
  Distribution = c("GHD", "HYP", "NIG"),
  AIC = c(11684.07, 11704.41, 11691.90)  # 直接使用 fit.table 里的 AIC 值
)

print(AIC_values)
print(AIC_values)
# 画出 AIC 值的柱状图
library(ggplot2)
ggplot(AIC_values, aes(x = Distribution, y = AIC, fill = Distribution)) +
  geom_bar(stat = "identity") +
  labs(title = "AIC Comparison of Different Distributions", y = "AIC Value") +
  theme_minimal()

LRT_results <- data.frame(
  Comparison = c("GHD vs NIG", "GHD vs HYP"),
  LR_Statistic = c(LRghdnig$statistic, LRghdhyp$statistic),
  p_Value = c(LRghdnig$p.value, LRghdhyp$p.value)
)

print(LRT_results)
#可视化P值
ggplot(LRT_results, aes(x = Comparison, y = p_Value, fill = Comparison)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(title = "Likelihood Ratio Test (LRT) Results", y = "p-value") +
  theme_minimal()