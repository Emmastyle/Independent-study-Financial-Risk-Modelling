## Probabilities
p <- seq(0.001, 0.05, 0.001)#sequence(from,to,by) 表示不同置信水平（概率）,这些概率值用于计算 VaR 和 ES
## VaR
ghd.VaR <- abs(qghyp(p, ghdfit)) #计算不同分布的VaR  q分位数函数 ghyp包（广义双曲分布）
hyp.VaR <- abs(qghyp(p, hypfit)) 
nig.VaR <- abs(qghyp(p, nigfit))
nor.VaR <- abs(qnorm(p, mean = mean(yret), sd = sd(c(yret[, 1])))) 
emp.VaR <- abs(quantile(x = yret, probs = p)) 
# Plot of VaR
plot(emp.VaR, type = "l", xlab = "", ylab = "VaR", axes = FALSE,
     ylim = range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.VaR, ghd.VaR, hyp.VaR,
                          nig.VaR, nor.VaR)))
lines(seq(along = p), ghd.VaR, col = "red")  #eq(along=p) 生成一个 从 1 到 length(p) 的整数序列
lines(seq(along = p), hyp.VaR, col = "blue")
lines(seq(along = p), nig.VaR, col = "green")
lines(seq(along = p), nor.VaR, col = "，orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),   #c(...) 创建一个字符向量
       col = col.def, lty = 1)   #lty=1 实现
## ES
ghd.ES <- abs(ESghyp(p, ghdfit)) 
hyp.ES <- abs(ESghyp(p, hypfit)) 
nig.ES <- abs(ESghyp(p, nigfit)) 
nor.ES <- abs(mean(yret) - sd(c(yret[, 1])) *
                dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(yret))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(yret))[1:x])))   #c(yret)：将 yret 变成向量，防止它是矩阵格式。sort(c(yret))：对收益 yret 进行从小到大的排序，确保低收益在前。[1:x]：取最小的 x 个观测值，即低于 p 分位数的 x 个收益。
## Plot of ES
plot(emp.ES, type = "l", xlab = "", ylab = "ES", axes = FALSE,  #自定义图形（如 axes=FALSE 时），适用于box()
     ylim = range(c(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES)))  
box() #在当前绘图窗口中添加边框
axis(1, at = 1:length(p), labels = names(emp.VaR), tick = FALSE)   #隐藏刻度线（ticks）
axis(2, at = pretty(range(emp.ES, ghd.ES, hyp.ES, nig.ES, nor.ES)))  #
lines(1:length(p), ghd.ES, col = "red")
lines(1:length(p), hyp.ES, col = "blue")
lines(1:length(p), nig.ES, col = "green")
lines(1:length(p), nor.ES, col = "orange")
legend("topright",
       legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),
       col = col.def, lty = 1)  #col.def 是一个变量，存储不同分布对应的颜色,之前的代码已经定义了

#e.g.如果 ES = 12，意味着在最糟糕的 1% (横坐标为置信水平）情况下，平均损失约为 12 个单位（如百万美元）。