library(fExtremes)
library(fBasics)
data(nyse)
NYSELevel <- timeSeries(nyse[, 2],
                        charvec = as.character(nyse[, 1]))
NYSELoss <- na.omit(-1.0 * diff(log(NYSELevel)) * 100)
colnames(NYSELoss) <- "NYSELoss"
## Point process data
NYSEPP <- pointProcess(x = NYSELoss, u = quantile(NYSELoss, 0.95))


## Declustering
#这些数据不是独立分布的
DC05 <- deCluster(x = NYSEPP, run = 5, doplot = TRUE)
DC10 <- deCluster(x = NYSEPP, run = 10, doplot = TRUE)
DC20 <- deCluster(x = NYSEPP, run = 20, doplot = TRUE)
DC40 <- deCluster(x = NYSEPP, run = 40, doplot = TRUE)
DC60 <- deCluster(x = NYSEPP, run = 60, doplot = TRUE)
DC120 <- deCluster(x = NYSEPP, run = 120, doplot = TRUE)
#去簇（Declustering） 是一种常用的极值分析技术，用于去除时间序列中由于自相关而产生的聚簇效应（例如市场连锁反应导致的连续大幅波动。主要用于处理点过程（Point Process）中的聚集现象（clustering）。
#去簇处理的主要目的是将这些次生事件（secondary events）从主要事件（primary events）中分离出来。e.g主震之后会有一系列余震，形成簇状聚集。
#较大的窗口只保留了真正独立的极端事件，适合进行 极值理论（EVT）建模，用于风险管理和预警。去簇窗口太大（如 120）会大大减少极端样本数量，可能会降低统计推断的精度。
#在实际应用中，需在 独立性 和 样本量 之间进行平衡。

## Fit of declustered data
DC05Fit <- gpdFit(DC05, u = min(DC05))
DC10Fit <- gpdFit(DC10, u = min(DC10))
DC20Fit <- gpdFit(DC20, u = min(DC20))
DC40Fit <- gpdFit(DC40, u = min(DC40))
DC60Fit <- gpdFit(DC60, u = min(DC60))
DC120Fit <- gpdFit(DC120, u = min(DC40))
#对去簇后的数据进行广义帕累托分布（GPD）拟合
#u = min(DCxx): 使用该簇中最小的值（即最大的损失）作为阈值。

