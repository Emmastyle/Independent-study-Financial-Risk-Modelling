# 安装 fGarch 包
install.packages("fGarch")
library(fGarch)

# 使用 gpdDiagnostics 进行诊断
gpdDiagnostics(BAFit)
install.packages("fExtremes")
## Loading of packages
library(fBasics)
library(fExtremes)
## Data handling
data(DowJones30)
DJ <- timeSeries(DowJones30[, -1],  #去除第一列日期列，只保留股价数据。
                 charvec = as.character(DowJones30[, 1]))  #使用第一列（通常是日期）作为时间索引。
BALoss <- -1.0 * returns(DJ[, "BA"], percentage = TRUE,
                         trim = TRUE)  #*-1将收益率转换为损失（负收益），便于极值分析中的风险建模。
                                       #trim=true, 去除包含缺失值（NA）的观测值，以避免在计算过程中因缺失值导致的错误或不准确的结果。


## MRL-plot 选择合适的阈值
#平均超越函数，MRL（Mean Residual Life）图帮助选择合适的阈值 u 来拟合广义帕累托分布（GPD）。
mrlPlot(BALoss, umin = -10, umax = 10) 
#在金融时间序列中，通常收益率（或损失率）分布在一个有限的范围内，比如 -10% 到 10% 之间，因为在正常市场波动中，超过这个范围的情况比较少见。


## GPD
BAFit <- gpdFit(BALoss, u = 3) #在  u = 3  以上，图形保持平稳的线性趋势。
## Diagnostic plots
plot(BAFit)
## Risk measures
gpdRiskMeasures(BAFit, prob = c(0.95, 0.99, 0.995)) #红色线，表示置信区间

#阈值  u < 0  的部分：平均超越值随着阈值的增加快速下降，呈现出非线性趋势。
#阈值  u  在大约 0 到 3 之间：平均超越值曲线变得较为平稳，接近水平线或线性增长趋势。这个阶段的点大致沿直线分布，说明在该范围内 GPD 拟合较为合适。
#阈值  u > 5  的部分：均值开始明显上升，并且波动剧烈。阈值过高，导致超越样本数不足（因为极端事件很少）。数据量太小，拟合不稳定，容易产生偏差。
