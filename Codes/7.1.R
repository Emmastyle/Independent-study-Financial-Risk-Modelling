# 加载 evir 包
library(evir)
# 加载西门子数据
data(siemens)

## 计算损失
SieLoss <- -100.0 * siemens    #将收益转换成百分比形式

## 使用 evir 包拟合 GEV 分布
SieGEV <- gev(SieLoss, block = "semester")    #GEV分块, 按照semester分块
SieGEV    #输出SieGEV

#提取按时间分块的极值数据,进行初步分析和可视化

## 绘制每学期最大损失图
plot(SieGEV$data, type = "h", col = "blue", xlab = "", #SieGEV$dat提取极值数据   #type=h,展示每半年内发生的最大损失   #xlab = ""，R 仍会根据数据的索引自动生成 x 轴刻度。
     ylab = "Block Maxima",
     main = "Maximum Biannual Losses of Siemens")    #画柱状图


install.packages("ismev")  
## 加载 ismev包  专门用于极值理论分析的一个统计包
library(ismev)
# 拟合 GEV 模型
SieGEV2 <- gev.fit(SieGEV$data)  
SieGEV2 
#对极值样本进行更严格的统计建模，并提供进一步的诊断工具。


# 绘制诊断图
#使用 ismev 包提供的 gev.diag() 函数来检查 GEV 模型拟合的合理性。
gev.diag(SieGEV2)
#注意看对角线，实际经验vs.建模结果

# 置信区间分析
par(mfrow = c(2, 1))   #同时上下绘制两个图，2行1列
gev.prof(SieGEV2, m = 20, xlow = 5, xup = 16, conf = 0.95)   #m=20:剖面似然，固定位置或尺度参数（取决于 gev.prof 函数默认处理哪个参数），在该值下重新拟合 GEV 模型，对其他参数进行最大似然估计（MLE）。
#gev.prof(): 对广义极值分布的 位置参数（location）或尺度参数（scale） 进行置信区间分析。
gev.profxi(SieGEV2, xlow = 0.0, xup = 0.7, conf = 0.95)
#蓝色水平线：表示 95% 置信区间的边界。取曲线的顶点，往下移动1.92个单位。顶点：（最大似然点）：表示参数的最优估计，即数据在此参数值下最有可能出现。
#根据似然比检验，95% 置信区间对应的似然函数值是：\text{最大似然值} - \frac{1}{2} \chi^2_{1, 0.95}；对于单个参数，\chi^2_{1, 0.95} = 3.84。因此，置信区间边界对应的对数似然值是：L_{\text{max}} - \frac{3.84}{2} \approx L_{\text{max}} - 1.92；在曲线的最高点（最大似然值）上向下移动 1.92 个单位，找到那条临界水平线。
#Return Level（重现水平）是极值理论中的一个重要概念，用于描述在特定时间周期内，某个极端事件预计将会达到或超过的强度。



# 计算最大损失
mLoss <- max(SieGEV$data)

# 计算经历最大损失所需的年数
mYears <- -1 / (1 - pgev(mLoss, 
                         mu = SieGEV2$mle[1], 
                         sigma = SieGEV2$mle[2], 
                         xi = SieGEV2$mle[3])) / 2

## 使用 fExtremes 包拟合 PWM 模型
library(fExtremes)
SieGEV3 <- gevFit(SieGEV$data, type = "pwm")
SieGEV3

#pic1 即把数据按时间分块（这里是半年），提取每个时间块中的最大损失值。
#pic2 该图显示了大部分数据点都沿着对角线分布，说明 GEV 模型对概率分布的拟合比较准确。
#     大部分点接近直线，但在右侧（高分位数）略有偏离，说明模型在预测极端大损失时可能存在偏差。
#     大部分数据点在置信区间内，表明 GEV 模型对未来极端事件的预测是可信的。预测显示，每 100 个时间单位可能会经历一次超过 25 的极端损失。
#     pdf可视化，拟合的密度曲线与数据直方图较为吻合，说明模型在大多数情况下拟合良好，但在极端尾部可能略有偏离。
#pic3 极值事件（损失在6%-12%）在未来发生时，其最大值有 95% 的可能性会落在这个范围内。准备好应对最糟糕情况下的 12% 亏损。
#     形状参数的最大似然值大约为 0.3-0.4 之间，说明数据分布具有一定程度的重尾特征（厚尾）。如果 ξ > 0，说明数据分布有较大的概率发生极端事件。
#     Return Level 的 95% 置信区间大约是 [6, 12]
#     曲线在蓝线以上的区域表示参数估计是“可信的”。当曲线穿过蓝线的位置，就是这个参数的 置信区间上下界。