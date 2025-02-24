## Loading of packages
library(evir)
library(ismev)
## Order statistics
data(bmw)
BmwLoss <- -1.0 * bmw * 100 
Years <- format(attr(BmwLoss, "time"), "%Y")
attr(BmwLoss, "years") <- Years
Yearu <- unique(Years) #unique() 函数会返回 Years 向量中所有不重复的年份。
idx <- 1:length(Yearu) #向量长度，idx 是用来遍历每一年数据的索引。
r <- 2   #r 表示要从每年的数据中提取前 2 个最大的值（即每年的前两个最大损失）。
BmwOrder <- t(sapply(idx, function(x)  #sapply() 会返回一个 长度为 2 的向量（因为 r = 2）。
  head(sort(BmwLoss[attr(BmwLoss, "years") ==
                      Yearu[x]], decreasing = TRUE), r)))
rownames(BmwOrder) <- Yearu
colnames(BmwOrder) <- paste("r", 1:r, sep = "")

## Plot of order data
plot(Yearu, BmwOrder[, 1], col = "black", ylim = range(BmwOrder),
     ylab = "Losses BMW (percentages)", xlab = "",
     pch = 21, bg = "black")
points(Yearu, BmwOrder[, 2], col = "grey", pch = 23, bg = "grey")

## Fit and diagnostics
BmwOrderFit <- rlarg.fit(BmwOrder) 
rlarg.diag(BmwOrderFit)

#pic3 横轴是理论分位数，纵轴是实际数据的分位数。k=2第二大极值，拟合的没有k=1好