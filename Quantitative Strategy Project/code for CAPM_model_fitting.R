rm(list=ls())
#设定工作目录
setwd("d:\\Financial Engineering")
#安装fPortfolio包
install.packages("fPortfolio")
library(fPortfolio)

#经过excel筛选得2018年2月的数据并导入
TRD <- read.csv("01.csv",header = T)
#筛选得上交所的数据
TRD <- subset(TRD,Markettype==1&Trdsta==1,select=c(Stkcd,Dretwd,Dretnd))
#计算所有个股的月均收益率
dretwd_mean <- as.matrix(tapply(TRD[,2],TRD[,1],mean))
dretnd_mean <- as.matrix(tapply(TRD[,3],TRD[,1],mean))

#市场组合的收益率
data <- read.csv(file="d:\\Financial Engineering\\index.csv")##录入指数数据
data1 <- data[,-(3:8)]##删除多余列
names(data1)=c("index","time","return rate")##更改名称
data1 <- data1[data1$index==000002,]##提取上证A股指数数据
data1$time <- as.Date(data1$time)##更改时间数据的数据类型，便于往后操作
month <- months.Date(data1$time)##计算对应月份
data1 <- cbind(data1,month)##数据合表
dt1 <- data1[,-(1:2)]
##计算每月指数的平均，作为贝塔值的分母
m2<- dt1[dt1$month=="二月",1]
m2mean <- mean(m2)
dretwd_market <- m2mean
dretnd_market <- m2mean
#无风险利率
rf <- 0.0041

#计算每只股票的beta系数
a1 <- dretwd_market-rf
b1 <- dretwd_mean-rf
beta1 <- matrix()
for (i in 1:length(dretwd_mean)) {
    beta1[i] <- solve(a1,b1[i])
}
beta1 <- na.omit(beta1)
a2 <- dretnd_market-rf
b2 <- dretnd_mean-rf
beta2 <- matrix()
for (i in 1:length(dretnd_mean)) {
    beta2[i] <- solve(a2,b2[i])
}
beta2 <- na.omit(beta2)

#计算每只股票的必要收益率
rrr1 <- rf+beta1*(dretwd_market-rf)
rrr2 <- rf+beta2*(dretnd_market-rf)
#计算每只股票2月26日-2月28日的平均收益率，并与必要收益率比较
r1_diff <- mean(TRD$Dretwd[(length(TRD$Dretwd)-2):(length(TRD$Dretwd))])-rrr1
r2_diff <- mean(TRD$Dretnd[(length(TRD$Dretnd)-2):(length(TRD$Dretnd))])-rrr2
#取前10作股票池
o1 <- order(r1_diff)
o1 <- o1[(length(o1)-9):length(o1)]
o2 <- order(r2_diff)
o2 <- o2[(length(o2)-9):length(o2)]
index <- duplicated(TRD$Stkcd)
TRD_nonrepeat <- TRD$Stkcd[!index]
pool1 <- TRD_nonrepeat[o1]
pool2 <- TRD_nonrepeat[o2]

#读取投资组合
portfolio <- read.table(file="d:\\Financial Engineering\\portfolio.csv", sep=",", header=T, encoding = "UTF-8")
portfolio <- as.timeSeries(portfolio)

#设定组合的期望收益率为0.025
spec <- portfolioSpec(portfolio=list(targetReturn=0.0041))
#设定组合的约束不许做空
cons <- 'LongOnly'
#求解
res <- efficientPortfolio(portfolio, spec = spec,constraints = cons)
summary(res)
