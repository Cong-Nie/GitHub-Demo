setwd("D:/")
# 将数据下载后放入上述目录
options(digits=3,scipen=3)     #有效数字不超过3位，并且小数点后数字不超过3位
install.packages("quadprog")
library(quadprog)

# 导入2014-2017年数据(20支股票回报率和4支债券)
X <- read.table("S2.csv", sep=",", header=T);
R <- X[,-1];

# 计算均值、标准差和协方差；
mean_vec <- apply(R, 2, mean);
cov_mat <- cov(R);
sd_vec <- sqrt(diag(cov_mat));

# 设置给定均值回报率进行风险最小化约束等式 (A'x=B)
# 约束矩阵(各资产比例之和为1；
#          资产组合目标收益率；
#          固定股票和债券投资比例（例如：股票：70%, 债券：30%）;
#          限制卖空：W[i]>=0; 各资产持有比例非负) 
A <- cbind(rep(1,24), 
           mean_vec, 
           c(rep(1,20),rep(0,4)),
           diag(1,nrow=24));    
# 设定600个目标收益率（由于限制卖空，必须处于受约束资产组合中最大和最小收益率之间）
Mu <- seq(0.12, 0.3, length=600);    
# 设置变量Sd用来存储资产组合收益率的标准差;
Sd <- Mu;  
# 设置变量W用来存储资产组合比例；
W <- matrix(0, nrow=600, ncol=24);  

# 利用二次规划(QP)为各个目标回报率计算最优投资组合
for (i in 1:length(Mu)) 
{
    B <- c(1, Mu[i], 0.7, rep(0,24))   # 约束向量
    result <-
        solve.QP(Dmat=2*cov_mat,dvec=rep(0,24),Amat=A,bvec=B,meq=2)
    Sd[i] <- sqrt(result$value) # 最小风险
    W[i,] <- result$solution    # 投资比例
}

# 作图(回报率和风险的组合)
plot(Sd,Mu,type="l",xlim=c(0,1.2),ylim=c(0,.6),lty=3, lwd=3,
     ylab="Expected Return", xlab="Risk (Standard Deviation)") 

# 无风险资产回报率(例如：3%)
mufree = 0.03 
points(0,mufree,cex=3,pch=20) 

# 计算 Sharpe's ratios
sharpe =( Mu-mufree)/Sd 
# 计算Sharpe's ratio的最大值
sharpe.max = (sharpe == max(sharpe))  
# tangency portfolio的投资比例
W.opt <- round(W[sharpe.max,],6);
# 切线投资组合
lines(c(0,2),mufree+c(0,2)*(Mu[sharpe.max]-mufree)/Sd[sharpe.max],lwd=3,lty=2,col="darkgrey")

# 风险资产的最优投资组合
points(Sd[sharpe.max],Mu[sharpe.max],cex=3,pch=20,col="red") 

# 计算最小方差投资组合 minimum variance portfolio
mvp = (Sd == min(Sd)) 
points(Sd[mvp],Mu[mvp],cex=3, pch=20, col="blue") 

# 有效边界 the efficient frontier
ef = (Mu > Mu[mvp])
lines(Sd[ef],Mu[ef],type="l",xlim=c(0,.5),
      ylim=c(0,.2),lwd=3, col="black")
