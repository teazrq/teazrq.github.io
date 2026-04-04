library(survival)
source("RISTfunctions.r")


K=5      # number of covariate considered per spilt
nmin=6   # minimum number of observed data in each node
M=50     # number of trees in each fold
L = 2    # number of folds
tao=6    # length of study
P = 50   # number of dimension for X

n=200    # training sample size
nval=200 # test sample size

dataX = data.frame(replicate(P, runif(n+nval)))
colnames(dataX) <- paste("X", c(1:P), sep="")

mu=sin(dataX$X1*pi)+2*abs(dataX$X2-0.5)+dataX$X3^3
y=rexp(n+nval, 1/mu)
C=runif(n+nval, 0, tao)
censor=(y<=C)
obtime=apply(cbind(y,C), 1, min)

dataset=data.frame(cbind(dataX, censor, obtime)[1:n,])
testset=data.frame(cbind(dataX, censor, obtime)[(n+1):(n+nval),])


R_Muti_ERT_build = Muti_ERT_fit(dataset, M, K, L, nmin, SupLogRank=1, tao, impute="random")
R_Muti_ERT_predict= Muti_ERT_Predict(testset, R_Muti_ERT_build$Forest_seq[[L]], R_Muti_ERT_build$SurvMat_seq[[L]], R_Muti_ERT_build$time_intrest)



###############################################
#############   read me !!! ###################
###############################################

1. 

Use "Muti_ERT_fit" to fit the model, and use "R_Muti_ERT_predict" to predict new subjects

Dataset and testset must be arranged in the following order:

(X, cencoring indicator, time)

where "cencor = 1" means failure, and "0" means censored.


"R_Muti_ERT_predict" gives three outputs: 
	1) Predicted min(T, tao). 
	2) Predicted survival function for each subject (one row for each subject, one column for each time point)
	3) All time points

2.

specify parameters:

K     # number of covariate considered per spilt, usually sqrt(p) or p/3
nmin  # minimum number of observed data in each node, default is 6.
M     # number of trees in each fold, default is 50
L     # number of folds, 1-5 are recommended.

tao # lengh of study. Must be larger than all survival times.
	As long as it is larger than all survival times, the value of it will not make any difference.

SupLogRank # "1" is default, using log-rank test to find best split. 
		 "2" is using sup log-rank test to find best split. This could be time consuming.
		 "0" is using t-test to compare two groups, not recommended.

impute # imputation method, always use "random". Please do not change this.



3. 

Since all computations are done in R (I'm still working on the C version), it can be a little bit slow for large data set.
You could first try a small value of M and L=1 see how much time would be needed.
The total computational time should grow linearly as M*L grows.






















