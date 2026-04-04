library(survival)

RIST_fit <-function(x, y, censor, M=50, K=5, L=3, nmin=6, split=c("logrank", "suplogrank", "random"), tao=max(y), impute=c("random", "expected"))
{
  if (!is.matrix(x)) stop("x must be a matrix, please covert categorical variables to binary")
  if (!is.numeric(x)) stop("x must be numerical")
  if (nrow(x) != length(y) | nrow(x) != length(censor)) stop("Number of observations do not match")
  
  match.arg(split, c("logrank", "suplogrank", "random"))
  match.arg(impute, c("random", "expected"))
  
  P = ncol(x)
  N = nrow(x)
  treelength = N/nmin
  time_interest = sort(y[censer == 1])
  
  Forest_0 = list()
  SurvMat_0 = list()
  
  for (i in 1:M) #fit M trees
  {
    Fit_Surv_Tree(x, y, censor, K, nmin, split)
  }    
  
  
  
  
  newset=dataset
  newp=dim(dataset)[2]
  P=newp-2
  need=ceiling(dim(dataset)[1]/nmin)
  time_intrest=sort(dataset[dataset[,P+1]==1, P+2])
  
  Forest=matrix(0,2*need,7*M)
  Large=rep(0,M)
  Forestdata=matrix(0,dim(dataset)[1],M)
  SurvMat=list()
  
  for (i in 1:M) #fit M trees
  {
    Treefit=Fit_ERT_Tree(newset,dataset, K, nmin, SupLogRank, Step=0, tao)
    Forest[1:dim(Treefit$Tree)[1],(7*(i-1)+1):(7*i)]=Treefit$Tree
    Large[i]=dim(Treefit$Tree)[1]
    Forestdata[,i]=Treefit$UpdateData[,dim(dataset)[2]]
    SurvMat[[i]]=Treefit$Survival_All
  }
  
  if (sum(newset[, P+1] == 0) > 0)
  {
    censornum=seq(1,dim(dataset)[1])[dataset[,P+1]==0]
    
    censorset = dataset[dataset[,P+1]==0,]
    
    censor_surv = Muti_ERT_Predict(censorset, Forest, SurvMat, time_intrest)$Surv_predict
    
    cond_sample_set=list()
    cond_sample_rate=list()
    
    len=length(time_intrest)
    
    for (q in 1:dim(censorset)[1])
    {
      sub_surv=c(1,censor_surv[q,])
      locate=sum(time_intrest<=censorset[q,P+2])+1
      
      con_surv1=sub_surv[locate:length(sub_surv)]
      con_surv2=c(1,con_surv1)-c(con_surv1,0) 
      con_surv=con_surv2[2:length(con_surv2)]
      cond_sample_rate[[q]]=con_surv/sum(con_surv)
      
      cond_sample_set[[q]]=c(time_intrest[time_intrest>censorset[q,P+2]],tao)
    }
  }
  
  Forest_0=Forest
  SurvMat_0=SurvMat
  
  if ((sum(newset[, P+1] == 0) == 0) & (L > 0))
  {
    warning("There is no censored observation to impute, set L to 0")
    L = 0
  }
  
  Forest_seq=list()
  SurvMat_seq=list()
  
  if (L>0)
  {
    for (j in 1:L)
    {
      Forest=matrix(0,3*need,7*M)
      Large=rep(0,M)
      Forestdata=matrix(0,dim(dataset)[1],M)
      SurvMat=list()
      
      for (i in 1:M) #fit M trees after imputation
      {
        newobtime=rep(0,dim(censorset)[1])
        newobtime2=rep(0,dim(censorset)[1])
        
        for (q in 1:dim(censorset)[1])
        {
          samplelong=length(cond_sample_rate[[q]])
          if (cond_sample_rate[[q]][samplelong]==1)
          {
            newobtime[q]=tao
            newobtime2[q]=tao
          }else
          {newsample=rmultinom(1,size=1, prob= cond_sample_rate[[q]])
          newobtime[q]=cond_sample_set[[q]][newsample==1]
          
          samplerate=cond_sample_rate[[q]][1:(samplelong-1)]/sum(cond_sample_rate[[q]][1:(samplelong-1)])
          exptime=sum(cond_sample_set[[q]][1:(samplelong-1)]*samplerate)
          newsample2=rbinom(1, 1, prob=cond_sample_rate[[q]][samplelong])
          newobtime2[q]=tao*(newsample2==1)+exptime*(newsample2==0)
          }
        }
        
        if (impute=="random")
        {
          newcensorind=1-(newobtime==tao)
          newset[censornum,P+1]=newcensorind
          newset[censornum,P+2]=newobtime
        }
        
        if (impute=="expected")
        {
          newcensorind=1-(newobtime2==tao)
          newset[censornum,P+1]=newcensorind
          newset[censornum,P+2]=newobtime2
        }
        
        Treefit=Fit_ERT_Tree(newset,dataset, K, nmin, SupLogRank, Step=1 ,tao)
        Forest[1:dim(Treefit$Tree)[1],(7*(i-1)+1):(7*i)]=Treefit$Tree
        Large[i]=dim(Treefit$Tree)[1]
        Forestdata[,i]=Treefit$UpdateData[,dim(dataset)[2]]
        SurvMat[[i]]=Treefit$Survival_All
      }
      
      censor_surv = Muti_ERT_Predict(censorset, Forest, SurvMat, time_intrest)$Surv_predict
      
      cond_sample_set=list()
      cond_sample_rate=list()
      
      len=length(time_intrest)
      
      for (q in 1:dim(censorset)[1])
      {
        sub_surv=c(1,censor_surv[q,])
        locate=sum(time_intrest<=censorset[q,P+2])+1
        
        con_surv1=sub_surv[locate:length(sub_surv)]
        con_surv2=c(1,con_surv1)-c(con_surv1,0) 
        con_surv=con_surv2[2:length(con_surv2)]
        cond_sample_rate[[q]]=con_surv/sum(con_surv)
        
        cond_sample_set[[q]]=c(time_intrest[time_intrest>censorset[q,P+2]],tao)
      }
      
      Forest_seq[[j]]=Forest
      SurvMat_seq[[j]]=SurvMat
    }
  }
  
  return(list("time_intrest"=time_intrest,
              "Forest_seq"=Forest_seq,
              "SurvMat_seq"=SurvMat_seq,
              "Forest_0"=Forest_0,
              "SurvMat_0"=SurvMat_0))
  
}




Fit_Surv_Tree <- function(x, y, censor, K, nmin, split)  #nmin is the minimal number of observed data 
{
  N = nrow(x)
  P = ncol(x)
  
  One_Tree = matrix(NA, 2*data_n/nmin,7)        # Global variable to save the tree
  
  BookingTable[1,1] <<- 1
  P <<- dim(dataset)[2]-2
  Tao <<- tao
  obs <- seq(1,dim(dataset)[1])
  rate <- rep(1, dim(dataset)[1])
  
  time_points <<- sort(dataset2[dataset2[, (P+1)]==1, (P+2) ])
  Survival_All <<- matrix(0, 2*data_n/nmin , length(time_points))
  
  dataset <- cbind(dataset,obs)
  backset <<- cbind(dataset2,obs,rate)
  
  Further_Split_A_Node(dataset, K, nmin, 1, SupLogRank, Step)
  
  output <- list()
  output$Tree <- One_Tree[BookingTable[,1]==1,]
  output$UpdateData <- backset
  output$Survival_All <- Survival_All[BookingTable[,1]==1,]
  out <- output
}






