
library(data.table)
library(nlme)

############################
#Script to run in batch mode
############################
i=Sys.getenv("SLURM_ARRAY_TASK_ID") 
dat=as.data.frame(fread(paste("MI",i,".csv",sep=""),sep=",")) #Read imputed dataset
dat$Child_ID=factor(dat$Child_ID)
dat$Teacher_Education=as.numeric(Teacher_Education)
g=gls(Math_Score~Teacher_Education*Time
                 +Parent1_Education*Time+factor(Locale)*Time+factor(Pub_Pri)*Time,data=dat,
                  correlation=corSymm(form=~1|Child_ID)) #GLS with unstructured covariance matrix
beta=coef(g)
se=sqrt(diag(vcov(g)))
save(beta,file=paste("beta",i,".Rdata",sep=""))
save(se,file=paste("se",i,".Rdata",sep=""))





