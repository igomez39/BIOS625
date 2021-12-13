library(ggplot2)

#Run on cluster after array
ests_MI=matrix(0,100,14)
ses_MI=matrix(0,100,14)

for(i in 1:100) {
  ests_MI[i,]=load(paste("beta",i,".Rdata",sep=""))
  ses_MI[i,]=load(paste("se",i,".Rdata",sep=""))
}

ests=apply(ests_MI,2,mean)
ses=sqrt(apply(ses_MI^2,2,mean)+(1+1/100)*apply(ests_MI,2,var))
write.csv(ests,"GLS.csv")
write.csv(ses,"GLS_SE.csv")

#Run on local computer to make plot
setwd("//Users//nicholashartman//Downloads")
GLS=read.csv("GLS.csv")[,-1]
GLS_SE=read.csv("GLS_SE.csv")[,-1]
low=GLS-1.96*GLS_SE
up=GLS+1.96*GLS_SE
label=c("Intercept","Teacher Education","Time","Parent Education","Suburb","Town","Rural",
            "Private School","Teacher Education x Time","Parent Education x Time","Suburb x Time",
            "Town x Time","Rural x Time","Private School x Time")
GLS_data=data.frame(GLS,low,up,label)
GLS_data=GLS_data[14:2,]
GLS_data=GLS_data[c(1:11,13,12),]
GLS_data$label_num=c(1:13)
g=ggplot(GLS_data)+geom_point(aes(x=label_num,y=GLS))+coord_flip()
g=g+geom_hline(yintercept=0,col="green")+theme_classic()
g=g+geom_errorbar(aes(x=label_num,ymin=low,ymax=up))
g=g+scale_x_continuous(breaks=GLS_data$label_num,label=GLS_data$label)
g=g+ylab("Estimate")+xlab("")
g


