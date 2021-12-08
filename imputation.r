library(mice)
data_long_org=read.csv("Data_Long_Format.csv")
id_Pub_rm=which(data_long_org$Pub_Pri==-1)
data_long=data_long_org[-id_Pub_rm,]
data_long=data_long_org[,-c(1,5)]
data_long=data_long[,-6]


id_te_9=which(data_long$Teacher_Education==-9)
id_pa1_9=which(data_long$Parent1_Education==-9)
data_long$Teacher_Education[id_te_9]=NA
data_long$Parent1_Education[id_pa1_9]=NA

impmethod <- character(ncol(data_long))
names(impmethod) <- colnames(data_long)
impmethod[c("Teacher_Education")]=c("2l.norm")
impmethod[c("Parent1_Education")]=c("2l.norm")
impmethod[c("Locale")]=c("2l.norm")
impmethod[c("Math_Score")]=c("2l.norm")
impmethod[c("Pub_Pri")]=c("2l.bin")
pm <- make.predictorMatrix(data_long)
pm[,"School_ID"]=0
pm["Teacher_Education",]=c(-2,0,2,0,1,1,1,1)
pm["Parent1_Education",]=c(-2,0,2,1,0,1,1,1)
pm["Pub_Pri",]=c(-2,0,2,1,1,0,1,1)
pm["Locale",]=c(-2,0,2,1,1,1,0,1)
pm["Math_Score",]=c(-2,0,2,1,1,1,1,0)

res.mice.md <- mice(data_long, m=1, predictorMatrix = pm,
                    method=impmethod, maxit=10)
data_pred=complete(res.mice.md)

data_pred[,-ncol(data_pred)]=round(data_pred[,-ncol(data_pred)])
data_pred$Dates=data_long_org$Dates

write.csv(data_pred,"Imputed_Data.csv")


