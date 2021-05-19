library(psych)
env<-read.csv("F:/jenv.csv",row.names=1)
head(env)
env1<-env[,c(1:9)]
env2<-env[,c(10:19)]
env3<-env[,c(20:26)]
env4<-env[,c(27:30)]
env.spearman<-corr.test(env3,env4,method="pearson")

env.spearman


