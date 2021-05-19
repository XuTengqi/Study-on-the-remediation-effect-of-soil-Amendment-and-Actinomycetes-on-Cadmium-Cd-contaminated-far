library(vegan)
library(ggplot2)
env<-read.csv("F:/132/env.csv",header=T,row=1)
env.sc<-env[,1:11]


summary(env.sc)

env.pca <- rda(env.sc, scale=TRUE)
env.pca 
summary(env.pca) 

si<-scores(env.pca)$sites  
sp<-scores(env.pca)$species  

##ggplot
factor<-read.csv("F:/132/factor.csv",row.names=1)
factor$treatment<-factor(factor$treatment,levels=c("CK","B","F","L","A","AB","AF","AL"))#设置处理的顺序
factor$AC<-factor(factor$AC,levels=c("NI","A"))#设置处理的顺序
factor$AM<-factor(factor$AM,levels=c("CK","B","F","L"))#设置处理的顺序
treatment<-factor[,1]
Ac<-factor[,2]
Am<-factor[,3]
col<-c("red","blue","green","orange")
pch<-c(16,17,15,20)###pch<-c(16,17,15,18,20)
write.csv(si,"F:/pcaenv1.csv")
si1<- read.csv("F:/pcaenv1.csv",row.names=1)


si1<-as.data.frame(si1)
a<-ggplot(si1,aes(x=PC1,y=PC2))+geom_point(size=4.5,alpha=0.9,aes(shape=env$AC,color=env$AM))+
theme_classic()+scale_color_manual(values=col)+
scale_shape_manual(values=pch)+
geom_vline(xintercept = 0,linetype=2,alpha= 0.50)+
geom_hline(yintercept = 0,linetype=2,alpha= 0.50)+
theme(text=element_text(size=12),panel.grid=element_blank(),
panel.background=element_rect(fill='transparent', color='black')
,panel.border=element_rect(fill='transparent', color='transparent'))
labs(x="PC1 51.29%",y="PC2 19.03%")
write.csv(sp,"F:/pcaenv2.csv")
pc<- read.csv("F:/pcaenv2.csv")
pc<-data.frame(pc)
 a+geom_segment(data =pc,aes(xend = pc[ ,2],yend=pc[ ,3]), x=0,y=0, colour="black",arrow=arrow(angle=30, length=unit(0.25, "cm")))+
  annotate("text",label="pH",color="black",-0.001538395,1.03053762

,size=5)+
  annotate("text",label="NH4+",color="black",1.088446283,0.05887309

,size=5)+
  annotate("text",label="NO3-",color="black",-0.763116763,-0.50493407
,size=5)+
  annotate("text",label="TN",color="black",1.138720685,-0.0629141

,size=5)+
  annotate("text",label="AP",color="black",1.137525347,0.03532678

,size=5)+
  annotate("text",label="AK",color="black",1.127670214,-0.24356236

,size=5)+
  annotate("text",label="OM",color="black",0.911416813,-0.62846057

,size=5)+
  annotate("text",label="CEC",color="black",0.670003339,0.17251072

,size=5)+
  annotate("text",label="TCd",color="black",-0.784705769,-0.56389263

,size=5)+
  annotate("text",label="DTPA-Cd",color="black",-0.612968606,-0.72417034

,size=5)
  annotate("text",label="TC",color="black",0.995800906,-0.57328797

,size=5)


bray1<-vegdist(scale(env.sc),"euclid")
adonis(bray1~Ac)
adonis(bray1~Am)
adonis(bray1~Treatment)
anosim(scale(env.sc),Ac)
anosim(scale(env.sc),Am)
anosim(scale(env.sc),treatment)

mantel(bray1,bray2,method="spearman")

