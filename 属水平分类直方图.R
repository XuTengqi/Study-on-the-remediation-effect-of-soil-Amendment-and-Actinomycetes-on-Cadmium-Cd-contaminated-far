#数据融合
library(reshape2)
a<-read.csv("F:/5.csv")
b<- melt(a, id=("Taxonomy"))
write.csv(b,"F:/6.csv")

#直方图
library(ggplot2)
windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),RMN=windowsFont("Times New Roman"),ARL=windowsFont("Times New Roman"))
c<-read.csv("F:/6.csv",row.names=1)
c$treatment<-factor(c$treatment,levels=c("CK","B","F","L","A","AB","AF","AL"))
c$treatment<-factor(c$treatment,levels = c("NI","A"))
c$Taxonomy<-factor(c$Taxonomy,levels = c("CK","B","F","L"))
c$Taxonomy<-factor(c$Taxonomy,levels = c("Ori","CK"))
c$variable<-factor(c$variable,levels = c("Others","Firmicutes","Latescibacteria","Gemmatimonadetes","Verrucomicrobia","Planctomycetes","Chloroflexi","Actinobacteria","Acidobacteria","Bacteroidetes","Proteobacteria"))
colors<-c("black","orange","cyan","pink","deeppink3","navy", "darkorchid1","gold", "limegreen", "dodgerblue3", "firebrick1")
p<-ggplot(c,aes(x=Taxonomy,y=value,fill=variable))+
geom_bar(position = "fill",stat="identity")+
theme_bw()+facet_grid(.~c$AC)+
scale_fill_manual(values=colors)+
labs(x=" ", y="Relative Abundance",fill="Taxon")+
theme(text=element_text(family="RMN",size=16),
strip.text=element_text(face="bold",size=rel(1)),
axis.text.y=element_text(size=14),
axis.text.x=element_text(size = 16),
axis.ticks.x=element_blank( ), 
legend.title=element_text(face="plain",size=14),
legend.text=element_text(size=12))

p
