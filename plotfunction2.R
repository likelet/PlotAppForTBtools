tbtoolsCounts<-read.table("GO.level.2.counts",header=T,sep="\t")
library(ggplot2)
library(ggthemes)
head(tbtoolsCounts)
GeneCount<-11578 # 所有有GO注释的基因个数

tbtoolsCounts<-tbtoolsCounts[order(tbtoolsCounts$Counts,decreasing=F),]
tbtoolsCounts<-tbtoolsCounts[order(tbtoolsCounts$Class,decreasing=T),]

# tbtoolsCounts<-tbtoolsCounts[order(tbtoolsCounts$Class,tbtoolsCounts$Counts,decreasing=F),]
str(tbtoolsCounts)
tbtoolsCounts$Description<-factor(tbtoolsCounts$Description,unique(as.character(tbtoolsCounts$Description)))
names(table(tbtoolsCounts$Class))

###########
#  横向图
#########

p<-ggplot(tbtoolsCounts)
p+geom_bar(aes(x=Description,y=Counts/GeneCount*100,fill=Class),stat="identity")+
  # geom_text(aes(x=Description,y=(Counts+max(tbtoolsCounts$Counts)*0.06)/GeneCount*100,label=Counts))+
  # ylim(0,max(tbtoolsCounts$Counts)*1.08)+
  # geom_line(aes(x=Description,group=1,color=Class),size=5,y=max(tbtoolsCounts$Counts*1.2))+
  
  #geom_text(aes(x=sum(tbtoolsCounts$Class==Class)/4*1,label=Class),y=max(tbtoolsCounts$Counts*1.2))+
  geom_text(aes(x=Description,y=(Counts+max(tbtoolsCounts$Counts)*0.06)/GeneCount*100,label=Counts))+
  
  coord_flip()+theme_bw()+theme(
    # legend.position="none",
    # axis.text.x = element_text(size=10),
    # axis.line=element_line(size=0.5,linetype="dashed"),
    # axis.ticks=element_blank(),
    legend.position = c(.8, .1),
    panel.grid.major=element_blank() ,
    panel.grid.minor=element_blank() ,
    panel.background=element_blank() # ,
    
    # panel.border=element_rect(linetype="dashed")
  )+
  scale_fill_stata()+
  xlab("GO Decription")+ylab("Percent (%)")
###############################
ggsave("GO.pdf",width=8,height=9)
ggsave("GO.tiff",width=8,height=9)
