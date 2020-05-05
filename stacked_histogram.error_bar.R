library(lattice)
library(plyr)
library(ggplot2)
library(Rmisc)
library(ggthemes)
library(tidyverse)

node.final <- read.table("node_stats.se.ac.txt", header=T, sep = "\t")
node.final$Varieties <- factor(node.final$Varieties,levels=c('SLX','CN422','ZN4','AZZ','AJNT','IR36','MH63', 
                                                             'HHZ','93-11','PA64s','H1','H2','H3'))
node.final$Noden <- factor(node.final$Noden,levels=c('Node6','Node5','Node4','Node3','Node2','Node1'))

ggplot(node.final, aes(x=Varieties, y=Length, fill=Noden)) + 
  geom_bar(stat="identity",
           colour="black",# Use black outlines,
           size=.25) + 
  ylab("Node length")+
  xlab("")+
  geom_errorbar(aes(ymin=Ac_len-se, ymax=Ac_len+se), width=.15) + # Width of the error bars
  facet_grid( .~ Group, scales = "free_x",space="free_x") +
  theme(strip.text=element_text(face="bold",size=rel(1)),
        panel.border = element_blank(),
        axis.text = element_text(face = 'bold',size = rel(1),colour="black"),
        axis.text.x = element_text(angle = 45, hjust = 0.3, vjust = 0.5),
        axis.title = element_text(face = 'bold',size = rel(1)),
        axis.line.x=element_line(colour="black"),
        strip.background=element_rect(fill="#CFCFCF"),
        panel.spacing=unit(0.5,"cm"),panel.background=element_rect(fill="#EBEBEB"))

ggsave(filename = "Nodes.length.pdf", dpi = 300, device = "pdf")




################################################################################
#compare 9311 and H1
node.c <- read.table("9311.H1.txt", header=T, sep = "\t")
node.c$Varieties <- factor(node.c$Varieties,levels=c('93-11','H1'))
node.c$Noden <- factor(node.c$Noden,levels=c('Node6','Node5','Node4','Node3','Node2','Node1'))

length <- read.table("9311.H1.ac_len.txt", header=T, sep = "\t")

ggplot(node.c, aes(x=Varieties, y=Length, fill=Noden)) + 
  geom_bar(stat="identity", colour="black", width = 0.5) + 
  geom_errorbar(aes(ymin=Ac_len-se, ymax=Ac_len+se), width=.15)+
  #facet_grid( .~ Group, scales = "free_x",space="free_x")+
  geom_segment(data=length, aes(x=1.25, xend=1.75, y=a1, yend=a2))+
  ylab("")+
  theme(strip.text=element_text(face="bold",size=rel(1.5)),
        panel.border = element_blank(),
        axis.text = element_text(face = 'bold',size = rel(1.5),colour="black"),
        axis.title = element_text(face = 'bold',size = rel(1.5)),
        axis.title.x = element_blank(),
        legend.text = element_text(face = 'bold',size = rel(1)),
        legend.title = element_text(face = 'bold',size = rel(1)),
        axis.line.x=element_line(colour="black"),
        strip.background=element_rect(fill="#CFCFCF"),
        panel.spacing=unit(0.5,"cm"),panel.background=element_rect(fill="#EBEBEB"))
ggsave(filename = "9311.H1.ac_len.pdf", width=3.5, device = "pdf")
