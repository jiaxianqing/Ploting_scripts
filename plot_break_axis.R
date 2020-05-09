rm(list=ls())

library(Rmisc)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(ggsci)
library(ggpubr)
##############################################################################################

rm(list=ls())
df <- read.table("plot_break_axis.txt", header = T, sep = "\t")
df.mod <- melt(df, id.vars = c('Type',"Repeat"), variable.name = 'Treat', value.name = 'Value')
df.mod.se <- summarySE(df.mod,  measurevar="Value", groupvars=c("Type","Treat"))

p1 <- ggplot(df.mod.se,aes(x=Treat,y=Value,fill=Type)) +
  geom_bar(position="dodge", stat="identity",width=.8, size=.25) + 
  geom_errorbar(aes(ymin=Value-se, ymax=Value+se), position=position_dodge(0.8), width=.25, size=0.5) +
  labs(x=NULL,y=NULL,fill=NULL)+
  theme_few()+
  guides(fill=FALSE)+
  scale_fill_lancet()+
  coord_cartesian(ylim = c(0,4))+
  scale_y_continuous(breaks = seq(0,4,1))

p2 <- ggplot(df.mod.se,aes(x=Treat,y=Value,fill=Type)) +
  geom_bar(position="dodge", stat="identity",width=.8, size=.25) + 
  geom_errorbar(aes(ymin=Value-se, ymax=Value+se), position=position_dodge(0.8), width=.25, size=0.5) +
  labs(x=NULL,y=NULL,fill=NULL) +
  theme_few()+
  guides(fill=FALSE)+
  scale_fill_lancet()+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) +
  coord_cartesian(ylim = c(15,45)) +
  scale_y_continuous(breaks = seq(15,45,10))
ggarrange(p2,p1,heights=c(2/5, 3/5),ncol = 1, nrow = 2,common.legend = TRUE,legend="right",align = "v") 
ggsave(file = "plot_break_axis.pdf", height = 3.2, width = 3, dpi = 300, device = "pdf")
