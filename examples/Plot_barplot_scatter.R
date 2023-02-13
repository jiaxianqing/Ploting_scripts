library(Rmisc)
library(tidyverse)
library(ggthemes)
library(reshape2)
library(ggsci)
library(ggpubr)
##############################################################################################

rm(list=ls())
df <- read.table("Plot_barplot_scatter.txt", header = T, sep = "\t")

df.mod <- melt(df, id.vars = c('Type', 'Gene', "Rep"), variable.name = 'Treat', value.name = 'Value')

df.se <- summarySE(df.mod,  measurevar="Value", groupvars=c("Type", "Gene", "Treat"))
write.table(df.se, file = "Figure 2e.data.se.txt", sep="\t", quote=F, row.names=F, col.names=T)
df.se <- read.table(pipe("pbpaste"), header = T, sep = "\t")

ggplot(df.se, aes(x=Treat, y=Value, fill=Treat)) +
  facet_grid( .~ Gene+Type, scales = "free_x",space="free_x")+
  geom_bar(aes(x=Treat, y=Value, fill=Treat), stat="identity", position="dodge", width=.7, size=.25) +
  geom_point(data=df.mod, aes(Treat, Value), position=position_dodge(0.9), color = "#666666", shape = 1, alpha=1, size = 1.5)+
  geom_errorbar(data=df.se,aes(x=Treat, ymin=Value, ymax=Value+se), position=position_dodge(0.9), width=.25, size=0.4) +
  theme_few()+
  ylab("Relative promoter activity") +
  xlab(NULL)+
  theme(strip.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle = 45, hjust = 0.4, color = "black", vjust = 0.5),
        axis.text.y = element_text(color = "black"),
        legend.position="none")
ggsave(file = "Plot_barplot_scatter.pdf", height = 3, width =3.8, dpi = 300, device = "pdf")

