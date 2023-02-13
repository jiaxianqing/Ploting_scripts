library(tidyverse)
library(ggsci)
library(reshape2)
library(ggthemes)
library(Rmisc)
library(ggsci)
library(ggpubr)
source( './geom_flat_violin.R' )

rm(list=ls())

data <- read.table("Plot_flat_violin_scatter.txt", sep = "\t", header = T)
data.mod <- melt(data, id.vars = c('Area'), variable.name = 'Trait', value.name = 'Value')
data.mod <- data.mod %>% drop_na(Value)

ggplot(data.mod, aes(x=Area, y=Value)) +
  geom_flat_violin(aes(fill=Area),position=position_nudge(x=.1),color="black",size=0.25) +
  geom_jitter(aes(color=Area), width=0.02, size=0.25) +
  geom_boxplot(width=.1, position=position_nudge(x=0.1), fill="white", size=0.25) +
  #coord_flip() + 
  #ggtitle("Under 1mM Pi (48h)") +
  ylab("") +
  xlab("")+
  theme_few() +
  scale_fill_lancet()+
  scale_color_lancet()+
  # scale_y_continuous(breaks=seq(0, 80, by=20))+
  # expand_limits(y=c(0, 80))+
  theme(strip.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.4, color = "black", vjust = 0.5),
        axis.text.y = element_text(color = "black"),
        legend.position="none")
 ggsave(file = "Plot_flat_violin_scatter.pdf", height = 2.5, width = 1.8, dpi = 300, device = "pdf")

