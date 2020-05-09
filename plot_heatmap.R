library(ggplot2)
library(reshape2)
library(ggthemes)

rm(list = ls())
data <- read.table("heatmap_data.txt", header = T, sep = "\t")
data.m <- melt(data)

data.m$Gene <- factor(data.m$Gene,levels=rev(c("LPB1","MPA1","MPA2","PHO1","PHOD","PHOX","PSR1","PTA1",
                                               "PTA2","PTA3","PTA4")))
data.m$variable <- factor(data.m$variable,levels=c("WT","Mutant"))
data.m$Treat <- factor(data.m$Treat,levels=c("With P","No P"))

ggplot(data.m, aes(variable, Gene)) + 
  facet_grid( .~ Treat)+
  geom_tile(aes(fill = value),colour = "white") +
  scale_fill_gradient2('log2(TPM)',low = "blue", high = "red", mid = 'white')+
  labs(x = "", y = "")+
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 45)) +
  coord_fixed(ratio=1)

ggsave(file = "heatmap.pdf", height = 6, width = 3, dpi = 300, device = "pdf")
