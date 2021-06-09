library(ggplot2)
library(ggthemes)
library(ggrepel)

windowsFonts(CA = windowsFont("Calibri"))


deg <- read.table("volcano_plot.txt", sep = "\t", header = T)

deg$Change <- as.factor(ifelse(deg$pvalue < 0.05 & deg$log2FoldChange	 > 1, 'Up', 
                               ifelse(deg$pvalue < 0.05 & deg$log2FoldChange < -1, 'Down', 'Normal')))
deg_tag <- subset(deg, abs(log2FoldChange)>2 & pvalue<1E-15)

ggplot(deg, aes(x = log2FoldChange, y = -log10(pvalue)))+
  geom_point(data = deg_tag, size=2, color = "black") +
  geom_point(alpha = 0.7,aes(color = Change)) +
  #scale_colour_brewer(palette = "Set1")+
  scale_color_manual(values = c("#008CD6","gray", "#E50044"))+
  geom_hline(yintercept = -log10(0.05), linetype = 3) +
  geom_vline(xintercept = c(-1,1), linetype = 3) +
  geom_text_repel(data = deg_tag, aes(x = log2FoldChange, y = -log10(pvalue), label = gene_id), 
                  color = "black", fontface = "italic",show.legend = FALSE)
labs(x=expression(paste(log[2], "(", "Fold change", ")", sep="")),
     y=expression(paste(-log[10], "(", italic(P), " ", value, ")", sep="")))+
  theme_bw()
ggsave(filename = "ARF3_data.pdf", heigh = 6, width =6, device = "pdf")


#scale_color_manual(values =c("Up" = "red", "Down" = "blue", "Normal" = "grey"))
