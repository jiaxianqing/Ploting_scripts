rm(list=ls())
#########################################################################
input       <- commandArgs(TRUE)[1]
order_file  <- commandArgs(TRUE)[2]
symlist     <- commandArgs(TRUE)[3]
size        <- commandArgs(TRUE)[4]
output      <- commandArgs(TRUE)[5]
#########################################################################

library("ggplot2")
library("ggrepel")

sym <- as.vector(unlist(strsplit(symlist,split = ",")))

isize <- as.vector(unlist(strsplit(size,split = ",")))

mycolour_5<-scale_fill_manual(values=c("#EF0808","#EEAD0E","#085A9C","black","grey"))
#windowsFonts(CA=windowsFont("Calibri"))
chr <- read.table(input,header = T)
chr$Type <- factor(chr$Type,levels=sym)


order <- read.table(order_file, header = F)
or_sam <- as.vector(order$V1)
or_num <- as.vector(order$V2)

#sam_count <- length(or_sam) * 1.4


site <- unique(union(chr$Xmin -1, chr$Xmax))

pdf(file=output, width = as.numeric(isize[1]), height = as.numeric(isize[2]))
p1 <- ggplot() + 
  geom_rect(data=chr, mapping=aes(xmin=Xmin/1000000, xmax=Xmax/1000000, ymin=Ymin, ymax=Ymax, fill=Type), alpha=1) +
  facet_wrap(~Chrom,ncol=1) +
  mycolour_5 +
  scale_x_continuous(expand=c(0,0), limits = c(min(chr$Xmin -1)/1000000, max(chr$Xmax)/1000000)) +
  theme(panel.grid =element_blank(),
        axis.line.x=element_line(colour="black"),
        axis.ticks.y = element_blank()) +
  xlab("Length (Mbp)")+
  ylab("")+
  scale_y_continuous(breaks=or_num, labels=or_sam)
if ("Gene" %in% chr$Type){
  gene <- subset(chr, Type == "Gene")
  p1 <- p1 + 
    geom_text_repel(data = gene, aes(x = (Xmin + Xmax)/2/1000000, y = Ymax, label = Tag), ylim = c(max(chr$Ymax), NA),
                       color = "black", fontface = "italic",show.legend = FALSE)
}
p1
dev.off()
#file.remove(input, order_file)
