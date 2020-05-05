#!plot CO(line), TE number(line), markers(bar)
library("ggplot2") # for the plot
library("ggrepel") # for spreading text labels on the plot
library("reshape2")
rm(list=ls())

data_sum <- read.table("summary.all.20180919.txt", header = T, sep = "\t")
data_mod <- melt(data_sum[,c(1,2,3,4,5)], id.vars=c("chr", "start", "end"), variable.name = "co_marker", value.name = "num")
centrome <- read.table("centromere.bed", sep = "\t", col.names = c("chr", "start", "end"))
sd1 <- read.table("SD1.bed", sep = "\t", col.names = c("chr", "start", "end", "name"))

ggplot(data=data_sum) +
  facet_grid(chr~.)+
  geom_area(data =data_sum, aes(x=(start + end)/2/1000000, y = te_prop),color = "#FFCC33", fill = "#FFCC33", stat = "identity")+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


pdf(file="output25.pdf", width = 12, height = 16)
ggplot(data=data_mod) + 
  geom_rect(data = sd1, aes(xmin=start/1000000, xmax=end/1000000, ymax=30, ymin=0), fill="#6600FF")+
  geom_area(data =data_sum, aes(x=(start + end)/2/1000000, y = te_prop*30),color = "#FFCC33", fill = "#FFCC33", stat = "identity")+
  geom_line(aes(x = (start + end)/2/1000000, y = num, color = co_marker), size = 0.5) + 
  #geom_line(aes(x = (start + end)/2/1000000, y = marker_den, color = "#085A9C"), size = 0.5) + 
  geom_rect(data = centrome, aes(xmin=start/1000000, xmax=end/1000000, ymax=30, ymin=0), fill="grey")+
  geom_text_repel(data = sd1, aes(x = (start + end)/2/1000000, y = 30, label = name), 
                  color = "black", fontface = "italic",show.legend = FALSE) +
  scale_fill_discrete(guide=FALSE)+
  scale_y_continuous(name = expression("Numbers"), sec.axis = sec_axis(~. * 1/ 30 , name = "Transposon proportion"), limits = c(0, 30)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0, max((data_sum$start + data_sum$end)/2/1000000), 5)) +
  facet_grid(chr~.)+
  xlab("Length(Mbp)")+
  scale_colour_manual(values=c("#EF0808","#085A9C")) +
  #scale_x_continuous(labels = comma) +
  theme_bw() + 
  theme(legend.position="none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text=element_text(face="bold",size=rel(1.5)),
        axis.text = element_text(face = 'bold',size = rel(1.5),colour="black"))+
  labs(title = "COs, TEs and markers distribution along chromosomes")
dev.off()
