# Ploting_scripts
R scripts for image ploting. (To be continued...@20200505)

---

* `plot.blocks.R`  

Plot blocks (genome segments, centromere region or gene location)
 
```
# Prepare input data
perl block2draw.pl -t 5 -d 5 \
    -s samples.list \ # sorted sample list
    -b blocks.csv \ # block file
    -g gene.bed \ # gene info, optional
    -c centromere.bed \ # gene info, optional
    -p test.draw.pos.csv \ # for ploting
     > test.draw.csv # for ploting

# Plot blocks
Rscript plot.blocks.R \
    test.draw.csv \
    test.draw.pos.csv \
    N/N,D/N,D/D,Gene,Centromere \ # legend names in order
    15,100 \ # width and height
    test.draw.pdf
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/plot.block.png"  div align = "center" width="75%" height="75%" />

---

* `stacked_histogram.error_bar.R`
 
 Plot stacked histogram with error bars and compare two sets of them. (Used in my paper [Jia et al., 2020](http://www.sciencedirect.com/science/article/pii/S0176161720300298))
```
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
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/Nodes.length.png" div align = "center" width="50%" height="50%" /><img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/9311.H1.ac_len.png" div align = "center" width="20%" height="20%" />  

---

* `plot_multi_ordinates.R` 

Plot multiple ordinates image. (Used in my paper [Jia et al., 2019](https://onlinelibrary.wiley.com/doi/abs/10.1111/tpj.14154))

```
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


    pdf(file="multi_ordinates.pdf", width = 12, height = 16)
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
```

<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/multi_ordinates.png" div align = "center" width="75%" height="75%" />  

---

* `plot_break_axis.R`

```
library(Rmisc)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(ggsci)
library(ggpubr)

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
```

<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/plot_break_axis.png" div align = "center" width="25%" height="25%" />  

