# Ploting_scripts


R scripts for image ploting. 

- [Scripts](#scripts)
    + [1. Plot blocks](#1-plot-blocks)
    + [2. Plot stacked histogram with error bars](#2-plot-stacked-histogram-with-error-bars)
    + [3. Plot multiple ordinates](#3-plot-multiple-ordinates)
    + [4. Plot broken axis](#4-plot-broken-axis)
    + [5. Plot pie and donut](#5-plot-pie-and-donut)
    + [6. Plot heatmap](#6-plot-heatmap)
    + [7. Plot volcano diagram](#7-plot-volcano-diagram)
    + [8. Plot barplot with scatter](#8-plot-barplot-with-scatter)
    + [9. Plot circular barplot](#9-plot-circular-barplot)
    + [10. Plot flat violin with scatter](#10-plot-flat-violin-with-scatter)
    + [11. Plot interaction](#11-plot-interaction)
---

# Scripts
### 1. Plot blocks

`plot.blocks.R` Plot blocks (genome segments, centromere region or gene location)
 
```
# Prepare input data
perl block2draw.pl -t 5 -d 5 \
    -s samples.list \ # sorted sample list
    -b blocks.csv \ # block file
    -g gene.bed \ # gene info, optional
    -c centromere.bed \ # centromere position info, optional
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

### 2. Plot stacked histogram with error bars
 
`stacked_histogram.error_bar.R` Plot stacked histogram with error bars and compare two sets of them. (Used in my paper [Jia et al., 2020](http://www.sciencedirect.com/science/article/pii/S0176161720300298))
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

### 3. Plot multiple ordinates

`plot_multi_ordinates.R` Plot multiple ordinates image. (Used in my paper [Jia et al., 2019](https://onlinelibrary.wiley.com/doi/abs/10.1111/tpj.14154))

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

### 4. Plot broken axis
`plot_break_axis.R` Plot histogram with break y axis.
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

---

### 5. Plot pie and donut
`plot_pie_donut.R`
```
 library(ggplot2)
 data <- data.frame(category = c('A','B','C','D','E'), 
                    count = c(10,23,8,33,12))
 data$category <- factor(data$category,levels=rev(c('A','B','C','D','E')))

 # Calculate proportion
 data$fraction <- round(data$count / sum(data$count), 2) * 100

 # Compute the cumulative percentages (top of each rectangle)
 data$ymax <- cumsum(data$fraction)

 # Compute the bottom of each rectangle
 data$ymin <- c(0, head(data$ymax, n=-1))

 # Compute label position
 data$labelPosition <- data$fraction/2 + data$ymin

 # Generate labels
 data$label <- paste0(data$fraction, "%")

 # Define blank theme
 blank_theme <- theme_minimal()+
   theme(
     axis.title.x = element_blank(),
     axis.title.y = element_blank(),
     axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     panel.border = element_blank(),
     panel.grid=element_blank(),
     axis.ticks = element_blank(),
     plot.title=element_text(size=14, face="bold")
   )

 # Pie and donut ploting
 ggplot(data=data, mapping=aes(x=2,y=fraction,fill=category))+
   geom_bar(stat="identity",color="white", width =1, linetype=1,size=1)+
   geom_text(x=2, aes(y=labelPosition, label=label), size=4)+
   blank_theme+
   coord_polar(theta = "y")+
   xlim(1.5, 2.5) # Pie plot
   # xlim(1.5, 2.5) # Donut plot

 # Note: An alternative way to plot pie or donut is by geom_rect() and coord_polar().
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/Pie_plot.png" div align = "center" width="30%" height="30%" /><img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/donut_plot.png" div align = "center" width="60%" height="60%" />

---

### 6. Plot heatmap
`plot_heatmap.R`
```
 library(ggplot2)
 library(reshape2)
 library(ggthemes)

 rm(list = ls())
 data <- read.table("heatmap_data.txt", header = T, sep = "\t")
 data.m <- melt(data)

 data.m$Gene <- factor(data.m$Gene,levels=rev(c("A","B","C","D","E","F","G","H","I","J","K")))
 data.m$variable <- factor(data.m$variable,levels=c("WT","Mutant"))
 data.m$Treat <- factor(data.m$Treat,levels=c("With N","No N"))

 ggplot(data.m, aes(variable, Gene)) + 
   facet_grid( .~ Treat)+
   geom_tile(aes(fill = value),colour = "white") +
   scale_fill_gradient2('log2(TPM)',low = "blue", high = "red", mid = 'white')+
   labs(x = "", y = "")+
   theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 45)) +
   coord_fixed(ratio=1)

 ggsave(file = "heatmap.pdf", height = 6, width = 3, dpi = 300, device = "pdf")
 
 # Note: we can also add more theme modifications to beautify the final image, like, coord_flip().
 ```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/heatmap.png" div align = "center" width="25%" height="25%" />


---

### 7. Plot volcano diagram
`plot_volcano_diagram.R`
```
library(ggplot2)
library(ggthemes)
library(ggrepel)

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
                  color = "black", fontface = "italic",show.legend = FALSE)+
  labs(x=expression(paste(log[2], "(", "Fold change", ")", sep="")),
       y=expression(paste(-log[10], "(", italic(P), " ", value, ")", sep="")))+
  theme_bw()
ggsave(filename = "ARF3_data.pdf", heigh = 6, width =6, device = "pdf")
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/volcano_plot.jpg" div align = "center" width="40%" height="40%" />

---

### 8. Plot barplot with scatter

`Plot_barplot_scatter.R` [Jia et al., 2023](https://www.cell.com/molecular-plant/abstract/S1674-2052(22)00437-3)
```
library(Rmisc)
library(tidyverse)
library(ggthemes)
library(reshape2)
library(ggsci)
library(ggpubr)

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
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/Plot_barplot_scatter.png" div align = "center" width="40%" height="40%" />

---

### 9. Plot circular barplot
`Plot_circular_barplot.R`[Jia et al., 2023](https://www.cell.com/molecular-plant/abstract/S1674-2052(22)00437-3)
```
library(tidyverse)
library(ggsci)

rm(list=ls())
data <- read.table("Plot_circular_barplot.txt", sep = "\t", header = T)

data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
empty_bar=2
base_data <- data %>% 
  group_by(Group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
ggplot(data, aes(x=as.factor(id), y=Average, fill=Treatment)) +
  geom_bar(aes(x=as.factor(id), y=Average, fill=Treatment), stat="identity") +
  geom_errorbar(aes(x=as.factor(id), ymin=Average, ymax=Average+SD), width=.25, size=0.4) +
  #geom_hline(yintercept = c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75))+
  ylim(-1.5, 2.5) +
  theme_minimal() +
  #scale_fill_lancet()+
  theme(
    #legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_segment(data=base_data, aes(x = start, y = -0.05, xend = end, yend = -0.05), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -0.1, label=Group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

ggsave(filename = "Plot_circular_barplot.pdf", heigh = 2.5, width = 3.2, device = "pdf")
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/Plot_circular_barplot.png" div align = "center" width="30%" height="30%" />

---

### 10. Plot flat violin with scatter
`Plot_flat_violin_scatter.R`
```
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
  ylab("") +
  xlab("")+
  theme_few() +
  scale_fill_lancet()+
  scale_color_lancet()+
  theme(strip.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(hjust = 0.4, color = "black", vjust = 0.5),
        axis.text.y = element_text(color = "black"),
        legend.position="none")
 ggsave(file = "Plot_flat_violin_scatter.pdf", height = 2.5, width = 1.8, dpi = 300, device = "pdf")
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/Plot_flat_violin_scatter.png" div align = "center" width="20%" height="20%" />

---

### 11. Plot interaction
`Plot_interaction.R`
```
library(ggraph)
library(igraph)
library(RColorBrewer)

edges <- read.table("Plot_interaction_edges.txt", header = T, sep = "\t")
vertices <- read.table("Plot_interaction_vertices.txt", header = T, sep = "\t")

mygraph<-graph_from_data_frame(edges,vertices=vertices)

ggraph(mygraph,layout='dendrogram',circular=T)+
  geom_edge_diagonal(colour="grey")+
  geom_node_point(aes(filter=leaf, size=value, color=group),alpha=0.6)+
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.2, y=y*1.2, filter = leaf, label=name, 
                     angle = -((-node_angle(x,y) + 90) %% 180) + 90, colour=group), 
                 position = "identity", size=2.7) +
  scale_colour_manual(values= rep(brewer.pal(9, "Paired") , 30)) +
  theme_void()+
  coord_fixed()

ggsave(file = "Plot_interaction.pdf", height = 6, width = 6, dpi = 300, device = "pdf")
```
<img src="https://github.com/jiaxianqing/Ploting_scripts/blob/master/examples/Plot_interaction.png" div align = "center" width="40%" height="40%" />

---

P.S. 
We all love ggplot2! A great tutorial for ggplot2: [Top50-Ggplot2-Visualizations](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html).

Xianqing Jia, jiaxq.nju@gmail.com

Updated: 2023-02-13
