# https://www.jianshu.com/p/c3e21bb5e1ec
# https://github.com/gaospecial/ccgraph
# https://blog.csdn.net/qq_27390023/article/details/125655983

#libraries 
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


