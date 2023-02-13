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
