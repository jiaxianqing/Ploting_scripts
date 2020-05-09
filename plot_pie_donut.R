
library(ggplot2)
data <- data.frame(category = c('A','B','C','D','E'), 
                   count = c(10,23,8,33,12))

data$category <- factor(data$category,levels=rev(c('A','B','C','D','E')))
data$fraction <- round(data$count / sum(data$count), 2) * 100

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- data$fraction/2 + data$ymin

# Compute a good label
#data$label <- paste0(data$category, " ", data$fraction, "%")
data$label <- paste0(data$fraction, "%")

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
ggplot(data=data, mapping=aes(x=2,y=fraction,fill=category))+
  geom_bar(stat="identity",color="white", width =1, linetype=1,size=1)+
  geom_text(x=2, aes(y=labelPosition, label=label), size=4)+
  blank_theme+
  coord_polar(theta = "y")+
  #xlim(1.5, 2.5)
  xlim(0, 5)


############################################################
# load library
library(ggplot2)

# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=4) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")
