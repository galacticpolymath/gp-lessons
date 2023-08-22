#penguinplots
require(pacman)
p_load(palmerpenguins,tidyverse,patchwork)
head(penguins)
source("scripts/dotplot.R")


# For Day 2 quick review of data visuals ----------------------------------
#scatter plot
S<-penguins %>% filter(species=="Adelie") %>% ggplot(aes(x=flipper_length_mm,y=bill_length_mm))+geom_point(fill="#ff3dac",col="#363636" ,pch=21,size=2)+ggGalactic()+xlab("penguin flipper length (mm)")+ylab("penguin bill length (mm)")
S
ggsave("assets/penguinPlots_scatter-plot_flipper-length.jpg")

#histogram
H<-penguins %>% filter(species=="Adelie") %>% ggplot(aes(x=flipper_length_mm))+geom_histogram(bins=10,fill="#ff3dac",col="#363636" )+ggGalactic()+xlab("penguin flipper length (mm)")
H
ggsave("assets/penguinPlots_histogram_flipper-length.jpg")
#unbinned dot plot
H2<-dotplot(H,size=2)
H2
ggsave("assets/penguinPlots_unbinned-dotplot_flipper-length.jpg")

#binned dot plot
H3 <- dotplot(H,size=2,binned=T)
H3
ggsave("assets/penguinPlots_binned-dotplot_flipper-length.jpg")

#combined plot
(H+H2)/(H3+S)
ggsave("assets/penguinPlots_all-visualizations.jpg",width=16,height=16)
