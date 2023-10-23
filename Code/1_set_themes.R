#load packages
library(tidyverse)
library(scales)
library(ggplot2)
library(brms)
library(vegan)
library(sjPlot)
library(ggrepel)
library(concaveman)
library(ggforce)
library(viridis)

#set theme for ggplot
themes <-  theme(panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(size = 0.5, linetype = "solid",
                                          colour = "black"),
                 axis.text= element_text(size=20),
                 axis.title = element_text(size =20),
                 legend.text=element_text(size=10))

