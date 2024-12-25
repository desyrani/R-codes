
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
crop.data <- read.csv("C:/Users/khali/Downloads/cropdata.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
summary(crop.data)
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)
tuckey.oneway <- TukeyHSD(one.way)
tuckey.oneway

one.way <- aov(yield ~ density, data = crop.data)
summary(one.way)
tuckey.oneway <- TukeyHSD(one.way)
tuckey.oneway

one.way <- aov(yield ~ block , data = crop.data)
summary(one.way)
tuckey.oneway <- TukeyHSD(one.way)
tuckey.oneway

