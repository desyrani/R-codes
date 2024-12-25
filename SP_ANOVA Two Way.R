install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
crop.data <- read.csv("cropdata.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
summary(crop.data)
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)
interaction <- aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction)
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)
summary(blocking)

library(AICcmodavg)

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way

tukey.plot.aov<-aov(yield ~ fertilizer:density, data=crop.data)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(
    yield = mean(yield)
  )

mean.yield.data$group <- c("a","b","b","b","b","c")

mean.yield.data

two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot

two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))

two.way.plot

two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)

two.way.plot


two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")

two.way.plot




