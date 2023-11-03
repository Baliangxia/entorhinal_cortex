library(ggplot2)
library(tidyverse)
library(esquisse)
EC<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/Opal data - Sheet1.csv",header = TRUE)
esquisser(EC)

Raw_EC<- read.csv('AMM20_5DetectionsL2.csv',header = TRUE)


gene1<-ggplot(EC) +
  aes(x = Opal_520) +
  geom_histogram(bins = 50, fill = "green") +
  labs(
    x = "value of Opal 520",
    y = "Counts",
    title = "Raw distribution for Opal 520"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

gene1_log<-ggplot(EC) +
  aes(x = log(Opal_520)) +
  geom_histogram(bins = 50, fill = "green") +
  labs(
    x = "log value of Opal 520",
    y = "Counts",
    title = "Distribution for Opal 520 with log"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))



 FI<-ggplot(EC) +
  aes(x = Distance, y = Opal_520, colour = Class) +
  geom_point(shape = "circle", size = 2) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Distance",
    y = "fluorescent intensity of Opal 520",
    title = "Distance Versus FI of Opal 520 in different cells",
    color = "Cell"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

FI_1<-ggplot(EC) +
  aes(x = Distance, y = Opal_520, colour = Class) +
  geom_point(shape = "circle", size = 4L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Distance",
    y = "fluorescent intensity of Opal 520",
    title = "FI of Opal 520 Greater than 1",
    color = "Cell"
  ) +
  theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14))+
  ylim(1, 10)

ggplot(EC) +
  aes(x = log(Opal_570+1), y = log(Opal_690+1),colour=Class) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Opal_570",
    y = "Opal_690",
    title = "Opal_570 Versus Opal_690 (With Logarithm)"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Class))

ggplot(EC) +
  aes(x = Opal_690, y = Distance) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Opal_690",
    y = "Distance",
    title = "Opal_690 Versus Distance"
    ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


