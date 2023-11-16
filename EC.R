library(ggplot2)
library(tidyverse)
library(esquisse)
library(stringr)
library(gridExtra)
EC<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM20_5.csv",header = TRUE)
esquisser(EC)
EC2<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM12_4.csv",header = TRUE)
EC3<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM12_5.csv",header = TRUE)

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
  aes(x = log(Opal_570), y = log(Opal_620),colour=Class) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "570",
    y = "620",
    title = "570 Versus 620 (With Logarithm)",
    color= "Cell"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggplot(EC2) +
  aes(x = log(Opal_570), y = log(Opal_620),colour=Class) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "570",
    y = "620",
    title = "570 Versus 620 (With Logarithm) in layer 2",
    color= "Cell"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Class))

ggplot(EC) +
  aes(x = log(Opal_520), y = log(Distance)) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "520",
    y = "Distance",
    title = "Distribution for 520"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

p1<-ggplot(EC) +
  aes(x =  log(Opal_520), y = log(Distance),colour=Class) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "520",
    y = "Distance",
    title = "Distribution for 520 in layer 1",
    color= "Cell"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Class))

p2<-ggplot(EC2) +
  aes(x =  log(Opal_520), y = log(Distance),colour=Class) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "520",
    y = "Distance",
    title = "Distribution for 520 in layer 2",
    color= "Class"
    ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Class))

grid.arrange(p1,p2,ncol=2)



ggplot(EC3) +
  aes(x =  log(Opal_620), y = log(Distance),colour=Class) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "620",
    y = "Distance",
    title = "Distribution for 520 in layer 3",
    color= "Class"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Cell))




ggplot(EC) +
  aes(x = Class, y = Opal_520) +
  stat_summary(fun = "mean", geom = "bar", fill = "skyblue") +
  labs(
    x = "Cell",
    y = "Average Fluorescent Intensity",
    title = "520 in different Cell"
  ) +
  coord_flip() +
  theme_minimal()

EC %>%
  filter(Class %in% c("520:570", "520:570:690", "520:570:620", "520:570:620:690")) %>%
  ggplot() +
  aes(x = Class,y = Opal_520) +
  stat_summary(fun = "mean", geom = "bar", fill = "skyblue") +
  labs(
    x = "Cell",
    y = "Average Fluorescent Intensity",
    title = "570 in different Cell"
  ) +
  coord_flip() +
  theme_minimal()

EC %>%
  filter(Class %in% c("Opal 620: Opal 520: *", "Opal 620: Opal 520: *: Opal 570", "Opal 620: Opal 520: Opal 690: *", 
                      "Opal 620: Opal 520: Opal 690: Opal 570")) %>%
  ggplot() +
  aes(x = Class, weight = Opal_620) +
  geom_bar(fill = "skyblue") +
  labs(
    x = "Class",
    y = "Counts",
    title = "Opal_620 in different region"
  ) +
  coord_flip()+
  theme_minimal()

EC %>%
  filter(Class %in% c("*: Opal 520: Opal 690", "*: Opal 520: Opal 690: Opal 570", "Opal 620: Opal 520: Opal 690: *", 
                      "Opal 620: Opal 520: Opal 690: Opal 570")) %>%
  ggplot() +
  aes(x = Class, weight = Opal_690) +
  geom_bar(fill = "skyblue") +
  labs(
    x = "Class",
    y = "Counts",
    title = "Opal_690 in different region"
  ) +
  coord_flip()+
  theme_minimal()


b1<-ggplot(EC) +
  aes(x = Class, y =log(Opal_570)) +
  geom_boxplot(fill = "red") +
  labs(
    x = "Type of cell",
    y = "Log of 570",
    title = "570 in different cell type in layer 1"
  ) +
theme(axis.text=element_text(size=8L),
      axis.title=element_text(size=14L),
      axis.text.x =element_text(angle = 45,vjust = 1,hjust = 1))

b2<-ggplot(EC2) +
  aes(x = Class, y =log(Opal_570)) +
  geom_boxplot(fill = "red") +
  labs(
    x = "Type of cell",
    y = "Log of 570",
    title = "570 in different cell type in layer 2"
  ) +
  theme(axis.text=element_text(size=8L),
        axis.title=element_text(size=14L),
  axis.text.x =element_text(angle = 45,vjust = 1,hjust = 1))

b3<-ggplot(EC3) +
  aes(x = Class, y =log(Opal_570)) +
  geom_boxplot(fill = "red") +
  labs(
    x = "Type of cell",
    y = "Log of 570",
    title = "520 in different cell type"
  ) +
  theme(axis.text=element_text(size=6L),
        axis.title=element_text(size=14L),
        axis.text.x =element_text(angle = 45,vjust = 1,hjust = 1))

grid.arrange(b1,b2,ncol=2)
