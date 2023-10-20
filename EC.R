library(ggplot2)
library(tidyverse)
library(esquisse)
EC<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/Opal data - Sheet1.csv",header = TRUE)
esquisser(EC)

Raw_EC<- read.csv('AMM20_5DetectionsL2.csv',header = TRUE)
gene1<-ggplot(EC) +
  aes(x = Cell..Opal.520.mean) +
  geom_histogram(bins = 50, fill = "green") +
  labs(
    x = "value of Gene 1",
    y = "Counts",
    title = "Raw distribution for Gene 1"
  ) +
  theme_minimal()

gene1_log<-ggplot(EC) +
  aes(x = log(Cell..Opal.520.mean)) +
  geom_histogram(bins = 50, fill = "green") +
  labs(
    x = "log value of Gene 1",
    y = "Counts",
    title = "Distribution for Gene 1 with log"
  ) +
  theme_minimal()

View(Raw_EC)

ggplot(Raw_EC)+
  aes(x = Cell..Autofluorescence.mean) +
  geom_histogram(bins = 30)
