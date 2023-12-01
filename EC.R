library(ggplot2)
library(tidyverse)
library(esquisse)
library(stringr)
library(gridExtra)
library(plotly)
EC<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM20_5.csv",header = TRUE)
EC2<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM12_4.csv",header = TRUE)
EC3<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM12_5.csv",header = TRUE)
esquisser(EC)

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

c1<-ggplot(EC) +
  aes(x = log(Opal_570), y = log(Opal_620)) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Log of FI for 570",
    y = "Log of FI for 620",
    title = "570 Versus 620 (With Logarithm) in layer 1"
  ) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12))+
  facet_wrap(vars(Class))

c2<-ggplot(EC2) +
  aes(x = log(Opal_570), y = log(Opal_620)) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Log of FI for 570",
    y = "Log of FI for 620",
    title = "570 Versus 620 (With Logarithm) in layer 2"
  ) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12))+
  facet_wrap(vars(Class))

grid.arrange(c1,c2,ncol=2)

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
  aes(x =  log(Opal_570), y = log(Distance)) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Log of FI for 570",
    y = "Log of Distance",
    title = "Distribution for 570 in layer 1"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Class))

p2<-ggplot(EC2) +
  aes(x =  log(Opal_570), y = log(Distance)) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Log of FI for 570",
    y = "Log of Distance",
    title = "Distribution for 570 in layer 2"
    ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Class))

grid.arrange(p1,p2,ncol=2)



ggplot(EC3) +
  aes(x =  log(Opal_620), y = log(Distance)) +
  geom_point(shape = "circle", size = 1L) +
  scale_color_hue(direction = 1) +
  labs(
    x = "620",
    y = "Distance",
    title = "Distribution for 520 in layer 3"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  facet_wrap(vars(Class))


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
    y = "Log of FI for 570",
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
    y = "Log of FI for 570",
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
    y = "Log of FI for 570",
    title = "520 in different cell type"
  ) +
  theme(axis.text=element_text(size=6L),
        axis.title=element_text(size=14L),
        axis.text.x =element_text(angle = 45,vjust = 1,hjust = 1))

grid.arrange(b1,b2,ncol=2)

EC3 %>%
  filter(Class %in% c("520:570", "520:570:620", "520:570:620:690", "520:570:690")) %>%
  ggplot() +
  aes(x = Class, y = log(Opal_620)) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

##shiwen's idea with layer 1
AMM12_1<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM12_4DetectionsL2.csv",header=TRUE)
AMM12_1 <- AMM12_1[c("Name","Cell..Opal.520.mean","Cell..Opal.570.mean", "Cell..Opal.620.mean", "Cell..Opal.690.mean" , "Distance.to.annotation.with.line.µm")]
colnames(AMM12_1) <- c("Name", "MFI520", "MFI570", "MFI620", "MFI690", "dist")


AMM12_1$'IND520' <- grepl("Opal 520", AMM12_1$Name)
AMM12_1$'IND570' <- grepl("Opal 570", AMM12_1$Name)
AMM12_1$'IND620' <- grepl("Opal 620", AMM12_1$Name)
AMM12_1$'IND690' <- grepl("Opal 690", AMM12_1$Name)

AMM12_1 <- AMM12_1[,-1]

AMM12_1 <- filter(AMM12_1, MFI520 != 0 & MFI570 != 0 & MFI620 != 0 & MFI690 != 0)

log_FI <- log(AMM12_1[,1:5])


fig_all <- plot_ly(log_FI, x = ~MFI570, y = ~MFI620, z = ~MFI690, marker = list(color = ~MFI520, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig_all <- fig_all %>% add_markers()
fig_all <- fig_all %>% layout(title = "520 vs. 570 vs 690,in layer 1",
                              scene = list(xaxis = list(title = '570'),
                                           yaxis = list(title = '620'),
                                           zaxis = list(title = '690')))

fig_all


log_FI <- log(filter(AMM12_1, IND520 == T & MFI520 < quantile(AMM12_1$MFI520, 0.95) ))
fig_520 <- plot_ly(log_FI, x = ~MFI570, y = ~MFI620, z = ~MFI690, marker = list(color = ~MFI520, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig_520 <- fig_520 %>% add_markers()
fig_520 <- fig_520 %>% layout(title = "520=T,520 vs. 570 vs 690, in layer 1",
                              scene = list(xaxis = list(title = '570'),
                                           yaxis = list(title = '620'),
                                           zaxis = list(title = '690')))
fig_520


log_FI <- log(filter(AMM12_1,  MFI520 < quantile(AMM12_1$MFI520, 0.95) & dist > quantile(AMM12_1$dist, 0.05)))
fig_dist <- plot_ly(log_FI, x = ~MFI520, y = ~MFI570, z = ~MFI690, marker = list(color = ~dist, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig_dist <- fig_dist %>% add_markers()
fig_dist <- fig_dist %>% layout(title = "520=T, 520 vs. 570 vs 690, in layer 1",
                                scene = list(xaxis = list(title = '520'), 
                                             yaxis = list(title = '570'), 
                                             zaxis = list(title = '690')))
fig_dist
## Another layer
AMM12_2<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM12_5DetectionsL2.csv",header=TRUE)
AMM12_2 <- AMM12_2[c("Name","Cell..Opal.520.mean","Cell..Opal.570.mean", "Cell..Opal.620.mean", "Cell..Opal.690.mean" , "Distance.to.annotation.with.line.µm")]
colnames(AMM12_2) <- c("Name", "MFI520", "MFI570", "MFI620", "MFI690", "dist")


AMM12_2$'IND520' <- grepl("Opal 520", AMM12_2$Name)
AMM12_2$'IND570' <- grepl("Opal 570", AMM12_2$Name)
AMM12_2$'IND620' <- grepl("Opal 620", AMM12_2$Name)
AMM12_2$'IND690' <- grepl("Opal 690", AMM12_2$Name)

AMM12_2 <- AMM12_2[,-1]

AMM12_2 <- filter(AMM12_2, MFI520 != 0 & MFI570 != 0 & MFI620 != 0 & MFI690 != 0)

log_FI2 <- log(AMM12_2[,1:5])


fig2_all <- plot_ly(log_FI2, x = ~MFI570, y = ~MFI620, z = ~MFI690, marker = list(color = ~MFI520, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig2_all <- fig2_all %>% add_markers()
fig2_all <- fig2_all %>% layout(title = "520 vs. 570 vs 690,in layer 2",
                                scene = list(xaxis = list(title = '570'),
                                           yaxis = list(title = '620'),
                                           zaxis = list(title = '690')))

fig2_all


log_FI2 <- log(filter(AMM12_2, IND520 == T & MFI520 < quantile(AMM12_2$MFI520, 0.95) ))
fig2_520 <- plot_ly(log_FI2, x = ~MFI570, y = ~MFI620, z = ~MFI690, marker = list(color = ~MFI520, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig2_520 <- fig2_520 %>% add_markers()
fig2_520 <- fig2_520 %>% layout("520=T,520 vs. 570 vs 690, in layer 2",
                                scene = list(xaxis = list(title = '570'),
                                           yaxis = list(title = '620'),
                                           zaxis = list(title = '690')))
fig2_520


log_FI2 <- log(filter(AMM12_2,  MFI520 < quantile(AMM12_2$MFI520, 0.95) & dist > quantile(AMM12_2$dist, 0.05)))
fig2_dist <- plot_ly(log_FI2, x = ~MFI520, y = ~MFI570, z = ~MFI690, marker = list(color = ~dist, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig2_dist <- fig2_dist %>% add_markers()
fig2_dist <- fig2_dist %>% layout(title = "520=T,520 vs. 570 vs 690, in layer 2",
                                  scene = list(xaxis = list(title = '520'), 
                                               yaxis = list(title = '570'),
                                               zaxis = list(title = '690')))
fig2_dist

## The third layer
AMM20_1<-read.csv("C:/Users/SamXi/OneDrive/Desktop/consulting/entorhinal_cortex/AMM20_5DetectionsL2.csv",header=TRUE)
AMM20_1 <- AMM20_1[c("Name","Cell..Opal.520.mean","Cell..Opal.570.mean", "Cell..Opal.620.mean", "Cell..Opal.690.mean" , "Distance.to.annotation.with.line.µm")]
colnames(AMM20_1) <- c("Name", "MFI520", "MFI570", "MFI620", "MFI690", "dist")


AMM20_1$'IND520' <- grepl("Opal 520", AMM20_1$Name)
AMM20_1$'IND570' <- grepl("Opal 570", AMM20_1$Name)
AMM20_1$'IND620' <- grepl("Opal 620", AMM20_1$Name)
AMM20_1$'IND690' <- grepl("Opal 690", AMM20_1$Name)

AMM20_1 <- AMM20_1[,-1]

AMM20_1 <- filter(AMM20_1, MFI520 != 0 & MFI570 != 0 & MFI620 != 0 & MFI690 != 0)

log_FI3 <- log(AMM20_1[,1:5])


fig3_all <- plot_ly(log_FI3, x = ~MFI570, y = ~MFI620, z = ~MFI690, marker = list(color = ~MFI520, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig3_all <- fig3_all %>% add_markers()
fig3_all <- fig3_all %>% layout(title = "520 vs. 570 vs 690,in layer 3",
                                scene = list(xaxis = list(title = '570'),
                                             yaxis = list(title = '620'),
                                             zaxis = list(title = '690')))

fig3_all


log_FI3 <- log(filter(AMM20_1, IND520 == T & MFI520 < quantile(AMM20_1$MFI520, 0.95) ))
fig3_520 <- plot_ly(log_FI3, x = ~MFI570, y = ~MFI620, z = ~MFI690, marker = list(color = ~MFI520, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig3_520 <- fig3_520 %>% add_markers()
fig3_520 <- fig3_520 %>% layout("520=T,520 vs. 570 vs 690, in layer 3",
                                scene = list(xaxis = list(title = '570'),
                                             yaxis = list(title = '620'),
                                             zaxis = list(title = '690')))
fig3_520


log_FI3 <- log(filter(AMM20_1,  MFI520 < quantile(AMM20_1$MFI520, 0.95) & dist > quantile(AMM20_1$dist, 0.05)))
fig3_dist <- plot_ly(log_FI3, x = ~MFI520, y = ~MFI570, z = ~MFI690, marker = list(color = ~dist, colorscale = "Rainbow", showscale = TRUE, size = 3))
fig3_dist <- fig3_dist %>% add_markers()
fig3_dist <- fig3_dist %>% layout(title = "520=T, 520 vs. 570 vs 690, in layer 3",
                                  scene = list(xaxis = list(title = '520'),
                                               yaxis = list(title = '570'),
                                               zaxis = list(title = '690')))
fig3_dist





