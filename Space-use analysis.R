
###########
# Coexistence in an ecotourism system entails spatial filtering of individual grizzly bears tolerant of human activity 
# Space-use visulization 
###########

library("tidyverse")
library("ggplot2")
library("ggdendro")
library("tidyr")
library("grid")
library("dplyr")
library("pheatmap")
library("ggplotify")
library("cluster")
library("factoextra")
library("grid")

setwd("")

atnarko.snags <- read.csv("atnarko_samples_to_individuals.csv") # A csv of samples to individuals for Atnarko hair snags only (2019-2021). 

# 1) Create a dataframe that examines the proportion of detections per individual that occurred in each treatment, as well as the detection frequency of that individual.
# Eg., If individual x was detected 4 times; twice in the ecotour treatment, and twice in the reference:
# Detection freqeuncy = 4; 
# Prop.ecotour = 0.50
# Prop.landbased = 0
# Prop.reference = 0.50

# 2) Create a dateframe that examines the proportion of overall detections that occur in each treatment
# Eg., of the 118 grizzly bears, what proportion was detected exclusively in the ecotour treatment?


# Subset grizzlies
grizz.atnarko.snags <- subset(atnarko.snags, species == "grizzly")
length(unique(grizz.atnarko.snags$individual[grizz.atnarko.snags$sex=="female"])) # should be 73
length(unique(grizz.atnarko.snags$individual[grizz.atnarko.snags$sex=="male"])) # should be 45

# Detections counted as detected at each revisit (i.e., multiple samples from one individual at one revisit count only as one detection)
detect.freq <- grizz.atnarko.snags %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise(detection.frequency=length(unique(revisitid)))

# Create the proportion column
proportion.of.detections.per.treatment <- grizz.atnarko.snags %>%
  dplyr::group_by(individual, sex, Treatment) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(individual, sex) %>% 
  dplyr::mutate(prop = count/sum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = "Treatment", values_from = "prop", 
              names_prefix = "prop.", values_fill = 0) %>% 
  dplyr::summarise(prop.Platform_Drift = sum(prop.Platform_Drift), prop.Landbased = sum(prop.Landbased), 
                   prop.Reference = sum(prop.Reference)) %>%
  dplyr::left_join(detect.freq)

proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "female"] <- "Female"
proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "male"] <- "Male"

# Create a heatmap that illustrates proportion of detections that occur in each treatment 

# Prep the data
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
         "Land-based" = "prop.Landbased",
         "No tours" = "prop.Reference")
 
# Create a matrix for  heat map
wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- wide2[, c(1, 3, 2)]
wide2 <- as.matrix(wide2)

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("Land- & boat-based", "Land-based", "No tours")
wide2 <- wide2[, col_order]

# Add sex back in to individuals to annotate the heatmap with sex
wide <- proportion.of.detections.per.treatment
wide.sex <- dplyr::select(wide, sex)
wide.sex <- as.matrix(wide.sex)
annot_row <- data.frame(row.names = wide.sex[, 1], sex = wide.sex[, 2])

# Heatmap 1
# Individuals that are clustered together have similar detection histories 
# This is all individuals (n = 118)

p1 <- as.ggplot(
  pheatmap(wide2, 
           annotation_row = annot_row,
           cluster_cols=FALSE, 
           cellheight = 11, 
           cellwidth = 70, 
           cutree_row = 3,
           fontsize = 30,
           fontsize_row = 12,
           fontsize_col = 30))

# # Export image with height of 2200 and width 890, or:
# ggsave(file="heatmap.1.png", width=10, height=0, dpi=300)

# Rotate the heatmap (ie transpose)
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "No tours" = "prop.Reference", 
                "Land-based" = "prop.Landbased")

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("individual", "sex", "Land-based", "Land- & boat-based","No tours", "detection.frequency")
wide <- wide[, col_order]

wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- wide2[, c(1, 3, 2)]
wide2 <- as.matrix(wide2)
wide.sex.t <- dplyr::select(wide, sex)
wide.sex.t <- t(wide.sex.t)
wide.sex.t <- as.matrix(wide.sex.t)
wide2.t <- t(wide2)
wide2.t <- as.matrix(wide2.t)

# Plot
p2 <- as.ggplot(pheatmap(wide2.t, 
         annotation_col = annot_row, 
         cluster_rows=FALSE, 
         cellheight = 40, 
         cellwidth = 3,
         show_colnames = F,
         fontsize_row = 12,
         cutree_cols = 3)) # cuts three into specified number of clusters
p2
# ggsave(file="heatmap_all.118.individuals.png", width=10, height=3.5, dpi=300)


# Heatmap for manuscript: individuals detected at least twice
# Subset the dataset to include individuals that have been detected at least twice
redetect <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 1) # detected at more than 1 revisit ID.
length(unique(redetect$individual)) # Should be 80. 
length(unique(redetect$individual[redetect$sex=="female"])) # should be 53
length(unique(redetect$individual[redetect$sex=="male"])) # should be 27

# Detections counted as detected at each revisit (i.e., multiple samples from one individual at one revisit count only as one detection)
detect.freq <- redetect %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise(detection.frequency=length(unique(revisitid)))

# Create the proportion column
proportion.of.detections.per.treatment <- redetect %>%
  dplyr::group_by(individual, sex, Treatment) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(individual, sex) %>% 
  dplyr::mutate(prop = count/sum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = "Treatment", values_from = "prop", 
                     names_prefix = "prop.", values_fill = 0) %>% 
  dplyr::summarise(prop.Platform_Drift = sum(prop.Platform_Drift), prop.Landbased = sum(prop.Landbased), 
                   prop.Reference = sum(prop.Reference)) %>%
  dplyr::left_join(detect.freq)

proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "female"] <- "Female"
proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "male"] <- "Male"

# Create a heatmap that illustrates proportion of detections that occur in each treatment 
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "Land-based" = "prop.Landbased",
                "No tours" = "prop.Reference")

# Create a matrix for  heat map
wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- wide2[, c(1, 3, 2)]
wide2 <- as.matrix(wide2)

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("Land- & boat-based", "Land-based", "No tours")
wide2 <- wide2[, col_order]

# Add sex back in to individuals to annotate the heatmap with sex
wide <- proportion.of.detections.per.treatment
wide.sex <- dplyr::select(wide, sex)
wide.sex <- as.matrix(wide.sex)
annot_row <- data.frame(row.names = wide.sex[, 1], sex = wide.sex[, 2])

as.ggplot(
  pheatmap(wide2, 
           annotation_row = annot_row,
           cluster_cols=FALSE, 
           cellheight = 11, 
           cellwidth = 60, 
           cutree_row = 3,
           fontsize = 30,
           fontsize_row = 12,
           fontsize_col = 30))

# Rotate the heatmap (ie transpose)
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "No tours" = "prop.Reference", 
                "Land-based" = "prop.Landbased")

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("individual", "sex", "Land-based", "Land- & boat-based","No tours", "detection.frequency")
wide <- wide[, col_order]

# Add detection frequency annotation 
wide.det.freq <- dplyr::select(proportion.of.detections.per.treatment, detection.frequency)
wide.det.freq <- as.matrix(wide.det.freq)
annot_row.det.freq <- data.frame(row.names = wide.det.freq[, 1], det.freq = wide.det.freq[, 2])
annot_row.det.freq <- t(annot_row.det.freq)

wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
# wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- dplyr::select(wide, -sex)
#wide2 <- wide2[, c(1, 3, 2)]
wide2 <- wide2[, c(3, 1, 2)] # Changes the order of the columns for the heatmap
wide2 <- as.matrix(wide2)
# wide.sex.t <- dplyr::select(wide, sex)
wide.sex.t <- dplyr::select(wide, sex, detection.frequency)
wide.sex.t <- t(wide.sex.t)
wide.sex.t <- as.matrix(wide.sex.t)
wide2.t <- t(wide2)
wide2.t <- as.matrix(wide2.t)

detection.freq <- c(annot_row.det.freq)

# Plot
heatmap.for.manuscript <- as.ggplot(pheatmap(wide2.t, 
                                 annotation_col = annot_row, 
                                 cluster_rows=FALSE, 
                                 labels_col = detection.freq,
                                 fontsize_col = 10,
                                 angle_col = 90, # angle of the column labels, right now one can choose only from few predefined options (0, 45, 90, 270 and 315)
                                 # show_colnames = T, # shows individual IDs
                                 cellheight = 100, 
                                 cellwidth = 9,
                                 fontsize_row = 12,
                                 cutree_cols = 3)) # cuts tree into specified number of clusters. 

heatmap.for.manuscript
# Add text to the plot
grid.text("Detection Frequency", 
          x = unit(0.8, "npc"),  # Center horizontally
          y = unit(0.1, "npc"), # Adjust the vertical position as needed
          gp = gpar(fontsize = 12))  # Customize text appearance

# ggsave(file="heatmap.for.manuscript.png", width=15, height=8, dpi=300) # width 1423 height 593 if exporting from plot environment


# Determine the optimal number of clusters 
# Elbow method
# https://uc-r.github.io/kmeans_clustering

fviz_nbclust(wide2, kmeans, method = "wss") + 
  theme_classic() + 
  labs(title = NULL)
# Optimal number of clusters is 3. 

final <- kmeans(wide2, 3)
fviz_cluster(final, data = wide2)


#### SI heatmaps for individuals detected at least x times

# At least 3 detectinos
redetect3 <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 2) # detected at more than 2 revisit ID (at least 3 detections)
length(unique(redetect3$individual)) # Should be 52 
length(unique(redetect3$individual[redetect3$sex=="female"])) 
length(unique(redetect3$individual[redetect3$sex=="male"])) 

# Detections counted as detected at each revisit (i.e., multiple samples from one individual at one revisit count only as one detection)
detect.freq <- redetect3 %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise(detection.frequency=length(unique(revisitid)))

# Create the proportion column
proportion.of.detections.per.treatment <- redetect3 %>%
  dplyr::group_by(individual, sex, Treatment) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(individual, sex) %>% 
  dplyr::mutate(prop = count/sum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = "Treatment", values_from = "prop", 
                     names_prefix = "prop.", values_fill = 0) %>% 
  dplyr::summarise(prop.Platform_Drift = sum(prop.Platform_Drift), prop.Landbased = sum(prop.Landbased), 
                   prop.Reference = sum(prop.Reference)) %>%
  dplyr::left_join(detect.freq)

proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "female"] <- "Female"
proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "male"] <- "Male"


# Create a heatmap that illustrates proportion of detections that occur in each treatment 
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "Land-based" = "prop.Landbased",
                "No tours" = "prop.Reference")

# Create a matrix for  heat map
wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- wide2[, c(1, 3, 2)]
wide2 <- as.matrix(wide2)

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("Land- & boat-based", "Land-based", "No tours")
wide2 <- wide2[, col_order]

# Add sex back in to individuals to annotate the heatmap with sex
wide <- proportion.of.detections.per.treatment
wide.sex <- dplyr::select(wide, sex)
wide.sex <- as.matrix(wide.sex)
annot_row <- data.frame(row.names = wide.sex[, 1], sex = wide.sex[, 2])


# Rotate the heatmap (ie transpose)
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "No tours" = "prop.Reference", 
                "Land-based" = "prop.Landbased")

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("individual", "sex", "Land-based", "Land- & boat-based","No tours", "detection.frequency")
wide <- wide[, col_order]

# Add detection frequency annotation 
wide.det.freq <- dplyr::select(proportion.of.detections.per.treatment, detection.frequency)
wide.det.freq <- as.matrix(wide.det.freq)
annot_row.det.freq <- data.frame(row.names = wide.det.freq[, 1], det.freq = wide.det.freq[, 2])
annot_row.det.freq <- t(annot_row.det.freq)

wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
# wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- dplyr::select(wide, -sex)
#wide2 <- wide2[, c(1, 3, 2)]
wide2 <- wide2[, c(3, 1, 2)] # Changes the order of the columns for the heatmap
wide2 <- as.matrix(wide2)
# wide.sex.t <- dplyr::select(wide, sex)
wide.sex.t <- dplyr::select(wide, sex, detection.frequency)
wide.sex.t <- t(wide.sex.t)
wide.sex.t <- as.matrix(wide.sex.t)
wide2.t <- t(wide2)
wide2.t <- as.matrix(wide2.t)

detection.freq <- c(annot_row.det.freq)

# Determine the optimum number of clusters for the heatmap. 
fviz_nbclust(wide2, kmeans, method = "wss") + 
  theme_classic() + 
  labs(title = NULL)
# Optimal number of clusters is 3. 
# ggsave(file="clusters.png", width=8, height=6, dpi=300)

final <- kmeans(wide2, 3)
fviz_cluster(final, data = wide2)


# Plot
redetectp3 <- as.ggplot(pheatmap(wide2.t, 
                                 annotation_col = annot_row, 
                                 cluster_rows=FALSE, 
                                 labels_col = detection.freq,
                                 fontsize_col = 10,
                                 angle_col = 90, # angle of the column labels, right now one can choose only from few predefined options (0, 45, 90, 270 and 315)
                                 # show_colnames = T, # shows individual IDs
                                 cellheight = 100, 
                                 cellwidth = 9,
                                 fontsize_row = 12,
                                 cutree_cols = 3))

# Add text to the plot
grid.text("Detection Frequency", 
          x = unit(0.8, "npc"),  # Center horizontally
          y = unit(0.1, "npc"), # Adjust the vertical position as needed
          gp = gpar(fontsize = 12))  # Customize text appearance


# At least 4 detectinos
redetect4 <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 3) # detected at more than 2 revisit ID (at least 3 detections)
length(unique(redetect4$individual)) # Should be 30
length(unique(redetect4$individual[redetect4$sex=="female"])) 
length(unique(redetect4$individual[redetect4$sex=="male"])) 

# Detections counted as detected at each revisit (i.e., multiple samples from one individual at one revisit count only as one detection)
detect.freq <- redetect4 %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise(detection.frequency=length(unique(revisitid)))

# Create the proportion column
proportion.of.detections.per.treatment <- redetect4 %>%
  dplyr::group_by(individual, sex, Treatment) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(individual, sex) %>% 
  dplyr::mutate(prop = count/sum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = "Treatment", values_from = "prop", 
                     names_prefix = "prop.", values_fill = 0) %>% 
  dplyr::summarise(prop.Platform_Drift = sum(prop.Platform_Drift), prop.Landbased = sum(prop.Landbased), 
                   prop.Reference = sum(prop.Reference)) %>%
  dplyr::left_join(detect.freq)

proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "female"] <- "Female"
proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "male"] <- "Male"


# Create a heatmap that illustrates proportion of detections that occur in each treatment 
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "Land-based" = "prop.Landbased",
                "No tours" = "prop.Reference")

# Create a matrix for  heat map
wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- wide2[, c(1, 3, 2)]
wide2 <- as.matrix(wide2)

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("Land- & boat-based", "Land-based", "No tours")
wide2 <- wide2[, col_order]

# Add sex back in to individuals to annotate the heatmap with sex
wide <- proportion.of.detections.per.treatment
wide.sex <- dplyr::select(wide, sex)
wide.sex <- as.matrix(wide.sex)
annot_row <- data.frame(row.names = wide.sex[, 1], sex = wide.sex[, 2])


# Rotate the heatmap (ie transpose)
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "No tours" = "prop.Reference", 
                "Land-based" = "prop.Landbased")

# Rearrange columns to be ecotour, landbased, reference
col_order <- c("individual", "sex", "Land-based", "Land- & boat-based","No tours", "detection.frequency")
wide <- wide[, col_order]

# Add detection frequency annotation 
wide.det.freq <- dplyr::select(proportion.of.detections.per.treatment, detection.frequency)
wide.det.freq <- as.matrix(wide.det.freq)
annot_row.det.freq <- data.frame(row.names = wide.det.freq[, 1], det.freq = wide.det.freq[, 2])
annot_row.det.freq <- t(annot_row.det.freq)

wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
# wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- dplyr::select(wide, -sex)
#wide2 <- wide2[, c(1, 3, 2)]
wide2 <- wide2[, c(3, 1, 2)] # Changes the order of the columns for the heatmap
wide2 <- as.matrix(wide2)
# wide.sex.t <- dplyr::select(wide, sex)
wide.sex.t <- dplyr::select(wide, sex, detection.frequency)
wide.sex.t <- t(wide.sex.t)
wide.sex.t <- as.matrix(wide.sex.t)
wide2.t <- t(wide2)
wide2.t <- as.matrix(wide2.t)

detection.freq <- c(annot_row.det.freq)

# Determine the optimum number of clusters for the heatmap. 
fviz_nbclust(wide2, kmeans, method = "wss") + 
  theme_classic() + 
  labs(title = NULL)
# Optimal number of clusters is 3. 
# ggsave(file="clusters.png", width=8, height=6, dpi=300)

final <- kmeans(wide2, 3)
fviz_cluster(final, data = wide2)


# Plot
redetectp4 <- as.ggplot(pheatmap(wide2.t, 
                                 annotation_col = annot_row, 
                                 cluster_rows=FALSE, 
                                 labels_col = detection.freq,
                                 fontsize_col = 10,
                                 angle_col = 90, # angle of the column labels, right now one can choose only from few predefined options (0, 45, 90, 270 and 315)
                                 # show_colnames = T, # shows individual IDs
                                 cellheight = 100, 
                                 cellwidth = 9,
                                 fontsize_row = 12,
                                 cutree_cols = 3))

# Add text to the plot
grid.text("Detection Frequency", 
          x = unit(0.6, "npc"),  
          y = unit(0.1, "npc"), 
          gp = gpar(fontsize = 12))  # Customize text appearance


# End code for manuscript. 
# The following code illustrates box plots the plot the number of detections by the number of treatments individuals were detected in, as well as detection summaries (e.g., number of individuals that were detected exclusively in the ecotour area, etc). 

# Box plots
# Plot the number of detections by the number of treatments the bear was detected in 

det.by.treat <- proportion.of.detections.per.treatment
det.by.treat$num.treat <- rowSums(det.by.treat[,3:5]>0) # Count how many treatments they were detected in. 

ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency)) +
  geom_point(position=position_jitter(h=0.1,w=0.1)) +
  scale_x_continuous(breaks=c(1,2,3)) +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments") +
  annotate(geom = "text", x=1, y=20, label="Individuals detected\n at least 2 times \n (n = 80)", color = "black") +
  theme_classic()

ggplot(det.by.treat, aes(x = detection.frequency, y = num.treat)) +
  geom_point(position = position_jitter(h = 0.15, w = 0.15), size =1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Adding the slope line
  scale_y_continuous(breaks = c(1, 2, 3)) +
  ylim(0, 3) +
  scale_x_continuous(breaks = c(2:22)) +
  ylab("Treatments per individual") +
  xlab("Detection frequency") +
  #annotate(geom = "text", x=21, y=2.75, label="Individuals detected\n at least 2 times \n (n = 80)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))    
# ggsave(file="treatment-per-bear-detected.atleast.2.png", width=8, height=6, dpi=300)


det.by.treat$num.treat <- as.factor(det.by.treat$num.treat)
ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency, fill = sex)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(2:22)) +
  scale_fill_discrete(name = "Sex") +
  ylab("Detection frequency") +
  xlab("Number of treatments the individual was detected in") +
  annotate(geom = "text", x=1, y=20, label="Individuals detected\n at least 2 times \n (n = 80)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))        
#ggsave(file="boxplot_80.individuals.png", width=8, height=6, dpi=300)

ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments the individual was detected in") +
  annotate(geom = "text", x=1, y=20, label="Individuals detected\n at least 2 times \n (n = 80)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))  
# ggsave(file="", width=8, height=6, dpi=300)

# Subset individuals who were detected at least 3 times and create boxplot as above
redetect_atleast3 <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 2) # detected at more than 1 revisit ID.
length(unique(redetect_atleast3$individual)) # 52

detect.freq <- redetect_atleast3 %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise(detection.frequency=length(unique(revisitid)))

# Create the proportion column
proportion.of.detections.per.treatment <- redetect_atleast3 %>%
  dplyr::group_by(individual, sex, Treatment) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(individual, sex) %>% 
  dplyr::mutate(prop = count/sum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = "Treatment", values_from = "prop", 
                     names_prefix = "prop.", values_fill = 0) %>% 
  dplyr::summarise(prop.Platform_Drift = sum(prop.Platform_Drift), prop.Landbased = sum(prop.Landbased), 
                   prop.Reference = sum(prop.Reference)) %>%
  dplyr::left_join(detect.freq)

proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "female"] <- "Female"
proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "male"] <- "Male"

# Plot the number of detections by the number of treatments the bear was detected in 
det.by.treat <- proportion.of.detections.per.treatment
det.by.treat$num.treat <- rowSums(det.by.treat[,3:5]>0)

ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency)) +
  geom_point(position=position_jitter(h=0.1,w=0.1)) +
  scale_x_continuous(breaks=c(1,2,3)) +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments") +
  annotate(geom = "text", x=1.5, y=20, label="Individuals detected\n at least 3 times \n (n = 52)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))  

ggplot(det.by.treat, aes(x = detection.frequency, y = num.treat)) +
  geom_point(position = position_jitter(h = 0.15, w = 0.15), size =1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Adding the slope line
  scale_y_continuous(breaks = c(1, 2, 3)) +
  ylim(0, 3) +
  scale_x_continuous(breaks = c(3:22)) +
  ylab("Treatments per individual") +
  xlab("Detection frequency") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))  
# ggsave(file="treatment-per-bear-detected.atleast.3.png", width=8, height=6, dpi=300)


det.by.treat$num.treat <- as.factor(det.by.treat$num.treat)
ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency, fill = sex)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments") +
  annotate(geom = "text", x=1, y=20, label="Individuals detected\n at least 3 times \n (n = 52)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))  
# ggsave(file="boxplot_52.individuals_detected.atleast.3.png", width=8, height=6, dpi=300)

ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments") +
  annotate(geom = "text", x=1, y=20, label="Individuals detected\n at least 3 times \n (n = 52)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))  
#ggsave(file="boxplot_52.individuals_detected.atleast.3_nosex.png", width=8, height=6, dpi=300)

# Subset individuals who were detected at least 4 times and create boxplot as above
redetect_atleast4 <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 3) # detected at more than 1 revisit ID.
length(unique(redetect_atleast4$individual)) # 30

detect.freq <- redetect_atleast4 %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise(detection.frequency=length(unique(revisitid)))

# Create the proportion column
proportion.of.detections.per.treatment <- redetect_atleast4 %>%
  dplyr::group_by(individual, sex, Treatment) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::group_by(individual, sex) %>% 
  dplyr::mutate(prop = count/sum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = "Treatment", values_from = "prop", 
                     names_prefix = "prop.", values_fill = 0) %>% 
  dplyr::summarise(prop.Platform_Drift = sum(prop.Platform_Drift), prop.Landbased = sum(prop.Landbased), 
                   prop.Reference = sum(prop.Reference)) %>%
  dplyr::left_join(detect.freq)

proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "female"] <- "Female"
proportion.of.detections.per.treatment["sex"][proportion.of.detections.per.treatment["sex"] == "male"] <- "Male"

# Plot the number of detections by the number of treatments the bear was detected in 
det.by.treat <- proportion.of.detections.per.treatment
det.by.treat$num.treat <- rowSums(det.by.treat[,3:5]>0)

ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency)) +
  geom_point(position=position_jitter(h=0.1,w=0.1)) +
  scale_x_continuous(breaks=c(1,2,3)) +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments") +
  annotate(geom = "text", x=1.5, y=20, label="Individuals detected\n at least 4 times \n (n = 30)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))  

ggplot(det.by.treat, aes(x = detection.frequency, y = num.treat)) +
  geom_point(position = position_jitter(h = 0.15, w = 0.15), size =1) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Adding the slope line
  scale_y_continuous(breaks = c(1, 2, 3)) +
  ylim(0, 3) +
  scale_x_continuous(breaks = c(4:22)) +
  ylab("Treatments per individual") +
  xlab("Detection frequency") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15)) 
# ggsave(file="treatment-per-bear-detected.atleast.4.png", width=8, height=6, dpi=300)

det.by.treat$num.treat <- as.factor(det.by.treat$num.treat)
ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency, fill = sex)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments") +
  annotate(geom = "text", x=1, y=20, label="Individuals detected\n at least 4 times \n (n = 30)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))  
#ggsave(file="boxplot_30.individuals_detected.atleast.4.png", width=8, height=6, dpi=300)

ggplot(det.by.treat, aes(x = num.treat, y = detection.frequency)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(2:22)) +
  ylab("Detection frequency") +
  xlab("Number of treatments") +
  annotate(geom = "text", x=1, y=20, label="Individuals detected\n at least 4 times \n (n = 30)", color = "black") +
  theme_classic()+
  theme(
    axis.text = element_text(size = 13),       
    axis.title = element_text(size = 14),      
    legend.text = element_text(size = 13),      
    legend.title = element_text(size = 14),     
    plot.title = element_text(size = 15))
# ggsave(file="boxplot_30.individuals_detected.atleast.4_nosex.png", width=8, height=6, dpi=300)


# Basic summaries of detection histories 

# Create a dateframe that examines the proportion of overall detections that occur in each treatment
# Eg., of the 118 grizzly bears, what proportion was detected exclusively in the ecotour treatment?
# Do this for all individuals, as well as individuals that were detected at least twice.
# These are the tables used to create the donut charts on study area map that show the proportion of individuals that were male vs female in each treatment. 

# All individuals (ie detected once or more)
# Proportion of male vs female by treatment 
# Redetections occur among treatments, which is why the sum of the totals per treatment do not add up to the total number of individuals detected
length(unique(grizz.atnarko.snags$individual)) # Should be 118

prop <- grizz.atnarko.snags %>%
  dplyr::group_by(year, sex,Treatment) %>% # groups by year
  dplyr::summarise(length.unique.indi = n_distinct(individual)) %>% # how many individuals by sex and treatment
  dplyr::group_by(year, Treatment) %>%
  dplyr::mutate(indivi.by.treatment = sum(length.unique.indi)) %>% # how many individuals by treatment (pool sex)
  dplyr::mutate(proportion.of.sex.by.treatment = length.unique.indi/indivi.by.treatment) %>% # sex proportions
  dplyr::mutate(percent = paste(round(100*proportion.of.sex.by.treatment, 2), "%", sep=""))

# Pool years
# Redetections occur among treatments, which is why the sum of the totals per treatment do not add up to the total number of individuals detected
prop_allyears <- grizz.atnarko.snags %>%
  dplyr::group_by(sex,Treatment) %>%
  dplyr::summarise(length.unique.indi = n_distinct(individual)) %>% # how many individuals by sex and treatment
  dplyr::group_by(Treatment) %>%
  dplyr::mutate(indivi.by.treatment = sum(length.unique.indi)) %>% # how many individuals by treatment (pool sex)
  dplyr::mutate(proportion.of.sex.by.treatment = length.unique.indi/indivi.by.treatment) %>% # sex proportions
  dplyr::mutate(percent = paste(round(100*proportion.of.sex.by.treatment, 2), "%", sep="")) 

## Individuals that were detected at least twice (i.e., individuals who were only detected once are ommitted) 
redetect <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 1) # detected at more than 1 revisit ID. 
length(unique(redetect$individual)) # Should be 80

# Proportion of male vs female by treatment 
# Redetections occur among treatments, which is why the sum of the totals per treatment do not add up to the total number of individuals detected
prop <- redetect %>%
  dplyr::group_by(year, sex,Treatment) %>% # groups by year
  dplyr::summarise(length.unique.indi = n_distinct(individual)) %>% # how many individuals by sex and treatment
  dplyr::group_by(year, Treatment) %>%
  dplyr::mutate(indivi.by.treatment = sum(length.unique.indi)) %>% # how many individuals by treatment (pool sex)
  dplyr::mutate(proportion.of.sex.by.treatment = length.unique.indi/indivi.by.treatment) %>% # sex proportions
  dplyr::mutate(percent = paste(round(100*proportion.of.sex.by.treatment, 2), "%", sep=""))

# Pool years
# Redetections occur among treatments, which is why the sum of the totals per treatment do not add up to the total number of individuals detected
prop_allyears <- redetect %>%
  dplyr::group_by(sex,Treatment) %>%
  dplyr::summarise(length.unique.indi = n_distinct(individual)) %>% # how many individuals by sex and treatment
  dplyr::group_by(Treatment) %>%
  dplyr::mutate(indivi.by.treatment = sum(length.unique.indi)) %>% # how many individuals by treatment
  dplyr::mutate(proportion.of.sex.by.treatment = length.unique.indi/indivi.by.treatment) %>% # sex proportions
  dplyr::mutate(percent = paste(round(100*proportion.of.sex.by.treatment, 2), "%", sep="")) 


# Summaries of detection histories

# How many individuals were: 

# never detected in the ecotour area:
never.eco <- grizz.atnarko.snags %>%
  group_by(individual) %>%
  filter(all(Treatment != 'Platform_Drift'))
length(unique(never.eco$individual))

# deteteced in the ecotour area at least once:
eco.atleast <- grizz.atnarko.snags %>%
  group_by(individual) %>%
  filter((Treatment == 'Platform_Drift'))
length(unique(eco.atleast$individual))

# deteteced only the ecotour area:
only.eco <- grizz.atnarko.snags %>%
  group_by(individual) %>%
  filter(all(Treatment == 'Platform_Drift'))
length(unique(only.eco$individual))
21/118 #17.8% of total (118) grizz

only.eco %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual))

# deteteced only the landbased area:
only.land <- grizz.atnarko.snags %>%
  group_by(individual) %>%
  filter(all(Treatment == 'Landbased'))
length(unique(only.land$individual))
27/118

only.land %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual))

# deteteced only the reference area:
only.ref <- grizz.atnarko.snags %>%
  group_by(individual) %>%
  filter(all(Treatment == 'Reference'))
length(unique(only.ref$individual))
38/118

only.ref %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual)) # 16 male 22 female

# Two treatments
two.treat <- grizz.atnarko.snags %>% 
  group_by(individual) %>% 
  filter(n_distinct(Treatment) == 2)
27/118

two.treat %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual)) # 19 female and 8 male

# Three treatments
three.treat <- grizz.atnarko.snags %>% 
  group_by(individual) %>% 
  filter(n_distinct(Treatment) == 3)
5/118

three.treat %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual)) # 3 female and 2 male

# Create a histogram that show how frequently an individual was detected twice, 3 times, etc. 
# Count the number of unique revisit id's per individial, because each unique revisit id represents one detection. 
require(plyr)
hist <- ddply(grizz.atnarko.snags, .(individual), mutate, count = length(unique(revisitid)))
# Look at each individual by the number of times they were detected
hist <- hist[!duplicated(hist$individual), ]
hist(hist$count,
     xlim=c(0,24),
     breaks=20)

ggplot(hist, aes(x=count)) + 
  geom_histogram(binwidth = 1, color="black", fill="grey") +
  xlab(label = "Number of detections") +
  ylab(label = "Frequency of individuals detected x times") +
  xlim(1, 25) +
  theme_classic()

# Do the same, but subset for bears that were detected at least twice. 
# Subset according to above
redetect <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 1) # detected at more than 1 revisit ID.
length(unique(redetect$individual)) # Should be 80

# never detected in the ecotour area:
never.eco <- redetect %>%
  group_by(individual) %>%
  filter(all(Treatment != 'Platform_Drift'))
length(unique(never.eco$individual))
58/80

never.eco %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual))

# deteteced in the ecotour area at least once:
eco.atleast <- redetect %>%
  group_by(individual) %>%
  filter((Treatment == 'Platform_Drift'))
length(unique(eco.atleast$individual))

# deteteced only the ecotour area:
only.eco <- redetect %>%
  group_by(individual) %>%
  filter(all(Treatment == 'Platform_Drift'))
length(unique(only.eco$individual))
9/80

only.eco %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual))

# deteteced only the landbased area:
only.land <- redetect %>%
  group_by(individual) %>%
  filter(all(Treatment == 'Landbased'))
length(unique(only.land$individual))
14/80

only.land %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual))

# deteteced only the reference area:
only.ref <- redetect %>%
  group_by(individual) %>%
  filter(all(Treatment == 'Reference'))
length(unique(only.ref$individual))
25/80

only.ref %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual)) 

# One treatment
one.treat <- redetect %>% 
  group_by(individual) %>% 
  filter(n_distinct(Treatment) == 1)
length(unique(one.treat$individual))
48/80

one.treat %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual)) 

# Ground truth
# write.csv(one.treat, "one.treat.csv")
# Confirmed: this subets individuals who were detected in only one treatment. 

# Two treatments
two.treat <- redetect %>% 
  group_by(individual) %>% 
  filter(n_distinct(Treatment) == 2)
length(unique(two.treat$individual))
27/80

two.treat %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual)) 

# Three treatments
three.treat <- redetect %>% 
  group_by(individual) %>% 
  filter(n_distinct(Treatment) == 3)
length(unique(three.treat$individual))
5/80

three.treat %>%
  group_by(sex) %>%
  dplyr::summarize(var = n_distinct(individual))

# Create a histogram that show how frequently an individual was detected twice, 3 times, etc. 
# Count the number of unique revisit id's per individial, because each unique revisit id represents one detection. 
require(plyr)
hist <- ddply(redetect, .(individual), mutate, count = length(unique(revisitid)))
# Look at each individual by the number of times they were detected
hist <- hist[!duplicated(hist$individual), ]
hist(hist$count,
     xlim=c(0,24),
     breaks=20)

ggplot(hist, aes(x=count)) + 
  geom_histogram(binwidth = 1, color="black", fill="grey") +
  xlab(label = "Number of detections") +
  ylab(label = "Frequency of individuals detected x times") +
  xlim(1, 25) +
  theme_classic()

# Other redetection information

# Subset individuals who were detected at least twice - this is the dataframe we are working with for the manuscript.
redetect <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 1) # detected at more than 1 revisit ID.
length(unique(redetect$individual)) # Should be 80. 

# How many bears were detected across multiple seasons? 
# Detected in more than 1 year
redetect2 <- redetect %>%
  group_by(individual, sex) %>%
  summarise(n_distinct(year) >1) # detected at more than 1 revisit ID and in multiple years. 

length(unique(redetect2$individual[redetect2$`n_distinct(year) > 1` == "TRUE"])) # detected in more than 1 year
length(unique(redetect2$individual[redetect2$`n_distinct(year) > 1` == "FALSE"]))# detected in one year only

length(unique(redetect2$individual[redetect2$`n_distinct(year) > 1` == "TRUE"&redetect2$sex=="female"])) # females detected in more than 1 year
length(unique(redetect2$individual[redetect2$`n_distinct(year) > 1` == "TRUE"&redetect2$sex=="male"])) # males detected in more than 1 year
# Should add up to 44. 

# Detected in more than 2 years
redetect2 <- redetect %>%
  group_by(individual, sex) %>%
  summarise(n_distinct(year) >2)  
length(unique(redetect2$individual[redetect2$`n_distinct(year) > 2` == "TRUE"])) # detected in more than 2 years
length(unique(redetect2$individual[redetect2$`n_distinct(year) > 2` == "FALSE"]))

length(unique(redetect2$individual[redetect2$`n_distinct(year) > 2` == "TRUE"&redetect2$sex=="female"])) # females detected in more than 2 years
length(unique(redetect2$individual[redetect2$`n_distinct(year) > 2` == "TRUE"&redetect2$sex=="male"])) # males detected in more than 2 years

# Detected in one year only
redetect2 <- redetect %>%
  group_by(individual) %>%
  summarise(n_distinct(year)==1) # detected at more than 1 revisit ID and in multiple years. 
length(unique(redetect2$individual[redetect2$`n_distinct(year) == 1` == "TRUE"])) # detected in one year only
length(unique(redetect2$individual[redetect2$`n_distinct(year) == 1` == "FALSE"]))

# Of the bears detected in one year only, how many were detected multiple times?
redetect2 <- redetect %>%
  group_by(individual) %>%
  mutate(n_distinct(year)==1) # creates a column that shows true for individuals detected in only one year
redetect3 <- subset(redetect2, `n_distinct(year) == 1` =="TRUE") # creates a dataframe of individuals who were detected in only one year
length(unique(redetect3$individual)) 
redetect3 <- redetect3 %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 1) # detected at more than 1 revisit ID.
length(unique(redetect3$individual))
