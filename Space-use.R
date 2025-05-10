
###########
# Are behavioral observations gathered from tolerant individuals? An assessment of grizzly bear-ecotourism coexistence through spatial and behavioral lenses
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
library("ggspatial")
library("sf")
library("basemaps")

setwd("")

atnarko.snags <- read.csv("atnarko_samples_to_individuals.csv") # A csv of samples to individuals for Atnarko hair snags only (2019-2021). 

# Subset grizzlies
grizz.atnarko.snags <- subset(atnarko.snags, species == "grizzly")
length(unique(grizz.atnarko.snags$individual[grizz.atnarko.snags$sex=="female"])) # should be 73
length(unique(grizz.atnarko.snags$individual[grizz.atnarko.snags$sex=="male"])) # should be 45

# An individual detected multiple times within a revisit is one detection.  
grizz.atnarko.snags <- grizz.atnarko.snags %>%
  dplyr::distinct(individual, revisitid, .keep_all = TRUE) %>%  
  dplyr::group_by(individual) %>%
  dplyr::mutate(detection.frequency = n())

# Make an object to join to figure later
detect.freq <- grizz.atnarko.snags %>%
  dplyr::group_by(individual) %>%
  dplyr::summarise(detection.frequency=length(unique(revisitid)))

# Movement maps for SI
# Identify some individuals that were detected exclusively in a given treatment:

# Count how many treatments each individual appears in
individual_treatment_counts <- grizz.atnarko.snags %>%
  distinct(individual, Treatment) %>%
  group_by(individual) %>%
  summarise(n_treatments = n(), .groups = "drop")

# Join this back to the data and filter for individuals that only appear in one treatment
filtered_snags <- grizz.atnarko.snags %>%
  left_join(individual_treatment_counts, by = "individual") %>%
  filter(n_treatments == 1)

# Within each treatment, find the most frequently detected individual (among the filtered ones)
most_detected <- filtered_snags %>%
  group_by(Treatment, individual) %>%
  summarise(detection_freq_within_treatment = n(), .groups = "drop") %>%
  group_by(Treatment) %>%
  slice_max(order_by = detection_freq_within_treatment, n = 1, with_ties = FALSE)

# Get all detections for these individuals per treatment
final_output <- filtered_snags %>%
  semi_join(most_detected, by = c("Treatment", "individual")) %>%
  left_join(most_detected, by = c("Treatment", "individual")) %>%
  dplyr::select(Treatment, individual, utme, utmn, utm_zone, detection_freq_within_treatment, revisitid)%>%
  arrange(individual)  # Order the dataframe by individual

# Now we have a dataframe with three individuals: of individuals that were detected exclusively in one treatment, these are the individuals that were detected the most frequently (within the treatment)

# Add another handful of representative individuals 
new_individuals <- c(49988, 50552, 40051, 43089, 50520, 38664, 40223)

# Filter, join, and select for all individuals at once
new_data <- grizz.atnarko.snags %>%
  filter(individual %in% new_individuals) %>%
  left_join(individual_treatment_counts, by = "individual") %>%
  dplyr::select(Treatment, individual, utme, utmn, utm_zone, revisitid, detection.frequency) %>%
  arrange(individual)

# Bind to the final output
final_output <- bind_rows(final_output, new_data) %>%
  arrange(individual)

final_output <- final_output %>%
  mutate(detection_frequency = coalesce(detection_freq_within_treatment, detection.frequency))%>%
  dplyr::select(-detection_freq_within_treatment, -detection.frequency)

# SI Map

# Create separate sf objects for each UTM zone
utm9_sf <- final_output %>%
  filter(utm_zone == 9) %>%
  st_as_sf(coords = c("utme", "utmn"), crs = 32609)

utm10_sf <- final_output %>%
  filter(utm_zone == 10) %>%
  st_as_sf(coords = c("utme", "utmn"), crs = 32610)

# Transform each to common CRS first
utm9_sf_3005 <- st_transform(utm9_sf, crs = 3005)
utm10_sf_3005 <- st_transform(utm10_sf, crs = 3005)

# Then combine
combined_sf <- bind_rows(utm9_sf_3005, utm10_sf_3005)

# Create movement lines by individual, ensuring each individual has multiple detections
movement_lines <- combined_sf %>%
  group_by(individual) %>%
  # Only keep individuals with more than one detection
  filter(n() > 1) %>%
  summarise(geometry = st_combine(geometry)) %>%
  mutate(geometry = st_cast(geometry, "LINESTRING"))

custom_titles <- c(
  "30318" = "Female : 9 Detections : Cluster B3",
  "38709" = "Female: 4 Detections : Cluster B1",
  "49988" = "Female : 5 Detections : Cluster B1",
  "50552" = "Female : 22 Detections : Cluster B2",
  "40051" = "Male : 11 Detections : Cluster B2",
  "43089" = "Male : 13 Detections : Cluster B2",
  "50520" = "Male : 5 Detections : Cluster B3",
  "41396" = "Female : 5 Detections : Cluster B2",
  "38664" = "Male : 4 Detections : Cluster B3",
  "40223" = "Male : 4 Detections : Cluster B2"
  
)

# Set factor levels for individual 
movement_lines <- movement_lines %>%
  mutate(individual = factor(individual, levels = c(
    "43089", "50552", "40051", "30318", "50520", 
    "41396", "40223", "49988", "38664", "38709"
  )))

combined_sf <- combined_sf %>%
  mutate(individual = factor(individual, levels = c(
    "43089", "50552", "40051", "30318", "50520", 
    "41396", "40223", "49988", "38664", "38709"
  )))

combined_sf$Treatment <- factor(combined_sf$Treatment,
                                levels = c("Platform_Drift", "Landbased", "Reference"))

ggplot() +
  annotation_map_tile(type = "cartolight") +
  geom_sf(data = movement_lines, 
          aes(color = individual), size = 2) +  
  geom_sf(data = combined_sf, 
          aes(color = individual, shape = Treatment), size = 2) + 
  theme_bw() +
  scale_color_manual(values = c(
    "30318" = "#e7298a",
    "38709" = "#d95f02",
    "49988" = "#66a61e",
    "50552" = "#1b9e77",
    "40051" = "#e6ab02",
    "43089" = "#7570b3",
    "50520" = "#1f77b4",  
    "41396" = "#ff7f0e",
    "38664" = "#a6761d",  
    "40223" = "#666666"   
  ), guide = "none") +
  scale_shape_manual(
    values = c(
      "Platform_Drift" = 17,
      "Landbased" = 15,
      "Reference" = 16
    ),
    name = "Spatial Treatment",
    labels = c(
      "Platform_Drift" = "Land- and boat-based",
      "Landbased" = "Land-based",
      "Reference" = "No Tours"
    )
  ) +
  facet_wrap(~individual, ncol = 2, nrow = 5, labeller = labeller(individual = custom_titles)) +
  theme(axis.text = element_text(size = 12),   
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),    
        legend.title = element_text(size = 14)) +
  labs(title = "") +
  annotation_north_arrow(location = "tl", height = unit(.3, "in"), width = unit(.3, "in")) +
  annotation_scale(location = "bl", width_hint = 0.1)

ggsave(file="rep_ind.png", height = 12, width = 13,
       dpi=200)

# Heatmap

# Proportion of detections per individual in each treatment
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

# Create a heatmap that illustrates spatial detection histories
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

# Rearrange columns 
col_order <- c("Land- & boat-based", "Land-based", "No tours")
wide2 <- wide2[, col_order]

# Add sex back in to individuals to annotate the heatmap with sex
wide <- proportion.of.detections.per.treatment
wide.sex <- dplyr::select(wide, sex)
wide.sex <- as.matrix(wide.sex)
annot_row <- data.frame(row.names = wide.sex[, 1], sex = wide.sex[, 2])

# Heatmap 
# Create one for all individuals (n = 118)

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

# Rotate the heatmap (ie transpose)
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "No tours" = "prop.Reference", 
                "Land-based" = "prop.Landbased")

# Rearrange columns
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

p2 <- as.ggplot(pheatmap(wide2.t, 
         annotation_col = annot_row, 
         cluster_rows=FALSE, 
         cellheight = 40, 
         cellwidth = 3,
         show_colnames = F,
         fontsize_row = 12,
         cutree_cols = 3)) # cuts three into specified number of clusters
p2

# Heatmap for manuscript (Figure 1B): individuals detected at least twice
# Subset the dataset to include individuals that have been detected at least twice
redetect <- grizz.atnarko.snags %>%
  group_by(individual) %>% 
  filter(n_distinct(revisitid)> 1) # detected at more than 1 revisit ID.
length(unique(redetect$individual)) # Should be 80. 
length(unique(redetect$individual[redetect$sex=="female"])) # should be 53
length(unique(redetect$individual[redetect$sex=="male"])) # should be 27

# Detection frequency to add as annotation to figure
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

# Create a matrix 
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "Land-based" = "prop.Landbased",
                "No tours" = "prop.Reference")

wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- wide2[, c(1, 3, 2)]
wide2 <- as.matrix(wide2)

# Rearrange columns 
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

ggsave(file="heatmap.for.manuscript.png", width=15, height=8, dpi=300) # width 1423 height 593 if exporting from plot environment


# Determine the optimal number of clusters 
# Elbow method
# https://uc-r.github.io/kmeans_clustering

fviz_nbclust(wide2, kmeans, method = "wss") + 
  theme_classic() + 
  labs(title = NULL)
# Optimal number of clusters is 3. 

final <- kmeans(wide2, 3)
fviz_cluster(final, data = wide2)


# SI heatmaps for individuals detected at least x times

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

# Create a matrix for  heat map
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "Land-based" = "prop.Landbased",
                "No tours" = "prop.Reference")
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

# Rearrange columns 
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

# Detection frequency 
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

# Create a matrix for  heat map
wide <- proportion.of.detections.per.treatment
wide <- wide %>%
  dplyr::rename("Land- & boat-based" = "prop.Platform_Drift",
                "Land-based" = "prop.Landbased",
                "No tours" = "prop.Reference")

wide <- wide %>% 
  remove_rownames %>% 
  column_to_rownames(var="individual")
wide2 <- dplyr::select(wide, -sex, -detection.frequency)
wide2 <- wide2[, c(1, 3, 2)]
wide2 <- as.matrix(wide2)

# Rearrange columns 
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

# Rearrange columns 
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

redetectp4
