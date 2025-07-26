
# ------------------------------------------------------------------------------
# Are behavioral observations gathered from tolerant individuals?
# An assessment of grizzly bear-ecotourism coexistence through spatial and behavioral lenses
# ------------------------------------------------------------------------------

# Behavior Analysis 
  
# There are two sections of this code with the following headings:
# 1) Data summaries and preparation
# 2) Model building and final plots

# ------------------------------
# 1) Data summaries and preparation
# ------------------------------

library(dplyr)
library(ggplot2)
library(lubridate)
library(hms)
library(tidyr)

setwd("")
data <- read.csv("clean_behaviour.csv")

# Sampling periods by year
data %>%
  group_by(year) %>%
  summarise(earliest_date = min(date))
data %>%
  group_by(year) %>%
  summarise(latest_date = max(date))

# Sampling hours of day
data %>%
  mutate(time_only = format(as.POSIXct(timestamp_focal.bhv.change), "%H:%M:%S")) %>%
  group_by(year) %>%
  summarise(earliest_time = min(time_only))
data %>%
  mutate(time_only = format(as.POSIXct(timestamp_focal.bhv.change), "%H:%M:%S")) %>%
  group_by(year) %>%
  summarise(latest_time = max(time_only))

# Calculate the mean, median, and range of durations of observation periods 
# Create a dataframe with unique values from focalbear_event
unique_focalbearevents <- data %>%
  group_by(focalbear_event) %>%
  summarize(total_length_within_focalevent = first(total_length_within_focalevent)) # total_length_within_focalevent is the total time duration within a focalbear_event 

# Group by focalbear_event and summarize statistics
summary_stats <- unique_focalbearevents %>%
  summarize(
    mean_total_length_within_focalevent = mean(total_length_within_focalevent, na.rm = TRUE),
    min_total_length_within_focalevent = min(total_length_within_focalevent, na.rm = TRUE),
    max_total_length_within_focalevent = max(total_length_within_focalevent, na.rm = TRUE),
    range_total_length_within_focalevent = difftime(max(total_length_within_focalevent, na.rm = TRUE), min(total_length_within_focalevent, na.rm = TRUE), units = "mins"),
    median_total_length_within_focalevent = median(total_length_within_focalevent, na.rm = TRUE)
  )

seconds_to_hms <- function(seconds) {
  hh <- floor(seconds / 3600)
  mm <- floor((seconds %% 3600) / 60)
  ss <- round(seconds %% 60)
  return(sprintf("%02d:%02d:%02d", hh, mm, ss))
}

# Convert the given seconds to HH:MM:SS format (the values are extracted from the 'summary_stats' dataframe)
seconds_to_hms(1061.279) # mean
seconds_to_hms(310) # min
seconds_to_hms(4214) # max
seconds_to_hms(776.5) # median

# Observation period duration frequencies 

# Create a histogram that shows frequency of observation period durations
# Count the number of unique focalbear_events 
# Create bins for different duration ranges
bins <- cut(unique_focalbearevents$total_length_within_focalevent,
            breaks = c(301, 601, 901, 1201, Inf), # Define breaks in seconds
            labels = c("5:01-10", "10:01-15", "15:01-20", ">20"))

# Calculate frequency of observation periods in each duration range
frequency_table <- table(bins)

# Convert frequency table to data frame
frequency_df <- as.data.frame(frequency_table)
names(frequency_df) <- c("Observation period durations (min)", "Frequency")

ggplot(data = frequency_df, aes(x = `Observation period durations (min)`, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  labs(x = "Observation period durations (min)",
       y = "Frequency") +
  theme_classic()
# ggsave(file="", width=6, height=4, dpi=300)

# How many minutes in total of behaviour data do we have?
# Sum up the total duration in seconds first
total_seconds <- sum(unique_focalbearevents$total_length_within_focalevent, na.rm = TRUE)
total_seconds / 60 # = 2405.567 minutes

# How many observation periods per year?
yearly_data <- data %>%
  group_by(year) %>%
  summarize(UniqueCount = n_distinct(focalbear_event))

ggplot(yearly_data, aes(x = factor(year), y = UniqueCount)) +
  geom_col(width = 0.9, fill = "grey") + 
  labs(x = "Year",
       y = "Number of observation periods") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# ggsave(file="", width=7, height=5, dpi=300)

# Observation periods per site
# Sites
length(unique(data$focalbear_event[data$site_name=="FISHERIES POOL"]))
length(unique(data$focalbear_event[data$site_name=="BELARKO"]))

summary_data <- data %>%
  group_by(site_name) %>%
  summarise(unique_events = n_distinct(focalbear_event))

# Plot the summarized data
ggplot(summary_data, aes(x = site_name, y = unique_events)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(x = "Site",
       y = "Number of observation periods") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# ggsave(file="", width=7, height=5, dpi=300)

# Age-sex class summary
length(unique(data$focalbear_event))
length(unique(data$focalbear_event[data$agesex=="Adult female"]))
length(unique(data$focalbear_event[data$agesex=="Adult male"]))
length(unique(data$focalbear_event[data$agesex=="Adult unknown"]))
length(unique(data$focalbear_event[data$agesex=="Subadult"]))
length(unique(data$focalbear_event[data$agesex=="Female with young"]))
length(unique(data$focalbear_event[data$agesex=="Unknown"]))

# Create a data frame with the frequencies of each sex class
sex_freq <- data.frame(
  agesex = c("Adult female", "Adult male", "Adult unknown", "Subadult", "Female w young"),
  frequency = c(51, 4, 14, 11,56)
)
# Order the levels of the 'agesex' factor from greatest to least frequency
sex_freq$agesex <- factor(sex_freq$agesex, levels = sex_freq$agesex[order(-sex_freq$frequency)])

# Create a bar plot
ggplot(sex_freq, aes(x = agesex, y = frequency)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(x = "Age-sex class",
       y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# ggsave(file="", width=7, height=5, dpi=300)

## Prepare data for analysis 

# Merge salmon data

# Salmon data
# Attribute weekly salmon count data from the Atnarko (same methods and salmon dataset as Field et al., 2024; https://doi.org/10.1111/csp2.13097)
salm <- read.csv("salmon.csv")

# Subset Belarko drift only
salm  <- salm  %>%
  filter(River_section_ID  == "Belarko Drift")

# Join by isoweek. 
data <- merge(data, salm[, c("yearweek_ID", "total.biomass")], by = "yearweek_ID", all.x = TRUE)

class(data$start_time_of_focal)
data$start_time_of_focal <- as.POSIXct(data$start_time_of_focal, format = "%H:%M:%S")

# Management treatment
data <- data %>%
  mutate(treatment = 
           case_when(
             site_name == 'BELARKO' ~ 'Designated', # ie designated bear viewing
             site_name == 'FISHERIES POOL' ~ 'Non-designated')) # ie non-designated bear viewing

# Correct typo
data <- data %>%
  mutate(
    coarse_code = case_when(
      coarse_code == 31 & focalbear_event == "2019_B0112_FCL3" ~ 30,
      TRUE ~ coarse_code # This keeps all other values unchanged
    )
  )

# Summarize fine_code proportions for each focalbear_event (i.e., the proportion of each focalbear_event that was a given fine behaviour code). 
# The variable 'propotion_fine' delineated below summarizes the proportion of time the bear was selecting a given behaviour within the focalbear_event. The variable was not used for the analysis in the manuscript but may be of interest for future analyses.

fine_summary <- data %>%
  group_by(year, site_name, focalbear_event, fine_code) %>%
  mutate(
    duration_numeric = as.numeric(as.POSIXct(duration_of_behave, format = "%H:%M:%S")) # Convert duration to numeric
  ) %>%
  summarise(
    total_duration = sum(duration_numeric), # Total duration for each unique fine_code
    .groups = 'drop'
  ) %>%
  group_by(year, site_name, focalbear_event) %>%
  mutate(
    proportion_fine = total_duration / sum(total_duration) # Proportion of time for each unique fine_code
  ) %>%
  ungroup() %>%
  distinct(year, site_name, focalbear_event, fine_code, proportion_fine) # Ensure no duplicates in summary

data <- data %>%
  left_join(fine_summary, by = c("year", "site_name", "focalbear_event", "fine_code")) # instantaneous behaviours are retained. 


# ------------------------------
# 2) Model building and final plots
# ------------------------------

library(arm)
library(MuMIn) 
library(coefplot)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(mclogit)
library(ggeffects)
library(dotwhisker)
library(ggpubr)

# Convert timestamp column to POSIXct format
head(data$timestamp_focal.bhv.change, 10)
data$timestamp_focal.bhv.change <- ymd_hm(data$timestamp_focal.bhv.change)
class(data$timestamp_focal.bhv.change)

# Arrange timestamps within each focalbear_event
data <- data %>%
  group_by(focalbear_event) %>%
  arrange(timestamp_focal.bhv.change, .by_group = TRUE)

# Arrange video_time_behave_change within focalbear_event and file_id
data <- data %>%
  group_by(focalbear_event, file_id) %>%
  arrange(video_time_behave_change, .by_group = TRUE)

# If there are multiple behaviour changes within a minute, choose only the last behaviour within that minute because it will be the closest behaviour that occurred to the scan sample. 
data <- data %>%
  group_by(focalbear_event) %>%
  arrange(timestamp_focal.bhv.change) %>%
  distinct(floor_timestamp = floor_date(timestamp_focal.bhv.change, unit = "1 minute"), .keep_all = TRUE) %>%
  ungroup() %>%
  dplyr::select(-floor_timestamp)

# Call in scan data
scans <- read.csv("scans.csv")

scans$timestamp.s <- ymd_hm(scans$timestamp.s)
scans <- scans %>%
  dplyr::rename(timestamp_scans = timestamp.s)

# Join scans to their respective behaviours
data <- left_join(
  data,scans,
  join_by(
    closest(timestamp_focal.bhv.change >= timestamp_scans), site_name))
data <- data %>%
  relocate(timestamp_scans, .after = timestamp_focal.bhv.change)

# Assign numeric behavioural codes to the behaviour itself
data <- data %>%
  mutate(coarse_behaviour =
           case_when(
             coarse_code == '30' ~ 'Alertness',
             coarse_code == '40' ~ 'Fishing',
             coarse_code == '0' ~ 'Unobservable',
             TRUE ~ 'Other'  # This handles all other cases
           ))

data <- data %>%
  mutate(fine_behaviour =
           case_when(
             fine_code == '31' ~ 'Subtle Alertness',
             fine_code == '32' ~ 'Overt Alertness',
             fine_code == '41' ~ 'Search',
             fine_code == '42' ~ 'Pursuit',
             fine_code == '43' ~ 'Handle',
             fine_code == '44' ~ 'Consume',
             fine_code == '45' ~ 'Handle',
             fine_code == '46' ~ 'Consume',
             fine_code == '47' ~ 'Handle',
             fine_code == '48' ~ 'Consume',
             fine_code == '0' ~ 'Unobservable',
             TRUE ~ 'Other'  # This handles all other cases
           ))

# Convert NA to 0's for bears, people, and boats (ie, there were always 0 or >0)
data<- data %>%
  mutate(other_bears_num_not_incl_fam = ifelse(is.na(other_bears_num_not_incl_fam), 0, other_bears_num_not_incl_fam))

data<- data %>%
  mutate(land_human_num = ifelse(is.na(land_human_num), 0, land_human_num))

data<- data %>%
  mutate(num_boats = ifelse(is.na(num_boats), 0,num_boats))

data <- data[!duplicated(data$timestamp_scans), ] # This makes the scan the case. What this is, is the first behaviour within a minute if there are multiple behaviour changes within that minute, and its anchored to the scan that came immediately before that. 

# Histogram to look at the number of cases within a observation period. 
ggplot(data, aes(x = reorder(focalbear_event, focalbear_event, function(x) -length(x)))) +
  labs(x = "Observation periods",
       y = "Number of cases") +
  geom_bar(fill = "grey", color = "black") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16))

# Sample size of scans before omitting males
data %>%
  group_by(year.x) %>%
  summarize(count = n()) %>%
  ungroup()

data %>%
  summarize(count = n()) %>%
  ungroup()

# How many hours of observation
hours <- data %>%
  group_by(focalbear_event, year.x) %>%                                   
  summarize(unique_length = sum(unique(total_length_within_focalevent))) %>%  
  ungroup() %>%
  group_by(year.x) %>%                                                    
  summarize(total_length_sum = sum(unique_length))  

hours <- hours %>%
  mutate(total_length_hours = total_length_sum / 3600)  
sum(hours$total_length_hours)

data$agesex[data$agesex == "Adult unknown"] <- NA

# How many focalbear_events with males included in the dataset
length(unique(data$focalbear_event))
# How many scans
data %>%
  summarize(count = n()) %>%
  ungroup()

data <- data %>%
  filter(agesex %in% c("Adult female", "Female with young", "Subadult"))

# How man focalbear_events without males
length(unique(data$focalbear_event))
# How many scans
data %>%
  summarize(count = n()) %>%
  ungroup()

# Number of scans without males subtracted from scans with males equals 87 scans with males. 
1060-973

# How many hours of observation without males
hours2 <- data %>%
  group_by(focalbear_event, site_name) %>%                                   
  summarize(unique_length = sum(unique(total_length_within_focalevent))) %>%  
  ungroup() %>%
  group_by(site_name) %>%                                                    
  summarize(total_length_sum = sum(unique_length))  

hours2 <- hours2 %>%
  mutate(total_length_hours = total_length_sum / 3600)  
sum(hours2$total_length_hours) #35.97 hours or 35 hours and 58 min

# Omit observation periods where there were either only 1 scan, or there were scans >5 min apart

mean_time_diff_per_group <- data %>%
  group_by(focalbear_event) %>%
  # Arrange by timestamp within each group
  arrange(timestamp_scans) %>%
  # Calculate the time difference between consecutive scans
  mutate(time_diff = as.numeric(difftime(timestamp_scans, lag(timestamp_scans), units = "mins"))) %>%
  # Calculate the mean time difference per group, ignoring NAs
  summarise(mean_time_diff = mean(time_diff, na.rm = TRUE)) %>%
  # Arrange by mean time difference (ascending)
  arrange(mean_time_diff)

# Extract focalbear_events from rows 108 to 118 because we are including events that had scans no more than mean 5 min apart. 
events_to_omit <- mean_time_diff_per_group %>%
  slice(108:118) %>%
  pull(focalbear_event)

data <- data %>%
  filter(!focalbear_event %in% events_to_omit)

mean_time_diff_per_group <- data %>%
  group_by(focalbear_event) %>%
  # Arrange by timestamp within each group
  arrange(timestamp_scans) %>%
  # Calculate the time difference between consecutive scans
  mutate(time_diff = as.numeric(difftime(timestamp_scans, lag(timestamp_scans), units = "mins"))) %>%
  # Calculate the mean time difference per group, ignoring NAs
  summarise(mean_time_diff = mean(time_diff, na.rm = TRUE)) %>%
  # Arrange by mean time difference (ascending)
  arrange(mean_time_diff)

mean(mean_time_diff_per_group$mean_time_diff)
range(mean_time_diff_per_group$mean_time_diff)

# How many scans per obs period.
row_counts <- data %>%
  group_by(focalbear_event) %>%
  summarise(count = n())

mean(row_counts$count)
median(row_counts$count)
range(row_counts$count)

# Other bear present/absent 
data <- data %>%
  mutate(bears = ifelse(other_bears_num_not_incl_fam > 0, 1, 0))

# Rescale variables
data$salmon <- rescale(data$total.biomass)
data$land_human <- rescale(data$land_human_num)
data$boats <- rescale(data$num_boats)
data$treatment <- as.factor(data$treatment)
data$agesex <- as.factor(data$agesex)
data$focalbear_event <- as.factor(data$focalbear_event)

table(data$coarse_behaviour)
data$coarse_behaviour[data$coarse_behaviour == "Unobservable"] <- NA
data$fine_behaviour <- factor(data$fine_behaviour,
                             levels = c("Other",
                                        "Search",
                                        "Pursuit",
                                        "Handle",
                                        "Consume",
                                        "Subtle Alertness",
                                        "Overt Alertness"))

data$coarse_behaviour <- as.factor(data$coarse_behaviour)
data$coarse_behaviour <- factor(data$coarse_behaviour,
                               levels = c("Other",
                                          "Fishing",
                                          "Alertness"))

# Sample size of scans after omitting males
sample <- data %>%
  group_by(year.x) %>%
  summarize(count = n()) %>%
  ungroup()
sum(sample$count) #936

length(unique(data$focalbear_event))

prop.table(table(data$coarse_behaviour))

data$agesex <- factor(data$agesex,
                     levels = c(
                       "Adult female",
                       "Female with young",
                       "Subadult"))

# Visual ID of bears

# Assign UNKNOWN to any bears with uncertainty for most conservative approach
data <- data %>%
  mutate(bear_name = ifelse(stringr::str_detect(bear_name, "\\?"), "UNKNOWN", bear_name))

length(unique(data$bear_name))

data <- data %>%
  mutate(
    bear_name = case_when(
      bear_name == "Unknown" ~ "UNKNOWN", 
      bear_name == "Unknown12" ~ "UNKNOWN", 
      bear_name == "UNKNOWN2" ~ "UNKNOWN", 
      bear_name == "Unknown3" ~ "UNKNOWN", 
      bear_name == "Unknown4" ~ "UNKNOWN", 
      bear_name == "Unknown8" ~ "UNKNOWN", 
      bear_name == "Unknown9" ~ "UNKNOWN", 
      bear_name == "UNKNOWNMOM" ~ "UNKNOWN",
      bear_name == "DARKMAMMAorWHITESPOTMAMMA" ~ "UNKNOWN",
      bear_name == "DARKMAMA" ~ "DARKMAMMA",
      bear_name == "TIPS" & year.x == 2021 ~ "TIPSB", # We did not attempt to ID bears across years, so adding "B" to distinguish between tips in 2020 and tips in 2021. 
      TRUE ~ bear_name # This keeps all other values unchanged
    ))

length(unique(data$bear_name)) # so 37 + "Unknown" assigned to all unknown

focals.per.year <- data %>%
  group_by(year.x) %>%
  summarise(unique_bear_count = n_distinct(focalbear_event), .groups = 'drop') 

# How many did we attempt to ID?
no_unknown <- data %>%
  filter(bear_name != "UNKNOWN")

length(unique(no_unknown$bear_name))

# How many did we attempt to ID each year?
no_unknown %>%
  group_by(year.x) %>%
  summarise(unique_bear_count = n_distinct(bear_name), .groups = 'drop')

no_unknown %>%
  group_by(year.x) %>%  # Group by year.x to count unique focalbear_events
  summarise(
    unique_bear_count = n_distinct(bear_name),   # Count unique bears per group
    unique_focalbear_event_count = n_distinct(focalbear_event)  # Count unique focalbear_events per year
  ) %>%
  ungroup()

# How many observation periods were unknowns?
unknowns <- data %>%
  filter(bear_name == "UNKNOWN") %>%   
  summarise(unique_ids = n_distinct(focalbear_event))

# Human histograms

# Maximum number of people on land per observation period
humans_per_focal <- data %>%
  group_by(focalbear_event) %>%
  summarize(max_land_human_num = max(land_human_num, na.rm = TRUE))

ggplot(humans_per_focal, aes(x = max_land_human_num)) +
  geom_histogram(color = "grey", bins = 20) +
  labs(x = "Maximum number of people on land",
       y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# ggsave(file="", width=7, height=5, dpi=300)

# Maximum boats per observation period
boats_per_focal <- data %>%
  group_by(focalbear_event) %>%
  summarize(max_boats = max(num_boats, na.rm = TRUE))

ggplot(boats_per_focal, aes(x = max_boats)) +
  geom_histogram(color = "grey", bins = 20) +
  labs(x = "Maximum number of boats",
       y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# ggsave(file="", width=7, height=5, dpi=300)

# People on land per scan
ggplot(data, aes(x = land_human_num)) +
  geom_histogram(color = "grey", bins = 20) +
  labs(x = "Number of people on land",
       y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# ggsave(file="", width=7, height=5, dpi=300)

# Boats per scan
ggplot(data, aes(x = num_boats)) +
  geom_histogram(color = "grey") +
  labs(x = "Number of boats",
       y = "Frequency") +
  theme_classic(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# ggsave(file="", width=7, height=5, dpi=300)

# Proportion of behaviours 

prop.table(table(data$coarse_behaviour))

# Redo when final data is done
behavior_distribution <- data.frame(
  Behaviour = factor(c("Fishing", "Alertness"," Other"), levels = c("Fishing", "Alertness"," Other")),
  Proportion = c(0.4000000, 0.2524823,0.3475177)
)

# Create the bar plot
behavior_distribution <- ggplot(behavior_distribution, aes(x = Behaviour, y = Proportion)) +
  geom_bar(stat = "identity", fill = "grey") + 
  labs(
    x = "",
    y = "Proportion of cases",
    title = ""
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none" # Hide legend if not needed
  )

print(behavior_distribution)

# T plus variable (lead effect)

data <- data %>%
  group_by(focalbear_event) %>%   # Group by focalbear_event to calculate leads within each event
  mutate(boats_tp1 = lead(num_boats, 1))%>%  # Lead variable for boats (t+1)
  mutate(boats_tp2 = lead(num_boats, 2)) %>%
  mutate(people_tp1 = lead(land_human_num, 1)) %>%
  mutate(people_tp2 = lead(land_human_num, 2))

data$boats_tp1 <- rescale(data$boats_tp1)
data$boats_tp2 <- rescale(data$boats_tp2)
data$people_tp1 <- rescale(data$people_tp1)
data$people_tp2 <- rescale(data$people_tp2)

# Build Models

# Null
LD_1 <- mblogit(
  coarse_behaviour ~ 1,
  random=c(~1|focalbear_event),
  data = data)

LD_2 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears,
  random=c(~1|focalbear_event),
  data = data)

LD_3 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_4 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    land_human,
  random=c(~1|focalbear_event),
  data = data
)

LD_5 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    land_human +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_6 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    boats,
  random=c(~1|focalbear_event),
  data = data
)

LD_7 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    boats +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_8 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    land_human +
    boats,
  random=c(~1|focalbear_event),
  data = data
)

LD_9 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    land_human +
    boats +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_10 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    land_human +
    boats +
    land_human:boats,
  random=c(~1|focalbear_event),
  data = data
)

LD_11 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    land_human +
    boats +
    land_human:boats +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_12 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp1,
  random=c(~1|focalbear_event),
  data = data
)

LD_13 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp1 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

# boats_tp1
LD_14 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    boats_tp1,
  random=c(~1|focalbear_event),
  data = data
)

LD_15 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    boats_tp1 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_16 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp1 +
    boats_tp1,
  random=c(~1|focalbear_event),
  data = data
)

LD_17 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp1 +
    boats_tp1 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_18 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp1 +
    boats_tp1 +
    people_tp1:boats_tp1,
  random=c(~1|focalbear_event),
  data = data
)

LD_19 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp1 +
    boats_tp1 +
    people_tp1:boats_tp1 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_20 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp2,
  random=c(~1|focalbear_event),
  data = data
)

LD_21 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp2 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_22 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    boats_tp2,
  random=c(~1|focalbear_event),
  data = data
)

LD_23 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    boats_tp2 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_24 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp2 +
    boats_tp2,
  random=c(~1|focalbear_event),
  data = data
)

LD_25 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp2 +
    boats_tp2 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

LD_26 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp2 +
    boats_tp2 +
    people_tp2:boats_tp2,
  random=c(~1|focalbear_event),
  data = data
)

LD_27 <- mblogit(
  coarse_behaviour ~ 
    salmon +
    bears +
    people_tp2 +
    boats_tp2 +
    people_tp2:boats_tp2 +
    agesex,
  random=c(~1|focalbear_event),
  data = data
)

# AIC
AIC(LD_1, LD_2, LD_3, LD_4, LD_5,LD_6,LD_7,LD_8,LD_9,LD_10,LD_11,LD_12,LD_13,LD_14,LD_15,LD_16,LD_17,LD_18,LD_19,LD_20, LD_21,LD_22,LD_23,LD_24,LD_25,LD_26,LD_27)

# Model selection table with AIC 
model_sel_lead <- model.sel(LD_1, LD_2, LD_3, LD_4, LD_5,LD_6,LD_7,LD_8,LD_9,LD_10,LD_11,LD_12,LD_13,LD_14,LD_15,LD_16,LD_17,LD_18,LD_19,LD_20, LD_21,LD_22,LD_23,LD_24,LD_25,LD_26,LD_27, rank = AIC)
model_sel_lead$cumulative.weight = cumsum(model_sel_lead$weight)

summary(LD_22)

# Plots

# Coefficients and standard errors
coef_data_LD_22 <- data.frame(
  term = c(
    "Fishing ~ Salmon", "Fishing ~ Bears", "Fishing ~ Boats t+2",
    "Alertness ~ Salmon", "Alertness ~ Bears", "Alertness ~ Boats t+2"
  ),
  estimate = c(
    -0.24842, 0.15476, 0.08784,   # Fishing vs Other (updated estimates)
    -0.84339, -0.79707, 0.03525   # Alertness vs Other (updated estimates)
  ),
  std.error = c(
    0.25332, 0.40121, 0.25749,    # Fishing vs Other (updated standard errors)
    0.27448, 0.53977, 0.27382     # Alertness vs Other (updated standard errors)
  ),
  model = rep(c("Fishing", "Alertness"), each = 3)  # Fishing and Alertness models
)

# Create the plot
plot_LD_22 <- dwplot(coef_data_LD_22, dot_args = list(color = "red"), 
                     whisker_args = list(color = "black")) +
  theme_bw() +
  theme(legend.position = "none") +  # Remove legend
  scale_color_manual(values = c("Fishing" = "black", "Alertness" = "black")) +  # Custom colors 
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey30") +  # Vertical line at 0
  labs(title = "",
       x = "Coefficient Estimate",
       y = "") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) 

print(plot_LD_22)

# Check for overdispersion 
deviance <- deviance(LD_22)  # Residual deviance
df <- df.residual(LD_22)     # Degrees of freedom
# Calculate overdispersion ratio
overdispersion_ratio <- deviance / df
# Print result
print(overdispersion_ratio)

sal <- ggpredict(LD_22, terms = "salmon")
# Un-center and scale by multiplying by sd and adding mean
# Mean and SD of salmon to re-center and scale
getsd.and.mean <- data[!is.na(data$total.biomass),]
m <- mean(getsd.and.mean$total.biomass)
sd <- sd(getsd.and.mean$total.biomass)
sal$x <- (sal$x * (2*sd)) + m 

sal <- sal %>% filter(response.level == "Alertness")
sal.p <- sal %>% plot()+
  labs(y = "P(Alertness)", x = "Salmon biomass (kg)", title = "") +
  theme_classic() +
  theme(plot.title = element_text(face="bold", hjust = -0.7), 
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Convert to percentage format
  scale_x_continuous(breaks = seq(0, max(sal$x), by = 5000)) +  # X-axis intervals of 2500
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#1b9e77", alpha = 0.2)
# annotate("text", x = max(sal$x), y = max(sal$conf.high), label = "** p = <0.01", 
#          hjust = 1, vjust = 1, size = 5)  # Add annotation in top-right corner
sal.p


ggarrange(plot_LD_22,sal.p, labels = c("A", "B"))
# ggsave(file="", width=7, height=5, dpi=300)

# Second top model
summary(LD_20)

# Updated coefficients and standard errors for the new model
coef_data_updated <- data.frame(
  term = c(
    "Fishing ~ Salmon", "Fishing ~ Bears", "Fishing ~ Visitors t+2",
    "Alertness ~ Salmon", "Alertness ~ Bears", "Alertness ~ Visitors t+2"
  ),
  estimate = c(
    -0.24910, 0.13851, -0.07914,   # Fishing vs Other (updated estimates)
    -0.845201, -0.803185, 0.006022  # Alertness vs Other (updated estimates)
  ),
  std.error = c(
    0.25339, 0.40020, 0.25579,     # Fishing vs Other (updated standard errors)
    0.274650, 0.539021, 0.260589    # Alertness vs Other (updated standard errors)
  ),
  model = rep(c("Fishing", "Alertness"), each = 3)  # Fishing and Alertness models
)

# Create the updated plot
plot_updated <- dwplot(coef_data_updated, dot_args = list(color = "red"), 
                       whisker_args = list(color = "black")) +
  theme_bw() +
  theme(legend.position = "none") +  # Remove legend
  scale_color_manual(values = c("Fishing" = "black", "Alertness" = "black")) +  # Custom colors 
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey30") +  # Vertical line at 0
  labs(title = "",
       x = "Coefficient Estimate",
       y = "") +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) 

print(plot_updated)

sal2 <- ggpredict(LD_20, terms = "salmon")
# Un-center and scale by multiplying by sd and adding mean
# Mean and SD of salmon to re-center and scale
getsd.and.mean <- data[!is.na(data$total.biomass),]
m <- mean(getsd.and.mean$total.biomass)
sd <- sd(getsd.and.mean$total.biomass)
sal2$x <- (sal2$x * (2*sd)) + m 

sal2 <- sal2 %>% filter(response.level == "Alertness")
sal.p2 <- sal2 %>% plot()+
  labs(y = "P(Alertness)", x = "Salmon biomass (kg)", title = "") +
  theme_classic() +
  theme(plot.title = element_text(face="bold", hjust = -0.7), 
        legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Convert to percentage format
  scale_x_continuous(breaks = seq(0, max(sal2$x), by = 5000)) +  # X-axis intervals of 2500
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "#d95f02", alpha = 0.2)
# annotate("text", x = max(sal$x), y = max(sal$conf.high), label = "** p = <0.01", 
#          hjust = 1, vjust = 1, size = 5)  # Add annotation in top-right corner
sal.p2

ggarrange(plot_updated,sal.p2,  labels = c("A", "B"))
# ggsave(file="", width=7, height=5, dpi=300)
