library("gplots")
library("RColorBrewer")
library("matrixStats")
library("plyr")
library("dplyr")
library("data.table")
library("stringr")
library("ggplot2")

# Import Example Summary Summary_Data
Summary_Data <- fread("Developer_Example_Data.csv", stringsAsFactors = F) # read in example Summary_Data
Summary_Data <- Summary_Data[-c(1:4,101:264650), -c(2, 5)] # This gets rid of the rows and columns we don't need
colnames(Summary_Data) <- c("Well", "Nuclear Area", "Nuclear Count") # We then rename the columns to something meaningful 
Summary_Data <- Summary_Data %>% 
  mutate_at(vars(`Nuclear Area`, `Nuclear Count`), as.numeric) # We then make sure the Summary_Data is in numeric format

# Add Rows Function
add_row <- function(df, row) {
  df %>%
    filter(str_detect(Well, row)) %>%
    mutate(Row = row)
}

# Add Columns Function
add_cols <- function(df) {
  df %>%
    mutate(Col = str_extract(Well, "(?<= - )\\d+"))
}

# List of rows to add - don't need to specify!
rows <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P")

# Apply add_row function to each row in the list and combine results
Summary_Data <- bind_rows(lapply(rows, function(row) add_row(Summary_Data, row)))

# Apply add_cols function to the combined Summary_Data
Summary_Data <- add_cols(Summary_Data)

# List the variables you want to keep - make sure wavelength is correct!
Variable_Names <- c("Row", "Col", "Nuclear Count", "Nuclear Area")

# Use your variables list to select the columns
Data_Tidy <- Summary_Data %>% 
  select(all_of(Variable_Names))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Tidy %>% 
  mutate(Treatment = case_when(Col == 1:6 ~ "DMSO", 
                               Col == 7:12 ~ "Palbo"))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Labelled %>% 
  mutate(Cells = case_when(Row %in% c("A","B","C","D") ~ "MB231", 
                           Row %in% c("E","F","G","H") ~ "SKMEL"))

#Filter and Split Summary_Data As Needed
Data_MB231 <- Data_Labelled %>%
  filter(Cells == "MB231")

Data_SKMEL <- Data_Labelled %>%
  filter(Cells == "SKMEL")

#Making and Saving Plots
# Make a Nuclear Count Plot - MB231
ggplot(Data_MB231, aes(x = Treatment, y = `Nuclear Count`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_point(position = position_dodge(width = .9), color = "blaCK") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231", x = "Treatment", y = "Nuclei Count") +
  guides(fill = guide_legend())

ggsave("Nuclear Count Plot MB231.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

# Make a Nuclear Area Plot - SKMEL
ggplot(Data_SKMEL, aes(x = Treatment, y = `Nuclear Area`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_point(position = position_dodge(width = .9), color = "blaCK") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "blue")) +
  labs(title = "Nuclear Area - SKMEL", x = "Treatment", y = expression("Nuclear Area (" * mu * "M"^"2" * ")")) +
  guides(fill = guide_legend())

ggsave("Nuclear Area Plot SKMEL1.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

## Adding Error Bars
# Calculate mean and standard deviation for each treatment group
summary_stats <- Data_MB231 %>%
  group_by(Treatment) %>%
  summarise(mean_count = mean(`Nuclear Count`), 
            sd_count = sd(`Nuclear Count`))

# Remake a Nuclear Count Plot - MB231
ggplot(Data_MB231, aes(x = Treatment, y = `Nuclear Count`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_errorbar(data = summary_stats, aes(y = mean_count, ymin = mean_count - sd_count, ymax = mean_count + sd_count),
                width = 0.4, position = position_dodge(width = 0.9)) +
  geom_point(position = position_dodge(width = .9), color = "blaCK") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231", x = "Treatment", y = "Nuclei Count") +
  guides(fill = guide_legend())

ggsave("Nuclear Count Plot MB231 with Error Bar.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

# Running a t-test

# Load ggpubr package for statistical testing
library(ggpubr)

# Conduct t-test
ttest <- t.test(`Nuclear Count` ~ Treatment, data = Data_MB231)

# Extract p-value from t-test result
p_value <- ttest$p.value

# Determine significance level
significance_level <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))

# Remake a Nuclear Count Plot - MB231
ggplot(Data_MB231, aes(x = Treatment, y = `Nuclear Count`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_errorbar(data = summary_stats, aes(y = mean_count, ymin = mean_count - sd_count, ymax = mean_count + sd_count),
                width = 0.4, position = position_dodge(width = 0.9)) +
  geom_point(position = position_dodge(width = .9), color = "black") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231", x = "Treatment", y = "Nuclei Count") +
  guides(fill = guide_legend()) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = list(c("DMSO", "Palbo"))) +
  annotate("text", x = 1.5, y = max(Data_MB231$`Nuclear Count`), 
           label = significance_level,
           vjust = -1.5, hjust = 0.5, size = 6, color = "black")

ggsave("Nuclear Count Plot MB231 with Error Bar and Significance.png", plot = last_plot(), width = 6, height = 4, dpi = 300)



