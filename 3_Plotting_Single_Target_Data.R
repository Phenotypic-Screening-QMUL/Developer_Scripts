library("gplots")
library("RColorBrewer")
library("matrixStats")
library("plyr")
library("dplyr")
library("data.table")
library("stringr")
library("ggplot2")

# Import Example ST ST_Data
ST_Data <- fread("Developer_Example_Data.csv", stringsAsFactors = F, skip=1338) # read in example ST_Data
ST_Data <- ST_Data[-c(1:4), -c(2, 5)] # This gets rid of the rows and columns we don't need
colnames(ST_Data) <- c("Well", "Nuclear Area", "Nuclear Count") # We then rename the columns to something meaningful 
ST_Data <- ST_Data %>% 
  mutate_at(vars(`Nuclear Area`, `Nuclear Count`), as.numeric) # We then make sure the ST_Data is in numeric format

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
ST_Data <- bind_rows(lapply(rows, function(row) add_row(ST_Data, row)))

# Apply add_cols function to the combined ST_Data
ST_Data <- add_cols(ST_Data)

# List the variables you want to keep - make sure wavelength is correct!
Variable_Names <- c("Row", "Col","Nuclear Area")

# Use your variables list to select the columns
Data_Tidy <- ST_Data %>% 
  select(all_of(Variable_Names))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Tidy %>% 
  mutate(Treatment = case_when(Col %in% c(1:6) ~ "DMSO", 
                               Col %in% c(7:12) ~ "Palbo"))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Labelled %>% 
  mutate(Cells = case_when(Row %in% c("A","B","C","D") ~ "MB231", 
                           Row %in% c("E","F","G","H") ~ "SKMEL"))

#Filter and Split ST_Data As Needed
Data_MB231 <- Data_Labelled %>%
  filter(Cells == "MB231")

Data_SKMEL <- Data_Labelled %>%
  filter(Cells == "SKMEL")

# Histogram with frequency distribution
ggplot(Data_MB231, aes(x = `Nuclear Area`, fill = Treatment)) + 
  geom_histogram(position = "dodge", alpha = 0.8, bins = 100) + 
  theme_bw() + 
  scale_fill_manual(values = c("#999999", "red")) + 
  labs(title = "Nuclear Area Historgram - MB231", x = "Nuclei Area", y = "Frequency") + 
  guides(fill = guide_legend()) + xlim (0,1000)

ggsave("Nuclear Area Historgram MB231.png", plot = last_plot(), width = 6, height = 4, dpi = 300)


# Density plot 
ggplot(Data_SKMEL, aes(x = `Nuclear Area`, fill = Treatment)) + 
  geom_density(alpha = 0.8) + 
  theme_bw() + 
  scale_fill_manual(values = c("#999999", "blue")) + 
  labs(title = "Nuclei Area Density Plot - SKMEL", x = expression("Nuclei Area (" * mu * "M"^"2" * ")"), y = "Density") +
  guides(fill = guide_legend())

ggsave("Nuclear Area Density Plot SKMEL.png", plot = last_plot(), width = 6, height = 4, dpi = 300)


# Plotting the percentage frequency polygon
ggplot(Data_MB231, aes(x = `Nuclear Area`, colour = Treatment)) + 
  geom_freqpoly(aes(y = ..count../sum(..count..)*100), bins = 200, size = 1.2) + 
  theme_bw() + 
  scale_color_manual(values = c("#999999", "red")) + 
  labs(title = "Nuclear Area % Frquency Distribution - MB231", x = "Nuclei Area", y = "Percentage (%)") + 
  guides(color = guide_legend()) + xlim (0,1000)

ggsave("Nuclear Area % Frequency Distribution MB231.png", plot = last_plot(), width = 6, height = 4, dpi = 300)
