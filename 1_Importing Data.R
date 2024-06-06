library("gplots")
library("RColorBrewer")
library("matrixStats")
library("plyr")
library("dplyr")
library("data.table")
library("stringr")
library("ggplot2")

# Import Summary Data
Summary_Data <- fread("Import_Test.csv", stringsAsFactors = F) # This reads in your csv but developer has an unhelpful format
Summary_Data <- Summary_Data[-c(1:4), -c(2, 5)] # This gets rid of the rows and columns we don't need
colnames(Summary_Data) <- c("Well", "Nuclear Area", "Nuclear Count") # We then rename the columns to something meaningful 
Summary_Data <- Summary_Data %>% 
  mutate_at(vars(`Nuclear Area`, `Nuclear Count`), as.numeric) # We then make sure the Summary_Data is in numeric format

# Import Single Target ST_Data
ST_Data <- fread("Import_Test.csv", stringsAsFactors = F, skip = 1550) # As above - but we have to skip - NOTE: The rows needed to skip will change based on your plate layout 
ST_Data <- ST_Data[-c(1:4), -c(2, 5)] # This gets rid of the rows and columns we don't need
colnames(ST_Data) <- c("Well", "Nuclear Area", "Nuclear Count") # We then rename the columns to something meaningful 
ST_Data <- ST_Data %>% 
  mutate_at(vars(`Nuclear Area`, `Nuclear Count`), as.numeric) # We then make sure the ST_Data is in numeric format
ST_Data <- ST_Data %>% 
  mutate(Index = row_number()) # This get gives each cell a unique identifier

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

# Apply add_cols function to the combined data
Summary_Data <- add_cols(Summary_Data)

# Apply add_row function to each row in the list and combine results
ST_Data <- bind_rows(lapply(rows, function(row) add_row(ST_Data, row)))

# Apply add_cols function to the combined data
ST_Data <- add_cols(ST_Data)


