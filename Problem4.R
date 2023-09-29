# Problem 4
# -------------------------------------------

library(tidyverse)
library(dplyr)

raw_file <- readLines(con = "UCNG_Table4.txt")

# Extracting variable names
variable_names <- 
  str_split(string = raw_file[1], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

# Processes the rest of the data. Replaces the "|" delimiter with comma 
# and removes spaces
comma_separated_values <- 
  raw_file[3:length(raw_file)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

# Combines the variable names with the rest of the data 
comma_separated_values_with_names <- 
  c(paste(variable_names, collapse = ","),
    comma_separated_values) 

# The processed data is written to a new file
cat(comma_separated_values_with_names, sep = "\n", file = "problem4_processed.txt")

# Reading the processed file
edited_file <- read_csv("problem4_processed.txt")

# Reading the file from problem 2
problem2_file <- read_csv("processed_data.txt")

# Joining the two files by name
joined_data <- left_join(edited_file, problem2_file, by = "name")

# Plotting velocity of each galaxy against their distance from us
joined_data %>% 
  ggplot(aes(x = D, y = cz)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  xlab("Distance from us (D)") + 
  ylab("Velocity of each galaxy (cz)") 

# From the plot we can observe that when the distance from us (D) increases,
# the velocity of each galaxy also increases. From this sample data it looks
# like Hubble's observation is true
