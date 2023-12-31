
# Skeleton file 1 for Assignment 1 in BAN400. 
# -------------------------------------------

library(tidyverse)

# Comments below describes briefly a set of steps that solves Problem 1.

# Read the entire data file into memory using the readLines()-function. Use the
# URL direcly or read the data from the local file that is in the repository.

data_file <- readLines(con = "suites_dw_Table1.txt")


# Identify the line number L (14 in this case) of the separator line between the 
# column names and the rest of the data table.
# Opening the txt file and counting rows



# Save the variable descriptions (i.e. the information in lines 1:(L-2)) in a
# text-file for future reference using the cat()-function

top_data <- data_file[1:12]
cat(top_data, sep = "\n", file = "top_data.txt")
extracted_data <- data_file[13:length(data_file)]


# Extract the variable names (i.e. line (L-1)), store the names in a vector.

heading <- extracted_data[1]


# Read the data. One way to do this is to rewrite the data to a new .csv-file
# with comma-separators for instance using cat() again, with the variable names
# from the step above on the first line (see for instance paste() for collapsing
# that vector to a single string with commas as separators).

# Read the finished .csv back into R in the normal way.


