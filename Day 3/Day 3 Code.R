# Load required packages from the Tidyverse collection
library(tidyverse)

# Read the schematic from a text file and preprocess it by adding a '.' at the beginning and end of each line
schematic <- paste0(".", readLines("Day 3/Input.txt"), ".")

# Extract all unique characters from the schematic
unique_chars <-
  unique(unlist(map(schematic, ~ str_split_1(.x, ""))))

# Filter out the unique characters to only include symbols (excluding '.' and numeric characters)
symbols <- unique_chars[!unique_chars %in% c(".", 0:9)]

# Function to check for adjacent symbols in the schematic
is_adjacent_symbol <- function(schematic, symbols, row, cols) {
  # Number of rows in the schematic
  rows <- length(schematic) 
  # Convert cols to numeric and remove NA values
  cols <- na.omit(as.numeric(cols))
  # Define the range of columns to check for adjacent symbols
  check_cols <-seq(from = max(c(min(cols) - 1, 1)), to = min(c(max(cols) + 1, nchar(schematic[row]))))
  
  # Iterate over the defined rows and columns to check for adjacent symbols
  for (r in max(1, row - 1):min(row + 1, rows)) {
    for (c in check_cols) {
      if (substring(schematic[r], c, c) %in% symbols) {
        return(list(TRUE, substring(schematic[r], c, c), c, r))
      }
    }
  }
  return(list(FALSE))
}

# Function to sum part numbers based on adjacent symbols
sum_part_numbers <- function(schematic, symbols) {
  # Initialize the sum of parts to 0
  sum_parts <- 0 
  # Initialize current_number as an empty string to store the current sequence of digits
  current_number <- "" 
  # Initialize current_cols as NA to store column indices of the current number
  current_cols <- NA 
  
  # Iterate over each row in the schematic
  for (row in seq_along(schematic)) {
    line <- schematic[row] # Extract the current line from the schematic
    # Iterate over each character in the current line
    for (col in seq_along(strsplit(line, "")[[1]])) {
      char <- substring(line, col, col) # Extract the current character
      if (grepl('[0-9]', char)) { # Check if the character is a digit
        # Append the digit to the current number and record its column
        current_number <- paste0(current_number, char)
        current_cols <- c(current_cols, col)
      } else {
        # When encountering a non-digit character
        if (nchar(current_number) > 0 && # Check if there's a number being processed
            is_adjacent_symbol(schematic, symbols, row, current_cols)[[1]]) { # Check if a symbol is adjacent to the number
          # Add the current number to the sum if an adjacent symbol is found
          sum_parts <- sum_parts + as.numeric(current_number)
        }
        # Reset current_number and current_cols for the next number
        current_number <- ""
        current_cols <- NA
      }
    }
  }
  # Return the total sum of parts
  sum_parts
}

# Execute the function to sum part numbers
sum_part_numbers(schematic, symbols)
#> [1] 546312

# Part 2 ------------------------------------------------------------------

# Function to find numbers adjacent to a given symbol in a schematic
find_adjacent_numbers <- function(schematic, symbols, row, col) {
  # Determine the number of rows in the schematic
  rows <- length(schematic)
  # Determine the number of columns in the schematic (assuming uniform length of rows)
  cols <- nchar(schematic[1]) 
  # Initialize an empty vector to store adjacent numbers found
  adjacent_numbers <- numeric(0) 
  
  # Iterate over the cells surrounding the specified location (row, col)
  for (r in max(1, row - 1):min(row + 1, rows)) {
    for (c in max(1, col - 1):min(col + 1, cols)) {
      if (!(r == row && c == col)) { # Avoid checking the cell containing the symbol itself
        current_char <- substring(schematic[r], c, c) # Extract the character at the current position
        # Check if the character is a number and not one of the specified symbols
        if (!current_char %in% symbols && grepl('[0-9]', current_char)) {
          # Expand search leftward to find the beginning of the number
          left_col <- c
          while (left_col >= 1 && grepl('[0-9]', substring(schematic[r], left_col, left_col))) {
            left_col <- left_col - 1
          }
          # Expand search rightward to find the end of the number
          right_col <- c
          while (right_col <= cols && grepl('[0-9]', substring(schematic[r], right_col, right_col))) {
            right_col <- right_col + 1
          }
          # Extract the complete number spanning from left_col to right_col
          current_number <- substring(schematic[r], left_col + 1, right_col - 1)
          # Add the number to adjacent_numbers if it is unique and valid (greater than 0)
          if (as.numeric(current_number) > 0 && !any(adjacent_numbers == as.numeric(current_number))) {
            adjacent_numbers <- c(adjacent_numbers, as.numeric(current_number))
          }
        }
      }
    }
  }
  
  # Return the vector containing all adjacent numbers found
  return(adjacent_numbers)
}


# Function to find asterisks in a vector of strings
find_asterisks <- function(input_vector) {
  # Initialize an empty dataframe to store row and column indices
  result_df <- data.frame(row = integer(), col = integer())
  # Iterate over each string in the input vector
  for (i in seq_along(input_vector)) {
    current_string <- input_vector[i]
    # Find positions of asterisks in the current string
    asterisk_positions <- gregexpr("\\*", current_string)[[1]]
    # Filter out -1 which indicates no match
    asterisk_positions <-
      asterisk_positions[asterisk_positions != -1]
    # Record the row and col for each asterisk
    for (position in asterisk_positions) {
      result_df <- rbind(result_df, data.frame(row = i, col = position))
    }
  }
  distinct(result_df)
}

# Execute a series of operations to calculate the ratio of adjacent numbers
# Find the rows and columns of asterisks in the schematic
find_asterisks(schematic) %>%
  # Add a column 'symbol' with '*' and repeat the schematic for each row
  mutate(symbol = "*", schematic = list(schematic)) %>%
  # Apply 'find_adjacent_numbers' function to each row
  pmap(., find_adjacent_numbers) %>%
  # Convert the list of adjacent numbers into a tibble
  tibble(value = .) %>%
  # Calculate the length (number of elements) in each list of adjacent numbers
  mutate(length = map_dbl(value, ~ length(.x))) %>%
  # Filter out rows where the length of adjacent numbers is not exactly 2
  filter(length == 2) %>%
  # Calculate the product of the two adjacent numbers
  mutate(prod = map_dbl(value, prod)) %>%
  # Sum up the products to get the final ratio
  summarise(ratio = sum(prod)) %>% 
  pull()
#> [1] 87449461