library(tidyverse)

# Read and preprocess schematic
schematic <- paste0(".", readLines("Day 3/Input.txt"), ".")

# Get unique characters
unique_chars <-  unique(unlist(map(schematic, ~str_split_1(.x, ""))))

# Filter for symbols
symbols <- unique_chars[!unique_chars %in% c(".", 0:9)]

# Check for adjacent symbols
is_adjacent_symbol <- function(schematic, symbols, row, cols) {
  rows <- length(schematic)
  cols <- na.omit(as.numeric(cols))
  check_cols <- seq(from = max(c(min(cols) - 1, 1)), to = min(c(max(cols) + 1, nchar(schematic[row]))))
  
  for (r in max(1, row - 1):min(row + 1, rows)) {
    for (c in check_cols) {
      if (substring(schematic[r], c, c) %in% symbols) {
        return(list(TRUE, substring(schematic[r], c, c), c, r))
      }
    }
  }
  return(list(FALSE))
}

# Sum part numbers based on adjacent symbols
sum_part_numbers <- function(schematic, symbols) {
  sum_parts <- 0
  current_number <- ""
  current_cols <- NA
  for (row in seq_along(schematic)) {
    line <- schematic[row]
    for (col in seq_along(strsplit(line, "")[[1]])) {
      char <- substring(line, col, col)
      if (grepl('[0-9]', char)) {
        current_number <- paste0(current_number, char)
        current_cols <- c(current_cols, col)
      } else {
        if (nchar(current_number) > 0 && is_adjacent_symbol(schematic, symbols, row, current_cols)[[1]]) {
          sum_parts <- sum_parts + as.numeric(current_number)
        }
        current_number <- ""
        current_cols <- NA
      }
    }
  }
  sum_parts
}

sum_part_numbers(schematic, symbols)

# Part 2 ------------------------------------------------------------------

find_adjacent_numbers <- function(schematic, symbols, row, col) {
  rows <- length(schematic)
  cols <- nchar(schematic[1])
  adjacent_numbers <- numeric(0)
  
  for (r in max(1, row - 1):min(row + 1, rows)) {
    for (c in max(1, col - 1):min(col + 1, cols)) {
      if (!(r == row && c == col)) { # Skip the center position where the symbol is
        current_char <- substring(schematic[r], c, c)
        if (!current_char %in% symbols && grepl('[0-9]', current_char)) {
          # Expand left and right to find the full number
          left_col <- c
          while (left_col >= 1 && grepl('[0-9]', substring(schematic[r], left_col, left_col))) {
            left_col <- left_col - 1
          }
          right_col <- c
          while (right_col <= cols && grepl('[0-9]', substring(schematic[r], right_col, right_col))) {
            right_col <- right_col + 1
          }
          current_number <- substring(schematic[r], left_col + 1, right_col - 1)
          if (as.numeric(current_number) > 0 && !any(adjacent_numbers == as.numeric(current_number))) {
            adjacent_numbers <- c(adjacent_numbers, as.numeric(current_number))
          }
        }
      }
    }
  }
  
  return(adjacent_numbers)
}


find_asterisks <- function(input_vector) {
  # Initialize an empty dataframe to store row and column indices
  result_df <- data.frame(row = integer(), col = integer())
  
  # Iterate over each string in the input vector
  for (i in seq_along(input_vector)) {
    current_string <- input_vector[i]
    
    # Find positions of asterisks in the current string
    asterisk_positions <- gregexpr("\\*", current_string)[[1]]
    
    # Filter out -1 which indicates no match
    asterisk_positions <- asterisk_positions[asterisk_positions != -1]
    
    # Record the row and col for each asterisk
    for (position in asterisk_positions) {
      result_df <- rbind(result_df, data.frame(row = i, col = position))
    }
  }
  
  distinct(result_df)
}


# Calculate ratio of adjacent numbers
find_asterisks(schematic) %>% 
  mutate(symbol = "*", schematic = list(schematic)) %>%
  pmap(., find_adjacent_numbers) %>% 
  tibble(value = .) %>% 
  mutate(length = map_dbl(value, ~length(.x))) %>% 
  filter(length == 2) %>% 
  mutate(prod = map_dbl(value, prod)) %>% 
  summarise(ratio = sum(prod))
