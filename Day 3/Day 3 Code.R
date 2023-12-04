library(tidyverse)


schematic <- paste0(".", readLines("Day 3/Input.txt"), ".")

unique_chars <- unique(unlist(map(schematic, ~str_split_1(.x, ""))))

# Symbols that indicate adjacent numbers are part numbers
symbols <- unique_chars[!unique_chars %in% c(".", 0:9)]


# Function to check if a position has a symbol nearby
has_symbol_nearby <- function(schematic, symbols, row, cols, current_number) {
  rows <- length(schematic)
  cols <- na.omit(as.numeric(cols))
  
  check_cols <-
    seq(from = c(min(cols) - 1, 1)[which.max(c(min(cols) - 1, 1))], 
        to = c(max(cols) + 1, nchar(schematic[row]))[which.min(c(max(cols) + 1, nchar(schematic[row])))])
  for (r in max(1, row - 1):min(row + 1, rows)) {
    for (c in check_cols) {
      valid <- substring(schematic[r], c, c) %in% symbols
      if (valid) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Function to find all part numbers
sum_part_numbers <- function(schematic, symbols) {
  sum_parts <- 0
  current_number <- ""
  current_cols <- NA
  for (row in 1:length(schematic)) {
    line <- schematic[row]
    for (col in 1:nchar(line)) {
      char <- substring(line, col, col)
      if (grepl('[0-9]', char)) {
        current_number <- paste0(current_number, char)
        current_cols <- c(current_cols, col)
      } else {
        if (nchar(current_number) > 0 && has_symbol_nearby(schematic, symbols, row, current_cols, current_number)) {
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

