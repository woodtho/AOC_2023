library(tidyverse)
library(stringi)

# Part 1 ------------------------------------------------------------------ 

# On each line, the calibration value can be found by combining the first digit
# and the last digit (in that order) to form a single two-digit number.
# 
# For example:
#   
# 1abc2
# pqr3stu8vwx
# a1b2c3d4e5f
# treb7uchet
# 
# In this example, the calibration values of these four lines are 12, 38, 15,
# and 77. Adding these together produces 142.
#
# Consider your entire calibration document. What is the sum of all of the
# calibration values?
  
# Read data from the file "Day 1/Input.txt"
read.table("Day 1/Input.txt") %>% 
  mutate(
    # Extract all digit sequences from each line in the V1 column
    numbers = str_extract_all(V1, "\\d"),
    # Extract the first digit from each set of numbers
    .first = map_chr(numbers, first),
    # Extract the last digit from each set of numbers
    .last = map_chr(numbers, last),
    # Combine the first and last digits and convert them into a numeric value
    .total = as.numeric(paste0(.first, .last))
  ) %>% 
  # Summarise the data by calculating the sum of the '.total' column
  summarise(.total = sum(.total))
#>   .total
#> 1  54644

# Part 2 ------------------------------------------------------------------

# It looks like some of the digits are actually spelled out with letters: one,
# two, three, four, five, six, seven, eight, and nine also count as valid
# "digits".
# 
# Equipped with this new information, you now need to find the real first and
# last digit on each line. For example:
#   
# two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen
# 
# In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76.
# Adding these together produces 281.
# 
# What is the sum of all of the calibration values?

# Create a named vector 'digit_map' where numbers 1 to 9 are mapped to their
# word representations
digit_map <- set_names(
  1:9,
  c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
)

# Extend 'digit_map' to include a mapping of numbers 1 to 9 to themselves
full_number_map <- c(digit_map, set_names(1:9, 1:9))

# Create a regex pattern 'digit_pattern' to match any of the words in
# 'digit_map'
digit_pattern <- paste(names(digit_map), collapse = "|")

# Read data from "Day 1/Input.txt" and
read.table("Day 1/Input.txt") %>%
  mutate(
    # Extract the first digit or word-digit from each line in V1 column
    .first = str_extract(V1, paste0("\\d|", digit_pattern)),
    # Extract the last digit or word-digit from each line in V1 column. This
    # involves reversing the string, extracting, and then reversing back
    .last = str_extract(stri_reverse(V1),
                        paste0(stri_reverse(digit_pattern), "|\\d")) %>%
      stri_reverse(.)
  ) %>%
  mutate(
    # Map the extracted word-digits to their numerical counterparts using
    # 'full_number_map'
    across(.first:.last, ~ full_number_map[.x]),
    # Concatenate the first and last numbers and convert them to numeric
    .total = as.numeric(paste0(.first, .last))
  ) %>%
  # Summarize the data by calculating the sum of the '.total' column
  summarise(.total = sum(.total))
#>   .total
#> 1  53348