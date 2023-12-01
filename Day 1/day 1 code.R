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
  
read.table("Day 1/Input.txt") %>% 
  mutate(numbers = str_extract_all(V1, "\\d"),
         .first = map_chr(numbers, first),
         .last = map_chr(numbers, last),
         .sum = as.numeric(paste0(.first, .last))) %>% 
summarise(total = sum(.sum))


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

digit_map <- set_names(
    1:9,
    c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    )

full_number_map <- c(digit_map, set_names(1:9, 1:9))

digit_pattern <- paste(names(digit_map), collapse = "|")

read.table("Day 1/Input.txt") %>%
  mutate(
    .first = str_extract(V1, paste0("\\d|", digit_pattern)),
    .last = str_extract(stri_reverse(V1),
                        paste0(stri_reverse(digit_pattern), "|\\d")) %>%
      stri_reverse(.)
  ) %>%
  mutate(across(.first:.last, ~ full_number_map[.x]),
         .sum = as.numeric(paste0(.first, .last))) %>%
  summarise(total = sum(.sum))
  