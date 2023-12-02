library(tidyverse)

# Part 1 ------------------------------------------------------------------

# Determine which games would have been possible if the bag had been loaded with
# only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the
# IDs of those games?

# Define maximum allowed values for each color in the game
max_allowed_colors <- list(
  red = 12,
  green = 13,
  blue = 14
)

# Load and preprocess game data from a file

# '~' is used as the delimiter so each row in the file is parsed as one value
game_data <- read.table("Day 2/Input.txt", sep = "~") %>%   
  # Split 'V1' into 'game_id' and 'game_picks'
  separate(V1, into = c("game_id", "game_picks"), sep = ": ") %>% 
  # Create separate rows for each game entry
  separate_rows(game_picks, sep = "; ") %>%   
  # Group by player id
  group_by(game_id) %>%
  # Assign a sequential number to each game entry
  mutate(pick_id = row_number(),
         # Convert game_id to a numeric value
         game_id = parse_number(game_id)) %>%
  # Split entries into separate rows
  separate_rows(game_picks, sep = ", ") %>%
  # Split game_picks into 'score' and 'color'
  separate(game_picks, into = c("score", "color")) %>%  
  # Transform data to wide format with colors as columns
  pivot_wider(names_from = color, values_from = score) %>% 
  # Convert character columns to numeric, replacing NAs with 0
  mutate(across(where(is.character), ~replace_na(parse_number(.x), 0))) 

# Evaluate game data against the defined color constraints and sum valid player IDs
valid_game_summary <- game_data %>% 
  # Validate each colour against maximum allowed value
  mutate(valid_red = all(red <= max_allowed_colors$red),       
         valid_green = all(green <= max_allowed_colors$green),
         valid_blue = all(blue <= max_allowed_colors$blue)) %>% #
  # Check if all color validations are true
  summarise(all_colors_valid = all(valid_red, valid_green, valid_blue)) %>% 
  # Keep only rows where all color validations are true
  filter(all_colors_valid) %>%   
  # Sum the player IDs of valid entries
  summarise(total_valid_game_ids = sum(game_id))          


# Part 2 ------------------------------------------------------------------

# The power of a set of cubes is equal to the numbers of red, green, and blue
# cubes multiplied together. For each game, find the minimum set of cubes that
# must have been present. What is the sum of the power of these sets?

# Calculate the maximum combined power based on the color scores
max_combined_power <- game_data %>%
  # Determine the maximum possible for each colour
  summarise(
    max_red = max(red),
    max_green = max(green),
    max_blue = max(blue),  
    # Calculate the 'power' as the product of r, g, and b
    power = r * g * b) %>%                         
  # Sum the power for all rows
  summarise(power = sum(power))  
            