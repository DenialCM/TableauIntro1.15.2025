# Load necessary libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(zipcodeR)

# Read in the raw data
dummy_data <- read.csv("dummy_dataset_raw.csv")

# Step 1: Remove demographic data columns (gender and race/ethnicity) from the main dataset
main_data <- dummy_data %>%
  select(-female, -male, -nonbinary, -a_gender_not_listed,
         -White, -Black_or_African_American, -Asian, 
         -American_Indian_or_Alaska_Native, 
         -Native_Hawaiian_or_Other_Pacific_Islander, 
         -Hispanic_or_Latino, -year_of_birth, -profession,-zip_code)

# Step 1: Create a dataframe for q1, q2, and q3 (numeric responses)
q1_q2_q3_data <- dummy_data %>%
  select(record_id, starts_with("q1"), starts_with("q2"), starts_with("q3")) %>%
  pivot_longer(cols = starts_with("q"), 
               names_to = c("question", "year"), 
               names_pattern = "(q\\d)_(\\d+)", 
               values_to = "response") %>%
  mutate(
    responses_numeric = as.numeric(response),  # Convert to numeric for q1, q2, q3
    responses_character = NA_character_       # Assign NA for the character column
  ) %>%
  select(record_id, question, year, responses_numeric, responses_character)

# Step 2: Create a dataframe for q4, q4_coded, and q4_improvements (character responses)
q4_data <- dummy_data %>%
  select(record_id, starts_with("q4")) %>%
  pivot_longer(cols = starts_with("q4"), 
               names_to = c("question", "year"), 
               names_pattern = "(q4.*)_(\\d+)", 
               values_to = "response") %>%
  mutate(
    responses_numeric = NA_real_,              # Assign NA for numeric column
    responses_character = as.character(response)  # Convert to character for q4 questions
  ) %>%
  select(record_id, question, year, responses_numeric, responses_character)

# Step 3: Combine the two dataframes (q1_q2_q3_data and q4_data)
combined_data <- bind_rows(q1_q2_q3_data, q4_data)


# Step 1: Replace 1's with category names
demographic_data <- dummy_data %>%
  # Pivot gender-related columns to long format
  pivot_longer(cols = starts_with(c("female", "male", "nonbinary", "a_gender_not_listed")),
               names_to = "gender",
               values_to = "gender_value") %>%
  filter(gender_value == 1) %>%
  select(-gender_value) %>%
  # Pivot race/ethnicity-related columns to long format
  pivot_longer(cols = starts_with(c("White", "Black_or_African_American", "Asian", 
                                    "American_Indian_or_Alaska_Native", 
                                    "Native_Hawaiian_or_Other_Pacific_Islander", 
                                    "Hispanic_or_Latino")),
               names_to = "race_ethnicity",
               values_to = "race_value") %>%
  filter(race_value == 1) %>%
  select(-race_value) %>%
  # Keep relevant columns for demographics
  select(record_id, gender, race_ethnicity, year_of_birth, profession,zip_code,major_city,county,state) %>% 
  mutate(
    gender = str_replace_all(gender, "_", " "),
    race_ethnicity = str_replace_all(race_ethnicity, "_", " ")
  )



# View the first few rows of the pivoted data and demographic data
head(combined_data)
head(demographic_data)

# Optionally, write the cleaned datasets to CSV files
write.csv(combined_data, "pivoted_data.csv", row.names = FALSE)
write.csv(demographic_data, "demographic_data.csv", row.names = FALSE)
