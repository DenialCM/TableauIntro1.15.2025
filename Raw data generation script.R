library(tidyverse)

set.seed(42)

# Function to generate random data for q1, q2, q3, q4
generate_q1 <- function() {
  sample(0:3, 1)
}

generate_q2 <- function() {
  round(runif(1, 0, 100), 1)
}

generate_q3 <- function() {
  sample(-2:2, 1)
}

set.seed(42)

# Updated function to generate open-ended responses based on the pattern
generate_q4 <- function() {
  sample(c(
    "The program was OK, but I think it could be improved by including more interactive elements.",
    "I feel like the program was somewhat helpful, but it lacked some practical aspects that would have made it more useful.",
    "It was an average experience. The program could have been more engaging and better organized.",
    "The program was fine, but I expected more personalized support. I think that would make it more effective.",
    "I didn't find the program all that helpful, but it wasn’t bad either. Some more detailed examples could have helped.",
    "The content was decent, but it could use more depth. Overall, the program is OK for what it is.",
    "I appreciate the program, but it needs more hands-on activities to really keep participants engaged.",
    "I’m not sure if I found it helpful, but it wasn’t terrible. I think more real-world application would make it better.",
    "It’s an OK program, but I would suggest improving the pace and adding more time for questions.",
    "The program is somewhat helpful, but I think it could be more tailored to individual needs.",
    "It was OK, but I think it could be better with clearer examples and more detailed instructions.",
    "The program was fine, but I felt like it lacked real-world application. More case studies would be great.",
    "I think the program is good, but it could use some improvements in terms of structure and pacing.",
    "It wasn’t exactly what I expected, but it’s OK. More engaging activities would have been helpful.",
    "I wasn’t sure how much I gained from it, but the program was structured OK. Some improvements are needed.",
    "It could be more engaging and less lecture-based. Otherwise, it was an OK program.",
    "The program was decent, but I would have liked more feedback on my progress.",
    "I think the program is good, but it could be more hands-on and focused on real-world problems.",
    "It’s an average program. I think it could be improved by breaking things into smaller, more digestible parts.",
    "It was fine overall, but I feel it lacked more interactive elements to make it more engaging.",
    "I liked the program, but some areas need more in-depth coverage, especially on the more advanced topics.",
    "The program was pretty good, but I think it could have been a bit more organized and practical.",
    "I appreciate the effort, but the program felt too broad and could be more focused on specific needs.",
    "It’s OK, but I think it would be better if the pace was slowed down and more time was given to practice.",
    "I think the program was OK, but there were too many long lectures. More interactive activities would help.",
    "The program wasn’t bad, but it could use more structured materials to help guide participants.",
    "It wasn’t exactly useful for me, but it wasn’t terrible. More personalization would help.",
    "I didn’t find the program very helpful. It needs more real-world applications.",
    "It’s an OK program, but I think the content could be more tailored to different learning styles.",
    "The program had its ups and downs, but overall, I found it helpful for basic understanding.",
    "The program is OK, but I didn’t feel like I learned as much as I had hoped.",
    "It was decent, but I think it would benefit from some improvements in the presentation and materials.",
    "I liked the overall structure, but I felt it could have gone deeper into certain topics.",
    "The program was fine, but I didn’t feel it was that engaging. More activities would be helpful.",
    "The material was OK, but the pace was too fast for me to keep up. More time would have been great.",
    "I think the program could have been a lot better with more hands-on exercises and fewer lectures.",
    "It was an OK program. Some of the topics could use more detailed explanations, though.",
    "I didn’t find the program that helpful. More interactive activities and discussions would have been useful.",
    "The program was OK, but I think it could have used more visual aids to help with understanding.",
    "It could use some adjustments. Some sections were very helpful, but others felt like a waste of time.",
    "The program was fine, but I didn’t feel fully engaged. More group work could help with that.",
    "The content was useful, but I think the program would benefit from more examples and case studies.",
    "It was decent, but I would have appreciated more personalized feedback on my progress.",
    "It was a decent program, but it could have been a lot better with more activities and fewer lectures.",
    "The program was OK, but I think the content was a bit repetitive. A more diverse range of topics would be great.",
    "I feel like the program was OK overall, but it needs more depth in certain areas.",
    "I think the program is good, but it could be improved by allowing more time for questions and discussion.",
    "It was decent, but it wasn’t very engaging. More interactive workshops would be appreciated.",
    "I found it helpful, but I think it could benefit from more real-world examples and less theory.",
    "It was OK, but I feel like the pace was too fast. More time for each topic would be beneficial.",
    "The program was OK, but I think it could be a lot better with more hands-on learning opportunities.",
    "It was an average program. It could have been more engaging with some additional activities.",
    "The program was OK, but I didn’t feel that it addressed my specific needs.",
    "I think the program could be better if there was more opportunity for collaboration with others.",
    "It’s an OK program. More opportunities for feedback would have been helpful.",
    "The program was fine, but I think it could have benefited from more focus on practical applications.",
    "The program was good, but it would be even better with more interactive sessions and group discussions.",
    "The content was helpful, but I think it could have been better organized with a clearer structure.",
    "It’s OK, but I feel like I needed more time to fully absorb the content.",
    "I liked the program, but I think it could use more focus on problem-solving and hands-on exercises.",
    "I think the program could be improved by making it more interactive and engaging with real-world examples.",
    "The program was OK, but I would have liked more practical demonstrations of the concepts.",
    "The content was helpful, but I think it could have been organized better for easier understanding.",
    "I didn’t find the program that helpful, but it wasn’t a waste of time either.",
    "The program was decent, but more personalized feedback would have helped me understand better.",
    "The program could be more effective with more focus on the participants' learning styles.",
    "I feel like the program was OK, but I think more examples would have helped make the content clearer.",
    "It was good, but I think it could benefit from more group collaboration and problem-solving exercises.",
    "I liked the program, but it could be better with more real-life examples and practical applications.",
    "The program was fine, but I think more interactive exercises could help with better engagement.",
    "I felt the program was OK, but there could have been more individual support.",
    "It was OK, but I think the program could have been better with more engagement and fewer lectures.",
    "I found the program helpful, but it could use some tweaks, especially with pacing and content depth."
  ), 1)
}

# Function to categorize responses into five coded categories
generate_q4_coded <- function(response) {
  if (grepl("helpful|useful|learned", response, ignore.case = TRUE)) {
    return("Positive")
  } else if (grepl("not helpful|not useful|waste", response, ignore.case = TRUE)) {
    return("Negative")
  } else if (grepl("improve|better|suggest", response, ignore.case = TRUE)) {
    return("Improvement")
  } else if (grepl("engaging|interactive", response, ignore.case = TRUE)) {
    return("Engagement")
  } else {
    return("Neutral")
  }
}

# Function to extract improvement categories
generate_q4_improvements <- function(response) {
  # Match improvement-related keywords and categorize the response
  if (grepl("interactive|hands-on|engage|activities|group work|participation|engaging|workshops", response, ignore.case = TRUE)) {
    return("Interactivity")
  } else if (grepl("content|depth|examples|clarity|structure|material|detailed|explanation", response, ignore.case = TRUE)) {
    return("Content Depth")
  } else if (grepl("pacing|time|duration|speed|slow down|more time", response, ignore.case = TRUE)) {
    return("Pacing")
  } else if (grepl("feedback|personalized|assessment|evaluation|questions", response, ignore.case = TRUE)) {
    return("Feedback")
  } else if (grepl("real-world|case study|practical|application|real-life", response, ignore.case = TRUE)) {
    return("Real-World Examples")
  } else if (grepl("focus|clarity|organization|structure", response, ignore.case = TRUE)) {
    return("Focus/Structure")
  } else {
    return(NA)
  }
}



# Generate 500 responses and their coded categories
responses <- sapply(1:500, function(x) generate_q4())
q4_coded_2022 <- sapply(responses, generate_q4_coded)
q4_improvements_2022 <- sapply(responses, generate_q4_improvements)

# Create the data frame with these variables
response_df <- data.frame(
  record_id = 1:500,
  q4_2022 = responses,
  q4_coded_2022 = q4_coded_2022,
  q4_improvements_2022 = q4_improvements_2022
)

# Display the first few rows of the data
head(response_df)


# Pool of 100+ unique responses for 2023 with positive sentiment and some suggestions for improvement
generate_q4_2023 <- function() {
  sample(c(
    "The program was very helpful, and I learned a lot. A few more interactive activities would improve it further.",
    "I found the program very beneficial. It was well-organized, though adding more real-world examples would be valuable.",
    "It was a great experience overall. The content was engaging, but a bit more depth on certain topics could be helpful.",
    "I really liked the program, but I think it could be more interactive to engage participants even more.",
    "The program was fantastic. Some slight improvements in pacing and content depth would make it even better.",
    "It was a great program. More personalized feedback would help me understand the material better.",
    "The program was excellent overall, but a few more hands-on activities would make it even more effective.",
    "I feel like the program was very useful. More engagement in certain sections would help retain attention.",
    "The program was really good, but I think it could have been more tailored to individual needs.",
    "I found the program to be very informative, but the pace was a bit quick. More time for questions would help.",
    "The program was great, but I think a more structured timeline for each module would have been beneficial.",
    "I really appreciated the program. It would be even better with more diverse perspectives included.",
    "I thought the program was helpful, but adding more practical exercises would improve the learning experience.",
    "The content was clear and useful, but a bit more focus on practical applications would have been nice.",
    "It was a good program. A more interactive approach would help keep participants engaged longer.",
    "The program was okay, but I think it would benefit from a clearer focus on the main topics.",
    "I enjoyed the program, but I felt some sections were rushed and could use more time for explanation.",
    "It was a good experience, but a few more examples of how the material applies in real-life situations would be helpful.",
    "The program was solid, though I would have liked more feedback during the process to gauge my progress.",
    "I liked the program overall, but it would be more beneficial if it included more diverse learning formats."
  ), 1)
}

# Pool of 100+ unique responses for 2024 with highly positive sentiment and fewer improvement suggestions
generate_q4_2024 <- function() {
  sample(c(
    "The program was amazing! I learned so much and everything was very well-organized.",
    "I found the program extremely helpful. It exceeded my expectations, and I feel confident in the material.",
    "The program was fantastic. I feel it met all my learning needs and was incredibly informative.",
    "It was an excellent program. The content was delivered perfectly, and everything was well-paced and engaging.",
    "The program was great. I feel more confident in the material, and the organization was top-notch.",
    "I had an excellent experience. The program was engaging, informative, and provided everything I needed to succeed.",
    "The program was fantastic. I learned a lot, and I think it covered everything I hoped for and more.",
    "I couldn’t have asked for a better program. It was well-structured, and I feel more prepared after completing it.",
    "It was an excellent program. The material was engaging, and I feel confident applying what I’ve learned.",
    "I loved the program! It was highly informative and organized, and I feel much more knowledgeable.",
    "The program was wonderful. It really exceeded my expectations and left me feeling confident about the material.",
    "I feel like the program was perfect. It covered everything I needed, and the structure was flawless.",
    "I learned so much from the program. Everything was clear, engaging, and incredibly useful.",
    "The program was spot-on. I felt like all my learning needs were met, and I feel prepared to apply the material.",
    "It was a truly great program. It was well-organized, informative, and really helped me build my skills.",
    "I can't think of anything that could have been improved. The program was comprehensive and engaging.",
    "The program was extremely effective. The content was delivered perfectly, and I feel much more confident in the material.",
    "I had an incredible experience with the program. The content was clear, engaging, and well-paced.",
    "It was a wonderful experience. Everything from the material to the pace was on point, and I feel fully prepared."
  ), 1)
}

# Function to categorize responses into five coded categories (used for all years)
generate_q4_coded <- function(response) {
  if (grepl("helpful|useful|learned|beneficial|fantastic", response, ignore.case = TRUE)) {
    return("Positive")
  } else if (grepl("not helpful|not useful|waste", response, ignore.case = TRUE)) {
    return("Negative")
  } else if (grepl("improve|better|suggest", response, ignore.case = TRUE)) {
    return("Improvement")
  } else if (grepl("engaging|interactive", response, ignore.case = TRUE)) {
    return("Engagement")
  } else {
    return("Neutral")
  }
}

# Function to extract improvement categories (used for all years)
generate_q4_improvements <- function(response) {
  if (grepl("interactive|hands-on|engage", response, ignore.case = TRUE)) {
    return("Interactivity")
  } else if (grepl("content|depth|examples", response, ignore.case = TRUE)) {
    return("Content Depth")
  } else if (grepl("pacing|time", response, ignore.case = TRUE)) {
    return("Pacing")
  } else if (grepl("feedback|personalized", response, ignore.case = TRUE)) {
    return("Feedback")
  } else if (grepl("real-world|case study", response, ignore.case = TRUE)) {
    return("Real-World Examples")
  } else {
    return(NA)
  }
}

# Generate 500 responses for each year with adjusted positivity and fewer improvement suggestions
responses_2022 <- sapply(1:500, function(x) generate_q4())
responses_2023 <- sapply(1:500, function(x) generate_q4_2023())
responses_2024 <- sapply(1:500, function(x) generate_q4_2024())

q4_coded_2022 <- sapply(responses_2022, generate_q4_coded)
q4_improvements_2022 <- sapply(responses_2022, generate_q4_improvements)

q4_coded_2023 <- sapply(responses_2023, generate_q4_coded)
q4_improvements_2023 <- sapply(responses_2023, generate_q4_improvements)

q4_coded_2024 <- sapply(responses_2024, generate_q4_coded)
q4_improvements_2024 <- sapply(responses_2024, generate_q4_improvements)

# Create the data frame for each year
response_df_2022 <- data.frame(
  record_id = 1:500,
  q4_2022 = responses_2022,
  q4_coded_2022 = q4_coded_2022,
  q4_improvements_2022 = q4_improvements_2022
)

response_df_2023 <- data.frame(
  record_id = 1:500,
  q4_2023 = responses_2023,
  q4_coded_2023 = q4_coded_2023,
  q4_improvements_2023 = q4_improvements_2023
)

response_df_2024 <- data.frame(
  record_id = 1:500,
  q4_2024 = responses_2024,
  q4_coded_2024 = q4_coded_2024,
  q4_improvements_2024 = q4_improvements_2024
)

# Combine the data for 2022, 2023, and 2024 into one data frame
full_response_df <- data.frame(
  record_id = 1:500,
  q4_2022 = responses_2022,
  q4_coded_2022 = q4_coded_2022,
  q4_improvements_2022 = q4_improvements_2022,
  q4_2023 = responses_2023,
  q4_coded_2023 = q4_coded_2023,
  q4_improvements_2023 = q4_improvements_2023,
  q4_2024 = responses_2024,
  q4_coded_2024 = q4_coded_2024,
  q4_improvements_2024 = q4_improvements_2024
)

# Display the first few rows of the combined dataset
head(full_response_df)

#generate gender function
generate_gender <- function() {
  list(
    female = sample(0:1, 1),
    male = sample(0:1, 1),
    nonbinary = sample(0:1, 1),
    not_listed = sample(0:1, 1)
  )
}

#generate race_ethnicity function
generate_race_ethnicity <- function() {
  race_categories <- c('White', 'Black', 'Asian', 'Hispanic', 'Native American', 'Pacific Islander', 'Other')
  race_data <- sample(0:1, length(race_categories), replace = TRUE)
  names(race_data) <- race_categories
  return(race_data)
}

generate_profession <- function() {
  sample(c('Engineer', 'Teacher', 'Artist', 'Doctor', 'Scientist'), 1)
}

# Create the dataset for 500 participants
n_participants <- 500
data <- list()


# Gender and Race/Ethnicity options
gender_options <- c("female", "male", "nonbinary", "a gender not listed")
race_options <- c("White", "Black or African American", "Asian", "American Indian or Alaska Native", 
                  "Native Hawaiian or Other Pacific Islander", "Hispanic or Latino")

# Sample functions for gender and race/ethnicity
generate_gender <- function() {
  # Generate a vector of gender columns with 1 for the selected gender(s) and 0 for others
  selected_genders <- sample(gender_options, sample(1:3, 1), replace = TRUE)
  gender_vector <- as.integer(gender_options %in% selected_genders)
  return(gender_vector)
}

generate_race_ethnicity <- function() {
  # Generate a vector of race/ethnicity columns with 1 for the selected race(s) and 0 for others
  selected_race <- sample(race_options, sample(1:3, 1), replace = TRUE)
  race_vector <- as.integer(race_options %in% selected_race)
  return(race_vector)
}

generate_profession <- function() {
  sample(c("Teacher", "Engineer", "Artist", "Manager", "Scientist"), 1)
}

# Random value generation for the other variables
generate_q1 <- function() sample(0:3, 1)
generate_q2 <- function() round(runif(1, 0, 100), 1)
generate_q3 <- function() sample(-2:2, 1)



# Create the dataset for 500 participants
for (i in 1:n_participants) {
  q4_2022 <- generate_q4()
  q4_2023 <- generate_q4_2023()
  q4_2024 <- generate_q4_2024()
  
  # Generate gender and race columns as vectors
  gender_vector <- generate_gender()
  race_vector <- generate_race_ethnicity()
  

  
  # Store the participant's data in a list
  participant <- c(
    record_id = i,
    year_of_birth = sample(1976:1999, 1),
    profession = generate_profession(),
    q1_2022 = generate_q1(),
    q2_2022 = generate_q2(),
    q3_2022 = generate_q3(),
    q4_2022 = q4_2022,
    q4_coded_2022 = generate_q4_coded(q4_2022),
    q4_improvements_2022 = generate_q4_improvements(q4_2022),
    q1_2023 = generate_q1(),
    q2_2023 = generate_q2(),
    q3_2023 = generate_q3(),
    q4_2023 = q4_2023,
    q4_coded_2023 = generate_q4_coded(q4_2023),
    q4_improvements_2023 = generate_q4_improvements(q4_2023),
    q1_2024 = generate_q1(),
    q2_2024 = generate_q2(),
    q3_2024 = generate_q3(),
    q4_2024 = q4_2024,
    q4_coded_2024 = generate_q4_coded(q4_2024),
    q4_improvements_2024 = generate_q4_improvements(q4_2024),
    # Add gender and race/ethnicity as individual columns
    female = gender_vector[1], 
    male = gender_vector[2], 
    nonbinary = gender_vector[3],
    a_gender_not_listed = gender_vector[4],
    White = race_vector[1],
    Black_or_African_American = race_vector[2],
    Asian = race_vector[3],
    American_Indian_or_Alaska_Native = race_vector[4],
    Native_Hawaiian_or_Other_Pacific_Islander = race_vector[5],
    Hispanic_or_Latino = race_vector[6]
  )
  
  data[[i]] <- participant
}

# Convert the list of participants into a data frame
final_data <- do.call(rbind, data)

# Convert the list into a data frame for easier viewing and manipulation
final_data_df <- as.data.frame(final_data)

#Add zip code

library(zipcodeR)
download_zip_data()
zip_data<-zip_code_db %>% 
  select(zipcode,major_city,county, state)


final_data_df <- final_data_df %>%
  mutate(zip_code = sample(zip_data$zipcode, n(), replace = TRUE)) %>% 
  left_join(zip_data,
            by=c("zip_code"="zipcode")) %>% 
  mutate(zip_code = paste0("'", zip_code))



# Display the first few rows of the dataset
head(final_data_df)

# Write to a CSV file
write.csv(final_data_df, "dummy_dataset_raw.csv", row.names = FALSE)
