emotions_df <- structure(list(
  Emotion = c("Happiness", "Sadness", "Anger", "Fear", 
              "Surprised", "Excitment", "Relaxation", "Confusion", 
              "Disappointment", "Embarrassment", "Pride"),
  Mild = c("Happy / Cheerful", 
           "Sad / Unhappy / Gloomy", 
           "Annoyed / Frustrated", 
           "Scared / Worried", 
           "Surprised / Startled", 
           "Excited / Eager", 
           "Relaxed / At ease", 
           "Confused", 
           "Disappointed", 
           "Embarrassed", 
           "Proud"),
  Moderate = c("Joyful / Delighted / Elated", 
               "Sorrowful / Dejected / Miserable", 
               "Angry / Mad / Indignant", 
               "Afraid / Anxious / Alarmed", 
               "Astonished / Stunned", 
               "Enthusiastic / Pumped", 
               "Calm / Peaceful", 
               "Puzzled / Bewildered", 
               "Letdown / Disheartened", 
               "Ashamed", 
               "Pleased / Gratified"),
  High = c("Thrilled / Overjoyed / Ecstatic", 
           "Heartbroken / Depressed", 
           "Furious / Enraged / Irate", 
           "Frightened / Terrified", 
           "Amazed / Shocked", 
           "Stoked", 
           "Tranquil", 
           "Perplexed", 
           "Crestfallen", 
           "Humiliated / Mortified", 
           "Triumphant")
), class = "data.frame", row.names = c(NA, -11L))

# 辅助函数：去除字符串两端的空白字符
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

# Function to generate a single quiz question.
# Parameters:
#   emotion: The emotion name (e.g., "Sadness")
#   row_data: The row of the data frame corresponding to the emotion
#   target_level: The target level for the question ("Mild", "Moderate", or "High")
#   n_distractors: The number of distractor (incorrect) options to include.
generate_question <- function(emotion, row_data, target_level, n_distractors = 2) {
  levels_all <- c("Mild", "Moderate", "High")
  
  # --- Select the correct option for the target level ---
  # Split the text in the target cell and randomly choose one phrase as the correct answer.
  target_variants <- trim(unlist(strsplit(as.character(row_data[[target_level]]), "/")))
  correct_option <- sample(target_variants, 1)
  
  # Build the full explanation for a correct answer: show all valid phrases from the target level.
  full_explanation <- paste0("Correct. ", paste(target_variants, collapse = ", "), ' can be used for ', tolower(target_level) , 'of ', tolower(emotion))
  
  # --- Build the distractor pool ---
  # The distractor pool is made up of variants from the other (non-target) levels.
  distractor_pool <- c()
  distractor_levels <- c()
  for (lev in levels_all[levels_all != target_level]) {
    # Get the phrases for this level.
    variants <- trim(unlist(strsplit(as.character(row_data[[lev]]), "/")))
    distractor_pool <- c(distractor_pool, variants)
    # Record the level for each variant.
    distractor_levels <- c(distractor_levels, rep(lev, length(variants)))
  }
  
  # Combine into a data frame for easy tracking.
  distractor_df <- data.frame(option = distractor_pool, level = distractor_levels, stringsAsFactors = FALSE)
  
  # Sample n_distractors from the pool.
  if (nrow(distractor_df) < n_distractors) {
    sampled_idx <- sample(1:nrow(distractor_df), n_distractors, replace = TRUE)
  } else {
    sampled_idx <- sample(1:nrow(distractor_df), n_distractors)
  }
  distractors <- distractor_df[sampled_idx, ]
  
  # --- Combine the correct answer and distractors ---
  # Create a data frame that includes both the correct option and the distractors.
  # The correct option is tagged with the target level.
  correct_df <- data.frame(option = correct_option, level = target_level, stringsAsFactors = FALSE)
  combined_df <- rbind(correct_df, distractors)
  
  # Randomize the order of the options.
  combined_df <- combined_df[sample(nrow(combined_df)), ]
  
  # --- Construct the question text ---
  question_text <- paste0("Which best represents the **", tolower(target_level), "** level of *", tolower(emotion), "*?")
  
  # --- Output the quiz question in Markdown format ---
  cat("## ", question_text, " {.quiz-question}\n\n", sep = "")
  
  # For each option, set the explanation.
  for (i in seq_len(nrow(combined_df))) {
    opt <- combined_df$option[i]
    opt_level <- combined_df$level[i]
    if (opt == correct_option) {
      explanation <- full_explanation
      cat(paste0("- [", opt, "]{.correct data-explanation=\"", explanation, "\"}\n"))
    } else {
      explanation <- paste0("Incorrect. ", opt, " belongs to the ", tolower(opt_level), " level.")
      cat(paste0("- [", opt, "]{ data-explanation=\"", explanation, "\" }\n"))
    }
  }
  cat("\n")
}

# For each emotion in the data frame, generate one question per level.
# You can adjust 'n_distractors' here to change the number of distractor options.
generate_quiz_questions <- function() {
  for (i in 1:nrow(emotions_df)) {
    emotion <- emotions_df$Emotion[i]
    for (lev in c("Mild", "Moderate", "High")) {
      generate_question(emotion, emotions_df[i, ], lev, n_distractors = 3)
    }
  }
}

