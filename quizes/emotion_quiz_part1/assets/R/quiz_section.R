generate_quiz_question <- function(data) {
  type <- sample(colnames(data), 1)
  selected <- sample(data[[type]], 5)
  
  ask_high <- sample(c(TRUE, FALSE), 1)
  indices <- match(selected, data[[type]])
  if (ask_high) {
    correct <- selected[which.max(indices)]
    question_text <- paste0("Which represents the **highest** level of **", type, "**?")
  } else {
    correct <- selected[which.min(indices)]
    question_text <- paste0("Which represents the **lowest** level of **", type, "**?")
  }
  cat("## ", question_text, " {.quiz-question}\n\n", sep = "")
  for (option in selected) {
    if (option == correct) {
      cat(paste0("- [", option, "]{.correct}\n"))
    } else {
      cat(paste0("- ", option, "\n"))
    }
  }
  cat("\n")
}

generate_quiz_questions <- function(n, data) {
  set.seed(Sys.time())
  for (i in 1:n) {
    generate_quiz_question(data)
  }
}