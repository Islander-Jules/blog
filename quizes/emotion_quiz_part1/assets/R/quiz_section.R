generate_quiz_question <- function(data) {
  # 随机选择讨论的类型
  type <- sample(colnames(data), 1)
  # 从对应列中随机抽取 5 个词作为选项
  selected <- sample(data[[type]], 5)
  
  # 随机决定题目是问“最高”还是“最低”
  ask_high <- sample(c(TRUE, FALSE), 1)
  
  # 根据原始数据中词语的排序确定正确答案
  indices <- match(selected, data[[type]])
  if (ask_high) {
    correct <- selected[which.max(indices)]
    question_text <- paste0("Which represents the **highest** level of **", type, "**?")
  } else {
    correct <- selected[which.min(indices)]
    question_text <- paste0("Which represents the **lowest** level of **", type, "**?")
  }
  
  # 对选项按照原始数据顺序进行排序（从低到高）
  sorted_selected <- selected[order(match(selected, data[[type]]))]
  # 建立选项与排序位置的对应关系（1 表示最低，5 表示最高）
  rank_map <- setNames(seq_along(sorted_selected), sorted_selected)
  
  # 对正确答案，输出完整的正确顺序说明
  correct_explanation <- paste("Correct answer. The correct order is:", paste(sorted_selected, collapse = ", "))
  
  cat("## ", question_text, " {.quiz-question}\n\n", sep = "")
  for (option in selected) {
    if (option == correct) {
      # 正确选项：显示完整排序
      explanation <- correct_explanation
      cat(paste0("- [", option, "]{.correct data-explanation=\"", explanation, "\"}\n"))
    } else {
      # 错误选项：与其他答案进行比较
      current_rank <- rank_map[[option]]
      if (ask_high) {
        # 如果题目问最高程度，则比当前选项高的选项在排序中位于其后面
        higher_options <- sorted_selected[which(seq_along(sorted_selected) > current_rank)]
        if (length(higher_options) > 0) {
          explanation <- paste(option, "is lower than", paste(higher_options, collapse = ", "))
        } else {
          # 理论上不会出现，因为正确答案总是最高（ask_high 为 TRUE时）
          explanation <- paste(option, "is the highest among the selected options.")
        }
      } else {
        # 如果题目问最低程度，则比当前选项低的选项在排序中位于其前面
        lower_options <- sorted_selected[which(seq_along(sorted_selected) < current_rank)]
        if (length(lower_options) > 0) {
          explanation <- paste(option, "is higher than", paste(lower_options, collapse = ", "))
        } else {
          # 理论上不会出现，因为正确答案总是最低（ask_high 为 FALSE时）
          explanation <- paste(option, "is the lowest among the selected options.")
        }
      }
      cat(paste0("- [", option, "]{ data-explanation=\"", explanation, "\" }\n"))
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