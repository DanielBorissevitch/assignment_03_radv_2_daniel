grade_calc <- function(assign_03) {
  quiz_01 <- 7
  quiz_02 <- 5
  quiz_03 <- 2
  assign_01 <- 9.75
  assign_02 <- 10

  score_quiz <- mean(c(quiz_01, quiz_02, quiz_03))
  score_assign <- mean(c(assign_01, assign_02, assign_03))

  final_grade <- ((0.15 * score_quiz) + (0.15 * score_assign)) / 0.3

  target_80 <- 7.76
  necessary_exam_grade_80 <- (0.75 * target_80 - 0.3 * final_grade) / 0.45

  target_85 <- 8.26
  necessary_exam_grade_85 <- (0.75 * target_85 - 0.3 * final_grade) / 0.45

  return(
    list(
      grade_pre_exam = final_grade,
      exam_grade_80 = necessary_exam_grade_80,
      exam_grade_85 = necessary_exam_grade_85
    )
  )
}

pips_grade <- function(quiz, assigment, exam, project) {
  final_grade <- quiz * 0.15 + assigment * 0.15 + exam * 0.45 + project *
    0.25
  return(final_grade)
}
