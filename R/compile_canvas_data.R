#' Compile a dataset from Canvas
#'
#' @param course_id The Canvas ID number for the course/module
#' @param sub_id The Canvas ID number for the assignment
#' @param sub_type Type of submission. String. Possible values are "assignments" or "quizzes".
#'
#' @return A tibble of Canvas data combining student infomation, submission information, and sections
#' @export
#'
#' @examples
#'  \dontrun{
#'  compile_canvas_data(course_id = 12345, sub_id = 67890)
#' }
#'
compile_canvas_data <- function(course_id, sub_id, sub_type = "assignments"){
  suppressWarnings({
    message("Getting student info...")
    students <- get_students(course_id)
    message("Getting section info...")
    sections <- get_sections(course_id)

    message("Getting submissions...")
    submissions <- get_submissions(course_id, sub_type, sub_id) |>
      dplyr::select(user_id, score, workflow_state) |>
      dplyr::rename(id = user_id)

    message("\nCompiling...")


    canvas_data <- students |>
      dplyr::left_join(sections, by = dplyr::join_by(sis_user_id)) |>
      dplyr::left_join(submissions, by = dplyr::join_by(id)) |>
      dplyr::mutate(marking_group = as.numeric(marking_group)) |>
      dplyr::filter(!is.na(marking_group)) |>
      dplyr::arrange(marking_group)
  })

  message("Done!")

  return(canvas_data)

}
