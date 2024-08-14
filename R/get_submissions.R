#' Get submissions for an assignment
#'
#' @param course_id Canvas ID number for the course (unit).
#' @param sub_type Type of submission. String. Possible values are "assignments" or "quizzes".
#' @param sub_id  Canvas ID number for the submission. String or numeric.
#'
#' @return a tibble
#' @export
#'
#' @examples
#'  \dontrun{
#'  get_submissions()
#' }
#'
get_submissions <- function(course_id, sub_type, sub_id)
{
  if (!sub_type %in% c("quizzes", "assignments"))
    stop("type must be 'quizzes' or 'assignments'")

  url <- sprintf("%scourses/%s/%s/%s/submissions", "https://canvas.sussex.ac.uk/api/v1/",
                 course_id, sub_type, sub_id)
  args <- list(access_token = rcanvas:::check_token(), per_page = 100)

  rcanvas:::process_response(url, args) |>
    dplyr::bind_rows() |>
    dplyr::mutate(course_id = course_id)
}
