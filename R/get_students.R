#' Get a list of students on a module/course
#'
#' @param course_id The Canvas ID number for the course/module
#'
#' @return A tibble of student information from Canvas
#' @export
#'
#' @examples
#'  \dontrun{
#'  get_students(course_id = 12345)
#' }
#'
get_students <- function(course_id) {

  students <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", course_id, "/students"),
    list(per_page = 100), "GET") |>
    rcanvas:::paginate() |>
    purrr::map(httr::content, "text") |>
    purrr::map(jsonlite::fromJSON, flatten = TRUE) |>
    dplyr::bind_rows()

  students <- students[!duplicated(students), ] |>
    dplyr::mutate(cand_no = gsub("Candidate No : ", "", sortable_name)) |>
    dplyr::select(-created_at, -sortable_name, -short_name, -integration_id, -pronouns) |>
    dplyr::mutate(cand_no = as.numeric(cand_no)) |>
    dplyr::filter(!is.na(cand_no))

  return(students)

}
