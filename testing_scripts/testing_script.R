# go to canvas and get stuff

# devtools::install_github("daranzolin/rcanvas")
course_id = 27531

canvas_setup()

get_submissions <- function(course_id, type, type_id)
{
  if (!type %in% c("quizzes", "assignments"))
    stop("type must be 'quizzes' or 'assignments'")

  url <- sprintf("%scourses/%s/%s/%s/submissions", "https://canvas.sussex.ac.uk/api/v1/",
                 course_id, type, type_id)
  args <- list(access_token = rcanvas:::check_token(), per_page = 100)

  rcanvas:::process_response(url, args) |>
    dplyr::bind_rows() |>
    dplyr::mutate(course_id = course_id)
}



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

get_sections <- function(course_id){
  sections <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", course_id, "/sections"),
    list(per_page = 100, `include[]` = c("students")), "GET") |>
    rcanvas:::paginate() |>
    purrr::map(httr::content, "text") |>
    purrr::map(jsonlite::fromJSON, flatten = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::filter(grepl("^marking", name, ignore.case = T)) |>
    tidyr::unnest() |>
    dplyr::mutate(marking_group = gsub("^Marking\\s*(\\d+).*", "\\1", name)) |>
    dplyr::select(sis_user_id, marking_group)
}




compile_canvas_data <- function(course_id, assign_id){

  students <- get_students(course_id)
  sections <- get_sections(course_id)

  submissions <- get_submissions(course_id, type = "assignments", assign_id) |>
    dplyr::select(user_id, score, workflow_state) |>
    dplyr::rename(id = user_id)

  canvas_data <- students |>
    dplyr::left_join(sections) |>
    dplyr::left_join(submissions) |>
    dplyr::mutate(marking_group = as.numeric(marking_group)) |>
    dplyr::filter(!is.na(marking_group)) |>
    dplyr::arrange(marking_group)

  return(canvas_data)

}

canvas_data <- compile_canvas_data(course_id = course_id, assign_id = "130992")
