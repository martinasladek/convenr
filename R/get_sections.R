#' Get Canvas sections
#'
#' @param course_id The Canvas ID number for the course/module
#' @param section_regex Regex match as a string for the shared element of the section names that indicate marking groups
#' @param section_number_regex Regex string to extract the group number. For example, "^marking\\s*(\\d+).*" identifies the number "1" in "Marking 1 : Take Away paper T2 Week 7"
#'
#' @return A tibble of section information from Canvas
#' @export
#'
#' @examples
#'  \dontrun{
#'  get_sections(course_id = 12345)
#' }
#'
get_sections <- function(course_id, section_regex = "^marking", section_number_regex = "^marking\\s*(\\d+).*"){
  sections <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", course_id, "/sections"),
    list(per_page = 100, `include[]` = c("students")), "GET") |>
    rcanvas:::paginate() |>
    purrr::map(httr::content, "text") |>
    purrr::map(jsonlite::fromJSON, flatten = TRUE) |>
    dplyr::bind_rows() |>
    dplyr::filter(grepl(section_regex, name, ignore.case = T)) |>
    tidyr::unnest() |>
    dplyr::mutate(marking_group = gsub(section_number_regex, "\\1", tolower(name))) |>
    dplyr::select(sis_user_id, marking_group)
}
