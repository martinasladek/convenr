
#' Set Up Canvas
#'
#' Edits the user's .Renviron file with new or updated environ variables and sets these for use with Canvas.
#'
#' This interactive function guides the user through the creation (or updating, if they already exist) of two variables, CANVAS_TOKEN and CANVAS_DOMAIN. The function first checks if there are already variables defined in the local .Renviron file with these names, and gives the option to update them or not.
#' Once both values are put in, the function then sets these for use with Canvas using [rcanvas::set_canvas_token()] and [rcanvas::set_canvas_domain()] respectively.
#'
#' @return Sets .Renviron variables and runs [canvas_setup()] to communicate with Canvas
#' @export
#'
#' @examples
#'  \dontrun{
#'  canvas_auto_setup()
#' }
#'
canvas_auto_setup <- function(){
  ## Intro message
  message("This function will set up your access to Canvas via R.\nYou will need your Canvas token and your uni's Canvas URL.\nSee {convenr} README for more details.")

  ## Get current .Renviron file
  scope = c("user", "project")
  path <- usethis:::scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
  renv_lines <- readLines(path)

  ## Check whether variable CANVAS_TOKEN already exists
  if(any(grepl("CANVAS_TOKEN", renv_lines) == TRUE)){
    message("You already have a 'CANVAS_TOKEN' in your .Renviron.")
    token_resp <- utils::menu(title = "Overwrite with new token?",
                      c("Yes, overwrite", "No, proceed without overwriting", "Abort"))
  } else {
    token_resp <- NA
  }

  ## If there IS a response but it's not 1 or 2, stop
  if(!(token_resp %in% 1:2) & !is.na(token_resp)) {
    stop("Canvas setup aborted", call. = FALSE)
    ## If response was overwrite, replace CANVAS_TOKEN line with new one
  } else if(token_resp %in% 1){
    renv_lines[grepl("CANVAS_TOKEN", renv_lines)] <- paste0("CANVAS_TOKEN = ",
                                                            readline(prompt = "Canvas token: "),
                                                            "\n")

    writeLines(renv_lines, path)
    ## If there was no previous CANVAS_TOKEN, create one
  } else if(is.na(token_resp)){
    renv_lines <- c(renv_lines,
      paste0("CANVAS_TOKEN = ",
             readline(prompt = "Canvas token: ")),
      "\n")
    writeLines(renv_lines, path)
  }

  ## Check whether variable CANVAS_DOMAIN already exists
  if(any(grepl("CANVAS_DOMAIN", renv_lines) == TRUE)){
    message("You already have a 'CANVAS_DOMAIN' in your .Renviron.")
    domain_resp <- utils::menu(title = "Overwrite with new domain?",
                      c("Yes, overwrite", "No, proceed without overwriting", "Abort"))
  } else {
    domain_resp <- NA
  }

  ## If there IS a response but it's not 1 or 2, stop
  if(!(domain_resp %in% 1:2) & !is.na(domain_resp)) {
    stop("Canvas setup aborted", call. = FALSE)
    ## If response was overwrite, replace CANVAS_DOMAIN line with new one
  } else if(domain_resp %in% 1){
    renv_lines[grepl("CANVAS_DOMAIN", renv_lines)] <- paste0("CANVAS_DOMAIN = ",
                                                            readline(prompt = "Canvas domain URL: "),
                                                            "\n")

    writeLines(renv_lines, path)
    ## If there was no previous CANVAS_TOKEN, create one
  } else if(is.na(domain_resp)){
    renv_lines <- c(renv_lines,
                    paste0("CANVAS_DOMAIN = ",
                           readline(prompt = "Canvas domain URL: "),
                           "\n")
    )
    writeLines(renv_lines, path)
  }

  ## Run convenr function to set these values
  canvas_setup()

}


