
#' Set Up Canvas Manually
#'
#' Edits the user's .Renviron file with new or updated environ variables and sets these for use with Canvas.
#'
#'
#' @return Sets .Renviron variables and runs [canvas_setup()] to communicate with Canvas
#' @export
#'
#' @examples
#'  \dontrun{
#'  canvas_manual_setup()
#' }
#'
canvas_manual_setup <- function(){
  ## Intro message
  message("WARNING: This function will overwrite any existing variables in your .Renviron file without asking!\nIf you want a guided setup with safeguards, run convenr::canvas_auto_setup() instead, you numpty.")

  ## Get current .Renviron file
  scope = c("user", "project")
  path <- usethis:::scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
  renv_lines <- readLines(path)

if(any(grepl("CANVAS_TOKEN", renv_lines) == TRUE)){
    renv_lines[grepl("CANVAS_TOKEN", renv_lines)] <- paste0("CANVAS_TOKEN = ",
                                                            readline(prompt = "Canvas token: "),
                                                            "\n")
  } else {
    renv_lines <- c(renv_lines,
                    paste0("CANVAS_TOKEN = ",
                           readline(prompt = "Canvas token: ")),
                    "\n")
  }

    if(any(grepl("CANVAS_DOMAIN", renv_lines) == TRUE)){
      renv_lines[grepl("CANVAS_DOMAIN", renv_lines)] <- paste0("CANVAS_DOMAIN = ",
                                                              readline(prompt = "Canvas domain: "),
                                                              "\n")
    } else {
      renv_lines <- c(renv_lines,
                      paste0("CANVAS_DOMAIN = ",
                             readline(prompt = "Canvas domain: ")),
                      "\n")
    }

    writeLines(renv_lines, path)

  ## Run convenr function to set these values
  canvas_setup()

}


