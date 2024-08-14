#' Set up access to Canvas
#'
#' @param domain Canvas url (string)
#'
#' @return Grants canvas access
#' @export
#'
#' @examples
#'  \dontrun{
#'  canvas_setup()
#' }
#'
canvas_setup <- function(){
  message("Setting Canvas token and domain...")

  if("" %in% c(Sys.getenv("CANVAS_TOKEN"), Sys.getenv("CANVAS_DOMAIN"))){
    stop("Either CANVAS_TOKEN or CANVAS_DOMAIN not specified successfully.\nRun convenr::canvas_auto_setup() to add both a Canvas token and domain URL to your .Renviron file.",
         call. = FALSE)
  }

  rcanvas::set_canvas_token(Sys.getenv("CANVAS_TOKEN"))
  rcanvas::set_canvas_domain(Sys.getenv("CANVAS_DOMAIN"))

  message("Canvas setup completed!")
}
