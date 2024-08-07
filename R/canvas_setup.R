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
canvas_setup <- function(domain = "https://canvas.sussex.ac.uk"){
  rcanvas::set_canvas_token(Sys.getenv("CANVAS_TOKEN"))
  rcanvas::set_canvas_domain(domain)
  message("Setup completed successfully (:")
}
