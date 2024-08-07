#' Run the ConvenR app
#'
#' @param ... Additional arguments
#'
#'
#' @return Runs a Shiny app
#'
#' @import shiny shinydashboard
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  run_convenr()
#' }
#'
run_convenr <- function(...){

  # Define UI for application
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody()
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}
