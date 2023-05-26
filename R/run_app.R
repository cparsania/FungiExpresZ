#' Run the Shiny Application
#'
#' @param ... other arguments to be passed to argument \code{golem_opts} inside function \code{with_golem_options}
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
    golem::with_golem_options(app = shiny::shinyApp(ui = app_ui, server = app_server), golem_opts = list(...))
}
