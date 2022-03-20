# Module UI
  
#' @title   mod_pathway_analysis_ui and mod_pathway_analysis_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_pathway_analysis
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_pathway_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
  shiny::fluidRow(
    shiny::column(width = 12)
  )
  )
}
    
# Module Server
    
#' @rdname mod_pathway_analysis
#' @export
#' @keywords internal
    
mod_pathway_analysis_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_pathway_analysis_ui("pathway_analysis_ui_1")
    
## To be copied in the server
# callModule(mod_pathway_analysis_server, "pathway_analysis_ui_1")
 
