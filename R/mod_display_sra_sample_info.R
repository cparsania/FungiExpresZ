# Module UI
  
#' @title   display_sra_sample_info_ui and display_sra_sample_info_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_display_sra_sample_info
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
display_sra_sample_info_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$div(style = "overflow-x:auto;",
             
             ## show sra sample info actn button 
             shinyWidgets::actionBttn(inputId = ns("show_sra_sample_info_action") ,label = "Sample info" ,
                                      icon = icon("vial") ,
                                      style = "gradient",
                                      color = "success",
                                      size = "md",
                                      block = T),
             
             
             ## show sra sample info through popup
             shinyBS::bsModal(id = ns("sra_sample_info_popup"),
                              trigger = ns("show_sra_sample_info_action"),
                              
                              shinyWidgets::pickerInput(
                                inputId = ns("sra_run_accession"),
                                label = "Select run accesssion", 
                                choices = "",
                                multiple = F,
                                width = "100%",
                                options = list(
                                  `actions-box` = TRUE,
                                  style = "btn-success", 
                                  `live-search` = TRUE
                                )
                              ),
                              
                              
                              DT::dataTableOutput(outputId = ns("sra_sample_info_to_display"),width = "100%")  %>% 
                                shinycssloaders::withSpinner(color = "#18BC9C")                 
             )
             
    )
    
  )
}
    
# Module Server
    
#' @param input session input
#'
#' @param output session output
#' @param session session
#' @param user_selected_sra_sample_info_tibble 
#'
#' @rdname mod_display_sra_sample_info
#' @keywords internal
display_sra_sample_info_server <- function(input, output, session, user_selected_sra_sample_info_tibble){
  req(user_selected_sra_sample_info_tibble())
  
  ## convert sra_sample_info  to long format   
  
  observe({
    req(user_selected_sra_sample_info_tibble())
    shinyWidgets::updatePickerInput(inputId = "sra_run_accession" , session = session,
                      choices = user_selected_sra_sample_info_tibble() %>% pull("run_accession")
    )
  })
  
  user_selected_sra_id_sample_info <- reactive({
    req(input$sra_run_accession)
    dd <- user_selected_sra_sample_info_tibble() %>% dplyr::filter(.data$run_accession == input$sra_run_accession)
    return(dd)
  })
  
  ## sra sample info table 
  output$sra_sample_info_to_display <- DT::renderDataTable({
    cols_to_select <- c("run_accession",
                        "bio_project",
                        "species",
                        "library_name",
                        "strain",
                        "genotype" , 
                        "study_title" , 
                        "study_abstract")
    id_col <- "run_accession"
    
    sra_sample_info_long <- user_selected_sra_id_sample_info() %>% 
      dplyr::select(!!!cols_to_select) %>% 
      tidyr::gather(key = "Feature" , "Description" , -!!id_col) %>% 
      dplyr::select(-!!id_col)
    return(sra_sample_info_long)
  }, options = list(dom = 't' , 
                    searchHighlight = TRUE,
                    ordering=FALSE, 
                    scorollX = TRUE,  
                    autoWidth = FALSE) ,  
  rownames = FALSE)
}
 
