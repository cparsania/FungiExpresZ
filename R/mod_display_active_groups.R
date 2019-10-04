# Module UI
  
#' @title   display_active_groups_ui and display_active_groups_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_display_active_groups
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
display_active_groups_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$div(style = "overflow-x:auto;",
             
             ## show group info actn button 
             shinyWidgets::actionBttn(inputId = ns("show_group_info_action") ,label = "Group info" ,
                                      icon = icon("sitemap") ,
                                      style = "gradient",
                                      color = "success",
                                      size = "md",
                                      block = T),
             
             # table for selected group type
             shinyBS::bsModal(id = ns("group_info_popup"),trigger = ns("show_group_info_action"),
                              size = "large",title = "Active group information",
                              
                              ## select group type to disply
                              shinyWidgets::radioGroupButtons(inputId = ns("active_group_display_type") , 
                                                              label = "",
                                                              choices  = c("Gene groups" = "row" ,"Sample groups" = "column") , 
                                                              status = "success" ,
                                                              size = 'sm' , 
                                                              direction = "horizontal",
                                                              justified = T,
                                                              individual = T, width = "100%"),
                              
                              ## group info table 
                              tags$div(style = "overflow-x:auto;",
                                       DT::dataTableOutput(outputId = ns("active_group_info"),
                                                           width = "100%")  %>%
                                         shinycssloaders::withSpinner(color = "#18BC9C")                 
                              )
             )
             
             
             
    )
  )
}    
# Module Server
    
#' @param input session input
#'
#' @param output session output 
#' @param session session
#' @param group_data 
#'
#' @rdname mod_display_active_groups
#' @keywords internal
    
display_active_groups_server <- function(input, output, session , group_data){
  
  row_groups <- reactive({
    ## long to wide 
    req(group_data()$row_groups)
    col_names  <- group_data()$row_groups %>% colnames()
    group_name <- col_names[1]
    group_member <- col_names[2]
    
    wide <- group_data()$row_groups %>%  
      dplyr::group_by(!!as.symbol(group_name)) %>%
      tidyr::nest() %>% 
      dplyr::mutate(group_member := map_chr(data , ~(.x %>% pull(1) %>% paste0(collapse = ",")))) %>%
      dplyr::select(-data)
    
    return(wide)
  })
  
  column_groups <- reactive({
    ## long to wide 
    req(group_data()$column_groups)
    
    req(group_data()$column_groups)
    col_names  <- group_data()$column_groups %>% colnames()
    group_name <- col_names[1]
    group_member <- col_names[2]
    
    wide <- group_data()$column_groups %>%  
      dplyr::group_by(!!as.symbol(group_name)) %>%
      tidyr::nest() %>% 
      dplyr::mutate(group_member := map_chr(data , ~(.x %>% pull(1) %>% paste0(collapse = ",")))) %>%
      dplyr::select(-data) 
    
    return(wide)
  })
  
  output$active_group_info <- DT::renderDataTable({
    if(input$active_group_display_type == "row")
      return(row_groups())
    if(input$active_group_display_type == "column")
      return(column_groups())
  },
  
  options = list(dom = 'Blfrtip' ,
                 searchHighlight = TRUE,
                 ordering=FALSE,
                 scorollX = TRUE,
                 autoWidth = TRUE,
                 buttons =
                   list("copy", 
                        list(extend = "collection", 
                             buttons = c("csv", "excel", "pdf"), text = "Download")
                        
                   ) # end of buttons customization
  ) ,
  rownames = FALSE, 
  selection = 'none'
  )
  
  
}    

