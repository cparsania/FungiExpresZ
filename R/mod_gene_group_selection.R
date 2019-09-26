# Module UI
  
#' @title   gene_group_selection_ui and gene_group_selection
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param gene_group_info
#' @param current_session_data_matrix
#' @param generate_plot_action
#'
#' @rdname mod_gene_group_selection
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
gene_group_selection_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::pickerInput(
      inputId = ns("select_gene_clusters"),
      label = "Select gene group(s)", 
      choices = "",
      multiple = TRUE,
      options = list(
        style = "btn-primary",
        `actions-box` = TRUE, `live-search` = TRUE
      ) , width = "100%"
    ),
    
    shiny::tags$h5(tags$i("*If No group(s) selected, resulted plot will be for all the genes."))
    
  )
}
    
# Module Server
    
#' @rdname mod_gene_group_selection
#' @export
#' @keywords internal
    
gene_group_selection <- function(input, output, session,  gene_group_info , current_session_data_matrix , generate_plot_action){
  
  ## update select_gene_cluster 
  observe({
    req(gene_group_info())
    gene_groups <- gene_group_info() %>% dplyr::pull(1) %>% unique()
    
    updatePickerInput(inputId = "select_gene_clusters" , 
                      session = session, 
                      choices = gene_groups, 
                      #selected = gene_groups %>% .[1] ,
                      choicesOpt = list(
                        content = sprintf("<span class='label label-%s'>%s</span>", 
                                          c("success"), 
                                          gene_groups)
                      )
    )
    
  })
  
  user_selected_gene_groups <-  reactive({
    #req(input$select_gene_clusters)
    return(input$select_gene_clusters)
  })
  
  ## subset data by user selected gene groups 
  gene_group_specific_data <- eventReactive(generate_plot_action(),{
    req(current_session_data_matrix())
    
    ## get 1st column header     
    gene_name_header <- current_session_data_matrix() %>% colnames() %>% .[1]
    
    
    ## if no group selected, return all the genes 
    if(is.null(user_selected_gene_groups())) {
      gene_group_specific_data <- current_session_data_matrix() %>% 
        left_join(gene_group_info() , by = stats::setNames("gene_group_members" , gene_name_header)) %>% 
        tidyr::replace_na(list(gene_groups = "No groups assigned"))
    } else {
      ## subset by user selected gene groups 
      gene_group_specific_data <- current_session_data_matrix() %>% 
        left_join(gene_group_info() , by = stats::setNames("gene_group_members" , gene_name_header)) %>% 
        tidyr::replace_na(list(gene_groups = "No groups assigned")) %>% ## NA will be converted to "No groups assigned"
        dplyr::filter(gene_groups %in% user_selected_gene_groups())  
    }
    
    ## if for a given group, none of the group member found in the uploaded data return NULL and throw error 
    if(gene_group_specific_data  %>% nrow() == 0 ){
      shinyWidgets::sendSweetAlert(session = session, title = "Error", 
                                   type = "error",
                                   text = "For selected gene group(s), none of the gene found in the data. Make sure that selected gene group members present in the uploaded data. " ) 
      return(NULL)
    }
    
    
    return(gene_group_specific_data)
    
  })
  
  
  
  return(reactive(gene_group_specific_data()))
  
}

