# Module UI
  
#' @title   cluster_wise_sample_information_ui and cluster_wise_sample_information_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param parent_id
#' @param cluster_wise_sample_information
#' 
#' @rdname mod_cluster_wise_sample_information
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
cluster_wise_sample_information_ui <- function(id){
  ns = NS(id)
  tagList(
    
    ## word cloud select sample cluster 
    fluidRow(
      column( offset = 2, width  = 4,
              shinyWidgets::pickerInput(inputId = ns("select_column_cluster") , 
                                        label = "Select column cluster" , 
                                        choices = NULL,
                                        multiple = FALSE ,
                                        options = list(style = "btn-success", 
                                                       `live-search` = TRUE)
                                        
              )
      ) , 
      ## word cloud select view type 
      column(width = 4,  
             shinyWidgets::pickerInput(inputId = ns("sample_infor_view_type") , 
                                       label = "View" , 
                                       choices = c("Table" = "table"  ,"Word cloud" = "word_cloud"),
                                       multiple = FALSE ,
                                       options = list(style = "btn-success", 
                                                      `live-search` = TRUE)
             )
      ),
      
      fluidRow(
        ## wc submit button 
        column(offset = 5, width = 2, 
               shinyWidgets::actionBttn(inputId = ns("sample_infor_submit") , 
                                        style = "gradient" ,
                                        size = "md" ,  
                                        label = "Submit",
                                        color = "success",
                                        icon = icon("arrow-right"), 
                                        block = F)
        )
      )
      
    ),
    hr(),
    
    fluidRow(
      column(width = 12,
             
             ## sample infor table output 
             conditionalPanel(condition =  
                                paste0("input['",ns("sample_infor_view_type"),"'] == 'table' "), 
                              
                              DT::dataTableOutput(outputId = ns("selected_cluster_sample_infor_table_output"),width = "100%")  %>% 
                                withSpinner(color = "#18BC9C")            
             ),
             
             ## sample infor wc output 
             conditionalPanel(condition =  
                                paste0("input['",ns("sample_infor_view_type"),"'] == 'word_cloud' "), 
                              
                              word_cloud_ui(id = ns("sample_infor_word_cloud"))  
                              
             )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_cluster_wise_sample_information
#' @export
#' @keywords internal
    
cluster_wise_sample_information_server <- function(input, output, session, parent_id,cluster_wise_sample_information ){
  
  ns <- session$ns
  ##  cluster_wise_sample_information is a list where names are cluster names and each elem is tibble with relavent information of respective cluster 
  
  ## get cluster names 
  cluster_names <- reactive({
    return(cluster_wise_sample_information() %>% names())
  })
  
  # observe({
  #   print("clusterwise_sample_infor")
  #   print(cluster_wise_sample_information())
  # })
  
  ## update cluster names
  observe({
    updatePickerInput(inputId = "select_column_cluster", 
                      session = session ,
                      choices = cluster_names())
  })
  
  ## prepare sample infor for user selected cluster 
  user_selected_cluster_sample_infor <- eventReactive(input$sample_infor_submit , {
    req(input$select_column_cluster)
    
    cluster_wise_sample_information()[[input$select_column_cluster]]
    
  })
  
  ##  user selected cluster abstract 
  user_selected_cluster_sample_abstracts <- eventReactive(input$sample_infor_submit , {
    req(input$select_column_cluster)
    
    user_selected_column_cluster <- input$select_column_cluster
    user_selected_cluster_sample_infor <- user_selected_cluster_sample_infor()
    
    ## get samples / column labels for user selected cluster 
    selected_cluster_column_labels <- user_selected_cluster_sample_infor  %>% 
      pull(1) %>%  ## column 1 is SRA sample id 
      unique()
    
    ## get abstract from selected labels 
    selected_cluster_abstracts <-  user_selected_cluster_sample_infor %>%
      dplyr::filter(.[[1]] %in% selected_cluster_column_labels) %>%  ## column 1 (run_accession) is SRA id col 
      dplyr::pull(8) %>%  ## column 8 is study_abstract
      unique() ## remove redundant abstracts
    
    
    # print("Number of unique abstracts")
    # print(length(selected_cluster_abstracts))
    # print("unique abstracts")
    # print(selected_cluster_abstracts)
    
    
    if(selected_cluster_abstracts %>% length() == 0 || 
       is.na(selected_cluster_abstracts) %>% all() ){
      shinyWidgets::sendSweetAlert(session = session ,
                                   type = "error",
                                   title = "Error...!!" ,
                                   text = h5( "Input text not found for given samples - " , selected_cluster_column_labels %>% print() ) )
      return(NULL)
    } else{
      return(selected_cluster_abstracts)
    }
  })
  
  observe({
    
  })
  
  
  ## generate word cloud of selected abstract. 
  callModule(module = word_cloud_server , 
             id = "sample_infor_word_cloud" , 
             parent_id = paste(parent_id , "-sample_infor_word_cloud" , sep=""),
             session = session , 
             input_text = reactive(user_selected_cluster_sample_abstracts()))
  
  
  ### render user selected cluster sample infor table 
  output$selected_cluster_sample_infor_table_output <- DT::renderDataTable({
    cols_to_select <- c( "bio_project","library_name","strain" , "genotype" ,"study_title" , "study_abstract")
    return(user_selected_cluster_sample_infor() %>% dplyr::select(1, cols_to_select))
  },
  selection  = "none",
  server = T,
  extensions = "Buttons",
  options = list(
    searchHighlight = TRUE,
    scrollX = TRUE,
    dom = "Blfrtip",
    buttons =
      list("copy", list(extend = 
                          "collection", buttons =
                          c("csv", "excel", "pdf"), text = "Download"
      )), # end of buttons customization
    
    # customize the length menu
    lengthMenu = list(
      c(10, 50, 100, 500, -1) # declare values
      , c(10, 50, 100, 500, "All") # declare titles
    ),
    pageLength = 10
  ))
  
  
}
    