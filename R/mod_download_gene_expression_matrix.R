# Module UI
  
#' @title   mod_download_gene_expression_matrix_ui and mod_download_gene_expression_matrix_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_download_gene_expression_matrix
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
download_gene_expression_matrix <- function(id){
  
  ns = NS(id)
  tagList(
    br(),
    tags$h2("Download Gene Expression Matrix" , style = "text-align:center; font-weight: bold;"),
    hr(),
    tags$div(fluidRow(
      column(width = 12,
             DT::dataTableOutput(outputId = ns("display_gene_expression_download_table"))  %>% 
               shinycssloaders::withSpinner(color = "#18BC9C")       
      )
    ), style = "margin:auto ;  width: 1000px;" )
  )
  
}
    
# Module Server
    
#' @param input session input
#'
#' @param output session output
#' @param session session 
#'
#' @rdname mod_download_gene_expression_matrix
#' @keywords internal
download_gene_expression_matrix_server <- function(input, output, session){
  
  gene_expr_data_download_related_stuff <- reactiveValues()
  
  ns <- session$ns
  
  ## get number of samples in each data file 
  species_wise_sra_sample_count <- function(){
    #expr_files <- list.files(paste("./app/",get_expression_mats_dir_path(),sep = "") ,  pattern = "*_expr_mat.rds" ,full.names = T)
    ## get expr mat files 
    expr_files <- list.files(get_expression_mats_dir_path() ,  pattern = "*_expr_mat.rds" ,full.names = T)
    names(expr_files) <- basename(expr_files)
    
    map_df(expr_files , function(x) {
      readr::read_rds(file = x) %>% 
        ncol() - 1 %>% ##  -1 because first column is gene names 
        tibble::tibble(`# sra_samples` = .  , )
    }) %>% 
      dplyr::mutate(expression_mat_data_file =  names(expr_files)) %>% ## expression_mat_data_file must not be changed.
      dplyr::select(2,dplyr::everything())
  }
  
  ## function below is with side effect. it has inputs defined within it 
  get_gene_expr_matrix_info_table <- function(){
    left_col <- "reference_annotation" ## from ref_annot_to_expr_rds file 
    right_col <-"genome"  ## fungi_db_species_info_rds file 
    
    ## rds file to species name mapping 
    ref_annot_to_expr_rds %>% 
      dplyr::left_join(species_table , by = stats::setNames(right_col,left_col) )  %>% 
      dplyr::select(3,2) %>% ## 3rd column is species & 2nd column is expression_mat_data_file
      dplyr::left_join(species_wise_sra_sample_count())
  }
  
  ## reactive version of data to be shown on browser 
  display_gene_expression_data <- reactive({
    get_gene_expr_matrix_info_table()
  })
  
  ## display gene expression data 
  output$display_gene_expression_download_table <- DT::renderDataTable({
    
    ### add column containing download button 
    action_button_column_name = "action"
    
    with_downlod_bttn_added <- display_gene_expression_data() %>% 
      dplyr::mutate(!!as.symbol(action_button_column_name) := shinyInput(downloadButton,
                                                                  len = dplyr::n(), 
                                                                  id = 'button_', 
                                                                  label = "Download .txt", 
                                                                  ns = ns, 
                                                                  onclick = sprintf("Shiny.setInputValue('%s', this.id)",
                                                                                    ns("select_button"))
      )  
      )## add column having download button 
    
    ## final data table to display 
    DT::datatable(with_downlod_bttn_added %>% 
                    dplyr::select(-2), ## remove column having name of .rds file 
                  escape = FALSE, 
                  selection = 'none',
                  options = 
                    list(
                      preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                      drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } '),
                      searchHighlight = TRUE,
                      scrollX =TRUE
                    )
    )
  })
  
  
  
  ## reference : 
  ## https://stackoverflow.com/questions/57973357/how-to-use-downloadhandler-for-the-download-button-create-inside-shiny-datatable/57978298#57978298
  
  ## create download handler for all download buttons in the go table 
  observe({
    lapply(c(1: (display_gene_expression_data() %>% nrow())), function(i){
      output[[paste0("button_",i)]] <- downloadHandler(
        filename = function() {
          gene_expr_data_download_related_stuff$download_file_name
        },
        content = function(to_file) {
          rio::convert(in_file  = gene_expr_data_download_related_stuff$download_file_from, 
                       out_file  = to_file)
        }
      )
    })
  })
  
  ## assign data to reactive values 
  observeEvent(input$select_button,{
    selectedRow <- as.numeric(str_split(input$select_button, "button_",)[[1]][2])
    
    ## go data file name to be downloaded 
    download_file_name <- display_gene_expression_data() %>% 
      slice(selectedRow) %>% 
      pull("species") %>% 
      paste(.  , "_gene_expression_mat_", Sys.time() , ".txt",sep = "") %>% 
      str_replace_all("\\s+" , "_")
    
    ## go data file which is going to be downloaded 
    download_file_from <- display_gene_expression_data() %>% 
      slice(selectedRow) %>% 
      pull("expression_mat_data_file") %>% 
      paste(get_expression_mats_dir_path() , . , sep = "/")
    
    ## assign to reactive values 
    gene_expr_data_download_related_stuff$download_file_from <- download_file_from
    gene_expr_data_download_related_stuff$download_file_name <- download_file_name
    
  })
  
}    

