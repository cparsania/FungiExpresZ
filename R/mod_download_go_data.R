# Module UI

#' @title   download_go_data_ui and download_go_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_download_go_data
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
download_go_data_ui <- function(id) {
    ns = NS(id)
    tagList(br(), tags$h2("Download Gene Ontology Data", style = "text-align:center; font-weight: bold;"), hr(), tags$div(fluidRow(column(width = 12, 
        DT::dataTableOutput(outputId = ns("display_go_data")) %>% shinycssloaders::withSpinner(color = "#18BC9C"))), style = "margin:auto ;  width: 1000px;"))
}

# Module Server

#' @param input session input
#'
#' @param output session output 
#' @param session session 
#' @param ah_data 
#'
#' @rdname mod_download_go_data
#' @keywords internal
download_go_data_server <- function(input, output, session, ah_data) {
    
    ns <- session$ns
    action_button_column_name = "action"
    go_data_download_related_stuff <- reactiveValues()
    
    ## subset GO data
    go_data <- reactive({
        ah_data %>% ## columns 1,2,6 are genome,species,orgdb_cols
        dplyr::select(1, 2, 6) %>% dplyr::mutate(`:=`(!!as.symbol(action_button_column_name), shinyInput(downloadButton, len = dplyr::n(), 
            id = "button_", label = "Download .txt", ns = ns, onclick = sprintf("Shiny.setInputValue('%s', this.id)", ns("select_button")))  ## add column having download button 
))
    })
    
    ## Prepare GO data to be displayed
    output$display_go_data <- DT::renderDataTable({
        
        DT::datatable(go_data() %>% dplyr::select(2, 1, !!as.symbol(action_button_column_name)) %>% janitor::clean_names(case = "snake"), 
            escape = FALSE, selection = "none", options = list(preDrawCallback = DT::JS("function() { Shiny.unbindAll(this.api().table().node()); }"), 
                drawCallback = DT::JS("function() { Shiny.bindAll(this.api().table().node()); } "), searchHighlight = TRUE, scrollX = TRUE))
    })
    
    ## reference :
    ## https://stackoverflow.com/questions/57973357/how-to-use-downloadhandler-for-the-download-button-create-inside-shiny-datatable/57978298#57978298
    
    ## create download handler for all download buttons in the go table
    observe({
        lapply(c(1:(go_data() %>% nrow())), function(i) {
            output[[paste0("button_", i)]] <- downloadHandler(filename = function() {
                go_data_download_related_stuff$go_download_file_name
            }, content = function(file) {
                readr::write_delim(x = go_data_download_related_stuff$go_data, path = file, delim = "\t")
            })
        })
    })
    
    ## assign data to reactive values
    observeEvent(input$select_button, {
        selectedRow <- as.numeric(stringr::str_split(input$select_button, "button_", )[[1]][2])
        
        ## go data to be downloaded
        dd <- go_data() %>% dplyr::slice(selectedRow) %>% dplyr::pull(orgdb_cols) %>% .[[1]]
        
        ## go data file name to be downloaded
        download_file_name <- go_data() %>% dplyr::slice(selectedRow) %>% ## column 1 is species name
        dplyr::pull(2) %>% paste(., "_go_data_", Sys.time(), ".txt", sep = "") %>% stringr::str_replace_all("\\s+", "_")
        
        ## assign to reactive values
        go_data_download_related_stuff$go_data <- dd
        go_data_download_related_stuff$go_download_file_name <- download_file_name
        
    })
}

## To be copied in the UI mod_download_go_data_ui('download_go_data_ui_1')

## To be copied in the server callModule(mod_download_go_data_server, 'download_go_data_ui_1')

