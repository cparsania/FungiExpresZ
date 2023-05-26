# Module UI
  
#' @title   add_sample_group_ui and add_sample_group
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_add_sample_group
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
add_sample_group_ui <-  function(id){
  ns <- NS(id)
  
  
  tagList(
    shinyWidgets::actionBttn(inputId = ns("set_groups_trigger") , 
                             label = "Samples" , 
                             color = "success" ,
                             size = "md",
                             block = T,
                             icon = icon("arrows-alt-h","fa-1.5x"),
                             style = "gradient"),
    
    shinyBS::bsModal(id = ns("set_groups_pop") , trigger = ns("set_groups_trigger") , 
                     title = "Assign sample groups", size = "large",
                     wellPanel(
                       shinyWidgets::radioGroupButtons(inputId = ns("group_input_method") ,label = "" ,
                                                       choices = c("Manual" = "manual" , 
                                                                   "Upload" = "user_upload" ,
                                                                   "Group by BioProject (NCBI)" = "ncbi_bioproject"), 
                                                       status = "success" ,width = "100%" ,justified = T , size = "sm"
                       )
                     ),
                     hr(),
                     
                     ## manually assigned sample group 
                     conditionalPanel(condition = paste0("input['",ns("group_input_method"),"'] == 'manual' "),
                                      
                                      tags$h4("Once data uploaded or selected from given SRA samples, column names (if uploaded) or SRA run id(if selected) will appear under ",tags$b("group memebres"), 
                                              "input."),
                                      hr(),
                                      
                                      ## column group 1 
                                      tags$h4(tags$b("Group 1")),
                                      wellPanel(
                                        textInput(inputId = ns("group_name1") , 
                                                  label = "Group name" ,value = "Group_1",  placeholder = "group name"),
                                        hr(),
                                        
                                        ## select group members 1 
                                        shinyWidgets::pickerInput(
                                          inputId = ns("group_members1"),
                                          label = "Group members", 
                                          choices = "", 
                                          options = list(
                                            style = "btn-primary" , `live-search` = TRUE),
                                          multiple = TRUE
                                        )
                                        
                                      ),
                                      hr(), 
                                      ## column group 2
                                      tags$h4(tags$b("Group 2")),
                                      wellPanel(
                                        textInput(inputId = ns("group_name2") , 
                                                  value = "Group_2",
                                                  label = "Group name" , 
                                                  placeholder = "group name"),
                                        
                                        ## select group members 2
                                        shinyWidgets::pickerInput(
                                          inputId = ns("group_members2"),
                                          label = "Group members", 
                                          choices = "", 
                                          options = list(
                                            style = "btn-primary", `live-search` = TRUE),
                                          multiple = TRUE
                                        )
                                        
                                      )
                                      
                                      
                     ),
                     
                     ## user upload sample groups 
                     conditionalPanel(condition = paste0("input['",ns("group_input_method"),"'] == 'user_upload' "),
                                      
                                      
                                      tags$h4("Upload sample groups either from file or paste data from clipboard. Follow " , tags$b("Upload instruction") , "given below to know about upload format."),
                                      hr(),
                                      
                                      
                                      ## upload sample group source 
                                      tags$h4(tags$b("Upload sample groups")),
                                      wellPanel(
                                        shiny::radioButtons(inputId = ns("sample_groups_source") , label = "" , 
                                                            choices = c("From file" ="sample_group_from_file" ,
                                                                        "From clipboard" ="sample_group_from_clipboard") , 
                                                            selected = "sample_group_from_file",inline = T, width = "100%"
                                        ),
                                        conditionalPanel(paste0("input['",ns("sample_groups_source"),"'] == 'sample_group_from_file' "),
                                                         shiny::fileInput(inputId = ns("sample_group_file") ,
                                                                          label = "Choose .txt file",
                                                                          accept = c(
                                                                            "text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv"),
                                                                          width = "100%")
                                        ),
                                        
                                        conditionalPanel(paste0("input['",ns("sample_groups_source"),"'] == 'sample_group_from_clipboard' "),
                                                         shiny::textAreaInput(inputId = ns("upload_groups_from_clipboard") ,
                                                                              rows = 10,cols = 10,
                                                                              placeholder = "follow instruction given below",
                                                                              label = "Paste sample groups"
                                                         )
                                        ),
                                        
                                        ## upload sample group instruction
                                        
                                        tags$h4(tags$b("Upload instruction")),
                                        tags$div(id = ns("upload_group_file_instruct"), 
                                                 "Input file  (.txt/.csv) or pasted data must have atleast two columns. If more than two columns found, only first two will be considered. ", tags$b("First column "),"corresponds to group names.", 
                                                 tags$b("Second column "), 
                                                 "corresponds to group members. Group members must be identical to the column names of the data uploaded. Each group member must belong to unique group name. Example data snap shot  have been shown below." , 
                                                 tags$b("First row "), 
                                                 "will be considered as header row. User can set headers of their choice.")
                                      ),
                                      hr(),
                                      ## column sep
                                      tags$h4(tags$b("Column seprator")),
                                      wellPanel(
                                        radioButtons(inputId = ns("sample_groups_info_file_sep") ,
                                                     label = "" ,
                                                     choices = c("Comma" = "," , "Semicolon" = ";" ,"Tab" = "\t") ,
                                                     inline = T,selected = "\t" ,width = "100%"
                                        )  
                                      ),
                                      
                                      hr(),
                                      
                                      ## example data snap
                                      tags$h4(tags$b("Example data snap")),
                                      wellPanel(
                                        
                                        tags$img(src="www/example_data_group_info.png", 
                                                 style="display: block; margin-left: auto; margin-right: auto;" , 
                                                 height = "50%", width = "50%")
                                      )
                                      
                     ),
                     
                     ## Ncbi bio project sample group 
                     conditionalPanel(condition = paste0("input['",ns("group_input_method"),"'] == 'ncbi_bioproject' "),
                                      
                                      tags$div(
                                        tags$h4("Applies only, when SRA samples are selected. SRA samples will be grouped by",
                                                tags$a( "NCBI BioProject Id." , href="https://www.ncbi.nlm.nih.gov/bioproject" ,target="_blank")  ,
                                                "Useful when samples from multiple BioProjects need to be compared.")
                                      )
                     ),
                     hr(),
                     ## submit button 
                     shinyWidgets::actionBttn(inputId = ns("set_groups") ,
                                              label = "Submit" ,
                                              icon = icon("thumbs-up") ,
                                              style = "gradient" , 
                                              size = "md" ,block = T,
                                              color = "success") 
                     
    )
    
  )
  
}    

# Module Server
    
#' @param input session input
#'
#' @param output session output
#' @param session session 
#' @param sample_names internal
#' @param bioproject_groups internal
#'
#' @rdname mod_add_sample_group
#' @keywords internal
add_sample_group <- function(input, output, session, sample_names , bioproject_groups){
  
  observeEvent(sample_names(),{
    req(sample_names())
    
    ## update group 1 choices 
    shinyWidgets::updatePickerInput(inputId = "group_members1" , session = session, choices = sample_names() , 
                      choicesOpt = list(
                        content = sprintf("<span class='label label-%s'>%s</span>", 
                                          c("success"), 
                                          sample_names())
                      )
    )
    
    ## update group 2 choices
    shinyWidgets::updatePickerInput(inputId = "group_members2" , session = session, choices = sample_names() , 
                      choicesOpt = list(
                        content = sprintf("<span class='label label-%s'>%s</span>", 
                                          c("success"), 
                                          sample_names())
                      ))
  })
  
  ## get user defined  column groups 
  user_uploaded_column_groups <- eventReactive(input$set_groups, {
    
    if(input$group_input_method == "manual") {
      group_names <- c(input$group_name1 , input$group_name2)
      group_members <- list((input$group_members1), (input$group_members2))  
    } else if(input$group_input_method == "ncbi_bioproject"){
      
      bp_grps <- bioproject_groups()  %>% base::unique() %>% # remove duplicate rows
        dplyr::select(1:2) %>% 
        dplyr::rename_all(funs(c("group_names" , "group_members")))%>%
        dplyr::mutate(group_members = gsub(pattern = " " , "_" , group_members)) %>% ## column names replace space to "_"
        tidyr::nest(group_members  ,.key = "group_members")  %>% 
        dplyr::mutate(group_members = purrr::map(group_members, function(ii){return(ii %>% dplyr::pull(1))})) %>% 
        dplyr::slice(gtools::mixedorder(group_names)) 
      
      group_names <- bp_grps %>% dplyr::pull("group_names")
      group_members <- bp_grps %>% dplyr::pull("group_members")
      
      
    } else if(input$group_input_method == "user_upload"){
      
      sample_group_from_user = tryCatch({
        if(input$sample_groups_source == "sample_group_from_file"){
          req(input$sample_group_file)
          user_sample_group <- readr::read_delim(file = input$sample_group_file$datapath, 
                                                 delim = input$sample_groups_info_file_sep, 
                                                 col_names = T, 
                                                 trim_ws = T) %>% 
            tidyr::drop_na()  ## rows will be dropped if value NA found in any column 
          #print(user_sample_group)
        }else{
          user_sample_group <- text_to_tibble(text = input$upload_groups_from_clipboard , 
                                              sep = input$sample_groups_info_file_sep) %>% 
            tidyr::drop_na()## rows will be dropped if value NA found in any column 
        }  
      }, error = function(e) {
        print(e)
        return(NULL)
      } , warning = function(w){
        print(w)
        return(NULL)
      })
      
      #print(sample_group_from_user)
      
      ## show alert/error if error produced while reading file. 
      if(is.null(sample_group_from_user)){ 
        shinyWidgets::sendSweetAlert(session = session ,type = "error",
                                     title = "Error...!!" ,
                                     text = paste("Error while uploading sample groups. Check upload data format.", sep = " "))
        return(NULL)
      }
      
      ## show error if less than 2 columns detected
      if(ncol(sample_group_from_user) < 2){
        shinyWidgets::sendSweetAlert(session = session ,type = "error",title = "Error...!!" ,
                                     text = paste("Upload data must contain atleast 2 columns.", sep = " "))
        return(NULL)
      }
      
      sample_group_from_user_p <- sample_group_from_user %>% 
        base::unique() %>% # remove duplicate rows
        dplyr::select(1:2) %>% 
        dplyr::rename_all(funs(c("group_names" , "group_members")))%>%
        dplyr::mutate(group_members = gsub(pattern = " " , "_" , group_members)) %>% ## column names replace space to "_"
        tidyr::nest(group_members  ,.key = "group_members")  %>% 
        dplyr::mutate(group_members = purrr::map(group_members, function(ii){return(ii %>% dplyr::pull(1))})) %>% 
        dplyr::slice(gtools::mixedorder(group_names)) 
      
      #print(sample_group_from_user)
      group_names <- sample_group_from_user_p %>% dplyr::pull("group_names")
      group_members <- sample_group_from_user_p %>% dplyr::pull("group_members")
      
      ### required checks 
      ##1) if more than 2 columns found, discard other columnns
      
      ## show warning if more than 2 columns detected
      if(ncol(sample_group_from_user) > 2){
        shinyWidgets::sendSweetAlert(session = session ,
                                     type = "warning",
                                     title = "Warning...!!" ,
                                     text = paste("File containes more than 2 columns. Only first 2 will be used.", sep = " "))
      }
      
    }
    
    ## validate group  inputs 
    ## check if any group names are empty strings
    if(any(group_names == "")){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Group name cannot be empty." ,type = "error" )
      return(NULL)
    }
    
    ## check if any group names duplicated. This is useful when user gives group name manually. 
    if(base::anyDuplicated(group_names) > 0 ){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Each group must have unique group name." ,type = "error" )
      return(NULL)
    }
    
    ## each group must have atleast one sample selected. This is useful when user gives group names manually.  
    if( any (lengths(group_members) ==  0 )){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Each group must have atleast one sample selected. " ,type = "error" )
      return(NULL)
    }
    
    ## check if group member present in more than one group
    if(base::anyDuplicated(group_members %>% unlist())  > 0 ){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,
                                   text = "Group members must be unique." ,type = "error" )
      return(NULL)
    }
    
    ll <- rlang::set_names(group_members,group_names)
    tbl <- tibble(groups = names(ll) , group_members = ll) %>% 
      tidyr::unnest()
    
    shinyWidgets::sendSweetAlert(session = session , title = "Success!" ,text = "Groups assigned successfully." ,type = "success" )  
    
    return(tbl)
    
  })
  
  ## 
  sample_groups <- reactiveValues()
  sample_groups$final_sample_groups <- NULL
  
  # default column grouping
  # As per the current settings, as soon as data change default group will be returned regardless of user uploaded groups available or not. 
  observe({
    tbl <- tibble(groups = "All selected samples" , group_members = as.list(sample_names())) %>% 
      tidyr::unnest()
    sample_groups$final_sample_groups <- tbl
  })
  
  
  observe({
    req(user_uploaded_column_groups())
    #if(!is.null(sample_groups$final_sample_groups)){
    sample_groups$final_sample_groups <- user_uploaded_column_groups()
    #}
  })
  
  return(reactive(sample_groups$final_sample_groups))
}
    
## To be copied in the UI
# mod_add_sample_group_ui("add_sample_group_ui_1")
    
## To be copied in the server
# callModule(mod_add_sample_group_server, "add_sample_group_ui_1")
 
