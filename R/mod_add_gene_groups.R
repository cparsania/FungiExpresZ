# Module UI
  
#' @title   add_gene_groups_ui and add_gene_groups
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_add_gene_groups
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
add_gene_groups_ui <-  function(id){
  ns <- NS(id)
  
  
  tagList(
    
    ## row groups action button
    shinyWidgets::actionBttn(inputId = ns("set_gene_groups_trigger") , 
                             label = "Genes" , 
                             color = "success" ,
                             size = "md",
                             block = T,
                             icon = icon("arrows-alt-v","fa-1.5x"),
                             style = "gradient"),
    
    ## row group action button trigger 
    shinyBS::bsModal(id = ns("set_gene_groups_pop") , 
                     trigger = ns("set_gene_groups_trigger") , 
                     title = "Assign gene groups",size = "large",
                     
                     ## upload sample group source 
                     tags$h4(tags$b("Upload gene groups")),
                     
                     tags$h3("Upload gene groups either from file or paste data from clipboard. Follow " , tags$b("Upload instruction") , "given below to know about upload format."),
                     hr(),
                     
                     wellPanel(
                       shiny::radioButtons(inputId = ns("gene_groups_source") , label = "" , 
                                           choices = c("From file" ="gene_group_from_file" ,
                                                       "From clipboard" ="gene_group_from_clipboard") , 
                                           selected = "gene_group_from_file",inline = T, width = "100%"
                       ),
                       conditionalPanel(paste0("input['",ns("gene_groups_source"),"'] == 'gene_group_from_file' "),
                                        shiny::fileInput(inputId = ns("upload_gene_groups_from_file") ,
                                                         label = "Choose .txt file",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv"),
                                                         width = "100%")
                       ),
                       
                       conditionalPanel(paste0("input['",ns("gene_groups_source"),"'] == 'gene_group_from_clipboard' "),
                                        shiny::textAreaInput(inputId = ns("upload_gene_groups_from_clipboard") ,
                                                             rows = 10,cols = 10,
                                                             placeholder = "follow instruction given below",
                                                             label = "Paste sample groups"
                                        )
                       ),
                       
                       ## gene group instruction
                       
                       tags$h4(tags$b("Upload instruction")),
                       tags$div(id = ns("upload_group_file_instruct"), 
                                tags$h5(tags$b("Upload instruction")), 
                                "Input file must be of .txt/.csv format with atleast two columns. If more than two columns found, only first two will be considered. ", tags$b("First column "),"corresponds to group names.", 
                                tags$b("Second column "), 
                                "corresponds to group members. Group members must be the gene names and present in the uploaded data. Each gene must belong to unique group. Example data snap shot have been shown below." , 
                                tags$b("First row "), 
                                "will be considered as header row. User can set headers of their choice."),
                       br()
                     ),
                     hr(),
                     
                     ## column sep
                     tags$h4(tags$b("Column seprator")),
                     wellPanel(
                       radioButtons(inputId = ns("gene_groups_file_sep") ,
                                    label = "" ,
                                    choices = c("Comma" = "," , "Semicolon" = ";" ,"Tab" = "\t") ,
                                    inline = T,selected = "\t" ,width = "100%"
                       )  
                     ),
                     
                     hr(),
                     
                     ## example data snap
                     tags$h4(tags$b("Example data snap")),
                     wellPanel(
                       
                       tags$img(src="www/example_data_gene_group_info.png" , 
                                style="display: block; margin-left: auto; margin-right: auto;" , 
                                height = "50%", width = "50%")
                     ),
                     
                     
                     hr(),
                     
                     ## assign gene groups action button
                     shinyWidgets::actionBttn(inputId = ns("set_gene_groups") ,
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
#' @param gene_names internal
#'
#' @rdname mod_add_gene_groups
#' @keywords internal
add_gene_groups <- function(input, output, session, gene_names){
  
  ## get user defined  gene groups 
  user_uploaded_groups <- eventReactive(input$set_gene_groups, {
    
    #req(input$upload_gene_groups_from_file)
    
    ## get  user uploaded gene groups 
    gene_groups_from_user = tryCatch({
      if(input$gene_groups_source == "gene_group_from_file"){  ## gene group from file 
        
        req(input$upload_gene_groups_from_file)
        
        user_gene_group <- readr::read_delim(file = input$upload_gene_groups_from_file$datapath, 
                                             delim = input$gene_groups_file_sep, col_names = T, trim_ws = T) %>%
          tidyr::drop_na() ## rows will be dropped if value NA found in any column 
        
      }else{  ## gene group from clip board 
        user_gene_group <- text_to_tibble(text = input$upload_gene_groups_from_clipboard , 
                                          sep = input$gene_groups_file_sep) %>% 
          tidyr::drop_na() ## rows will be dropped if value NA found in any column 
      }  
    }, error = function(e) {
      print(e)
      return(NULL)
    } , warning = function(w){
      print(w)
      return(NULL)
    })
    
    print(gene_groups_from_user)
    
    ## show alert/error if error produced while reading file. 
    if(is.null(gene_groups_from_user)){
      
      shinyWidgets::sendSweetAlert(session = session ,type = "error",title = "Error...!!" ,
                                   text = paste("Error while uploading gene groups. Check upload data format.", sep = " "))
      return(NULL)
    }
    
    ## file must have atleast two columns 
    
    ## show alert/error if error produced while reading file. 
    if(gene_groups_from_user %>% ncol() < 2){
      
      shinyWidgets::sendSweetAlert(session = session ,type = "error",title = "Error...!!" ,
                                   text = paste("File must have atlest two columns. Make sure that you selected correct column seperator", 
                                                sep = " "))
      return(NULL)
    }
    
    
    ##  process user gene groups 
    gene_groups_from_user_p <- gene_groups_from_user %>% 
      dplyr::select(1:2) %>% 
      dplyr::rename_all(funs(c("gene_group_names" , "gene_group_members")))%>%
      dplyr::mutate(gene_group_members = gsub(pattern = " " , "_" , gene_group_members)) %>% ## column names replace space to "_"
      tidyr::nest(gene_group_members  ,.key = "gene_group_members")  %>% 
      dplyr::mutate(gene_group_members = purrr::map(gene_group_members, function(ii){return(ii %>% dplyr::pull(1))})) %>%  ## get first column for each grp
      dplyr::slice(gtools::mixedorder(gene_group_names)) ## arrange gene groups by grp names
    
    gene_group_names <- gene_groups_from_user_p %>% dplyr::pull("gene_group_names")
    gene_group_members <- gene_groups_from_user_p %>% dplyr::pull("gene_group_members")
    
    # print("gene_groups")
    # print(gene_group_names)
    # print("gene_group_members")
    # print(gene_group_members)
    
    ## validate group  inputs 
    
    ## check if any group names are empty strings
    if(any(gene_group_names == "")){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Group name cannot be empty." ,type = "error" )
      return(NULL)
    }
    
    ## each group must have atleast one gene assigned
    if( any (lengths(gene_group_members) ==  0 )){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Each group must have atleast one gene assigned. " ,type = "error" )
      return(NULL)
    }
    
    ## number of groups must be < 100. However, this can be problem in scatter plot as it only support 12 unique colors  
    if(gene_group_names %>% length() > 100){
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Total number of groups cannot be more than 100 " ,type = "error" )
      return(NULL)
    }
    
    ## each group must have atleast one gene assigned
    if( any (lengths(gene_group_members) ==  0 )){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Each group must have atleast one gene assigned. " ,type = "error" )
      return(NULL)
    }
    
    ## check if any gene  present in more than one group 
    if(base::anyDuplicated(gene_group_members %>% unlist())  > 0 ){ 
      shinyWidgets::sendSweetAlert(session = session , title = "Error..." ,text = "Each gene must be in one group only." ,type = "error" )
      return(NULL)
    }
    
    ## show warning if more than 2 columns detected
    if(ncol(gene_groups_from_user) > 2){
      shinyWidgets::sendSweetAlert(session = session ,type = "warning",title = "Warning...!!" ,
                                   text = paste("File containes more than 2 columns. Only first 2 will be used.", sep = " "))
      
    }else{
      shinyWidgets::sendSweetAlert(session = session , title = "Success!" ,text = "Gene groups assigned successfully." ,type = "success" )    
    }
    
    ll <- rlang::set_names(gene_group_members,gene_group_names)
    tbl <- tibble(gene_groups = names(ll) , gene_group_members = ll) %>% 
      tidyr::unnest()
    return(tbl)
    
  })
  
  group_info <- reactiveValues()
  group_info$final_gene_groups = NULL 
  
  
  # default gene grouping.
  # As per the current settings, as soon as data change default group will be returned regardless of user uploaded groups available or not. 
  observe({
    tbl <- tibble(gene_groups = "All genes" , gene_group_members = as.list(gene_names())) %>% 
      tidyr::unnest()
    #print("group assignemnt changed ")
    group_info$final_gene_groups = tbl   
  })
  
  ## if user uploaded data available, assign them 
  observe({
    req(user_uploaded_groups())
    group_info$final_gene_groups <- user_uploaded_groups()
  })
  
  return(reactive(group_info$final_gene_groups))
  
  #return(reactive(final_gene_groups()))
}
    

