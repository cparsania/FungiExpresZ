
#' app server
#' 
#' @param input session input
#' @param output session output 
#' @param session session  
#' @importFrom dplyr  %>%
#' @importFrom dplyr  add_count
#' @importFrom dplyr  add_row
#' @importFrom dplyr  add_rownames
#' @importFrom dplyr  add_tally
#' @importFrom dplyr  all_vars
#' @importFrom dplyr  any_vars
#' @importFrom dplyr  arrange
#' @importFrom dplyr  arrange_all
#' @importFrom dplyr  arrange_at
#' @importFrom dplyr  arrange_if
#' @importFrom dplyr  as_data_frame
#' @importFrom dplyr  as_tibble
#' @importFrom dplyr  bind_cols
#' @importFrom dplyr  bind_rows
#' @importFrom dplyr  case_when
#' @importFrom dplyr  count
#' @importFrom dplyr  count_
#' @importFrom dplyr  data_frame
#' @importFrom dplyr  distinct
#' @importFrom dplyr  distinct_all
#' @importFrom dplyr  distinct_at
#' @importFrom dplyr  distinct_if
#' @importFrom dplyr  distinct_prepare
#' @importFrom dplyr  enquo
#' @importFrom dplyr  enquos
#' @importFrom dplyr  everything
#' @importFrom dplyr  expr
#' @importFrom dplyr  filter
#' @importFrom dplyr  filter_all
#' @importFrom dplyr  filter_at
#' @importFrom dplyr  filter_if
#' @importFrom dplyr  first
#' @importFrom dplyr  funs
#' @importFrom dplyr  glimpse
#' @importFrom dplyr  group_by
#' @importFrom dplyr  group_by_
#' @importFrom dplyr  group_by_all
#' @importFrom dplyr  group_by_at
#' @importFrom dplyr  if_else
#' @importFrom dplyr  intersect
#' @importFrom dplyr  last
#' @importFrom dplyr  last_col
#' @importFrom dplyr  left_join
#' @importFrom dplyr  mutate
#' @importFrom dplyr  mutate_
#' @importFrom dplyr  mutate_all
#' @importFrom dplyr  mutate_at
#' @importFrom dplyr  mutate_each
#' @importFrom dplyr  mutate_if
#' @importFrom dplyr  n
#' @importFrom dplyr  n_distinct
#' @importFrom dplyr  n_groups
#' @importFrom dplyr  na_if
#' @importFrom dplyr  order_by
#' @importFrom dplyr  pull
#' @importFrom dplyr  quo
#' @importFrom dplyr  quo_name
#' @importFrom dplyr  quos
#' @importFrom dplyr  rename
#' @importFrom dplyr  rename_all
#' @importFrom dplyr  rename_at
#' @importFrom dplyr  rename_if
#' @importFrom dplyr  rename_vars
#' @importFrom dplyr  rename_vars_
#' @importFrom dplyr  right_join
#' @importFrom dplyr  row_number
#' @importFrom dplyr  rowwise
#' @importFrom dplyr  same_src
#' @importFrom dplyr  sample_frac
#' @importFrom dplyr  sample_n
#' @importFrom dplyr  select
#' @importFrom dplyr  select_
#' @importFrom dplyr  select_all
#' @importFrom dplyr  select_at
#' @importFrom dplyr  select_if
#' @importFrom dplyr  select_var
#' @importFrom dplyr  select_vars
#' @importFrom dplyr  select_vars_
#' @importFrom dplyr  slice
#' @importFrom dplyr  slice_
#' @importFrom dplyr  setdiff
#' @importFrom dplyr  starts_with
#' @importFrom dplyr  summarise
#' @importFrom dplyr  summarise_
#' @importFrom dplyr  summarise_all
#' @importFrom dplyr  summarise_at
#' @importFrom dplyr  summarise_each
#' @importFrom dplyr  summarise_each_
#' @importFrom dplyr  summarise_if
#' @importFrom dplyr  summarize
#' @importFrom dplyr  summarize_
#' @importFrom dplyr  summarize_all
#' @importFrom dplyr  summarize_at
#' @importFrom dplyr  summarize_each
#' @importFrom dplyr  summarize_each_
#' @importFrom dplyr  summarize_if
#' @importFrom dplyr  tally
#' @importFrom dplyr  tally_
#' @importFrom dplyr  tbl
#' @importFrom dplyr  tbl_df
#' @importFrom dplyr  tibble
#' @importFrom dplyr  top_n
#' @importFrom dplyr  tribble
#' @importFrom dplyr  ungroup
#' @importFrom dplyr  union
#' @importFrom dplyr  union_all
#' @importFrom dplyr  vars
#' @importFrom dplyr  contains
#' @importFrom dplyr  if_else
#' @importFrom tidyr %>%
#' @importFrom tidyr drop_na
#' @importFrom tidyr gather
#' @importFrom tidyr nest
#' @importFrom tidyr replace_na
#' @importFrom tidyr separate
#' @importFrom tidyr separate_rows
#' @importFrom tidyr spread
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @importFrom magrittr %>% 
#' @importFrom readr read_csv
#' @importFrom readr read_delim
#' @importFrom readr read_file
#' @importFrom readr read_lines
#' @importFrom readr read_rds
#' @importFrom readr read_tsv
#' @importFrom readr type_convert
#' @importFrom readr write_csv
#' @importFrom readr write_csv2
#' @importFrom readr write_delim
#' @importFrom readr write_file
#' @importFrom readr write_lines
#' @importFrom readr write_rds
#' @importFrom readr write_tsv
#' @importFrom purrr as_function
#' @importFrom purrr as_mapper
#' @importFrom purrr as_vector
#' @importFrom purrr compact
#' @importFrom purrr compose
#' @importFrom purrr is_list
#' @importFrom purrr keep
#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr map_call
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_depth
#' @importFrom purrr map_df
#' @importFrom purrr map_dfc
#' @importFrom purrr map_dfr
#' @importFrom purrr map_if
#' @importFrom purrr map_int
#' @importFrom purrr map_lgl
#' @importFrom purrr map_raw
#' @importFrom purrr map2
#' @importFrom purrr map2_chr
#' @importFrom purrr map2_dbl
#' @importFrom purrr map2_df
#' @importFrom purrr map2_dfc
#' @importFrom purrr map2_dfr
#' @importFrom purrr map2_int
#' @importFrom purrr map2_lgl
#' @importFrom purrr map2_raw
#' @importFrom purrr negate
#' @importFrom purrr partial
#' @importFrom purrr safely
#' @importFrom purrr set_names
#' @importFrom purrr splice
#' @importFrom tibble add_column
#' @importFrom tibble add_row
#' @importFrom tibble as_data_frame
#' @importFrom tibble as_tibble
#' @importFrom tibble as.tibble
#' @importFrom tibble column_to_rownames
#' @importFrom tibble data_frame
#' @importFrom tibble enframe
#' @importFrom tibble glimpse
#' @importFrom tibble repair_names
#' @importFrom tibble rowid_to_column
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom glue glue
#' @importFrom stringr str_c
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_glue
#' @importFrom stringr str_length
#' @importFrom stringr str_match
#' @importFrom stringr str_match_all
#' @importFrom stringr str_order
#' @importFrom stringr str_pad
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace_na
#' @importFrom stringr str_sort
#' @importFrom stringr str_split
#' @importFrom stringr str_wrap
#' @importFrom forcats %>%
#' @importFrom forcats as_factor
#' @importFrom forcats fct_c
#' @importFrom forcats fct_drop
#' @importFrom forcats fct_expand
#' @importFrom forcats fct_inorder
#' @importFrom forcats fct_match
#' @importFrom forcats fct_other
#' @importFrom forcats fct_recode
#' @importFrom forcats fct_relabel
#' @importFrom forcats fct_relevel
#' @importFrom forcats fct_reorder
#' @importFrom forcats fct_reorder2
#' @importFrom clipr clear_clip
#' @importFrom clipr read_clip
#' @importFrom clipr read_clip_tbl
#' @importFrom clipr write_clip
#' @importFrom clipr write_last_clip
#' @importFrom rlang :=
#' @importFrom rlang !!
#' @importFrom rlang !!!
#' @importFrom rlang %@%
#' @importFrom rlang %@%<-
#' @importFrom rlang %|%
#' @importFrom rlang %||%
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang quo
#' @importFrom rlang quos
#' @importFrom rlang set_names
#' @importFrom tm Corpus
#' @importFrom tm Docs
#' @importFrom tm DocumentTermMatrix
#' @importFrom tm removeNumbers
#' @importFrom tm removePunctuation
#' @importFrom tm removeSparseTerms
#' @importFrom tm removeWords
#' @importFrom tm SimpleCorpus
#' @importFrom tm stemDocument
#' @importFrom tm stopwords
#' @importFrom tm TermDocumentMatrix
#' @importFrom tm termFreq
#' @importFrom tm Terms
#' @importFrom tm tm_filter
#' @importFrom tm tm_index
#' @importFrom tm tm_map
#' @importFrom tm tm_parLapply
#' @importFrom tm tm_parLapply_engine
#' @importFrom tm tm_reduce
#' @importFrom tm tm_term_score
#' @importFrom tm VCorpus
#' @importFrom tm VectorSource
#' @importFrom tm content_transformer
#' @importFrom tm stripWhitespace
#' @importFrom MASS kde2d
#' @importFrom janitor clean_names
#' @importFrom broom tidy
#' @importFrom rio convert
#' @importFrom scales squish
#' @keywords internal
app_server <- function(input, output,session) {
  # List the first level callModules here
  
  
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  ## prepare user applied filters' (while selecting pre loaded data) expressions .
  
  ##  NOTE : following filter expressions will be used to filter the sample_info (pre loaded data) table.
  ## Therefore, column names - `Organism` and `strain` (given as names of vector) must be identical to the column names of the sample_info table.
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Display, filter and select existing data ----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ### strains by species 
  strains_by_species <- reactive({
    req(sample_info)
    sample_info %>% 
      dplyr::select(species, strain, genotype) %>%
      dplyr::group_by(species) %>% 
      dplyr::summarise(genotype = list(genotype) , strain = list(strain)) 
  })
  
  ## Update species on sra sample info table 
  observe({
    req(strains_by_species())
    ## all species 
    available_species <- base::unique(strains_by_species()$species)
    
    shinyWidgets::updatePickerInput(session = session, 
                                    inputId = "select_species", 
                                    choices = available_species,
                                    selected = "Aspergillus nidulans")
  })
  
  ## get selected species strain
  selected_species_strains <- reactive({
    req(input$select_species ,strains_by_species())
    
    ## strains for selected species 
    available_strains <- strains_by_species() %>% 
      dplyr::filter(species == input$select_species) %>%
      dplyr::select(strain) %>% 
      tidyr::unnest() %>%
      tidyr::drop_na() %>%
      pull(1) %>%
      unique() %>% 
      sort()
    
    return(available_strains)
  })
  
  ## update strain info
  observe({
    req(selected_species_strains())
    shinyWidgets::updatePickerInput(session = session, 
                                    inputId = "select_strain", 
                                    choices = selected_species_strains())
  })
  
  ## reset strain filter 
  observeEvent(input$reset_strain_filter , {
    shinyWidgets::updatePickerInput(session = session, 
                                    inputId = "select_strain",
                                    choices = selected_species_strains())
    
    
  })
  
  ## get genotypes for selected species 
  selected_species_genotype <- reactive({
    req(input$select_species ,strains_by_species())
    
    ## genotype for selected species 
    
    available_genotype <- strains_by_species() %>% 
      dplyr::filter(species == input$select_species) %>%
      dplyr::select(genotype) %>% 
      tidyr::unnest() %>%
      tidyr::drop_na() %>%
      pull(1) %>%
      unique() %>% 
      sort()
    
    return(available_genotype)
  })
  
  
  ## update genotype  info
  observe({
    req(selected_species_genotype())
    shinyWidgets::updatePickerInput(session = session, 
                                    inputId = "select_genotype", 
                                    choices = selected_species_genotype())
  })
  
  ## reset genotype  filter 
  observeEvent(input$reset_genotype_filter , {
    shinyWidgets::updatePickerInput(session = session, 
                                    inputId = "select_genotype", 
                                    choices = selected_species_genotype())
    
    
  })
  
  ## show number of rows selected 
  user_selected_rows  <- reactive({
    if(is.null(input$pre_loaded_data_sra_sample_info_rows_selected)){
      return(0)
    }else{
      return(length(input$pre_loaded_data_sra_sample_info_rows_selected))  
    }
  })
  
  ## Render text for number of rows selected 
  output$sra_info_number_of_rows_selected <- renderText(
    paste(user_selected_rows() ,"row(s) selected")
  )
  
  ## Update strain on sra sample info table 
  user_filters_expressions <- reactive({
    u_filters_vals <- c(species = input$select_species, 
                        strain = input$select_strain , 
                        genotype =  input$select_genotype)
    u_filters_expr <- lapply(names(u_filters_vals), function(col) {
      my_filter(col, "==", value = u_filters_vals[[col]])
    })
    return(u_filters_expr)
  })
  
  ## SRA sample info table user display 
  sra_sample_info_user_display <- reactive({
    ## add ncbi urls whereever possible 
    dd <- sample_info %>% 
      dplyr::filter(!!!user_filters_expressions())
    return(dd)
  })
  
  ## pre loaded data
  output$pre_loaded_data_sra_sample_info <- DT::renderDataTable({
    return(sra_sample_info_user_display()) #%>% 
    # dplyr::mutate(run_accession = 
    #                 map_chr(run_accession , 
    #                         ~ tags$a( .x, href = paste("https://www.ncbi.nlm.nih.gov/sra/" , 
    #                                                    .x , 
    #                                                    sep = "") , target = "blank") %>% 
    #                           as.character()
    #                         ))
  },
  selection = "multiple",
  class = "cell-border stripe",
  rownames = FALSE,
  extensions = c("Buttons"),
  server = F,
  options = list(
    scrollX = TRUE,
    dom = "Blfrtip",
    autoWidth = TRUE,
    searchHighlight = TRUE,
    columnDefs = list(
      list(width = '700px', targets = c(5, 6,9)), # Fix width columns 
      list(targets = c(2,3, 8:18), visible = FALSE)# The column number must be identical to the columns in colvis extension 
    ),  
    buttons =
      list("copy", 
           list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download"),
           
           list(extend = "colvis", columns = c(2,3,8:18))
      ), # end of buttons customization
    lengthMenu = list(
      c(10, 20 ,50, 70, 100,500 ) # declare values
      , c(10, 20 ,50, 70, 100,500 ) # declare titles
    ),
    pageLength = 10
  ), ## end of options
  )
  
  ## reset rows selection in preloaded data sample information 
  observeEvent(input$deselect_all_rows, {
    proxy <- DT::dataTableProxy("pre_loaded_data_sra_sample_info")
    DT::selectRows(proxy = proxy, NULL)
  })
  
  ## select all rows in preloaded data sample information 
  observe({
    #req(pre_loaded_data_sra_sample_info())
    proxy = DT::dataTableProxy("pre_loaded_data_sra_sample_info")
    
    if (is.null(input$sra_sample_info_select_all_rows)) {
      DT::selectRows(proxy = proxy, NULL)
    } else if(input$sra_sample_info_select_all_rows == "TRUE") {
      DT::selectRows(proxy = proxy, input$pre_loaded_data_sra_sample_info_rows_all)
    }
    
  })
  
  ## user selected SRA id sample info
  user_selected_sra_id_sample_info <- eventReactive(input$submit_user_sra_samples, {
    req(input$pre_loaded_data_sra_sample_info_rows_selected)
    
    selected_rows <- input$pre_loaded_data_sra_sample_info_rows_selected
    n_select <-  selected_rows %>% length() 
    
    n_max_sra_limit <-1000
    if(n_select > n_max_sra_limit){
      shinyWidgets::sendSweetAlert(session = session ,
                                   type = "warning",
                                   title = "Warning...!!" ,
                                   text = paste("Due to memory limit, current version allows maximum", 
                                                n_max_sra_limit, 
                                                "samples. You have selected " ,
                                                n_select, "." ,
                                                "Application will continue with first",
                                                n_max_sra_limit,  "samples.",
                                                sep = " "))
      selected_rows <- selected_rows[1:n_max_sra_limit]
    }
    
    ##  process user applied filter expressions
    user_selected_data_sra_ids <- sra_sample_info_user_display() %>%
      dplyr::slice(selected_rows)
    
  })
  
  ## prepare tibble having sra_id and bioproject_id columns 
  user_selected_sra_id_to_bioproject_id <- reactive({
    user_selected_sra_id_sample_info() %>% dplyr::select(bio_project , run_accession) %>% 
      dplyr::mutate_all( ~replace_na(. ,"--NA--"))
  })
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## User selected SRA data  ----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## user selected data  
  user_selected_sra_id_data <- eventReactive(input$submit_user_sra_samples,{
    req(user_selected_sra_id_sample_info() )
    
    
    ##  process user applied filter expressions
    user_selected_data_sra_ids <- user_selected_sra_id_sample_info()%>%
      dplyr::select(23) %>% ## column 23  is Run/SRA id. IMP: change this, if column order in the sample_info table changed.
      dplyr::pull(1)
    
    ## Objective : load species specific data. 
    ## Reference annotation name of respective species will be used to load species specific data 
    ## 1st step is to get the user reference annotation name (e.g FungiDB-39_AnidulansFGSCA4 for Aspergillus nidulans and  FungiDB-39_CalbicansSC5314 for Candida albicans)
    ## Once reference annotation name obtained, use it to get the respective species expression .rds file name. 
    ## Reference annotations to .rds file name mapping given in the rds object "reference_annotation_to_expr_map.rds" 
    ## once the name of .rds file obtained, load it and get the user selected data 
    
    user_selected_species_ref_annot <- sra_sample_info_user_display()%>%
      dplyr::select(2) %>% ## column 2  is get the name of ref annot 
      dplyr::pull(1) %>% 
      as.character() %>% 
      .[1]
    
    #ref_annot_to_expr_rds  contains two cols  ---> 'reference_annotation' , 'expression_mat_data_file'
    ## load respective expression mat     
    expr_data_mat_rds_file <- ref_annot_to_expr_rds %>% 
      dplyr::filter(.data$reference_annotation == !!user_selected_species_ref_annot) %>% 
      dplyr::select(2) %>% ## 2nd column is expression_mat_data_file
      pull(1) %>% 
      .[1] ### it make sure that if reference annot match to multiple rows, it only returns .rds data mat file for first match. 
    
    
    ## load expr data
    withProgress(message = "Loadind data from .rds" , {
      incProgress(0.5)
      #print("expr_mat_file_path")
      #print(paste(get_expression_mats_dir_path() , expr_data_mat_rds_file , sep = "/"))
      
      expr_data_mat <- readr::read_rds(paste(get_expression_mats_dir_path() , expr_data_mat_rds_file , sep = "/"))
      
      incProgress(1)
    })
    
    user_selected_sra_id_data <- expr_data_mat %>% 
      dplyr::select(1, !!user_selected_data_sra_ids) 
    
    
    return(user_selected_sra_id_data)
    
  })
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ### display active group info in the in side panel ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  #observe(group_info,{
  callModule(module = display_active_groups_server ,
             id = "display_active_groups",
             group_data = reactive(group_info)
  )
  
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ### display sample info for user selected sample info in side panel ----
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  observeEvent(user_selected_sra_id_sample_info(),{
    req(user_selected_sra_id_sample_info())
    
    callModule(module = display_sra_sample_info_server ,
               id = "user_selected_sra_id_sample_info"  ,
               user_selected_sra_sample_info_tibble = reactive(user_selected_sra_id_sample_info())
    )
    
  })
  
  ## set output status for sra sample info display on the side panel.
  ## UI will displayed only if sra samples selected by users
  output$display_sra_sample_info_in_side_panel <- reactive({
    req(user_selected_sra_id_sample_info())
    return(TRUE)
  })
  
  outputOptions(x = output , name = "display_sra_sample_info_in_side_panel" , suspendWhenHidden = FALSE)
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Process user uploaded  data ----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## user upoaded  data  
  user_uploaded_data <- eventReactive(input$submit_user_uploaded_data,{
    
    req(input$upload_data_source)
    
    upload_data_source <- input$upload_data_source
    
    data_from_user = tryCatch({
      if(upload_data_source == "upload_from_file"){
        req(input$file1)
        data_from_user <- readr::read_delim(file = input$file1$datapath, delim = input$sep, col_names = T, trim_ws = T)  
      }else if(upload_data_source == "upload_from_clipboard") {
        data_from_user <- text_to_tibble(text = input$user_pasted_data , sep = input$sep)
      }  
      
    }, error = function(e) {
      print(paste("File uploading error." , e, sep = " "))
      return(NULL)
    } , warning = function(w){
      print(paste("File uploaded with warnings." , w, sep = " "))
      return(NULL)
    })
    
    ## show alert/error if error produced while reading file. 
    if(is.null(data_from_user)){ 
      shinyWidgets::sendSweetAlert(session = session ,type = "error",title = "Error...!!" ,text = paste("Error while reading the data. Probably unsupported file format (if uploaded from file) or uncorrect pasted data.", sep = " "))
      return(NULL)
    }
    
    ## error if first column is not type char
    if(!purrr::map_lgl(data_from_user[1], is.character) ){ 
      shinyWidgets::sendSweetAlert(session = session ,type = "error",title = "Error...!!" ,text = paste("First column must be of type character.", sep = " "))
      return(NULL)
    }
    
    ## other than 1st column, all must be of type nueric 
    if( ! all(purrr::map_lgl(data_from_user[-1], is.numeric)) ){ 
      shinyWidgets::sendSweetAlert(session = session ,type = "error",title = "Error...!!" ,text = paste("Other than first column, all must be of type numeric.", sep = " "))
      return(NULL)
    }
    
    ## column 1 must not have duplicates.
    if(any(base::duplicated(data_from_user[[1]]))){ 
      shinyWidgets::sendSweetAlert(session = session ,type = "error" ,title = "Error...!!" ,text = paste("Column 1 must have unique values.", sep = " "))
      return(NULL)
    }
    
    ## uploded data must have atleast two columns 
    if(base::ncol(data_from_user) < 2){ 
      shinyWidgets::sendSweetAlert(session = session ,type = "error",title = "Error...!!" ,text = paste("Uploaded data must have atleast 2 columns.\n Make sure that you have selected correct separator.", sep = " "))
      return(NULL)
    }
    
    ## remove genes which are 0 in all samples 
    processed_data <- data_from_user %>% 
      tidyr::drop_na() %>% ## genes will be dropped if any column contain NA
      dplyr::filter_if(is.numeric , .vars_predicate = dplyr::any_vars(. != 0 )) ## select genes which have atleast one non zero value
    
    ## log2 trasform user uploaded data 
    if(input$log_transform_user_upload_data == "log2"){
      processed_data <- processed_data %>% 
        dplyr::filter_if(is.numeric , dplyr::any_vars( . > 0)) %>% ## remove genes having negative values (value < 0) in any sample
        dplyr::mutate_if(is.numeric , ~(log2( . + 1)))
    }
    
    ## log10 trasform user uploaded data 
    if(input$log_transform_user_upload_data == "log10"){
      processed_data <- processed_data %>% 
        dplyr::filter_if(is.numeric , dplyr::any_vars( . > 0)) %>%## remove genes having negative values (value < 0) in any sample
        dplyr::mutate_if(is.numeric , ~(log10( . + 1))) 
    }
    
    ## remove na and round the values. 
    processed_data <- processed_data %>% 
      dplyr::mutate_if(is.numeric, round, 4) %>% ## 4 digits after decimal
      as_tibble() %>% 
      tidyr::drop_na() ## rows containing NA (after log transform) will be removed 
    
    ## check the number of rows in the processed data. if number of row remain zero throw error.
    if(base::nrow(processed_data) == 0 ){ 
      warning_text <- paste("All genes have value NA after log transformation (if selected) in one of the sample or value 0 in all the samples. Please upload correct data. If log transformation used, make sure that values are non-negative. ")
      shinyWidgets::sendSweetAlert(session = session ,
                                   type = "error",
                                   title = "Error...!!" ,
                                   text = warning_text)
      return(NULL)
    }
    
    ## Update alert type depending on number of genes remained after removing NA. 
    ## if number of rows in the processed data less than user uploaded rows, throw warning.
    
    if(base::nrow(processed_data) < base::nrow(data_from_user)){ 
      genes_removed <- dplyr::setdiff(data_from_user[[1]] , processed_data[[1]])
      type = "warning"
      title = "Data upload with warning...!!!"
      numb_of_genes_removed <- base::nrow(data_from_user) - base::nrow(processed_data)
      text <- tags$h5(numb_of_genes_removed, 
                      "  genes have been removed due to one of the following reasons...", 
                      tags$br(),tags$br(),
                      "1). Genes have value NA in one of the sample. NA found either directly from uploaded data or due to log transformation (if selected).", 
                      tags$br(),tags$br(),
                      "2). Genes have value 0 in all the samples.",
                      tags$br(),tags$br(), 
                      "Removed genes are ...",
                      tags$br(),tags$br(),
                      tags$h5(paste0(genes_removed , collapse = ","),
                              style = "height: 50px;white-space:nowrap;overflow-x:auto;"), style = "text-align:justify;")
      
      ## if number of rows in the processed data equal to the user uploaded rows, show success. 
    } else if(base::nrow(processed_data) == base::nrow(data_from_user)) {
      type = "success"
      title = "Success...!!!"
      text <- paste("Data uploaded successfully.")
    }
    
    shinyWidgets::sendSweetAlert(session = session ,
                                 type = type,
                                 title = title ,
                                 text = text )
    
    ## column names replace space to "_", this must be applied to groups also when group info uploaded from file
    user_uploaded_processed_data <- processed_data %>%  
      rename_all(function(.){gsub(pattern = " " , "_" , .)})
    
    return(user_uploaded_processed_data)
  })
  
  ## disable UI elems if upload example  data checked on  
  observe({
    if(input$upload_sample_data){
      shinyjs::hide(id ="user_data_upload_section")
      shinyWidgets::updateProgressBar(session = session , id = "upload_sample_data_pb" , value = 100)
      
    } else {
      shinyjs::show(id ="user_data_upload_section")
      shinyWidgets::updateProgressBar(session = session , id = "upload_sample_data_pb" , value = 0)
    }
    
    ## enable disable  data upload. at a time either of the upload from file or upload from clipboard will be enabled   
    if(input$upload_data_source == "upload_from_file"){
      shinyjs::enable(id = "file1")
    } else{
      shinyjs::disable(id = "file1")
    }
    
  })
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Finalize active plot data  ----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  plot_data <- reactiveVal(NULL)
  
  ## when user hit action  button on sra sample info page, update plot data. 
  observeEvent(input$submit_user_sra_samples ,{
    req(user_selected_sra_id_data())
    
    final_data <- user_selected_sra_id_data() %>% 
      tidyr::drop_na() %>% 
      dplyr::filter_if(is.numeric , .vars_predicate = dplyr::any_vars(. != 0 )) ## select genes which have atleast one non zero value
    
    if(nrow(final_data) == 0 ){ ## send error if number of rows remained after removing na is 0. 
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "All the rows have atleast one missing or NA value found. Input file needs to be fixed.",
        type = "error"
      )
      plot_data(NULL)
    }
    
    ## display success alert  when data uploaded 
    sendSweetAlert(
      session = session,
      title = paste(final_data %>% ncol() - 1 , "data selected"),
      text = paste("Selected data now available for analysis.\n\nRun accession will be used as identifier for downstream analysis.\n\nClose selection window to use data further.\n\n"),
      type = "success"
    )
    plot_data(final_data)
  })
  
  
  ## if user upload the data , make it plot data 
  observeEvent(user_uploaded_data(),{
    req(user_uploaded_data()) ## to execute this user data must be available
    final_data <- user_uploaded_data() %>%
      tidyr::drop_na() %>%
      dplyr::filter_if(is.numeric , .vars_predicate = dplyr::any_vars(. != 0 )) ## select genes which have atleast one non zero value
    plot_data(final_data)
  })
  
  ## make plot data to example data when th eaction button input$upload_sample_data triggered 
  observeEvent(input$upload_sample_data,{
    
    if(input$upload_sample_data){
      example_data_file  =  system.file("app", "cartoon_data" ,"cartoon_log2fpkm_log2fc.txt" ,package = "FungiExpresZ" )
      example_data <- readr::read_delim(file = example_data_file , delim = "\t") %>% 
        tidyr::drop_na() %>%
        dplyr::filter_if(is.numeric , .vars_predicate = dplyr::any_vars(. != 0 )) ## select genes which have atleast one non zero value
      plot_data(example_data)
    } 
    ## if input$upload_sample_data == FALSE set NULL
    else {
      plot_data(NULL)
    }
  })
  
  ## when submit_user_uploaded_data  triggered and join data  button is on 
  
  observeEvent(input$submit_user_uploaded_data,{
    req(input$join_user_data, user_uploaded_data()) ## to execute this user data must be available  
    
    user_upload_join_by <-  colnames(user_uploaded_data())[1]
    
    merge_data <- user_uploaded_data() %>% 
      dplyr::left_join(user_selected_sra_id_data() , by = set_names("geneName" , user_upload_join_by)) 
    merge_data <- merge_data %>% tidyr::drop_na() ## rows with NA after merger will be removed. 
    
    ## send warning  if number of rows remained after removing na is < user_uploaded_data()
    if(merge_data %>% nrow() < user_uploaded_data() %>% nrow() && merge_data %>% nrow() > 0){
      sendSweetAlert(
        session = session,
        title = "Warning...!",
        text = paste0("Only ", merge_data %>% nrow(), " out of " , user_uploaded_data() %>% nrow() , " genes matched to database ID. Application will continue with ",  merge_data %>% nrow() , " genes."),
        type = "warning"
      )
      final_data <- merge_data %>% 
        dplyr::filter_if(is.numeric , .vars_predicate = dplyr::any_vars(. != 0 )) ## select genes which have atleast one non zero value
      plot_data(final_data)
    }
    
    ## send warning  if number of rows remained after removing na is 0 return user uploaded data 
    if(nrow(merge_data) == 0 ){ 
      sendSweetAlert(
        session = session,
        title = "Warning...",
        text = "None of the ID from first column of uploaded data match with database id. Either fix the input id or application continue with uploaded data.",
        type = "warning"
      )
      final_data <- user_uploaded_data() %>%
        tidyr::drop_na() %>% 
        dplyr::filter_if(is.numeric , .vars_predicate = dplyr::any_vars(. != 0 )) ## select genes which have atleast one non zero value
      plot_data(final_data)
    }
    
    ## send success message if All the IDs from the first column of uploaded data match with database id
    if(merge_data %>% nrow() >= user_uploaded_data() %>% nrow()){
      sendSweetAlert(
        session = session,
        title = "Data joined successfully...",
        text = "All the IDs from the first column of uploaded data matched with database id.",
        type = "success"
      )
      final_data <- merge_data %>% tidyr::drop_na() %>% 
        dplyr::filter_if(is.numeric , .vars_predicate = dplyr::any_vars(. != 0 )) ## select genes which have atleast one non zero value
      plot_data(final_data)
    }
  })
  
  ## print final plot data on terminal 
  observe({
    req(plot_data())
    # print("plot_data dims")
    # print(dim(plot_data()))
    # print("plot_data")
    # print(plot_data())
  })
  
  
  ## preserve plot data original row order 
  plot_data_original_row_order <- reactive({
    req(plot_data())
    orig_row_ord <- plot_data() %>% 
      dplyr::select(1) %>% 
      dplyr::mutate(!!generate_random_strings(1) := dplyr::row_number())
    return(orig_row_ord)
  })
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Define  groups  ----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  ##reactive values containing group info 
  group_info <- reactiveValues()
  
  ## User defined column groups 
  user_defined_column_groups <- callModule(module  = add_sample_group , id =  "sample_group" , 
                                           sample_names = reactive(base::colnames(plot_data())[-1]) ,
                                           bioproject_groups =  reactive(user_selected_sra_id_to_bioproject_id()))
  
  
  ## Assign user groups to reactive values. Below observe will be executed if user_defined_groups changed 
  observe({
    group_info$column_groups <- user_defined_column_groups()
  })
  
  ## User defined row groups 
  user_defined_row_groups <-  callModule(module  = add_gene_groups , session = session,
                                         id =  "gene_groups" ,
                                         gene_names = reactive( plot_data() %>% .[[1]] ))
  
  
  
  ## Assign user defined row groups 
  observe({
    group_info$row_groups <- user_defined_row_groups()
  })
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Reset UI elems of each plot panel based on data being uploaded or selected from pre-loaded    ----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  ### if no rows selected, remove x/y choices
  observe({
    # if ((input$data_selection == "select_sample" &
    #     base::is.null(input$pre_loaded_data_sra_sample_info_rows_selected)) |
    #     (input$data_selection == "upload_user_data" & base::is.null(input$file1))
    #     ) {
    
    if(is.null(plot_data())){
      
      ## update scatter x
      updateSelectInput(
        session = session,
        inputId = "scatter_x",
        choices = character(0)
      )
      
      ## update scatter y
      updateSelectInput(
        session = session,
        inputId = "scatter_y",
        choices = character(0)
      )
      
      ## update multi scatter  x
      updateSelectInput(
        session = session,
        inputId = "multi_scatter_vars",
        choices = character(0)
      )
      
      
      ## update density x
      updateSelectInput(
        session = session,
        inputId = "density_x",
        choices = character(0)
      )
      
      ## update box x
      updateSelectInput(
        session = session,
        inputId = "box_x",
        choices = character(0)
      )
      
      ## update line x
      updateSelectInput(
        session = session,
        inputId = "lineplot_x",
        choices = character(0)
      )
      
      ## joy plot 
      updateSelectInput(
        session = session,
        inputId = "joy_plot_x",
        choices = character(0)
      )
      
      ## heatmap vars 
      updateSelectInput(
        session = session,
        inputId = "heatmap_vars",
        choices = character(0)
      )
      
      ## pca plot 
      updateSelectInput(
        session = session,
        inputId = "pca_plot_vars",
        choices = character(0)
      )
      
      ## corr heat box 
      updateSelectInput(
        session = session,
        inputId = "corr_heatbox_vars",
        choices = character(0)
      )
      
      ## violin 
      updateSelectInput(
        session = session,
        inputId = "violin_x",
        choices = character(0)
      )
      ## histogram 
      updateSelectInput(
        session = session,
        inputId = "histogram_x",
        choices = character(0)
      )
      
      ## bar  
      updateSelectInput(
        session = session,
        inputId = "barplot_vars",
        choices = character(0)
      )
      
    }
    ### if no file uploaded, remove x/y choices
    # if (input$data_selection == "upload_user_data" & base::is.null(input$file1)) {
    #   
    #   ## update scatter x
    #   updateSelectInput(
    #     session = session,
    #     inputId = "scatter_x",
    #     choices = character(0)
    #   )
    #   
    #   ## update scatter y
    #   updateSelectInput(
    #     session = session,
    #     inputId = "scatter_y",
    #     choices = character(0)
    #   )
    #   
    #   ## update multi scatter  x
    #   updateSelectInput(
    #     session = session,
    #     inputId = "multi_scatter_vars",
    #     choices = character(0)
    #   )
    #   
    #   
    #   ## update density x
    #   updateSelectInput(
    #     session = session,
    #     inputId = "density_x",
    #     choices = character(0)
    #   )
    #   
    #   ## update box x
    #   updateSelectInput(
    #     session = session,
    #     inputId = "box_x",
    #     choices = character(0)
    #   )
    #   
    #   ## update line x
    #   updateSelectInput(
    #     session = session,
    #     inputId = "lineplot_x",
    #     choices = character(0)
    #   )
    #   
    #   ## joy plot 
    #   updateSelectInput(
    #     session = session,
    #     inputId = "joy_plot_x",
    #     choices = character(0)
    #   )
    #   
    #   ## heatmap vars 
    #   updateSelectInput(
    #     session = session,
    #     inputId = "heatmap_vars",
    #     choices = character(0)
    #   )
    #   
    #   ## pca plot 
    #   updateSelectInput(
    #     session = session,
    #     inputId = "pca_plot_vars",
    #     choices = character(0)
    #   )
    #   
    #   ## corr heat box 
    #   updateSelectInput(
    #     session = session,
    #     inputId = "corr_heatbox_vars",
    #     choices = character(0)
    #   )
    #   
    #   ## violin 
    #   updateSelectInput(
    #     session = session,
    #     inputId = "violin_x",
    #     choices = character(0)
    #   )
    #   ## histogram 
    #   updateSelectInput(
    #     session = session,
    #     inputId = "histogram_x",
    #     choices = character(0)
    #   )
    #   
    #   ## bar  
    #   updateSelectInput(
    #     session = session,
    #     inputId = "barplot_vars",
    #     choices = character(0)
    #   )
    #   
    #   
    # }
    
  })
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Fix reference annotations ----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  genome_for_annotations <-  reactiveVal(NULL)
  
  ## reference annot when user choose SRA data 
  observeEvent(input$submit_user_sra_samples , {
    
    #if(input$data_selection == "select_sample"){ ## when data selected from existing one 
    req(input$pre_loaded_data_sra_sample_info_rows_selected)
    
    genome_for_annotations <- sample_info %>% ## sample_info is pre loaded data
      dplyr::filter(!!!user_filters_expressions()) %>% ## filter by user applied filters
      dplyr::select(2) %>% ## column 2 is reference_annotation columns. it contains genome name identical to fungidb annotations. IMP: change this, if column order in the sample_info table changed.
      dplyr::slice(input$pre_loaded_data_sra_sample_info_rows_selected)%>% ## user selected rows
      dplyr::pull(1)
    
    genome_for_annotations(genome_for_annotations[1])
  })
  
  ## reference annot when user upload data or select example data 
  observe({
    genome_for_annotations(input$user_selected_species)
    if(input$upload_sample_data){
      genome_for_annotations("FungiDB-42_CalbicansSC5314")  
    }
  })
  
  
  ## Check current annotations 
  observe({
    req(genome_for_annotations())
    print(paste("User selected geneome", genome_for_annotations() ,sep = " "))
  })
  
  
  ## For selected genome  sample ids
  output$sample_selected_species_gene_id <- renderText({
    req(genome_for_annotations())
    sample_ids_to_display <- tryCatch({
      ah_data_summary2 %>%  
        dplyr::filter(genome == genome_for_annotations()) %>% 
        dplyr::select(gr_cols) %>% 
        tidyr::unnest() %>%
        dplyr::pull(ID) %>% sample(20) %>% paste0(collapse = ", ")  
    }, error = function(x){
      return("Trouble to get Ids")
    })
    
    return(sample_ids_to_display)
    
  })
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # scatter plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  #
  ## Question : Both evenReactive and observeEvent can be invalidated by action button.
  ## Then, why am I using eventReactive here ? Why not observeEvent ??
  ## Ans :: eventReactive is there to delay the reactions. Given the reactive value, it triggers the chunk of code and
  ## return some values, which can be cached and used further in the app. While, observeEvent is also there to trigger the action
  ## on the basis of given reactive values but it does not return anything. It has side effects. Side effects are the actions due
  ## to functions but without returning anything.
  
  
  ## check whether x and y are numeric type
  scatter_vars_validated <- eventReactive(input$generate_scatter, {
    req(input$scatter_x, input$scatter_y, plot_data())
    
    
    x_vals <- plot_data()[, input$scatter_x][[1]]
    y_vals <- plot_data()[, input$scatter_y][[1]]
    
    ## validate user selected variables
    if (!is.numeric(x_vals)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = "X must be of type numeric",
        type = "error"
      )
      return(FALSE)
    } else if (!is.numeric(y_vals)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Y must be of type numeric",
        type = "error"
      )
      return(FALSE)
    }
    
    return(TRUE)
  })
  
  ## update plot axis limits, reason to use observe event is,
  ## it won't cache the values in the computer memory. Whenever user clicks on "Generate Plot" (input$plot) invalidated
  ## the axis limits will be updated.
  
  observeEvent(input$generate_scatter, {
    req(scatter_vars_validated())
    
    # selected x y cols
    
    x_vals <- plot_data()[, input$scatter_x][[1]]
    y_vals <- plot_data()[, input$scatter_y][[1]]
    
    # calculate axis limits
    # xlim
    x_range <- round(range(x_vals), 3)
    
    # ylim
    y_range <- round(range(y_vals), 3)
    
    # update UI
    # X min and max
    updateNumericInput(
      session = session,
      inputId = "plot_xmin",
      min = x_range[1],
      max = x_range[2],
      value = x_range[1]
    )
    
    updateNumericInput(
      session = session,
      inputId = "plot_xmax",
      min = x_range[1],
      max = x_range[2],
      value = x_range[2]
    )
    # Y min and max
    updateNumericInput(
      session = session,
      inputId = "plot_ymin",
      min = y_range[1],
      max = y_range[2],
      value = y_range[1]
    )
    
    updateNumericInput(
      session = session,
      inputId = "plot_ymax",
      min = y_range[1],
      max = y_range[2],
      value = y_range[2]
    )
  })
  
  ## update x, y variables
  observe({
    
    ## update x
    updateSelectInput(
      session = session,
      inputId = "scatter_x",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
    
    ## update y
    updateSelectInput(
      session = session,
      inputId = "scatter_y",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  
  ## scatter plot user selected gene group data 
  scatter_plot_data <- callModule(module  = gene_group_selection , 
                                  id = "scatter_plot_select_gene_groups" , 
                                  gene_group_info =  reactive(group_info$row_groups), 
                                  generate_plot_action = reactive(input$generate_scatter),
                                  current_session_data_matrix = plot_data)
  
  ### current scatter plot genome, updates only when  generate scatter hits. 
  current_scatter_plot_genome <- eventReactive(input$generate_scatter , {
    return(genome_for_annotations())
  })
  
  
  ## get scatter plot elements : corr value, density,  intercept , slope and base ggplot 
  scatter_plot_elems <- eventReactive(input$generate_scatter, {
    req(scatter_plot_data())
    
    xvar <- scatter_plot_data() %>% dplyr::pull(input$scatter_x)
    yvar <- scatter_plot_data() %>% dplyr::pull(input$scatter_y)
    
    # linear model param
    mm <- lm(formula = yvar ~ xvar)
    linear_model_param <- broom::tidy(mm)
    
    # calculte corr value
    xvar <- scatter_plot_data() %>% dplyr::pull(input$scatter_x)
    yvar <- scatter_plot_data() %>% dplyr::pull(input$scatter_y)
    
    corr <- cor(xvar, yvar)
    scatter_xy_corr <- format(round(corr, 3), nsmall = 2) ## restric digits after decimal
    
    # calculate density
    x_y_density <- get_density(scatter_plot_data()[, input$scatter_x][[1]], scatter_plot_data()[, input$scatter_y][[1]])
    
    # xy scatter plot
    gp <- ggplot(scatter_plot_data(), aes_string(x = as.symbol(input$scatter_x), y = as.symbol(input$scatter_y))) 
    
    gff_feat =  ah_data_summary2 %>% 
      dplyr::filter(genome == current_scatter_plot_genome()) %>% 
      dplyr::select(gr_cols) %>% 
      tidyr::unnest()
    
    return(list(linear_model_param = linear_model_param, 
                scatter_xy_corr = scatter_xy_corr, gp = gp, 
                x_y_density = x_y_density, 
                gff_feat = gff_feat))
  })
  
  # observe({
  #   req(scatter_plot_data())
  #   #print(scatter_plot_data())
  # })
  
  
  ## scatterplot condition panel:show only if variable (x, y) validated and displayed on UI
  output$scatter_plot_status <- reactive({
    req(final_scatter())
    return(TRUE)
  })
  
  outputOptions(output, "scatter_plot_status", suspendWhenHidden = FALSE)
  
  ## final scatter plot 
  final_scatter <- reactive({
    req(scatter_vars_validated() , scatter_plot_elems() ,
        input$plot_xmin, input$plot_xmax,input$plot_ymin, input$plot_ymax
    )
    # req(input$plot_xmax,input$plot_xmin) ## all four xmin, xmax, ymin and ymax must be there to generate the plot
    
    withProgress(message = "Preparing scatter plot", {
      incProgress(0.5)
      
      ## color by density,  default colors   
      if (input$scatter_color_chooser == "default") {
        gp <- scatter_plot_elems()$gp + 
          geom_point(aes(color = scatter_plot_elems()$x_y_density), 
                     alpha = input$scatter_alpha , size= input$scatter_point_size) +
          viridis::scale_color_viridis()
      }
      
      ## color by density,  user  defined colors 
      if (input$scatter_color_chooser == "manual") {
        gp <- scatter_plot_elems()$gp + 
          geom_point(aes(color = scatter_plot_elems()$x_y_density), alpha = input$scatter_alpha , size= input$scatter_point_size) +
          scale_color_gradientn(colours = c(input$scatter_col_low, input$scatter_col_medium, input$scatter_col_high))
      }
      
      ## color by genesets ,   default  colors 
      if (input$scatter_color_chooser == "by_gene_groups") {
        gp <- scatter_plot_elems()$gp + 
          geom_point(aes(color = gene_groups),  alpha = input$scatter_alpha , size= input$scatter_point_size)  + 
          scale_color_discrete()
      }
      
      # real time change of axis limits.
      gp <- gp +
        xlim(input$plot_xmin, input$plot_xmax) +
        ylim(input$plot_ymin, input$plot_ymax)
      
      ## default plot title 
      gp <- callModule(module = plot_title_and_axis_label_server , 
                       id = "scatter_plot_title_and_labels", 
                       my_ggplot = gp ,
                       plot_title = base::paste("Pearson correlation:", 
                                                scatter_plot_elems()$scatter_xy_corr, sep = ""), 
                       axis_x_title = shiny::isolate(input$scatter_x), 
                       axis_y_title = shiny::isolate(input$scatter_y),
                       x_tick_angle = 0, 
                       color_legend_title = ifelse(input$scatter_color_chooser == "by_gene_groups" , "Groups", "Density")
                       
      )
      
      ##  add / remove regression  line
      if (input$scatter_diagonal_line == "manual") {
        gp <- gp + geom_abline(slope = input$manual_slope, intercept = input$manual_intercept)
      }
      
      if (input$scatter_diagonal_line == "from_data") {
        i_cept <- scatter_plot_elems()$linear_model_param %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
        slope <- scatter_plot_elems()$linear_model_param %>% dplyr::filter(term == "xvar") %>% pull(estimate)
        gp <- gp + geom_abline(intercept = i_cept, slope = slope ) 
      }
      incProgress(1)
    })
    return(gp)
    
  })
  
  ## render scatter plot
  output$scatter_plot <- renderPlot({
    req(final_scatter())
    final_scatter()
  },
  height = function() {
    return(session$clientData$output_scatter_plot_width) ## dynamic height
  },
  
  width = function() {
    return(session$clientData$output_scatter_plot_width) ## dynamic height
  }, res = 96
  ) ## dynamic resolution
  
  ## render corr value
  output$scatter_xy_corr <- renderText({
    return(scatter_plot_elems()$scatter_xy_corr)
  })
  
  ## brush table for scatter plot
  scatter_plot_brushed_points <- eventReactive(input$plot_brush, {
    req(scatter_plot_data())
    
    scatter_plot_brushed_points <- brushedPoints(
      df = scatter_plot_data(),
      brush = input$plot_brush,
      xvar = scatter_plot_elems()$gp$labels$x,  #input$scatter_x
      yvar = scatter_plot_elems()$gp$labels$y
    )
    
    #map metadata info to geneNames
    join_col_x <- base::colnames(scatter_plot_brushed_points)[1] ## first column containing geneNames
    join_col_y <- base::colnames(scatter_plot_elems()$gff_feat)[7] ## "id" column
    
    display_data <- scatter_plot_brushed_points %>%
      left_join(scatter_plot_elems()$gff_feat, by = setNames(join_col_y, join_col_x)) %>%
      dplyr::select(1,c("seqnames", "start", "end","width", "strand", "description"), dplyr::everything())
    
    return(display_data)
  })
  
  ## render brushed data
  output$brush_table <- DT::renderDataTable({
    req(scatter_plot_brushed_points())
    return(scatter_plot_brushed_points() %>%
             dplyr::mutate_all(funs(replace_na(as.character(.),"--NA--"))) %>% 
             dplyr::mutate_if(is.numeric , round  , 2))
    
  },selection  = "none",  
  server = F,
  extensions = c("Buttons"),
  options = list(
    deferRender = TRUE,
    scrollX = TRUE,
    dom = "Blfrtip",
    searchHighlight = TRUE,
    buttons =
      list("copy", 
           list(extend ="collection", 
                buttons = c("csv", "excel", "pdf"), text = "Download"
           )), # end of buttons customization
    
    # customize the length menu
    lengthMenu = list(
      c(10, 50, 100, 500 ) # declare values
      , c(10, 50, 100, 500) # declare titles
    ),
    pageLength = 10
  )
  )
  
  ## call module to download scatter plot 
  callModule(module = export_plot, 
             id = "export_scatter", 
             file_name = "scatter",  
             plot = final_scatter
  )
  
  ## call module for scatter plot functional analysis  
  callModule(module = functional_analysis_server , 
             id = "scatterplot_functional_analysis_ui" , 
             ui_id = "scatterplot_functional_analysis_ui",
             session = session,
             gene_set = reactive(split(x = scatter_plot_brushed_points()[[1]],  ## genename
                                       f = "1"))  ,
             genome = reactive(genome_for_annotations()) )
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # multi scatter plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  ## check whether x and y are numeric type
  multi_scatter_vars_validated <- eventReactive(input$generate_multi_scatter, {
    req(input$multi_scatter_vars)
    
    selected_vars <- plot_data() %>% dplyr::select(input$multi_scatter_vars)
    col_class <- plot_data() %>% 
      dplyr::select(input$multi_scatter_vars) %>% 
      dplyr::summarise_all(is.numeric) %>% 
      tidyr::gather()
    
    if (!all(col_class$value)) {
      non_numeric_cols <- col_class %>% dplyr::filter(!value)
      
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = paste0(paste0(non_numeric_cols$key, collapse = " "), " must be of type numeric", collapse = ""),
        type = "error"
      )
      return(FALSE)
    } 
    
    if (length(input$multi_scatter_vars) > 5) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = paste0("Due to memory limit, maximum 5 variables allowed at moment. Try CorrHeatBox for more than 5 variables."),
        type = "error"
      )
      return(FALSE)
    } 
    else {
      return(TRUE)
    }
  })
  
  ## multi scatterplot condition panel:show only if variable (x, y) validated and displayed on UI
  output$multi_scatter_plot_status <- reactive({
    req(final_multi_scatter_plot())
    return(TRUE)
  })
  outputOptions(output, "multi_scatter_plot_status", suspendWhenHidden = FALSE)
  
  
  ## update x variables
  observe({
    req(plot_data())
    ## update x
    updateSelectInput(
      session = session,
      inputId = "multi_scatter_vars",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  
  ## multi scatter plot user selected gene group data 
  multi_scatter_plot_data <- callModule(module  = gene_group_selection , 
                                        id = "multi_scatter_plot_gene_group_selection" , 
                                        gene_group_info =  reactive(group_info$row_groups), 
                                        generate_plot_action = reactive(input$generate_multi_scatter),
                                        current_session_data_matrix = plot_data
  )
  
  ## prepare multi scatter plot
  ggpair_plot <- eventReactive(input$generate_multi_scatter, {
    req(multi_scatter_plot_data() , multi_scatter_vars_validated())
    
    gp <- GGally::ggpairs(multi_scatter_plot_data(), 
                          columns = c(input$multi_scatter_vars), 
                          upper = list(continuous = GGally::wrap("cor", 
                                                                 size = input$multi_scatter_corr_text_text_size , 
                                                                 color = input$multi_scatter_corr_text_col)))
    
    return(gp)
  })
  
  ## multi scatter final plot
  final_multi_scatter_plot <- reactive({
    req(ggpair_plot())
    
    multi_scatter_gp <- ggpair_plot()
    
    ## decorate multi scatter through module 
    multi_scatter_gp <- callModule(module = plot_title_and_axis_label_server , 
                                   id = "multi_scatter_plot_title_and_labels" ,
                                   my_ggplot = multi_scatter_gp , 
                                   axis_x_title = "Value" , 
                                   axis_y_title = "Value",
                                   x_tick_angle = 0 , 
                                   aspect_ratio =NULL)
    return(multi_scatter_gp)
  })
  
  output$multi_scatter_plot <- renderPlot({
    req(final_multi_scatter_plot())
    return(
      withProgress(message = "Display multi-scatter plot in progress",{
        incProgress(0.5)
        print(final_multi_scatter_plot())  
        incProgress(1)
      })
    )  
  },
  height = function() {
    return(session$clientData$output_multi_scatter_plot_width)
  }, ## dynamic height
  width = function() {
    return(session$clientData$output_multi_scatter_plot_width)
  }
  ) ## dynamic width
  
  ## call module to export multi scatter plot
  callModule(module = export_plot, 
             id = "export_multi_scatter" , 
             file_name = "multi_scatter_plot", 
             plot = final_multi_scatter_plot )
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # CorrHeatBox server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update x, y variables
  observe({
    ## update x
    updateSelectInput(
      session = session,
      inputId = "corr_heatbox_vars",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  
  ## check whether selected x var(s) are of numeric type
  corr_heatbox_vars_validated <- eventReactive(input$generate_corr_heatbox, {
    req(input$corr_heatbox_vars)
    
    ## validate selected variables
    selected_non_num_vars <- plot_data() %>% 
      dplyr::select(c(input$corr_heatbox_vars)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  ## corr_heatbox  status 
  output$corr_heatbox_status <- reactive({
    req(final_corr_heatbox())
    return(TRUE)
  })
  outputOptions(output, "corr_heatbox_status", suspendWhenHidden = FALSE)
  
  ## corr heat box user selected gene group data 
  corr_heatbox_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                           id = "corr_heat_box_gene_group_selection" , 
                                                           gene_group_info =  reactive(group_info$row_groups), 
                                                           generate_plot_action = reactive(input$generate_corr_heatbox),
                                                           current_session_data_matrix = plot_data
  )
  
  
  ## prepare corr_heatbox data 
  corr_heatbox_data <- eventReactive(input$generate_corr_heatbox, {
    req(input$corr_heatbox_vars, corr_heatbox_vars_validated() , corr_heatbox_user_selected_gene_group_data())
    
    withProgress(message = "Preparing CorrHeatBox " , {
      incProgress(0.5)
      
      ## tibble to data.frame with row names 
      corr_heatbox_data <- corr_heatbox_user_selected_gene_group_data() %>% 
        dplyr::select(1,c(input$corr_heatbox_vars)) %>% 
        as.data.frame() %>% 
        tibble::column_to_rownames(colnames(corr_heatbox_user_selected_gene_group_data())[1]) 
      
      ## calculate correlation 
      corr_mat <- tryCatch({
        cor_mat <- stats::cor(corr_heatbox_data)  
      },error = function(e){
        return(NULL)  
      })
      incProgress(1)
    })
    return(corr_mat)
  })
  
  
  ## show error if corr data is NULL 
  observe({
    if(is.null(corr_heatbox_data())){
      shinyWidgets::sendSweetAlert(session = session,title = "Error..!",
                                   text = "Error while calculating correlation (stats::cor). Please check the data.")
    }
  })
  
  
  ## final corrHeatBox
  final_corr_heatbox <- reactive({
    req(corr_heatbox_data())
    
    chb <- tryCatch({
      chb <- ggcorrplot::ggcorrplot(corr = corr_heatbox_data() , 
                                    method = input$heatbox_method , 
                                    type =  input$heatbox_type,
                                    hc.order = input$cluster_heatbox,
                                    show.diag =  as.logical(input$heatbox_show_diag),
                                    lab_col = input$heatbox_corr_text_col , 
                                    lab_size = input$heatbox_corr_text_text_size,
                                    lab = input$heatbox_show_corr_value
      ) 
    },error = function(e){
      return(NULL)
    })
    
    ## return NULL (at function)  if ggcorrplot::ggcorrplot throws an error 
    if(is.null(chb)){
      return(NULL)
    }
    
    ## set color scale auto: 
    if(input$corr_heatbox_scale_manual == "auto"){
      chb <- chb + 
        scale_fill_gradientn(colours = viridis::viridis(n = input$corr_heatbox_colors) %>% rev())
    }
    
    ## set color scale manual
    if(input$corr_heatbox_scale_manual == "manual"){
      chb <- chb +
        scale_fill_gradientn(colours = viridis::viridis(n = input$corr_heatbox_colors) %>% rev(),
                             limits = c(input$corr_heatbox_scale_manual_min , input$corr_heatbox_scale_manual_max) , oob=scales::squish)
    }
    
    ## decorate plot 
    chb <- callModule(plot_title_and_axis_label_server,
                      id = "corr_heatbox_title_and_labels" ,
                      my_ggplot = chb, x_tick_angle = 45 , 
                      fill_legend_title = "Corr",
                      apply_theme =FALSE)
    
    return(chb)
  })
  
  ## show error if corr data is NULL 
  observe({
    if(is.null(final_corr_heatbox())){
      shinyWidgets::sendSweetAlert(session = session,title = "Error..!",
                                   type = "error",
                                   text = "Error while generating corr plot (ggcorrplot::ggcorrplot). Make sure that you selected at least two samples or else check input data.")
    }
  })
  
  ## render corr_heatbox
  output$corr_heatbox <- renderPlot({
    req(final_corr_heatbox())
    return(print(final_corr_heatbox()))
  }, height = function() {
    return(session$clientData$output_corr_heatbox_width)
  }, width = function() {
    return(session$clientData$output_corr_heatbox_width)
  }, res = 90)
  
  ## render corr heat box data 
  output$corr_heatbox_data <- DT::renderDataTable({
    req(corr_heatbox_data())
    return(corr_heatbox_data() %>% 
             as.data.frame() %>% 
             rownames_to_column("RowNames") %>% 
             as_tibble() %>% 
             dplyr::mutate_if(is.numeric, function(i){round(i,2)}))
  },selection  = "none",  
  server = F,
  extensions = "Buttons",
  options = list(
    scrollX = TRUE,
    dom = "Blfrtip",
    searchHighlight = TRUE,
    buttons =
      list("copy", 
           list(extend ="collection", 
                buttons = c("csv", "excel", "pdf"), text = "Download"
           )), # end of buttons customization
    
    # customize the length menu
    lengthMenu = list(
      c(10, 50, 100, 500, -1) # declare values
      , c(10, 50, 100, 500, "All") # declare titles
    ),
    pageLength = 10
  )
  )
  
  ## export corr heat box 
  callModule(module = export_plot, 
             id = "export_corr_heatbox" , 
             file_name = "corr_heatbox", 
             plot = final_corr_heatbox )
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # density plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update x, y variables
  observe({
    ## update x
    updateSelectInput(
      session = session,
      inputId = "density_x",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## check whether selected x var(s) are of numeric type
  density_vars_validated <- eventReactive(input$generate_density, {
    req(input$density_x)
    
    ## validate selected variables
    selected_non_num_vars <- plot_data() %>%
      dplyr::select(c(input$density_x)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  ## density plot status 
  output$density_plot_status <- reactive({
    req(final_density_plot())
    return(TRUE)
  })
  outputOptions(output, "density_plot_status", suspendWhenHidden = FALSE)
  
  
  ## density plot user selected gene group data 
  density_plot_data <- callModule(module  = gene_group_selection , 
                                  id = "density_plot_gene_group_selection" , 
                                  gene_group_info =  reactive(group_info$row_groups), 
                                  generate_plot_action = reactive(input$generate_density),
                                  current_session_data_matrix = plot_data
  )
  
  
  ## density plot
  dp <- eventReactive(input$generate_density, {
    req(density_plot_data() , density_vars_validated())
    
    withProgress(message = "Preparing density plot" , {
      incProgress(0.5)
      
      plot_data <- density_plot_data() %>% 
        dplyr::select(c(input$density_x , !!as.symbol("gene_groups"))) 
      
      melted <- plot_data %>%
        tidyr::gather(samples, value , -gene_groups) %>% 
        left_join(group_info$column_groups , by = c (samples = "group_members")) %>% ## addd sample groups 
        tidyr::replace_na(list(groups = "No groups assigned")) %>% ## NA will be converted to "No groups assigned"
        dplyr::arrange(groups , samples)%>%
        dplyr::mutate(samples = forcats::fct_inorder(samples))
      incProgress(1)
    })
    
    ##  base density plot 
    dp <- ggplot(melted, aes(x = value, fill = samples))
    return(dp)
  })
  
  ## final density plot 
  
  final_density_plot <- reactive({
    ## dynamic alpha
    req(dp())
    dp <- dp() + geom_density(alpha = input$density_plot_alpha) #+ theme_bw()
    
    ## fill and facet density plot 
    out <- callModule(module = ggplot_fill_and_facet , 
                      id = "density_plot_fill_and_facet" ,
                      ggplot = dp , allow_x_var_selection = FALSE)
    dp <- out$plot
    
    ## decorate plot 
    dp <- callModule(plot_title_and_axis_label_server , 
                     id = "decorate_density_plot" , 
                     my_ggplot = dp , 
                     axis_x_title = "Value",
                     x_tick_angle = 0,
                     axis_y_title = "Density",
                     fill_legend_title = out$fill_var,
                     color_legend_title = out$fill_var
                     
    )
    return(dp)
  })
  
  ## render density plot
  output$density_plot <- renderPlot({
    return(print(final_density_plot()))
  }, height = function() {
    return(session$clientData$output_density_plot_width)
  }, width = function() {
    return(session$clientData$output_density_plot_width)
  }, res = 90)
  
  ## prepare density plot data to be shown as table
  density_filtered_column <- eventReactive(dp(), {
    dd <- plot_data()
    return(dd)
  })
  
  ## render density plot data
  output$density_filtered_column_display <- DT::renderDataTable({
    return(density_filtered_column())
  })
  
  
  callModule(module = export_plot, id = "export_density" , file_name = "density_plot", plot = final_density_plot )
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # histogram server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update x, y variables
  observe({
    ## update x
    updateSelectInput(
      session = session,
      inputId = "histogram_x",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## check whether selected x var(s) are of numeric type
  histogram_vars_validated <- eventReactive(input$generate_histogram, {
    req(input$histogram_x)
    
    ## validate selected variables
    selected_non_num_vars <- plot_data() %>%
      dplyr::select(c(input$histogram_x)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  ## histogram status 
  output$histogram_status <- reactive({
    req(final_histogram())
    return(TRUE)
  })
  outputOptions(output, "histogram_status", suspendWhenHidden = FALSE)
  
  ## get histogram gene group specific data 
  histogram_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                        id = "histogram_gene_group_selection" , 
                                                        gene_group_info =  reactive(group_info$row_groups), 
                                                        generate_plot_action = reactive(input$generate_histogram),
                                                        current_session_data_matrix = plot_data)
  
  
  
  ## histogram
  hg <- eventReactive(input$generate_histogram, {
    req(input$histogram_x, histogram_vars_validated() , histogram_user_selected_gene_group_data())
    
    withProgress(message = "Preparing histogram" , {
      incProgress(0.5)
      plot_data <- histogram_user_selected_gene_group_data() %>%
        dplyr::select(c(input$histogram_x , !!as.symbol("gene_groups"))) 
      
      melted <- plot_data %>% 
        tidyr::gather(samples, value , -gene_groups) %>% 
        left_join(group_info$column_groups , by = c (samples = "group_members")) %>% 
        tidyr::replace_na(list(groups = "No groups assigned")) %>% ## NA will be converted to "No groups assigned"
        dplyr::arrange(groups , samples)%>%
        dplyr::mutate(samples = forcats::fct_inorder(samples))
      incProgress(1)
      
    })
    hg <- ggplot(melted, aes(x = value, fill = samples, col = samples))
    return(hg)
  })
  
  ## final histogram
  final_histogram <- reactive({
    ## dynamic alpha
    req(hg())
    
    hg <- hg() + geom_histogram(alpha = input$histogram_alpha , 
                                position = input$histogram_positions , 
                                bins  = input$histogram_number_of_bins , col ="black")
    
    
    ## fill and facet histogram 
    out <- callModule(module = ggplot_fill_and_facet , 
                      id = "histogram_fill_and_facet" ,
                      ggplot = hg , allow_x_var_selection = FALSE)
    
    hg <- out$plot
    
    ## decorate plot 
    hg <- callModule(plot_title_and_axis_label_server , 
                     id = "decorate_histogram" , 
                     my_ggplot = hg , 
                     axis_x_title = "Value",
                     x_tick_angle = 0,
                     axis_y_title = "Count",
                     fill_legend_title = out$fill_var,
                     color_legend_title = out$fill_var
                     
    )
    ## remove legend title 
    return(hg)
  })
  
  ## render histogram
  output$histogram <- renderPlot({
    return(print(final_histogram()))
  }, height = function() {
    return(session$clientData$output_histogram_width)
  }, width = function() {
    return(session$clientData$output_histogram_width)
  }, res = 90)
  
  callModule(module = export_plot, id = "export_histogram" , file_name = "histogram", plot = final_histogram)
  
  
  
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Joy plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update x, y variables
  observe({
    ## update x
    updateSelectInput(
      session = session,
      inputId = "joy_plot_x",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## check whether selected x var(s) are of numeric type
  joy_plot_vars_validated <- eventReactive(input$generate_joy_plot, {
    req(input$joy_plot_x)
    
    ## validate selected variables
    selected_non_num_vars <- plot_data() %>%
      dplyr::select(c(input$joy_plot_x)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  ## multi scatterplot condition panel:show only if variable (x, y) validated and displayed on UI
  output$joy_plot_status <- reactive({
    req(final_joy_plot())
    return(TRUE)
  })
  outputOptions(output, "joy_plot_status", suspendWhenHidden = FALSE)
  
  ## joy plot user selected gene group data 
  
  joy_plot_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                       id = "joy_plot_gene_group_selection" , 
                                                       gene_group_info =  reactive(group_info$row_groups), 
                                                       generate_plot_action = reactive(input$generate_joy_plot),
                                                       current_session_data_matrix = plot_data)
  
  
  
  ## joy plot
  ggjoy_plot <- eventReactive(input$generate_joy_plot, {
    req(input$joy_plot_x, joy_plot_vars_validated() , joy_plot_user_selected_gene_group_data())
    
    plot_data <- joy_plot_user_selected_gene_group_data() %>% 
      dplyr::select(c(input$joy_plot_x) , gene_groups) %>% 
      tidyr::gather(samples, value , -gene_groups) %>% 
      left_join(group_info$column_groups , by = c(samples = "group_members")) %>% 
      tidyr::replace_na(list(groups = "No groups assigned")) %>% ## NA will be converted to "No groups assigned"
      dplyr::mutate(samples = forcats::fct_inorder(samples)) %>% ## preserve original order 
      dplyr::mutate(groups = forcats::fct_inorder(groups))# %>%
    
    ggjoy_plot <- ggplot(plot_data) + aes(x = value)
    
    return(ggjoy_plot)
  })
  
  ## final joy plot 
  final_joy_plot <- reactive({
    req(ggjoy_plot())
    
    
    ## fill and facet  
    ggjoy_plot <- ggjoy_plot()
    out <- callModule(module = joyplot_fill_and_facet , id = "joy_plot_fill_and_facet" , joyplot = ggjoy_plot)
    ggjoy_plot <- out$plot
    
    withProgress(message = "Preparing joy plot",{
      incProgress(0.5)
      ggjoy_plot <- ggjoy_plot + 
        scale_x_continuous(expand = c(0.01, 0)) +
        scale_y_discrete(expand = c(0.01, 0)) #+ 
      #theme_bw()
      
      ## decorate joy plot 
      ggjoy_plot <- callModule(plot_title_and_axis_label_server , 
                               id = "decorate_joy_plot" , 
                               my_ggplot = ggjoy_plot ,
                               axis_x_title = ggjoy_plot$mapping$x  %>% rlang::quo_text(),
                               axis_y_title = ggjoy_plot$mapping$y  %>% rlang::quo_text(),
                               x_tick_angle = 0,
                               fill_legend_title = out$fill_var)
      incProgress(1)
    })
    return(ggjoy_plot)
    
  })
  
  ## render density plot
  output$joy_plot <- renderPlot({
    ## dynamic alpha
    req(final_joy_plot())
    return(print(final_joy_plot()))
  }, height = function() {
    return(session$clientData$output_joy_plot_width)
  }, width = function() {
    return(session$clientData$output_joy_plot_width)
  }, res = 90)
  
  callModule(module = export_plot , id = "export_joy_plot" , file_name = "joy_plot" , plot = final_joy_plot)
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # box plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update box plot variables
  observeEvent(plot_data(), {
    updateSelectInput(
      session = session,
      inputId = "box_x",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## validate boxplot selected variables
  box_vars_validated <- eventReactive(input$generate_box, {
    req(input$box_x)
    
    selected_non_num_vars <- plot_data() %>%
      dplyr::select(c(input$box_x)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  ## boxplot condition panel : show only if selected variable(s) validated and displayed on UI
  output$box_plot_status <- reactive({
    req(final_box_plot())
    return(TRUE)
  })
  outputOptions(output, "box_plot_status", suspendWhenHidden = FALSE)
  
  ## box plot user selected gene group data
  box_plot_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                       id = "box_plot_gene_group_selection" , 
                                                       gene_group_info =  reactive(group_info$row_groups), 
                                                       generate_plot_action = reactive(input$generate_box),
                                                       current_session_data_matrix = plot_data)
  
  
  ## box plot
  boxplt <- eventReactive(input$generate_box, {
    req(input$box_x , box_vars_validated() , box_plot_user_selected_gene_group_data())
    
    box_plot_data <- box_plot_user_selected_gene_group_data() %>% 
      dplyr::select(c(input$box_x , "gene_groups")) %>% 
      tidyr::gather(key = samples , value = value , -gene_groups) %>% 
      dplyr::left_join(group_info$column_groups , by = c(samples = "group_members")) %>% 
      tidyr::replace_na(list(groups = "No groups assigned")) %>% ## NA will be converted to "No groups assigned"
      dplyr::mutate(samples = forcats::fct_inorder(samples)) %>%
      dplyr::mutate(groups = forcats::fct_inorder(groups))# %>%
    
    
    ## base plot
    bp <- ggplot(box_plot_data)  
    return(bp)
  })
  
  final_box_plot <- reactive({
    req(boxplt())
    withProgress(message = "Preparing box plot" , {
      incProgress(0.5)
      
      
      
      boxplt <- boxplt()
      
      ## y axis and alpha 
      boxplt <-  boxplt + geom_boxplot(alpha = input$box_plot_alpha) + 
        aes(y = value)
      
      ## fill and facet box plot 
      out <- callModule(module = ggplot_fill_and_facet , 
                        id = "box_plot_fill_and_facet" ,
                        ggplot = boxplt)
      
      boxplt <- out$plot
      
      
      # dynamic display pvalue
      if (input$box_plot_pvalue == "TRUE") {
        
        box_x_var_type <- boxplt$data %>% 
          dplyr::pull(!!boxplt$mapping$x) %>% 
          unique() %>% as.character()
        
        if(length(box_x_var_type) >= 2){
          all_combin <- combn(box_x_var_type, 2, simplify = F)
          boxplt <- boxplt + 
            ggpubr::stat_compare_means(comparisons = all_combin, 
                               method = input$box_plot_pval_method)
          
        }
      }
      
      ## box plot dots 
      if(input$box_plot_show_dots %>% as.logical()){
        boxplt <- boxplt + geom_jitter(width = input$boxplot_dots_width , color = input$boxplot_dots_color , alpha = input$boxplot_dots_transprancy)
      }
      
      
      ## decorate box plot 
      boxplt <- callModule(module = plot_title_and_axis_label_server ,
                           id = "box_plot_label_and_title",
                           my_ggplot = boxplt,
                           axis_x_title = boxplt$mapping$x  %>% rlang::quo_text(),
                           axis_y_title = boxplt$mapping$y  %>% rlang::quo_text(),
                           fill_legend_title = boxplt$mapping$fill  %>% rlang::quo_text(),
                           x_tick_angle = 90,
      )
      incProgress(1)  
    })
    
    return(boxplt)
    
  })
  
  ## render boxplot
  output$box_plot <- renderPlot({
    req(final_box_plot())
    return(print(final_box_plot()))
  }, height = function() {
    return(session$clientData$output_box_plot_width)
  }, width = function() {
    return(session$clientData$output_box_plot_width)
  }, res = 96)
  
  
  ## export box plot 
  callModule(module = export_plot , id = "export_box_plot" , file_name = "box_plot" ,plot = final_box_plot)
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # violin plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update violin plot variables
  observeEvent(plot_data(), {
    updateSelectInput(
      session = session,
      inputId = "violin_x",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## validate violin plot selected variables
  violin_vars_validated <- eventReactive(input$generate_violin, {
    req(input$violin_x)
    
    selected_non_num_vars <- plot_data() %>%
      dplyr::select(c(input$violin_x)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  ## violin plot condition panel : show only if selected variable(s) validated and displayed on UI
  output$violin_plot_status <- reactive({
    req(final_violin_plot())
    return(TRUE)
  })
  outputOptions(output, "violin_plot_status", suspendWhenHidden = FALSE)
  
  ## violin plot data 
  
  violin_plot_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                          id = "violin_plot_gene_group_selection" , 
                                                          gene_group_info =  reactive(group_info$row_groups), 
                                                          generate_plot_action = reactive(input$generate_violin),
                                                          current_session_data_matrix = plot_data)
  
  ## violin plot
  violinplt <- eventReactive(input$generate_violin, {
    req(input$violin_x , violin_vars_validated() , violin_plot_user_selected_gene_group_data())
    
    violin_plot_data <- violin_plot_user_selected_gene_group_data() %>% 
      dplyr::select(c(input$violin_x) , gene_groups) %>% 
      tidyr::gather(key = samples , value = value , -gene_groups) %>% 
      left_join(group_info$column_groups , by = c(samples = "group_members")) %>% 
      tidyr::replace_na(list(groups = "No groups assigned")) %>% ## NA will be converted to "No groups assigned"
      dplyr::mutate(samples = forcats::fct_inorder(samples)) %>%
      dplyr::mutate(groups = forcats::fct_inorder(groups))# %>%
    
    
    ## fill violin plot by variable, default
    vp <- ggplot(violin_plot_data)  
    return(vp)
  })
  
  
  final_violin_plot <- reactive({
    req(violinplt())
    withProgress(message = "Preparing violin plot" , {
      incProgress(0.5)
      violin_plot_vars <- isolate(input$violin_x)
      violinplt <- violinplt()
      #violinplt <-  violinplt + aes(x = samples, y = value , fill = samples ) 
      
      ## y axis and alpha 
      violinplt <-  violinplt + 
        geom_violin(alpha = input$violin_plot_alpha , draw_quantiles = c(input$violin_show_quantile))+ 
        aes(y = value)
      
      
      # # dynamic fill and alpha
      # violinplt <- violinplt +  
      #   geom_violin(alpha = input$violin_plot_alpha , draw_quantiles = c(input$violin_show_quantile)) 
      # 
      # 
      
      
      ## fill and facet violin plot 
      out <- callModule(module = ggplot_fill_and_facet , 
                        id = "violin_plot_fill_and_facet" ,
                        ggplot = violinplt)
      
      violinplt <- out$plot
      
      
      
      
      # dynamic display pvalue
      if (input$violin_plot_pvalue == "TRUE") {
        
        violin_x_var_type <- violinplt$data %>% 
          dplyr::pull(!!violinplt$mapping$x) %>% 
          unique() %>% as.character()
        
        if(length(violin_x_var_type) >= 2){
          all_combin <- combn(violin_x_var_type, 2, simplify = F)
          violinplt <- violinplt + 
            ggpubr::stat_compare_means(comparisons = all_combin, method = input$violin_plot_pval_method)  
        }
      }
      
      
      # decorate box plot
      violinplt <- callModule(module = plot_title_and_axis_label_server ,
                              id = "violin_plot_label_and_title",
                              my_ggplot = violinplt,
                              axis_x_title = violinplt$mapping$x  %>% rlang::quo_text(),
                              axis_y_title = violinplt$mapping$y  %>% rlang::quo_text(),
                              x_tick_angle = 90,
                              fill_legend_title = violinplt$mapping$fill  %>% rlang::quo_text()
      )
      
      
      
      incProgress(1)  
    })
    
    return(violinplt)
    
  })
  
  ## render violine
  output$violin_plot <- renderPlot({
    req(final_violin_plot())
    return(print(final_violin_plot()))
  }, height = function() {
    return(session$clientData$output_violin_plot_width)
  }, width = function() {
    return(session$clientData$output_violin_plot_width)
  }, res = 96)
  
  
  ## export box plot 
  callModule(module = export_plot , id = "export_violin_plot" , file_name = "violin_plot" ,plot = final_violin_plot)
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # bar plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update bar plot variables
  observeEvent(plot_data(), {
    updateSelectInput(
      session = session,
      inputId = "barplot_vars",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## update bar plot genes
  observeEvent(plot_data(), {
    updateSelectInput(
      session = session,
      inputId = "barplot_select_genes",
      choices = plot_data() %>% dplyr::pull(1), ## first column is gene name 
      selected = plot_data() %>% dplyr::pull(1) %>% .[1]
    )
  })
  
  ## validate barplot selected variables
  barplot_vars_validated <- eventReactive(input$generate_barplot, {
    req(input$barplot_vars)
    
    selected_non_num_vars <- plot_data() %>%
      dplyr::select(c(input$barplot_vars)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  ## barplot condition panel : show only if selected variable(s) validated and displayed on UI
  output$barplot_status <- reactive({
    req(final_barplot())
    return(TRUE)
  })
  outputOptions(output, "barplot_status", suspendWhenHidden = FALSE)
  
  
  ## barplot
  barplot <- eventReactive(input$generate_barplot, {
    req(input$barplot_vars , barplot_vars_validated(),input$barplot_select_genes)
    
    barplot_data <- plot_data() %>% 
      dplyr::rename_at(1 , funs(return("GeneNames"))) %>%## set first column name as geneNames
      dplyr::select(1, c(input$barplot_vars)) %>% 
      dplyr::filter(GeneNames %in% input$barplot_select_genes) %>% 
      tidyr::gather(key = samples , value = value ,  -GeneNames) %>% 
      left_join(group_info$column_groups , by = c(samples = "group_members")) %>% 
      tidyr::replace_na(list(groups = "No groups assigned")) %>% ## NA will be converted to "No groups assigned"
      dplyr::mutate(samples = forcats::fct_inorder(samples)) %>% ## order by first appearance 
      dplyr::mutate(groups = forcats::fct_inorder(groups))# %>% ## order by first appearance
    
    ## if user groups uploaded chunk below make sure that order in each group is as per user uploaded samples
    
    if(TRUE){ ## original (user supplied) sample order
      barplot_data <- barplot_data %>%
        dplyr::mutate(samples = forcats::fct_relevel(samples ,group_info$column_groups %>% pull(2)  %>%
                                                       as.character() %>% unique() )) #%>% ## order by group members
    }
    
    if(TRUE){ ## original (user supplied) column group order
      barplot_data <- barplot_data %>%
        dplyr::mutate(groups = forcats::fct_relevel(groups ,group_info$column_groups %>% pull(1) %>%
                                                      as.character() %>% unique()))
    }
    
    
    
    ## barplot base ggplot 
    barplot <- ggplot(barplot_data)  
    return(barplot)
  })
  
  final_barplot <- reactive({
    req(barplot())
    withProgress(message = "Preparing bar plot" , {
      incProgress(0.5)
      
      barplot <- barplot()
      
      ## current bar plot data 
      current_bar_plot_data <- barplot$data
      
      ## dynamic color aesthatic (color_by_var)  is opposite to x axis aesthatic. 
      ## can be one of the : samples or GeneNames. is the gene column is always named as GeneName ?
      color_by_var <- ifelse(input$barplot_xaxis_choices == "samples" , "GeneNames" , "samples")
      
      barplot <-  barplot + 
        geom_col(aes(x = !!as.symbol(input$barplot_xaxis_choices), 
                     y = value   ,  col = !!(as.symbol(color_by_var)) ) , position = "dodge" ,
                 alpha = input$barplot_alpha) + 
        
        scale_color_manual(breaks = current_bar_plot_data[[color_by_var]] %>% unique(), 
                           values = c(rep("black", current_bar_plot_data[[color_by_var]] %>% 
                                            unique() %>% length() ))) +  
        guides(color = FALSE, size = FALSE)
      
      
      ## fill by groups or variable.
      if(input$fill_barplot != "identical"){
        barplot <- barplot + aes(fill = !!as.name(input$fill_barplot))   
      }
      
      ## fill bar plot identical
      if (input$fill_barplot == "identical") {
        barplot <- barplot + 
          aes(fill = input$barplot_color_chooser) + ## fake aesthatic 
          scale_fill_manual(breaks = current_bar_plot_data[[color_by_var]] %>% unique(), 
                            values = c(rep(input$barplot_color_chooser, current_bar_plot_data[[color_by_var]] %>% 
                                             unique() %>% length() )))
      }
      
      ## facet bar plot 
      if(input$barplot_facet_value != "none") {
        barplot <- barplot + 
          facet_wrap(~ eval(as.symbol(input$barplot_facet_value)) , scales = c(input$barplot_yscale ))
      }
      
      ## decorate box plot 
      barplot <- callModule(module = plot_title_and_axis_label_server ,
                            id = "barplot_label_and_title",
                            my_ggplot = barplot,
                            axis_x_title = input$barplot_xaxis_choices,
                            axis_y_title = "Value",
                            x_tick_angle = 90,
                            fill_legend_title = input$fill_barplot
      )
      incProgress(1)  
    })
    
    return(barplot)
    
  })
  
  ## render barplot
  output$barplot <- renderPlot({
    req(final_barplot())
    return(print(final_barplot()))
  }, height = function() {
    return(session$clientData$output_barplot_width)
  }, width = function() {
    return(session$clientData$output_barplot_width)
  }, res = 96)
  
  
  ## export barplot
  callModule(module = export_plot , id = "export_barplot" , file_name = "barplot" ,plot = final_barplot)
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # line plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## update x variables
  observe({
    # update x
    updateSelectInput(
      session = session,
      inputId = "lineplot_x",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## validate line plot parameters 
  line_plot_params_validated <- eventReactive(input$generate_lineplot, {
    req(input$lineplot_x)
    
    selected_non_num_vars <- plot_data() %>%
      dplyr::select(c(input$lineplot_x)) %>%
      dplyr::summarise_all(is.numeric) %>%
      tidyr::gather() %>%
      dplyr::filter(value == F)
    
    ## check if selected variables are of type numeric
    if (nrow(selected_non_num_vars) >= 1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = ifelse(length(selected_non_num_vars$key) == 1,
                      base::paste0(selected_non_num_vars$key, " is non numeric varibales"),
                      base::paste0(base::paste0(selected_non_num_vars$key, collapse = ","), " are non numeric varibales")
        ),
        type = "error"
      )
      return(FALSE)
    } 
    
    ## atleast 2 variable req to generate line plot
    if(length(input$lineplot_x) ==  1) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = paste0("Select atleast two columns to draw line plot"),
        type = "error"
      )
      return(FALSE)
    }
    
    ## value given for the `lineplot_top_n_genes` must be of type numeric
    if(!is.numeric(input$lineplot_top_n_genes)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = paste0("`# top variable genes to show` value must be of type numeric"),
        type = "error"
      )
      return(FALSE)
    }
    
    ## value given for the `line_plot_nclust` must be of type numeric
    if(!is.numeric(input$line_plot_nclust)) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Error...",
        text = paste0("`# of clusters (k-means)` value must be of type numeric"),
        type = "error"
      )
      return(FALSE)
    }
    
    else {
      return(TRUE)
    }
    
  })
  
  ## line plot condition panel : show only if selected variable(s) validated and displayed on UI
  output$line_plot_status <- reactive({
    req(line_plt())
    return(TRUE)
  })
  outputOptions(output, "line_plot_status", suspendWhenHidden = FALSE)
  
  ## line plot user selected gene group data 
  line_plot_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                        id = "line_plot_gene_group_selection" , 
                                                        gene_group_info =  reactive(group_info$row_groups), 
                                                        generate_plot_action = reactive(input$generate_lineplot),
                                                        current_session_data_matrix = plot_data)
  
  
  ## preapre line plot data.
  line_plot_data <- eventReactive(eventExpr = input$generate_lineplot, {
    req(line_plot_params_validated())
    
    withProgress(message = 'Preparing line plot data', {
      # Update progress
      incProgress(0.5) ## display 50% task finished when it start
      
      # Long Running Task
      set.seed(1234)
      
      ## handle error if generated while performing clustering 
      clustered <-  tryCatch({
        
        clustered <- tibble_to_row_clusters(x = line_plot_user_selected_gene_group_data(),
                                            row_ident = 1 , 
                                            cols_to_use = input$lineplot_x,
                                            use_z_score_for_clustering = ifelse(input$lineplot_cluster_value_type == 'zscore' , 
                                                                                TRUE, FALSE) ,
                                            num_of_top_var_rows = ifelse(input$lineplot_genes_selection == 'top_variable_genes',input$lineplot_top_n_genes , -1),
                                            nclust = ifelse(input$line_plot_cluster_genes_by == "kmeans",
                                                            as.numeric(input$line_plot_nclust) ,  1 ) ## if user selects cluster by gene groups, value of nclust = 1
        )
        ## add count to cluster name 
        
        
      }, error = function(x){
        shinyWidgets::sendSweetAlert(session = session , 
                                     type = "error",
                                     title = "Error" , 
                                     text = tags$h5(tags$code(x)))
        return(NULL)
      })
      
      # Update progress
      incProgress(1)
    }) ## with progress ends 
    
    ## return NULL if oject clustered in NULL
    if(is.null(clustered)){
      return(NULL)
    }
    
    ## select which value to display on plot, zscore or raw value.
    if(input$lineplot_display_value_type == "zscore"){
      forPlot <- clustered$zscore %>% left_join(clustered$clusters) 
    } else{
      forPlot <- clustered$raw_value %>% left_join(clustered$clusters) 
    }
    
    ## if cluster by gene_groups assign user defined groups   
    if(input$line_plot_cluster_genes_by == "gene_groups"){
      
      ## tibble of two cols, where 1st column is gene names and 2nd is user supplied gene_groups 
      user_gene_groups <- line_plot_user_selected_gene_group_data() %>% 
        dplyr::select(1, dplyr::last_col())
      
      ## get column names to use them inside left_join 
      left_col <- colnames(forPlot)[1]
      right_col <- colnames(user_gene_groups)[1]
      
      ## get the name of target column which is going to join as a result of left join. 
      ## the name of resulted joined column will be used later to remove column from the df. 
      column_to_be_joined <- colnames(user_gene_groups)[2]
      
      forPlot <- forPlot %>% 
        left_join(user_gene_groups , setNames(right_col, left_col)) %>% 
        dplyr::mutate(clust = !!as.symbol(column_to_be_joined)) %>% 
        dplyr::select(-!!as.symbol(column_to_be_joined))
      
    }
    
    ##  Add gene count in cluster name
    forPlot <- forPlot %>% 
      dplyr::group_by(clust) %>% 
      dplyr::add_count() %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(clust = paste(clust, ":(N=" ,n , ")" , sep = "")) %>% 
      dplyr::select(-n)  
    
    ## plot all lines vs avg line based on tick of activate_avg_line_plot
    if (input$activate_avg_line_plot == "average") {
      forPlot <- forPlot %>%
        dplyr::group_by(clust) %>%
        dplyr::summarise_if(is.numeric, input$avg_line_type)
    }
    
    forPlot <- forPlot %>% 
      tidyr::gather(key = "variable", value = "value" , input$lineplot_x) %>%
      dplyr::mutate(variable =  forcats::fct_inorder(variable))  %>%  ## preserve original order of samples
      dplyr::group_by(variable) 
    
    return(forPlot)
  })
  
  ## plot
  line_plt <- eventReactive(eventExpr = input$generate_lineplot, {
    req(line_plot_data())
    
    ### plot
    gp <- line_plot_data() %>% 
      dplyr::mutate(clust = forcats::fct_relevel(clust, as.numeric(clust) %>% 
                                                   unique %>% 
                                                   sort %>% as.character)) %>% ## numeric cluster to ordered factor 
      dplyr::mutate(row_num = 1:n()) %>%
      ggplot2::ggplot(aes(x = variable, 
                          y = value, group = row_num , alpha = 1))
    
    return(gp)
  })
  
  ## final line plot 
  final_line_plot  <- reactive({
    req(line_plt())
    validate(need(is.numeric(input$line_plot_ncols) ,
                  message = "Number of columns must be numeric"))
    
    ## line plot geoms 
    line_plt <- line_plt() + 
      geom_point(size  = input$lineplot_point_size) +
      #geom_line(aes(col = clust) , size = input$lineplot_line_size) + 
      ggalt::geom_xspline(aes(col = clust), 
                          spline_shape=input$line_plot_splin_shape, 
                          size=input$lineplot_line_size ,)+
      theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.4))
    
    ## line plot color identical 
    if(input$line_plot_color_by == 'identical'){
      
      ## get number of clusters 
      n_clust <- line_plt$data %>% 
        dplyr::pull(clust) %>%   ## pull cluster var column
        base::unique() %>% 
        as.character() %>% length()
      
      ## generate colors for each cluster
      colrs <- c(rep(input$line_plot_color_chooser,   n_clust))
      
      ## color and fill manual 
      line_plt <- line_plt + 
        scale_fill_manual(values = colrs) +
        scale_color_manual(values = colrs) 
    }
    
    ## line plot manual alpha 
    line_plt <-  line_plt + 
      scale_alpha(range = input$line_plot_line_transparancy) + ## override alpha in the plot 
      guides(alpha = FALSE , 
             color = guide_legend(override.aes = list(alpha = input$line_plot_line_transparancy))) ## override alpha in the legend
    
    ## lie plot facet 
    if(input$line_plot_separate_by == 'gene_groups'){
      line_plt <- line_plt + facet_wrap(~clust, ncol = input$line_plot_ncols , 
                                        scales = input$line_plot_facet_scale_free)  
    }
    
    ## decorate line plot 
    line_plot <- callModule(module = plot_title_and_axis_label_server , 
                            id = "decorate_line_plot" , 
                            my_ggplot = line_plt , 
                            axis_x_title = "Samples",
                            axis_y_title = "Value",
                            color_legend_title = "Cluster",
                            x_tick_angle = 90
    )
    
    # ## fill color identical 
    # line_plot <- callModule(module = ggplot_fill_colour , "line_plot_color_identical" , 
    #                         gp = line_plot ,  times = input$line_plot_nclust)
    
    return(line_plot)
  })
  
  
  ## render line plot
  output$line_plot <- renderPlot({
    req(final_line_plot())
    return(print(final_line_plot()))
    #return(
      # withProgress(message = "Display line plot in progress", {
      #   incProgress(0.5)
        #print(final_line_plot())  
        #incProgress(1)
      #})
    #)
  }, height = function() {
    return(session$clientData$output_line_plot_width)
  }, width = function() {
    return(session$clientData$output_line_plot_width)
  }, res = 96)
  
  ## lineplot data to display
  line_plot_data_disply <- eventReactive(input$generate_lineplot , {
    req(line_plot_data())
    display_data <- line_plot_data() %>% 
      tidyr::spread(key = "variable" , value = "value")%>% 
      dplyr::select(1,input$lineplot_x,dplyr::everything())
  })
  
  
  
  ## genome annotation for line plot  
  ## global genome annotations object cannot be used in following scenario
  ## For example : user has generated one lineplot  Now accidently or purposely user selected / uploaded data 
  ## for different species than the previous lineplot generated. In this scenario, global reference genome will
  ## be updated , and  therefore,  annotations in the existing lineplot will also be affected. 
  
  ## To prepvent this seprate heatmap ref genome from global annot. lineplot ref annot will be updated only when 
  ## plot lineplot submit button hit 
  
  lineplot_reference_annot <- eventReactive(input$generate_lineplot , {
    req(genome_for_annotations())
    return(genome_for_annotations())
  })
  
  
  
  ## line plot output table
  output$line_plot_clustred_data <- DT::renderDataTable({
    
    ## map gene features if average line not asked 
    if(isolate(input$activate_avg_line_plot  != "average")){
      
      user_selected_species_annot <- ah_data_summary2 %>% 
        dplyr::filter(genome == lineplot_reference_annot())%>%
        dplyr::select(gr_cols) %>% 
        tidyr::unnest()  
      
      
      join_col_x <- base::colnames(line_plot_data_disply())[1] ## first column containing geneNames
      join_col_y <- base::colnames(user_selected_species_annot)[7] ## "id" column
      
      line_plot_data_to_show <- line_plot_data_disply()%>% 
        left_join(user_selected_species_annot, by = setNames(join_col_y, join_col_x)) %>%
        dplyr::select(1,c("seqnames", "start", "end", "strand", "description"), colnames(line_plot_data_disply())) %>%
        dplyr::mutate_if(is.numeric, round, 4)%>%
        dplyr::mutate_all(funs(replace_na(as.character(.),"--NA--")))
    }else{
      
      line_plot_data_to_show <- line_plot_data_disply()
    }
    
    return(line_plot_data_to_show %>% dplyr::arrange(clust) %>% 
             dplyr::mutate_if(is.numeric , round  , 3))
  },
  selection  = "none",
  server = F,
  extensions = "Buttons",
  options = list(
    scrollX = TRUE,
    dom = "Blfrtip",
    searchHighlight = TRUE,
    buttons =
      list("copy", list(
        extend =
          "collection", buttons =
          c("csv", "excel", "pdf"), text = "Download"
      )), # end of buttons customization
    
    # customize the length menu
    lengthMenu = list(
      c(10, 50, 100, 500, -1) # declare values
      , c(10, 50, 100, 500, "All") # declare titles
    ),
    pageLength = 10
  ) ## end of options
  )
  
  ## export line plot 
  callModule(module = export_plot ,id = "export_line_plot" ,file_name = "line_plot" , plot = final_line_plot)
  
  
  
  ## line plot functional analysis 
  callModule(module = functional_analysis_server , id = "lineplot_functional_analysis_ui" , 
             ui_id = "lineplot_functional_analysis_ui",
             session = session,
             gene_set = reactive(split(x = line_plot_data_disply()[[1]],  ## genename
                                       f = line_plot_data_disply()$clust))  ,
             genome = reactive({lineplot_reference_annot()}))
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # PCA  plot server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## check whether x and y are numeric type
  pca_plot_vars_validated <- eventReactive(input$generate_pca_plot, {
    req(input$pca_plot_vars)
    withProgress(message = "validate data" , {
      incProgress(0.3)
      selected_vars <- plot_data() %>% dplyr::select(input$pca_plot_vars)
      col_class <- plot_data() %>% dplyr::select(input$pca_plot_vars) %>% 
        dplyr::summarise_all(is.numeric) %>% 
        tidyr::gather()
      
      incProgress(0.5)
      if (!all(col_class$value)) {
        non_numeric_cols <- col_class %>% dplyr::filter(!value)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = paste0(paste0(non_numeric_cols$key, collapse = " "), " must be of type numeric", collapse = ""),
          type = "error"
        )
        return(FALSE)
      } 
      incProgress(0.7)
      if(length(input$pca_plot_vars) ==  1) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = paste0("Select atleast two columns to perfrom PCA."),
          type = "error"
        )
        return(FALSE)
      }
      else {
        return(TRUE)
      }
      incProgress(1)
    })
    
  })
  
  ## update x variables
  observe({
    req(plot_data())
    # update x
    updateSelectInput(
      session = session,
      inputId = "pca_plot_vars",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
  })
  
  ## pca plot user selected gene group data 
  pca_plot_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                       id = "pca_plot_gene_group_selection" , 
                                                       gene_group_info =  reactive(group_info$row_groups), 
                                                       generate_plot_action = reactive(input$generate_pca_plot),
                                                       current_session_data_matrix = plot_data)
  
  ## prepare PCA input matrix , where columns are gene names and rows are sample names 
  pca_input_matrix  <- eventReactive(eventExpr = input$generate_pca_plot,{
    req(pca_plot_vars_validated() , pca_plot_user_selected_gene_group_data())
    
    withProgress(message = "Prepare PCA data",{
      
      incProgress(0.3)
      plot_data_t <- pca_plot_user_selected_gene_group_data() %>% 
        dplyr::select(1, input$pca_plot_vars) %>%
        dplyr::select_if(function(.) {
          !all(. == 0)
        }) %>% ## remove if all values are 0 in the sample
        tidyr::gather(key = sname, value, -1) %>% 
        tidyr::spread(1, value) ## transpose data --> genes will be column and samples will be row 
      
      ## conver dt to matrix for prcomp input 
      pca_input_data <- plot_data_t %>% 
        as.data.frame() %>%
        column_to_rownames("sname")
    })
    return(pca_input_data)
    
  })
  
  
  ## prform PCA 
  pca_plot_pr_comp <- eventReactive(eventExpr = input$generate_pca_plot,{
    req(pca_input_matrix())
    
    withProgress(message = "Perform PCA" , {
      
      # perform pca
      pr <- tryCatch({
        prcomp(pca_input_matrix()) ## remove last column having sample groups 
      } , error = function(x){
        sendSweetAlert(session = session ,title = "Error while performing PCA" , 
                       text = tags$h4("Error message from function :" , 
                                      tags$code("prcomp()"),br(),br(), tags$code(x)) ,
                       type = "error")
        return(NULL)
      })
    })
    return(pr)
  })
  
  ### perform pca sample k-means 
  pca_data_sample_kmeans <- reactive({
    req(pca_input_matrix())
    
    validate(
      need(is.numeric(input$pca_sample_sample_kmeans_n) && 
             (input$pca_sample_sample_kmeans_n > 0 ) , message = "Number of cluster must be numeric")
    )
    
    ## perform kmenas 
    km_data <- tryCatch({
      set.seed(12345)
      km <- kmeans(pca_input_matrix() , centers = input$pca_sample_sample_kmeans_n )
      tt <- tibble::tibble(sname = rownames(pca_input_matrix()) , kmeans_clust = km$cluster )  
    }, error = function(x){
      sendSweetAlert(session = session ,title = "PCA sample K-means error!!!" , 
                     text = tags$h4("Putting all samples in one group.",br(),
                                    "Error message from function :" , 
                                    tags$code("kmeans()"),br(),br(), tags$code(x)) ,
                     type = "error")
      tt <- tibble::tibble(sname = rownames(pca_input_matrix()) , kmeans_clust = "All samples")  
      return(tt)
    })
    return(km_data)
  }) 
  
  ## Attach % variance with each PC 
  my_pr_comp_names <- reactive({
    req(pca_plot_pr_comp())
    ## get proportion of variance for each PC 
    pc_prop_of_var <- summary(pca_plot_pr_comp())$importance %>%
      #summary(pr)$importance %>%
      as.data.frame() %>% 
      rownames_to_column(var = "feature_type") %>% 
      tidyr::gather(pc , value , -feature_type) %>% 
      dplyr::filter(feature_type == "Proportion of Variance") %>% 
      dplyr::select(-feature_type) %>%
      dplyr::mutate(value = round(value * 100 ,1)) %>% ## convert to percentage
      dplyr::mutate(with_var = paste(pc ," (" , value ,"%",")" , sep = "")) %>% 
      dplyr::select(-value) 
    
    ## return named vector 
    pc_prop_of_var %>% dplyr::pull(with_var) %>% rlang::set_names( pc_prop_of_var$pc)
    
  })
  
  
  ## Get PCs from prcomp obtained object. pr_comp_derieved_tibble is a tibble where columns are PCs and rows are samples. 
  pr_comp_derieved_tibble <- reactive({
    req(pca_plot_pr_comp())
    
    ## get pr_comp tibble, and map sample and group info
    pr_comp_tbl <- pca_plot_pr_comp()$x %>% 
      as.data.frame() %>% 
      dplyr::rename_all(~(my_pr_comp_names())) %>% 
      tibble::rownames_to_column("sname") %>%
      as_tibble() ## rename column names. new column names are with 'prop of var' attached. 
    ## create column sname (sample names ) from row names
    
    ## prevent rendering from global sample group update
    isolate({
      pr_comp_tbl_grps <- pr_comp_tbl %>% 
        left_join(group_info$column_groups , by = c (sname = "group_members")) %>% ## add column groups denoting  sample groups 
        tidyr::replace_na(list(groups = "No groups assigned")) ## in the column groups replace NA to "No groups assigned"
    })  
    ## add sample kmeans data
    pr_comp_tbl_grps  <-  pr_comp_tbl_grps %>%
      left_join(pca_data_sample_kmeans() , by = "sname") %>%
      tidyr::replace_na(list(kmeans_clust = "No groups assigned")) ## in the column groups
    
    return(pr_comp_tbl_grps)
  })
  
  
  ## PCA plot update  show / hide sample groups. It allows user to exclude sample groups directly from PCA plot  
  observe({
    req(pr_comp_derieved_tibble())
    if(input$pca_plot_colour == "groups"){
      shinyWidgets::updateMultiInput(inputId = "pca_plot_hide_sample_groups" , session = session,
                                     choices = pr_comp_derieved_tibble() %>% pull("groups") %>% unique()
      )
    }
    if(input$pca_plot_colour == "kmeans"){
      shinyWidgets::updateMultiInput(inputId = "pca_plot_hide_sample_groups" , session = session,
                                     choices = pr_comp_derieved_tibble() %>% pull("kmeans_clust") %>% unique()
      )
    }
  })
  
  
  ### Remove user excluded groups before final PCA plot 
  pca_data_after_groups_excluded_by_user <- reactive({
    #req(input$pca_plot_hide_sample_groups)
    user_excluded_groups <- input$pca_plot_hide_sample_groups
    plot_data <-  pr_comp_derieved_tibble() %>% 
      dplyr::filter(! xor((groups  %in% user_excluded_groups),(kmeans_clust %in% user_excluded_groups)))
    
    ## make sure pca_data_after_groups_excluded_by_user()  > 1 row. 
    ### This is necessary to check when user exclude all the groups
    validate(
      
      need(plot_data %>% nrow() > 1 , "Atleast one sample group must be selected" )
      
    )
    return(plot_data)
  })
  
  ## update PC choices 
  observe({
    req(my_pr_comp_names())
    available_pc <- my_pr_comp_names()
    default_pc <- available_pc[1:2] ## default PCs 
    
    updateSelectizeInput(session = session , 
                         inputId = "select_pcs_to_plot" , 
                         choices =  available_pc %>% unname(), 
                         selected = default_pc ,server = TRUE)  
  })
  
  ## get names of PC columns
  pcs_to_plot <- reactive({
    
    validate(
      need(input$select_pcs_to_plot %>% length() ==  2 , "Select 2 Pcs.")
    )
    return(input$select_pcs_to_plot)
  })
  
  ## pca plot
  pca_gp <- reactive({
    req(all(pcs_to_plot() %in% my_pr_comp_names()))
    
    withProgress(message = "Preparing PCA plot",{
      incProgress(0.5)
      
      ## add group information 
      plot_data <- pca_data_after_groups_excluded_by_user() 
      
      #print("PCA plot data")
      #print(plot_data %>% dplyr::select(sname, groups))
      
      ## PCs to plot 
      pc_x = pcs_to_plot()[1]## PC1
      pc_y = pcs_to_plot()[2]  ## PC2
      
      ## plot , default PC1 and PC2 
      pca_gp <- ggplot(data = plot_data  , 
                       aes_string(x = as.symbol(pc_x), y = as.symbol(pc_y) )) + 
        geom_point(size = input$pca_sample_dot_size) #+ theme_bw() 
      
      ## whether sample names should be shown or not 
      if(input$pca_display_sample_names)  {
        pca_gp <-  pca_gp + 
          ggrepel::geom_text_repel(aes(label = sname), 
                          size = input$pca_plot_sample_names_size) 
      }
      
      ### color groups
      if(input$pca_plot_colour == "groups")  {
        pca_gp <-  pca_gp + aes(color = groups) + 
          scale_color_manual(values = get_gg_colors(pr_comp_derieved_tibble() %>%  
                                                      pull(groups) %>% 
                                                      as_factor() %>% levels()) )
      }
      
      ## color by sample kmeans 
      if(input$pca_plot_colour == "kmeans")  {
        pca_gp <-  pca_gp + aes(color = kmeans_clust) + 
          scale_color_manual(values = get_gg_colors(pr_comp_derieved_tibble() %>%  
                                                      pull(kmeans_clust) %>% 
                                                      as_factor() %>% levels()) )
      }
      
      
      ## color identical  , NOTE: using breaks = NULL explicitly will be helpful to getrid of legends
      if(input$pca_plot_colour == "identical")  {
        pca_gp <-  pca_gp + aes(col = "") + 
          scale_color_manual( breaks = NULL , values = input$pca_sample_name_color) 
      }
      incProgress(1)
    })
    
    return(pca_gp)
  })
  
  ## final pca plot 
  final_pca_plot <- reactive({
    req(pca_gp())
    withProgress(message = "Decorate PCA plot",{
      incProgress(0.5)
      
      pca_gp <- callModule(module = plot_title_and_axis_label_server , 
                           id = "decorate_pca_plot" , 
                           my_ggplot = pca_gp() , 
                           axis_x_title = pca_gp()$labels$x, 
                           axis_y_title = pca_gp()$labels$y , 
                           x_tick_angle = 0, 
                           color_legend_title = input$pca_plot_colour)
      
      pca_gp <- pca_gp +
        scale_x_continuous(expand = expand_scale(mult = 0.2)) + # expand x scale
        scale_y_continuous(expand = expand_scale(mult = 0.2)) # expand y scale
      incProgress(1)
      return(pca_gp)
    })
    
  })
  
  ## render pca 
  output$pca_plot <- renderPlot({
    req(final_pca_plot())
    final_pca_plot()
    #return(print(final_pca_plot()))
  },
  height = function() {
    req(final_pca_plot())
    return(session$clientData$output_pca_plot_width)
  }, ## dynamic height
  width = function() {
    req(final_pca_plot())
    return(session$clientData$output_pca_plot_width)
  }, res = 96
  ) ## dynamic width
  
  
  ## export  pca plot 
  callModule(module = export_plot , id = "export_pca_plot" , file_name = "pca_plot" , plot  = final_pca_plot)
  ## current pca plot SRA sample info, update only when generate pca plot submit button hit 
  current_pca_plot_sra_sample_info  <- eventReactive(eventExpr = input$generate_pca_plot,{
    return(user_selected_sra_id_sample_info())
  })
  
  ### pla plot brush data 
  pca_brushed_data <- eventReactive(input$pca_plot_brush , {
    
    pc_x = pcs_to_plot()[1] ## PC1
    pc_y = pcs_to_plot()[2] ## PC2
    
    ## subset brushed points data
    bp <- brushedPoints(df = pca_data_after_groups_excluded_by_user() , 
                        brush = input$pca_plot_brush , 
                        xvar = pc_x, 
                        yvar = pc_y)
    
    ## map required sample info  
    display_pca_data  <- current_pca_plot_sra_sample_info() %>%
      dplyr::filter(run_accession %in% bp$sname) %>% 
      dplyr::select(-c("reference_annot" , "taxon_id","scientific_name" , "instrument_model" , "updated_date"))
    
    return(display_pca_data)
  })
  
  ## render PCA brushed data
  output$pca_brushed_datatable <- DT::renderDataTable({
    req(pca_brushed_data())
    return(pca_brushed_data() %>%
             dplyr::mutate_all(funs(replace_na(as.character(.),"--NA--"))) %>% 
             dplyr::mutate_if(is.numeric , round  , 2))
    
  },selection  = "none",  
  server = F,
  extensions = c("Buttons"),
  
  options = list(
    deferRender = TRUE,
    scrollX = TRUE,
    dom = "Blfrtip",
    searchHighlight = TRUE,
    columnDefs = list(
      list(targets = c(6:15), visible = FALSE) # The column number must be identical to the columns in colvis extension 
    ),
    
    buttons =
      list("copy", 
           list(extend = "collection", 
                buttons = c("csv", "excel", "pdf"), text = "Download"
           ), 
           list(extend = "colvis", 
                columns = c(6:15)
           )), # end of buttons customization
    
    # customize the length menu
    lengthMenu = list(
      c(10, 50, 100, 500, -1) # declare values
      , c(10, 50, 100, 500, "All") # declare titles
    ),
    pageLength = 10
  )
  )
  
  ## pca plot brushed data table status , decide whether should be displayed or not 
  output$pac_plot_brushed_data_table_status <-  reactive({
    req(pca_brushed_data())
    return(TRUE)
  })
  outputOptions(output,name = "pac_plot_brushed_data_table_status" , suspendWhenHidden = FALSE)
  
  
  ## pca plot condition panel:show only if variable (x, y) validated and displayed on UI
  output$pca_plot_status <- reactive({
    req(pca_plot_pr_comp())
    return(TRUE)
  })
  outputOptions(output, "pca_plot_status", suspendWhenHidden = FALSE)
  
  
  
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # heatmap server----
  ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  ## check whether x and y are numeric type
  heatmap_parmas_validated <- eventReactive(input$generate_heatmap, {
    req(input$heatmap_vars)
    
    selected_vars <- plot_data() %>% dplyr::select(input$heatmap_vars)
    col_class <- plot_data() %>% dplyr::select(input$heatmap_vars) %>% 
      dplyr::summarise_all(is.numeric) %>% 
      tidyr::gather()
    withProgress(message  = "Validating inputs", {
      incProgress(1/10)
      if (!all(col_class$value)) {
        non_numeric_cols <- col_class %>% dplyr::filter(!value)
        
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = paste0(paste0(non_numeric_cols$key, collapse = " "), " must be of type numeric", collapse = ""),
          type = "error"
        )
        return(FALSE)
      } 
      incProgress(2/10)
      ## atleast two vars req  to generate hm
      if(length(input$heatmap_vars) ==  1) { 
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error...",
          text = paste0("Select atleast two columns to draw heatmap"),
          type = "error"
        )
        return(FALSE)
      }
      incProgress(3/10)
      # check if number of genes given are of type numeric
      
      if ((!is.numeric(input$heatmap_top_n_genes)) & (input$hm_genes_selection == "top_variable_genes")) {
        
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "'# top variable genes to show' must be numeric",
          type = "error"
        )
        
        return(NULL)
      }
      incProgress(4/10)
      ## check if user given color scale min is valid 
      if (!is.numeric(input$heatmap_scale_min)) {
        
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Heatmap color scale (minimum) must be numeric value",
          type = "error"
        )
        return(NULL)
      }
      incProgress(5/10)
      ## check if user given color scale max valid or not 
      if (!is.numeric(input$heatmap_scale_max)) {
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Heatmap color scale (maximum) must be numeric value",
          type = "error"
        )
        return(NULL)
      }
      incProgress(6/10)
      ## check if number of cluster given are of type numeric. 
      if (!is.numeric(input$heatmap_row_nclust) || !is.numeric(input$heatmap_coulm_nclust)) {
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "'# of clusters (column / row k-means)' must be of type numeric",
          type = "error"
        )
        return(NULL)
      }
      incProgress(7/10)
      
      ## check if number of cluster given are > 0 and <= total rows in the data  or 
      
      if (input$heatmap_row_nclust == 0 || 
          input$heatmap_coulm_nclust == 0  || 
          input$heatmap_row_nclust >= nrow(plot_data()) || 
          input$heatmap_row_nclust >= input$heatmap_top_n_genes ||
          input$heatmap_coulm_nclust >= ncol(plot_data()) || 
          input$heatmap_coulm_nclust >= length(input$heatmap_vars)  
      ) {
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "'# of clusters (column / row k-means)' must be > 0 and < total rows (row clusters) or total columns (column clusters)",
          type = "error"
        )
        return(NULL)
      }
      
      else {
        return(TRUE)
      }
      incProgress(10/10)
    })
    
    
  })
  
  ## heatmap conditional panel 
  output$heatmap_status <- reactive({
    req(heatmap_to_display())
    return(TRUE)
  })
  outputOptions(output, "heatmap_status", suspendWhenHidden = FALSE)
  
  
  ## update x variables
  observe({
    req(plot_data())
    ## update x
    updateSelectInput(
      session = session,
      inputId = "heatmap_vars",
      choices = base::colnames(plot_data())[-1],
      selected = base::colnames(plot_data())[2]
    )
    
  })
  
  
  ## heatmap  user selected gene group data 
  heatmap_user_selected_gene_group_data <- callModule(module  = gene_group_selection , 
                                                      id = "heatmap_select_gene_groups" , 
                                                      gene_group_info =  reactive(group_info$row_groups), 
                                                      generate_plot_action = reactive(input$generate_heatmap),
                                                      current_session_data_matrix = plot_data)
  
  
  ##  prepare heatmap data.
  heatmap_top_variable_genes_df <- eventReactive(eventExpr = input$generate_heatmap, {
    req(heatmap_parmas_validated(), heatmap_user_selected_gene_group_data())
    
    withProgress(message = "Preparing heatmap data " , {
      
      incProgress(0.1)
      
      # fix number of genes to plot, default all genes (if any gene group selected, all the genes from selected gene group will be plotted)
      num_of_genes_for_hm <- nrow(heatmap_user_selected_gene_group_data())
      
      if (input$hm_genes_selection == "top_variable_genes") {
        ## if user given number of genes argument < total genes, all genes will be used in the plot.
        if (nrow(heatmap_user_selected_gene_group_data()) < input$heatmap_top_n_genes) {
          num_of_genes_for_hm <- nrow(heatmap_user_selected_gene_group_data())
        } else {
          num_of_genes_for_hm <- input$heatmap_top_n_genes
        }
      }
      
      # max genes cannot be higher than 20k
      max_hm_genes <- 20000
      if (num_of_genes_for_hm > max_hm_genes) {
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = paste("Number of genes cannot be higher than ", max_hm_genes, ".", sep = ""),
          type = "error"
        )
        return(NULL)
      }
      
      incProgress(0.3 , message = "row wise clustering")
      ## get clusters and zscore data 
      clustered <-  tryCatch({
        tibble_to_row_clusters(x = heatmap_user_selected_gene_group_data(),
                               row_ident = 1 , 
                               cols_to_use = input$heatmap_vars,
                               use_z_score_for_clustering = ifelse(input$heatmap_cluster_value_type == 'zscore' , TRUE, FALSE),
                               #use_z_score_for_clustering = input$hm_use_zscore  ,
                               num_of_top_var_rows = ifelse(input$hm_genes_selection == 'top_variable_genes',
                                                            num_of_genes_for_hm , -1),
                               nclust = as.numeric(input$heatmap_row_nclust)
        )  
      } , error = function(x){
        return(x)
      })
      ## if error generated while clustering
      if(inherits(clustered , "error")){
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = paste0("Row k-means error : ", clustered, collapse = " "),
          type = "error"
        )
        return(NULL)
      }
      
      ## select which value to display on plot, zscore or raw value.
      if(input$heatmap_display_value_type == "zscore"){
        heatmap_data <- clustered$zscore 
        #print(heatmap_data)
      } else{
        heatmap_data <- clustered$raw_value
        #print(heatmap_data)
      }
      
      
      ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ## arrange heatmap input data in original order is important when user wants to have same order of genes in the heatmap which was uploaded. 
      ## Following things must be kept in mind when arranging heatmap data matrix in the original order 
      ## 1) output of the  tibble_to_row_clusters function throws list containing raw_data_matrix, zscore_matrix and row_clusters 
      ##    if order of the rows in heatmap data matrix (zscore or raw data) change, order in the row clusters must change. 
      ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      
      gene_name_col_names <- plot_data_original_row_order() %>% colnames() %>% .[1]
      row_order_col_name <- plot_data_original_row_order() %>% colnames() %>% .[2]
      
      heatmap_data <- heatmap_data %>% 
        left_join(plot_data_original_row_order() , by = setNames(gene_name_col_names , colnames(heatmap_data)[1]))  %>% 
        dplyr::arrange(.data[[row_order_col_name]]) %>% ## arrange by original gene order 
        dplyr::select(-.data[[row_order_col_name]]) ## remove column 
      
      
      ## as mentioned above change order in row clusters 
      row_splt <- clustered$clusters %>%
        left_join(plot_data_original_row_order() , by = setNames(gene_name_col_names , colnames(clustered$clusters)[1])) %>%
        dplyr::arrange(.data[[row_order_col_name]]) %>% 
        dplyr::select(-.data[[row_order_col_name]]) 
      
      
      ### which clusters to use ? kmeans or user defined 
      if(input$heatmap_row_clusters_choices == "kmeans") { 
        row_splt <- row_splt%>%
          pull(2)
        
      } else if(input$heatmap_row_clusters_choices ==  "gene_groups"){ 
        
        ## get user given row groups 
        row_splt =  heatmap_data %>% 
          as_tibble() %>% 
          dplyr::left_join(group_info$row_groups , 
                           by = setNames(colnames(group_info$row_groups)[2] , 
                                         colnames(heatmap_data)[1])) %>%
          pull(colnames(group_info$row_groups)[1]) 
        
        ## set levels in the orginal order of user supplied groups 
        row_splt <- row_splt %>% 
          as.factor() %>% 
          fct_relevel(., group_info$row_groups %>% 
                        pull(1) %>% unique())
      }
      
      
      ## number of genes in each cluster 
      gene_count <- table(row_splt)
      
      ## Fix row cluster names 
      row_splt <- row_splt %>% 
        tibble(x = . ) %>% 
        dplyr::add_count(x) %>% 
        dplyr::mutate(cluster_name = paste(input$hm_cluster_prefix , x ,":(N=", n,")" , sep = "")) %>% 
        pull(cluster_name) 
      
      ## convert tibble to data frame 
      heatmap_data <- heatmap_data %>%
        as.data.frame()
      
      ##generate matrix with rownames as gene name
      rownames(heatmap_data) <- heatmap_data[[1]]
      heatmap_data <- heatmap_data[-1]
      
      incProgress(0.7 , message = "column wise clustering")
      
      ## column clusters, which clusters to use ? kmeans or user defined 
      
      if(input$heatmap_column_clusters_choices == "kmeans"){ #
        column_splt <- tryCatch({
          
          set.seed(1234)
          column_km_out <- kmeans(t(heatmap_data) ,centers = input$heatmap_coulm_nclust)
          column_km_out$cluster
          
        } , error = function(x){
          return(x)
        })  
        
      } else if(input$heatmap_column_clusters_choices == "sample_groups"){ # use sample groups as column cluster 
        
        ## heatmap column names to user  supplied grp names tibble 
        column_splt_ss =  heatmap_data %>% 
          colnames() %>% 
          tibble(sname = .) %>% 
          left_join(group_info$column_groups , by = c (sname = "group_members")) #%>% 
        
        ## arrange by user supplied column order 
        column_splt_ss <- column_splt_ss %>% 
          dplyr::mutate(sname =  forcats::fct_relevel(as.character(sname) , group_info$column_groups %>% pull(2) %>% unique()  ))
        
        ## get named vector of heatmap column names, where vector elems are group names and names are column names. 
        column_splt <- column_splt_ss %>% 
          pull(colnames(group_info$column_groups)[1]) %>% ## pull user supplied group columns 
          rlang::set_names( . ,column_splt_ss$sname)
        
        ## rearrange heatmap data as split order changed 
        heatmap_data <- heatmap_data %>% dplyr:: select(names(column_splt))
        
      }
      ##  Fix column cluster name 
      column_splt <- column_splt %>% 
        tibble(x = .) %>% 
        dplyr::add_count(x) %>% 
        dplyr::mutate(cluster_name = paste(input$hm_column_cluster_prefix , x ,"\n(N=", n,")" , sep = "")) %>% 
        pull(cluster_name) %>% 
        set_names(names(column_splt)) ## set orig names if it has 
      
      ## handle column split error 
      if(inherits(column_splt , "error")){
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = paste0("Column k-means error : ", column_splt, collapse = " "),
          type = "error"
        )
        return(NULL)
      }
      
      incProgress(0.9 , message = "final data")
      ## set heatmap data to a list 
      rr= NULL
      rr$mat <- heatmap_data
      rr$clust <- row_splt
      rr$column_clusters <- column_splt
      rr$row_sd <- rr$mat %>% 
        rownames %>% 
        tibble(geneName = . ) %>% 
        left_join(clustered$std_dev , by =c("geneName" = colnames(clustered$std_dev)[1])) %>% 
        as.data.frame() %>% 
        column_to_rownames("geneName") %>% as.matrix()
      
      incProgress(0.1)
      return(rr)
      
    })
  })
  
  ## update heatmap legend range 
  
  observe({
    #req(heatmap_top_variable_genes_df())
    
    if(input$heatmap_display_value_type == "zscore"){
      updateNumericInput(session = session , 
                         inputId = "heatmap_scale_min",
                         value = -1.5)
      updateNumericInput(session = session , 
                         inputId = "heatmap_scale_max",
                         value = 1.5)  
    } 
    if(input$heatmap_display_value_type != "zscore"){ 
      updateNumericInput(session = session , 
                         inputId = "heatmap_scale_min",
                         value = 0)
      updateNumericInput(session = session , 
                         inputId = "heatmap_scale_max",
                         value = 10)  
    }
    
  })
  
  
  ##  generate heatmap
  heatmap <- eventReactive(input$generate_heatmap, {
    req(heatmap_top_variable_genes_df())
    
    withProgress(message = "Preparing heatmap",{
      incProgress(0.5)
      
      # generate heatmap
      hm_row_threshold <- 2000 ##if number of genes higher than this, gene names will be disabled. 
      
      # ## hm colors 
      hm_colors <- tryCatch({
        circlize::colorRamp2(
          breaks = seq(from = input$heatmap_scale_min,
                       to = input$heatmap_scale_max , length.out = 3),
          colors = c(input$heatmap_col_low, input$heatmap_col_medium, input$heatmap_col_high)
        )
      }, error = function(e){
        
        ## send warning if color function give error
        sendSweetAlert(session = session , title = "Warning" , 
                       type = "warning" ,
                       text = tags$h4("Error while generating user defined colors", 
                                      br(),
                                      "Error from" , tags$code("circlize::colorRamp2()"),
                                      br(), 
                                      tags$code(e),br(),
                                      "Default scale (-2 to 2) will be used."
                       ))
        ## default color : get minimum and maximum from 
        circlize::colorRamp2(
          breaks = seq(from = -2,
                       to = 2 , length.out = 3),
          colors = c(input$heatmap_col_low, input$heatmap_col_medium, input$heatmap_col_high)
        )
        
      })
      
      
      
      ## prepare  column annotations
      column_ha = NULL ## default column annotations
      
      ## boxplot and column name annotations 
      if(!is.null(input$heatmap_anno_type)){ ## make sure atleast one annot selected
        
        ## convert NA to "No groups assigned"
        no_grp_label <- "NA"
        
        ## get groups for hm cols 
        hm_selected_column_groups <- heatmap_top_variable_genes_df()$mat %>% 
          colnames() %>% 
          tibble(hm_cols = .  ) %>% 
          left_join(group_info$column_groups , by = c(hm_cols =  "group_members")) %>%   
          pull(groups) %>% 
          replace_na(no_grp_label)
        
        
        ### column grp annot
        grp_colors <- get_gg_colors(x = hm_selected_column_groups %>% as_factor() %>% levels())
        
        ## column box annot 
        column_box_anno <-  ComplexHeatmap::anno_boxplot(heatmap_top_variable_genes_df()$mat ,
                                         axis = T ,
                                         border = T ,
                                         axis_param = list(gp = grid::gpar(fontsize = 8),
                                                           side = "left",
                                                           facing = "outside"
                                         ),
                                         height = unit(input$heatmap_top_annot_height,"cm")
        )
        
        ## display both grp and box annot 
        if(all(c("show_heatmap_column_groups" , "show_heatmap_column_boxplot") %in% input$heatmap_anno_type)){ ## all annotations
          
          column_ha = ComplexHeatmap::HeatmapAnnotation(Groups =  hm_selected_column_groups ,
                                        Box = column_box_anno, 
                                        col = list(Groups = grp_colors)
                                        
          )
          
          ## only grp annot   
        }else if(input$heatmap_anno_type == "show_heatmap_column_groups") {## sample group annotations
          
          column_ha = ComplexHeatmap::HeatmapAnnotation(Groups =  hm_selected_column_groups , col = list(Groups = grp_colors))
          
          ## only box annot 
        }else if(input$heatmap_anno_type == "show_heatmap_column_boxplot"){ ## box plot annotations
          column_ha = ComplexHeatmap::HeatmapAnnotation(box = column_box_anno)
        }
      }
      
      ## final hm
      hm <- ComplexHeatmap::Heatmap(heatmap_top_variable_genes_df()$mat, 
                                    #row_title = "cluster_%s",
                                    cluster_column_slices = F,#input$hm_cluster_column_slice %>% as.logical() , ## it make sure that column clusters arrange in order
                                    column_split = heatmap_top_variable_genes_df()$column_clusters %>% 
                                      factor(., levels = gtools::mixedsort(unique(.))),
                                    show_column_names = input$show_hm_colum_names %>% as.logical(),
                                    show_column_dend = input$show_hm_column_dend %>% as.logical(),
                                    cluster_columns = input$hm_cluster_columns %>% as.logical(),
                                    
                                    column_names_gp = grid::gpar(fontsize = input$hm_column_names_font_size),
                                    top_annotation = column_ha,
                                    column_title_rot = 90,
                                    column_title_gp = grid::gpar(fontsize = 10),
                                    
                                    # if SD heatmap AND sort by SD both on together, turn off row clustering. 
                                    cluster_rows = ifelse(input$sort_hm_by_std_dev %>% as.logical() && 
                                                            input$show_std_dev_hm %>% as.logical() , FALSE , as.logical(input$hm_cluster_rows)),
                                    # if number of genes higher than hm_row_threshold disable genenames
                                    show_row_names = ifelse(nrow(heatmap_top_variable_genes_df()$mat) <= hm_row_threshold, as.logical(input$show_hm_row_names), FALSE),
                                    show_row_dend = input$show_hm_row_dend %>% as.logical() ,
                                    #row_split = paste0("Clust_",heatmap_top_variable_genes_df()$clust), 
                                    row_split = heatmap_top_variable_genes_df()$clust %>% factor(., levels = gtools::mixedsort(unique(.))) ,
                                    row_names_gp = grid::gpar(fontsize = input$hm_row_names_font_size),
                                    row_order = if(all(input$sort_hm_by_std_dev %>% as.logical() , input$show_std_dev_hm %>% as.logical())) order(heatmap_top_variable_genes_df()$row_sd[,1]) else NULL,
                                    cluster_row_slices = F, #input$hm_cluster_row_slice,
                                    row_title_rot = 0,
                                    row_title_gp = grid::gpar(fontsize = 10),
                                    
                                    name = ifelse(input$heatmap_display_value_type == "zscore" , "zscore", input$heatmap_legend_name),
                                    heatmap_legend_param = list(legend_direction = input$heatmap_legened_direction),
                                    col = hm_colors,
                                    border = input$hm_border %>% as.logical()
                                    #width = unit(5 , "in"),
                                    #height = unit(5 , "in")
                                    
      )
      incProgress(1)
    })
    
    return(hm)
  })
  
  
  ## standard deviation heatmap
  sd_heatmap <- eventReactive(input$generate_heatmap, {
    req(heatmap_top_variable_genes_df())
    hm_row_threshold = 2000
    sd_hm <- ComplexHeatmap::Heatmap(heatmap_top_variable_genes_df()$row_sd, 
                                     row_split = heatmap_top_variable_genes_df()$clust, 
                                     show_row_names = ifelse(nrow(heatmap_top_variable_genes_df()$mat) <= hm_row_threshold, as.logical(input$show_hm_row_names), FALSE),
                                     row_names_gp = grid::gpar(fontsize = input$hm_row_names_font_size),
                                     heatmap_legend_param = list(legend_direction = input$heatmap_legened_direction),
                                     name = "std_dev")
    
    return(sd_hm)
  })
  
  ## final heatmap : data heatmap or combined with SD heatmap
  heatmap_to_display <- eventReactive(input$generate_heatmap , {
    req(heatmap())
    if(input$show_std_dev_hm %>% as.logical()) {
      show_hm <- heatmap()  + sd_heatmap() ## temparory SD heatmap de activted 
    }else{
      show_hm <- heatmap() 
    }
    return(show_hm)
  })
  
  
  ## render heatmap
  output$heatmap <- renderPlot({
    #req(heatmap_to_display())
    # heatmap display in progress
    # withProgress(message = "Display heatmap in progress" , {
    #   incProgress(0.5)
    #   set.seed(123)
    #   #hm <- shiny::isolate(ComplexHeatmap::draw(heatmap_to_display() , heatmap_legend_side = input$heatmap_legend_pos))
    #   incProgress(1)
    # })
    return(heatmap_to_display())
  },
  height = function() {
    req(heatmap_top_variable_genes_df())
    minimum_height <- 400
    ht <- minimum_height + (input$hm_row_height * nrow(heatmap_top_variable_genes_df()$mat))
    return(ht)
  }, ## dynamic height
  width = function() {
    req(heatmap_top_variable_genes_df())
    minimum_width <- 500
    wd <- minimum_width + (input$hm_col_width * ncol(heatmap_top_variable_genes_df()$mat))
    return(wd)
  }, res = 108
  ) ## dynamic width
  
  # Export heatmap
  callModule(module = export_base_graphics , 
             id = "export_heatmap" ,
             file_name = "heatmap" ,
             plot  =  as_mapper(~heatmap_to_display()),  ## pass as a function does not open device locally
             legend_pos = reactive(input$heatmap_legend_pos))
  
  
  
  ## prepare  heatmap  data for display
  active_heatmap_data <- reactive({
    
    req(heatmap_to_display())
    
    # clusts must be a list. However, if only 1 cluster (rowwise) given , `clusts` becomes vector.
    # To overcome this, convert clusts to list explicitly when number of row clusters  == 1.
    
    if(heatmap()@matrix_param$row_split %>% pull(row_split) %>% unique() %>% length() == 1) { ## this can be improved 
      clusts <- list("1" = ComplexHeatmap::row_order(heatmap()))
    } else {
      ## return list where names of each elem indicates the cluster name displyed in heatmap 
      clusts <- ComplexHeatmap::row_order(heatmap()) 
    }
    
    ## prepare the tibble of two column where first column is cluster names and second column is row number of original data matrix given to the ComplexHeatmap::Heatmap function 
    names(clusts) <- as.character(names(clusts))
    hm_clust <- data_frame(clust = names(clusts), row_num = clusts) %>% 
      tidyr::unnest()
    
    ##column order 
    hm_coloum_ord <- ComplexHeatmap::column_order(heatmap()) %>% unlist(use.names = F)
    
    ## final data to be shown below heatmap as heatmap data 
    hm_data <- heatmap_top_variable_genes_df()$mat %>% ## this is the original data supplied to the heatmap
      as.data.frame() %>%
      dplyr::select(hm_coloum_ord) %>% 
      rownames_to_column(var = "gene_name") %>%
      as_data_frame() %>% 
      dplyr::mutate(row_num = dplyr::row_number()) %>%
      right_join(hm_clust , by = "row_num") %>% ## two tibble will be join by row_num column 
      dplyr::select(-row_num) %>% 
      left_join(heatmap_top_variable_genes_df()$row_sd %>% 
                  as.data.frame() %>%  
                  tibble::rownames_to_column("gene_name") , by = "gene_name")## add std dev 
    
    return(hm_data %>% 
             dplyr::mutate_if(is.numeric, round, 4))
  })
  
  
  ## genome annotation for heatmap 
  ## global genome annotations object cannot be used in following scenario
  ## For example : user has generated one heatmap. Now accidently or purposely user selected / uploaded data 
  ## for different species than the previous heatmap generated. In this scenario, global reference genome will
  ## be updated , and  therefore,  annotations in the existing heatmap will also be affected. 
  
  ## To prepvent this seprate heatmap ref genome from global annot. Heatmap ref annot will be updated only when 
  ## plot heatmap submit button hit 
  
  heatmap_reference_annot <- eventReactive(input$generate_heatmap , {
    req(genome_for_annotations())
    return(genome_for_annotations())
  })
  
  
  ## render heatmap data 
  output$heatmap_data <- DT::renderDataTable({
    
    ## map species annotations while displaying heatmap data
    
    user_selected_species_annot <- ah_data_summary2 %>% 
      dplyr::filter(genome == heatmap_reference_annot()) %>% 
      dplyr::select(gr_cols) %>% 
      tidyr::unnest()  
    
    
    join_col_x <- base::colnames(active_heatmap_data())[1] ## first column containing geneNames
    join_col_y <- base::colnames(user_selected_species_annot)[7] ## "id" column
    
    heat_map_data_to_show <- active_heatmap_data() %>% 
      left_join(user_selected_species_annot, by = setNames(join_col_y, join_col_x)) %>%
      dplyr::select(1,c("seqnames", "start", "end", "strand", "description"), colnames(active_heatmap_data()))%>%
      dplyr::mutate_all(funs(replace_na(as.character(.),"--NA--")))
    
    return(heat_map_data_to_show)
    
  },rownames = T,
  selection  = "none",  
  server = T,
  extensions = "Buttons",
  options = list(
    scrollX = TRUE,
    dom = "Blfrtip",
    searchHighlight = TRUE,
    buttons =
      list("copy", list(
        extend =
          "collection", buttons =
          c("csv", "excel"), text = "Download"
      )), # end of buttons customization
    
    # customize the length menu
    lengthMenu = list(
      c(10, 50, 100, 500, -1) # declare values
      , c(10, 50, 100, 500, "All") # declare titles
    ),
    pageLength = 10
  )
  )
  
  ## prepare column cluster data
  heatmap_column_cluster <- eventReactive(input$generate_heatmap,{
    req(heatmap())
    
    hm_column_labels <- heatmap()@column_names_param$labels
    hm_column_ord <- ComplexHeatmap::column_order(heatmap())
    
    named_list_from_vector <- purrr::as_mapper(~ 
                                                 if(!is_list(.x)){
                                                   list(`1` = .x)
                                                 }else{
                                                   .x
                                                 } ) 
    
    ## given a list of a char vector it returns a char vector where each elem of list collpase in a single vector elem. 
    colum_list_to_chr  = as_mapper(~map_chr(., paste0 , collapse =","))
    
    hm_column_ord_lst <- named_list_from_vector(hm_column_ord)
    
    ## wide  format 
    wide <- tibble(cluster = names(hm_column_ord_lst) , column_order = hm_column_ord_lst) %>% 
      tidyr::unnest() %>% 
      dplyr::mutate(column_labels = hm_column_labels[column_order]) %>% 
      dplyr::select(-column_order) %>% 
      dplyr::group_by(cluster) %>% 
      dplyr::summarise(column_labels = list(column_labels)) %>% 
      dplyr::mutate(count = lengths(column_labels)) %>% 
      dplyr::mutate(column_labels = colum_list_to_chr(column_labels)) %>% 
      dplyr::select(cluster, count, column_labels) %>% 
      dplyr::slice(match(names(hm_column_ord_lst) , .$cluster )) ## arrange clusters in original heatmap order  
    
    ## given a char vector , reeturns list of vectors. Each elem of char will be splitted by 'split' and all splitted  elems will be strored in a vector
    colum_chr_to_list  = as_mapper(~map(., function(.) {strsplit(. , split = ",") %>% unlist() })) 
    
    ## Long format 
    long <- wide %>% 
      dplyr::mutate(column_labels =  colum_chr_to_list(column_labels)) %>% 
      tidyr::unnest(column_labels)
    
    list(wide = wide, long = long)
  })
  
  ## prepare row cluster data 
  heatmap_row_cluster <-eventReactive(input$generate_heatmap,{
    req(active_heatmap_data())
    
    ## get data from reactive elems 
    active_hm_data  <- active_heatmap_data()
    
    cols_of_interest <- c(colnames(active_hm_data)[1] , "clust") 
    
    wide  <- active_hm_data %>% 
      dplyr::select(cols_of_interest) %>% ## select first (gene name) and row cluster column : 'clust'
      dplyr::group_by(clust) %>% 
      dplyr::summarise(row_labels = list(!!as.symbol(cols_of_interest[1] ))) %>% 
      dplyr::mutate(count = lengths(row_labels)) %>% 
      dplyr::mutate(row_labels = map_chr(row_labels, ~paste0(.x , collapse = ","))) %>% 
      dplyr::select( clust, count , row_labels) %>% 
      dplyr::slice(match( active_heatmap_data()$clust %>% unique(), .$clust  )) ## arrange in orginal order 
    
    
    ## given a char vector , reeturns list of vectors. Each elem of char will be splitted by 'split' and all splitted  elems will be strored in a vector
    colum_chr_to_list  = as_mapper(~map(., function(.) {strsplit(. , split = ",") %>% unlist() })) 
    
    long <- wide %>% 
      dplyr::mutate(row_labels = colum_chr_to_list(row_labels)) %>% 
      tidyr::unnest(row_labels)
    
    list(wide = wide, long = long )
    
  })
  
  # render  heatmap  cluster data
  output$heatmap_display_cluster_data <- DT::renderDataTable({
    if(input$heatmap_cluster_type == 'show_hm_row_side_clusters'){
      return(heatmap_row_cluster()[[input$heatmap_cluster_data_format]])
    } else if (input$heatmap_cluster_type == 'show_hm_column_side_clusters'){
      return(heatmap_column_cluster()[[input$heatmap_cluster_data_format]] )
    }
    
  }, rownames = T,
  selection  = "none",
  server = T,
  extensions = "Buttons",
  options = list(
    scrollX = TRUE,
    dom = "Blfrtip",
    searchHighlight = TRUE,
    buttons =
      list("copy",
           list(extend = "collection",
                buttons =c("csv", "excel"),
                text = "Download"
           )
      ), # end of buttons customization
    # customize the length menu
    pageLength = 10
  ))
  
  
  ## heatmap functional analysis 
  callModule(module = functional_analysis_server , 
             id = "heatmap_functional_analysis_ui" , 
             ui_id = "heatmap_functional_analysis_ui",
             session = session, 
             gene_set = reactive(split(x = active_heatmap_data()$gene_name,  
                                       f = factor(active_heatmap_data()$clust , levels = active_heatmap_data()$clust %>% unique())))  ,
             genome = reactive({heatmap_reference_annot()}) )
  
  
  ## update heatmap column cluster in the wordcloud 
  observeEvent(input$generate_heatmap,{
    shinyWidgets::updatePickerInput(session = session , 
                                    inputId = "wordcloud_column_cluster", 
                                    choices = heatmap_column_cluster()$wide$cluster %>% gtools::mixedsort())
  })
  
  ## heatmap word cloud 
  hm_cluster_wise_sample_infor_ll <- eventReactive(input$generate_heatmap, {
    
    ## get samples / column labels for user selected cluster 
    cluster_name_column_lable_tbl <- heatmap_column_cluster()$long %>% 
      dplyr::select(cluster, column_labels ) 
    
    ## get abstract from selected labels 
    join_col_x <- cluster_name_column_lable_tbl %>% colnames() %>% .[2] ## column having hm column labels 
    join_col_y <- user_selected_sra_id_sample_info() %>% colnames()%>% .[23] ## column having SRA id 
    
    
    cluster_wise_sample_infor_tbl <- cluster_name_column_lable_tbl %>% 
      left_join(user_selected_sra_id_sample_info() , 
                by = setNames(join_col_y,join_col_x)) %>% 
      dplyr::group_by(cluster) %>% 
      tidyr::nest() 
    
    cluster_wise_sample_infor_ll <- cluster_wise_sample_infor_tbl %>% 
      dplyr::pull(2) 
    names(cluster_wise_sample_infor_ll) <- cluster_wise_sample_infor_tbl %>% 
      pull(1) 
    
    return(cluster_wise_sample_infor_ll)
    
  })
  
  observe({
    callModule(id = "hm_sample_infor" , 
               module = cluster_wise_sample_information_server , parent_id = "hm_sample_infor",
               cluster_wise_sample_information = reactive(hm_cluster_wise_sample_infor_ll()))  
  })
  
  ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ## Download page server code  ----
  ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ## download GO data 
  callModule(module = download_go_data_server , id = "download_go_data" , ah_data = ah_data_summary2)
  
  ## download gene expression matrix 
  
  callModule(module = download_gene_expression_matrix_server , id = "download_gene_expression_data")
  
}
