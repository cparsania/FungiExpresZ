# Module UI
  
#' @title   functional_analysis_ui and functional_analysis_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_functional_analysis
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
functional_analysis_ui <- function(id){
  
  ns <- NS(id)
  tagList(
    fluidRow(
      ## ontology type
      column(width = 4,
             shinyWidgets::pickerInput(inputId = ns("go_type"),
                                       label = "Ontology" ,
                                       choices = c("Biological Process"  = "Biological Process",
                                                   "Molecular Function" = "Molecular Function" ,
                                                   "Cellular Component" = "Cellular Component"),
                                       width = "100%")
      ),
      
      
      ## select gene cluster for dotplot 
      conditionalPanel(condition =  
                         paste0("input['",ns("view"),"'] == 'dotplot'"),
                       ## select gene set
                       column(width = 4,
                              shinyWidgets::pickerInput(inputId = ns("dotplot_input_geneset"),
                                                        label = "Gene cluster" , 
                                                        multiple = T,
                                                        options = list(
                                                          `actions-box` = TRUE),
                                                        choices = "",
                                                        width = "100%")
                       )),
      
      ## select gene cluster for other than dot plot 
      conditionalPanel(condition =  
                         paste0("input['",ns("view"),"'] == 'emapplot' || 
                                input['",ns("view"),"'] == 'heatplot' || 
                                input['",ns("view"),"'] == 'cnetplot'||
                                input['",ns("view"),"'] == 'upsetplot'||
                                input['",ns("view"),"'] == 'barplot' ||
                                input['",ns("view"),"'] == 'table'" ),
                       ## select gene set
                       column(width = 4,
                              shinyWidgets::pickerInput(inputId = ns("except_dotplot_input_geneset"),
                                                        label = "Gene cluster" , 
                                                        multiple = F,
                                                        choices = "",
                                                        width = "100%")
                       )),
      
      
      ## view
      column(width = 4,
             shinyWidgets::pickerInput(inputId = ns("view"),
                                       label = "View type" ,
                                       choices = c("Table" =  "table" ,
                                                   "Dotplot" = "dotplot", 
                                                   "Barplot" = "barplot", 
                                                   "Emapplot" ="emapplot",
                                                   "Cnetplot" = "cnetplot", 
                                                   "Upsetplot" = "upsetplot",
                                                   "Heatplot"  ="heatplot") , 
                                       selected = "emapplot" , width = "100%") 
      )
    ),
      
      
      
      ## evidance type
      # column(width = 2,
      #        shinyWidgets::pickerInput(inputId = ns("go_evidence_code"),label = "Evidence" ,
      #                                  choices = c("Computed" = "computed",
      #                                              "Curated" =  "curated") , 
      #                                  multiple = T
      #        )
      # ),
      ## pvalue
      # column(width = 2, 
      #        numericInput(inputId = ns("pval_cutoff"),label = "P-adjust" ,
      #                     min = 0, max = 1, step = 0.001 ,value = 1)
      # ),
      # ## qvalue
      # column(width = 2,
      #        numericInput(inputId = ns("qval_cutoff"),label = "Q-value" , 
      #                     min = 0, max = 1, step = 0.001  ,value = 1)
      # ),
      
    
    fluidRow(
      
      column(width = 4,
             shinyWidgets::pickerInput(inputId = ns("cross_go_bg_to_godb"),
                                       label = "Background Genes to GO mapping" , 
                                       choices = c("Parent + Offsprings" = "parent_and_offsprings" , 
                                                   "Parent" = "parent_only"
                                                   ),
                                       width = "100%"
                                       )
             
             ),
      
      ## minmum number of genes in background GO term
      column(width = 4, 
             shiny::numericInput(inputId = ns("min_gs_size"),
                                 label = "# of minimum genes in a GO term" ,
                                 min = 1,
                                 value = 10, 
                                 width = "100%")
             
      ),
      ## maximum number of genes in background GO term
      column(width = 4, 
             shiny::numericInput(inputId = ns("max_gs_size"),
                                 label = "# of maximum genes in a GO term" ,
                                 min = 1,
                                 value = 500, 
                                 width = "100%")
             
      )
      
    ),
    
    fluidRow(
      # p-adj method 
      ##c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
      column(width = 4, 
             shinyWidgets::pickerInput(inputId = ns("p_adj_method"),
                                       label = "P.adjust" ,
                                       choices = c("holm" = "holm", 
                                                   "hochberg" = "hochberg", 
                                                   "hommel" = "hommel", 
                                                   "bonferroni" ="bonferroni", 
                                                   "BH" = "BH", 
                                                   "BY"="BY",
                                                   "fdr"="fdr", 
                                                   "none"="none"),
                                       selected = "BH",
                                       width = "100%")
             
      )
    ),
    
    ## perform go enrichment action button 
    fluidRow(
      column(
        offset = 4,
        width = 4,
        shinyWidgets::actionBttn(inputId = ns("perform_go_analysis_trigger") , 
                                 label = "Submit" , 
                                 color = "success" ,
                                 size = "md",
                                 block = T,
                                 icon = icon("arrow-circle-right","fa-1.5x"),
                                 style = "gradient")
      )
    ),
    hr(),
    
    ####
    ## GO table panel ----
    ####
    conditionalPanel(condition =  paste0("input['",ns("view"),"'] == 'table' && input['",ns("perform_go_analysis_trigger"),"'] "),
                     DT::dataTableOutput(outputId = ns("functional_out_data") , 
                                     width = "auto", height = "auto") %>% 
                       shinycssloaders::withSpinner(color = "#18BC9C")
    ),
    
    ####
    ## dotplot panel ----
    ####
    conditionalPanel(condition =  paste0("input['",ns("view"),"'] == 'dotplot' && input['",ns("perform_go_analysis_trigger"),"'] "),
                     plotOutput(outputId = ns("go_enrichment_dotplot") , height = "auto") %>% 
                       shinycssloaders::withSpinner(color = "#18BC9C"),
                     hr(),
                     fluidRow(
                       column(
                         width = 12, 
                         column(width = 6,
                                plot_title_and_axis_label_ui(id = ns("dot_plot_title_and_legend"))),
                         
                         ## dot plot  advanced option 
                         column(width = 3,
                                
                                dropdownButton(
                                  inputId = ns("dot_plot_advance_settings"),
                                  icon = icon("gears"),
                                  label = "Advance settings",
                                  size = "sm",
                                  circle = F,
                                  status = "success",
                                  up = T,
                                  right = F,
                                  
                                  ## x axis 
                                  shiny::selectInput(inputId = ns("dot_plot_xaxis") , label = "X-axis" ,
                                                     choices = c("Count" ="Count" ,  "GeneRation" ="GeneRatio") ,
                                                     multiple = F , selected = "Count"),
                                  
                                  ## color by
                                  shiny::selectInput(inputId = ns("dot_plot_color_by") , multiple = F,
                                                     label = "Color by" ,
                                                     choices = c("P-value" = "pvalue" ,"P-adjust" = "p.adjust" , "Q-value" ="qvalue") , 
                                                     selected = "pvalue"),
                                  
                                  ## number of category
                                  shiny::numericInput(inputId = ns("dot_plot_number_of_category") , 
                                                      label = "Show terms" , 
                                                      min = 1, max = 50 , 
                                                      value = 5, step = 1 , width = "100%")
                                )
                         ),
                         
                         column(width = 3,
                                export_plot_ui(id = ns("export_dotplot"))
                         )    
                       )
                       
                     )
    ),
    
    
    ####
    ## barplot panel ----
    ####
    conditionalPanel(condition =  paste0("input['",ns("view"),"'] == 'barplot' && input['",ns("perform_go_analysis_trigger"),"'] "),
                     plotOutput(outputId = ns("go_enrichment_bar_plot") , height = "auto") %>% 
                       shinycssloaders::withSpinner(color = "#18BC9C"),
                     hr(),
                     fluidRow(
                       column(
                         width = 12, 
                         column(width = 6,
                                plot_title_and_axis_label_ui(id = ns("bar_plot_title_and_legend"))),
                         
                         ## bar plot  advanced option 
                         column(width = 3,
                                
                                dropdownButton(
                                  inputId = ns("bar_plot_advance_settings"),
                                  icon = icon("gears"),
                                  label = "Advance settings",
                                  size = "sm",
                                  circle = F,
                                  status = "success",
                                  up = T,
                                  right = F,
                                  
                                  ## x axis 
                                  shiny::selectInput(inputId = ns("bar_plot_xaxis") , label = "X-axis" ,
                                                     choices = c("Count" ="Count" ,  "GeneRation" ="GeneRatio") ,
                                                     multiple = F , selected = "Count"),
                                  
                                  ## color by
                                  shiny::selectInput(inputId = ns("bar_plot_color_by") , multiple = F,
                                                     label = "Color by" ,
                                                     choices = c("P-value" = "pvalue" ,"P-adjust" = "p.adjust" , "Q-value" ="qvalue") , 
                                                     selected = "pvalue"),
                                  
                                  ## number of category
                                  shiny::numericInput(inputId = ns("bar_plot_number_of_category") , 
                                                      label = "Show terms" , 
                                                      min = 1, max = 50 , 
                                                      value = 5, step = 1 , width = "100%")
                                )
                         ),
                         
                         column(width = 3,
                                export_plot_ui(id = ns("export_bar_plot"))
                         )    
                       )
                       
                     )
    ),
    
    
    
    
    
    ####
    ## emapplot panel ----
    ####
    conditionalPanel(condition =  paste0("input['",ns("view"),"'] == 'emapplot' && input['",ns("perform_go_analysis_trigger"),"'] "),
                     
                     plotOutput(outputId = ns("go_enrichment_emapplot") , height = "auto")%>% 
                       shinycssloaders::withSpinner(color = "#18BC9C"),
                     
                     hr(),
                     fluidRow(
                       column(
                         width = 12, 
                         column(width = 3), ## empty column
                         column(width = 3,
                                dropdownButton(
                                  inputId = ns("emap_plot_advance_settings"),
                                  icon = icon("gears"),
                                  label = "Advance settings",
                                  size = "sm",
                                  circle = F,
                                  status = "success",
                                  up = T,
                                  right = F,
                                  
                                  ## emap plot node size 
                                  sliderInput(
                                    inputId = ns("emap_node_label_size"), label = "Font size: Node label",
                                    min = 1, max = 30, value = 5, step = 0.01
                                  ),
                                  
                                  ## emapp plot color by 
                                  shiny::selectInput(inputId = ns("emap_plot_color_by") , multiple = F,
                                                     label = "Color by" ,
                                                     choices = c("P-value" = "pvalue" ,"P-adjust" = "p.adjust" , "Q-value" ="qvalue") , 
                                                     selected = "pvalue"),
                                  
                                  ## emap plot layout type
                                  shiny::selectInput(inputId = ns("emap_plot_layout_type") , multiple = F,
                                                     label = "Layout" ,
                                                     
                                              
                                                     
                                                     choices = c("Star"="star", 
                                                                 "Circle"= "circle", 
                                                                 "Gem" = "gem", 
                                                                 "DH" = "dh", 
                                                                 "Graphopt" = "graphopt", 
                                                                 "Grid" = "grid", 
                                                                 "mds" = "mds", 
                                                                 "Randomly" = "randomly", 
                                                                 "FR" = "fr", 
                                                                 "KK" = "kk", 
                                                                 "DRL" = "drl",
                                                                 "LGL" = "lgl") , 
                                                     
                                                     selected = "KK"),
                                  
                                  # emap plot number of category to display 
                                  shiny::numericInput(inputId = ns("emap_plot_number_of_category") , 
                                                      label = "Show terms" , min = 1, max = 50 , value = 20, step = 1 , width = "100%")
                                )
                         ),
                         
                         column(width = 3,
                                export_plot_ui(id = ns("export_emapplot"))
                         ),
                         column(width = 3) ## empty column
                       )
                     )
    ),
    
    ####
    ## cnetplot panel----
    ####
    conditionalPanel(condition =  paste0("input['",ns("view"),"'] == 'cnetplot' && input['",ns("perform_go_analysis_trigger"),"'] "),
                     plotOutput(outputId = ns("go_enrichment_cnetplot") , height = "auto")%>% 
                       shinycssloaders::withSpinner(color = "#18BC9C"),
                     hr(),
                     fluidRow(
                       column(
                         width = 12, 
                         column(width = 3), ## empty column
                         column(width = 6,
                                
                                dropdownButton(
                                  inputId = ns("cnet_plot_advance_settings"),
                                  icon = icon("gears"),
                                  label = "Advance settings",
                                  size = "sm",
                                  circle = F,
                                  status = "success",
                                  up = T,
                                  right = F,
                                  
                                  ## cnet plot node size 
                                  sliderInput(
                                    inputId = ns("cnet_node_label_size"), label = "Font size: Node label",
                                    min = 1, max = 30, value = 5, step = 0.01
                                  ),
                                  
                                  ## cnet plot layout type 
                                  shiny::selectInput(inputId = ns("cnet_plot_layout_type") , multiple = F, 
                                                     label = "Layout" , 
                                                     choices = c("Circular" = "circle" ,
                                                                 "KK" = "kk" , 
                                                                 "Linear" ="linear",
                                                                 "Matrix" = "matrix",
                                                                 "Tree" ="tree") , 
                                                     selected = "kk"),
                                  
                                  # cnet plot number of category to display 
                                  shiny::numericInput(inputId = ns("cnet_plot_number_of_category") , 
                                                      label = "Show terms" , min = 1, max = 50 , value = 5, step = 1 , width = "100%")
                                  
                                  
                                )
                                
                         ),
                         column(width = 3,
                                export_plot_ui(id = ns("export_cnetplot"))
                         ),
                         column(width = 3) ## empty column
                       )
                     )
    ),
    
    
    ####
    ## upsetplot  ----
    ####
    
    conditionalPanel(condition =  paste0("input['",ns("view"),"'] == 'upsetplot' && input['",ns("perform_go_analysis_trigger"),"'] "),
                     plotOutput(outputId = ns("go_enrichment_upsetpot") , height = "auto")%>% 
                       shinycssloaders::withSpinner(color = "#18BC9C"),
                     hr(),
                     fluidRow(
                       column(
                         width = 12, 
                         column(width = 3), ## empty column
                         column(width = 6,
                                
                                dropdownButton(
                                  inputId = ns("upset_plot_advance_settings"),
                                  icon = icon("gears"),
                                  label = "Advance settings",
                                  size = "sm",
                                  circle = F,
                                  status = "success",
                                  up = T,
                                  right = F,
                                  
                                  ## upset plot number of terms 
                                  numericInput(
                                    inputId = ns("upset_number_of_terms"), label = "Number of terms",
                                    min = 1, max = 30, 
                                    value = 5, step = 1
                                  )
                                  
                                  # latest version of enrichplot doesn't support below arguments
                                  
                                  # upset line size 
                                  # sliderInput(inputId = ns("upset_line_size") ,
                                  #             label = "Line size", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 1, 
                                  #             step = 0.01),
                                  # 
                                  # # intersection size title
                                  # sliderInput(inputId = ns("upset_intersection_size_title") ,
                                  #             label = "Intersection size title", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 2, 
                                  #             step = 0.01),
                                  # 
                                  # # intersection size tick labels
                                  # sliderInput(inputId = ns("upset_intersection_size_tick_labels") ,
                                  #             label = "Intersection size tick labels", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 2, 
                                  #             step = 0.01),
                                  # 
                                  # #set size title
                                  # sliderInput(inputId = ns("upset_set_size_title") ,
                                  #             label = "Set size", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 2, 
                                  #             step = 0.01),
                                  # 
                                  # # upset point size 
                                  # sliderInput(inputId = ns("upset_point_size") ,
                                  #             label = "point size", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 4, 
                                  #             step = 0.01),
                                  # 
                                  # #set size tick labels
                                  # sliderInput(inputId = ns("upset_set_size_tick_labels") ,
                                  #             label = "Set size tick labels", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 2, 
                                  #             step = 0.01),
                                  # 
                                  # 
                                  # #set names,
                                  # sliderInput(inputId = ns("upset_set_names") ,
                                  #             label = "Set names", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 1.5, 
                                  #             step = 0.01),
                                  # 
                                  # #numbers above bars,
                                  # sliderInput(inputId = ns("upset_number_above_bars") ,
                                  #             label = "Numbers above bars", 
                                  #             min = 0.1 ,
                                  #             max = 10 ,
                                  #             value = 2.5, 
                                  #             step = 0.01),
                                  # 
                                  # # upset main bar color 
                                  # colourpicker::colourInput(
                                  #   inputId = ns("upset_plot_mainbar_color"),
                                  #   label = "Main bar color",
                                  #   value = "#2D3E50",
                                  #   returnName = TRUE
                                  # ),
                                  # 
                                  # # upset sets.bar.color
                                  # colourpicker::colourInput(
                                  #   inputId = ns("upset_sets_bar_color"),
                                  #   label = "Sets bar color",
                                  #   value = "#23B99A",
                                  #   returnName = TRUE
                                  # ),
                                  # 
                                  # 
                                  # # upset matrix color 
                                  # colourpicker::colourInput(
                                  #   inputId = ns("upset_plot_matrix_color"),
                                  #   label = "Matrix color",
                                  #   value = "#F7A40A",
                                  #   returnName = TRUE
                                  # )
                                  
                                  
                                  #sets.bar.color = "gray23"
                                  
                                )
                         ),
                         column(width = 3,
                                export_base_graphics_ui(id = ns("export_upsetplot"))
                         ),
                         column(width = 3) ## empty column
                       )
                     )
    ),
    
    ####
    ## heatplot 
    ####
    
    conditionalPanel(condition =  paste0("input['",ns("view"),"'] == 'heatplot' && input['",ns("perform_go_analysis_trigger"),"'] "),
                     plotOutput(outputId = ns("go_enrichment_heatplot") , height = "auto")%>% 
                       shinycssloaders::withSpinner(color = "#18BC9C"),
                     hr(),
                     fluidRow(
                       column(
                         width = 12, 
                         column(width = 6,
                                plot_title_and_axis_label_ui(id = ns("heatplot_title_and_legend"))
                         ), 
                         column(width = 3,
                                
                                dropdownButton(
                                  inputId = ns("heatplot_advance_settings"),
                                  icon = icon("gears"),
                                  label = "Advance settings",
                                  size = "sm",
                                  circle = F,
                                  status = "success",
                                  up = T,
                                  right = F,
                                  
                                  
                                  ## heatplot tile colors 
                                  colourpicker::colourInput(
                                    inputId = ns("heatplot_tile_color"),
                                    label = "Select",
                                    value = "#E74C3C",
                                    returnName = TRUE
                                  ),
                                  
                                  # heatplot number of category to display 
                                  shiny::numericInput(inputId = ns("heatplot_number_of_category") , 
                                                      label = "Show terms" , min = 1, max = 50 , value = 5, step = 1 , width = "100%"),
                                  
                                  ## heat plot flip axis 
                                  shiny::checkboxInput(inputId = ns("heatplot_flip_axis") , label = "Flip axis" ,value = TRUE)
                                )
                                
                         ),
                         column(width = 3,
                                export_plot_ui(id = ns("export_heatplot"))
                         )
                         #column(width = 3) ## empty column
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
#' @param gene_set internal
#' @param genome internal
#' @param ui_id internal
#'
#' @rdname mod_functional_analysis
#' @import enrichplot
#' @keywords internal
functional_analysis_server <- function(input, output, session , gene_set = NULL, genome = NULL , ui_id = NULL){
  
  ##update list of genesets
  observe({
    req(gene_set())
    shinyWidgets::updatePickerInput(session = session , 
                                    inputId = "dotplot_input_geneset" , 
                                    choices = names(gene_set()) , 
                                    selected = names(gene_set()))
    
    shinyWidgets::updatePickerInput(session = session , 
                                    inputId = "except_dotplot_input_geneset" , 
                                    choices = names(gene_set()))
  })
  
  ####
  ## perform enrichment ----
  ####
  
  ## dot plot multiple genesets enrichment 
  compare_enriched_terms <- eventReactive(input$perform_go_analysis_trigger,{
    req(input$view == "dotplot")
    
    ## validate inputs
    validate(need(is.numeric(input$min_gs_size), "'# of minimum genes in a GO term must' be numeric"))
    validate(need(is.numeric(input$max_gs_size), "'# of maximum genes in a GO term must' be numeric"))
    
    q_genes <- gene_set()[input$dotplot_input_geneset]  
    comp_enr <- perform_go_enrichmet(genome = genome() , 
                                     ontology = input$go_type, 
                                     p_adjust_method = input$p_adj_method,
                                     pval_cutoff = 1,
                                     qval_cutoff = 1,
                                     min_gs_size = input$min_gs_size,
                                     max_gs_size =input$max_gs_size,
                                     cross_ref_go_db = ifelse(input$cross_go_bg_to_godb == "parent_and_offsprings" , TRUE, FALSE),
                                     query_genes = q_genes)
    
    ## check if CompareCluster result is real or an error 
    if(inherits(comp_enr, "rlang_error")){
      shinyWidgets::sendSweetAlert(session = session ,
                                   type = "error",
                                   title = "Error...!!" ,
                                   text = tags$h4(paste(comp_enr)))
      return(NULL)
    }
    return(comp_enr)
    
  })
  
  enriched_terms <- eventReactive(input$perform_go_analysis_trigger ,{
    
    validate(need(is.numeric(input$min_gs_size), "'# of minimum genes in a GO term must' be numeric"))
    validate(need(is.numeric(input$max_gs_size), "'# of maximum genes in a GO term must' be numeric"))
    
    q_genes <- gene_set()[[input$except_dotplot_input_geneset]]  
    if(input$view == "dotplot" &&  length(input$dotplot_input_geneset) == 1){
      q_genes <- gene_set()[[input$dotplot_input_geneset]]  
    }
    
    enr <- perform_go_enrichmet(genome = genome() , 
                                ontology = input$go_type, 
                                p_adjust_method = input$p_adj_method,
                                pval_cutoff = 1,
                                qval_cutoff = 1,
                                min_gs_size = input$min_gs_size,
                                max_gs_size =input$max_gs_size,
                                cross_ref_go_db = ifelse(input$cross_go_bg_to_godb == "parent_and_offsprings" , TRUE, FALSE),
                                query_genes = q_genes)
    
    
    ## check if enrichment result is real or an error 
    if(inherits(enr, "rlang_error") ){
      shinyWidgets::sendSweetAlert(session = session ,
                                   type = "error",
                                   title = "Error...!!" ,
                                   text = tags$h4(paste(enr)))
      return(NULL)
    }
    return(enr)
  })
  
  ####
  ## GO enrichment data table ----
  ####
  
  output$functional_out_data <- DT::renderDataTable({
    req(enriched_terms())
    go_table <- enriched_terms()@result %>% as_tibble() %>% dplyr::mutate_if(is.numeric, round, 4) 
    return(go_table)
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
  )
  )
  
  ####
  ## create dotplot ----
  ####
  
  dotplot_out <- reactive({
    input$perform_go_analysis_trigger
    
    validate(need(is.numeric(input$dot_plot_number_of_category), "Show terms must be a number"))
    gene_set_length = isolate(length(input$dotplot_input_geneset))
    validate(need(gene_set_length > 0, "No cluster selected"))
    
    ## if more than one gene set selected compare clusters
    if(gene_set_length > 1){
      req(compare_enriched_terms())
      dotplot_out <- dotplot(compare_enriched_terms() , 
                             color = input$dot_plot_color_by , 
                             showCategory = input$dot_plot_number_of_category)  + 
        theme(text = element_text(size = 15))
      
      ## wrap long terms 
      dotplot_out$data <- dotplot_out$data %>% as_tibble() %>% 
        dplyr::mutate(Description = factor(stringr::str_wrap(Description , 25) , levels = stringr::str_wrap(levels(Description) , 25)) ) 
      
      dotplot_out <- callModule(module  = plot_title_and_axis_label_server , 
                                id = "dot_plot_title_and_legend" ,
                                my_ggplot = dotplot_out ,
                                color_legend_title = input$dot_plot_color_by,
                                axis_x_title = shiny::isolate("Clusters") ,
                                axis_y_title = shiny::isolate("Terms") ,
                                x_tick_angle = 0)
      
      
    }else if(gene_set_length  == 1){## if one  gene set selected  show one geneset 
      req(enriched_terms())
      dotplot_out <- dotplot(enriched_terms() , 
                             color = input$dot_plot_color_by , 
                             showCategory = input$dot_plot_number_of_category , 
                             x = input$dot_plot_xaxis , 
                             orderBy  = input$dot_plot_xaxis) +
        theme(text = element_text(size = 20))  
      
      ## wrap long terms 
      dotplot_out$data <- dotplot_out$data %>% 
        as_tibble() %>% 
        dplyr::mutate(Description = stringr::str_wrap(Description , 25)  %>% 
                 fct_reorder(GeneRatio , .desc = F))
      
      dotplot_out <- callModule(module  = plot_title_and_axis_label_server , 
                                id = "dot_plot_title_and_legend" ,
                                my_ggplot = dotplot_out , 
                                axis_x_title = shiny::isolate(input$dot_plot_xaxis) , 
                                axis_y_title = shiny::isolate("Terms") , 
                                x_tick_angle = 0)  
      
    }else{
      return(NULL)
    }
    return(dotplot_out)
  })
  
  ## render dotplot 
  output$go_enrichment_dotplot <- renderPlot({
    req(dotplot_out())
    return(print(dotplot_out()))
  },res = 96,
  height = function() {
    dot_client_width = paste("output_" , ui_id, "-go_enrichment_dotplot_width",sep="")
    return(session$clientData[[dot_client_width]]) ## dynamic height
    
  },
  width = function() {
    dot_client_width = paste("output_" , ui_id, "-go_enrichment_dotplot_width",sep="")
    return(session$clientData[[dot_client_width]]) ## dynamic height
  }
  ) ## dynamic resolution
  
  ## export dotplot
  callModule(module = export_plot, id = "export_dotplot" , file_name = "dotplot", plot = dotplot_out )
  
  
  
  ####
  ## create barplot ----
  ####
  
  barplot_out <- reactive({
    req(enriched_terms())
    validate(need(is.numeric(input$bar_plot_number_of_category), "Show terms must be a number"))
    
    barplot_out <- barplot(enriched_terms() , 
                           color = input$bar_plot_color_by , 
                           showCategory = input$bar_plot_number_of_category , 
                           x = input$bar_plot_xaxis , 
                           orderBy  = input$bar_plot_xaxis) +
      theme(text = element_text(size = 20))  
    
    ## wrap long terms
    barplot_out$data <- barplot_out$data %>%
      as_tibble() %>%
      dplyr::mutate(Description = stringr::str_wrap(Description , 25)  %>%
               fct_reorder(GeneRatio, .desc = F))
    
    
    barplot_out <- callModule(module  = plot_title_and_axis_label_server , 
                              id = "bar_plot_title_and_legend" ,
                              my_ggplot = barplot_out , 
                              axis_y_title = shiny::isolate(input$bar_plot_xaxis) , 
                              axis_x_title = shiny::isolate("Terms") , 
                              x_tick_angle = 0)  
    
    return(barplot_out)
  })
  
  
  ## render bar plot
  output$go_enrichment_bar_plot <- renderPlot({
    req(barplot_out())
    return(print(barplot_out()))
    
  },res = 96,
  height = function() {
    barplot_client_width = paste("output_" , ui_id, "-go_enrichment_bar_plot_width",sep="")
    return(session$clientData[[barplot_client_width]]) ## dynamic height
  },
  width = function() {
    barplot_client_width = paste("output_" , ui_id, "-go_enrichment_bar_plot_width",sep="")
    return(session$clientData[[barplot_client_width]]) ## dynamic width
  }
  )
  
  ## export bar plot
  callModule(module = export_plot, id = "export_bar_plot" , file_name = "barplot", plot =  barplot_out)
  
  
  ####
  ## create emapplot ----
  ####
  emapplot_out <- reactive({
    req(enriched_terms())
    validate(need(is.numeric(input$emap_plot_number_of_category), "Show terms must be a number"))
    validate(need(input$emap_plot_number_of_category > 1, "Show terms must be > 1"))
    
    sim_mat <- enrichplot::pairwise_termsim(x = enriched_terms(),showCategory = input$emap_plot_number_of_category)
    emapplot_out <- emapplot(sim_mat , 
                             showCategory = input$emap_plot_number_of_category , 
                             color = input$emap_plot_color_by , 
                             layout.params = list(layout = input$emap_plot_layout_type)) 
    
    ## find GeomTextRepel layer
    geom_text_repel_index <- which(sapply(emapplot_out$layers, function(x) class(x$geom)[1]) == "GeomTextRepel")
    emapplot_out$layers[[geom_text_repel_index]] <- NULL
    #print("node label size ")
    #print(input$emap_node_label_size)
    emapplot_out <- emapplot_out %+% 
      ggrepel::geom_text_repel(aes(label = name , x = x, y = y  , size = input$emap_node_label_size ))
    return(emapplot_out)
  })
  
  ## render emapplot
  output$go_enrichment_emapplot <- renderPlot({
    req(emapplot_out())
    return(print(emapplot_out()))
    
  },res = 96,
  height = function() {
    emap_client_width = paste("output_" , ui_id,"-go_enrichment_emapplot_width",sep="")
    return(session$clientData[[emap_client_width]]) ## dynamic height
    
  },
  width = function() {
    emap_client_width = paste("output_" , ui_id,"-go_enrichment_emapplot_width",sep="")
    return(session$clientData[[emap_client_width]]) ## dynamic height
  }
  )
  ## export emaplot
  callModule(module = export_plot, id = "export_emapplot" , file_name = "emapplot", plot =  emapplot_out)
  
  ####
  ## create cnetplot ----
  ####
  cnetplot_out <- reactive({
    req(enriched_terms())
    validate(need(is.numeric(input$cnet_plot_number_of_category), "Show terms must be a number"))
    
    cnetplot_out <- cnetplot(enriched_terms() , 
                             node_label = "none",
                             circular = ifelse(input$cnet_plot_layout_type == "circle", TRUE , FALSE) ,
                             color.params = list(edge = TRUE), 
                             showCategory = input$cnet_plot_number_of_category,
                             layout = input$cnet_plot_layout_type)
    
    #cnetplot_out$layer[3] <- NULL
    cnetplot_out <- cnetplot_out %+%
      ggrepel::geom_text_repel(data = subset(cnetplot_out$data,color != "#B3B3B3") , 
                               aes(x = x, y = y , label = name) ,
                               size = input$cnet_node_label_size) +
      guides(edge_colour = "none")
    
    # cnetplot_out <- callModule(module  = plot_title_and_axis_label_server ,
    #                            id = "cnetplot_title_and_legend" ,
    #                           my_ggplot = cnetplot_out)
    
    return(cnetplot_out)
  })
  
  ## render cnetplot
  output$go_enrichment_cnetplot <- renderPlot({
    req(cnetplot_out())
    return(print(cnetplot_out()))
    
  },res = 96,
  height = function() {
    cnet_client_width = paste("output_" , ui_id, "-go_enrichment_cnetplot_width",sep="")
    
    #output_heatmap_functional_analysis_ui-go_enrichment_cnetplot_width
    
    return(session$clientData[[cnet_client_width]]) ## dynamic height
  },
  width = function() {
    cnet_client_width = paste("output_" , ui_id, "-go_enrichment_cnetplot_width",sep="")
    return(session$clientData[[cnet_client_width]]) ## dynamic width
  }
  )
  ## export cnetplot
  callModule(module = export_plot, id = "export_cnetplot" , file_name = "cnetplot", plot =  cnetplot_out)
  
  
  ####
  ## create upsetplot ----
  ####
  
  upset_plot_out <- reactive({
    req(enriched_terms())
    validate(need(is.numeric(input$upset_number_of_terms), "Number of terms must be a number"))
    
    upset_plot_out <- enrichplot::upsetplot(enriched_terms() ,  
                                            n = input$upset_number_of_terms
                                            
                                            ## latest version of enrichplot doesn't support below arguments. 
                                            
                                            # matrix.color = input$upset_plot_matrix_color,
                                            # main.bar.color = input$upset_plot_mainbar_color,
                                            # line.size = input$upset_line_size,
                                            # point.size = input$upset_point_size,
                                            # sets.bar.color = input$upset_sets_bar_color,
                                            # text.scale = c(input$upset_intersection_size_title , 
                                            #                input$upset_intersection_size_tick_labels, 
                                            #                input$upset_set_size_title ,
                                            #                input$upset_set_size_tick_labels , 
                                            #                input$upset_set_names , 
                                            #                input$upset_number_above_bars)
    )
    return(upset_plot_out)
  })
  
  #c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
  
  ## render cnetplot
  output$go_enrichment_upsetpot <- renderPlot({
    req(upset_plot_out())
    return(print(upset_plot_out()))
    
  },res = 96,
  height = function() {
    upset_client_width = paste("output_" , ui_id, "-go_enrichment_upsetpot_width",sep="")
    
    return(session$clientData[[upset_client_width]]) ## dynamic height
  },
  width = function() {
    upset_client_width = paste("output_" , ui_id, "-go_enrichment_upsetpot_width",sep="")
    return(session$clientData[[upset_client_width]]) ## dynamic width
  }
  )
  ## export cnetplot
  callModule(module = export_base_graphics,
             id = "export_upsetplot" ,
             file_name = "upsetplot",
             plot = upset_plot_out,
             isComplexHeatmap = FALSE
             
  )
  
  
  
  
  ####
  ## create heatplot ----
  ####
  
  heatplot_out <- reactive({
    req(enriched_terms())
    validate(need(is.numeric(input$heatplot_number_of_category), "Show terms must be a number"))
    
    heatplot_out <- enrichplot::heatplot(enriched_terms() , 
                              showCategory = input$heatplot_number_of_category, 
                              label_format = 25) + 
    geom_tile(aes(x = Gene , y = categoryID) ,
              fill= input$heatplot_tile_color ,
              colour = "white")
    
    ## flip axis
    if(input$heatplot_flip_axis) {
      heatplot_out <- heatplot_out + coord_flip()
    }
    heatplot_out <- callModule(module  = plot_title_and_axis_label_server , 
                               id = "heatplot_title_and_legend" ,
                               my_ggplot = heatplot_out , 
                               axis_x_title = shiny::isolate("Genes") , 
                               axis_y_title = shiny::isolate("Terms") , 
                               x_tick_angle =45)  
    
    ## wrap long text 
    # heatplot_out$data <- heatplot_out$data %>% 
    #   as_tibble() %>% 
    #   dplyr::mutate(categoryID = factor(stringr::str_wrap(categoryID , 25) , 
    #                                     levels = levels(categoryID) %>% stringr::str_wrap(25))) 
    
    return(heatplot_out)
  })
  
  
  ## render heatplot
  output$go_enrichment_heatplot <- renderPlot({
    req(heatplot_out())
    return(print(heatplot_out()))
    
  },res = 96,
  height = function() {
    heatplot_client_width = paste("output_" , ui_id, "-go_enrichment_heatplot_width",sep="")
    return(session$clientData[[heatplot_client_width]]) ## dynamic height
  },
  width = function() {
    heatplot_client_width = paste("output_" , ui_id, "-go_enrichment_heatplot_width",sep="")
    return(session$clientData[[heatplot_client_width]]) ## dynamic width
  }
  )
  
  
  
  
  ## export heatplot
  callModule(module = export_plot, id = "export_heatplot" , file_name = "heatplot", plot =  heatplot_out)
}

    

