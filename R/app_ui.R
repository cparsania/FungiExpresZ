#' app_ui
#' @import ggplot2
#' @import png
#' @import ggridges
#' @import wordcloud
#' @import clusterProfiler
#' @import shinycssloaders
#' @import ggcorrplot
#' @import jcolors
#' @import prettydoc
#' @import svglite
#' @import circlize
#' @import GGally
#' @import ggfortify
#' @import ggrepel
#' @import ComplexHeatmap
#' @import shinyWidgets
#' @import ggpubr
#' @import shiny
#' @import golem
#' @import htmltools
#' @import processx
#' @import attempt
#' @import shinythemes
#' @import viridis
#' @import formattable
#' @import colourpicker
#' @import ggalt
#' @import gtools
#' @importFrom  scales hue_pal
#' @importFrom  shinyjs hide
#' @importFrom  shinyjs show
#' @importFrom  shinyjs enable
#' @importFrom  shinyjs disable
#' @importFrom  shinyjs useShinyjs
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableProxy
#' @importFrom DT selectRows
#' @importFrom DT dataTableOutput
#' @importFrom DT JS
#' @importFrom DT datatable
#' @importFrom badger ver_devel
#' 
#' @keywords internal
#' 
app_ui <- function() {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      
      #tags$head(includeScript("./google_analytics.js")),
      #tags$head(includeScript("./inst/app/www/google_analytics.js")),
      #tags$head(includeScript("www/google_analytics.js")),
      theme = shinythemes::shinytheme("flatly"),
      shinyjs::useShinyjs(),
      
      ## import google fonts 
      tags$head(
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Ribeye&display=swap'
    "))
      ),
      
      ## arrange title panel 
      tags$div(
        #tags$a(img(src="https://raw.githubusercontent.com/cparsania/FungiExpresZ/master/inst/app/www/hex_sticker.png" , height="150")),
        titlePanel(
        tags$h1("Fungi", 
                tags$a(img(src="https://raw.githubusercontent.com/cparsania/FungiExpresZ/master/inst/app/www/hex_sticker.png" , height="150"),
                           href = "https://raw.githubusercontent.com/cparsania/FungiExpresZ/master/inst/app/www/hex_sticker.png" , target= "_blank"),
                "ExpresZ", 
                tags$h2("Fungal Gene Expression Data Analysis and Visualization Tool" , style = "font-family: 'Ribeye'") , 
                align = "center", 
                style = "font-family: 'Ribeye'; font-size: 80px;"
        ),
        windowTitle = "FungiExpresZ") ,
        style = "color:white;
    background-color:#2C3E50;
    text-align: center;
    padding: 5px 0 "),
      
      
      # CSS reference : https://stackoverflow.com/questions/28630433/how-to-align-navbar-to-center-with-css
      
      ## adjust  navbar panel 
      tags$style(HTML("
ul.nav.navbar-nav {
    left: 0px;
    width: 100%;
    display: flex;
    justify-content: center;
}")),
      
      #navbar navbar-default navbar-static-top
      navbarPage(collapsible = T,fluid = T,
                 title = "",
                 tabPanel(title = "App", icon = icon("home"),
                          
                          sidebarLayout(
                            
                            ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                            ## UI sidebar panel  ----
                            ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                            
                            sidebarPanel(
                              width = 3,
                              h2("Inputs"),
                              
                              ## upload data
                              shinyBS::bsCollapse(
                                open = c("Select / Upload data" , "Assign Groups"),
                                multiple = T,
                                
                                
                                shinyBS::bsCollapsePanel(
                                  title = "Select / Upload data", style = "primary",
                                  
                                  radioButtons(inputId = "data_selection", 
                                               label = "", 
                                               choices = c("Select data" = "select_sample", 
                                                           "Upload / Use example data" = "upload_user_data"),
                                               inline = F),
                                  
                                  ## select data action button
                                  conditionalPanel(
                                    condition = "input.data_selection =='select_sample'",
                                    shinyWidgets::actionBttn(
                                      inputId = "select_sample_trigger",
                                      label = "Select", color = "success",
                                      style = "gradient", size = "md",
                                      icon = icon("database"), block = T
                                    )
                                  ),
                                  
                                  
                                  ## Select datatrigger
                                  shinyBS::bsModal(
                                    id = "trigger_select_sample_data",
                                    title = fluidRow(
                                      fluidRow(column(11, offset = 1,tags$h2(tags$b("Sequence Read Archive (SRA) Run Inforamation Table"))))
                                    ),
                                    trigger = "select_sample_trigger", 
                                    size = "large",
                                    
                                    ## row having selection instruction 
                                    fluidRow(
                                      column (
                                        width = 12,
                                        tags$h3(tags$b("Click on a row to to select SRA samples for analysis and visualisation.", tags$br() , tags$br(), "Once selection made click submit")) 
                                      )
                                    ),
                                    
                                    hr(),
                                    
                                    ## row having row filter title
                                    fluidRow(
                                      column(
                                        width = 4, 
                                        offset = 5, 
                                        tags$h2(tags$b("Row filters"))
                                      )
                                    ),
                                    
                                    ### row having filer buttons   
                                    fluidRow(
                                      column(
                                        width = 4,
                                        
                                        ## Species 
                                        shinyWidgets::pickerInput(
                                          inputId = "select_species",
                                          label = "Organism",
                                          choices = NULL, 
                                          choicesOpt = T,
                                          width = "100%",
                                          options = list(
                                            size = 10,
                                            `selected-text-format` = "count > 10",
                                            style = "btn-success", "max-options" = 1, `live-search` = TRUE
                                          )
                                        )
                                      ),
                                      
                                      ## Strain 
                                      column(
                                        width = 4,
                                        shinyWidgets::pickerInput(
                                          inputId = "select_strain",
                                          label = "Strain",
                                          choices = NULL, 
                                          choicesOpt = F,
                                          selected = "", 
                                          multiple = T, 
                                          width = "100%",
                                          options = shinyWidgets::pickerOptions(liveSearch = TRUE, 
                                                                  size = 10 , 
                                                                  selectedTextFormat = "count > 10" ,
                                                                  maxOptions = 1,
                                                                  style = "btn-success")
                                          
                                        )
                                        
                                      ),
                                      
                                      ## Genotype 
                                      column(
                                        width = 4,
                                        shinyWidgets::pickerInput(
                                          inputId = "select_genotype",
                                          label = "Genotype",
                                          choices = NULL, choicesOpt = F,
                                          selected = "", multiple = T, width = "100%",
                                          options = shinyWidgets::pickerOptions(liveSearch = TRUE, 
                                                                  size = 10 , 
                                                                  selectedTextFormat = "count > 10" ,
                                                                  maxOptions = 1,
                                                                  style = "btn-success")
                                        )
                                      )
                                      
                                    ),
                                    
                                    ## reset filters 
                                    fluidRow(
                                      
                                      column(width = 4,
                                             offset = 4,
                                             
                                             ## reset strain
                                             shinyWidgets::actionBttn(
                                               inputId = "reset_strain_filter",
                                               label = "",
                                               icon = icon("sync-alt"),
                                               color ="success",
                                               style ="gradient",
                                               size = "sm"
                                             )
                                      ),
                                      
                                      column(width = 4,
                                             #reset genotype 
                                             shinyWidgets::actionBttn(
                                               inputId = "reset_genotype_filter",
                                               label = "",
                                               icon = icon("sync-alt"),
                                               color = "success",
                                               style ="gradient",
                                               size = "sm"
                                             )
                                      )
                                      
                                    ),
                                    
                                    
                                    hr(),
                                    
                                    
                                    ## select all rows 
                                    shinyWidgets::checkboxGroupButtons(
                                      inputId = "sra_sample_info_select_all_rows",
                                      label = "",
                                      choices = c("Select all rows" = TRUE),
                                      status = "success",selected = FALSE,
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon"),
                                        no = icon("remove",
                                                  lib = "glyphicon"))
                                    ),
                                    
                                    
                                    
                                    br(), br(),
                                    
                                    ## SRA info table 
                                    div(DT::dataTableOutput(outputId = "pre_loaded_data_sra_sample_info" , width = "auto") %>% 
                                          shinycssloaders::withSpinner(color = "#18BC9C") , style = "font-size:80%"),
                                    
                                    
                                    br(), br(),
                                    
                                    ## Display number of rows selected 
                                    
                                    fluidRow(
                                      column(width = 12,offset =5,
                                             tags$h3(tags$b(textOutput(outputId = "sra_info_number_of_rows_selected")))
                                      )
                                    ),
                                    
                                    
                                    br(), br(),
                                    
                                    
                                    
                                    fluidRow(
                                      ## clear all 
                                      column(width = 4,offset = 3,
                                             shinyWidgets::actionBttn(inputId = "deselect_all_rows", 
                                                                      label = "Clear all" ,
                                                                      size = "md" ,
                                                                      style = "gradient",
                                                                      icon = icon("times-circle"),
                                                                      
                                                                      color = "success")
                                      ),
                                      
                                      ## submit
                                      column(width = 4,
                                             shinyWidgets::actionBttn(inputId = "submit_user_sra_samples", 
                                                                      label = "Submit" , 
                                                                      size = "md", 
                                                                      style = "gradient" ,
                                                                      icon = icon("thumbs-up"),
                                                                      color = "success")
                                      )
                                    )
                                    
                                    
                                    
                                    
                                  ),
                                  
                                  ## upload data  action button
                                  conditionalPanel(
                                    condition = "input.data_selection =='upload_user_data'",
                                    shinyWidgets::actionBttn(
                                      inputId = "upload_sample_trigger",
                                      label = "Upload", color = "success",
                                      style = "gradient", size = "md",
                                      icon = icon("upload"), block = T
                                    )
                                  ),
                                  
                                  ## upload data trigger
                                  shinyBS::bsModal(size = "large",
                                          id = "trigger_upload_user_data", title = "Upload Data",
                                          trigger = "upload_sample_trigger",
                                          
                                          ## upload sample data checkbox
                                          shinyWidgets::materialSwitch(
                                            inputId = "upload_sample_data",
                                            label = tags$b("Upload example data"),
                                            value = FALSE,
                                            status = "info"
                                          ),
                                          shinyWidgets::progressBar(id = "upload_sample_data_pb", value = 0, status = "success", striped = TRUE),
                                          
                                          hr(),
                                          
                                          ##upload data, either from file or from clipboard 
                                          tags$div(id = "user_data_upload_section",
                                                   
                                                   tags$h2(tags$b("Step 1: Upload data")),
                                                   wellPanel(
                                                     fluidRow(
                                                       column(width = 12,
                                                              radioButtons(inputId = "upload_data_source" , label = "",
                                                                           choices = c("From file" = "upload_from_file", "From clipboard" = "upload_from_clipboard"),
                                                                           width = "100%",inline = T,
                                                              )
                                                       )
                                                       
                                                     ),
                                                     fluidRow(
                                                       conditionalPanel(condition = "input.upload_data_source == 'upload_from_file'",
                                                                        column(width = 12,
                                                                               fileInput(
                                                                                 inputId = "file1",
                                                                                 label = "",
                                                                                 #label = tags$h3(tags$b("Read from file")), #"Choose .txt file",
                                                                                 multiple = F,width = "100%",placeholder = "Upload .txt file",
                                                                                 accept = c(
                                                                                   "text/csv",
                                                                                   "text/comma-separated-values,text/plain",
                                                                                   ".csv"
                                                                                 )
                                                                               )
                                                                        ) 
                                                       ),
                                                       
                                                       conditionalPanel(condition = "input.upload_data_source == 'upload_from_clipboard'",
                                                                        column(width = 12,
                                                                               textAreaInput(placeholder = "Paste data here",
                                                                                             inputId = "user_pasted_data",
                                                                                             label = "",cols = 10, rows = 10
                                                                                             #label = tags$h3(tags$b("Read from file")), #"Choose .txt file",
                                                                                             #multiple = F,width = "100%",placeholder = "Upload .txt file",
                                                                               )
                                                                        )
                                                       )
                                                       
                                                     ),
                                                     
                                                     tags$div(id = "file_instruct", tags$h4(tags$b("Data uploading instructions")), "While uploding data from file, file must be of .txt format. Columns are variables (e.g. sample names) and rows are observations (e.g. genes). Each column and row must have unique identity given as first row and first column respectively. Same rules also appy while pasting data from clipboard.")
                                                   ),
                                                   
                                                   ## choice of column seperator
                                                   tags$h2(tags$b("Step 2: Select column separator")),
                                                   wellPanel(
                                                     radioButtons(
                                                       inputId = "sep",
                                                       label = "",
                                                       width = "100%",
                                                       choices = c(
                                                         Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"
                                                       ),
                                                       selected = "\t",
                                                       inline = T
                                                     )
                                                   ),
                                                   
                                                   ## select species
                                                   tags$h2(tags$b("Step 3: Select species (optional)")), 
                                                   tags$h5(tags$i(
                                                     "Corrrect species selection is required for gene annotations and GO analysis."
                                                   )),
                                                   br(),
                                                   
                                                   wellPanel(
                                                     shinyWidgets::pickerInput(
                                                       inputId = "user_selected_species",
                                                       label = "",
                                                       choices = rlang::set_names(species_table$genome , species_table$species),
                                                       selected = "FungiDB-42_AnidulansFGSCA4",
                                                       choicesOpt = list(
                                                         style = "btn-primary",
                                                         subtext = species_table$genome),
                                                       options = list(`showSubtext` = TRUE)
                                                     ),
                                                     
                                                     ## show random gene id  for selected species 
                                                     tags$div( tags$h4(tags$b("Values in the first column of file being uploaded must have below id type :")) ,
                                                               textOutput(outputId = "sample_selected_species_gene_id") %>% 
                                                                 shinycssloaders::withSpinner(color = "#18BC9C")) 
                                                     
                                                     
                                                   ),
                                                   
                                                   ## log transformation
                                                   tags$h2(tags$b("Step 4: Log transformation (optional)")),
                                                   wellPanel(
                                                     radioButtons(inputId = "log_transform_user_upload_data" ,
                                                                  label = "",
                                                                  choices = c("None" = "none" ,
                                                                              "Log2(value + 1)" = "log2" ,
                                                                              "Log10(value + 1)" = "log10"),
                                                                  selected = "none",inline = T
                                                     )
                                                   ),
                                                   
                                                   # decide whether user data to join with existing data
                                                   tags$h2(tags$b("Step 5: Join data (optional)")),
                                                   wellPanel(
                                                     shinyWidgets::materialSwitch(
                                                       inputId = "join_user_data",
                                                       label = ,
                                                       value = FALSE,
                                                       status = "success"
                                                     ),
                                                     ## join data
                                                     tags$div(id = "join_data_instruction",
                                                              tags$b("Join data"),
                                                              "feature allows users to merge user uploaded data with pre existing data if selected any. Uploaded dataset will be merge by gene names given in the first column."),
                                                     br()
                                                   ),
                                                   
                                                   ## submit
                                                   fluidRow(
                                                     column(width = 4 , offset = 5,
                                                            shinyWidgets::actionBttn(inputId = "submit_user_uploaded_data" ,
                                                                                     label = "Submit" ,
                                                                                     icon = icon("thumbs-up") ,
                                                                                     style = "gradient",
                                                                                     color = "success" ,
                                                                                     size = "md" ,
                                                                                     block = F)
                                                     )
                                                   )
                                                   
                                          )
                                          
                                          
                                  )
                                )
                              ),
                              hr(),
                              ## Assign  groups 
                              shinyBS::bsCollapse(id = "assing_gene_groups_and_sample_groups",
                                                  open = "Assign groups",
                                                  shinyBS::bsCollapsePanel(title = "Assign groups", style = "primary",
                                                                           add_sample_group_ui(id = "sample_group"),
                                                                           br(),
                                                                           add_gene_groups_ui(id = "gene_groups")
                                                  )
                              ),
                              hr(),
                              
                              ## Display active group info in the side panel
                              conditionalPanel(condition = "true" ,
                                               
                                               shinyBS::bsCollapse(id = "display_active_group_collapse_panel",
                                                                   open = "View active groups",
                                                                   shinyBS::bsCollapsePanel(title = "View active groups",
                                                                                            style = "primary" ,
                                                                                            display_active_groups_ui(id  = "display_active_groups")
                                                                   )
                                               )
                              ),
                              
                              
                              
                              ## Display user selected sra id sample info 
                              conditionalPanel(condition = "output.display_sra_sample_info_in_side_panel==true" , 
                                               
                                               shinyBS::bsCollapse(id = "display_sra_sample_info_collapse_panel",
                                                                   open = "View SRA sample info",
                                                                   shinyBS::bsCollapsePanel(title = "View SRA sample info",
                                                                                            style = "primary",
                                                                                            display_sra_sample_info_ui(id  = "user_selected_sra_id_sample_info")                    
                                                                   )
                                                                   
                                               )
                              ),
                              br(),
                               ## display website statistics in side panel 
                              shinyBS::bsCollapse(id = "display_live_statistics",
                                                  open = "Usage",
                                                  shinyBS::bsCollapsePanel(title = "Usage", 
                                                                           style = "primary",
                                                                           tags$div(
                                                                             HTML("<script type=\"text/javascript\" src=\"//rf.revolvermaps.com/0/0/7.js?i=5cen61amuqv&amp;m=0&amp;c=ff0000&amp;cr1=ffffff&amp;sx=0\" async=\"async\"></script>")
                                                                             
                                                                           )
                                                  )
                                                  
                              )
                            
                              
                              ## upload data ends
                            ),
                            
                            ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                            ## UI main panel
                            ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                            
                            mainPanel(
                              width = 9,
                              tabsetPanel(
                                type = "tabs",
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## scatter plot tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                tabPanel(
                                  title = "Scatter plot", value = "scatter_plot_panel", id = "scatter_plot_panel",
                                  
                                  ## select plot varibale
                                  h3(),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        multiple = T, open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## select plot variable
                                          fluidRow(
                                            column(width = 6,
                                                   ## select x variable
                                                   shinyWidgets::pickerInput(
                                                     inputId = "scatter_x",
                                                     label = "Select sample (X-axis)",
                                                     choices = NULL, choicesOpt = F,
                                                     selected = "", multiple = F, width = "100%",
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       style = "btn-success", `live-search` = TRUE
                                                     )
                                                   )
                                            ),
                                            
                                            column(width = 6,
                                                   ## select y variable
                                                   shinyWidgets::pickerInput(
                                                     inputId = "scatter_y",
                                                     label = "Select sample (Y-axis)",
                                                     choices = NULL, choicesOpt = F,
                                                     selected = "", multiple = F, width = "100%",
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       style = "btn-success", `live-search` = TRUE
                                                     )
                                                   )
                                                   
                                            )
                                          ),
                                          
                                          ## select gene groups 
                                          gene_group_selection_ui(id = "scatter_plot_select_gene_groups"),
                                          
                                          ## plot action
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_scatter" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                        )
                                      )
                                    )
                                  ),
                                  
                                  # plot output
                                  conditionalPanel(
                                    condition = "input.generate_scatter && output.scatter_plot_status==true",
                                    shinyBS::bsCollapse(
                                      multiple = T, open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty column
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "scatter_plot", 
                                                       brush = "plot_brush", height = "auto", width = "auto") 
                                          ),
                                          column(width = 2) ## empty column
                                        ),
                                        
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        ## scatter plot advanced options
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        
                                        fluidRow(
                                          hr(),
                                          # column(width = 1), ## empty column
                                          column(
                                            #offset = 1,
                                            width = 12,
                                            
                                            ## scatter plot Labels & Title button
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui(id = "scatter_plot_title_and_labels")
                                            ),
                                            
                                            ## scatter plot Advance options
                                            column(
                                              width = 3,
                                              shinyWidgets::dropdownButton(
                                                inputId = "scatter_plot_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = T,
                                                
                                                h4(tags$b("Axis limits")),
                                                actionLink(
                                                  inputId = "trigger_scatter_xy_limits", label = "Set",
                                                  icon = icon(name = "edit", lib = "font-awesome")
                                                ),
                                                
                                                hr(),
                                                
                                                ## scatter plot trend line  
                                                shinyWidgets::radioGroupButtons(
                                                  
                                                  inputId = "scatter_diagonal_line",
                                                  label = h4(tags$b("Trend line")),
                                                  choices = c(
                                                    "Off" = "off",
                                                    "From data" = "from_data",
                                                    "Manual" = "manual"
                                                  ),
                                                  individual = TRUE ,
                                                  justified = T, width = "400px",
                                                  status = "success" ,
                                                  size = "sm",
                                                  checkIcon = list(
                                                    yes = icon("ok", 
                                                               lib = "glyphicon"),
                                                    no = icon("remove",
                                                              lib = "glyphicon"))
                                                ),
                                                
                                                
                                                
                                                ## manual slope
                                                conditionalPanel(
                                                  condition = "input.scatter_diagonal_line == 'manual'",
                                                  fluidRow(
                                                    column(
                                                      width = 6,
                                                      numericInput(inputId = "manual_slope", label = h4(tags$b("Slope")), value = 1)
                                                    ),
                                                    column(
                                                      width = 6,
                                                      numericInput(inputId = "manual_intercept", label = h4(tags$b("Intercept")), value = 0)
                                                    )
                                                  )
                                                ),
                                                hr(),
                                                
                                                # color selection
                                                shinyWidgets::radioGroupButtons(
                                                  inputId = "scatter_color_chooser",
                                                  label = h4(tags$b("Color by")), choices = c("Default" = "default",
                                                                                              "Manual" = "manual" ,
                                                                                              "Gene group"="by_gene_groups"),
                                                  individual = TRUE ,
                                                  justified = T , width = "400px",
                                                  status = "success" ,size = "sm", 
                                                  checkIcon = list(
                                                    yes = icon("ok", 
                                                               lib = "glyphicon"),
                                                    no = icon("remove",
                                                              lib = "glyphicon"))
                                                ),
                                                
                                                ## color : low 
                                                conditionalPanel(
                                                  condition = "input.scatter_color_chooser == 'manual'",
                                                  fluidRow(
                                                    column(
                                                      width = 4,
                                                      colourpicker::colourInput(
                                                        inputId = "scatter_col_low",
                                                        label = "low",
                                                        value = "blue",
                                                        returnName = TRUE,
                                                        palette = "limited"
                                                      )
                                                    ),
                                                    ## color : med 
                                                    column(
                                                      width = 4,
                                                      colourpicker::colourInput(
                                                        inputId = "scatter_col_medium",
                                                        label = "med",
                                                        value = "yellow",
                                                        returnName = TRUE,
                                                        palette = "limited"
                                                      )
                                                    ),
                                                    
                                                    ## color : high 
                                                    column(
                                                      width = 4,
                                                      colourpicker::colourInput(
                                                        inputId = "scatter_col_high",
                                                        label = "high",
                                                        value = "red",
                                                        returnName = TRUE,
                                                        palette = "limited"
                                                      )
                                                    )
                                                  )
                                                  
                                                ),
                                                hr(),
                                                ## transparancy
                                                sliderInput(
                                                  inputId = "scatter_alpha",
                                                  label = h4(tags$b("Transparancy")), min = 0, max = 1, value = 0.5, step = 0.01, width = "99%"
                                                ),
                                                hr(),
                                                
                                                ##  adjust dot size
                                                sliderInput(
                                                  inputId = "scatter_point_size",
                                                  label = h4(tags$b("Point size")), min = 1, max = 10, value = 2, step = 0.1,width = "99%"
                                                )
                                                
                                              )
                                            ),
                                            
                                            ## export plot as image
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_scatter")
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    
                                    ### Triggered actions
                                    ## set X-Y limits
                                    shinyBS::bsModal(
                                      id = "scatter_xy_limits", title = "Set axis limits", trigger = "trigger_scatter_xy_limits",
                                      
                                      column(
                                        width = 6,
                                        ## X-lim
                                        numericInput(inputId = "plot_xmin", label = "X-Min:", value = ""),
                                        numericInput(inputId = "plot_xmax", label = "X-Max:", value = "")
                                      ),
                                      column(
                                        width = 6,
                                        ## Y-lim
                                        numericInput(inputId = "plot_ymin", label = "Y-Min:", value = ""),
                                        numericInput(inputId = "plot_ymax", label = "Y-Max:", value = "")
                                      )
                                    )
                                    ### set X-Y limits ends
                                    
                                  ),
                                  
                                  
                                  ## scatter plot table output
                                  conditionalPanel(
                                    condition = "output.scatter_plot && input.plot_brush",
                                    #downloadButton(outputId = "download_scatter_data", label = "", icon = icon("download")),
                                    
                                    ## scatter plot data or functional analysis 
                                    radioGroupButtons(
                                      inputId = "scatterplot_data_or_functional_analysis",
                                      choices = c("Show Data" = "show_scatterplot_data", 
                                                  "Functional Analysis" =  "scatterplot_functional_analysis"),
                                      status = "success",
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon")) , 
                                      size = "normal" ,
                                      direction = "horizontal" ,
                                      justified = TRUE , 
                                      individual =  TRUE ,selected = "",
                                      width = "100%"
                                      
                                    ),
                                    br(),
                                    
                                    ## show scatter plot brushed data
                                    conditionalPanel(
                                      condition = "input.scatterplot_data_or_functional_analysis == 'show_scatterplot_data'",
                                      shinyBS::bsCollapse(
                                        open = "Selected points",
                                        shinyBS::bsCollapsePanel(
                                          title = "Selected points", style = "primary",
                                          DT::dataTableOutput(outputId = "brush_table", width = "auto")  %>% shinycssloaders::withSpinner(color = "#18BC9C")
                                        )
                                      )
                                    ),
                                    ## show scatter plot functional analysis
                                    conditionalPanel(
                                      condition = "input.scatterplot_data_or_functional_analysis == 'scatterplot_functional_analysis'",
                                      shinyBS::bsCollapse(
                                        open = "Functional analysis",
                                        shinyBS::bsCollapsePanel(
                                          title = "Functional analysis", style = "primary",
                                          functional_analysis_ui(id = "scatterplot_functional_analysis_ui")
                                        )
                                      )
                                    )
                                  )
                                  
                                  
                                  
                                ),
                                ## scatter plot tab panel ends
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## multi scatter plot tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                tabPanel(
                                  title = "Multi-scatter plot", value = "multi_scatter_plot_panel", id = "multi_scatter_plot_panel",
                                  br(),
                                  
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          # select plot variables
                                          shinyWidgets::pickerInput(
                                            inputId = "multi_scatter_vars",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE
                                            )
                                          ),
                                          
                                          # select gene groups
                                          gene_group_selection_ui("multi_scatter_plot_gene_group_selection"),
                                          
                                          ## plot action
                                          # actionButton(
                                          #   inputId = "generate_multi_scatter",
                                          #   label = "Plot"
                                          # )
                                          
                                          hr(),
                                          ## plot action
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_multi_scatter" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.generate_multi_scatter && output.multi_scatter_plot_status==true",
                                    shinyBS::bsCollapse(
                                      multiple = T, open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty column
                                          column(
                                            width = 8,
                                            
                                            plotOutput(outputId = "multi_scatter_plot", height = "auto", width = "auto") 
                                          ),
                                          column(width = 2) ## empty column
                                        ),
                                        
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        ## multi scatter plot advanced options
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        
                                        fluidRow(
                                          hr(),
                                          # column(width = 1), ## empty column
                                          column(
                                            width = 12,
                                            
                                            ## multi scatter labels and Titles 
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui(id = "multi_scatter_plot_title_and_labels")
                                            ),
                                            
                                            ## multi scatter Advance options 
                                            column(
                                              width = 3,
                                              dropdownButton(
                                                inputId = "multi_scatter_plot_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = T,
                                                
                                                ## corr text color   
                                                colourpicker::colourInput(
                                                  inputId = "multi_scatter_corr_text_col",
                                                  label = tags$h4(tags$b("Corr text color")),
                                                  value = "red",
                                                  returnName = TRUE,
                                                  palette = "limited"
                                                ),
                                                
                                                ## corr text font size
                                                sliderInput(
                                                  inputId = "multi_scatter_corr_text_text_size", label = tags$h4(tags$b("Font size: Corr text")),
                                                  min = 1, max = 30, value = 5, step = 1
                                                )
                                                
                                              )
                                              
                                            ),
                                            
                                            ## export plot as image
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_multi_scatter")
                                            )
                                          )
                                          # column(width = 1) ## empty column
                                        )
                                      )
                                    )
                                  )
                                  
                                ),
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## Corr heatbox panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                tabPanel(
                                  title = "CorrHeatBox", value = "corr_heatbox_panel", id = "corr_heatbox_panel",
                                  br(),
                                  
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## corr heat box : select plot variables
                                          shinyWidgets::pickerInput(
                                            inputId = "corr_heatbox_vars",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE
                                            )
                                          ),
                                          
                                          ## corr heat box : select gene groups
                                          gene_group_selection_ui("corr_heat_box_gene_group_selection"),
                                          
                                          ## plot action
                                          
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_corr_heatbox" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.generate_corr_heatbox && output.corr_heatbox_status == true",
                                    shinyBS::bsCollapse(
                                      multiple = T, open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty column
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "corr_heatbox", height = "auto", width = "auto") 
                                          ),
                                          column(width = 2) ## empty column
                                        ),
                                        
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        ## corr_heatbox advanced options
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        
                                        fluidRow(
                                          column(
                                            width = 12,
                                            ## corr_heatbox labels and Titles 
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui(id = "corr_heatbox_title_and_labels")
                                            ),
                                            
                                            ## corr_heatbox Advance options 
                                            column(
                                              width = 3,
                                              dropdownButton(
                                                inputId = "corr_heatbox_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = F,
                                                
                                                sliderInput(inputId = "corr_heatbox_colors" , 
                                                            label = "Number of colors" ,
                                                            value = 5,min = 1, 
                                                            max = 5, 
                                                            step = 1, 
                                                            width = "100%"),
                                                hr(),
                                                ## Cluster by correlation 
                                                shinyWidgets::radioGroupButtons(inputId = "cluster_heatbox" , 
                                                                                label = tags$h4(tags$b("Cluster variables")),
                                                                                choices = c("TRUE" = TRUE , "FALSE" = FALSE),
                                                                                selected = TRUE, 
                                                                                width = "100%",  
                                                                                direction = "horizontal", 
                                                                                individual = TRUE ,
                                                                                justified = T,
                                                                                status = "success" ,
                                                                                size = "sm",
                                                                                checkIcon = list(
                                                                                  yes = icon("ok", 
                                                                                             lib = "glyphicon"),
                                                                                  no = icon("remove",
                                                                                            lib = "glyphicon"))
                                                ),
                                                hr(),
                                                ## heatbox method 
                                                shinyWidgets::radioGroupButtons(inputId = "heatbox_method" , 
                                                                                label = tags$h4(tags$b("Method")),
                                                                                choices = c("Circle" ="circle" ,
                                                                                            "Square" = "square") , 
                                                                                selected = "square",
                                                                                width = "100%",
                                                                                direction = "horizontal", 
                                                                                individual = TRUE ,
                                                                                justified = T,
                                                                                status = "success",
                                                                                size = "sm",
                                                                                checkIcon = list(
                                                                                  yes = icon("ok", 
                                                                                             lib = "glyphicon"),
                                                                                  no = icon("remove",
                                                                                            lib = "glyphicon"))
                                                ),
                                                hr(),
                                                ## heatbox type 
                                                shinyWidgets::radioGroupButtons(inputId = "heatbox_type" ,
                                                                                label = tags$h4(tags$b("Type")),
                                                                                choices = c("Full" ="full" , "Upper" = "upper" , "Lower" = "lower"),
                                                                                width = "100%",
                                                                                direction = "horizontal",
                                                                                individual = TRUE ,
                                                                                justified = T,
                                                                                status = "success",
                                                                                size = "sm" ,
                                                                                checkIcon = list(
                                                                                  yes = icon("ok", 
                                                                                             lib = "glyphicon"),
                                                                                  no = icon("remove",
                                                                                            lib = "glyphicon"))
                                                ),
                                                
                                                
                                                conditionalPanel(
                                                  condition = "input.heatbox_type != 'full'",
                                                  
                                                  ## show diagonal 
                                                  shinyWidgets::radioGroupButtons(inputId = "heatbox_show_diag" , 
                                                                                  label = tags$h4(tags$b("Show diagonal")),
                                                                                  choices = c("TRUE" = TRUE , "FALSE" = FALSE),
                                                                                  selected = "TRUE", 
                                                                                  width = "100%" , 
                                                                                  direction = "horizontal", 
                                                                                  individual = TRUE ,
                                                                                  justified = T,
                                                                                  status = "success" ,
                                                                                  size = "sm",
                                                                                  checkIcon = list(
                                                                                    yes = icon("ok", 
                                                                                               lib = "glyphicon"),
                                                                                    no = icon("remove",
                                                                                              lib = "glyphicon"))
                                                  )
                                                  
                                                ),
                                                hr(),
                                                
                                                
                                                ## corr heatbox scale choice 
                                                shinyWidgets::radioGroupButtons(inputId = "corr_heatbox_scale_manual" ,
                                                                                label = tags$h4(tags$b("Scale")) ,
                                                                                choices = c("Auto" = "auto" , "Manual" = "manual") , 
                                                                                selected = "auto" ,
                                                                                width = "100%" , 
                                                                                direction = "horizontal", 
                                                                                individual = TRUE ,
                                                                                justified = T,
                                                                                status = "success",
                                                                                size = "sm",
                                                                                checkIcon = list(
                                                                                  yes = icon("ok", 
                                                                                             lib = "glyphicon"),
                                                                                  no = icon("remove",
                                                                                            lib = "glyphicon"))
                                                ), 
                                                ## corr heat box manual color scale 
                                                conditionalPanel(condition = "input.corr_heatbox_scale_manual == 'manual'" , 
                                                                 fluidRow(
                                                                   column(
                                                                     width = 6, 
                                                                     numericInput(inputId = "corr_heatbox_scale_manual_min" ,
                                                                                  label = "Scale low",
                                                                                  value = -1, min = -1, max = 1,step = 0.1,width = "100%")
                                                                   ),
                                                                   column(width = 6,
                                                                          numericInput(inputId = "corr_heatbox_scale_manual_max" ,
                                                                                       label = "Scale high",
                                                                                       value = 1, 
                                                                                       min = -1, 
                                                                                       max = 1,
                                                                                       step = 0.1,width = "100%")  
                                                                   )
                                                                 )
                                                ),
                                                hr(),
                                                
                                                ## show / hide corr value
                                                shinyWidgets::radioGroupButtons(
                                                  inputId = "heatbox_show_corr_value" , 
                                                  label = tags$h4(tags$b("Show corr value")),
                                                  choices = c("TRUE"= TRUE, "FALSE" = FALSE),
                                                  selected  = FALSE ,
                                                  width = "100%",
                                                  direction = "horizontal", 
                                                  individual = TRUE ,
                                                  justified = T,
                                                  status = "success",
                                                  size = "sm",
                                                  checkIcon = list(
                                                    yes = icon("ok", 
                                                               lib = "glyphicon"),
                                                    no = icon("remove",
                                                              lib = "glyphicon"))
                                                ),
                                                
                                                # checkboxInput(inputId = "heatbox_show_corr_value" , 
                                                #               label = "Show corr value",value = FALSE , width = "100%"),
                                                
                                                hr(),
                                                
                                                ## corr text font size
                                                sliderInput(
                                                  inputId = "heatbox_corr_text_text_size", 
                                                  label = "Font size: Corr text",width = "100%",
                                                  min = 1, max = 30, value = 5, step = 0.1
                                                ),
                                                hr(),
                                                
                                                ## corr text color
                                                colourpicker::colourInput(
                                                  inputId = "heatbox_corr_text_col",
                                                  label = "Corr text color",
                                                  value = "yellow",
                                                  returnName = TRUE, 
                                                  palette = "limited"
                                                )
                                              )
                                            ),
                                            
                                            ## export plot as image
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_corr_heatbox")
                                            )
                                          )
                                          # column(width = 1) ## empty column
                                        )
                                      )
                                    )
                                    
                                  ),
                                  
                                  ## show corr heatbox  data 
                                  conditionalPanel(condition = "input.generate_corr_heatbox && output.corr_heatbox_status == true",
                                                   shinyBS::bsCollapse(
                                                     open = "CorrHeatboxData",
                                                     shinyBS::bsCollapsePanel(
                                                       title = "CorrHeatboxData", 
                                                       style = "primary",
                                                       DT::dataTableOutput(outputId = "corr_heatbox_data", width = "auto") %>% 
                                                         shinycssloaders::withSpinner(color = "#18BC9C")
                                                     )
                                                   )
                                  )
                                ),
                                
                                
                                
                                
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## density plot tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                tabPanel(
                                  title = "Density plot", value = "density_plot_panel", id = "density_plot_panel",
                                  br(),
                                  
                                  ## select x variables
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## select density plot variables 
                                          shinyWidgets::pickerInput(
                                            inputId = "density_x",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE
                                            )
                                          ),
                                          
                                          ## density gene group selection 
                                          gene_group_selection_ui("density_plot_gene_group_selection"),
                                          
                                          ## plot action
                                          
                                          hr(),
                                          column(width =6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_density" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  ## display density plot
                                  conditionalPanel(
                                    condition = "input.generate_density && output.density_plot_status == true",
                                    shinyBS::bsCollapse(
                                      open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty col
                                          
                                          ## plot output column
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "density_plot", height = "auto")
                                          ),
                                          column(width = 2) ## empty col
                                        ),
                                        
                                        ## density plot advanced setting buttons
                                        fluidRow(
                                          hr(),
                                          column(
                                            width = 12,
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui(id = "decorate_density_plot")
                                            ),
                                            column(
                                              width = 3,
                                              dropdownButton(
                                                inputId = "density_plot_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = T,
                                                
                                                br(),
                                                
                                                ## fill and facet density plot 
                                                ggplot_fill_and_facet_ui(id = "density_plot_fill_and_facet"),
                                                
                                                ## density transparancy 
                                                sliderInput(
                                                  inputId = "density_plot_alpha",
                                                  label = tags$h4(tags$b("Transparancy")), min = 0, max = 1, value = 0.5, step = 0.01, width = "99%"
                                                )
                                                
                                              )
                                            ),
                                            
                                            ## export plot as image
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_density")
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                ),
                                ## density plot tab panel ends
                                
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## histogram ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                tabPanel(
                                  title = "Histogram", value = "histogram_panel", id = "histogram_panel",
                                  br(),
                                  
                                  
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## select histogram  variables
                                          shinyWidgets::pickerInput(
                                            inputId = "histogram_x",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              `live-search` = TRUE, 
                                              style = "btn-success"
                                            )
                                          ),
                                          
                                          gene_group_selection_ui("histogram_gene_group_selection"),
                                          
                                          ## plot action
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_histogram" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  ## display histogram
                                  conditionalPanel(
                                    condition = "input.generate_histogram && output.histogram_status == true",
                                    shinyBS::bsCollapse(
                                      open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty col
                                          
                                          ## plot output column
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "histogram", height = "auto")
                                          ),
                                          column(width = 2) ## empty col
                                        ),
                                        
                                        ## histogram  advanced options 
                                        fluidRow(
                                          hr(),
                                          column(
                                            width = 12,
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui(id = "decorate_histogram")
                                            ),
                                            column(
                                              width = 3,
                                              dropdownButton(
                                                inputId = "histogram_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = T,
                                                
                                                br(),
                                                
                                                ggplot_fill_and_facet_ui(id = "histogram_fill_and_facet"),
                                                
                                                ## hitogram position choices.
                                                selectInput(
                                                  inputId = "histogram_positions",choices = c("Dodge" = "dodge" , "Stack" = "stack" , "Identity" = "identity"), 
                                                  label = tags$h4(tags$b("Bar positions")) ,
                                                  selected = "dodge",multiple = FALSE, width = "100%" 
                                                ),
                                                hr(),
                                                
                                                ## histogram number of bins
                                                numericInput(
                                                  inputId = "histogram_number_of_bins",
                                                  label = tags$h4(tags$b("Number of bins")), value = 30 , min = 1, max = 1000 , step = 1,
                                                  width = "100%"
                                                ),
                                                hr(),
                                                
                                                ## histogram transparancy 
                                                sliderInput(
                                                  inputId = "histogram_alpha",
                                                  label = tags$h4(tags$b("Transparancy")), min = 0, max = 1, value = 0.5, step = 0.01 , width = "99%"
                                                )
                                                
                                              )
                                            ),
                                            
                                            ## export plot as image
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_histogram")
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                  
                                ),
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## Joy plot   tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                tabPanel(
                                  title = "Joy plot", value = "joy_plot_panel", id = "joy_plot_panel",
                                  br(),
                                  
                                  ## select x variables
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## select joy plot variables 
                                          shinyWidgets::pickerInput(
                                            inputId = "joy_plot_x",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE
                                            )
                                          ),
                                          
                                          ##  joy plot gene groups 
                                          gene_group_selection_ui("joy_plot_gene_group_selection"),
                                          
                                          
                                          ## plot action
                                          
                                          hr(),
                                          column(width =6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_joy_plot" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  ## display Joy plot
                                  conditionalPanel(
                                    condition = "input.generate_joy_plot && output.joy_plot_status == true",
                                    shinyBS::bsCollapse(
                                      open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty col
                                          
                                          ## plot output column
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "joy_plot", height = "auto")
                                          ),
                                          column(width = 2) ## empty col
                                        ),
                                        
                                        ## joy  plot settings 
                                        fluidRow(
                                          hr(),
                                          
                                          ## joy plot labels and title 
                                          column(
                                            width = 12,
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui("decorate_joy_plot")
                                            ),
                                            
                                            ## joy plot advance settings
                                            column(
                                              width = 3,
                                              dropdownButton(
                                                inputId = "joy_plot_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = T,width = "450px",
                                                
                                                br(),
                                                
                                                ## joyplot fill and facet UI 
                                                joy_plot_fill_and_facet_ui("joy_plot_fill_and_facet")
                                              )
                                            ),
                                            
                                            
                                            ## export plot as image
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_joy_plot")
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                ),
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## box plot tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                tabPanel(
                                  title = "Box plot", value = "box_plot_panel", id = "box_plot_panel",
                                  
                                  
                                  br(),
                                  ## select plot varibale
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## select x variable
                                          
                                          shinyWidgets::pickerInput(
                                            inputId = "box_x",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success" , `live-search` = TRUE
                                            )
                                          ),
                                          
                                          gene_group_selection_ui("box_plot_gene_group_selection"),
                                          
                                          ## plot action
                                          
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_box" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  ## display box plot
                                  conditionalPanel(
                                    condition = "input.generate_box && output.box_plot_status == true",
                                    shinyBS::bsCollapse(
                                      open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty cols
                                          
                                          ## display box  plot
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "box_plot", height = "auto", width = "auto")
                                          ),
                                          column(width = 2) ## empty cols
                                        ),
                                        
                                        ## box plot setting options.
                                        fluidRow(
                                          hr(),
                                          column(width = 12, 
                                                 column(
                                                   width = 6,
                                                   plot_title_and_axis_label_ui(id = "box_plot_label_and_title")
                                                 ),
                                                 
                                                 ## box plot advance options. 
                                                 column(
                                                   width = 3,
                                                   dropdownButton(
                                                     inputId = "box_plot_options",
                                                     icon = icon("cogs"),
                                                     label = "Advance options",
                                                     size = "sm",
                                                     circle = F,
                                                     status = "success",
                                                     up = T,
                                                     right = T,
                                                     br(),
                                                     
                                                     ## fill and facet ui 
                                                     ggplot_fill_and_facet_ui(id = "box_plot_fill_and_facet"),
                                                     
                                                     
                                                     ## pvalue
                                                     radioGroupButtons(
                                                       inputId = "box_plot_pvalue", 
                                                       label = tags$h4(tags$b("P-value")) ,
                                                       size = "sm" , individual = T,
                                                       justified = T, 
                                                       status = "success",
                                                       choices = c("Hide" = "FALSE", "Show" = "TRUE"),
                                                       checkIcon = list(
                                                         yes = icon("ok", 
                                                                    lib = "glyphicon"),
                                                         no = icon("remove",
                                                                   lib = "glyphicon"))
                                                     ),
                                                     
                                                     ## pvalue method
                                                     conditionalPanel(
                                                       condition = "input.box_plot_pvalue == 'TRUE'",
                                                       selectInput(
                                                         inputId = "box_plot_pval_method",
                                                         label = tags$h4(tags$b("Method")),
                                                         choices = c("t.test" = "t.test", "wilcox.test" = "wilcox.test")
                                                       )
                                                     ),
                                                     hr(),
                                                     
                                                     ## box plot dots 
                                                     
                                                     radioGroupButtons(
                                                       inputId = "box_plot_show_dots", 
                                                       label = tags$h4(tags$b("Dots")) ,
                                                       size = "sm" , individual = T,
                                                       justified = T, 
                                                       status = "success",
                                                       choices = c("Hide" = "FALSE", "Show" = "TRUE"),
                                                       checkIcon = list(
                                                         yes = icon("ok", 
                                                                    lib = "glyphicon"),
                                                         no = icon("remove",
                                                                   lib = "glyphicon"))
                                                     ),
                                                     hr(),
                                                     
                                                     ## box plot dots related UI 
                                                     conditionalPanel("input.box_plot_show_dots =='TRUE'", 
                                                                      ##box plot dots width 
                                                                      sliderInput(inputId = "boxplot_dots_width" ,
                                                                                  label = tags$h4(tags$b("Dots width")),
                                                                                  min = 0.1, 
                                                                                  max = 1, 
                                                                                  value = 0.1,
                                                                                  step = 0.01,width = "99%"
                                                                      ),
                                                                      
                                                                      
                                                                      ## box plot dots color 
                                                                      colourpicker::colourInput(
                                                                        inputId = "boxplot_dots_color",
                                                                        label = tags$h4(tags$b("Dots color")),
                                                                        value = "black",
                                                                        returnName = TRUE,
                                                                        palette = "limited"
                                                                      ),
                                                                      
                                                                      
                                                                      ## box plot dots transparancy 
                                                                      sliderInput(inputId = "boxplot_dots_transprancy" ,
                                                                                  label = tags$h4(tags$b("Dots transparancy")),
                                                                                  min = 0.1, 
                                                                                  max = 1, 
                                                                                  value = 0.5,
                                                                                  step = 0.01,
                                                                                  width = "99%"
                                                                      )
                                                                      
                                                                      
                                                     ),
                                                     
                                                     
                                                     
                                                     
                                                     
                                                     ## Box transparacny 
                                                     sliderInput(
                                                       inputId = "box_plot_alpha",
                                                       label = tags$h4(tags$b("Box transparancy")) , 
                                                       min = 0, 
                                                       max = 1, 
                                                       value = 0.5, 
                                                       step = 0.01, 
                                                       width = "99%"
                                                     ),
                                                     hr()
                                                     
                                                     
                                                   )
                                                 ),
                                                 
                                                 ## export plot as image
                                                 column(
                                                   width = 3,
                                                   export_plot_ui(id = "export_box_plot")
                                                 )
                                          ))
                                      )
                                    )
                                  )
                                ),
                                ## box plot tab panel ends
                                
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## violin plot tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                
                                tabPanel(
                                  title = "Violin  plot", value = "violin_plot_panel", id = "violin_plot_panel",
                                  
                                  
                                  br(),
                                  
                                  ## select plot varibale
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## select x variable
                                          shinyWidgets::pickerInput(
                                            inputId = "violin_x",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE
                                            )
                                          ),
                                          
                                          ## violin plot gene group selection 
                                          gene_group_selection_ui("violin_plot_gene_group_selection"),
                                          
                                          
                                          ## plot action
                                          
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_violin" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  ## display violin plot
                                  conditionalPanel(
                                    condition = "input.generate_violin && output.violin_plot_status == true",
                                    shinyBS::bsCollapse(
                                      open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty cols
                                          
                                          ## display violin  plot
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "violin_plot", height = "auto", width = "auto")
                                          ),
                                          column(width = 2) ## empty cols
                                        ),
                                        
                                        ## violin plot setting options.
                                        fluidRow(
                                          hr(),
                                          column(width = 12, 
                                                 column(
                                                   width = 6,
                                                   plot_title_and_axis_label_ui(id = "violin_plot_label_and_title")
                                                 ),
                                                 
                                                 ## violin plot advance options. 
                                                 column(
                                                   width = 3,
                                                   dropdownButton(
                                                     inputId = "violin_plot_options",
                                                     icon = icon("cogs"),
                                                     label = "Advance options",
                                                     size = "sm",
                                                     circle = F,
                                                     status = "success",
                                                     up = T,
                                                     right = T,
                                                     br(),
                                                     
                                                     # # fill violin
                                                     # shinyWidgets::radioGroupButtons(
                                                     #   inputId = "fill_violin_plot",
                                                     #   label = tags$h4(tags$b("Fill by")), 
                                                     #   choices = c("Samples" = "samples", "Sample groups" = "groups" ,
                                                     #               "Identical" = "identical") ,
                                                     #   selected = "samples" ,status = "success" ,size = "sm", 
                                                     #   direction = "horizontal",
                                                     #   individual = TRUE ,
                                                     #   justified = T,
                                                     #   checkIcon = list(
                                                     #     yes = icon("ok", 
                                                     #                lib = "glyphicon"),
                                                     #     no = icon("remove",
                                                     #               lib = "glyphicon"))
                                                     # ),
                                                     # 
                                                     # ## fill violin manually
                                                     # conditionalPanel(
                                                     #   condition = "input.fill_violin_plot == 'identical'",
                                                     #   fluidRow(
                                                     #     column(
                                                     #       width = 4,
                                                     #       colourpicker::colourInput(
                                                     #         inputId = "violin_plot_color_chooser",
                                                     #         label = tags$h4(tags$b("Select")) ,
                                                     #         value = "black",
                                                     #         returnName = TRUE,
                                                     #         palette = "limited"
                                                     #       )
                                                     #     )  
                                                     #   )
                                                     # ),
                                                     # 
                                                     # hr(),
                                                     
                                                     ggplot_fill_and_facet_ui(id = "violin_plot_fill_and_facet"),
                                                     
                                                     # ## pvalue
                                                     radioGroupButtons(
                                                       inputId = "violin_plot_pvalue", 
                                                       label = "P-value",
                                                       size = "sm" , 
                                                       individual = T, justified = T,
                                                       status = "success",
                                                       choices = c("Hide" = "FALSE", "Show" = "TRUE"),
                                                       checkIcon = list(
                                                         yes = icon("ok", 
                                                                    lib = "glyphicon"),
                                                         no = icon("remove",
                                                                   lib = "glyphicon"))
                                                     ),
                                                     
                                                     ## pvalue method
                                                     conditionalPanel(
                                                       condition = "input.violin_plot_pvalue == 'TRUE'",
                                                       selectInput(
                                                         inputId = "violin_plot_pval_method",
                                                         label = "Method",
                                                         choices = c("t.test" = "t.test", "wilcox.test" = "wilcox.test")
                                                       )
                                                     ),
                                                     hr(),
                                                     
                                                     ## show quantiles lines
                                                     shinyWidgets::checkboxGroupButtons(inputId = "violin_show_quantile" ,
                                                                                        label = tags$h4(tags$b("Show quantile")) , 
                                                                                        choices = c("0.1"=0.1, "0.25"=0.35, "0.5"=0.5 , "0.75"=0.75 , "0.9"=0.9) , 
                                                                                        width = "100%", 
                                                                                        selected = 0.5 ,
                                                                                        status = "success" ,size = "sm", 
                                                                                        direction = "horizontal", 
                                                                                        individual = TRUE ,
                                                                                        justified = T
                                                     ),
                                                     hr(),
                                                     
                                                     # ##  flip violin
                                                     # shinyWidgets::radioGroupButtons(
                                                     #   inputId = "violin_flip_xy",
                                                     #   label = h4(tags$b("Flip axis")),
                                                     #   choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                     #   selected = "FALSE" , status = "success" ,size = "sm", 
                                                     #   direction = "horizontal", 
                                                     #   individual = TRUE ,
                                                     #   justified = T,
                                                     #   width = "300px",
                                                     #   checkIcon = list(
                                                     #     yes = icon("ok", 
                                                     #                lib = "glyphicon"),
                                                     #     no = icon("remove",
                                                     #               lib = "glyphicon"))
                                                     # ),
                                                     # hr(),
                                                     
                                                     # ## facetting 
                                                     # shinyWidgets::radioGroupButtons(
                                                     #   inputId = "violin_facet_groups",
                                                     #   label = h4(tags$b("Separate groups")),
                                                     #   choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                     #   selected = "FALSE" , status = "success" ,size = "sm", 
                                                     #   direction = "horizontal",
                                                     #   individual = TRUE ,
                                                     #   justified = T, 
                                                     #   width = "300px",
                                                     #   checkIcon = list(
                                                     #     yes = icon("ok", 
                                                     #                lib = "glyphicon"),
                                                     #     no = icon("remove",
                                                     #               lib = "glyphicon"))
                                                     # ),
                                                     # hr(),
                                                     # 
                                                     
                                                     
                                                     ## alpha
                                                     sliderInput(
                                                       inputId = "violin_plot_alpha",
                                                       label = tags$h4(tags$b("Transparancy")),
                                                       min = 0, 
                                                       max = 1, value = 0.5, step = 0.01, width = "99%"
                                                     ),
                                                     hr()
                                                   )
                                                 ),
                                                 
                                                 ## export plot as image
                                                 column(
                                                   width = 3,
                                                   export_plot_ui(id = "export_violin_plot")
                                                 )
                                          ))
                                      )
                                    )
                                  )
                                ),
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## bar plot panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                tabPanel(
                                  title = "Bar plot", value = "barplot_panel", id = "barplot_panel",
                                  br(),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          shinyWidgets::pickerInput(
                                            inputId = "barplot_vars",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE
                                            )
                                          ),
                                          
                                          ## select genes to include in the bar plot.
                                          shinyWidgets::pickerInput(
                                            inputId = "barplot_select_genes",
                                            label = "Select genes(s)",
                                            choices = NULL, 
                                            choicesOpt = F,
                                            selected = "", 
                                            multiple = T, 
                                            width = "100%",
                                            options = list(
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              `live-search` = TRUE,
                                              style = "btn-success"
                                            )
                                          ),
                                          
                                          ## plot action
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_barplot" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                        )
                                      )
                                    )
                                    
                                  ),
                                  conditionalPanel(
                                    condition = "input.generate_barplot && output.barplot_status == true",
                                    shinyBS::bsCollapse(
                                      multiple = T, open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 2), ## empty column
                                          column(
                                            width = 8,
                                            plotOutput(outputId = "barplot", height = "auto", width = "auto")
                                          ),
                                          column(width = 2) ## empty column
                                        ),
                                        
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        ## bar plot advanced options
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        
                                        
                                        fluidRow(
                                          hr(),
                                          column(width = 12, 
                                                 column(
                                                   width = 6,
                                                   plot_title_and_axis_label_ui(id = "barplot_label_and_title")
                                                 ),
                                                 
                                                 ## 
                                                 column(
                                                   width = 3,
                                                   dropdownButton(
                                                     inputId = "barplot_options",
                                                     icon = icon("cogs"),
                                                     label = "Advance options",
                                                     size = "sm",
                                                     circle = F,
                                                     status = "success",
                                                     up = T,
                                                     right = T,
                                                     br(),
                                                     
                                                     
                                                     ## xaxis : genes, or sample 
                                                     shinyWidgets::radioGroupButtons(
                                                       inputId = "barplot_xaxis_choices",
                                                       label = h4(tags$b("X-axis")), 
                                                       choices = c("Genes" =  "GeneNames" ,"Samples" = "samples") ,
                                                       selected = "GeneNames" ,status = "success" ,size = "sm", 
                                                       direction = "horizontal", 
                                                       individual = TRUE ,
                                                       justified = T,
                                                       width = "400px",
                                                       checkIcon = list(
                                                         yes = icon("ok", 
                                                                    lib = "glyphicon"),
                                                         no = icon("remove",
                                                                   lib = "glyphicon")), 
                                                     ),
                                                     hr(),
                                                     
                                                     # bar plot fill bars
                                                     shinyWidgets::radioGroupButtons(
                                                       inputId = "fill_barplot",
                                                       label = h4(tags$b("Fill by")), 
                                                       choices = c("Genes" =  "GeneNames" ,"Samples" = "samples", 
                                                                   "Sample groups" = "groups" ,"Identical" = "identical") ,
                                                       selected = "samples" ,status = "success" ,size = "sm", 
                                                       direction = "horizontal", 
                                                       individual = TRUE ,
                                                       justified = T,
                                                       width = "400px",
                                                       checkIcon = list(
                                                         yes = icon("ok", 
                                                                    lib = "glyphicon"),
                                                         no = icon("remove",
                                                                   lib = "glyphicon"))
                                                     ),
                                                     
                                                     ## fill bars manually
                                                     conditionalPanel(
                                                       condition = "input.fill_barplot == 'identical'",
                                                       fluidRow(
                                                         column(
                                                           width = 4,
                                                           colourpicker::colourInput(
                                                             inputId = "barplot_color_chooser",
                                                             label = h4(tags$b("Select")),
                                                             value = "black",
                                                             returnName = TRUE,
                                                             palette = "limited"
                                                           )
                                                         )
                                                       )
                                                     ),
                                                     hr(),
                                                     
                                                     ## facetting 
                                                     shinyWidgets::radioGroupButtons(
                                                       inputId = "barplot_facet_value",
                                                       label = h4(tags$b("Separate by")), 
                                                       choices = c("None" = "none" , "Genes" =  "GeneNames" ,
                                                                   "Samples" = "samples", 
                                                                   "Sample groups" = "groups"),
                                                       selected = "none",status = "success" ,size = "sm", 
                                                       direction = "horizontal", 
                                                       individual = TRUE ,
                                                       justified = T,
                                                       width = "400px",
                                                       checkIcon = list(
                                                         yes = icon("ok", 
                                                                    lib = "glyphicon"),
                                                         no = icon("remove",
                                                                   lib = "glyphicon"))
                                                     ),
                                                     
                                                     hr(),
                                                     
                                                     ## Y scale during facetting 
                                                     shinyWidgets::radioGroupButtons(
                                                       inputId = "barplot_yscale",
                                                       label = h4(tags$b("Scale")), 
                                                       choices = c("Fix" = "fixed" , "Free" =  "free", 
                                                                   "Free X" = "free_x" , "Free Y" = "free_y"),
                                                       selected = "fixed",status = "success" ,size = "sm", 
                                                       direction = "horizontal", 
                                                       individual = TRUE ,
                                                       justified = T,width = "400px",
                                                       checkIcon = list(
                                                         yes = icon("ok", 
                                                                    lib = "glyphicon"),
                                                         no = icon("remove",
                                                                   lib = "glyphicon"))
                                                     ),
                                                     hr(),
                                                     
                                                     ## alpha
                                                     sliderInput(
                                                       inputId = "barplot_alpha",
                                                       label = h4(tags$b("Transparancy")), 
                                                       min = 0, 
                                                       max = 1,
                                                       value = 0.5, step = 0.01, width = "99%"
                                                     )
                                                   )
                                                 ),
                                                 
                                                 ## export plot as image
                                                 column(
                                                   width = 3,
                                                   export_plot_ui(id = "export_barplot")
                                                 )
                                          )
                                        )
                                      )
                                    )
                                  )
                                ),
                                
                                
                                
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## PCA plot tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                tabPanel(
                                  title = "PCA plot", value = "pca_plot_panel", id = "pca_plot_panel",
                                  br(),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          # PCA plot select variables
                                          shinyWidgets::pickerInput(
                                            inputId = "pca_plot_vars",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE , 
                                              selectOnTab = TRUE
                                            )
                                          ),
                                          
                                          
                                          gene_group_selection_ui("pca_plot_gene_group_selection"),
                                          
                                          ## plot action
                                          
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_pca_plot" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.generate_pca_plot && output.pca_plot_status == true",
                                    shinyBS::bsCollapse(
                                      multiple = T, open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          #column(width = 2), ## empty column
                                          column(
                                            width = 9,
                                            plotOutput(outputId = "pca_plot", 
                                                       brush = "pca_plot_brush", 
                                                       height = "auto", 
                                                       width = "auto") #%>% shinycssloaders::withSpinner(color = "#18BC9C")
                                          ),
                                          
                                          ### show / hide sample groups 
                                          column(width = 3 , 
                                                 
                                                 shinyWidgets::multiInput(width = "100%",
                                                                          inputId = "pca_plot_hide_sample_groups",
                                                                          label = "Show / Hide sample groups", 
                                                                          choices = "",
                                                                          
                                                 ) %>% shinycssloaders::withSpinner(color = "#18BC9C")
                                                 
                                          )
                                        ),
                                        
                                        
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        ## PCA plot advanced options
                                        ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                        
                                        fluidRow(
                                          hr(),
                                          # column(width = 1), ## empty column
                                          column(
                                            width = 12,
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui(id = "decorate_pca_plot")
                                            ),
                                            column(
                                              width = 3,
                                              
                                              ## PCA Advance options drop down menu.
                                              dropdownButton(
                                                inputId = "pca_plot_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = T,
                                                
                                                ## select PCs
                                                shiny::selectizeInput("select_pcs_to_plot", label="Select PCs",
                                                                      choices=NULL,
                                                                      multiple=TRUE ,
                                                                      options = list(maxItems = 2)),
                                                
                                                ## display / hide sample names 
                                                shinyWidgets::radioGroupButtons(inputId = "pca_display_sample_names", 
                                                                                label = tags$h4(tags$b("Disply sample names")) , 
                                                                                choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                                                selected = "FALSE",status = "success" ,
                                                                                size = "sm", 
                                                                                direction = "horizontal", individual = TRUE ,
                                                                                justified = T,
                                                                                width = "300px",
                                                                                checkIcon = list(
                                                                                  yes = icon("ok", 
                                                                                             lib = "glyphicon"),
                                                                                  no = icon("remove",
                                                                                            lib = "glyphicon"))
                                                ),
                                                hr(),
                                                ## PCA sample names size . 
                                                conditionalPanel(condition = "input.pca_display_sample_names == 'TRUE'" ,
                                                                 sliderInput(
                                                                   inputId = "pca_plot_sample_names_size",
                                                                   label = tags$h4(tags$b("Font size: Sample names")),
                                                                   min = 1,
                                                                   max = 10,
                                                                   value = 5,
                                                                   step = 0.05
                                                                 ),
                                                                 hr()
                                                ),
                                                
                                                
                                                ## PCA dot  size. 
                                                sliderInput(inputId = "pca_sample_dot_size" , 
                                                            label = tags$h4(tags$b("Dot size")),
                                                            min = 1,
                                                            max = 10,
                                                            value = 5,
                                                            step = 0.05),
                                                hr(),
                                                
                                                ## pca colors 
                                                shinyWidgets::radioGroupButtons(inputId = "pca_plot_colour" , 
                                                                                status = "success",
                                                                                size = "sm",
                                                                                direction = "horizontal",
                                                                                justified = T, 
                                                                                label = tags$h4(tags$b("Color by")) , 
                                                                                choices = c("Identical"="identical",
                                                                                            "Sample groups" = "groups" , 
                                                                                            "K-means"  = "kmeans") ,
                                                                                selected = "identical" , 
                                                                                width = "300px",
                                                                                checkIcon = list(
                                                                                  yes = icon("ok", 
                                                                                             lib = "glyphicon"),
                                                                                  no = icon("remove",
                                                                                            lib = "glyphicon"))
                                                ),
                                                hr(),
                                                
                                                ## color choices color identical 
                                                conditionalPanel(condition = "input.pca_plot_colour == 'identical' " ,
                                                                 colourpicker::colourInput(
                                                                   inputId = "pca_sample_name_color",
                                                                   label = tags$h4(tags$b("Select color")),
                                                                   value = "black",
                                                                   returnName = TRUE,
                                                                   palette = "limited"
                                                                 )
                                                ),
                                                
                                                
                                                ## color choices color identical 
                                                conditionalPanel(condition = "input.pca_plot_colour == 'kmeans' " ,
                                                                 shiny::numericInput(inputId = "pca_sample_sample_kmeans_n" , 
                                                                                     label = "Number of cluster" , 
                                                                                     value = 1 , step = 1, min = 1, max = 100)
                                                                 
                                                )
                                              )
                                            ),
                                            
                                            ## export plot as imagess
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_pca_plot")
                                              
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    
                                    ### Triggered actions
                                    
                                    ## set axis and plot titles
                                    
                                    shinyBS::bsModal(
                                      id = "pca_plot_edit_titles", title = "Title and labels", trigger = "trigger_pca_plot_edit_titles",
                                      ## plot titles
                                      h4(tags$b("Title")),
                                      textInputAddon(inputId = "pca_plot_title", label = "", value = "", addon = "Main :"),
                                      hr()
                                    )
                                  ),
                                  
                                  
                                  ### pca plot brushed points datatable 
                                  fluidRow(
                                    column(width = 12,
                                           conditionalPanel(
                                             condition = "output.pac_plot_brushed_data_table_status==true",
                                             shinyBS::bsCollapse(
                                               open = "Selected SRA points sample information (Does not apply to user uploaded data)",
                                               shinyBS::bsCollapsePanel(
                                                 title = "Selected SRA points sample information (Does not apply to user uploaded data)", style = "primary",
                                                 DT::dataTableOutput(outputId = "pca_brushed_datatable", width = "auto")  %>% shinycssloaders::withSpinner(color = "#18BC9C")
                                               )
                                             )
                                           )
                                    )
                                  )
                                ),
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## line plot tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                
                                tabPanel(
                                  title = "Line plot", value = "line_plot_panel", id = "line_plot_panel",
                                  
                                  br(),
                                  
                                  ## select plot varibale
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          
                                          ## select x variable
                                          shinyWidgets::pickerInput(
                                            inputId = "lineplot_x",
                                            label = "Select sample(s)",
                                            choices = NULL, choicesOpt = F,
                                            selected = "", multiple = T, width = "100%",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 10",
                                              style = "btn-success", `live-search` = TRUE
                                            )
                                            
                                          ),
                                          
                                          ## select line plot gene groups 
                                          gene_group_selection_ui(id = "line_plot_gene_group_selection"),
                                          
                                          hr(),
                                          
                                          
                                          fluidRow(
                                            column(width = 12,
                                                   ## line plot gene selection 
                                                   shinyWidgets::radioGroupButtons(
                                                     inputId = "lineplot_genes_selection",
                                                     label = tags$h4(tags$b("Genes to plot")),
                                                     choices = c("# top variable genes (By standard deviation)" = "top_variable_genes", "All genes" = "all"),
                                                     individual = T,
                                                     justified = T,
                                                     status = "success" , 
                                                     size = "sm" , 
                                                     checkIcon = list(
                                                       yes = icon("ok", 
                                                                  lib = "glyphicon"),
                                                       no = icon("remove",
                                                                 lib = "glyphicon"))
                                                   ),
                                                   
                                                   ## number of top variable genes for lineplot
                                                   conditionalPanel(
                                                     condition = "input.lineplot_genes_selection == 'top_variable_genes'",
                                                     numericInput(
                                                       inputId = "lineplot_top_n_genes",
                                                       label = tags$h4(tags$b("# top variable genes to show")),
                                                       value = 500, min = 50, max = 2000, step = 50 #width = "100%"
                                                     )
                                                   ),
                                                   hr(),
                                                   
                                                   ## line plot clustering 
                                                   shinyWidgets::radioGroupButtons(
                                                     inputId = "line_plot_cluster_genes_by",
                                                     label = tags$h4(tags$b("Gene cluster")),
                                                     choices = c("K-means" = "kmeans", 
                                                                 "Gene groups" = "gene_groups"),
                                                     individual = T,
                                                     justified = T,
                                                     status = "success" , 
                                                     size = "sm" , 
                                                     checkIcon = list(
                                                       yes = icon("ok", 
                                                                  lib = "glyphicon"),
                                                       no = icon("remove",
                                                                 lib = "glyphicon"))
                                                   ),
                                                   
                                                   ## line plot number of k-means cluster
                                                   conditionalPanel(
                                                     condition = "input.line_plot_cluster_genes_by == 'kmeans'",
                                                     numericInput(
                                                       inputId = "line_plot_nclust", 
                                                       label = tags$h4(tags$b("# of clusters (k-means)")), 
                                                       value = 5, min = 1, max = 10, step = 1 
                                                     )
                                                   ),
                                                   
                                                   hr(),
                                                   ## line plot choice for value to use in cluster 
                                                   fluidRow(
                                                     column(
                                                       width = 6,
                                                       shinyWidgets::radioGroupButtons(
                                                         inputId = "lineplot_cluster_value_type",
                                                         label = tags$h4(tags$b("Cluster by")), 
                                                         choices = c("Raw value" = "raw_val" ,
                                                                     "Z-Score" = "zscore"),
                                                         selected = "zscore", justified = T,
                                                         size = "sm" , 
                                                         status = "success",
                                                         width = "100%",
                                                         checkIcon = list(
                                                           yes = icon("ok", 
                                                                      lib = "glyphicon"),
                                                           no = icon("remove",
                                                                     lib = "glyphicon"))
                                                       )  
                                                     ),
                                                     
                                                     ## line plot choice of value to display 
                                                     column(
                                                       width = 6,
                                                       shinyWidgets::radioGroupButtons(
                                                         inputId = "lineplot_display_value_type",
                                                         label = tags$h4(tags$b("Display value")), 
                                                         choices = c("Raw value" = "raw" , "Z-Score" = "zscore"),
                                                         selected = "zscore",
                                                         size = "sm" ,  
                                                         justified = T, 
                                                         status = "success", 
                                                         width = "100%",
                                                         checkIcon = list(
                                                           yes = icon("ok", 
                                                                      lib = "glyphicon"),
                                                           no = icon("remove",
                                                                     lib = "glyphicon"))
                                                       )
                                                     )
                                                   ),
                                                   hr(),
                                                   ## all lines or average line
                                                   shinyWidgets::radioGroupButtons(inputId = "activate_avg_line_plot" ,
                                                                                   label = tags$h4(tags$b("Display lines for")) ,
                                                                                   choices = c("Individual gene" = "individual"   , "Average of all"= "average" ) , 
                                                                                   selected = "individual", 
                                                                                   size = "sm" , 
                                                                                   width = "100%" , 
                                                                                   justified = T, 
                                                                                   individual = T, 
                                                                                   checkIcon = list(
                                                                                     yes = icon("ok", 
                                                                                                lib = "glyphicon"),
                                                                                     no = icon("remove",
                                                                                               lib = "glyphicon")),
                                                                                   status = "success"),
                                                   
                                                   ## average line type : mean or median 
                                                   conditionalPanel(
                                                     "input.activate_avg_line_plot == 'average'",
                                                     
                                                     shinyWidgets::radioGroupButtons(inputId = "avg_line_type" ,
                                                                                     label = "Average type" ,
                                                                                     choices = c("Mean" = "mean", 
                                                                                                 "Median" = "median") , 
                                                                                     selected = "median", 
                                                                                     size = "sm" ,
                                                                                     width = "100%" , 
                                                                                     justified = T, 
                                                                                     individual = T, 
                                                                                     status = "success",
                                                                                     checkIcon = list(
                                                                                       yes = icon("ok", 
                                                                                                  lib = "glyphicon"),
                                                                                       no = icon("remove",
                                                                                                 lib = "glyphicon"))
                                                     )
                                                   )
                                            )
                                            
                                          ),
                                          ## plot action 
                                          hr(),
                                          column(width = 6 , offset = 5,
                                                 shinyWidgets::actionBttn(inputId = "generate_lineplot" ,
                                                                          label = "Plot" ,
                                                                          icon = icon("ruler-combined") ,
                                                                          style = "gradient",
                                                                          color = "success" ,
                                                                          size = "md",
                                                                          block = F)
                                          )
                                          
                                          
                                          
                                        )
                                      )
                                    )
                                  ),
                                  
                                  ## line plot plot panel 
                                  conditionalPanel(
                                    condition = "input.generate_lineplot && output.line_plot_status == true",
                                    shinyBS::bsCollapse(
                                      open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(
                                          column(width = 1), ## empty column
                                          ## display line  plot
                                          column(
                                            width = 10, #style = "overflow-y:auto;",
                                            plotOutput(outputId = "line_plot", height = "auto", width = "auto")
                                          ),
                                          column(width = 1) ## empty column
                                        ),
                                        
                                        ## line plot advance options
                                        fluidRow(
                                          hr(),
                                          
                                          ## line plot title and labels 
                                          column(
                                            width = 12,
                                            column(
                                              width = 6,
                                              plot_title_and_axis_label_ui(id = "decorate_line_plot")
                                            ),
                                            
                                            ## line plot Advance options 
                                            column(
                                              width = 3,
                                              dropdownButton(
                                                inputId = "line_plot_options",
                                                icon = icon("cogs"),
                                                label = "Advance options",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = T,
                                                
                                                
                                                ## line plot color choices  
                                                shinyWidgets::radioGroupButtons(
                                                  inputId = "line_plot_color_by",
                                                  label = tags$h4(tags$b("Color")),
                                                  choices = rlang::set_names(c("gene_groups","identical") , 
                                                                             c("Gene groups", "Identical")),
                                                  individual = T,
                                                  justified = T,
                                                  status = "success" , 
                                                  size = "sm" , 
                                                  width = "300px"
                                                ),
                                                
                                                ## line plot color indentical 
                                                conditionalPanel(
                                                  condition =  paste0("input.line_plot_color_by == 'identical'"),
                                                  fluidRow(
                                                    column(
                                                      width = 4,
                                                      colourpicker::colourInput(
                                                        inputId = "line_plot_color_chooser",
                                                        label = tags$h4(tags$b("Select")),
                                                        value = "black",
                                                        returnName = TRUE,
                                                        palette = "limited"
                                                      )
                                                    )
                                                  )
                                                ),
                                                
                                                hr(),
                                                
                                                ## line plot facet 
                                                shinyWidgets::radioGroupButtons(
                                                  inputId = "line_plot_separate_by",
                                                  label = tags$h4(tags$b("Separate by")),
                                                  choices = rlang::set_names(c("none" , "gene_groups") , 
                                                                             c("None" , "Gene groups")),
                                                  individual = T,
                                                  justified = T,
                                                  status = "success" , 
                                                  size = "sm" , 
                                                  width = "300px"
                                                ),
                                                hr(),
                                                
                                                ## scale y 
                                                conditionalPanel("input.line_plot_separate_by == 'gene_groups'",
                                                                 shinyWidgets::radioGroupButtons(inputId = "line_plot_facet_scale_free",
                                                                                                 label = "Scales" , 
                                                                                                 choices = c("Fix" ="fixed" , 
                                                                                                             "Free"  = "free",
                                                                                                             "Free X" ="free_x",
                                                                                                             "Free Y"  = "free_y"),
                                                                                                 selected = "fixed" ,
                                                                                                 status = "success" ,size = "sm", 
                                                                                                 direction = "horizontal", 
                                                                                                 individual = TRUE ,
                                                                                                 justified = T,
                                                                                                 checkIcon = list(
                                                                                                   yes = icon("ok", 
                                                                                                              lib = "glyphicon"),
                                                                                                   no = icon("remove",
                                                                                                             lib = "glyphicon"))
                                                                                                 
                                                                 ),
                                                                 hr()
                                                ),
                                                hr(),
                                                
                                                ## line plot splin shape 
                                                sliderInput(inputId = "line_plot_splin_shape" ,
                                                            label = tags$h4(tags$b("Line shape")) ,
                                                            min = -1,
                                                            max = 1,
                                                            step = 0.01 ,
                                                            value = 0 , 
                                                            width = "99%"),
                                                hr(),
                                                
                                                ## line plot line transparency 
                                                sliderInput(inputId = "line_plot_line_transparancy" ,
                                                            label = tags$h4(tags$b("Transparancy")) ,
                                                            min = 0,
                                                            max = 1,
                                                            step = 0.01 ,
                                                            value = 0.5 , 
                                                            width = "99%"),
                                                hr(),
                                                
                                                ## fix number of columns in the facetted ggplot.  
                                                numericInput(inputId = "line_plot_ncols",
                                                             label = tags$h4(tags$b("Number of columns")),
                                                             value = 3 , 
                                                             min = 1, 
                                                             max = 10, 
                                                             step = 1,
                                                             width = "100%"),
                                                hr(),
                                                
                                                ## line plot point size
                                                sliderInput(inputId = "lineplot_point_size" , 
                                                            label = "Point size" ,
                                                            min = 1, 
                                                            max = 10,
                                                            value = 2, 
                                                            step = 0.01),
                                                
                                                ## line plot line size
                                                sliderInput(inputId = "lineplot_line_size" ,
                                                            label = "Line size" , 
                                                            min = 1,
                                                            max = 10, 
                                                            value = 2, 
                                                            step = 0.01)
                                              )
                                              
                                              
                                            ),
                                            
                                            ## line plot export  
                                            column(
                                              width = 3,
                                              export_plot_ui(id = "export_line_plot")
                                            ) 
                                            
                                          )
                                          
                                        )
                                      )
                                    ),
                                    
                                    ## line plot data or functional analysis 
                                    radioGroupButtons(
                                      inputId = "lineplot_data_or_functional_analysis",
                                      choices = c("Show Data" = "show_lineplot_data", 
                                                  "Functional Analysis" =  "lineplot_functional_analysis"),
                                      status = "success",
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon")) , 
                                      size = "normal" ,
                                      direction = "horizontal" ,
                                      justified = TRUE , individual =  TRUE ,
                                      width = "100%", 
                                      selected = ""
                                      
                                    ),
                                    br(),
                                    
                                    ## show lineplot data
                                    conditionalPanel(
                                      condition = "input.lineplot_data_or_functional_analysis == 'show_lineplot_data'",
                                      shinyBS::bsCollapse(
                                        open = "Clustered data",
                                        shinyBS::bsCollapsePanel(
                                          title = "Clustered data", style = "primary",
                                          DT::dataTableOutput(outputId = "line_plot_clustred_data", width = "auto")%>% shinycssloaders::withSpinner(color = "#18BC9C")
                                        )
                                      )
                                    ),
                                    ## show lineplot functional analysis
                                    conditionalPanel(
                                      condition = "input.lineplot_data_or_functional_analysis == 'lineplot_functional_analysis'",
                                      shinyBS::bsCollapse(
                                        open = "Functional analysis",
                                        shinyBS::bsCollapsePanel(
                                          title = "Functional analysis", style = "primary",
                                          functional_analysis_ui(id = "lineplot_functional_analysis_ui")
                                        )
                                      )
                                    )
                                    
                                  ),
                                  
                                  ## triggered action
                                  shinyBS::bsModal(
                                    id = "line_edit_titles", title = "Edit titles", trigger = "trigger_line_edit_titles",
                                    ## plot titles
                                    
                                    textInput(inputId = "line_plot_title", label = "Plot Title", value = ""),
                                    hr(),
                                    textInput(inputId = "line_x_axis_title", label = "Axis title: X", value = ""),
                                    hr(),
                                    textInput(inputId = "line_y_axis_title", label = "Axis title: Y", value = "value")
                                  )
                                  
                                ),
                                
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                ## Heatmap  tab panel ----
                                ## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                                tabPanel(
                                  title = "Heatmap", value = "heatmap_panel", id = "heatmap_panel",
                                  br(),
                                  fluidRow(
                                    column(
                                      width = 12,
                                      shinyBS::bsCollapse(
                                        open = "Select plot variables",
                                        shinyBS::bsCollapsePanel(
                                          title = "Select plot variables", style = "primary",
                                          column(
                                            width = 12, style = "overflow-x:auto;",
                                            
                                            ## select heatmap variables
                                            shinyWidgets::pickerInput(
                                              inputId = "heatmap_vars",
                                              label = "Select sample(s)",
                                              choices = NULL, choicesOpt = F,
                                              selected = "", multiple = T, width = "100%",
                                              options = list(
                                                `actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 10",
                                                style = "btn-success", `live-search` = TRUE
                                              )
                                            ),
                                            
                                            
                                            ## heatmap select gene groups 
                                            gene_group_selection_ui(id = "heatmap_select_gene_groups"),
                                            hr(),
                                            
                                            ## top variable genes to plot in heatmap 
                                            shinyWidgets::radioGroupButtons(
                                              inputId = "hm_genes_selection",
                                              label = tags$h4(tags$b("Number of genes to plot")),
                                              choices = c("# top variable genes (By standard deviation)" = "top_variable_genes", "All genes" = "all"),
                                              selected = "top_variable_genes" ,
                                              status = "success" , 
                                              size = "normal", 
                                              direction = "horizontal",
                                              justified = T, individual = T ,
                                              checkIcon = list(
                                                yes = icon("ok", 
                                                           lib = "glyphicon"),
                                                no = icon("remove",
                                                          lib = "glyphicon"))
                                            ),
                                            
                                            
                                            conditionalPanel(
                                              condition = "input.hm_genes_selection == 'top_variable_genes'",
                                              # heatmap top n genes
                                              numericInput(
                                                inputId = "heatmap_top_n_genes",
                                                label = tags$h4(tags$b("# top variable genes to show")),
                                                value = 500, min = 50, max = 2000, step = 50 #width = "25%"
                                              )
                                            ),
                                            hr(),
                                            
                                            ## heatmap choice for value to use in cluster 
                                            fluidRow(
                                              column(
                                                width = 6,
                                                shinyWidgets::radioGroupButtons(
                                                  inputId = "heatmap_cluster_value_type",
                                                  label = tags$h4(tags$b("Cluster by")), 
                                                  choices = c("Raw value" = "raw_val" , "Z-Score" = "zscore"),
                                                  selected = "zscore",status = "success",
                                                  size = "normal",
                                                  direction = "horizontal" ,
                                                  justified = T , individual = T, width = "100%",
                                                  checkIcon = list(
                                                    yes = icon("ok", 
                                                               lib = "glyphicon"),
                                                    no = icon("remove",
                                                              lib = "glyphicon"))
                                                  
                                                )
                                              ),
                                              
                                              ## heatmap choice of value to plot 
                                              column(
                                                width = 6,
                                                shinyWidgets::radioGroupButtons(
                                                  inputId = "heatmap_display_value_type",
                                                  label = tags$h4(tags$b("Display value")), 
                                                  choices = c("Raw value" = "raw" , "Z-Score" = "zscore"),
                                                  selected = "zscore",
                                                  direction = "horizontal" ,
                                                  justified = T , 
                                                  individual = T,
                                                  width = "100%" , status = "success",
                                                  checkIcon = list(
                                                    yes = icon("ok", 
                                                               lib = "glyphicon"),
                                                    no = icon("remove",
                                                              lib = "glyphicon"))
                                                )
                                              )
                                              
                                            ),
                                            
                                            hr(),
                                            
                                            
                                            # heat map column row and legend options
                                            fluidRow(
                                              #style = "min-width:1200px",
                                              column(
                                                width = 4, style = "min-width:150px",
                                                
                                                # heatmap row options
                                                shinyBS::bsCollapse(
                                                  open = "Row options",
                                                  shinyBS::bsCollapsePanel(
                                                    title = "Row options", style = "primary",
                                                    
                                                    ## Row names"
                                                    shinyWidgets::radioGroupButtons(
                                                      inputId = "show_hm_row_names",
                                                      label = tags$h4(tags$b("Row names")),
                                                      choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                      selected  = FALSE , status = "success",
                                                      size = "sm", 
                                                      direction = "horizontal",
                                                      justified = T, 
                                                      individual = T,width = "100%",
                                                      checkIcon = list(
                                                        yes = icon("ok", 
                                                                   lib = "glyphicon"),
                                                        no = icon("remove",
                                                                  lib = "glyphicon"))
                                                    ),
                                                    
                                                    ## Font size : Row names
                                                    sliderInput(
                                                      width = "100%",
                                                      inputId = "hm_row_names_font_size",
                                                      label = tags$h4(tags$b("Row names font size")), 
                                                      min = 0.1,
                                                      max = 20,
                                                      value = 5, 
                                                      step = 0.1
                                                    ),
                                                    
                                                    ## Adjust height : Rows
                                                    # sliderInput(
                                                    #   width = "100%",
                                                    #   inputId = "hm_row_height",
                                                    #   label = tags$h4(tags$b("Row height")),
                                                    #   min = 0.1,
                                                    #   max = 50,
                                                    #   value = 0.1,
                                                    #   step = 0.1
                                                    # ),
                                                    
                                                    ## Row clusters
                                                    shinyWidgets::radioGroupButtons(
                                                      inputId = "heatmap_row_clusters_choices" ,
                                                      label = tags$h4(tags$b("Row cluster")),
                                                      choices = c("K-means" = "kmeans" , "Gene groups" ="gene_groups"),
                                                      selected = "kmeans",
                                                      status = "success",
                                                      size = "sm", 
                                                      direction = "horizontal",
                                                      justified = T, 
                                                      individual = T,
                                                      width = "100%",
                                                      checkIcon = list(
                                                        yes = icon("ok", 
                                                                   lib = "glyphicon"),
                                                        no = icon("remove",
                                                                  lib = "glyphicon"))
                                                    ),
                                                    
                                                    
                                                    ##  of clusters (k-means)
                                                    conditionalPanel(condition = "input.heatmap_row_clusters_choices  == 'kmeans'",
                                                                     numericInput(
                                                                       width = "100%",
                                                                       inputId = "heatmap_row_nclust", 
                                                                       label = tags$h4(tags$b("# of row clusters (k-means)")), 
                                                                       value = 1, 
                                                                       min = 1, 
                                                                       max = 10, 
                                                                       step = 1
                                                                     )  
                                                    ),
                                                    
                                                    ## row  cluster prefix 
                                                    shiny::textInput(inputId = "hm_cluster_prefix" ,
                                                                     label = tags$h4(tags$b("Row cluster label prefix")) , value = "Clust_" ,
                                                                     width = "100%" , placeholder = "Text will appear as row cluster name prefix" ),
                                                    
                                                    
                                                    
                                                    # Cluster rows (within the cluster)
                                                    shinyWidgets::radioGroupButtons(
                                                      inputId = "hm_cluster_rows", 
                                                      label = tags$h4(tags$b("Row cluster (within the cluster)")),
                                                      choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                      selected  = FALSE , 
                                                      status = "success",
                                                      size = "sm", 
                                                      direction = "horizontal",
                                                      justified = T, 
                                                      individual = T,
                                                      width = "100%",
                                                      checkIcon = list(
                                                        yes = icon("ok", 
                                                                   lib = "glyphicon"),
                                                        no = icon("remove",
                                                                  lib = "glyphicon"))
                                                    ),
                                                    
                                                    # # Cluster row slices  
                                                    # shinyWidgets::radioGroupButtons(
                                                    #   inputId = "hm_cluster_row_slice", 
                                                    #   label = tags$h4(tags$b("Arrange row cluster slice")),
                                                    #   choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                    #   selected  = TRUE , 
                                                    #   status = "success",
                                                    #   size = "sm", 
                                                    #   direction = "horizontal",
                                                    #   justified = T, 
                                                    #   individual = T,
                                                    #   width = "100%",
                                                    #   checkIcon = list(
                                                    #     yes = icon("ok", 
                                                    #                lib = "glyphicon"),
                                                    #     no = icon("remove",
                                                    #               lib = "glyphicon"))
                                                    # ),
                                                    # 
                                                    
                                                    
                                                    
                                                    ## show / hide row dendogram 
                                                    conditionalPanel(
                                                      condition = "input.hm_cluster_rows  == 'TRUE'",
                                                      shinyWidgets::radioGroupButtons(
                                                        inputId = "show_hm_row_dend",
                                                        label = tags$h4(tags$b("Row dendogram")),
                                                        choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                        selected  = FALSE , 
                                                        status = "success",
                                                        size = "sm", 
                                                        direction = "horizontal",
                                                        justified = T, 
                                                        individual = T,
                                                        width = "100%",
                                                        checkIcon = list(
                                                          yes = icon("ok", 
                                                                     lib = "glyphicon"),
                                                          no = icon("remove",
                                                                    lib = "glyphicon"))
                                                      )
                                                      
                                                    ),
                                                    
                                                    
                                                    # show border around cluster 
                                                    shinyWidgets::radioGroupButtons(
                                                      inputId = "hm_border", 
                                                      label = tags$h4(tags$b("Row cluster border")),
                                                      choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                      selected  = FALSE , 
                                                      status = "success",
                                                      size = "sm", 
                                                      direction = "horizontal",
                                                      justified = T, 
                                                      individual = T,
                                                      width = "100%",
                                                      checkIcon = list(
                                                        yes = icon("ok", 
                                                                   lib = "glyphicon"),
                                                        no = icon("remove",
                                                                  lib = "glyphicon"))
                                                    ),
                                                    
                                                    
                                                    ## std dev heatmap 
                                                    shinyWidgets::radioGroupButtons(
                                                      inputId = "show_std_dev_hm",
                                                      label = tags$h4(tags$b("Add standard deviation heatmap")),
                                                      choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                      selected  = FALSE , 
                                                      status = "success",
                                                      size = "sm", 
                                                      direction = "horizontal",
                                                      justified = T, 
                                                      individual = T,
                                                      width = "100%",
                                                      checkIcon = list(
                                                        yes = icon("ok", 
                                                                   lib = "glyphicon"),
                                                        no = icon("remove",
                                                                  lib = "glyphicon"))
                                                    ),
                                                    
                                                    ## sort by SD heatmap 
                                                    conditionalPanel(condition = "input.show_std_dev_hm == 'TRUE'" ,
                                                                     shinyWidgets::radioGroupButtons(
                                                                       inputId = "sort_hm_by_std_dev",
                                                                       label = tags$h4(tags$b("Sort by standard deviation")),
                                                                       choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                                       selected  = FALSE , 
                                                                       status = "success",
                                                                       size = "sm", 
                                                                       direction = "horizontal",
                                                                       justified = T, 
                                                                       individual = T,
                                                                       width = "100%",
                                                                       checkIcon = list(
                                                                         yes = icon("ok", 
                                                                                    lib = "glyphicon"),
                                                                         no = icon("remove",
                                                                                   lib = "glyphicon"))
                                                                     )               
                                                    )
                                                  )
                                                )
                                              ),
                                              column(
                                                width = 4, style = "min-width:150px",
                                                
                                                ## heatmap column options
                                                shinyBS::bsCollapse(
                                                  open = "Column options",
                                                  shinyBS::bsCollapsePanel("Column options",
                                                                  style = "primary",
                                                                  
                                                                  ## heatmap show / hide column names  
                                                                  shinyWidgets::radioGroupButtons(
                                                                    inputId = "show_hm_colum_names",
                                                                    label = tags$h4(tags$b("Column names")),
                                                                    choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                                    selected  = TRUE , 
                                                                    status = "success",
                                                                    size = "sm", 
                                                                    direction = "horizontal",
                                                                    justified = T, 
                                                                    individual = T,
                                                                    width = "100%",
                                                                    checkIcon = list(
                                                                      yes = icon("ok", 
                                                                                 lib = "glyphicon"),
                                                                      no = icon("remove",
                                                                                lib = "glyphicon"))
                                                                    
                                                                  ),
                                                                  
                                                                  ## heatmap change font size column names 
                                                                  sliderInput(
                                                                    inputId = "hm_column_names_font_size",
                                                                    label = tags$h4(tags$b("Column names font size")),
                                                                    min = 0.1,
                                                                    max = 20,
                                                                    value = 10, 
                                                                    step = 0.1,
                                                                    width = "100%"
                                                                  ),
                                                                  
                                                                  # ## adjust column width
                                                                  # sliderInput(
                                                                  #   inputId = "hm_col_width",
                                                                  #   label = tags$h4(tags$b("Column width")), 
                                                                  #   min = 0.1,
                                                                  #   max = 50,
                                                                  #   value = 0.5,
                                                                  #   step = 0.1,
                                                                  #   width = "100%"
                                                                  # ),
                                                                  
                                                                  
                                                                  ## Column clusters
                                                                  shinyWidgets::radioGroupButtons(
                                                                    inputId = "heatmap_column_clusters_choices" ,
                                                                    label = tags$h4(tags$b("Column cluster")),
                                                                    choices = c("K-means" = "kmeans" , "Sample groups" ="sample_groups"),
                                                                    selected = "kmeans",
                                                                    status = "success",
                                                                    size = "sm", 
                                                                    direction = "horizontal",
                                                                    justified = T, 
                                                                    individual = T,
                                                                    width = "100%",
                                                                    checkIcon = list(
                                                                      yes = icon("ok", 
                                                                                 lib = "glyphicon"),
                                                                      no = icon("remove",
                                                                                lib = "glyphicon"))
                                                                  ),
                                                                  
                                                                  ## number of column cluster                                                        
                                                                  conditionalPanel(
                                                                    condition = "input.heatmap_column_clusters_choices  == 'kmeans'",
                                                                    numericInput(
                                                                      width = "100%",
                                                                      inputId = "heatmap_coulm_nclust", 
                                                                      label = tags$h4(tags$b("# of column clusters (k-means)")), 
                                                                      value = 1, 
                                                                      min = 1, 
                                                                      max = 10, 
                                                                      step = 1
                                                                    )
                                                                  ),
                                                                  
                                                                  ## column cluster prefix 
                                                                  shiny::textInput(inputId = "hm_column_cluster_prefix" ,
                                                                                   label = tags$h4(tags$b("Column cluster label prefix")) , value = "Clust_" ,
                                                                                   width = "100%" , placeholder = "Text will appear as column cluster name prefix" ),
                                                                  
                                                                  ## heatmap cluster column with in the cluster 
                                                                  shinyWidgets::radioGroupButtons(
                                                                    inputId = "hm_cluster_columns",
                                                                    label = tags$h4(tags$b("Column cluster (within the cluster)")),
                                                                    choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                                    selected  = FALSE ,
                                                                    status = "success",
                                                                    size = "sm",
                                                                    direction = "horizontal",
                                                                    justified = T,
                                                                    individual = T,
                                                                    width = "100%",
                                                                    checkIcon = list(
                                                                      yes = icon("ok",
                                                                                 lib = "glyphicon"),
                                                                      no = icon("remove",
                                                                                lib = "glyphicon"))
                                                                  ),
                                                                  
                                                                  # # Cluster column  slices  
                                                                  # shinyWidgets::radioGroupButtons(
                                                                  #   inputId = "hm_cluster_column_slice", 
                                                                  #   label = tags$h4(tags$b("Arrange column cluster slice")),
                                                                  #   choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                                  #   selected  = TRUE , 
                                                                  #   status = "success",
                                                                  #   size = "sm", 
                                                                  #   direction = "horizontal",
                                                                  #   justified = T, 
                                                                  #   individual = T,
                                                                  #   width = "100%",
                                                                  #   checkIcon = list(
                                                                  #     yes = icon("ok", 
                                                                  #                lib = "glyphicon"),
                                                                  #     no = icon("remove",
                                                                  #               lib = "glyphicon"))
                                                                  # ),
                                                                  # 
                                                                  
                                                                  
                                                                  ## show / hide column dendogram 
                                                                  conditionalPanel("input.hm_cluster_columns == 'TRUE'",
                                                                                   shinyWidgets::radioGroupButtons(
                                                                                     inputId = "show_hm_column_dend",
                                                                                     label = tags$h4(tags$b("Column dendogram")),
                                                                                     choices = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                                                                     selected  = FALSE , 
                                                                                     status = "success",
                                                                                     size = "sm", 
                                                                                     direction = "horizontal",
                                                                                     justified = T, 
                                                                                     individual = T,
                                                                                     width = "100%",
                                                                                     checkIcon = list(
                                                                                       yes = icon("ok", 
                                                                                                  lib = "glyphicon"),
                                                                                       no = icon("remove",
                                                                                                 lib = "glyphicon"))
                                                                                   )                 
                                                                  ),
                                                                  
                                                                  
                                                                  
                                                                  ## heatmap top annotations 
                                                                  shinyWidgets::checkboxGroupButtons(
                                                                    inputId = "heatmap_anno_type",
                                                                    label = tags$h4(tags$b("Column annotation")),
                                                                    choices = c("Sample groups" = "show_heatmap_column_groups",
                                                                                "Boxplot" = "show_heatmap_column_boxplot") ,
                                                                    status = "success",
                                                                    size = "sm", 
                                                                    direction = "horizontal",
                                                                    justified = T, 
                                                                    individual = T,
                                                                    width = "100%",
                                                                    checkIcon = list(
                                                                      yes = icon("ok", 
                                                                                 lib = "glyphicon"),
                                                                      no = icon("remove",
                                                                                lib = "glyphicon"))
                                                                    
                                                                    
                                                                  ),
                                                                  
                                                                  ## heatmap top annot height 
                                                                  sliderInput(inputId = "heatmap_top_annot_height" , 
                                                                              label = tags$h4(tags$b("Column annotaion height")) ,
                                                                              min = 1 , max = 10, 
                                                                              step = 0.05 , value = 1 , width = "100%")
                                                                  
                                                  )
                                                )
                                              ),
                                              column(
                                                width = 4, style = "min-width:150px",
                                                
                                                # heatmap legend options
                                                shinyBS::bsCollapse(
                                                  open = "Legend options",
                                                  shinyBS::bsCollapsePanel("Legend options",
                                                                  style = "primary",
                                                                  textInput(inputId = "heatmap_legend_name", 
                                                                            label = tags$h4(tags$b("Legend name")),
                                                                            value = "Value", 
                                                                            width = "100%"),
                                                                  ## heatmap legend position
                                                                  radioGroupButtons(
                                                                    width = "100%",
                                                                    inputId = "heatmap_legend_pos",
                                                                    label = tags$h4(tags$b("Legend position")),
                                                                    individual = T,justified = T,
                                                                    choices = c("T" = "top", "B" = "bottom", "L" = "left", "R" = "right"), 
                                                                    selected = "right",status = "success" ,
                                                                    size = "sm",
                                                                    checkIcon = list(
                                                                      yes = icon("ok", 
                                                                                 lib = "glyphicon"),
                                                                      no = icon("remove",
                                                                                lib = "glyphicon"))
                                                                  ),
                                                                  ## heatmap legend direction
                                                                  radioGroupButtons(
                                                                    width = "100%",
                                                                    inputId = "heatmap_legened_direction",
                                                                    label = tags$h4(tags$b("Legend direction")), 
                                                                    individual = T,justified = T,
                                                                    choices = c("H" = "horizontal", "V" = "vertical"), 
                                                                    selected = "vertical",
                                                                    status = "success" , size = "sm",
                                                                    checkIcon = list(
                                                                      yes = icon("ok", 
                                                                                 lib = "glyphicon"),
                                                                      no = icon("remove",
                                                                                lib = "glyphicon"))
                                                                  ),
                                                                  br(),
                                                                  tags$h4(tags$b(("Heatmap Colors"))),
                                                                  br(),
                                                                  
                                                                  ## heatmap color : low
                                                                  column(
                                                                    width = 4,
                                                                    colourpicker::colourInput(
                                                                      inputId = "heatmap_col_low",
                                                                      label = tags$h4(tags$b("low")),
                                                                      value = "#004E63",
                                                                      returnName = TRUE
                                                                    )
                                                                  ),
                                                                  
                                                                  ## heatmap color : med
                                                                  column(
                                                                    width = 4,
                                                                    colourpicker::colourInput(
                                                                      inputId = "heatmap_col_medium",
                                                                      label = tags$h4(tags$b("med")),
                                                                      value = "white",
                                                                      returnName = TRUE
                                                                    )
                                                                  ),
                                                                  
                                                                  ## heatmap color : high
                                                                  column(
                                                                    width = 4,
                                                                    colourpicker::colourInput(
                                                                      inputId = "heatmap_col_high",
                                                                      label = tags$h4(tags$b("high")),
                                                                      value = "#BD1717",
                                                                      returnName = TRUE
                                                                    )
                                                                  ),
                                                                  br(),
                                                                  
                                                                  ### set legend minimum
                                                                  br(),
                                                                  tags$h4(tags$b(("Heatmap scale"))),
                                                                  br(),
                                                                  column(
                                                                    width = 6, 
                                                                    numericInput(inputId = "heatmap_scale_min" , 
                                                                                 label = tags$h4(tags$b("Minimum")), 
                                                                                 value = 0)
                                                                  ),
                                                                  ### set legend maximum
                                                                  column(width = 6,
                                                                         numericInput(inputId = "heatmap_scale_max" , 
                                                                                      label = tags$h4(tags$b("Maximum")) , 
                                                                                      value = 20 )
                                                                  )
                                                  )
                                                )
                                              )
                                            ),
                                            
                                            hr(),
                                            
                                            ## generate heatmap submit button. 
                                            column(width = 6 , offset = 5,
                                                   shinyWidgets::actionBttn(inputId = "generate_heatmap" ,
                                                                            label = "Plot" ,
                                                                            icon = icon("ruler-combined") ,
                                                                            style = "gradient",
                                                                            color = "success" ,
                                                                            size = "md",
                                                                            block = F)
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ),
                                  
                                  ## heatmap plot panel 
                                  conditionalPanel(
                                    condition = "input.generate_heatmap && output.heatmap_status == true",
                                    shinyBS::bsCollapse(
                                      open = "Plot",
                                      shinyBS::bsCollapsePanel(
                                        title = "Plot", style = "primary",
                                        fluidRow(style = "overflow-y:auto; max-height:2000px; position:auto;",
                                                 column(width = 1), ## empty column
                                                 
                                                 ## display heatmap
                                                 column(
                                                   width = 11, 
                                                   plotOutput(outputId = "heatmap", height = "auto", width = "auto") %>%
                                                     shinycssloaders::withSpinner(color = "#18BC9C")
                                                 ),
                                                 column(width = 1) ## empty column
                                        ),
                                        hr(),
                                        
                                        ## empty col
                                        column(width = 1),
                                        column(width = 10,
                                               column(width = 6,
                                                      ## adjust current heatmap height and width 
                                                      dropdownButton(
                                                        inputId = "plot_hm_drop_down",
                                                        icon = icon("resize-full", lib = "glyphicon"),
                                                        label = "Heatmap browser dimensions",
                                                        size = "sm",
                                                        circle = F,
                                                        status = "success",
                                                        up = T,
                                                        right = F,
                                                        
                                                        # Adjust height : Rows
                                                        sliderInput(
                                                          #width = "100%",
                                                          inputId = "hm_row_height",
                                                          label = tags$h4(tags$b("Row height")),
                                                          min = 0.1,
                                                          max = 50,
                                                          value = 0.1,
                                                          step = 0.1
                                                        ),
                                                        
                                                        ## adjust column width
                                                        sliderInput(
                                                          inputId = "hm_col_width",
                                                          label = tags$h4(tags$b("Column width")), 
                                                          min = 0.1,
                                                          max = 50,
                                                          value = 0.5,
                                                          step = 0.1,
                                                          #width = "100%"
                                                        )
                                                        
                                                      )
                                               ),
                                               
                                               column(width = 6,
                                                      ## export heatmap
                                                      export_base_graphics_ui(id = "export_heatmap")
                                               )
                                        ), ## empty column
                                        column(width = 1) ## empty column
                                      )
                                    ),
                                    
                                    ## show / hide heatmap data , perform functional analysis , wordcloud. 
                                    radioGroupButtons(
                                      inputId = "heatmap_data_or_functional_analysis",
                                      choices = c("Heatmap all data + gene annotation" = "show_heatmap_data", 
                                                  "Heatmap clusters"  = "show_heatmap_clusters",
                                                  "Functional analysis" =  "heatmap_functional_analysis" ,
                                                  "Sample information" = "display_sample_inforamtion"
                                      ),
                                      status = "success",
                                      checkIcon = list(
                                        yes = icon("ok", 
                                                   lib = "glyphicon")) , 
                                      size = "normal" ,
                                      direction = "horizontal" ,justified = TRUE , 
                                      individual =  TRUE ,
                                      width = "100%",selected = ""
                                      
                                    ),
                                    br(),
                                    
                                    ## display heatmap clusters 
                                    conditionalPanel(
                                      condition = "input.heatmap_data_or_functional_analysis == 'show_heatmap_clusters'",
                                      shinyBS::bsCollapse(
                                        open = "Heatmap clusters",
                                        shinyBS::bsCollapsePanel(
                                          title = "Heatmap clusters", style = "primary",
                                          
                                          ## heatmap cluster side :  row or column 
                                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                                              shinyWidgets::radioGroupButtons(inputId = "heatmap_cluster_type" ,
                                                                              label = "Cluster type" ,
                                                                              choices = c("Row side" = "show_hm_row_side_clusters" , 
                                                                                          "Column side" =  "show_hm_column_side_clusters") , 
                                                                              checkIcon = list(yes = icon("ok", 
                                                                                                          lib = "glyphicon")),
                                                                              selected = "show_hm_row_side_clusters" , 
                                                                              status = "success" ,size = "normal" ,
                                                                              direction = "horizontal" ,individual = T)
                                          ),
                                          
                                          ## heatmap cluster format :  wide or long 
                                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                                              shinyWidgets::radioGroupButtons(inputId = "heatmap_cluster_data_format" ,
                                                                              label = "Data format" ,
                                                                              choices = c("Wide" = "wide" , 
                                                                                          "Long" =  "long") ,
                                                                              checkIcon = list(yes = icon("ok", 
                                                                                                          lib = "glyphicon")),
                                                                              selected = "wide" , status = "success" ,size = "normal" ,
                                                                              direction = "horizontal" ,individual = T)
                                              
                                          ),
                                          hr(),
                                          ## hm cluster data output 
                                          DT::dataTableOutput(outputId = "heatmap_display_cluster_data", width = "auto") %>% shinycssloaders::withSpinner(color = "#18BC9C")
                                          
                                        )
                                      )
                                    ), 
                                    
                                    
                                    ## display heatmap expr mat 
                                    conditionalPanel(
                                      condition = "input.heatmap_data_or_functional_analysis == 'show_heatmap_data'",
                                      shinyBS::bsCollapse(
                                        open = "Heatmap all data + gene annotation",
                                        shinyBS::bsCollapsePanel(
                                          title = "Heatmap all data + gene annotation", style = "primary",
                                          DT::dataTableOutput(outputId = "heatmap_data", width = "auto")%>% shinycssloaders::withSpinner(color = "#18BC9C")
                                        )
                                      )
                                    ), 
                                    
                                    ## heatmap functional analysis 
                                    conditionalPanel(
                                      condition = "input.heatmap_data_or_functional_analysis == 'heatmap_functional_analysis'",  
                                      shinyBS::bsCollapse(
                                        open = "Functional analysis",
                                        shinyBS::bsCollapsePanel(
                                          title = "Functional analysis", style = "primary",
                                          functional_analysis_ui(id = "heatmap_functional_analysis_ui")
                                        )
                                      )
                                    ),
                                    
                                    ## heatmap sample information 
                                    conditionalPanel(condition = "input.heatmap_data_or_functional_analysis == 'display_sample_inforamtion' " , 
                                                     shinyBS::bsCollapse(
                                                       open = "Sample information",
                                                       shinyBS::bsCollapsePanel(
                                                         title = "Sample information", style = "primary",
                                                         fluidRow(
                                                           column(
                                                             width = 12,
                                                             cluster_wise_sample_information_ui("hm_sample_infor")
                                                           )
                                                         )
                                                       )
                                                     )
                                    )
                                  )
                                )
                              )
                            )
                          )
                 ),
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 ## Tutorial page ----
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 tabPanel(title = "Tutorial", icon = icon("question-circle"),
                          
                          tags$style(HTML("
              section.page-header {
                             display: none;
                             }      
                    
              section.main-content{
                            max-width: 1000px;
                            font-size: 100%;
                             }
             
              div#TOC{
                            max-width: 1024px;
                             font-size: 100%;
                             }
                             ")),
                          includeHTML(system.file("app","markdown_and_html","Tutorial.html" , package = "FungiExpresZ"))
                          #includeHTML(system.file( "markdown", "Tutorial.html" , package = "FungiExpresZ"))
                 ),
                 
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 ## Download page  ----
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 navbarMenu(title = "Downloads",
                            
                            
                            ## download GO data 
                            tabPanel("GO data",
                                     download_go_data_ui("download_go_data")
                            ),
                            
                            ## download gene expression data 
                            tabPanel("Gene expression data",
                                     
                                     download_gene_expression_matrix("download_gene_expression_data")
                                     
                                     
                            ),icon  = icon("download") ),
                 
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 ## News & Updates ----
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 tabPanel(title = "News & Updates", icon = icon("newspaper"),
                          
                          
                          tags$style(HTML("
              section.page-header {
                             display: none;
                             }      
                    
              section.main-content{
                            max-width: 1000px;
                            font-size: 100%;
                             }
             
              div#TOC{
                            max-width: 1024px;
                             font-size: 100%;
                             }
                             ")),
                          
                          #includeHTML("./markdown/newz_and_update.html")
                          includeHTML(system.file("app","markdown_and_html","newz_and_update.html" , package = "FungiExpresZ"))
                          
                 ),
                 
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 ## Citations page ----
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 
                 tabPanel(title = "Citations", icon = icon("stack-exchange")),
                 
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 ## github page ----
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 #tabPanel(title = "Github", icon = icon("github", "fa-2x")),
                 
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 ## contact page ----
                 ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
                 tabPanel(title = "Contact" ,icon = icon("envelope")
                 )
      ),
      
      ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      ## app footer ----
      ####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      hr(),
      
      tags$style(type='text/css', "#app_footer { text-align: center;}"),
      tags$div(id = "app_footer",tags$b("FungiExpresZ"), "is developed by ", tags$b("Chirag Parsania"), "from Chris Wong Lab at University of Macau. For any query or suggestions, please contact", a("Chirag Parsania", href = "mailto:chirag.parsania@gmail.com") ,"."),
      
      br(),
      
      ## display latest release
      
      tags$div(
        style = "text-align:center;",
        
        ## shinyapps.io release 
        # tags$a(img(src=badge_custom_url(x = "shinyapps.io" , y = "v0.0.2" ,col = "green" , 
        #                                 add_github_logo = FALSE)), 
        #        href="https://github.com/cparsania/FungiExpresZ/releases/tag/v0.0.2"),
        
        
        ## latest github relaese 
        tags$a(img(alt="GitHub release (latest by date)",
               src="https://img.shields.io/github/v/release/cparsania/fungiexpresz?logo=github"),
               href = "https://github.com/cparsania/FungiExpresZ/releases", target="_blank"),
        
        
        ## latest github devel
        tags$a(img(src= badge_custom_url(x = "devel" , 
                                         y = paste("v",badger::ver_devel("cparsania/fungiexpresz"),sep = "") , 
                                         col ="red" , add_github_logo = TRUE)) , 
               href = "https://github.com/cparsania/fungiexpresz", target="_blank")
      ),
      
      # fluidRow(
      #   
      #   column(width = 12,
      #          
      #         
      #          
      #          )
      # ),
      
      
      br(), br(), br(), br(), br(), br(), br(), br()
      
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'FungiExpresZ')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(ico = "www/favicon.ico" , rel = "shortcut icon"),
    golem::favicon(ico = "www/apple-touch-icon.png" , rel = "apple-touch-icon"),
    golem::favicon(ico = "www/favicon-32x32.png" , rel = "icon"),
    golem::favicon(ico = "www/favicon-16x16.png" , rel = "icon"),
    golem::favicon(ico = "www/site.webmanifest" , rel = "manifest"),
    tags$script(src="www/google_analytics.js")
    
    #tags$html("www/markdown/Tutorial.html"),
    #tags$html("www/markdown/newz_and_update.html")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    
    #tags$head(includeScript("www/google_analytics.js"))
  )
}
