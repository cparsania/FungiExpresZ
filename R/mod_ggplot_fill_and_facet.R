# Module UI
  
#' @title   ggplot_fill_and_facet_ui and ggplot_fill_and_facet
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_ggplot_fill_and_facet
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
ggplot_fill_and_facet_ui <- function(id){
  ns = NS(id)
  
  tagList(
    
    
    ## select x axis var type  
    shinyWidgets::radioGroupButtons(
      inputId = ns("x_var_type"),
      label = tags$h4(tags$b("X-axis")), 
      choices = c("Samples" = "samples", 
                  "Sample groups" = "sample_groups" ,
                  "Gene groups" = "gene_groups"
      ) ,
      selected = "samples" ,
      status = "success" ,
      size = "sm", 
      direction = "horizontal", 
      individual = TRUE ,
      justified = T,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon")) ,width = "450px"
    ),
    
    
    #  plot  fill choices 
    shinyWidgets::radioGroupButtons(
      inputId = ns("fill_plot"),
      label = tags$h4(tags$b("Fill by")), 
      choices = c("None" = "identical",
                  "Samples" = "samples", 
                  "Sample groups" = "sample_groups" ,
                  "Gene groups" = "gene_groups"
      ) ,
      selected = "samples" ,
      status = "success" ,
      size = "sm", 
      direction = "horizontal", 
      individual = TRUE ,
      justified = T,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon")) ,width = "450px"
    ),
    
    #  plot  fill manual  
    conditionalPanel(
      condition = paste0("input['",ns("fill_plot"),"'] == 'identical'"),
      fluidRow(
        column(
          width = 4,
          colourpicker::colourInput(
            inputId = ns("plot_color_chooser"),
            label = tags$h4(tags$b("Select")),
            value = "black",
            returnName = TRUE,
            palette = "limited"
          )
        )  
      )
    ),
    hr(),
    
    # facetting 
    shinyWidgets::radioGroupButtons(
      inputId = ns("separate_plot"),
      label = tags$h4(tags$b("Separate by")), 
      choices = c("None" = "none", 
                  "Samples" = "samples",
                  "Sample groups" = "groups" ,
                  "Gene groups" = "gene_groups"),
      selected = "none" ,status = "success" ,size = "sm", 
      direction = "horizontal", 
      individual = TRUE ,
      justified = T,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon")),width = "450px"
    ),
    hr(),
    
    # facetted plots  : scales 
    conditionalPanel(
      condition = paste0("input['",ns("separate_plot"),"'] != 'none'"),
      
      ## free scale options free_x or free_y
      fluidRow(
        column(
          width = 12,
          shinyWidgets::radioGroupButtons(inputId = ns("facet_scale_free"),
                                          label = "Scales" , 
                                          choices = c("Fix" ="fixed" , "Free"  = "free",
                                                      "Free X" ="free_x" , "Free Y"  = "free_y"),
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
                                          
          )
        )  
      ),
      
      hr()  
      
    )
    
    
  )
  
  
}
    
# Module Server
    
#' @param input session input
#'
#' @param output session output 
#' @param session session 
#' @param ggplot internal
#' @param allow_x_var_selection internal
#'
#' @rdname mod_ggplot_fill_and_facet
#' @keywords internal
ggplot_fill_and_facet <- function(input, output, session, ggplot , allow_x_var_selection = TRUE){
  
  observe({
    if(!allow_x_var_selection){
      shinyjs::hide(id = "x_var_type")
    }
  })
  
  gp <- ggplot
  
  ## NOTE: x_var_selection does not apply to density and histogram 
  ## select x var type 
  if(allow_x_var_selection){
    
    if(input$x_var_type == "sample_groups"){
      gp  <- gp + aes(x = groups)   
    } else if(input$x_var_type == "gene_groups"){
      gp  <- gp + aes(x = gene_groups)   
    } else if(input$x_var_type == "samples"){
      gp  <- gp + aes(x = samples)   
    }else{
      gp <- gp
    }
    
  }
  
  
  ## fill ggplot 
  if (input$fill_plot == "identical"){
    plot_x_var <- gp$data %>% 
      dplyr::pull(!!gp$mapping$x ) %>%   ## pull x var column 
      base::unique() %>% 
      as.character()
    
    colrs <- c(rep(input$plot_color_chooser,   length(plot_x_var) ))
    gp  <- gp +
      aes(fill = "black") + ## fake aesthatic 
      scale_fill_manual(breaks = plot_x_var , values = colrs)  
  } else if(input$fill_plot == "sample_groups"){
    gp  <- gp + aes(fill = groups)   
  } else if(input$fill_plot == "gene_groups"){
    gp  <- gp + aes(fill = gene_groups)   
  } else if(input$fill_plot == "samples"){
    gp  <- gp + aes(fill = samples)   
  }else{
    gp <- gp
  }
  
  ##separate ggplot 
  if(input$separate_plot  != "none" ){
    gp <- gp + facet_wrap(as.symbol(input$separate_plot) ,
                          scales = input$facet_scale_free)
  }
  return(list(plot = gp , fill_var = input$fill_plot ))  
}    

