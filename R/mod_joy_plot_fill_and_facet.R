# Module UI
  
#' @title   joy_plot_fill_and_facet_ui and joyplot_fill_and_facet
#' @description  A shiny Module.
#' 
#' 
#' @param id shiny id
#'
#' @rdname mod_joy_plot_fill_and_facet
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
joy_plot_fill_and_facet_ui <- function(id){
  ns = NS(id)
  
  tagList(
    
    ## x var type 
    shinyWidgets::radioGroupButtons(
      inputId = ns("x_var_type"),
      label = tags$h4(tags$b("Y-axis")), 
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
                  lib = "glyphicon")) ,width = "100%"
    ),
    
    
    ## joy plot fill options
    
    shinyWidgets::radioGroupButtons(
      inputId = ns("fill_plot"),
      label = tags$h4(tags$b("Fill by")), 
      choices = c("Value" = "value", 
                  "Probability" = "probability", 
                  "Quantile" = "quantile" ,
                  "Alternative" =  "alternate",
                  "Samples" = "samples", 
                  "Sample groups" = "sample_groups" ,
                  "Gene groups" = "gene_groups"
      ) ,
      selected = "value" ,
      status = "success" ,
      size = "sm", 
      direction = "vertical",
      individual = TRUE ,
      justified = T,
      #direction = "verticle",
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon")) ,width = "100%"
    ),
    
    ## joy plot fill choices 
    conditionalPanel(paste0("input['",ns("fill_plot"),"'] != 'alternate'"),
                     
                     radioGroupButtons(
                       inputId = ns("plot_fill_choice"),
                       label = tags$h4(tags$b("Fill choices")) , individual = T,
                       choices = c("A" = "A", "B" = "B", "C" = "C", "D" = "D", "E" = "E"),
                       selected = "C" , 
                       status = "success" , 
                       size = "sm",
                       width = "100%" ,
                       justified = T,
                       checkIcon = list(
                         yes = icon("ok", 
                                    lib = "glyphicon"),
                         no = icon("remove",
                                   lib = "glyphicon"))
                     )
    ),
    
    #fill alternate 
    conditionalPanel(paste0("input['",ns("fill_plot"),"'] == 'alternate'"),  
                     colourpicker::colourInput(
                       inputId = ns("fill_alt_1"),
                       label = tags$h4(tags$b("First")),
                       value = "blue",
                       returnName = TRUE,
                       palette = "limited"
                     ) , 
                     
                     colourpicker::colourInput(
                       inputId = ns("fill_alt_2"),
                       label = tags$h4(tags$b("Second")) ,
                       value = "green",
                       returnName = TRUE,
                       palette = "limited"
                     )
                     
                     
    ),
    
    # Fill reverse
    shinyWidgets::radioGroupButtons(
      inputId = ns("plot_fill_direction"),
      label = tags$h4(tags$b("Fill reverse")) , 
      choices = c("TRUE" = TRUE, "FALSE" =  FALSE),
      individual = T, selected = "FALSE",
      status = "success" , 
      size = "sm",
      width = "100%" , 
      justified = T,
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"),
        no = icon("remove",
                  lib = "glyphicon"))
    ),
    
    
    #  plot ridge color
    colourpicker::colourInput(
      inputId = ns("plot_ridges_col"),
      label = tags$h4(tags$b("Color ridges")),
      value = "black",
      returnName = TRUE,
      palette = "limited"
    ),
    
    
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
                  lib = "glyphicon")),width = "100%"
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
#' @param joyplot a joy plot object 
#'
#' @rdname mod_joy_plot_fill_and_facet
#' @keywords internal
joyplot_fill_and_facet <- function(input, output, session, joyplot){
  
  joyplot <- joyplot 
  
  ## xvar selection 
  
  ## select x var type 
  if(input$x_var_type == "sample_groups"){
    joyplot  <- joyplot + aes(y = groups)   
  } else if(input$x_var_type == "gene_groups"){
    joyplot  <- joyplot + aes(y = gene_groups)   
  } else if(input$x_var_type == "samples"){
    joyplot  <- joyplot + aes(y = samples)   
  }else{
    joyplot <- joyplot
  }
  
  ## fill ggplot 
  if(input$fill_plot == "sample_groups"){
    joyplot  <- joyplot + 
      ggridges::stat_density_ridges(geom = "density_ridges_gradient" , 
                          mapping = aes( fill = groups) , 
                          col = input$plot_ridges_col , scale = 1 ) + 
      viridis::scale_fill_viridis(name = input$fill_plot , 
                         option = input$plot_fill_choice , 
                         direction = ifelse(input$plot_fill_direction , -1,1), 
                         discrete = T)
    
  } else if(input$fill_plot == "samples"){
    print(input$plot_fill_choice)
    print(input$plot_fill_direction)
    joyplot  <- joyplot + 
      ggridges::stat_density_ridges(geom = "density_ridges_gradient" , 
                          mapping = aes( fill = samples) , 
                          col = input$plot_ridges_col, scale = 1 ) + 
      viridis::scale_fill_viridis(name = input$fill_plot , 
                         option = input$plot_fill_choice , 
                         direction = ifelse(input$plot_fill_direction , -1,1), 
                         discrete = T)
    
  } else if(input$fill_plot == "gene_groups"){
    print(input$plot_fill_choice)
    print(input$plot_fill_direction)
    joyplot  <- joyplot + ggridges::stat_density_ridges(geom = "density_ridges_gradient" , 
                                              mapping = aes( fill = gene_groups) , 
                                              col = input$plot_ridges_col, scale = 1 ) + 
      viridis::scale_fill_viridis(name = input$fill_plot , 
                         option = input$plot_fill_choice , 
                         direction = ifelse(input$plot_fill_direction , -1,1), 
                         discrete = T)
    
    
  }else if(input$fill_plot == "probability"){
    joyplot <- joyplot  + 
      ggridges::stat_density_ridges(geom = "density_ridges_gradient" , 
                          mapping = aes( fill = 0.5 - abs(0.5 - ..ecdf..)) , 
                          calc_ecdf = TRUE , 
                          col = input$plot_ridges_col, scale = 1 ) + 
      viridis::scale_fill_viridis(name = input$fill_plot , 
                         option = input$plot_fill_choice , 
                         direction = ifelse(input$plot_fill_direction , -1,1))
    
    
  } else if (input$fill_plot == "value"){
    joyplot <- joyplot  + 
      ggridges::stat_density_ridges(geom = "density_ridges_gradient" , 
                          mapping = aes(fill = ..x..) , 
                          col = input$plot_ridges_col, scale = 1 ) + 
      viridis::scale_fill_viridis(name = input$fill_plot , 
                         option = input$plot_fill_choice , 
                         direction = ifelse(input$plot_fill_direction , -1,1))
  } else if(input$fill_plot == "alternate") {
    joyplot <- joyplot  + 
      ggridges::stat_density_ridges(geom = "density_ridges_gradient" , 
                          mapping = aes(fill = !!joyplot$mapping$y) , ## fake aesthatic
                          col = input$plot_ridges_col , scale = 1 ) + 
      scale_fill_cyclical(values  = c(input$fill_alt_1 , input$fill_alt_2))
    
  } else if(input$fill_plot == "quantile"){
    print(input$plot_fill_choice)
    print(input$plot_fill_direction)
    joyplot <- joyplot +
      ggridges::stat_density_ridges(geom = "density_ridges_gradient" , 
                          mapping = aes(fill = ..quantile..) , 
                          col = input$plot_ridges_col , 
                          calc_ecdf = TRUE , 
                          quantiles = 4, 
                          quantile_lines = T, scale = 1 ) + 
      viridis::scale_fill_viridis(name = input$fill_plot , 
                         option = input$plot_fill_choice , 
                         direction = ifelse(input$plot_fill_direction , -1,1), 
                         discrete = T)
  }else{
    joyplot <- joyplot
  }
  
  ##separate ggplot 
  if(input$separate_plot  != "none" ){
    joyplot <- joyplot + facet_wrap(as.symbol(input$separate_plot) ,
                                    scales = input$facet_scale_free)
  }
  
  return(list(plot = joyplot , fill_var = input$fill_plot ))  
  
  
}
    
