# Module UI
  
#' @title   ggplot_fill_colour_ui and ggplot_fill_colour
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param fill_attribute
#' @param input internal
#' @param output internal
#' @param session internal
#' @param gp
#' @param times
#'
#' @rdname mod_ggplot_fill_colour
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
ggplot_fill_colour_ui <- function(id , fill_attribute  ="Samples"){
  ns  <- NS(id)
  
  tagList(
    
    ## fill box
    shinyWidgets::radioGroupButtons(
      inputId = ns("fill_color_identical"),
      label = tags$h4(tags$b("Color by")),
      #choices = c("By variable" = "by_var", "Identical" = "identical")
      choices = rlang::set_names(c("by_var" , "groups" ,"identical") , c(fill_attribute  ,"By sample groups" , "Identical")),
      individual = T,
      justified = T,
      status = "success" , 
      size = "sm" , 
      width = "300px"
    ),
    
    ## fill box manually
    conditionalPanel(
      #condition = "input.fill_color_identical == 'identical'",
      condition =  paste0("input['",ns("fill_color_identical"),"'] == 'identical' "),
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
    ## transparency 
    sliderInput(inputId = ns("set_transparancy") ,label = tags$h4(tags$b("Transparancy")) ,min = 0,max = 1, step = 0.01 ,value = 0.5 , width = "99%")
    
    
    
  )
  
}
    
# Module Server
    
#' @rdname mod_ggplot_fill_colour
#' @export
#' @keywords internal
    
ggplot_fill_colour <- function(input, output, session, gp , times ){
  
  # black and white
  if (input$fill_color_identical == "identical") {
    
    colrs <- c(rep(input$plot_color_chooser, times))
    
    gp <- gp + 
      scale_fill_manual(values = colrs) +
      scale_color_manual(values = colrs) + 
      scale_alpha(range = input$set_transparancy) + ## override alpha in the plot 
      guides(alpha = FALSE , 
             color = guide_legend(override.aes = list(alpha = input$set_transparancy)))+ ## override alpha in the legend
      theme(legend.position = "none") 
    
  } else {
    gp <- gp + scale_alpha(range = input$set_transparancy) + ## override alpha in the plot 
      guides(alpha = FALSE , 
             fill = guide_legend(override.aes = list(alpha = input$set_transparancy)))
  }
  
  return(gp)
  
}
    
