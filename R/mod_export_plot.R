# Module UI
  
#' @title   export_plot_ui export_plot
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param file_name
#' @param plot
#'
#' @rdname mod_export_plot
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 

export_plot_ui <- function(id){
  ns  <- NS(id)
  
  dropdownButton(
    inputId = ns("plot_export_drop_down"),
    icon = icon("export", lib = "glyphicon"),
    label = "Export plot",
    size = "sm",
    circle = F,
    status = "success",
    up = T,
    right = T,
    
    sliderInput(
      inputId = ns("adjust_width"),
      label = "Adjust width (inch):", 
      min = 1,
      max = 40, 
      step = 0.1, 
      value = 10
      
    ),
    br(),
    sliderInput(
      inputId = ns("adjust_height"),
      label = "Adjust height (inch):", 
      min = 1,
      max = 40, 
      step = 0.1, 
      value = 10
    ),
    
    tags$hr(),
    
    fluidRow(
      column(width = 4,
             shinyWidgets::downloadBttn(outputId =  ns("dowload_png") , label = "png",
                                        style = "bordered",
                                        color = "success"
             )
      ),
      
      column(width = 4 ,
             shinyWidgets::downloadBttn(outputId =  ns("dowload_pdf") , label = "pdf",
                                        style = "bordered",
                                        color = "success"
             )
      ),
      
      column(width = 4 ,
             shinyWidgets::downloadBttn(outputId =  ns("dowload_svg") , label = "svg",
                                        style = "bordered",
                                        color = "success"
             )  
      )
    )
  ) 
  
}    


# Module Server
    
#' @rdname mod_export_plot
#' @export
#' @keywords internal
    
export_plot <- function(input, output, session , file_name, plot){
  
  output$dowload_png <- downloadHandler(
    filename = function() { paste(file_name, "png", sep = ".") },
    content = function(file) {
      ggsave(filename = file, plot = plot(), device = "png" , width = input$adjust_width , height = input$adjust_height) 
    }
  )
  
  output$dowload_pdf <- downloadHandler(
    filename = function() { paste(file_name, "pdf", sep = ".") },
    content = function(file) {
      ggsave(file, plot = plot(), device = "pdf" , width = input$adjust_width , height = input$adjust_height)
    }
  )
  
  output$dowload_svg <- downloadHandler(
    filename = function() { paste(file_name, "svg", sep = ".") },
    content = function(file) {
      ggsave(file, plot = plot(), device = "svg" , width = input$adjust_width , height = input$adjust_height)
    }
  )
  
  
}

