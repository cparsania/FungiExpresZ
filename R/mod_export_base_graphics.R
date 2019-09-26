# Module UI
  
#' @title   export_base_graphics_ui and export_base_graphics
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param file_name
#' @param plot
#' @param legend_pos
#' @param isComplexHeatmap
#'
#' @rdname mod_export_base_graphics
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
export_base_graphics_ui <- function(id){
  ns  <- NS(id)
  dropdownButton(
    inputId = ns("plot_export_drop_down"),
    icon = icon("export", lib = "glyphicon"),
    label = "Export plot",
    size = "sm",
    circle = F,
    status = "success",
    up = T,
    right = F,
    
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
    
    
    fluidRow(
      column(width = 3 , offset = 1,
             shinyWidgets::downloadBttn(outputId =  ns("dowload_png") , label = "png",
                                        style = "bordered",
                                        color = "success"
             )
      ),
      
      column(width = 3 ,
             shinyWidgets::downloadBttn(outputId =  ns("dowload_pdf") , label = "pdf",
                                        style = "bordered",
                                        color = "success"
             )
      ),
      
      column(width = 3 ,
             shinyWidgets::downloadBttn(outputId =  ns("dowload_svg") , label = "svg",
                                        style = "bordered",
                                        color = "success"
             )  
      )
    )
  )
  
}    
# Module Server
    
#' @rdname mod_export_base_graphics
#' @export
#' @keywords internal
    
export_base_graphics = function(input, output, session , file_name, plot ,
                                legend_pos = reactive({"top"}) , 
                                isComplexHeatmap = TRUE){
  
  output$dowload_png <- downloadHandler(
    filename = function() { paste(file_name, "png", sep = ".") },
    content = function(file) {
      png(filename = file, height = input$adjust_height , width = input$adjust_width ,units = "in" ,res = 108)
      if(isComplexHeatmap){
        ComplexHeatmap::draw(plot() , heatmap_legend_side = legend_pos())  
      }else{
        print(plot())
      }
      dev.off()
    }
  )
  
  output$dowload_pdf <- downloadHandler(
    filename = function() { paste(file_name, "pdf", sep = ".") },
    content = function(file) {
      pdf(file = file,  height = input$adjust_height , width = input$adjust_width)
      if(isComplexHeatmap){
        ComplexHeatmap::draw(plot() , heatmap_legend_side = legend_pos())
      }
      else{
        print(plot())
      }
      dev.off()
    }
  )
  
  output$dowload_svg <- downloadHandler(
    filename = function() { paste(file_name, "svg", sep = ".") },
    content = function(file) {
      svg(filename = file, width  = input$adjust_width , height = input$adjust_height)
      if(isComplexHeatmap){
        ComplexHeatmap::draw(plot() , heatmap_legend_side = legend_pos())
      } else{
        print(plot())
      }
      dev.off()
    }
  )
  
  
}
    
