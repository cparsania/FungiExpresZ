# Module UI
  
#' @title   plot_title_and_axis_label_ui and plot_title_and_axis_label_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_plot_title_and_axis_label
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
#' 
plot_title_and_axis_label_ui <- function(id) { 
  
  ns = NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6, 
        dropdownButton(
          inputId = ns("labels_and_title"),
          icon = icon("font"),
          label = "Labels & Title",
          size = "sm",
          circle = F,
          status = "success",
          up = T,
          right = T,
          
          ## Title
          h4(tags$b("Plot title")),
          textInputIcon(inputId = ns("plot_title"), label = "", value = "",placeholder = ""),
          
          sliderInput(
            inputId = ns("title_labs_size"), label = "Font size: Title",
            min = 1, max = 50, value = 15, step = 1
          ),
          hr(),
          
          ## Axis labels 
          h4(tags$b("Axis labels")),
          textInputIcon(inputId = ns("x_axis_title"), label = "", placeholder  = "X :"),
          textInputIcon(inputId = ns("y_axis_title"), label = "", placeholder  = "Y :"),
          
          ## Axis labels font size 
          # sliderInput(
          #   inputId = ns("x_y_labs_size"), label = "Font size: Axis Labels",
          #   min = 1, max = 30, value = 15, step = 1
          # ),
          
          sliderInput(
            inputId = ns("x_labs_size"), label = "Font size: X axis labels",
            min = 1, max = 50, value = 15, step = 1
          ),
          
          sliderInput(
            inputId = ns("y_labs_size"), label = "Font size: Y axis labels",
            min = 1, max = 50, value = 15, step = 1
          ),
          
          hr(),
          
          ## axis ticks 
          h4(tags$b("Axis ticks")),
          sliderInput(
            inputId = ns("x_tick_size"), label = "Font size: X tick labels",
            min = 1, max = 50, value = 15, step = 1
          ),
          sliderInput(
            inputId = ns("y_tick_size"), label = "Font size: Y tick labels",
            min = 1, max = 50, value = 15, step = 1
          ),
          hr(),
          
          ## Strip
          h4(tags$b("Strip")),
          sliderInput(
            inputId = ns("strip_text_size"), label = "Font size: Strip text",
            min = 1, max = 50, value = 10, step = 1
          )
          
        )
      ),
      
      ## Legend drop down 
      column(
        width = 6,
        dropdownButton(
          inputId = ns("legend"),
          icon = icon("elementor"),
          label = "Theme & Legend",
          size = "sm",
          circle = F,
          status = "success",
          up = T,
          right = T,width = "330px",
          
          ## themes
          shiny::selectInput(inputId = ns("select_theme") ,label = "Theme" , 
                             choices  = c("Grey"="theme_grey()",
                                          "BW"="theme_bw()",
                                          "Linedraw"="theme_linedraw()",
                                          "Light"="theme_light()",
                                          "Dark"="theme_dark()",
                                          "Minimal"="theme_minimal()",
                                          "Classic"="theme_classic()",
                                          "Test"="theme_test()"),
                             multiple = F, width = "100%" ,selected = "theme_bw()"),
          
          ## legend title 
          textInputIcon(inputId = ns("legend_title"), label = "Legend", placeholder = "Title :" , width = "100%"),
          
          ## legend title size 
          sliderInput(inputId = ns("legend_title_size") , 
                      label = "Font size: Legend title" ,
                      min = 1, max = 50, value = 15, step = 0.1,
                      width = "100%"),
          
          ## legend text size  
          sliderInput(inputId = ns("legend_text_size") , 
                      label = "Font size: Legend text" , 
                      min = 1, max = 50, value = 13, step = 0.1,
                      width = "100%"),
          
          ## legend key height 
          sliderInput(inputId = ns("legend_key_height") , 
                      label = "Height: Legend key" , 
                      min = 1, max = 50, value = 6, step = 0.1, 
                      width = "100%"),
          
          ## legend key width 
          sliderInput(inputId = ns("legend_key_width") ,
                      label = "Width: Legend key" , 
                      min = 1, max = 50, value = 6, step = 0.1, 
                      width = "100%"),
          
          ## legend position
          radioGroupButtons(
            inputId = ns("legend_pos"),
            label = "Legend position", individual = T,
            choices = c("N" = "none", "T" = "top", "B" = "bottom", "L" = "left", "R" = "right"),
            selected = "right", 
            status = "success" , 
            size = "sm" , 
            justified = TRUE,
            width = "100%",
            checkIcon = list(
              yes = icon("ok", 
                         lib = "glyphicon"),
              no = icon("remove",
                        lib = "glyphicon"))
          ),
          
          ## aspect ratio 
          
          sliderInput(inputId = ns("plot_aspect_ratio") , 
                      label = "Aspect ratio" ,
                      min = 0.1, max = 10, value = 1, step = 0.1,
                      width = "100%")
          
        )
      )
    )
    
    
  )
  
  
}
    
# Module Server
    
#' @param input internal
#' @param output internal 
#' @param session internal
#' @param plot_title internal
#' @param axis_x_title internal
#' @param axis_y_title internal
#' @param color_legend_title internal
#' @param fill_legend_title internal
#' @param my_ggplot internal
#' @param aspect_ratio internal
#' @param apply_theme internal
#' @param ... Other parameters passed to \code{decorate_ggplot} and ultimately \code{ggplot2::theme}
#'
#' @rdname mod_plot_title_and_axis_label
#' @keywords internal
plot_title_and_axis_label_server <- function(input, output, session, 
                                             plot_title = "", 
                                             axis_x_title = "" , 
                                             axis_y_title = "", 
                                             color_legend_title = NULL, 
                                             fill_legend_title = NULL, 
                                             my_ggplot, 
                                             aspect_ratio =  1, 
                                             apply_theme = TRUE, ...){
  req(my_ggplot)
  
  ## plot title 
  if(input$plot_title == ""){
    plot_title = plot_title
  }else{
    plot_title = input$plot_title
  }
  
  ## x axis title 
  if(input$x_axis_title == ""){
    axis_x_title = axis_x_title
  } else {
    axis_x_title = input$x_axis_title
  }
  
  ## y axis title 
  if(input$y_axis_title == ""){
    axis_y_title = axis_y_title
  } else {
    axis_y_title = input$y_axis_title
  }
  
  ## color legend title 
  if(input$legend_title == ""){
    color_legend_title = color_legend_title
  } else {
    color_legend_title = input$legend_title
  }
  
  ## fill legend title 
  if(input$legend_title == ""){
    fill_legend_title = fill_legend_title
  } else {
    fill_legend_title = input$legend_title
  }
  
  ## apply theme
  if(apply_theme){
    my_ggplot <- my_ggplot + eval(parse(text=input$select_theme))  
  }
  
  ## if aspect ratio is NULL means plot dosen't need is 
  if(!is.null(aspect_ratio)) {
    aspect_ratio = input$plot_aspect_ratio %>% as.numeric()
  }
  
  my_ggplot_decorated <- decorate_ggplot(
    gplot = my_ggplot,
    plot_title = plot_title,
    axis_x_title = axis_x_title,
    axis_y_title  = axis_y_title,
    #x_y_labs_size = input$x_y_labs_size,
    x_labs_size = input$x_labs_size,
    y_labs_size = input$y_labs_size,
    x_tick_size = input$x_tick_size,
    y_tick_size = input$y_tick_size,
    title_labs_size = input$title_labs_size,
    legend_text_size = input$legend_text_size,
    legened_title_size = input$legend_title_size,
    legend_key_height = input$legend_key_height,
    legend_key_width = input$legend_key_width,
    legend.position = input$legend_pos,
    legend.direction  =  input$legend_direction,
    color_legend_title = color_legend_title,
    fill_legend_title = fill_legend_title,
    strip_text_size = input$strip_text_size,
    aspect.ratio= aspect_ratio,
    #strip_text_size = 10,
    ...
  )
  
  
  
  return(my_ggplot_decorated)
  
}