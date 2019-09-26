# Module UI
  
#' @title   word_cloud_ui and word_cloud_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param parent_id 
#' @param input_text
#' 
#' @rdname mod_word_cloud
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 


word_cloud_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    ## word cloud plot output
    conditionalPanel(
      condition =  
        paste0("output['",ns("wc_plot_status"),"']"),
      
      fluidRow(style = "overflow-y:scroll;overflow-x:scroll",
               column(width = 12,
                      div(style="height:1000px",
                          plotOutput(outputId = ns("wordcloud_plot")) %>% 
                            withSpinner(color = "#18BC9C")  
                      )
                      
               )
      ),
      hr(),
      fluidRow(
        column(
          width = 12, 
          
          ## word cloud advance settings 
          column(width = 6,
                 shinyWidgets::dropdownButton(  inputId = ns("advance_settings"),
                                                icon = icon("font"),
                                                label = "Advance settings",
                                                size = "sm",
                                                circle = F,
                                                status = "success",
                                                up = T,
                                                right = F,
                                                
                                                ## wc colors
                                                shiny::selectInput(inputId = ns("wc_color_palatte") ,label = "Color palette" , 
                                                                   choices = c("Accent"="Accent",
                                                                               "Dark2" = "Dark2",
                                                                               "Paired" = "Paired",
                                                                               "Pastel1" ="Pastel1",
                                                                               "Pastel2" ="Pastel2",
                                                                               "Set1" = "Set1",
                                                                               "Set2" = "Set2",
                                                                               "Set3" = "Set3"),
                                                                   multiple = F, width = "100%" ,
                                                                   selected = "Paired"),
                                                ## max words in wc 
                                                shiny::sliderInput(inputId = ns("wc_max_words") ,
                                                                   label = "Max words" ,
                                                                   min = 10,
                                                                   max = 500 ,
                                                                   value = 200 , 
                                                                   step = 1)
                                                
                 )
          ),
          
          # word cloud plot export
          column(width = 6,
                 export_base_graphics_ui(id = ns("export_wordcloud"))
          )
          
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_word_cloud
#' @export
#' @keywords internal

word_cloud_server <- function(input, output, session ,parent_id, input_text){
  
  ## user defined set of words to remove from word cloud 
  specific_words <- reactive({
    specific_words <- c("gene", "genes","expression" ,
                        "expressed" ,"cell","cellular",
                        "used","nidulans" ,
                        "strain","aspergillus",
                        "transcription",
                        "transcriptome" ,"revealed" ,"rnaseq" ,"use",
                        "regul" ,"rna" , "type" , "function" , 
                        "conditions" ,"design","role","cells","data" ,
                        "analysis" , "transcriptional" , "seq" , "biological" ,
                        "major" ,"mechanisms","sequencing",
                        "candida","albicans" ,"glabrata" ,"illumina" ,
                        "pathogen","fungal","involved" ,"study" ,
                        "fungi" , "fungus" , "model"  ,
                        "total", "cerevisiae" ,"species" ,"strains" ,
                        "specific" ,"samples","genome" ,"genomewide")       
  })
  
  ## clean text 
  docs <- reactive({
    validate(
      need(!is.null(input_text()) , message = "No abstract available for selected cluster.")
    )
    text_to_clean_document(text = input_text() , 
                           specific_words = specific_words() , 
                           remove_numbers = F, 
                           remove_stop_words = T, 
                           remove_specific_words = T , 
                           remove_punctuation = T, 
                           text_stemming = F)
  })
  
  word_fq_df <- reactive({
    dtm <- tm::DocumentTermMatrix(docs())
    
    ## normalize word between docs 
    ## logic :: given the fq of a word across document , below function devide fq by (total occurances * total docs )  * number of documents in it appears    
    normalize_between_docs <- as_mapper(~  (.x /(sum(.) * length(.x)) ) * (.x > 0) %>% sum(.) )
    
    ## fq matrix  
    m <- as.matrix(dtm) %>% 
      as.data.frame() %>%
      as_tibble() 
    
    ## perform normalization
    # if(TRUE){
    #   m <- m %>%
    #     mutate_if(is.numeric , normalize_between_docs)
    # 
    # }
    
    ## convert matrix   
    m <- m %>% 
      as.matrix() %>% 
      t() 
    
    ## perform  row wide sum to get total fq of each word 
    v <- sort(rowSums(m),decreasing=TRUE)
    
    ## back to df 
    d <- data.frame(word = names(v),freq=v)
    return(d)
  })
  
  ## wc plot function 
  get_wc_plot  = function(word, freq,palatte,max_words) {
    wordcloud(words = word, freq = freq,
              fixed.asp = FALSE,
              min.freq = 0.1,
              max.words=max_words,
              random.order=FALSE,
              rot.per=0,
              colors=brewer.pal(8, palatte))
  }
  
  ## render wc plot 
  output$wordcloud_plot <- renderPlot({
    req(word_fq_df())
    
    set.seed(1234)
    
    ## df to tibble 
    d <- word_fq_df() %>% 
      as_tibble() 
    
    ## plot WC 
    get_wc_plot(d$word , d$freq , palatte = input$wc_color_palatte , max_words = input$wc_max_words)
    
    
  }, res = 96,
  height = function() {
    req(word_fq_df())
    wc_client_height_objct = paste("output_" , parent_id, "-wordcloud_plot_height",sep="")
    wc_client_height = session$clientData[[wc_client_height_objct]]
    wc_client_height = wc_client_height * 3
    return(wc_client_height) ## dynamic height
    
  },
  width = function() {
    req(word_fq_df())
    wc_client_width_objct = paste("output_" , parent_id, "-wordcloud_plot_width",sep="")
    wc_client_width = session$clientData[[wc_client_width_objct]]
    return(wc_client_width) ## dynamic height
  }
  )
  
  ## conditional display of wc plot 
  output$wc_plot_status <- reactive({
    req(word_fq_df())
    return(TRUE)
  })
  outputOptions(output, "wc_plot_status", suspendWhenHidden = FALSE)
  
  
  ## explort word cloud 
  callModule(module = export_base_graphics, 
             id = "export_wordcloud" , 
             file_name = "word_cloud_plot", 
             plot = as_mapper(~ get_wc_plot(word_fq_df()$word , 
                                            word_fq_df()$freq , 
                                            palatte = input$wc_color_palatte , 
                                            max_words = input$wc_max_words) ),
             isComplexHeatmap = FALSE
             
  )
}

    
    

    
