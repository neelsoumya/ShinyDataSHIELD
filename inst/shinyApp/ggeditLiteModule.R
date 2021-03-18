ggeditLiteUI <- function(id) {
  fluidRow(
    column(12,
           actionButton(NS(id, "display_modal"),"Plot editor"),
           )
  )
}

ggeditLiteServer <- function(id, plot_name) {
  plot_modified <- reactiveVal(ggplot())
  values = reactiveValues(to_print = "",   ## This is the text that will be displayed
                          modal_closed=F)  ## This prevents the values$to_print output from 
  #  updating until the modal is closed
  observeEvent(input[[NS(id, "display_modal")]],{
    values$modal_closed <- F
    showModal(modalDialog(
      plotOutput(NS(id, "ggeditLitePlot")),
      h4("Edit options:"),
              fluidRow(column(4,
                              uiOutput(NS(id, "text_size_ui")),
                              uiOutput(NS(id, "x_axis_angle_ui")),
                              uiOutput(NS(id, "title_ui")),
                              uiOutput(NS(id, "subtitle_ui"))
                              ),
                       column(4,
                              uiOutput(NS(id, "caption_ui")),
                              uiOutput(NS(id, "x_title_ui")),
                              uiOutput(NS(id, "y_title_ui")),
                              uiOutput(NS(id, "legend_ui"))
                              ),
                       column(4,
                              selectInput(NS(id, "palette"), "Theme", c("Default", "flat", "flat dark",
                                                                        "camouflage", "chalk", "copper",
                                                                        "dust", "earth", "fresh",
                                                                        "grape", "grass", "greyscale",
                                                                        "light", "lilac", "pale",
                                                                        "sea", "sky", "solarized")),
                              selectInput(NS(id, "palette_type"), "Theme type", c("inner", "outer")),
                              selectInput(NS(id, "palette_layout"), "Theme layout", c("clean", "clear", "minimal",
                                                                                      "plain", "scientific"))
                              # selectInput(NS(id, "font_family"), "Font family", names(pdfFonts()))
                              )),
              actionButton(NS(id, "ala"),"Update plot"),
              downloadButton(NS(id, "plot_download"), "Download modified plot"),
      ## This footer replaces the default "Dismiss" button,
      #  which is 'footer = modalButton("Dismiss")'
      footer = actionButton(NS(id, "dismiss_modal"),label = "Close")
    ))
  })
  
  observeEvent(input[[NS(id, "dismiss_modal")]],{
    values$modal_closed <- T
    removeModal()
  })
  
  ## values$to_print is only updated once the modal is closed.
  observe({
    if(values$modal_closed){
      ggthemr_reset()
    }
  })
  moduleServer(id, function(input, output, session) {
    observeEvent(input$display_modal, {
      plot_modified(plots[[plot_name]])
      output$ggeditLitePlot <- renderPlot({plot_modified()})
      
      output$text_size_ui <- renderUI({
        numericInput(NS(id, "text_size"), "Text size", if(is.null(plot_modified()$theme$text$size)){theme_get()$text$size} else{
          plot_modified()$theme$text$size
        })
      })
      output$x_axis_angle_ui <- renderUI({
        numericInput(NS(id, "x_axis_angle"), "X axis angle", 
                     if(is.null(plot_modified()$theme$axis.text.x$angle)){0} 
                     else{plot_modified()$theme$axis.text.x$angle})
      })
      output$title_ui <- renderUI({
        textInput(NS(id, "title"), "Title", plot_modified()$labels$title)
      })
      output$subtitle_ui <- renderUI({
        textInput(NS(id, "subtitle"), "Subtitle", plot_modified()$labels$subtitle)
      })
      output$caption_ui <- renderUI({
        textInput(NS(id, "caption"), "Caption", plot_modified()$labels$caption)
      })
      output$x_title_ui <- renderUI({
        textInput(NS(id, "x_title"), "X Title", plot_modified()$labels$x)
      })
      output$y_title_ui <- renderUI({
        textInput(NS(id, "y_title"), "Y Title", plot_modified()$labels$y)
      })
      output$legend_ui <- renderUI({
        textInput(NS(id, "legend"), "Legend", if(is.null(plot_modified()$guides$colour$title)){plot_modified()$guides$fill$title}else{plot_modified()$guides$colour$title})
      })
    })
    
    observeEvent(input$ala, {
      # browser()
      plot_modified(plot_modified() + 
                      ggplot2::theme(text = ggplot2::element_text(size=if(is.null(input$text_size) | is.na(input$text_size)){NULL}
                                                                  else{input$text_size}),
                                     axis.text.x = ggplot2::element_text(angle=input$x_axis_angle)) +
                      ggplot2::labs(title = input$title,
                                    subtitle = input$subtitle,
                                    caption = input$caption) +
                      ggplot2::xlab(input$x_title) +
                      ggplot2::ylab(input$y_title) +
                      guides(color = guide_legend(paste0(input$legend, '\n'))) +
                      guides(fill = guide_legend(paste0(input$legend, '\n')))
      )
      if(input$palette != "Default"){
        ggthemr(input$palette, type = input$palette_type, layout = input$palette_layout)
        if(tryCatch({length(unique(plot_modified()$data[[rlang::as_name(plot_modified()$mapping$colour)]]))}, error = function(w){0}) > length(ggthemr::swatch()) - 1){
          ggthemr::set_swatch(colorRampPalette(ggthemr::swatch())(length(unique(plot_modified()$data[[rlang::as_name(plot_modified()$mapping$colour)]]))+1))
        }
        plot_modified(plot_modified() + scale_colour_ggthemr_d())# + scale_fill_manual(values = ggthemr::swatch()[-1]))
      } else{ggthemr_reset()}
    })
    output$plot_download <- downloadHandler(
      filename = "plot.png",
      content = function(file) {
        ggsave(file, plot = last_plot())
      }
    )
    })
}