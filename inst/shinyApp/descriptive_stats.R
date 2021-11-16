observeEvent(input$select_tables_descr_stats, {
  if(length(input$available_tables_render_rows_selected) > 0){
    same_cols <- TRUE
    different_study_server <- TRUE
    if(length(input$available_tables_render_rows_selected) > 1){
      same_cols <- all(lapply(input$available_tables_render_rows_selected, function(i){
        res<-all(match(lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][i,1])]], 
                       lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]))
        if(is.na(res)){FALSE} else{res}
      }))
      different_study_server <- nrow(unique(lists$available_tables[input$available_tables_render_rows_selected,3])) ==
                                  length(input$available_tables_render_rows_selected)
      }
    if(!same_cols | !different_study_server){
      shinyalert("Oops!",
                 if(!same_cols){
                   "Selected tables do not share the same columns, can't pool unequal tables."
                 }else{
                   "Selected tables are not on different study servers, can't pool tables on the same study server."
                 }
                 , type = "error")
      js$disableTab("summary")
      js$disableTab("s_plot")
      js$disableTab("h_plot")
      js$disableTab("hm_plot")
      js$disableTab("box_plot")
      updateTabsetPanel(session, "d_statistics_t",
                        selected = "a_tables")
    }
    else{
      datashield.rm(connection$conns, "tables_descriptive")
      tables_available <- lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected,]
      expr <- as.list(tables_available$name)
      names(expr) <- tables_available$server
      DSI::datashield.assign.expr(connection$conns, "tables_descriptive", expr)
      withProgress(message = "Getting the column types for selected tables", value = 0, {
        lists$table_columns_types <- NULL
        for(var in lists$table_columns[[input$available_tables_render_rows_selected[1]]]){
          type <- ds.class(paste0("tables_descriptive$", var), 
                           connection$conns[as.numeric(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected,2][1])])[[1]]
          lists$table_columns_types <- cbind(lists$table_columns_types, rbind(var, paste(type, collapse = ", ")))
          incProgress(1/length(lists$table_columns[[1]]))
        }
      })
      
      lists$table_columns_types <- as.data.table(t(lists$table_columns_types))
      colnames(lists$table_columns_types) <- c("variable", "type")
      js$enableTab("summary")
      js$enableTab("s_plot")
      js$enableTab("h_plot")
      js$enableTab("hm_plot")
      js$enableTab("box_plot")
      updateTabsetPanel(session, "d_statistics_t",
                        selected = "summary")
    }
  }
})

output$d_statistics_variable_selector <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_approach <- renderUI({
      selectInput("d_statistics_variable_selector_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_value", "Select variable", 
                lists$table_columns_types[type %in% c("factor", "numeric")]$variable
                )
  }
  else{
    selectInput("d_statistics_variable_selector_value", "Select variable", 
                lists$table_columns_types[type %in% c("factor", "numeric")]$variable
                )
  }
})

output$d_statistics_variable_selector_scatter <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_scatter_approach <- renderUI({
      selectInput("d_statistics_variable_selector_scatter_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_scatter_value", "Select variable", 
                lists$table_columns_types[type %in% c("numeric")]$variable
    )
  }
  else{
    selectInput("d_statistics_variable_selector_scatter_value", "Select variable", 
                lists$table_columns_types[type %in% c("numeric")]$variable
    )
  }
})
output$d_statistics_variable_selector_scatter2 <- renderUI({
  selectInput("d_statistics_variable_selector_scatter_value2", "Select variable", 
              lists$table_columns_types[type %in% c("numeric")]$variable
             )
})

output$d_statistics_variable_selector_histogram <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_histogram_approach <- renderUI({
      selectInput("d_statistics_variable_selector_histogram_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_histogram_value", "Select variable", 
                lists$table_columns_types[type %in% c("numeric")]$variable
    )
  }
  else{
    selectInput("d_statistics_variable_selector_histogram_value", "Select variable", 
                lists$table_columns_types[type %in% c("numeric")]$variable
    )
  }
})

output$d_statistics_variable_selector_heatmap <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_heatmap_approach <- renderUI({
      selectInput("d_statistics_variable_selector_heatmap_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_heatmap_value", "Select variable", 
                lists$table_columns_types[type %in% c("numeric")]$variable
    )
  }
  else{
    selectInput("d_statistics_variable_selector_heatmap_value", "Select variable", 
                lists$table_columns_types[type %in% c("numeric")]$variable
    )
  }
})
output$d_statistics_variable_selector_heatmap2 <- renderUI({
  selectInput("d_statistics_variable_selector_heatmap_value2", "Select variable", 
              lists$table_columns_types[type %in% c("numeric")]$variable
  )
})

output$d_statistics_variable_selector_boxplot <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_boxplot_approach <- renderUI({
      selectInput("d_statistics_variable_selector_boxplot_value_approach", "Select approach", c("pooled", "split"))
    })
    selectInput("d_statistics_variable_selector_boxplot_value", "Select variable(s)", 
                lists$table_columns_types[type %in% c("numeric")]$variable,
                multiple = TRUE
    )
  }
  else{
    selectInput("d_statistics_variable_selector_boxplot_value", "Select variable(s)", 
                lists$table_columns_types[type %in% c("numeric")]$variable,
                multiple = TRUE
    )
  }
})
output$d_statistics_variable_selector_boxplot2 <- renderUI({
  selectInput("d_statistics_variable_selector_boxplot_value2", "Select grouping variable", 
              c("", lists$table_columns_types[type %in% c("factor")]$variable)
  )
})
output$d_statistics_variable_selector_boxplot3 <- renderUI({
  selectInput("d_statistics_variable_selector_boxplot_value3", "Select second grouping variable", 
              c("", lists$table_columns_types[type %in% c("factor")]$variable)
  )
})

observe({
  if(input$tabs == "d_statistics") {
    # Get column names from available tables
    tables_available <- lists$available_tables[type_resource == "table"]
    tables_available_aux <- tables_available
    aux <- list()
    if(length(lists$tables_columns) == 0){
      withProgress(message = "Reading column names from available tables", value = 0, {
        while(any(duplicated(tables_available_aux$server))){
          duplicateds <- !duplicated(tables_available_aux$server)
          expr <- as.list(paste0("colnamesDS('", tables_available_aux$name[duplicateds], "')"))
          names(expr) <- tables_available_aux$server[duplicateds]
          table_columns <- DSI::datashield.aggregate(connection$conns, expr)
          names(table_columns) <- tables_available_aux$name[duplicateds]
          tables_available_aux <- tables_available_aux[!duplicateds,]
          aux <- c(aux, table_columns)
        }
        expr <- as.list(paste0("colnamesDS('", tables_available_aux$name, "')"))
        names(expr) <- tables_available_aux$server
        table_columns <- DSI::datashield.aggregate(connection$conns, expr)
        names(table_columns) <- tables_available_aux$name
        aux <- c(aux, table_columns)
        lists$table_columns <- aux[tables_available$name]
      })
    }
    output$available_tables <- renderUI({
      dataTableOutput("available_tables_render")
    })
  }
})