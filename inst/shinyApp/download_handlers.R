output$descriptive_summary_download <- downloadHandler(
  filename = "descriptive_summary.csv",
  content = function(file) {
    write.csv(
      tryCatch({
        if(is.null(input$d_statistics_variable_selector_value_approach)){type <- "combine"} else{
          type <- input$d_statistics_variable_selector_value_approach
        }
        if(ds.class(paste0("tables_descriptive$", input$d_statistics_variable_selector_value), datasources = connection$conns[
          as.numeric(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1], 2])
        ]) == "factor") {
          taula <- ds.table(rvar = paste0("tables_descriptive$", input$d_statistics_variable_selector_value), datasources = connection$conns[
            as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
          ])
          if(type == "combine"){
            table <- data.frame(taula$output.list$TABLES.COMBINED_all.sources_counts)
            colnames(table) <- "Counts"
          }
          else{
            table <- data.frame(taula$output.list$TABLE_rvar.by.study_counts)
            colnames(table) <- paste0(names(connection$conns[
              as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
            ]))
          }
          table
        }
        else {
          Quantiles <- ds.quantileMean(paste0("tables_descriptive$", input$d_statistics_variable_selector_value), datasources = connection$conns[
            as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
          ], type = type)
          table <- data.frame(matrix(unlist(Quantiles), nrow=length(Quantiles), byrow=T))
          rownames(table) <- names(Quantiles)
          colnames(table) <- names(Quantiles[[1]])
          round(table, digits = 4)
        }
      }, error = function(w){})
      , file)
  }
)

output$glm_results_table_download <- downloadHandler(
  filename = "glm_results_table.csv",
  content = function(file) {
    write.csv(if(input$glm_approach == "Pooled"){
      glm_results$glm_result_table$coefficients
    }else{glm_results$glm_result_table$SLMA.pooled.ests.matrix}, 
              file, quote = F)
  }
)

output$glm_slma_down <- downloadHandler(
  filename = "glm_slma.png",
  content = function(file) {
    png(file = file)
    plots$glm_slma()
    dev.off()
  }
)

output$glmer_results_table_download <- downloadHandler(
  filename = "glmer_results_table.csv",
  content = function(file) {
    write.csv(if(is.list(glm_results$glmer_result_table$output.summary[[input$glmer_results_select_value]])){
      eval(str2expression(paste0("glm_results$glmer_result_table$output.summary$", input$glmer_results_select_value, "$coefficients")))
    }else{try(eval(str2expression(paste0("glm_results$glmer_result_table$output.summary$", input$glmer_results_select_value))))}, 
              file, quote = F)
  }
)

output$glmer_slma_down <- downloadHandler(
  filename = "glmer_slma.png",
  content = function(file) {
    png(file = file)
    plots$glmer_slma()
    dev.off()
  }
)

output$plink_results_table_download <- downloadHandler(
  filename = "plink_results_table.csv",
  content = function(file) {
    write.csv(plink_results$result_table[[1]]$results, 
              file, row.names = FALSE, quote = F)
  }
)

output$vcf_results_table_download <- downloadHandler(
  filename = "vcf_results_table.csv",
  content = function(file) {
    write.csv(do.call("rbind", vcf_results$result_table_gwas), 
              file, row.names = FALSE, quote = F)
  }
)

output$limma_results_table_download <- downloadHandler(
  filename = "limma_results_table.csv",
  content = function(file) {
    write.csv({
      exp <- paste0("res <- rbind(", paste0("as.data.table(limma_results$result_table$", unique(lists$available_tables$server), ")", 
                                            collapse = ","), ")")
      eval(str2expression(exp))
      res
    },file, row.names = FALSE, quote = F)
  }
)

output$d_statistics_scatter_plot_download <- downloadHandler(
  filename = "d_statistics_scatter_plot.png",
  content = function(file) {
    png(file = file)
    plots$ds_scatter_plot()
    dev.off()
  }
)

output$d_statistics_histogram_plot_download <- downloadHandler(
  filename = "d_statistics_histogram_plot.png",
  content = function(file) {
    png(file = file)
    plots$ds_histogram_plot()
    dev.off()
  }
)

output$d_statistics_heatmap_plot_download <- downloadHandler(
  filename = "d_statistics_heatmap_plot.png",
  content = function(file) {
    png(file = file)
    plots$ds_heatmap_plot()
    dev.off()
  }
)

output$genomics_manhattan_plink_plot_download <- downloadHandler(
  filename = "genomics_manhattan_plink_plot.png",
  content = function(file) {
    ggsave(file, plot = last_plot())
  }
)

output$genomics_manhattan_vcf_plot_download <- downloadHandler(
  filename = "genomics_manhattan_vcf_plot.png",
  content = function(file) {
    ggsave(file, plot = last_plot())
  }
)

output$survival_results_table_download <- downloadHandler(
  filename = "survival_results_table.csv",
  content = function(file) {
    write.csv(
      tryCatch({
        round(survival_models$survival_models[[input$survival_results_table_study_selector]]$coefficients, digits = 4)
      }, error = function(w){
        round(survival_models$survival_models[[1]]$coefficients, digits = 4)
      }), file, row.names = TRUE, quote = F)
  }
)

output$survival_plot_download <- downloadHandler(
  filename = "survival_plot.png",
  content = function(file) {
    png(file = file)
    plots$survival_plot()
    dev.off()
  }
)

