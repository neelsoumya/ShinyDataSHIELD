output$d_statistics_scatter_plot <- renderPlot({
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_scatter_value_approach)){type <- "combine"} else{
      type <- input$d_statistics_variable_selector_scatter_value_approach
    }
    
    plots$ds_scatter_plot <- function(){
      ds.scatterPlot(x = paste0("tables_descriptive$", input$d_statistics_variable_selector_scatter_value), 
                     y = paste0("tables_descriptive$", input$d_statistics_variable_selector_scatter_value2),
                     type = type,
                     datasources = connection$conns[
                       as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
                     ])
    }
    plots$ds_scatter_plot()
  }, error = function(w){})
})

output$d_statistics_histogram_plot <- renderPlot({
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_histogram_value_approach)){type <- "combine"} else{
      type <- input$d_statistics_variable_selector_histogram_value_approach
    }
    
    plots$ds_histogram_plot <- function(){
      ds.histogram(x = paste0("tables_descriptive$", input$d_statistics_variable_selector_histogram_value),
                   type = type,
                   datasources = connection$conns[
                     as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
                   ])
    }
    plots$ds_histogram_plot()
  }, error = function(w){})
})

output$d_statistics_heatmap_plot <- renderPlot({
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_heatmap_value_approach)){type <- "combine"} else{
      type <- input$d_statistics_variable_selector_heatmap_value_approach
    }
    
    plots$ds_heatmap_plot <- function(){
      ds.heatmapPlot(x = paste0("tables_descriptive$", input$d_statistics_variable_selector_heatmap_value),
                     y = paste0("tables_descriptive$", input$d_statistics_variable_selector_heatmap_value2),
                     type = type,
                     datasources = connection$conns[
                       as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
                     ])
    }
    plots$ds_heatmap_plot()
  }, error = function(w){})
})

output$d_statistics_boxplot_plot <- renderPlot({
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_boxplot_value_approach)){type <- "pooled"} else{
      type <- input$d_statistics_variable_selector_boxplot_value_approach
    }
    plots$ds_boxplot_plot <- ds.boxPlot(x = "tables_descriptive", 
                 variables  = input$d_statistics_variable_selector_boxplot_value,
                 group = if(input$d_statistics_variable_selector_boxplot_value2 == ""){NULL} else{
                   input$d_statistics_variable_selector_boxplot_value2
                 },
                 group2 = if(input$d_statistics_variable_selector_boxplot_value3 == ""){NULL} else{
                   input$d_statistics_variable_selector_boxplot_value3
                 },
                 type = type,
                 datasources = connection$conns[
                   as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
                 ])
    plots$ds_boxplot_plot
  }, error = function(w){})
})

output$glm_slma_plot <- renderPlot({
  plots$glm_slma <- function(){
    method_lookup <- c("ML", "REML") #, "FE")
    names(method_lookup) <- c("Maximum Likelihood", "REstricted Maximum Likelihood") #, "Fixed-Effects meta-analysis")
    method <- method_lookup[input$glm_slma_method]
    tryCatch({ds.forestplot(glm_results$glm_result_table, method = method)}, error = function(w){})
  }
  plots$glm_slma()
})

output$glmer_slma_plot <- renderPlot({
  plots$glmer_slma <- function(){
    method_lookup <- c("ML", "REML") #, "FE")
    names(method_lookup) <- c("Maximum Likelihood", "REstricted Maximum Likelihood") #, "Fixed-Effects meta-analysis")
    method <- method_lookup[input$glmer_slma_method]
    tryCatch({ds.forestplot(glm_results$glmer_result_table, method = method)}, error = function(w){})
  }
  plots$glmer_slma()
})

output$survival_meta_analysis_plot <- renderPlot({
  metafor::forest.rma(x = survival_models$meta_model,
                      digits = 3L,
                      slab = unlist(unique(lists$available_tables[input$available_tables_sm_render_rows_selected,3])))
})

output$survival_plot <- renderPlot({
  dsSurvivalClient::ds.survfit(formula='survival_object~1', objectname='survfit_object',
                               datasources = connection$conns)
  plots$survival_plot <- function(){
    dsSurvivalClient::ds.plotsurvfit(formula = "survfit_object",
                                     datasources = connection$conns[
                                       as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_sm_render_rows_selected, 2]))
                                     ])
  }
  plots$survival_plot()
})

output$manhattan <- renderPlot({
  data <- do.call("rbind", vcf_results$result_table_gwas)
  featureCol <- 1
  chrCol <- 2
  posCol <- 3
  pvalCol <- 6
  plots$genomics_manhattan_vcf_plot <- dsOmicsClient::manhattan(data, featureCol = featureCol, chrCol = chrCol,
            posCol = posCol, pvalCol = pvalCol)
  plots$genomics_manhattan_vcf_plot
})

output$manhattan2 <- renderCachedPlot({
  data <- plink_results$result_table[[1]]$results
  featureCol <- 2
  chrCol <- 1
  posCol <- 3
  pvalCol <- 9
  plots$genomics_manhattan_plink_plot <- manhattan(data, featureCol = featureCol, chrCol = chrCol,
                                                    posCol = posCol, pvalCol = pvalCol)
  plots$genomics_manhattan_plink_plot
},
cacheKeyExpr = plink_results$result_table[[1]]$results
)

