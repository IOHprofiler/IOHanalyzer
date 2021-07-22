FV_DSC_rank_result <- reactive({
  req(input$FV_Stats.DSC.Create_rank)
  
  isolate({
    withProgress({
      data <- FV_DSC_data()
      req(length(data) > 0)
      # if (input$FV_Stats.DSC.Value_type != 'Raw') {
      #   shinyjs::alert("This feature is not yet implemented in the current version of IOHanalyzer.")
      # }
      test_type <- if (input$FV_Stats.DSC.Test_rank == "Anderson-Darling") "AD" else "KS"
      rank_res <- get_dsc_rank(data, FV_stats_DSC_targets_obj, which = 'by_FV', 
                               alpha = input$FV_Stats.DSC.Alpha_rank, 
                               test_type = test_type,
                               monte_carlo_iterations = input$FV_Stats.DSC.MCsamples_rank,
                               epsilon = input$FV_Stats.DSC.Epsilon_rank)
      if (is.null(rank_res)) {
        shinyjs::alert("There was an error getting the ranking data from DSCtool. Please 
        select different settings and try again")
        return(NULL)
      }
      updateSelectInput(session, 'FV_Stats.DSC.Omni_options', choices = rank_res$valid_methods, 
                        selected = rank_res$valid_methods[[1]])
      
      
      rank_res
    }, message = "Getting DSC ranking data")
  })
})

FV_render_performviz <- reactive({
  rank_res <- FV_DSC_rank_result()
  if (is.null(rank_res)) {return(NULL)}
  Plot.Performviz(rank_res)
})

output$FV_Stats.DSC.PerformViz <- renderPlot({
  FV_render_performviz()
})

output$FV_Stats.DSC.Download_rank_table <- downloadHandler(
  filename = function() {
    eval(FV_DSC_table_name_rank)
  },
  content = function(file) {
    mlist <- FV_DSC_rank_result()$ranked_matrix
    df <- rbindlist(lapply(seq(length(mlist)), function(problem_idx) {
      df_temp <- rbindlist(lapply(mlist[[problem_idx]]$result, 
                                  function(x) {
                                    list(algorithm = x$algorithm, rank =  x$rank)
                                  }))
      df_temp[, problem := mlist[[problem_idx]]$problem]
    }))
    df <- reshape2::acast(df, algorithm ~ problem, value.var = 'rank')
    save_table(df, file)
  }
)

output$FV_Stats.DSC.Download_rank <- downloadHandler(
  filename = function() {
    eval(FV_DSC_figure_name_rank)
  },
  content = function(file) {
    #Temporary settings, will replace plotting from complexheatmap to 
    #Some other plotly-compatible library later, and use save_plotly
    suppressWarnings({
      pdf(file = file, width = 16, height = 9)
      RT_render_performviz()
      dev.off()
    })
  },
  contentType = paste0('image/', input$FV_Stats.DSC.Format_rank)
)

FV_DSC_omni_result <- reactive({
  input$FV_Stats.DSC.Create_omni
  
  isolate({
    withProgress({
      rank_res <- FV_DSC_rank_result()
      req(rank_res)
      omni_res <- get_dsc_omnibus(rank_res, method = input$FV_Stats.DSC.Omni_options,
                                  alpha = input$FV_Stats.DSC.Alpha_omni)
      
    }, message = "Creating Comparison, this might take a while")
  })
})


output$FV_Stats.DSC.Output_omni <- renderText({
  res_omni <- FV_DSC_omni_result()
  req(res_omni)
  paste0(res_omni$message, "(p-value: ", res_omni$p_value, ")")
})


FV_DSC_posthoc_result <- reactive({
  input$FV_Stats.DSC.Create_posthoc
  
  isolate({
    withProgress({
      omni_res <- FV_DSC_omni_result()
      req(omni_res)
      data <- FV_DSC_data()
      req(length(data) > 0)
      df_posthoc <- rbindlist(lapply(get_id(data), function(algname){
        posthoc_res <- get_dsc_posthoc(omni_res, length(get_id(data)), 
                                       nrow(FV_stats_DSC_targets_obj),
                                       alpha = input$FV_Stats.DSC.Alpha_posthoc, 
                                       base_algorithm = algname,
                                       method = input$FV_Stats.DSC.Posthoc_test)
        if (is.null(posthoc_res)) { return(NULL)}
        values <- lapply(seq(4), function(method_idx) {
          lapply(posthoc_res$adjusted_p_values[[method_idx]]$algorithms, 
                 function(x) {
                   x$value
                 })
        })
        
        algnames <- lapply(posthoc_res$adjusted_p_values[[1]]$algorithms, 
                           function(x) {
                             x$algorithm
                           })
        df <- data.table("baseline" = algname, "compared alg" = algnames, z = values[[1]], "unadjusted P" = values[[2]], 
                         "Holm" = values[[3]], "Hochberg" = values[[4]])
        
      }))
      as.data.table(format(df_posthoc, digits = 3))
    }, message = "Creating Comparison, this might take a while")
  })
})

output$FV_Stats.DSC.PosthocTable <- DT::renderDataTable({
  
  req(length(DATA_RAW()) > 0)
  FV_DSC_posthoc_result()
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

FV_render_DSC_plot <- reactive({
  dt <- FV_DSC_posthoc_result()
  if (is.null(dt) || nrow(dt) < 1) {return(NULL)}
  # plot_general_data(dt[method == input$FV_Stats.DSC.method], x_attr = 'algId', y_attr = 'value', type = 'bar',
  #                   legend_attr = 'algId')
  dt <- dt[,c('baseline', 'compared alg', input$FV_Stats.DSC.Posthoc_method), with = F]
  p_matrix <- acast(dt, baseline ~ `compared alg`, value.var = input$FV_Stats.DSC.Posthoc_method)
  storage.mode(p_matrix) <- "numeric"
  y <- p_matrix <= input$FV_Stats.DSC.Alpha_posthoc
  colorScale <- data.frame(x = c(-1, -0.33, -0.33, 0.33, 0.33, 1),
                           col = c('blue', 'blue', 'white', 'white', 'red', 'red')
  )
  heatmap <-  y - t(y)
  
  p <- plot_ly(x = colnames(y), y = rownames(y), z = heatmap, type = 'heatmap',
               xgap = 0.2, ygap = 0.2, colorscale = colorScale, showscale = F)
  p %<>% layout(yaxis = list(autorange = 'reversed', scaleratio = 1),
                xaxis = list(tickangle = 45))
  p
})

output$FV_Stats.DSC.PosthocViz <- renderPlotly({
  FV_render_DSC_plot()
})

output$FV_Stats.DSC.DownloadTable <- downloadHandler(
  filename = function() {
    eval(FV_DSC_table_name)
  },
  content = function(file) {
    df <- FV_DSC_posthoc_result()
    save_table(df, file)
  }
)

output$FV_Stats.DSC.Download <- downloadHandler(
  filename = function() {
    eval(FV_DSC_figure_name)
  },
  content = function(file) {
    save_plotly(FV_render_DSC_plot(), file)
  },
  contentType = paste0('image/', input$FV_Stats.DSC.Format)
)


FV_DSC_data <- function() {
  data <- subset(DATA_RAW(), ID %in% isolate(input$FV_Stats.DSC.Algid))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM %in% input$FV_Stats.DSC.Dim)
  data <- subset(data, funcId %in% input$FV_Stats.DSC.Funcid)
  if (length(unique(get_id(data))) < 2) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple algorithms for the selected functions and dimensions.")
    return(NULL)
  }
  data
}

FV_stats_DSC_targets <- reactive({
  data <- FV_DSC_data()
  if (is.null(data)) return(NULL)
  get_target_dt(data, "by_FV")
})

FV_stats_DSC_targets_obj <- NULL

proxy_FV_Stats.DSC.Targets <- dataTableProxy('FV_Stats.DSC.Targets')

output$FV_Stats.DSC.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  FV_stats_DSC_targets_obj <<- FV_stats_DSC_targets()
  FV_stats_DSC_targets_obj
}, editable = list(target = 'cell', disable = list(columns = c(0,1))), rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$FV_Stats.DSC.Targets_cell_edit, {
  info <- input$FV_Stats.DSC.Targets_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  data <- FV_DSC_data()
  if (is.null(data)) return(NULL)
  FV_stats_DSC_targets_obj$target[[i]] <<- v
  replaceData(proxy, FV_stats_DSC_targets_obj, resetPaging = FALSE, rownames = FALSE)
})