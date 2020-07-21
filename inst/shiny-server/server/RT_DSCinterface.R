RT_DSC_rank_result <- reactive({
  req(input$RT_Stats.DSC.Create_rank)
  
  isolate({
    withProgress({
      data <- RT_DSC_data()
      req(length(data) > 0)
      test_type <- if (input$RT_Stats.DSC.Test_rank == "Anderson-Darling") "AD" else "KS"
      rank_res <- get_dsc_rank(data, RT_stats_DSC_targets_obj, which = 'by_RT', 
                               alpha = input$RT_Stats.DSC.Alpha_rank, 
                               test_type = test_type,
                               monte_carlo_iterations = input$RT_Stats.DSC.MCsamples_rank,
                               epsilon = input$RT_Stats.DSC.Epsilon_rank, 
                               na.correction = input$RT_Stats.DSC.Value_type)
      if (is.null(rank_res)) {
        shinyjs::alert("There was an error getting the ranking data from DSCtool. Please 
        select different settings and try again")
        return(NULL)
      }
      updateSelectInput(session, 'RT_Stats.DSC.Omni_options', choices = rank_res$valid_methods, 
                        selected = rank_res$valid_methods[[1]])
      
      
      rank_res
    }, message = "Getting DSC ranking data")
  })
})

RT_render_performviz <- reactive({
  rank_res <- RT_DSC_rank_result()
  if (is.null(rank_res)) {return(NULL)}
  Plot.Performviz(rank_res)
})

output$RT_Stats.DSC.PerformViz <- renderPlot({
  RT_render_performviz()
})

output$RT_Stats.DSC.Download_rank <- downloadHandler(
  filename = function() {
    eval(RT_DSC_figure_name_rank)
  },
  content = function(file) {
    save_plotly(RT_render_performviz(), file)
  },
  contentType = paste0('image/', input$RT_Stats.DSC.Format_rank)
)

RT_DSC_omni_result <- reactive({
  input$RT_Stats.DSC.Create_omni
  
  isolate({
    withProgress({
      rank_res <- RT_DSC_rank_result()
      req(rank_res)
      omni_res <- get_dsc_omnibus(rank_res, method = input$RT_Stats.DSC.Omni_options,
                                  alpha = input$RT_Stats.DSC.Alpha_omni)
      
    }, message = "Creating Comparison, this might take a while")
  })
})


output$RT_Stats.DSC.Output_omni <- renderText({
  res_omni <- RT_DSC_omni_result()
  req(res_omni)
  paste0(res_omni$message, "(p-value: ", res_omni$p_value, ")")
})


RT_DSC_posthoc_result <- reactive({
  input$RT_Stats.DSC.Create_posthoc
  
  isolate({
    withProgress({
      omni_res <- RT_DSC_omni_result()
      req(omni_res)
      data <- RT_DSC_data()
      req(length(data) > 0)
      df_posthoc <- rbindlist(lapply(get_algId(data), function(algname){
        posthoc_res <- get_dsc_posthoc(omni_res, length(get_algId(data)), 
                                       nrow(RT_stats_DSC_targets_obj),
                                       alpha = input$RT_Stats.DSC.Alpha_posthoc, 
                                       base_algorithm = algname,
                                       method = input$RT_Stats.DSC.Posthoc_test)
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

output$RT_Stats.DSC.PosthocTable <- DT::renderDataTable({
  
  req(length(DATA_RAW()) > 0)
  RT_DSC_posthoc_result()
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

RT_render_DSC_plot <- reactive({
  dt <- RT_DSC_posthoc_result()
  if (is.null(dt) || nrow(dt) < 1) {return(NULL)}
  # plot_general_data(dt[method == input$RT_Stats.DSC.method], x_attr = 'algId', y_attr = 'value', type = 'bar',
  #                   legend_attr = 'algId')
  dt <- dt[,c('baseline', 'compared alg', input$RT_Stats.DSC.Posthoc_method), with = F]
  p_matrix <- acast(dt, baseline ~ `compared alg`, value.var = input$RT_Stats.DSC.Posthoc_method)
  storage.mode(p_matrix) <- "numeric"
  y <- p_matrix <= input$RT_Stats.DSC.Alpha_posthoc
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

output$RT_Stats.DSC.PosthocViz <- renderPlotly({
  RT_render_DSC_plot()
})

output$RT_Stats.DSC.DownloadTable <- downloadHandler(
  filename = function() {
    eval(RT_DSC_table_name)
  },
  content = function(file) {
    df <- RT_DSC_posthoc_result()
    if (input$RT_Stats.DSC.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)

output$RT_Stats.DSC.Download <- downloadHandler(
  filename = function() {
    eval(RT_DSC_figure_name)
  },
  content = function(file) {
    save_plotly(RT_render_DSC_plot(), file)
  },
  contentType = paste0('image/', input$RT_Stats.DSC.Format)
)


RT_DSC_data <- function() {
  data <- subset(DATA_RAW(), algId %in% isolate(input$RT_Stats.DSC.Algid))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM %in% input$RT_Stats.DSC.Dim)
  data <- subset(data, funcId %in% input$RT_Stats.DSC.Funcid)
  if (length(unique(get_algId(data))) < 2) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple algorithms for the selected functions and dimensions.")
    return(NULL)
  }
  data
}

RT_stats_DSC_targets <- reactive({
  data <- RT_DSC_data()
  if (is.null(data)) return(NULL)
  get_target_dt(data, "by_RT")
})

RT_stats_DSC_targets_obj <- NULL

proxy_RT_Stats.DSC.Targets <- dataTableProxy('RT_Stats.DSC.Targets')

output$RT_Stats.DSC.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  RT_stats_DSC_targets_obj <<- RT_stats_DSC_targets()
  RT_stats_DSC_targets_obj
}, editable = list(target = 'cell', disable = list(columns = c(0,1))), rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$RT_Stats.DSC.Targets_cell_edit, {
  info <- input$RT_Stats.DSC.Targets_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  data <- RT_DSC_data()
  if (is.null(data)) return(NULL)
  RT_stats_DSC_targets_obj$target[[i]] <<- v
  replaceData(proxy, RT_stats_DSC_targets_obj, resetPaging = FALSE, rownames = FALSE)
})