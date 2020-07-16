#' Convert a DataSetList to the format needed for the DSCtool
#' 
#' 
#' @param dsList The DataSetList object
#' @param targets Optional list of target values (Runtime or target value)
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' 
#' @nord
#' @examples 
#' convert_to_dsc_compliant(dsl)
convert_to_dsc_compliant <- function(dsList, targets = NULL, which = 'by_RT') {
  maximize <- attr(dsList, 'maximization')
  variable <- fid <- value <- NULL #Set local binding to remove warnings
  by_rt <- which == 'by_RT'
  
  if (is.null(targets)) {
    targets <- get_target_dt(dsList, which)
  }
  
  final_list <- lapply(get_algId(dsList), function(algname) {
    dsl_sub <- subset(dsList, algId == algname)
    problems <- lapply(dsl_sub, function(ds) {
      target <- targets[funcId == attr(ds, 'funcId') & DIM == attr(ds, 'DIM'), 'target']
      if (by_rt) {
        data <- get_RT_sample(ds, target, output = 'long')$RT
      }
      else {
        data <- get_FV_sample(ds, target, output = 'long')$`f(x)`
      }
      return(list(name = paste0("F", attr(ds, 'funcId'), "_", attr(ds, 'DIM'),"D"),
                  data = data))
    })
    return(list(algorithm = algname, problems = problems))
  })
  
  
  return(final_list)
}

#' Get the matrix of rankings using the DSCtool api for a DataSetList
#' 
#' 
#' @param dsList The DataSetList object
#' @param targets Optional list of target values (Runtime or target value)
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' @param test_type Either 'AD' for Anderson-Darling or KS for Kolmogorov-Smirnov tests
#' @param alpha Threshold value for statistical significance
#' @param epsilon Minimum threshold to have practical difference between algorithms (eDSC)
#' @param monte_carlo_iterations How many monte-carlo-simulations to perform 
#' (set to 0 to use regular DSC)
#' 
#' @return A named list containing a ranked-matrix which has the rankin of each algorithm
#' on each problem, as well as a list of which omnibus tests can be used to further process
#' this data. This can be further analyzed using `get_dsc_omnibus`
#' 
#' @export
#' @examples 
#' get_dsc_rank(dsl)
get_dsc_rank <- function(dsList, targets = NULL, which = 'by_RT', test_type = "AD", alpha = 0.05,
                         epsilon = 0, monte_carlo_iterations = 0) {
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/rank"
  dsc_list <- convert_to_dsc_compliant(dsList, targets, which)
  json_list <- list(method = list(name = test_type, alpha = alpha), epsilon = epsilon, 
                    data = dsc_list, monte_carlo_iterations = monte_carlo_iterations)
  
  result_json <- POST(url,
                      authenticate(keyring::key_get("DSCtool_name"),keyring::key_get("DSCtool")),
                      add_headers(.headers = c('Content-Type' = "application/json",
                                               'Accept' = "application/json")),
                      body = json_list, encode = "json")
  
  return(content(result_json)$result)
}


#' Perform omnibus statistical tests on the matrix of rankings from the DSCtool api 
#' 
#' 
#' @param res The result of a call to the `get_dsc_rank`
#' @param method Which method to use to do the tests. 
#' Has be be one of the allowed ones in `res$valid_methods`. 
#' When NULL, the first valid option is chosen by default
#' @param alpha Threshold value for statistical significance
#' 
#' @return A named list containing the algorithm means
#' 
#' @export
#' @examples 
#' get_dsc_omnibus(get_dsc_rank(dsl))
get_dsc_omnibus <- function(res, method = NULL, alpha = 0.05) {
  if (is.null(method)) method <- res$valid_methods[[1]]
  else req(method %in% res$valid_methods)
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/omnibus"
  new_json <- list(method = list(name = method, alpha = alpha),
                   ranked_matrix = res$ranked_matrix,
                   number_algorithms = res$number_algorithms,
                   parametric_tests = res$parametric_tests)
  result_json <- POST(url,
                      authenticate(keyring::key_get("DSCtool_name"),keyring::key_get("DSCtool")),
                      add_headers(.headers = c('Content-Type' = "application/json",
                                               'Accept' = "application/json")),
                      body = new_json, encode = "json")
  
  return(content(result_json)$result)
}

#' Perform post-hoc processing on data from DSCtool
#' 
#' 
#' @param omni_res The result from a call to `get_dsc_omnibus`
#' @param nr_algs The number of algorithms present in `omni_res`
#' @param nr_problems The number of problems present in `omni_res`
#' @param base_algorithm The base algorithm to which the other are compared. 
#' This has to be present in `omni_res$algorithm_means` as an `algorithm` property
#' @param method Either 'friedman' or 'friedman-aligned-rank'
#' @param alpha Threshold value for statistical significance
#' 
#' @return A named list containing 4 types of analyses:
#' * Zvalue
#' * UnadjustedPValue
#' * Holm
#' * Hochberg
#' 
#' @export
#' @examples 
#' get_dsc_posthoc(get_dsc_omnibus(get_dsc_rank(dsl)), 2, 2)
get_dsc_posthoc <- function(omni_res, nr_algs, nr_problems, base_algorithm = NULL, 
                            method = "friedman", alpha = 0.05) {
  if (is.null(base_algorithm)) base_algorithm <- omni_res$algorithm_means[[1]]$algorithm
  else req(base_algorithm %in% unlist(omni_res$algorithm_means))
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/posthoc"
  new_json <- list(method = list(name = method, alpha = alpha),
                   algorithm_means = omni_res$algorithm_means,
                   n = nr_algs,
                   k = nr_problems,
                   base_algorithm = base_algorithm)
  result_json <- POST(url,
                      authenticate(keyring::key_get("DSCtool_name"),keyring::key_get("DSCtool")),
                      add_headers(.headers = c('Content-Type' = "application/json",
                                               'Accept' = "application/json")),
                      body = new_json, encode = "json")
  
  return(content(result_json)$result)
}


get_dsc_performviz <- function(res, method = "performViz") {
  
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/visualize"
  new_json <- list(method = method,
                   ranked_matrix = res$ranked_matrix)
  # result <- POST(url,authenticate(username,password),
  #                     add_headers(.headers = c('Content-Type' = "application/json",
  #                                              'Accept' = "image/png")),
  #                     body = new_json, encode = "json")
  # 
  # return(result)
  download.file(content(POST(url,authenticate(username,password),
                             add_headers(.headers = c('Content-Type'="application/json",
                                                      'Accept' = "image/png"))),
                        body=new_json,encode="json"),"myimagename.png",mode="wb")
  
} 

data_table_posthoc <- reactive({
  input$RT_Stats.DSC.Create
  
  isolate({
    withProgress({
      data <- RT_DSC_data()
      req(length(data) > 0)
      rank_res <- get_dsc_rank(data, RT_stats_DSC_targets_obj, which = 'by_RT', 
                               alpha = input$RT_Stats.DSC.Alpha, test_type = "AD")
      if (is.null(rank_res)) {
        shinyjs::alert("There was an error getting the ranking data from DSCtool. Please 
        select different settings and try again")
        return(NULL)
      }
      omni_res <- get_dsc_omnibus(rank_res, alpha = input$RT_Stats.DSC.Alpha)
      posthoc_res <- get_dsc_posthoc(omni_res, length(get_algId(data)), 
                                     nrow(RT_stats_DSC_targets_obj),
                                     alpha = input$RT_Stats.DSC.Alpha, 
                                     base_algorithm = input$RT_Stats.DSC.Reference_alg)
      if (is.null(posthoc_res)) { return(NULL)}
      df <- rbindlist(lapply(seq(4), function(method_idx) {
        dt_temp <- rbindlist(lapply(posthoc_res$adjusted_p_values[[method_idx]]$algorithms, 
                                    function(x) {
                                      data.table(x$algorithm, x$value)
                             }))
        dt_temp[,('V3') := posthoc_res$adjusted_p_values[[method_idx]]$name]
        colnames(dt_temp) <- c('algId', 'value', 'method')
        dt_temp
      }))
      as.data.table(format(df, digits = 3))
    }, message = "Creating Comparison, this might take a while")
  })
})

output$RT_Stats.DSC.PosthocTable <- DT::renderDataTable({
  
  req(length(DATA_RAW()) > 0)
  data_table_posthoc()
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

render_DSC_plot <- reactive({
  dt <- data_table_posthoc()
  if (is.null(dt)) {return(NULL)}
  plot_general_data(dt[method == input$RT_Stats.DSC.method], x_attr = 'algId', y_attr = 'value', type = 'bar',
                    legend_attr = 'algId')
})

output$RT_Stats.DSC.PosthocViz <- renderPlotly({
  render_DSC_plot()
})

output$RT_Stats.DSC.DownloadTable <- downloadHandler(
  filename = function() {
    eval(RT_DSC_table_name)
  },
  content = function(file) {
    df <- data_table_posthoc()
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
    save_plotly(render_DSC_plot(), file)
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