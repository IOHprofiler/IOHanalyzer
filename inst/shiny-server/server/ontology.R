
# set up list of datasets (scan the repository, looking for .rds files)
observe({
  req(input$Ontology.Source)
  input_source <- input$Ontology.Source
  available_functions <- get_ontology_var("Fid", input_source)
  available_dimensions <- get_ontology_var("DIM", input_source)
  available_algorithms <- get_ontology_var("AlgId", input_source)
  available_iid <- get_ontology_var("Iid", input_source)
  available_suite <- get_ontology_var("Suite", input_source)
  
  if (is.null(available_functions) || length(available_functions) == 0) {# TODO: the alert msg should be updated
    shinyjs::alert("No connection could be made with the OPTION ontology. Please try again later.")
    shinyjs::disable('Ontology.Load')
    return() 
  }
  updateSelectInput(session, 'Ontology.Functions', choices = available_functions, selected = available_functions)
  updateSelectInput(session, 'Ontology.Dimensions', choices = available_dimensions, selected = available_dimensions)
  updateSelectInput(session, 'Ontology.Algorithms', choices = available_algorithms, selected = available_algorithms)
  updateSelectInput(session, 'Ontology.Iids', choices = available_iid, selected = available_iid)
  updateSelectInput(session, 'Ontology.NG_Suite', choices = available_suite, selected = available_suite[[1]])
  shinyjs::enable('Ontology.Load')
  
})

# load data from ontology according to selected options
observeEvent(input$Ontology.Load, {
  req(input$Ontology.Source)
  
  
  if (length(DataList$data) > 0 && attr(data, 'maximization') != attr(DataList$data, 'maximization')) {
    shinyjs::alert(paste0("Attempting to add data from a different optimization type to the currently",
                          " loaded data.\nPlease either remove the currently loaded data or",
                          " choose a different dataset to load."))
    return(NULL)
  }
  
  DataList$data <- c(DataList$data, data)
  update_menu_visibility(attr(DataList$data, 'suite'))
  # set_format_func(attr(DataList$data, 'suite'))
  algids <- get_algId(DataList$data)
  if (!all(algids %in% get_color_scheme_dt()[['algnames']])) {
    set_color_scheme("Default", algids)
  }
})

