output$Report.Generate <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = "report.pdf",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    # shinyjs::alert("Test")
    # tempReport <- file.path(tempdir(), "Template_test.Rmd")
    # file.copy("RMD/Template_test.Rmd", tempReport, overwrite = TRUE)
    # shinyjs::alert(tempReport)
    
    # Set up parameters to pass to Rmd document
    # params <- list(input = input)
    generate_report(file)
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    # rmarkdown::render(tempReport, output_file = file,
    #                   params = params,
    #                   envir = new.env(parent = globalenv())
    # )
  },
  contentType = "pdf"
)

generate_report <- function(file){
  # shinyjs::alert("Test")
  dir_loc <- tempdir()
  tempReport <- file.path(dir_loc, "Template_test.Rmd")
  file.copy("RMD/Template_test.Rmd", tempReport, overwrite = TRUE)
  file.copy("RMD/Single_ERT.Rmd", file.path(dir_loc, "Single_ERT.Rmd"), overwrite = TRUE)
  file.copy("RMD/Multi_ERT.Rmd", file.path(dir_loc, "Multi_ERT.Rmd"), overwrite = TRUE)
  file.copy("RMD/FV_Rank.Rmd", file.path(dir_loc, "FV_Rank.Rmd"), overwrite = TRUE)
  
  file.copy("RMD/bibliography.bib", file.path(dir_loc, "bibliography.bib"))
  # shinyjs::alert(tempReport)
  
  # Set up parameters to pass to Rmd document
  params <- list(input = input, dsl = DATA_RAW(), figure_folder = dir_loc, REG = REG)
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(tempReport, output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv()))
}