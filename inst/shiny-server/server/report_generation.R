output$Report.Generate <- downloadHandler(
  filename = "report.pdf",
  content = function(file) {
    generate_report(file)
  },
  contentType = "pdf"
)

output$Report.Generate.Tar <- downloadHandler(
  filename = "report.tar",
  content = function(file) {
    get_tarbal_report(file)
  },
  contentType = "tar"
)

generate_report <- function(file){
  dir_loc <- tempdir()
  dir.create(file.path(dir_loc, "Report"), showWarnings = F)
  file.copy("RMD/Report/", file.path(dir_loc), overwrite = TRUE, recursive = TRUE)
  tempReport <- file.path(file.path(dir_loc, "Report"), "Template_test.Rmd")
  params <- list(input = input, dsl = DATA_RAW(), figure_folder = dir_loc, REG = REG)
  rmarkdown::render(tempReport, output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv()))
}

get_tarbal_report <- function(file){
  dir_loc <- tempdir()
  if(length(list.files(dir_loc, pattern = ".tex")) == 0){
    generate_report(tempfile(fileext = "pdf"))
  }
  dir.create(file.path(dir_loc, "Report_files"), showWarnings = F)
  file.copy(list.files(dir_loc, pattern = ".tex", full.names = T), file.path(dir_loc, "Report_files"))
  file.copy(list.files(file.path(dir_loc, "Report"), pattern = ".pdf", full.names = T), file.path(dir_loc, "Report_files"))
  tar(tarfile = file, files = file.path(dir_loc, "Report_files"))
  file.remove(list.files(file.path(dir_loc, "Report_files"), full.names = T))
  file.remove(list.files(file.path(dir_loc, "Report"), pattern = ".pdf", full.names = T))
  file.remove(file.path(dir_loc, list.files(dir_loc, pattern = ".tex")))
}