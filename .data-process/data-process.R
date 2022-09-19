# find root directory
root <- rprojroot::is_rstudio_project
source(
  root$find_file(
    ".data-process",
    "data-process-nas1982.R"
  )
)
data_process_nas1982(overwrite = TRUE)
