data_process_stevens <- function(overwrite = FALSE) {
  root <- rprojroot::is_rstudio_project
  stevens_rda <- root$find_file(
    "data",
    "stevens.rda"
  )
  if (!file.exists(stevens_rda)) {
    write <- TRUE
  } else {
    if (overwrite) {
      write <- TRUE
    } else {
      write <- FALSE
    }
  }
  if (write) {
    stevens <- read.csv(
      root$find_file(
        ".data-raw",
        "stevens.txt"
      )
    )
    save(
      stevens,
      file = root$find_file(
        "data",
        "stevens.rda"
      )
    )
  }
}
