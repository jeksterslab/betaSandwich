data_process_nas1982 <- function(overwrite = FALSE) {
  root <- rprojroot::is_rstudio_project
  nas1982_rda <- root$find_file(
    "data",
    "nas1982.rda"
  )
  if (!file.exists(nas1982_rda)) {
    write <- TRUE
  } else {
    if (overwrite) {
      write <- TRUE
    } else {
      write <- FALSE
    }
  }
  if (write) {
    nas1982 <- read.csv(
      root$find_file(
        ".data-raw",
        "nas1982.txt"
      )
    )
    save(
      nas1982,
      file = root$find_file(
        "data",
        "nas1982.rda"
      ),
      compress = FALSE
    )
  }
}
