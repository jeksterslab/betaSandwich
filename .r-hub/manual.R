root <- rprojroot::is_rstudio_project
unlink(
  root$find_file(
    "betaSandwich.pdf"
  )
)
pack <- "betaSandwich"
path <- find.package(pack)
system(
  paste(
    shQuote(
      file.path(
        R.home("bin"),
        "R"
      )
    ),
    "CMD",
    "Rd2pdf",
    shQuote(
      path
    )
  )
)
