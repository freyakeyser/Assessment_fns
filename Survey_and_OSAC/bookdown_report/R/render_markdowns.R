# name of the parameterised report rmarkdown
rmarkdown_file <- "parameterised_report.Rmd"
print(getwd())
# the parameter we're going to pass to the report
banks <- c(#"Ban", "BanIce",
          "Mid", "Sab", "Ger",
          "BBs",
         "BBn", "GB"#,
  #"GBa", "GBb"
           )
banknum <- 1:length(banks)

# index file
if(!any(c("GBa", "GBb") %in% banks)) index_file <- "index.Rmd"
if(any(c("GBa", "GBb") %in% banks)) index_file <- "index2.Rmd"

# output file
if(!any(c("GBa", "GBb") %in% banks)) to = "output.Rmd"
if(any(c("GBa", "GBb") %in% banks)) to = "output2.Rmd"

# Run Parameterised Reports ----------------------------------------------------
# run through the years and render each version of the report.
# the important part here is "run_pandoc = FALSE" which will leave us with
# a .md file for each report.
markdowns <- lapply(
  banks,
  function(x, filename){

    rmarkdown::render(filename, params = list(bank = x, banknum=which(x==banks)), run_pandoc = FALSE)

    base_filename <- xfun::sans_ext(filename)
    # filename.knit.md will be created by the render
    knit_file <- xfun::with_ext(filename, ".knit.md")
    # to avoid saving over it each time, rename it
    new_file_name <- xfun::with_ext(paste0(base_filename, "_", x), ".knit.md")
    file.rename(knit_file, new_file_name)

    # return our new file name
    new_file_name

  },
  filename = rmarkdown_file
)

# Run index file ---------------------------------------------------------------
# do the same for the index file
rmarkdown::render(index_file, run_pandoc = FALSE)
index_file <- xfun::with_ext(xfun::sans_ext(index_file), ".knit.md")

# add the index file to the list of other markdowns
markdowns <- append(index_file, markdowns)

# # if you have a "special" bank-specific rmd, put it here.
# extra_file <- "parameterised_report_GB.Rmd"
# rmarkdown::render(extra_file, run_pandoc = FALSE)
# extra_file <- xfun::with_ext(xfun::sans_ext(extra_file), ".knit.md")
# markdowns <- append(markdowns, extra_file)

# Stitch together markdowns ----------------------------------------------------

# paste the markdowns together
# this is taken from bookdown:::merge_chapters()
content = unlist(mapply(markdowns, markdowns, SIMPLIFY = FALSE, FUN = function(f, o) {
  x = xfun::read_utf8(f)
  # add a comment at the end of each file
  c(x, '', paste0('<!--chapter:end:', o, '-->'), '')
}))

# write to a new file
xfun::write_utf8(content, to)
Sys.chmod(to, '644')

# Render one big Rmarkdown -----------------------------------------------------
rmarkdown::render(to, run_pandoc = TRUE)

# Clean up ---------------------------------------------------------------------
# get rid of all of the extra markdowns we made
file.remove(to)
unlink(unlist(markdowns))
