library(pkgdown)
override_data_authors <- function (pkg = ".") {
  pkg <- as_pkgdown(pkg)
  author_info <- data_author_info(pkg)
  all <- pkg %>% pkg_authors() %>% purrr::map(author_list,
                                              author_info)
  main <- all
  needs_page <- FALSE
  print_yaml(list(all = all, main = main, needs_page = needs_page))
}
override_build_authors <- function (pkg = ".") {
  pkg <- as_pkgdown(pkg)
  data <- list(pagetitle = "Authors", authors = data_authors(pkg)$all)
  render_page(pkg, "authors", data, "authors.html")
}
override_rmarkdown_format <- function (pkg, name, depth = 1L, data = list(), toc = TRUE) 
{
  template <- rmarkdown_template(pkg, name, depth = depth, 
                                 data = data)
  out <- rmarkdown::html_document(toc = toc, toc_depth = pkg$meta$toc$depth %||% 
                                    2, self_contained = FALSE, theme = NULL, template = template$path,
                                  df_print = "paged")
  out$knitr$opts_chunk <- fig_opts_chunk(pkg$figures, out$knitr$opts_chunk)
  attr(out, "__cleanup") <- template$cleanup
  out
}

envpkgd <- getNamespace("pkgdown")
R.utils::reassignInPackage("data_authors", "pkgdown", override_data_authors, keepOld=F)
# R.utils::reassignInPackage("build_authors", "pkgdown", override_build_authors, keepOld=F)
R.utils::reassignInPackage("build_rmarkdown_format", "pkgdown", override_rmarkdown_format, keepOld=F)
build_site(new_process = FALSE)

