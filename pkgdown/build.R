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

envpkgd <- getNamespace("pkgdown")
R.utils::reassignInPackage("data_authors", "pkgdown", override_data_authors, keepOld=F)
# R.utils::reassignInPackage("build_authors", "pkgdown", override_build_authors, keepOld=F)
build_site(new_process = FALSE)

 