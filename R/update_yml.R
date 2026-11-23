#' Function to add new terms to _pkgdown.yml
#'
#' @return A message if the _pkgdown.yml has been updated.
#' @export
update_yml <- function() {
  # read in _pkgdown.yml file
  yaml <- yaml::read_yaml("_pkgdown.yml") |>
    suppressWarnings()
  existing_yaml_terms <- yaml$reference[[1]]$contents |>
    as.vector()
  
  # list terms' files in man folder, and subtract ".Rd" file extension
  documented_terms <- list.files("man", pattern = "\\.Rd$", full.names = FALSE) |>
    tools::file_path_sans_ext() |>
    as.vector() |>
    # remove helper functions' documentation
    setdiff(c("add_object", "read_in", "shiny_dd", "validate_Rd"))
  
  # add new terms to the _pkgdown.yml file
  if (any(documented_terms %notin% existing_yaml_terms)) {
    new_terms <- documented_terms[documented_terms %notin% existing_yaml_terms]
    yaml$reference[[1]]$contents <- sort(c(existing_yaml_terms, new_terms))
    yaml::write_yaml(yaml, "_pkgdown.yml")
    message("The _pkgdown.yml file has been updated with new definitions.")
  } else {
    message("No new definitions to add to the _pkgdown.yml file.")
  }
}
