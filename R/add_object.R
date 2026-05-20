#' Function to add an object to the JSON
#'
#' @param input_list A list of fields corresponding to entries. Must include
#'  \code{name, description, Examples, ICES code type, Units, Rationale, Alternatives, Range of possible values}
#' @return An R list in the JSON format which includes the new term.
#' @export
add_object <- function(input_list) {
  json_obj <- jsonlite::fromJSON(system.file(
    "extdata",
    "top20.json",
    package = "fishdictionary"
  ))
  # Return an error if the object already exists
  if (input_list$name %in% json_obj[["name"]]) {
    #sapply(json_obj, FUN = get, x = "name")) {
    stop(input_list$name, " already exists.")
  }
  # Validate the json input and return an error if it fails
  if (
    jsonvalidate::json_validate(
      json = jsonlite::toJSON(input_list, auto_unbox = TRUE),
      schema = file.path("inst", "extdata", "schema.json"),
      verbose = TRUE,
      error = TRUE
    )
  ) {
    add <- dim(json_obj)[1] + 1
    json_obj[add, ] <- input_list
    return(json_obj)
  }
}
