#' @title Convert neuromorpho neuron IDs into neuron names, and vica versa
#'
#' @description Every neuron in the neuromorpho repository has its own unique neuron ID, starting at 1,
#' and name. This can be mapped to a neuron name (also unique), or another (non-unique) neuron field entry. 
#' Names and IDs can be used to read neurons from neuromorpho.org, via \code{neuromorpho_read_neurons}. 
#' @inheritParams neuromorpho_read_neurons
#' @param field another field for neurons' metadata, i.e. one of the fields as returned by \code{neuromorpho_fields}
#' @details retrieves the meta data associated with a neuron ID / neuron name, in order to find its corresponding name / ID. 
#' This is useful because the name can be ued to directly 
#' locate a neuron's SWC file, but the neuron ID cannot.
#' @return a vector of field entries corresponding to the given neuron IDs / names
#' @seealso \code{\link{neuromorpho_search}}, 
#' \code{\link{neuromorpho_read_neurons}}
#' @examples
#' \dontrun{
#' # find the first 10 neurons uploaded to the neuromorpho respository
#' neuron_names = neuromorpho_names_from_ids(c(1:10))
#' 
#' # and see that we get the same IDs back if we search the other way
#' neuron_ids = neuromorpho_ids_from_names(neuron_names)
#' }
#' @export
#' @rdname neuromorpho_names_from_ids
neuromorpho_names_from_ids <- function(neuron_id,
                                       progress = TRUE,
                                       batch.size = 10,
                                       neuromorpho_url = "http://neuromorpho.org", ...){
  paths = paste0(neuromorpho_url,"/api/neuron/id/", neuron_id)
  res = neuromorpho_async_req(urls = paths, FUN = neuromorpho_parse_json, batch.size = batch.size, progress = progress, ...)
  neuron_names = sapply(res, function(r) r$neuron_name)
  names(neuron_names) = neuron_id
  neuron_names
}
#' @export
#' @rdname neuromorpho_names_from_ids
neuromorpho_ids_from_names <- function(neuron_name,
                                       progress = TRUE,
                                       batch.size = 10,
                                       neuromorpho_url = "http://neuromorpho.org", ...){
  paths = paste0(neuromorpho_url,"/api/neuron/name/", neuron_name)
  res = neuromorpho_async_req(urls = paths, FUN = neuromorpho_parse_json, batch.size = batch.size, progress = progress, ...)
  neuron_ids = sapply(res, function(r) r$neuron_id)
  names(neuron_ids) = neuron_name
  neuron_ids
}
#' @export
#' @rdname neuromorpho_names_from_ids
neuromorpho_field_entries_from_names <- function(neuron_name,
                                       progress = TRUE,
                                       batch.size = 10,
                                       neuromorpho_url = "http://neuromorpho.org",
                                       field = "archive",
                                       ...){
  paths = paste0(neuromorpho_url,"/api/neuron/name/", neuron_name)
  res = neuromorpho_async_req(urls = paths, FUN = neuromorpho_parse_json, 
                              batch.size = batch.size, progress = progress, 
                              message = paste0("reading ", field), ...)
  field_entries = sapply(res, function(r) paste(r[[field]],collapse="/"))
  names(field_entries) = neuron_name
  field_entries
}


neuromorpho_articles_from_neurons <- function(neuron_name = NULL,
                                              neuron_id = NULL,
                                              progress = TRUE,
                                              neuromorpho_url = "http://neuromorpho.org", ...){
  
}
  
  
  
  
