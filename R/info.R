#' @title Get all meta data associated with a neuron
#'
#' @description Retrieve the meta data associated with query neurons. Neurons can be queried using either their
#' neuron ID or their name. 
#' @inheritParams neuromorpho_read_neurons
#' @details The meta data for reconstructions has been extracted by neuromorpho.org from corresponding publications, 
#' and generally include:
#' \itemize{
#'   \item Web URL of archives (if available) with any additional information about the reconstruction
#'   \item Subject-related information - species, strain, age, weight, sex
#'   \item Experiment-related information - protocol, experimental condition, staining method, slicing direction and thickness, objective type and magnification.
#'   \item Brain region at three levels: main region/structure, sub-region, detailed sub-region (example: Hippocampus, dentate gyrus, granule cell layer)
#'   \item Neuron type at three levels: main neuron class type, sub-class, detailed sub-class (example: Interneuron, basket cell, nested)
#'   \item Reconstruction method
#'   \item Format of original data, though currently this package only retrieves the data as standardised by neuromorpho.org
#' }
#' @return a list containing all the meta data for a neuron stored on neuromorpho.org
#' @seealso \code{\link{neuromorpho_search}}, 
#' \code{\link{neuromorpho_field_entries}},
#' \code{\link{neuromorpho_fields}}
#' @examples
#' \dontrun{ 
#' # These are some names for a few elephant neurons
#' elephant_neuron_names = c("155-2-1Stel", "155-2-2Stel", "155-2-5Stel")
#' 
#' # We can now get the meta information associated with these reconstructions
#' elephant_neuron_info = neuromorpho_neurons_info(elephant_neuron_names)
#' 
#' # We can get essentially the same information as a data frame instead
#' elephant_neuron_meta = neuromorpho_neurons_meta(elephant_neuron_names)
#' }
#' @export
#' @rdname neuromorpho_neurons_info
neuromorpho_neurons_info <- function(neuron_name = NULL, 
                                     neuron_id = NULL,
                                     batch.size = 10,
                                     progress = TRUE,
                                     neuromorpho_url = "http://neuromorpho.org", ...){
  if(is.null(neuron_name)){
    paths = paste0(neuromorpho_url,"/api/neuron/id/", neuron_id)
  }else{
    paths = paste0(neuromorpho_url,"/api/neuron/name/", neuron_name)
  }
  res = neuromorpho_async_req(urls = paths, FUN = neuromorpho_parse_json, batch.size = batch.size, 
                              progress = progress, message = "reading neuromorpho metadata", ...)
  names(res) = ifelse(is.null(neuron_name),neuron_id,neuron_name)[!is.na(res)]
  res = res[!is.na(res)]
  nullToNA(res)
}

# hidden 
neuromorpho_neuron_info <- function(neuron_name = NULL, 
                                    neuron_id = NULL,
                                    neuromorpho_url = "http://neuromorpho.org", 
                                    ...){
  if(!is.null(neuron_id)){
    res = neuromorpho_fetch(path = sprintf("api/neuron/id/%i", neuron_id), neuromorpho_url = neuromorpho_url, ...)
  }else if(is.null(neuron_name)){
    stop("Please supply either a valid neuromorpho neuron name or neuron ID")
  }else{
    res = neuromorpho_fetch(path = sprintf("api/neuron/name/%s", neuron_name), neuromorpho_url = neuromorpho_url, ...)
  }
  nullToNA(res)
}

#' @title Get some of the meta data associated with a neuron
#'
#' @description Retrieve some the meta data associated with query neurons, in a compact way. Neurons can be queried using either their
#' neuron ID or their name. 
#' @inheritParams neuromorpho_read_neurons
#' @details The meta data for reconstructions has been extracted by neuromorpho.org from corresponding publications. 
#' Some of these neuron fields will have multiple entries, 
#' for example there might be multiple designations for a cell type, or brain region, 
#' or multiple labs count be credited for a neuron's reconstruction. 
#' This function returns the majority of the information stored on neuromorpho.org for a given set of neurons, 
#' but collapses some field entries into one field entry, so we can use a simpler data structure (a data.frame) to represent 
#' this information, rather than a list. This is useful for adding to \code{neuronlist} objects as an attribute, as is standard in 
#' \code{nat} family packages. If \code{light = TRUE}, then only the following information is returned in the final data frame:
#' \itemize{
#'   \item neuron_id
#'   \item neuron_name
#'   \item species
#'   \item brain_region
#'   \item cell_type
#'   \item archive
#' }
#' Use \code{\link{neuromorpho_neurons_info}} if you need the full complement of information for a neuron.
#' @return a data frame, with one entry per neuron queried
#' @seealso \code{\link{neuromorpho_search}}, 
#' \code{\link{neuromorpho_neurons_info}},
#' \code{\link{neuromorpho_fields}}
#' @examples
#' \dontrun{
#' # These are some names for a few elephant neurons
#' elephant_neuron_names = c("155-2-1Stel", "155-2-2Stel", "155-2-5Stel")
#' 
#' # We can now get the meta information associated with these reconstructions
#' elephant_neuron_info = neuromorpho_neurons_info(elephant_neuron_names)
#' 
#' # We can get essentially the same information as a data frame instead
#' elephant_neuron_meta = neuromorpho_neurons_meta(elephant_neuron_names)
#' }
#' @export
#' @rdname neuromorpho_neurons_meta
neuromorpho_neurons_meta <- function(neuron_name = NULL, 
                                    neuron_id = NULL,
                                    light = TRUE,
                                    progress = TRUE,
                                    batch.size = 10,
                                    neuromorpho_url = "http://neuromorpho.org", ...){
  if(is.null(neuron_name)){
    paths = paste0(neuromorpho_url,"/api/neuron/id/", neuron_id)
  }else{
    paths = paste0(neuromorpho_url,"/api/neuron/name/", neuron_name)
  }
  res = neuromorpho_async_req(urls = paths, FUN = neuromorpho_parse_json, batch.size = batch.size, 
                              progress = progress, message = "reading neuromorpho metadata", ...)
  res = res[!is.na(res)]
  l = lapply(res, neuromorpho_process_meta)
  df = do.call(rbind, l)
  df = df[!duplicated(df$neuron_name),]
  df[,] = unlist(df)
  rownames(df) = df$neuron_name
  if(light){
    df = df[,c("neuron_id", "neuron_name", "species", "brain_region", "cell_type", "archive")]
  }
  df
}

#' @export
#' @rdname neuromorpho_neurons_meta
neuromorpho_neuron_meta <- function(neuron_name = NULL, 
                                    neuron_id = NULL,
                                    light = TRUE,
                                    neuromorpho_url = "http://neuromorpho.org", ...){
  res = neuromorpho_neuron_info(neuron_name=neuron_name,
                                neuron_id=neuron_id,
                                neuromorpho_url, ...)
  ### Process neuron meta data ###
  df = neuromorpho_process_meta(res)
  df[,] = unlist(df)
  if(light){
    df = df[,c("neuron_id", "neuron_name", "species", "brain_region", "cell_type", "archive")]
  }
  df
}

# hidden
neuromorpho_process_meta <- function(res){
  df = data.frame(# ID
    neuron_id = res$neuron_id,
    neuron_name = res$neuron_name,
    # Species
    species = paste(res$species,collapse= "/"),
    strain = paste(res$strain,collapse= "/"),
    scientific_name = paste(res$scientific_name,collapse= "/"),
    # Neuranatomy
    brain_region = paste(res$brain_region,collapse= "/"),
    cell_type = paste(res$cell_type,collapse= "/"),
    domain = paste(res$domain,collapse= "/"),
    physical_Integrity = paste(res$physical_Integrity,collapse= "/"),
    # Metrics
    soma_surface = paste(res$soma_surface,collapse= "/"),
    surface = paste(res$surface,collapse= "/"),
    volume = paste(res$volume,collapse= "/"),
    # Animal
    sex = paste(res$gender,collapse= "/"),
    age_classification = paste(res$age_classification,collapse= "/"),
    min_age = paste(res$min_age,collapse= "/"),
    max_age = paste(res$max_age,collapse= "/"),
    min_weight = paste(res$min_weight,collapse= "/"),
    max_weight = paste(res$max_weight,collapse= "/"),
    # Sample
    stain = paste(res$stain,collapse= "/"),
    experiment_condition = paste(res$experiment_condition,collapse= "/"),
    protocol = paste(res$protocol,collapse= "/"),
    # Imaging
    slicing_direction = paste(res$slicing_direction,collapse= "/"),
    slicing_thickness = paste(res$slicing_thickness,collapse= "/"),
    objective_type = paste(res$objective_type,collapse= "/"),
    original_format = paste(res$original_format,collapse= "/"),
    attributes = paste(res$attributes,collapse= "/"),
    magnification = paste(res$magnification,collapse= "/"),
    shrinkage_reported = paste(res$shrinkage_reported,collapse= "/"),
    shrinkage_corrected = paste(res$shrinkage_corrected,collapse= "/"),
    reported_value = paste(res$reported_value,collapse= "/"),
    reported_xy = paste(res$reported_xy,collapse= "/"),
    reported_z = paste(res$reported_z,collapse= "/"),
    corrected_value = paste(res$corrected_value,collapse= "/"),
    corrected_xy = paste(res$corrected_xy,collapse= "/"),
    corrected_z = paste(res$corrected_z,collapse= "/"),
    reconstruction_software = paste(res$reconstruction_software,collapse= "/"),
    # Reference
    archive = paste(res$archive,collapse= "/"),
    date = paste(res$deposition_date,collapse= "/"),
    reference_doi = res$reference_doi[[1]],
    reference_pmid = res$reference_pmid[[1]]
  )
  nullToNA(df)
}
