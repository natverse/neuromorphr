#' @title Return measurements for queried neurons
#'
#' @description For each neuron corresponding to a given neuron name or neuron ID, return a list of measurements
#' stored on neuromorpho.org. See details for the sort of measurements you can get.
#' @inheritParams neuromorpho_read_neurons
#' @param data_frame if \code{TRUE}, a \code{data.frame} object is returned, rather than a \code{list} object, with the same information. If 
#' a nested list (dictionary-like) has been returned for a neuron, this option will result in some data loss.
#' @details 
#' neurmorpho.org calculates some basic measures for neuronal skeleton reconstructions. Distances are given in μm, and angles in °. Typical measures:
#' #' \itemize{
#'   \item neuron_name
#'   \item neuron_id
#'   \item surface (μm^2) -- Total arborisation surface area
#'   \item volume (μm^3) -- Total internal volume of the arborisation
#'   \item n_branch -- Total number of branches
#'   \item width  (μm) -- Neuronal width (95% of second principal component)
#'   \item height (μm) -- Neuronal height (95% of first principal component)
#'   \item depth (μm) -- Neuronal depth (95% of third principal component)
#'   \item diameter (μm) -- Average branch diameter
#'   \item eucDistance (μm) -- Maximum Euclidean (straight) distance from soma to tips
#'   \item pathDistance -- Maximum Path (along the tree) distance from soma to tips
#'   \item branch_Order -- Maximum Branch order (number of bifurcations from soma to tips)
#'   \item contraction -- Average Contraction (the ratio between Euclidean and path length calculated on each branch)
#'   \item fragmentation -- Total number of reconstruction points
#'   \item partition_asymmetry -- Topological asymmetry (average over all bifurcations of the absolute value of (n1-n2)/(n1+n2-2), where n1 and n2 are the numbers of tips in the two subtrees)
#'   \item Rall -- Rall's Ratio (average over all bifurcations of the sum of the diameters of the two daughters, elevated to 1.5, divided by the diameter of the parent, elevated to 1.5)
#'   \item pk_classic
#'   \item bif_ampl_local
#'   \item fractal_Dim
#'   \item soma_Surface (μm^2) -- Soma surface area
#'   \item n_stems -- Total number of branches (bifurcations plus terminations)
#'   \item n_bifs -- Total number of bifurcations
#'   \item bif_ampl_remote
#'   \item length (μm) -- Total arborisation length
#'   \item Local Bifurcation angle (average over all bifurcations of the angle between the first two daughter compartments)
#'   \item Remote Bifurcation angle (average over all bifurcations of the angle between the following bifurcations or tips)
#' }
#' 
#'  Neurons may also be searched by their morphological measures via the web browser at \url{http://neuromorpho.org/MorphometrySearch.jsp}
#' 
#'  For exact definitions and/or more information about these measurements, please visit the \href{http://cng.gmu.edu:8080/Lm/help/index.htm}{L-Measure} website. 
#'  Note that each of these parameters is extracted from the whole neuron (axon plus dendrites). 
#' @seealso 
#' \code{\link{neuromorpho_persistence_vectors}},
#' \code{\link{neuromorpho_read_neurons}}
#' @return a list of measurements for a neuron, or a list of lists if neuron_name/neuron_id is a vector with more than one element
#' @examples
#' # Let's get some measurements for the
#' 
#' # let's get neocortical neurons from both the African elephant and the humpback whale
#' big.neocortex.df = neuromorpho_search(search_terms= c("species:elephant,humpback whale",
#'                                                       "brain_region:neocortex"))
#' 
#' ## Pull measurements, in a data frame
#' measurements = neuromorpho_morphometry(big.neocortex.df$neuron_name, data_frame = TRUE)
#' 
#' ## Assign species column
#' measurements$species = big.neocortex.df[rownames(measurements),"species"]
#' 
#' ## Boxlot
#' boxplot(as.numeric(length)~species, data=measurements, notch=FALSE, 
#'      col=(c("deepskyblue1","firebrick1")),
#'      main="neocortical neuron volumes", xlab="species")
#' 
#' @export
#' @rdname neuromorpho_morphometry
neuromorpho_morphometry <- function(neuron_name = NULL, 
                                    neuron_id = NULL,
                                    batch.size = 10,
                                    data_frame = TRUE,
                                    neuromorpho_url = "http://neuromorpho.org",
                                    progress = TRUE,
                                    ...){
  neuromorpho_is_api_healthy()
  if(is.null(neuron_name)&is.null(neuron_id)){
    stop("Please supply either a valid neuromorpho neuron name or neuron ID")
  }else if(is.null(neuron_name)){
    paths = paste0(neuromorpho_url,"/api/morphometry/id/", neuron_id)
    neuron_name = neuromorpho_names_from_ids(neuron_id = neuron_id, neuromorpho_url = neuromorpho_url, ...)
  }else{
    paths = paste0(neuromorpho_url,"/api/morphometry/name/", neuron_name)
  }
  res = neuromorpho_async_req(urls = paths, FUN = neuromorpho_parse_json, batch.size = batch.size, progress = progress, message = "pulling neuromorpho measurements", ...)
  names(res) = neuron_name
  res = lapply(res, nullToNA)
  if(data_frame){
    res = do.call(rbind,res)
    res = as.data.frame(res)
    res[,] = unlist(res)
  }
  res
}

#' @title Return persistence vectors for queried neurons
#'
#' @description For each neuron corresponding to a given neuron name or neuron ID, return the persistence vectors
#' stored on neuromorpho.org. See details for a brief explanation for these persistence vectors.
#' @inheritParams neuromorpho_morphometry
#' @details 
#' Text taken from neuromorpho.org's \href{http://neuromorpho.org/myfaq.jsp}{FAQs}:
#' 
#' Persistent homology is a methodology to characterise and summarise geometric shapes, as well as to describe meaningful features across scales. 
#' In this approach, a given shape is summarised using a mathematical construct called a "filtration", 
#' which consists of a nested sequence of subsets of the original shape. One can think of a filtration as a 
#' specific way to grow and generate the shape being examined. As we "filter" through the shape using this sequence, 
#' new topological features may be created and some older ones may be destroyed. In the case of neuronal morphologies, 
#' the standard swc format represents a tree embedded in three-dimensional physical space, and the observed features could be, 
#' for instance, neurite branches. Persistent homology tracks the creation ("birth") and destruction ("death") of these 
#' topological features during the filtration process. The resulting births and deaths of features are summarised in a 
#' so-called persistence diagram: a set of 2D points whose (x, y) coordinates represent the birth and death times of the features. 
#' The life-time of a feature (death time minus birth time) is called the persistence of this feature, encoding how long this 
#' feature exists during the filtration.
#' 
#' In particular, for the persistence feature included here, the descriptor function is derived from a simplified representation 
#' of the neuron that only considers the roots, bifurcations, and terminations while ignoring all continuation points along 
#' the branches. In other words, we put a straight segment between any two tree nodes (with degree not equal to 2); the weight 
#' of this arc is its Euclidean length. The descriptor function is the total length of the unique path (i.e., the geodesic distance) 
#' from the tree root (as specified in the SWC file) to any point in the tree. We then filter the neuron tree by sweeping it in 
#' decreasing function values, and compute the induced persistence diagram. Intuitively, the set of points in the persistence diagram 
#' captures a nested branch decomposition of the neuron tree with respect to the chosen description.
#' 
#' Finally, we convert the persistence diagram summary into a 1D persistence feature vector. The first two numbers in the persistence 
#' vector file represent the range (minimum and maximum values) of the descriptor function. The remaining entries represent the 
#' function values at 100 positions sampled uniformly in this range.
#' 
#' The original software code to compute Persistence Vectors from SWC files is available open source at the 
#' \href{https://github.com/Nevermore520/NeuronTools}{NeuronTools} repository.
#' 
#' \strong{Reference}: 
#' Li Y, Wang D, Ascoli GA, Mitra P, Wang Y (2017) \emph{Metrics for comparing neuronal tree shapes based on persistent homology}. 
#' PLoS ONE 12(8): e0182184. DOI:10.1371/journal.pone.0182184. 
#' 
#' For each neuron, the following is returned:
#' \itemize{
#'   \item neuron_id
#'   \item scaling factor
#'   \item distance
#'   \item coefficients
#' }
#' @seealso
#' \code{\link{neuromorpho_morphometry}},
#' \code{\link{neuromorpho_read_neurons}}
#' @return a list of measurements for a neuron, or a list of lists if neuron_name/neuron_id is a vector with more than one element
#' @export
#' @rdname neuromorpho_persistence_vectors
neuromorpho_persistence_vectors <- function(neuron_name = NULL, 
                                    neuron_id = NULL,
                                    batch.size = 10,
                                    progress = TRUE,
                                    neuromorpho_url = "http://neuromorpho.org",
                                    ...){
  neuromorpho_is_api_healthy()
  if(is.null(neuron_name)&is.null(neuron_id)){
    stop("Please supply either a valid neuromorpho neuron name or neuron ID")
  }else if(is.null(neuron_name)){
    neuron_id = neuromorpho_ids_from_names(neuron_name = neuron_name, 
                                           neuromorpho_url = neuromorpho_url, 
                                           progress = FALSE, message = "obtaining persistence vectors", ...)
  }
  paths = paste0(neuromorpho_url, "/api/pvec/id/", neuron_id)
  res = neuromorpho_async_req(urls = paths, FUN = NULL, batch.size = batch.size, progress = progress, ...)
  res = res[!is.na(res)]
  res
}

