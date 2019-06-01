#' @title Read neurons from neuromorpho.org
#'
#' @description Read standardised neurons from neuromorpho.org, given a single, or vector of, neuron ID or neuron name. 
#' Neurons can be returned as SWC-style data frames or as a \code{nat} package neuron/neuronlist object, 
#' that can be be plotted in 3D using \code{rgl} and analysed with tools from the \code{nat} ecosystem. Each neuron
#' in the neuromorpho repository is represented by a name, general information (metadata), 
#' the original and standardised SWC files of the digital morphological reconstruction 
#' (see details on \href{http://neuromorpho.org/StdSwc1.21.jsp}{standardisation process}),
#' and a set of morphometric features (see details on available \href{http://neuromorpho.org/myfaq.jsp?id=qr4#QS3}{measures}).
#' @param neuron_name a neuron name, or vector of neuron names, as recorded in the neuromorpho database. Names and neuron IDs 
#' can be found by searching the repository, for example via \code{neuromorpho_search}
#' @param neuron_id a neuron ID, or vector of neuron IDs, as recorded in the neuromorpho database. If neuron_name is given
#' this supersedes \code{neuron_id}, which is then treated as if its value were \code{NULL}.
#' @param nat if TRUE, neurons are returned formatted as a \code{\link[nat]{neuron}} object, in a \code{\link[nat]{neuronlist}} 
#' See details for more information. Otherwise, a data frame is returned in the \href{http://www.neuronland.org/NLMorphologyConverter/MorphologyFormats/SWC/Spec.html}{SWC} file type format.
#' If TRUE, the resulting neuronlist object's associated meta data will be pulled using \code{neuromorpho_neuron_meta}
#' @param meta if TRUE, meta data is retrieved for the returned \code{neuronlist} or \code{list} object, 
#' using \code{neuromorpho_neuron_meta}.
#' @param light if TRUE, the only a subset of the full meta data for each neurons is returned with the resulting \code{\link[nat]{neuronlist}}.
#' @param batch.size the number of requests sent at once to the neuromorpho.org, using \code{\link[curl]{multi_run}}. 
#' Requests are sent to neuromorpho.org in parallel to speed up the process of reading neurons. Batches of queries are processed serially.
#' Increasing the value of \code{batch.size} may reduce read time.
#' @param find if TRUE, then we scrape each neuron's webpage to find the correct link to download its SWC file. This is more stable, but more time consuming, 
#' than setting \code{find = FALSE} and using the standard neuromorpho.org format for the download link. If the database changes, or you cannot find your neuron 
#' even though you know it exists, try setting \code{find = TRUE}
#' @param neuromorpho_url the base URL for querying the neuromorpho database, defaults to \url{http://neuromorpho.org}
#' @param progress if \code{TRUE} or a numeric value, a progress bar is shown. 
#' The bar progresses when each batch is completed.
#' If \code{TRUE}, or \code{100}, the bar completes where all batches are done.
#' @param ... methods passed to \code{neuromorpho_async_req}, or in some cases, \code{neuromorphr:::neuromorpho_fetch}
#' @details A single neuron can be read using using \code{neuromorpho_read_neuron},
#'  or multiple using \code{neuromorpho_read_neurons}. If \code{nat = TRUE}, 
#'  then neurons are returned as a \code{\link[nat]{neuron}} object,
#'  If multiple neurons are returned, they will be given together in a \code{\link[nat]{neuronlist}}. 
#'  This format and its manipulation is described in detail \href{https://jefferis.github.io/nat/}{here}.
#'  When using \code{neuromorpho_read_neurons}, meta data for the neuron is also returned
#'  using \code{\link{neuromorpho_neuron_meta}}. If \code{light = TRUE}, 
#'  then only a subset of this metadata is returned, i.e. the fields: 
#'  \code{neuron_id} 
#'  \code{neuron_name} 
#'  \code{species} 
#'  \code{brain_region}
#'  \code{cell_type} 
#'  \code{archive} .
#'  Note that since neurons are reconstructed from many different neural systems and species, 
#'  there is no 'standard' orientation. Instead, neuromorpho.org's standardisation process orients the 
#'  morphologies by placing the soma in the origin of coordinates and aligning the first three principal 
#'  components of all XYZ coordinates with heights, width, and depth. 
#' @return if \code{nat = TRUE}, then a neuronlist object is returned. 
#' If FALSE, then a list of data frames for neuron morphologies in 
#' \href{http://www.neuronland.org/NLMorphologyConverter/MorphologyFormats/SWC/Spec.html}{SWC} format are returned.
#' @seealso \code{\link{neuromorpho_neurons_info}}, \code{\link{neuromorpho_neurons_meta}}
#' @examples
#' \dontrun{ # Let's get all the elephant neurons in the repository
#' 
#' ## First, we need to find their names or IDs
#' elephant.df = neuromorpho_search(search_terms="species:elephant")
#' 
#' ## Let's see what cell types we have here
#' t = table(elephant.df$cell_type)
#' t
#' 
#' ## We have many pyramidal cells. Let's get those.
#' neuron_names = subset(elephant.df, cell_type == names(t)[which.max(t)])$neuron_name
#' 
#' ## Now we are ready to read some neurons!
#' elephant.principal.cells = neuromorpho_read_neurons(neuron_name = neuron_names, nat = TRUE)
#' 
#' ## Great, now we can plot them
#' nopen3d()
#' plot3d(elephant.principal.cells, soma=T)
#' 
#' ## And get some summary information on the skeleton we have retrieved
#' summary(elephant.principal.cells)
#' }
#' @export
#' @rdname neuromorpho_read_neurons
neuromorpho_read_neurons <- function(neuron_name = NULL, 
                                     neuron_id = NULL,
                                     nat = TRUE,
                                     batch.size = 2,
                                     meta = TRUE,
                                     light = TRUE,
                                     find = FALSE,
                                     progress = TRUE,
                                     neuromorpho_url = "http://neuromorpho.org",
                                     ...){
  neuromorpho_is_api_healthy()
  if(is.null(neuron_name)){
    neuron_name = neuromorpho_names_from_ids(neuron_id = neuron_id, neuromorpho_url = neuromorpho_url, ...)
  }
  if(meta|!find){
    df = neuromorpho_neurons_meta(neuron_name = neuron_name,
                                  neuron_id = NULL,
                                  light = light,
                                  neuromorpho_url = neuromorpho_url,
                                  progress = progress,
                                  ...)
    archives = sapply(df$archive, FirstLower)
    neuron_name = df$neuron_name
  }
  if(find){
    paths = paste0(neuromorpho_url,"/neuron_info.jsp?neuron_name=", neuron_name)
    swc.urls = unlist(neuromorpho_async_req(urls=paths, batch.size = batch.size, FUN = neuromorpho_get_swc_url, progress = progress, message = "finding neuromorpho neurons"))
  }else{
    swc.urls = sapply(seq_along(neuron_name), function(x)
      paste0("http://neuromorpho.org/dableFiles/", archives[x], "/CNG%20version/", neuron_name[x],".CNG.swc"))
  }
  resn = neuromorpho_async_req(swc.urls, batch.size = batch.size, FUN = neuromorpho_read_swc, progress = progress, message = "reading neuromorpho neurons")
  retrieved = !is.na(resn)
  resn = resn[retrieved]
  neuron_name = neuron_name[retrieved]
  if(nat){
    neurons = nat::neuronlist()
    success = c()
    for(s in 1:length(resn)){
      swc = resn[[s]]
      n = tryCatch(nat::as.neuronlist(nat::as.neuron(swc)), error = function(e) NULL)
      if(!is.null(n)){
        neurons = c(neurons, n)
        success = c(success, s)
      }
    }
  }else{
    neurons = resn
  }
  if(meta|nat){
    df = df[retrieved,]
    df = df[success,]
    attr(neurons, "df") = df
  }
  if(nat){
    names(neurons) = df$neuron_id
  }
  neurons
}

#' @export
#' @rdname neuromorpho_read_neurons
neuromorpho_read_neuron <- function(neuron_name = NULL, 
                                    neuron_id = NULL,
                                    nat = TRUE,
                                    neuromorpho_url = "http://neuromorpho.org", ...){
  ### Get Meta Data ###
  if(!is.null(neuron_id)){
    ### Get Neuron Name ###
    neuron_name = neuromorpho_names_from_ids(neuron_id = neuron_id, neuromorpho_url = neuromorpho_url, ...)
  }else if(is.null(neuron_name)){
    stop("Please supply either a valid neuromorpho neuron name or neuron ID")
  }
  ### Get SWC file ###
  res2 = neuromorpho_fetch(path = sprintf("neuron_info.jsp?neuron_name=%s", neuron_name), neuromorpho_url = neuromorpho_url, parse.json = FALSE, ...)
  text = httr::content(res2, as = "text", encoding = "UTF-8")
  search = "href=dableFiles.*Morphology File \\(Standardized\\)|Morphology File \\(Standardized\\).*href=dableFiles"
  html.split = unlist(strsplit(text,"</a>"))
  swc.url = gsub(".*href=|>.*", "", html.split[grepl(search,html.split)])
  swc = utils::read.csv(file.path(neuromorpho_url,swc.url))
  swc = swc[!grepl("#",unlist(swc)),]
  swc = do.call(rbind,lapply(swc, function(x) as.numeric(unlist(strsplit(x," ")))))
  swc = data.frame(swc)
  swc = swc[,!is.na(swc[1,])]
  colnames(swc) = c("PointNo","Label","X","Y","Z","W","Parent")
  rownames(swc) = swc$PointNo
  if(nat){
    ### Make nat neuron object ###
    nat::as.neuron(swc)
  }else{
    swc
  }
}

# hidden
neuromorpho_get_swc_url <- function(res, neuromorpho_url = "http://neuromorpho.org"){
  text = rawToChar(res$content)
  search = "href=dableFiles.*Morphology File \\(Standardized\\)|Morphology File \\(Standardized\\).*href=dableFiles"
  html.split = unlist(strsplit(text,"</a>"))
  swc = gsub(".*href=|>.*", "", html.split[grepl(search,html.split)])
  file.path(neuromorpho_url,swc)
}

# hidden
neuromorpho_read_swc <- function(res){
  text = rawToChar(res$content)
  text.split = unlist(strsplit(text,"\n"))
  swc = text.split[!grepl("#",text.split)]
  swc = do.call(rbind,lapply(swc, function(x) as.numeric(unlist(strsplit(x," ")))))
  swc = data.frame(swc)
  swc = swc[,!is.na(swc[1,])]
  if(length(swc)==0){
    warning("neuron could not be read from ", res$url)
    return(NA)
  }else{
    colnames(swc) = c("PointNo","Label","X","Y","Z","W","Parent")
    rownames(swc) = swc$PointNo
    swc 
  }
}




