#' @title Check whether the neuromorpho.org API is up and running
#'
#' @description Test that the base neuromorpho.org API url, \url{http://neuromorpho.org/api}, can be reached. 
#' The API provides access to the neuron information, 
#' morphometry data and literature via three main endpoints 
#' using conventional HTTP requests. These endpoints are used by function in this package
#' to read neurons from the repository, get meat information associated with those neurons, and 
#' perform limited searches of the literature describing neuronal morphologies.
#' @inheritParams neuromorpho_read_neurons
#' @details Checks to see whether status the neuromorpho.org API is 'UP' at \url{http://neuromorpho.org/api/health}
#' @return TRUE if the API can be reached, FALSE with a warning, if not
#' @seealso \code{\link{neuromorpho_search}}, 
#' \code{\link{neuromorpho_read_neurons}},
#' \code{\link{neuromorpho_neurons_info}}
#' @examples
#' \dontrun{
#' ## Check if the neuromorpho API is healthy
#' if(neuromorpho_is_api_healthy()){
#'      message("We can talk to neuromorpho.org!")
#' }else{
#'      message("Oh dear")
#' }
#' }
#' @export
#' @rdname neuromorpho_is_api_healthy
neuromorpho_is_api_healthy <- function(neuromorpho_url = "http://neuromorpho.org", ...){
  res = tryCatch(neuromorpho_fetch(path = "api/health", neuromorpho_url = neuromorpho_url, ...),
                 error = function(e) NULL)
  if(is.null(res)){
    stop("Slow/no response from ", neuromorpho_url)
    FALSE
  }else if(res$status=="UP"){
    TRUE
  }else{
    stop("Cannot talk to the API for ", neuromorpho_url)
    FALSE
  }
}

#' @title Make multiple asynchronous http requests to neuromorpho.org
#'
#' @description Make batches of AJAX style concurrent requests. Used to run multiple asynchronous tasks in parallel
#' and wait for them all to complete before further batch-wise processing.
#' @inheritParams neuromorpho_read_neurons
#' @param urls URLs to be queried
#' @param FUN a function to run on the responses to requests made using \code{urls}. 
#' If NULL, the response is returned without any processing. The default is to parse returned JSON format.
#' @param message a message to be given alongside progress bar
#' @param ... methods passed to \code{FUN}
#' @details Speed querying neuromorpho.org by making batches of AJAX style concurrent requests, 
#' using using \code{\link[curl]{multi_run}}, and running a custom function on the responses to these requests. 
#' @return a list of data retrieved from the requests defined by \code{urls}, and processed by \code{FUN}
#' @seealso  
#' \code{\link{neuromorpho_read_neurons}}
#' @examples 
#' \dontrun{ 
#' # Let's just grab metadata for all neurons in neuromorpho
#' all.species = neuromorpho_field_entries(field="species)
#' urls = paste0("http://neuromorpho.org/api/neuron/select?q=species:", all.species) 
#' res = neuromorpho_async_req(urls = urls, batch.size = 10, progress = TRUE, message = "having a look")
#' }
#' @export
#' @rdname neuromorpho_async_req
neuromorpho_async_req <- function(urls, 
                                  FUN = neuromorphr:::neuromorpho_parse_json, 
                                  batch.size = 10,
                                  progress = NULL, 
                                  message = NULL,
                                  ...){
  neuromorpho_is_api_healthy()
  check_batch_size(batch.size=batch.size)
  batches = ceiling(length(urls)/batch.size)
  i = 1
  data <- list()
  for(b in 1:batches){
    pool <- curl::new_pool()
    success <- function(res, ...){
      d <- if(is.null(FUN)){
        res
      }else{
        FUN(res, ...) 
      }
      data <<- c(data, list(d))
    }
    failure <- function(msg, verbose = verbose){
      warning("request for failed ", msg, "\n")
    }
    for(bi in 1:batch.size){
      if(i<=length(urls)){
        curl::curl_fetch_multi(urls[i], success, failure, pool = pool)
        i = i + 1 
      }
    }
    out <- curl::multi_run(pool=pool)
    while(out$pending>0){
      out <- curl::multi_run(pool=pool)
    }
    if(out$error>0){
      warning("neuromorpho request errors for ", out$error, " requests")
    }
    if(!is.null(progress)&!isFALSE(progress)){
      if(isTRUE(progress)){
        progress = 100
      }
      neuromorpho_progress(b/batches*100, max = progress, message = message)
    }
  }
  data
}

# hidden
neuromorpho_fetch <- function(path, 
                              body = NULL, 
                              neuromorpho_url = "http://neuromorpho.org", 
                              parse.json = TRUE, 
                              simplifyVector=FALSE, 
                              include_headers = FALSE, ...){
  path = gsub("\\/$|^\\/","",path)
  req <-
    if (is.null(body)) {
      httr::GET(url = file.path(neuromorpho_url, path, fsep = "/"), ...)
    }else {
      httr::POST(url = file.path(neuromorpho_url, path, fsep = "/"),
                 body = body, ...)
    }
  httr::stop_for_status(req)
  if (parse.json) {
    parsed = neuromorpho_parse_json(req, simplifyVector = simplifyVector, raw = FALSE)
    neuromorpho_error_check(parsed)
    if (include_headers) {
      fields_to_include = c("url", "headers")
      attributes(parsed) = c(attributes(parsed), req[fields_to_include])
    }
    parsed
  }
  else req
}

# hidden
neuromorpho_parse_json <- function (req, simplifyVector = FALSE, raw = TRUE, ...) {
  if(raw){
    text <- rawToChar(req$content)
  }else{
    text <- httr::content(req, as = "text", encoding = "UTF-8")
  }
  if (identical(text, "")){
    warning("No output to parse", call. = FALSE)
    return(NULL)
  }
  p = tryCatch(jsonlite::fromJSON(text, simplifyVector = simplifyVector, ...), error = function(e) NULL)
  if(is.null(p)){
    warning("error parsing JSON") 
  }
  nullToNA(p)
}

# hidden
neuromorpho_error_check <- function(x){
    err_fields = c("error", "message")
    if (sum(names(x) %in% err_fields)>1) {
      stop(x$error, ": ", x$message)
    }
    NULL
}

# hidden
check_batch_size <-function(batch.size){
  if (batch.size > 5){
    warning("Currently, the infastructure of neuromorpho.org cannot support too many requests at once,
            and does not have support for rate limiting. The API would be fine with 5 times per second
            and the web page once every 5 seconds. To prevent crashing the site, batch.size <= 5 is preferable, 
            yours is ", batch.size)
  }else{
    NULL
  }
}


