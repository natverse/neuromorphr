#' xyz2swc_check: Check SWC files for standard format using xyz2swc tool from NeuroMorpho.org
#'
#' This function sends SWC files to NeuroMorpho.org so that it can check if those SWC files are in the standard format using its xyz2swc tool. The user gets a data frame describing, for each file, any errors it contains.
#'
#' @param source_dir A character string specifying the directory where the source SWC files are located.
#' @param source_file A character string specifying the full path to a single source SWC file.
#' @param neuromorpho_url A character string specifying the URL for the NeuroMorpho.org website (default is "https://neuromorpho.org").
#' @param ... Additional arguments.
#'
#' @return A data frame containing a description of any errors in the SWC files.
#'
#' @examples
#' \dontrun{
#' # check a single file
#' xyz2swc_check(source_file = "path/to/file.swc")
#'
#' # check all files in a directory
#' xyz2swc_check(source_dir = "path/to/directory/")
#' }
#' @seealso \code{\link{xyz2swc_convert}} for a function to convert into standard SWC files.
#'
#' @export
xyz2swc_check <- function(source_dir = NULL,
                    source_file = NULL,
                    neuromorpho_url = "https://neuromorpho.org",
                    ...){
  
  # Check if the API is healthy
  xyz2swc_is_api_healthy()
  
  # Get files to convert
  if(!is.null(source_file)){
    if(!grepl("swc$",basename(source_file))){
      stop("source_file must be the full path to a single SWC file")
    }
  }else if(is.null(source_dir)){
    stop("Please supply either a valid source_dir or source_file")
  }else{
    source_files <- list.files(source_dir, pattern = "swc$" , full.names = TRUE)
    if(is.null(source_files)){
      stop("source_dir must contain SWC files")
    }
    res <- pbapply::pblapply(source_files, function(x) xyz2swc_check(source_dir = NULL,
                                                          source_file=x, 
                                                          neuromorpho_url=neuromorpho_url))
    return(do.call(plyr::rbind.fill, res))
  }

  # Create the file object
  file_obj <- httr::upload_file(source_file, type = "text/csv")
  
  # Upload files
  headers <- c(
    accept = "application/json",
    `Content-Type` = "multipart/form-data"
  )
  folder <- random_string <- paste0(sample(letters, 10, replace = TRUE), collapse = "")
  res <- tryCatch(neuromorpho_fetch(path = sprintf("xyz2swc/checkfiles?folder=%s",folder), 
                           body = list(files=file_obj),
                           config = httr::add_headers(.headers = headers),
                           neuromorpho_url = neuromorpho_url,
                           encode = "multipart"), error = function(e) NULL)
  
  # Reformat
  if(is.null(res)){
    res.df <- data.frame(file = basename(source_file),
                         path = dirname(source_file),
                         check = "not_run")
    res.df[,c("Check","Value","Status","ErrorMsg")] <- NA
  }else{
    res.df <- rvest::html_table(rvest::read_html(res$data$checked[[1]]$report))
    res.df <- as.data.frame(res.df)[,c("Check","Value","Status","ErrorMsg")]
    res.df$file <- basename(source_file)
    res.df$path <- dirname(source_file)
    res.df$check <- res$data$checked[[1]]$status
  }
  
  # return
  res.df
  
}

#' Convert neuron morphology files to standard SWC format
#'
#' This function takes neuron morphology files and converts them into standardised SWC files.
#' It can take 23 different, common file types as input. The user gets a .zip file downloaded and
#' a data frame of any errors or corrections made to the files.
#'
#' @param source_dir A character string indicating the directory containing the source files.
#' @param source_file A character string indicating the full path to the source file.
#' @param target_zip A character string indicating the full path and filename for the resulting .zip file.
#' @param get_logs A logical indicating whether to create a log file of any errors or corrections made to the files.
#' @param neuromorpho_url A character string indicating the URL of the Neuromorpho website.
#' @param folder A character string indicating the name of the folder to store the converted files on the server.
#' @param ... Additional arguments to pass to `neuromorpho_fetch`.
#'
#' @return A data frame of any errors or corrections made to the files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert a single file
#' xyz2swc_convert(source_file = "path/to/source/file")
#'
#' # Convert all files in a directory
#' xyz2swc_convert(source_dir = "path/to/source/directory")
#'
#' # Specify a custom URL
#' xyz2swc_convert(source_file = "path/to/source/file", neuromorpho_url = "https://mycustomurl.com")
#'}
#' @seealso \code{\link{xyz2swc_check}} for a function to validate SWC files.
#' 
xyz2swc_convert <- function(source_dir = NULL,
                            source_file = NULL,
                            target_zip = file.path(getwd(),"xyz2swc.zip"),
                            get_logs = TRUE,
                            neuromorpho_url = "https://neuromorpho.org",
                            folder =  paste0(sample(letters, 10, replace = TRUE), collapse = ""),
                          ...){
  
  # Check if the API is healthy
  xyz2swc_is_api_healthy()
  
  # Get files to convert
  if(!is.null(source_file)){
    res.df <- xyz2swc_convert_one(source_file=source_file,
                                  folder=folder,
                                  neuromorpho_url=neuromorpho_url)
  }else if(is.null(source_dir)){
    stop("Please supply either a valid source_dir or source_file")
  }else{
    source_files <- list.files(source_dir, full.names = TRUE)
    res <- pbapply::pblapply(source_files, function(x) xyz2swc_convert_one(source_dir = NULL,
                                                                     source_file=x,
                                                                     folder=folder,
                                                                     neuromorpho_url=neuromorpho_url))
    res.df <- do.call(plyr::rbind.fill, res)
  }
  
  # Get data
  req <- httr::GET(url = file.path(neuromorpho_url, sprintf("xyz2swc/getzipped/%s",folder), fsep = "/"))
  httr::stop_for_status(req)
  
  # write the contents of the response object to a file
  writeBin(req$content, target_zip)
  
  # Write logs
  if(get_logs){
    
    # Format the date and time in the desired format
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Write
    utils::write.csv(res.df, file = file.path(dirname(target_zip),sprintf("%s_swc_conversion_log.csv",timestamp)))
  }
  # Return res.df
  res.df
  
}

# hidden
xyz2swc_is_api_healthy <- function(neuromorpho_url = "http://neuromorpho.org", ...){
  res = tryCatch(neuromorpho_fetch(path = "xyz2swc", neuromorpho_url = neuromorpho_url, ...),
                 error = function(e) NULL)
  if(is.null(res)){
    stop("Slow/no response from ", neuromorpho_url)
    FALSE
  }else if(grepl("UP",res$data$message)){
    TRUE
  }else{
    stop("Cannot talk to the API for ", neuromorpho_url)
    FALSE
  }
}

# hidden
xyz2swc_convert_one <- function(source_file = NULL,
                                neuromorpho_url = "https://neuromorpho.org",
                                folder =  paste0(sample(letters, 10, replace = TRUE), collapse = ""),
                                ...){
  # Create the file object
  file_obj <- httr::upload_file(source_file, type = "text/csv")
  
  # Upload files
  headers <- c(
    accept = "application/json",
    `Content-Type` = "multipart/form-data"
  )
  res <- tryCatch(neuromorpho_fetch(path = sprintf("xyz2swc/convertfiles?folder=%s",folder), 
                                    body = list(files=file_obj),
                                    config = httr::add_headers(.headers = headers),
                                    neuromorpho_url = neuromorpho_url,
                                    encode = "multipart"), error = function(e) NULL)
  # Reformat
  if(is.null(res)){
    res.df <- data.frame(file = basename(source_file),
                         path = dirname(source_file),
                         check = "not_run")
    res.df[,c("Check","Value","Status","ErrorMsg","Correction")] <- NA
  }else{
    res.df <- rvest::html_table(rvest::read_html(res$data$converted[[1]]$report))
    res.df <- as.data.frame(res.df)[,c("Check","Value","Status","ErrorMsg","Correction")]
    res.df$file <- basename(source_file)
    res.df$path <- dirname(source_file)
    res.df$check <- res$data$converted[[1]]$status
  }
  
  # Return
  res.df
}
