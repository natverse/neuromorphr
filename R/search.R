#' @title Return the available meta data fields for neuromorpho neurons
#'
#' @description Returns a vector of neuron fields available from the neuromorpho.org database.
#' These fields can be used in the field section of the custom queries.
#' @inheritParams neuromorpho_read_neurons
#' @details All the data fields can be seen and explored on neuromorpho.org
#' at \url{http://neuromorpho.org/MetaData.jsp}. 
#' @seealso \code{\link{neuromorpho_neurons_info}}, 
#' \code{\link{neuromorpho_read_neurons}}, 
#' \code{\link{neuromorpho_field_entries}}
#' @export
#' @rdname neuromorpho_fields
neuromorpho_fields <- function(neuromorpho_url = "http://neuromorpho.org", 
                               ...){
  neuromorpho_is_api_healthy()
  res = unlist(neuromorpho_fetch(path = "api/neuron/fields", neuromorpho_url = neuromorpho_url, ...))
  names(res) = NULL
  res
}

#' @title Return the available meta data entires for neuromorpho neuron fields
#'
#' @description Returns a list of values present in the repository for the neuron field requested.
#' These values can be used in the search criteria section of the custom queries.
#' @inheritParams neuromorpho_read_neurons
#' @param field a valid neuron field, as returned by \code{neuromorpho_fields}
#' @details All the data fields, and their entries, can be seen and explored on neuromorpho.org
#' at \url{http://neuromorpho.org/MetaData.jsp}. 
#' @seealso \code{\link{neuromorpho_neurons_info}}, 
#' \code{\link{neuromorpho_read_neurons}},
#' \code{\link{neuromorpho_fields}}
#' @export
#' @rdname neuromorpho_field_entries
neuromorpho_field_entries <- function(field = "species", 
                                      neuromorpho_url = "http://neuromorpho.org", 
                                      ...){
  f = neuromorpho_fields(neuromorpho_url=neuromorpho_url,...)
  if(!field%in%f){
    stop("The given field ", f," is not available, to see available fields, call neuromorpho_fields()")
  }
  res = neuromorpho_fetch(path = paste0("api/neuron/fields/",field), neuromorpho_url = neuromorpho_url, ...)
  fields = unlist(res$fields)
  names(fields) = NULL
  ### Read multiple pages of data ###
  pages = res$page
  if(pages$totalPages>1){
    for(page in 1:(pages$totalPages-1)){ # Pages are numbered from 0
      resp = neuromorpho_fetch(path = sprintf("api/neuron/fields/%s?page=%i",field, page), neuromorpho_url = neuromorpho_url, ...)
      fieldsp = unlist(resp$fields)
      names(fieldsp) = NULL
      fields = c(fields, fieldsp)
    }
  }
  fields
}

#' @title Make a custom neuron search to the neuromorpho.org repository
#'
#' @description Search for neurons in the neuromorpho.org repository. The returned 
#' data frame can  be used to get neuron IDs and neuron names, that can be fed to \code{neuromorpho_read_neurons}, 
#' in order to read neurons from neuromorpho.org. 
#' Searches are made by looking at the meta data for neurons in the repositroy, i.e. querying for certain entries in certain neuron fields.
#' @inheritParams neuromorpho_read_neurons
#' @param search_terms a vector of search terms, with each term formatted as \code{"field:field entry"}.
#' All valid neuron fields can be seen by calling \code{neuromorpho_fields}, and all the entries for certain fields
#' can be seen using \code{neuromorpho_fields_entries}.
#' Only meta data for neurons that have the entries described for all given search terms will be returned.
#' To find neurons that are in either of two field entries, one can use, for example, \code{"species:rat,mouse"} to return 
#' all rat or mouse neurons.
#' @details All the data fields, and their entries, can be seen and explored on neuromorpho.org
#' at \url{http://neuromorpho.org/MetaData.jsp}.
#' @return a data data frame, where each entry is a neuron matching the query specifications.
#' @seealso \code{\link{neuromorpho_field_entries}}, 
#' \code{\link{neuromorpho_read_neurons}},
#' \code{\link{neuromorpho_fields}}
#' @export
#' @rdname neuromorpho_search
neuromorpho_search <- function(search_terms = c("archive:Jacobs", "species:elephant", "brain_region:cerebellum"),
                               neuromorpho_url = "http://neuromorpho.org", 
                               ...){
  f = neuromorpho_fields(neuromorpho_url=neuromorpho_url,...)
  fields = gsub(":.*","",search_terms)
  if(sum(!fields%in%f)){
    stop("The given field(s) ", fields," are not available, to see available fields, call neuromorpho_fields()")
  }
  search_terms = sapply(search_terms,function(st) paste(unlist(strsplit(st," ")),collapse="?"))
  search_terms = paste(search_terms, collapse = "&fq=")
  res = neuromorpho_fetch(path = paste0("api/neuron/select?q=",search_terms), neuromorpho_url = neuromorpho_url, ...)
  df = neuromorpho_unpack_search(res)
  ### Read multiple pages of data ###
  pages = res$page
  if(pages$totalPages>1){
    for(page in 1:(pages$totalPages-1)){ # Pages are numbered from 0
      resp = neuromorpho_fetch(path = paste0("api/neuron/select?q=",search_terms,"&page=",page),
                               neuromorpho_url = neuromorpho_url, ...)
      df = rbind(df, neuromorpho_unpack_search(resp))
    }
  }
  df = df[!duplicated(df$neuron_name),]
  rownames(df) = df$neuron_name
  df
}

#' @title Get the number of different entries for a neuromorpho neuron field
#'
#' @description Returns a vector of counts for every field entry for a given neuromorpho neuron field.
#' E.g. you can find out how many neurons there are in the neuromorpho.org repository for each species it has data on. 
#' @inheritParams neuromorpho_field_entries
#' @param field a valid neuron field, as returned by \code{neuromorpho_fields}
#' @details All the data fields, and their entries, can be seen and explored on neuromorpho.org
#' at \url{http://neuromorpho.org/MetaData.jsp}. Coutns can be seen as pie charts under the 'browse'
#' tab at \url{http://neuromorpho.org}, for example, \url{http://neuromorpho.org/byspecies.jsp}.
#' @seealso \code{\link{neuromorpho_search}}, 
#' \code{\link{neuromorpho_field_entries}},
#' \code{\link{neuromorpho_fields}}
#' @examples 
#' \dontrun{
#' # Get counts for the number of neurons the repository has, by species
#' species.count = neuromorpho_field_counts(field = "species")
#' species.count.sorted = sort(species.count,decreasing=TRUE)
#' species.count.top = c(species.count.top[1:7], 
#'                       sum(species.count.sorted[8:length(species.count.sorted)]))
#' names(species.count.top)[length(species.count.top)] = "other"
#' 
#' # Plot
#' pie(x=species.count.top, labels = names(species.count.top), 
#'     main="neuron morphologies from different species")
#' 
#' }
#' @export
#' @rdname neuromorpho_field_counts
neuromorpho_field_counts <- function(field = "species", 
                                     neuromorpho_url = "http://neuromorpho.org", 
                                     ...){
  f = neuromorpho_fields(neuromorpho_url=neuromorpho_url,...)
  if(!field%in%f){
    stop("The given field ", f," is not available, to see available fields, call neuromorpho_fields()")
  }
  res = neuromorpho_fetch(path = paste0("api/neuron/partition/",field), neuromorpho_url = neuromorpho_url, ...)
  fields = unlist(res$fields)
  ### Get missing data ###
  pages = res$page
  if(pages$totalPages>1){
    for(page in 1:(pages$totalPages-1)){ # Pages are numbered from 0
      resp = neuromorpho_fetch(path = sprintf("api/neuron/fields/%s?page=%i",field, page), neuromorpho_url = neuromorpho_url, ...)
      fieldsp = unlist(resp$fields)
      fields = c(fields, fieldsp)
    }
  }
  fields.num = suppressWarnings(as.numeric(fields))
  names(fields.num) = names(fields)
  names(fields.num)[is.na(fields.num)] = fields[is.na(fields.num)]
  fields.num[is.na(fields.num)] = 1
  sort(fields.num, decreasing = TRUE)
}

# Hidden
neuromorpho_unpack_search <- function(res){
  l = list()
  if(is.null(res$`_embedded`)){
    warning("no reults found")
    return(NULL)
  }else{
    for(i in 1:length(res$`_embedded`$neuronResources)){
      dfi = neuromorpho_process_meta(nullToNA(res$`_embedded`$neuronResources[[i]]))
      l[[i]] = dfi
    }
    do.call(rbind,l) 
  }
}

