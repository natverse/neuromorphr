#' @title Return the available meta data fields for the literature for neuromorpho neurons
#'
#' @description Returns a vector of literature fields available from the neuromorpho.org database.
#' These fields can be used in the field section of the custom queries.
#' @inheritParams neuromorpho_read_neurons
#' @details Articles can also be searched in a web browser at \url{http://neuromorpho.org/LS.jsp} 
#' @family literature functions
#' @examples
#' # Let's see what meta data fields neuromorpho.org has for its literature database
#' lit.fields = neuromorpho_literature_fields()
#' print(lit.fields)
#' 
#' @export
#' @rdname neuromorpho_literature_fields
neuromorpho_literature_fields <- function(neuromorpho_url = "http://neuromorpho.org", 
                               ...){
  neuromorpho_is_api_healthy()
  res = unlist(neuromorpho_fetch(path = "api/literature/fields", neuromorpho_url = neuromorpho_url, ...))
  names(res) = NULL
  res
}

#' @title Return the available meta data entries for the literature for neuromorpho neuron
#'
#' @description Returns a list of values present in the repository for the neuron field requested.
#' These values can be used in the for custom literature search queries.
#' @inheritParams neuromorpho_read_neurons
#' @param field a valid literature field, as returned by \code{neuromorpho_literature_fields}
#' @details Articles can also be searched in a web browser at \url{http://neuromorpho.org/LS.jsp}. 
#' @family literature functions
#' @examples
#' \dontrun{
#' # Let's see what the possible entries are for literature fields on neuromorpho.org
#' 
#' ## let's have a look at what species are in the literature
#' lit.fields = neuromorpho_literature_field_entries(field="species")
#' print(lit.fields)
#' 
#' ## Note this is a little different from what we get when we pull this information for neurons
#' neuron.fields = neuromorpho_field_entries(field="species")
#' print(neuron.fields)
#' }
#'
#' @export
#' @rdname neuromorpho_literature_field_entries
neuromorpho_literature_field_entries <- function(field = "species", 
                                      neuromorpho_url = "http://neuromorpho.org", 
                                      ...){
  f = neuromorpho_literature_fields(neuromorpho_url=neuromorpho_url,...)
  if(!field%in%f){
    stop("The given field ", f," is not available, to see available fields, call neuromorpho_literature_fields()")
  }
  res = neuromorpho_fetch(path = paste0("api/literature/fields/",field), neuromorpho_url = neuromorpho_url, ...)
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

#' @title Make a custom literature search to the neuromorpho.org repository
#'
#' @description Search for literature in the neuromorpho.org repository. The returned 
#' data frame can  be used to get article IDs and neuron names, that can be fed to \code{neuromorpho_get_article}, 
#' in order to get ifnormation on studies that have submitted reconstructions to neuromorpho.org. 
#' Searches are made by looking at the meta data for items of literature in the repositroy, 
#' i.e. querying for certain field entries for certain literature fields.
#' @inheritParams neuromorpho_read_neurons
#' @param search_terms a vector of search terms, with each term formatted as \code{"field:field entry"}.
#' All valid neuron fields can be seen by calling \code{neuromorpho_fields}, and all the entries for certain fields
#' can be seen using \code{neuromorpho_fields_entries}. 
#' Only meta data for neurons that have the entries described for all given search terms will be returned.
#' @details Beware, that these terms are
#' different from the equivalent fields and field entries for neurons, e.g. the cell type literature field is \code{cellType} 
#' rather than \code{cell_type}, as it is for neurons. Articles can also be searched in a web browser at \url{http://neuromorpho.org/LS.jsp}. 
#' @family literature functions
#' @examples
#' \dontrun{
#' # Let's search for some literature that has contributed reconstructions to neuromorpho.org
#' 
#' ## let's have a look at articles that have worked on Drosophila melanogaster
#' lit.search = neuromorpho_literature_search(field="species:Drosophila melanogaster")
#' 
#' ## We have found this many papers
#' print(nrow(lit.search))
#' 
#' ## Note this is a little different from what we get when we pull this information for neurons
#' neuron.fields = neuromorpho_field_entries(field="species")
#' print(neuron.fields)
#' }
#'
#' @export
#' @rdname neuromorpho_literature_search
neuromorpho_literature_search <- function(search_terms = c("species:Drosophila melanogaster"),
                               neuromorpho_url = "http://neuromorpho.org", 
                               ...){
  f = neuromorpho_literature_fields(neuromorpho_url=neuromorpho_url,...)
  fields = gsub(":.*","",search_terms)
  if(sum(!fields%in%f)){
    stop("The given field(s) ", fields," are not available, to see available fields, call neuromorpho_literature_fields()")
  }
  search_terms = paste(search_terms, collapse = "&fq=")
  res = neuromorpho_fetch(path = paste0("api/literature/select?q=",search_terms), neuromorpho_url = neuromorpho_url, ...)
  df = neuromorpho_unpack_literature_search(res)
  ### Read multiple pages of data ###
  pages = res$page
  if(pages$totalPages>1){
    for(page in 1:(pages$totalPages-1)){ # Pages are numbered from 0
      resp = neuromorpho_fetch(path = paste0("api/literature/select?q=",search_terms,"&page=",page,"&size=",pages$size,"&sort=title,asc"),
                               neuromorpho_url = neuromorpho_url, ...)
      df = rbind(df, neuromorpho_unpack_literature_search(resp))
    }
  }
  df
}

#' @title Get the number of different articles for a neuromorpho literature field
#'
#' @description Returns a vector of counts for every field entry for a given neuromorpho literature field.
#' E.g. you can find out how the number of papers that used particula neuron recosntruction softwares. 
#' @inheritParams neuromorpho_field_entries
#' @param field a valid neuron field, as returned by \code{neuromorpho_fields}
#' @details Articles can also be searched in a web browser at \url{http://neuromorpho.org/LS.jsp}. 
#' @family literature functions
#' @export
#' @rdname neuromorpho_literature_field_counts
neuromorpho_literature_field_counts <- function(field = "tracingSystem", 
                                     neuromorpho_url = "http://neuromorpho.org", 
                                     ...){
  f = neuromorpho_literature_fields(neuromorpho_url=neuromorpho_url,...)
  if(!field%in%f){
    stop("The given field ", f," is not available, to see available fields, call neuromorpho_literature_fields()")
  }
  res = neuromorpho_fetch(path = paste0("api/literature/partition/",field), neuromorpho_url = neuromorpho_url, ...)
  fields = unlist(res$fields)
  ### Get missing data ###
  pages = res$page
  if(pages$totalPages>1){
    for(page in 1:(pages$totalPages-1)){ # Pages are numbered from 0
      resp = neuromorpho_fetch(path = paste0("api/literature/select?q=",field,"&page=",page,"&size=",pages$size,"&sort=title,asc"),
                               neuromorpho_url = neuromorpho_url, ...)
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

#' @title Get information on an article by querying an article ID
#'
#' @description Returns a data frame describing an article; article_ids can be obtained using \code{neuromorpho_articles_from_neurons}
#' @inheritParams neuromorpho_field_entries
#' @inheritParams neuromorpho_read_neurons
#' @param article_id a unique neuromorpho article ID, number in the repository start from 1.
#' article_ids can be obtained using \code{neuromorpho_articles_from_neurons}
#' @details Articles can also be searched in a web browser at \url{http://neuromorpho.org/LS.jsp}. 
#' @family literature functions
#' @export
#' @rdname neuromorpho_get_article
neuromorpho_get_article <- function(article_id,
                                    batch.size = 10,
                                    neuromorpho_url = "http://neuromorpho.org",
                                    progress = TRUE,
                                    ...){
  paths = paste0(neuromorpho_url, "/api/literature/id/", article_id)
  res = neuromorpho_async_req(urls = paths, FUN = neuromorpho_parse_json, batch.size = batch.size, 
                              progress = progress, message = "reading neuromorpho metadata", ...)
  res = neuromorpho_fetch(path = paste0("api/literature/id/", article_id), neuromorpho_url = neuromorpho_url, ...)
  names(res) = article_id
  res = res[!is.na(res)]
  l = list()
  if(is.null(res)){
    warning("no results found")
    return(NULL)
  }else{
    for(i in 1:length(res)){
      dfi = neuromorpho_process_literature(res[[i]])
      l[[i]] = dfi
    }
    df = as.data.frame(do.call(rbind,l)) 
  }
  df
}

# hidden
neuromorpho_process_literature <- function(res){
  df = data.frame(# Article
    article_id = paste(res$article_id,collapse= "/"),
    pmid = paste(res$pmid,collapse= "/"),
    doi = paste(res$doi,collapse= "/"),
    journal = paste(res$journal,collapse= "/"),
    title = paste(res$title,collapse= "/"),
    # Authors
    authors = paste(res$authors,collapse= " "),
    # data
    species = paste(res$species,collapse= "/"),
    brainRegion = paste(res$brainRegion,collapse= "/"),
    cellType = paste(res$cellType,collapse= "/"),
    tracingSystem = paste(res$tracingSystem,collapse= "/"),
    evaluatedDate = paste(res$evaluatedDate,collapse= "/"),
    globalStatus = paste(res$globalStatus,collapse= "/"),
    collection = paste(res$collection,collapse= "/"),
    dataUsage = paste(res$dataUsage,collapse= "/"),
    sharedReconstructions = paste(res$sharedReconstructions,collapse= "/"),
    nReconstructions = paste(res$status$nReconstructions,collapse= "/")
  )
  nullToNA(df)
}

# hidden
neuromorpho_unpack_literature_search <- function(res){
  l = list()
  if(is.null(res$`_embedded`)){
    warning("no results found")
    return(NULL)
  }else{
    for(i in 1:length(res$`_embedded`$publicationResources)){
      dfi = neuromorpho_process_literature(res$`_embedded`$publicationResources[[i]])
      l[[i]] = dfi
    }
    as.data.frame(do.call(rbind,l)) 
  }
}