#' @title Read insect neurons from insectbraindb.org
#'
#' @description Read templatebrain-registered neurons from insectbraindb.org (2018 version), given a single ID or vector of neuron IDs. Alternatively,
#' if \code{ids} is set to \code{NULL}, all neurons in the database that can be read, are read (~100). Metadata for available neurons can be seen, and IDs chosen,
#' by calling \code{\link{insectbraindb_neuron_info}}. Neurons are returned as a \code{nat} package \code{\link[nat]{neuronlist}} object, 
#' that can be be plotted in 3D using \code{rgl} and analysed with tools from the \code{nat} ecosystem. Each neuron
#' in the insectbraindb is represented by a unique numerical ID, a name and is associated with a species of insect and
#' a publiction, by DOI. Some neurons cannot be read because a SWC file is not available from insectbraindb.org. 
#' You can examine the available neurons here \href{https://insectbraindb.org/app/neurons}{here}), while the available species
#' may be examined \href{https://insectbraindb.org/app/species}{here}). Critically, these neurons are registered to standard templates
#' that may be obtained as \code{hxsurf} objects in R by using \code{\link{insectbraindb_read_brain}}.
#' @param ids a neuron ID, or vector of neuron IDs, as recorded in the insectbraindb.org. If set to \code{NULL} (default)
#' then all neurons in the databse are read. As of May 2018, this is only 100 readable neurons. A helpful information
#' page on each neuron can be seen by visiting "https://insectbraindb.org/app/neuron/id", where id is a number, e.g.
#' for Bogon moth neuron \code{219}, this is yuor \href{https://insectbraindb.org/app/neuron/219}{ticket}.
#' @param progress if \code{TRUE} or a numeric value, a progress bar is shown to track the state of your download 
#' @details Multiple neurons are read, their SWC files as hosted at https://ibdb-file-storage.s3.amazonaws.com/
#' are downloaded to a temporary directory, and read using \code{\link[nat]{read.neurons}} into a \code{\link[nat]{neuronlist}} object in R.
#' This format and its manipulation is described in detail \href{https://jefferis.github.io/nat/}{here}.
#' When using \code{insectbraindb_read_neurons}, meta data for the neuron is also returned that gives a neuron's ID,
#' short name, long name, associated laboratory and publication, if available, and the assoeciated insect species. As
#' of May 2019, data from the following \href{https://insectbraindb.org/app/species}{species} is hosted on insectbraindb.org :
#'\itemize{
#'  \item 	\emph{Agrotis infusa}	 Bogong moth
#'  \item 	\emph{Agrotis segetum}	 Turnip moth
#'  \item 	\emph{Apis mellifera}	 Honeybee
#'  \item 	\emph{Apis mellifera}	 Honeybee
#'  \item 	\emph{Danaus plexippus}	 Monarch Butterfly
#'  \item 	\emph{Helicoverpa armigera}	 Cotton Bollworm,
#'  \item 	\emph{Helicoverpa assulta}	 Oriental tobacco budworm
#'  \item 	\emph{Heliothis virescens}	 Tobacco budworm
#'  \item 	\emph{Macroglossum stellatarum}	 Hummingbird hawk moth
#'  \item 	\emph{Manduca sexta}	 Tobacco hornworm
#'  \item 	\emph{Megalopta genalis}	 Sweat bee
#'  \item 	\emph{Nasonia vitripennis}	 Jewel wasp
#'  \item 	\emph{Scarabaeus lamarcki}	 Diurnal dung beetle
#'  \item 	\emph{Schistocerca gregaria}	 Desert Locust
#'}
#'  Note that since neurons are reconstructed from many different neural species, 
#'  there is no 'standard' orientation between species, but within a species these neurons are registered to a
#'  template brain, usually using elastix. To visualise the templatebrain, use \code{plot3d} on class \code{hxsurf}
#'  objects downloaded using \code{\link{insectbraindb_read_brain}}.
#' @return a \code{nat} package \code{\link[nat]{neuronlist}} object, replete with metadata, is returned
#' @seealso \code{\link{insectbraindb_neuron_info}}, \code{\link{insectbraindb_read_brain}}, \code{\link{insectbraindb_species_info}}
#' @examples
#' \dontrun{ 
#' ## What neurons does the insectbraindb.org host?
#' available.neurons = insectbraindb_neuron_info()
#' 
#' ## Let's just download all of the neurons in the database to play with,
#' ## there are not very many:
#' nrow(available.neurons)
#' 
#' ## First, we call the read neurons function, with ids set to NULL
#' insect.neurons = insectbraindb_read_neurons(ids = NULL)
#' 
#' ## Hmm, let's see how many neurons we have perspecies
#' table(insect.neurons[,"common_name"])
#' 
#' ## So, it seem the Monarch Butterfly is the clear winner there, 
#' ## maybe let's just have those
#' butterfly.neurons = 
#' subset(insect.neurons, common_name == "Monarch Butterfly")
#' 
#' ## And let's plot them
#' plot3d(butterfly.neurons, lwd = 2, soma = 5)
#' 
#' ## Cool! But maybe we also want to see it's template brain? 
#' ## Let's check if they have it
#' available.brains = insectbraindb_species_info()
#' available.brains
#' 
#' ## Great, they do, let's get it
#' butterfly.brain = insectbraindb_read_brain(species = "Danaus plexippus")
#' 
#' ## And plot in a translucent manner
#' plot3d(butterfly.brain, alpha = 0.1)
#' 
#' ## Oop, that's a lot of neuropils. 
#' ## Let's go for only a subset. What's available?
#' butterfly.brain$RegionList
#' butterfly.brain$neuropil_full_names
#' 
#' # There lateral horn (LH) and the antennal lobe (AL) are my favourites.
#' # Let's plot those
#' clear3d()
#' plot3d(subset(butterfly.brain, "LH|AL"), alpha = 0.5)
#' 
#' }
#' @export
#' @rdname insectbraindb_read_neurons
insectbraindb_read_neurons <- function(ids = NULL, progress = TRUE){
  if(is.null(ids)){
    ids =insectbraindb_neuron_info()$id
  }
  temp = tempdir()
  urls = success = c()
  df = data.frame()
  for(i in 1:length(ids)){
    id = ids[i]
    neuron_info = neuromorpho_fetch(path = paste0("archive/neuron/?format=json&id=",id), 
                                    body = NULL, 
                                    parse.json = TRUE, 
                                    simplifyVector=FALSE, 
                                    include_headers = FALSE,
                                    neuromorpho_url = "https://insectbraindb.org")
    if(!length(neuron_info[[1]]$data$reconstructions)){
      warning("neuron with id ", id," could not be read")
      next
    } else if (!length(neuron_info[[1]]$data$reconstructions[[1]]$viewer_files)){
      warning("neuron with id ", id," could not be read")
      next
    }
    uuid = neuron_info[[1]]$data$reconstructions[[1]]$viewer_files[[1]]$uuid
    pub = unlist(neuron_info[[1]]$data$publications)
    ndf = data.frame(id = id,
                     short_name = changenull(neuron_info[[1]]$data$short_name),
                     neuropil_full_name = changenull(neuron_info[[1]]$data$full_name),
                     scientific_name = changenull(neuron_info[[1]]$data$species$scientific_name),
                     common_name = changenull(neuron_info[[1]]$data$species$common_name),
                     sex = changenull(neuron_info[[1]]$data$sex),
                     hemisphere = changenull(neuron_info[[1]]$data$hemisphere),
                     laboratory = changenull(neuron_info[[1]]$data$group_head),
                     publication = changenull(pub["doi"]),
                     year = changenull(pub["date"]))
    df = rbind(df, ndf)
    rownames(df) = df[,"id"]
    swc.path = neuromorpho_fetch(path = paste0("filestore/download_url/?uuid=", uuid), 
                                 body = NULL, 
                                 parse.json = TRUE, 
                                 simplifyVector=FALSE, 
                                 include_headers = FALSE,
                                 neuromorpho_url = "https://insectbraindb.org")$url
    success = c(success, i)
    urls = c(urls, swc.path)
    if(progress) neuromorpho_progress(i/length(ids)*100, max = 100, message = "downloading neuron information")
  }
  temp.files = success = c()
  for (url in 1:length(urls)){
    localfile= paste0(temp,"/", url, ".swc")
    if(!file.exists(localfile)){
      t=try(utils::download.file(urls[url], localfile, mode='wb', quiet = TRUE))
      if(inherits(t,'try-error')) {
        message("unable to download ", urls[url])
        next
      } 
    }
    success = c(success, url)
    temp.files = c(temp.files, localfile)
    if(progress) neuromorpho_progress(url/length(urls)*100, max = 100, message = "downloading SWC files")
  }
  neurons = nat::read.neurons(temp.files)
  names(neurons) = df[success,"id"]
  neurons[,] = df[success,]
  neurons
}

#' @title Read 3D insect brain meshes from insectbraindb.org
#'
#' @description Read templatebrains, comprised of their different neuropils, for various insect species from from insectbraindb.org (2018 version), 
#' given a single latin names for the species desired. Metadata for available neurons can be seen, and IDs chosen,
#' by calling \code{\link{insectbraindb_species_info}}. 3D triangular brain meshes are returned as a \code{nat} package \code{\link[nat]{hxsurf}} 
#' object, which mimics the Amira surface format. These can be be plotted in 3D using \code{rgl} and analysed with tools from the \code{nat} ecosystem.
#' This incldue subseting by neuropil, i.e.. if you only want to visualise or analyse the antennal lobe.
#' @param species the full scientific name for a species. The available options can be seen \href{https://insectbraindb.org/app/species}{here}
#' @param brain.sex the sex of the species' brain. The available options can be seen \href{https://insectbraindb.org/app/species}{here}
#' @param progress if \code{TRUE} or a numeric value, a progress bar is shown to track the state of your download 
#' @details A single 3D brain object is read, a .obj file for each of its neuropils is downloaded from https://ibdb-file-storage.s3.amazonaws.com/
#'  to a temporary directory, and read using \code{\link[readobj]{read.obj}} into a
#'  \code{\link[nat]{hxsurf}} object in R, which mimics the Amira surface format.
#'  As of May 2019, data from the following \href{https://insectbraindb.org/app/species}{species} is hosted on insectbraindb.org :
#'\itemize{
#'  \item 	\emph{Agrotis infusa}	 Bogong moth
#'  \item 	\emph{Agrotis segetum}	 Turnip moth
#'  \item 	\emph{Apis mellifera}	 Honeybee
#'  \item 	\emph{Apis mellifera}	 Honeybee
#'  \item 	\emph{Danaus plexippus}	 Monarch Butterfly
#'  \item 	\emph{Helicoverpa armigera}	 Cotton Bollworm,
#'  \item 	\emph{Helicoverpa assulta}	 Oriental tobacco budworm
#'  \item 	\emph{Heliothis virescens}	 Tobacco budworm
#'  \item 	\emph{Macroglossum stellatarum}	 Hummingbird hawk moth
#'  \item 	\emph{Manduca sexta}	 Tobacco hornworm
#'  \item 	\emph{Megalopta genalis}	 Sweat bee
#'  \item 	\emph{Nasonia vitripennis}	 Jewel wasp
#'  \item 	\emph{Scarabaeus lamarcki}	 Diurnal dung beetle
#'  \item 	\emph{Schistocerca gregaria}	 Desert Locust
#'  }
#'  Note that since neurons are reconstructed from many different neural species, 
#'  there is no 'standard' orientation between species, but within a species these neurons are registered to a
#'  template brain, usually using elastix. 
#' @return a \code{nat} package \code{\link[nat]{hxsurf}} object, which mimics the Amira surface format, replete with metadata that can be 
#' accessed using \code{$}
#' @seealso \code{\link{insectbraindb_neuron_info}}, \code{\link{insectbraindb_read_neurons}}, \code{\link{insectbraindb_species_info}}
#' @inherit insectbraindb_read_neurons examples
#' @export
#' @rdname insectbraindb_read_brain
insectbraindb_read_brain <- function(species = insectbraindb_species_info()$scientific_name,
                                     brain.sex = c("UNKNOWN", "MALE", "FEMALE"),
                                     progress = TRUE){
  species = match.arg(species)
  brain.sex = match.arg(brain.sex)
  db = insectbraindb_species_info()
  id = db[db$scientific_name==species,"id"]
  if(!length(id)){
    stop("This the species ", species ,"is not available for public download")
  }
  brain_info = neuromorpho_fetch(path = paste0("archive/species/most_current_permitted/?species_id=", id), 
                                 body = NULL, 
                                 parse.json = TRUE, 
                                 simplifyVector=FALSE, 
                                 include_headers = FALSE,
                                 neuromorpho_url = "https://insectbraindb.org")
  meta = names(brain_info)[!sapply(brain_info,is.list)]
  meta = unlist(brain_info[meta])
  sexes  = c(tryCatch(brain_info$reconstructions[[1]]$sex, error = function(e) NA),
             tryCatch(brain_info$reconstructions[[2]]$sex, error = function(e) NA),
             tryCatch(brain_info$reconstructions[[3]]$sex, error = function(e) NA))
  sex.index = match(brain.sex, sexes)
  if(is.na(sex.index)){
    warning("No reconstruction for species ", species, " returning NULL")
    return(NULL)
  }
  data = tryCatch(brain_info$reconstructions[[sex.index]]$viewer_files,
                  error = function(e){
                    warning("No reconstruction for species ", species, " returning NULL")
                    return(NULL)
                  }
  )
  if(!length(data)){
    warning("No reconstruction for species ", species, " of sex ", brain.sex, " returning NULL")
    return(NULL)
  }
  paths = hemispheres = structure.names = structure.shorts = structure.colors = sex = c()
  for(d in data){
    sex = c(sex, d$sex)
    paths = c(paths, d$p_file$path)
    hemispheres = c(hemispheres, changenull(d$structures[[1]]$hemisphere, to = "noside"))
    structure.names = c(structure.names, changenull(d$structures[[1]]$structure$name, to = "ambiguous"))
    structure.shorts = c(structure.shorts, changenull(d$structures[[1]]$structure$abbreviation, to = "AMBIG"))
    structure.colors = c(structure.colors, changenull(d$structures[[1]]$structure$color, to = "grey"))
  }
  obj.name = paste(structure.shorts, hemispheres, 1:length(structure.shorts), sep = "_")
  obj.name = gsub("_$","",obj.name)
  urls = paste0("https://s3.eu-central-1.amazonaws.com/ibdb-file-storage/",paths)
  temp.files = success = c()
  temp = tempdir()
  for (url in 1:length(urls)){
    localfile = paste0(temp,"/", paste0(paste(unlist(strsplit(species," ")),collapse="_"), "_", sex[1], "_", basename(urls[url]) ))
    if(!file.exists(localfile)){
      t=try(utils::download.file(urls[url], localfile, mode='wb', quiet = TRUE))
      if(inherits(t,'try-error')) {
        warning("unable to download ", urls[url], " and save as ", localfile)
        next
      } 
    }
    success = c(success, url)
    temp.files = c(temp.files, localfile)
    if(progress) neuromorpho_progress(url/length(urls)*100, max = 100, message = "downloading .obj files")
  }
  objs = lapply(temp.files, function(x)
    tryCatch(readobj::read.obj(x, convert.rgl = T), error = function(e) NULL)[[1]])
  success = success[!sapply(objs, is.null)]
  objs = objs[!sapply(objs, is.null)]
  objs = lapply(objs, nat::as.hxsurf)
  brain = list()
  brain$Vertices = data.frame()
  brain$Regions = list()
  brain$RegionList = brain$RegionColourList = c()
  class(brain) = c("hxsurf","list")
  count = 0
  for(i in 1:length(objs)){
    o = objs[[i]]
    v = o$Vertices
    v$PointNo = v$PointNo + count
    brain$Vertices = rbind(brain$Vertices, v)
    region.name = obj.name[success][i]
    regions = o$Regions$Interior+count
    brain$Regions[[region.name]] = regions
    brain$RegionList = c(brain$RegionList, region.name)
    brain$RegionColourList = c(brain$RegionColourList, structure.colors[success][i])
    count = count + nrow(o$Vertices)
    if(progress) neuromorpho_progress(i/length(objs)*100, max = 100, message = "assembling hxsurf object")
  }
  brain$neuropil_full_names = structure.names[success]
  brain$hemispheres = hemispheres[success]
  brain$scientific_name = meta["scientific_name"]
  brain$common_name = meta["common_name"]
  brain$sex = brain.sex
  brain$id = meta["id"]
  brain$host_lab = meta["host_lab"]
  brain$description = meta["description"]
  brain
}

#' @title Get information on species/neurons from insectbraindb.org
#'
#' @description Get information on the neurons and brain templates hosted by insectbraindb.org, including neuron names, species names, associated publications, etc.
#' @details 
#'  As of May 2019, data from the following \href{https://insectbraindb.org/app/species}{species} is hosted on insectbraindb.org :
#'\itemize{
#'  \item 	\emph{Agrotis infusa}	 Bogong moth
#'  \item 	\emph{Agrotis segetum}	 Turnip moth
#'  \item 	\emph{Apis mellifera}	 Honeybee
#'  \item 	\emph{Apis mellifera}	 Honeybee
#'  \item 	\emph{Danaus plexippus}	 Monarch Butterfly
#'  \item 	\emph{Helicoverpa armigera}	 Cotton Bollworm,
#'  \item 	\emph{Helicoverpa assulta}	 Oriental tobacco budworm
#'  \item 	\emph{Heliothis virescens}	 Tobacco budworm
#'  \item 	\emph{Macroglossum stellatarum}	 Hummingbird hawk moth
#'  \item 	\emph{Manduca sexta}	 Tobacco hornworm
#'  \item 	\emph{Megalopta genalis}	 Sweat bee
#'  \item 	\emph{Nasonia vitripennis}	 Jewel wasp
#'  \item 	\emph{Scarabaeus lamarcki}	 Diurnal dung beetle
#'  \item 	\emph{Schistocerca gregaria}	 Desert Locust
#'  }
#' @return a \code{data.frame} that most importantly gives the user the latin scientific name, common name or neuron names and insectbraindb.org-specific ID number
#' for each insect species / neuron on which data is held on the site
#' @seealso \code{\link{insectbraindb_read_brain}}, \code{\link{insectbraindb_read_neurons}}
#' @inherit insectbraindb_read_neurons examples
#' @export
#' @rdname insectbraindb_info
insectbraindb_species_info <- function(){
  species_info = neuromorpho_fetch(path = "api/species/min/", 
                                  body = NULL, 
                                  parse.json = TRUE, 
                                  simplifyVector=FALSE, 
                                  include_headers = FALSE,
                                  neuromorpho_url = "https://insectbraindb.org")
  df = data.frame()
  for(s in species_info){
    s = nullToNA(s)
    df  = rbind(df, unlist(s))
  }
  colnames(df) = names(species_info[[1]])
  rownames(df) = df[,"id"]
  df
}

#' @export
#' @rdname insectbraindb_info
insectbraindb_neuron_info <- function(){
  neurons_info = neuromorpho_fetch(path = "api/neurons/base/?format=json", 
                                   body = NULL, 
                                   parse.json = TRUE, 
                                   simplifyVector=FALSE, 
                                   include_headers = FALSE,
                                   neuromorpho_url = "https://insectbraindb.org")
  df = data.frame()
  for(n in neurons_info){
    n = nullToNA(n)
    meta = names(n)[!sapply(n,is.list)]
    meta = unlist(n[meta])
    meta["scientific_name"] = n$species$scientific_name
    meta["common_name"] = n$species$common_name
    df  = rbind(df, meta)
  }
  colnames(df) = names(meta)
  df
}
