<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://api.travis-ci.org/jefferislab/neuromorphr.svg?branch=master)](https://travis-ci.org/jefferislab/neuromorphr) [![Coverage status](https://codecov.io/gh/jefferislab/neuromorphr/branch/master/graph/badge.svg)](https://codecov.io/github/jefferislab/neuromorphr?branch=master) [![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](http://jefferislab.github.io/neuromorphr/reference/)

neuromorphr
===========

The goal of *neuromorphr* is to provide R client utilities for interacting with the [API](http://neuromorpho.org/api.jsp) for [neuromorpho.org](http://neuromorpho.org), as well as other, smaller repositories for single-cell neuron morphologies. The site [neuromorpho.org](http://neuromorpho.org) is a well-known centrally curated inventory of digitally reconstructed neurons associated with peer-reviewed publications. It is continuously updated as new morphological reconstructions are collected, published and shared. It contains contributions from hundreds of laboratories worldwide (see many [here](http://neuromorpho.org/acknowl.jsp)). To date, [neuromorpho.org](http://neuromorpho.org) is the largest collection of publicly accessible 3D neuronal reconstructions (&gt;100,000) and associated metadata which can be used for detailed single cell simulations. This R package was built to work with version 7.7 of [neuromorpho.org](http://neuromorpho.org), as well as the [insectbraindb](https://insectbraindb.org/app/) (see below).

If python is more your poison, there are a few python clients available, for example from [BonsaiNet](https://github.com/BonsaiNet/Neuromorpho.org). Using this R package in concert with the [nat](https://github.com/jefferis/nat) ecosystem developed primarily by [Gregory Jefferis](https://en.wikipedia.org/wiki/Gregory_Jefferis) is highly recommended. The curators of [neuromorpho.org](http://neuromorpho.org) can be contacted at *<nmadmin@gmu.edu>*.

In the following, we detail some of *neuromorphr*'s functionality, and copy and paste some text from [neuromorpho.org](http://neuromorpho.org) in order to fill you in on its database and outlook.

Installation
------------

``` r
# install
if (!require("devtools")) install.packages("devtools")
devtools::install_github("jefferislab/neuromorphr")

# use 
library(neuromorphr)
```

Key Functions
-------------

Now we can have a look at what is available, here are some of the key functions. Their help details examples of their use. You can summon the help in RStudio using `?` followed by the function name.

``` r
# Sometimes neuromorphr.org can be slow, or down from capacity issues or have a faulty API. Can I check this?
neuromorpho_is_api_healthy()

# And how can I read neurons from neuromorpho?
?neuromorphr_read_neurons()

# But first I need neuron names and/or neuron IDs, how do I get those?
?neuprint_search()

# I see, so what metadata do I get with my neurons?
?neuprint_neurons_info()
?neuprint_neurons_meta()

# Interesting, I see a pubmed ID (pmid) is also given. Can I find the scientific artices that describe these neurons?
?neuromorpho_articles_from_neurons()
?neuromorpho_literature_search()
?neuromorpho_get_article()

# I heard something about persistence vectors, they're used to describe describe meaningful morphological features? Can I get those?
?neuromorpho_persistence_vectors

# What about insectbrainDB.org?
?insectbraindb_read_neurons # Get neurons registered to a standard brainspace
?insectbraindb_read_brain # Get 3D neuropil-subdivided brain models for those brainspaces
```

Each neuron in [neuromorpho.org](http://neuromorpho.org) is represented by a name, general information (metadata) and a standardised [SWC](http://www.neuronland.org/NLMorphologyConverter/MorphologyFormats/SWC/Spec.html) file for the digital morphological reconstruction. Most neurons also have some basic measurements calculated, including for example cable length, in micrometers, and volume, in micrometers cubed. The functions above can get you this information.

NeuroMorpho.org Example
-----------------------

Here is a quick example for reading some neocortical rat, elephant and whale neurons; plotting them and comparing them:

``` r
# Let's get neocortical neurons from both the African elephant, the humpback whale and the rat
neocortex.df = neuromorpho_search(search_terms= c("species:elephant,humpback whale,rat", "brain_region:neocortex"))

## Pull measurements, in a data frame
measurements = neuromorpho_morphometry(neocortex.df$neuron_name, batch.size = 2, data_frame = TRUE)
 
## Assign species column
measurements$species = neocortex.df[rownames(measurements),"species"]
 
## Boxplot, neuron length
boxplot(as.numeric(length)~species, data=measurements, notch=FALSE, 
      col=(c("deepskyblue1","firebrick1", "darkolivegreen1")),
      main="neocortical neuron lengths", xlab="species")
 
## Boxplot, neuron volume
boxplot(as.numeric(volume)~species, data=measurements, notch=FALSE, 
      col=(c("deepskyblue1","firebrick1", "darkolivegreen1")),
      main="neocortical neuron volumes", xlab="species")

## Read neurons
pyramidal.cells = neuromorpho_read_neurons(neuron_name = neocortex.df$neuron_name, batch.size = 2, nat = TRUE, progress = TRUE)

## Plot 3D
plot3d(pyramidal.cells, col = as.factor(species))
```

Here's a subset. In red, elephant, and in blue, whale, cortical pyramidal neurons:

![3dneurons](https://raw.githubusercontent.com/jefferislab/neuromorphr/master/inst/images/3dneurons.png)

insectbraindb.org Example
-------------------------

Let's also have a look at an example pulling neurons and brain meshes from [insectbraindb.org](https://insectbraindb.org/app/). Here we shall take a look at neurons from the brain of the Monarch butterlfy that have been registered to a template brain. Excitingly, we can also visualise this template brain.

``` r
## What neurons does the insectbraindb.org host?
available.neurons = insectbraindb_neuron_info()

## Let's just download all of the neurons in the database to play with,
## there are not very many:
nrow(available.neurons)

## First, we call the read neurons function, with ids set to NULL
insect.neurons = insectbraindb_read_neurons(ids = NULL)

## Hmm, let's see how many neurons we have perspecies
table(insect.neurons[,"common_name"])

## So, it seem the Monarch Butterfly is the clear winner there, 
## maybe let's just have those
butterfly.neurons = subset(insect.neurons, common_name == "Monarch Butterfly")

## And let's plot them
nopen3d(userMatrix = structure(c(0.999986588954926, -0.00360279157757759, 
-0.00371213257312775, 0, -0.00464127957820892, -0.941770493984222, 
-0.336223870515823, 0, -0.00228461623191833, 0.336236596107483, 
-0.941774606704712, 0, 0, 0, 0, 1), .Dim = c(4L, 4L)), zoom = 0.600000023841858, 
    windowRect = c(1460L, 65L, 3229L, 1083L))
plot3d(butterfly.neurons, lwd = 2, soma = 5)

## Cool! But maybe we also want to see it's template brain? 
## Let's check if they have it
available.brains = insectbraindb_species_info()
available.brains

## Great, they do, let's get it
butterfly.brain = insectbraindb_read_brain(species = "Danaus plexippus")

## And plot in a translucent manner
plot3d(butterfly.brain, alpha = 0.1)

## Oop, that's a lot of neuropils. 
## Let's go for only a subset. What's available?
butterfly.brain$RegionList
butterfly.brain$full_names

## There lateral horn (LH) and the antennal lobe (AL) are my favourites.
## Let's plot those
clear3d()
plot3d(subset(butterfly.brain, "LH|AL"), alpha = 0.5)
plot3d(butterfly.neurons, lwd = 2, soma = 5)

### Ffff, doesn't look like we have any neurons in my favourite neuropils :(
```

![butterfly\_brain\_neurons](https://raw.githubusercontent.com/jefferislab/neuromorphr/master/inst/images/butterfly_brain_neurons.png)

Submit to neuromorpho.org
-------------------------

As well as using this package to interact with neuromorpho.org curated data, you can consider submitted your own neurons if you have been involved in lab work that has acquired 3D reconstructions. The submission process is very straightforward:

1.  E-mail the reconstruction files (zipped, if possible) to: *<neuromorpho@gmail.com>*
2.  Fill in as much information possible in the [Metadata Form](http://neuromorpho.org/about.jsp) and include it in your e-mail

Feed the beast.

Acknowledging the data and tools
--------------------------------

The [neuromorpho.org](http://neuromorpho.org) has a [terms of use](http://neuromorpho.org/useterm.jsp), as does the [insectbraindb.org](https://insectbraindb.org/app/terms), which provides guidance on how best to credit data from these repositories. Most neurons have an associated publication that you can fidn using this package or directly on tthe repositroy websites.

This package was created by Alexander Bates, while in the group of [Dr. Gregory Jefferis](https://en.wikipedia.org/wiki/Gregory_Jefferis). You can cite this package as:

``` r
citation(package = "neuromorphr")
```

**Bates AS** (2019). *neuromorphr: R client utilities for interacting with neuron morphology repositories.* **R package** version 0.1.0. <https://github.com/jefferislab/neuromorphr>

Acknowledgements
----------------

[neuromorpho.org](http://neuromorpho.org) was started and is maintained by the Computational Neuroanatomy Group at the Krasnow Institute for Advanced Study, George Mason University, under the direction of Prof. Giorgio Ascoli, PhD. This project is part of a consortium for the creation of a "Neuroscience Information Framework," endorsed by the Society for Neuroscience, funded by the National Institutes of Health, led by Cornell University (Dr. Daniel Gardner), and including numerous academic institutions such as Yale University (Dr. Gordon Shepherd), Stanford University (Dr. Paul Sternberg), and University of California, San Diego (Dr. Maryann Martone). The [neuromorpho.org](http://neuromorpho.org) mission statement can be found [here](http://neuromorpho.org/about.jsp). The [insectbraindb.org](https://insectbraindb.org/app/) is primarily curated by [Dr. Stanley Heinze](https://www.biology.lu.se/stanley-heinze), and was buily by [Kevin Tedore](https://tedore.com/), and has several significant [supporters](https://insectbraindb.org/app/), including the ERC.

References
----------

**Ascoli GA** (2006) *Mobilizing the base of neuroscience data: the case of neuronal morphologies*. **Nature Rev. Neurosci.**, 7:318-324

**Ascoli GA, Donohue DE, Halavi M.** (2007) *NeuroMorpho.Org: a central resource for neuronal morphologies.* **J Neurosci.**, 27(35):9247-51
