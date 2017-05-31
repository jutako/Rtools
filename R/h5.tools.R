## Tools to read FIOH BWRC HDF5 data files

require('h5')

# Load data from HDF5 file
load.h5.array <- function(infile, element){
  
  f <- h5file(infile, 'r')
  
  ndim <- length(attr(f[element],'maxdim'))
  if (ndim==1){
    data = d1h5_to_array(f, element)
  } else if (ndim == 2) {
    data = d2h5_to_array(f, element)
  } else if (ndim == 3) {
    data = d3h5_to_array(f, element)
  } else (stop())
  
  h5close(f)
  
  data
}


# Load all average ERPs from HDF5 file
# Note: assumes structure /erpavg/<dataset id> use loaddir.h5*() to load from single files.
load.h5.erpavg <- function(infile){
  
  f <- h5file(infile, 'r')
  
  if ('/erpavg' %in% list.groups(f)){
    ds.arr <- list.datasets(f['/erpavg'])  
  } else {
    ds.arr <- '/erpavg'
  }
  
  dlst <- list()
  for ( ds in ds.arr ){
    data <- d2h5_to_array(f, ds)
    dlst <- c(dlst, list(data))
    rm(data)
  }
  
  h5close(f)
  
  names(dlst) <- basename(ds.arr)
  dlst
} 


# Load all single-trial ERPs from HDF5 file
# Note: assumes structure /erp/<dataset id> use loaddir.h5*() to load from single files.
load.h5.erp <- function(infile){
  
  f <- h5file(infile, 'r')
  
  if ('/erp' %in% list.groups(f)){
    ds.arr <- list.datasets(f['/erp'])  
  } else {
    ds.arr <- '/erp'
  }
  
  dlst <- list()
  for ( ds in ds.arr ){
    data <- d3h5_to_array(f, ds)
    dlst <- c(dlst, list(data))
    rm(data)
  }
  
  h5close(f)
  
  names(dlst) <- basename(ds.arr)
  dlst
} 


# Load data from all hdf5 files in a directory, output a list
loaddir.h5 <- function(indir, element, pattern = '*.h5'){
  
  #indir <- '~/ukonkuva/projects/SeamlessCare_2015-16/analysis/ctap_branch/erp/prepro/ica/ERP_nback_allNTrg/this/figures/CTAP_plot_ERP/set6_fun3/'
  #pattern <- '.*NB01.*_ERPdata.h5'
  #element <- '/erp'
  
  fname.arr <- list.files(indir, pattern = pattern, full.names = F) #list hdf5 files
  if (length(fname.arr)==0){
    stop(sprintf('No files found using pattern: %s', pattern))
  }
  
  # Load data
  dl <- lapply(fname.arr, function(x){load.h5.array(file.path(indir, x), element)} )
  names(dl) <- fname.arr
  dl
}


# Load ERP data from all hdf5 files in a directory, output a list
loaddir.h5.erp <- function(indir, element, pattern = '*_ERPdata.h5'){
  
  #indir <- '~/ukonkuva/projects/SeamlessCare_2015-16/analysis/ctap_branch/erp/prepro/ica/ERP_nback_allNTrg/this/figures/CTAP_plot_ERP/set6_fun3/'
  #pattern <- '.*NB01.*_ERPdata.h5'
  #element <- '/erp'
  
  # Load data
  dl <- loaddir.h5(indir, element, pattern)
  
  # File info
  fdata <- lapply(names(dl), function(x){as.data.frame(parse.savename(x))} ) #parse filenames
  fdata <- plyr::ldply(fdata) #from list to data.frame
  
  # Dimension info
  n.trial <- sapply(fdata$filename, function(x){h5.get.dataset.attr(file.path(indir,x),NULL,'/erp','maxdim')[[3]]})
  fdata$n.trial <- n.trial
  
  # Rename list elements
  names(dl) <- fdata$subject
  attr(dl, 'filedata') <- fdata
  dl
}


# Transform HDF5 numericdata to array

# vector
d1h5_to_array <- function(h5handle, dpath){
  
  d1ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd1ID') )
  
  data <- h5handle[dpath][]
  dimnames(data) <- list(d1ID)
  data
}

# matrix
d2h5_to_array <- function(h5handle, dpath){
  
  attr.arr <- list.attributes(h5handle[dpath])
  
  if ('rowID' %in% attr.arr){
    # todo: legacy mode. Remove this in the future.
    d1ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'rowID') )
    d2ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'colID') )
    
  } else {
    d1ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd1ID') )
    d2ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd2ID') )
  }
  
  data <- h5handle[dpath][]
  dimnames(data) <- list(d1ID, d2ID)
  data
}

# 3 dimensional array
d3h5_to_array <- function(h5handle, dpath){
  
  d1ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd1ID') )
  d2ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd2ID') )
  d3ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd3ID') )
  
  data <- h5handle[dpath][]
  dimnames(data) <- list(d1ID, d2ID, d3ID)
  data
}

# Convert HDF5 attribute to dimnames() combatible format
# Note: HDF5 representation of char arrays / cellstr is user defined.
# Using the convention of collapsing into single string with ";" separators.
h5attr2dimname <- function(attrdata){
  if (is.character(attrdata)){
    attrdata <- strsplit(attrdata, ';')[[1]]
  } 
  attrdata
}

# Fetch some attribut from a HDF5 file
h5.get.dataset.attr <- function(hdf5file, dgroup, dataset, attrib){
  # hdf5file = '/home/jkor/tmp/test.h5'
  # dgroup <- '/erp'
  # dataset <- 'SEAMPILOT301'
  # attrib <- 'maxdim'
  
  f <- h5file(hdf5file, 'r')
  
  # List available datasets in the desired group
  grps <- list.groups(f)
  
  if (is.null(dgroup)){
    ds <- dataset
  } else {
    match <- grps %in% dgroup
    ds <- list.datasets(f[grps[match]])
  }
  
  # Get attribute
  out <- NULL
  if (dataset == 'all'){
    out <- list()
    for (d in ds){
      out <- c(out, list(attr(f[d], attrib)) )
    }
  } else {
    # single dataset
    idx <- grep(dataset, ds) #find correct dataset within group
    if (length(idx)==1){
      out <- attr(f[ds[idx]], attrib)  
    } 
  }

  
  h5close(f)
  out
}
