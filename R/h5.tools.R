## Tools to read FIOH BWRC HDF5 data files

#' Load data from HDF5 file
#'
#' @description
#' Load data from HDF5 file
#'
#' @param infile character, Full path to the h5 file
#' @param element character, h5 data element to load
#' 
#' @return Element 'element' from file as a numeric array
#'
#' @importFrom h5 h5file
#' @importFrom h5 h5close
#'
#' @export
load.h5.array <- function(infile, element){
  
  f <- h5::h5file(infile, 'r')
  
  ndim <- length(attr(f[element],'maxdim'))
  if (ndim==1){
    data = d1h5_to_array(f, element)
  } else if (ndim == 2) {
    data = d2h5_to_array(f, element)
  } else if (ndim == 3) {
    data = d3h5_to_array(f, element)
  } else (stop())
  
  h5::h5close(f)
  
  data
}


#' Load all average ERPs from HDF5 file
#'
#' @description
#' Load all average ERPs from HDF5 file
#' Note: assumes structure /erpavg/<dataset id> use loaddir.h5*() to 
#' load from single files.
#'
#' @param infile character, Full file name of the ERP HDF5 file
#' 
#' @return A list of erpavg data matrices
#'
#' @importFrom h5 h5file
#' @importFrom h5 h5close
#' @importFrom h5 list.groups
#' @importFrom h5 list.datasets
#'
#' @export
load.h5.erpavg <- function(infile){
  
  f <- h5::h5file(infile, 'r')
  
  if ('/erpavg' %in% h5::list.groups(f)){
    ds.arr <- h5::list.datasets(f['/erpavg'])  
  } else {
    ds.arr <- '/erpavg'
  }
  
  dlst <- list()
  for ( ds in ds.arr ){
    data <- d2h5_to_array(f, ds)
    dlst <- c(dlst, list(data))
    rm(data)
  }
  
  h5::h5close(f)
  
  names(dlst) <- basename(ds.arr)
  dlst
} 


#' Load all single-trial ERPs from HDF5 file
#'
#' @description
#' Load all single-trial ERPs from HDF5 file
#' Note: assumes structure /erp/<dataset id> use loaddir.h5*() to 
#' load from single files.
#'
#' @param infile character, Full file name of the ERP HDF5 file
#' 
#' @return A list of single-trial ERP data matrices
#'
#' @importFrom h5 h5file
#' @importFrom h5 h5close
#' @importFrom h5 list.groups
#' @importFrom h5 list.datasets
#' 
#' @export
load.h5.erp <- function(infile){
  
  f <- h5::h5file(infile, 'r')
  
  if ('/erp' %in% h5::list.groups(f)){
    ds.arr <- h5::list.datasets(f['/erp'])  
  } else {
    ds.arr <- '/erp'
  }
  
  dlst <- list()
  for ( ds in ds.arr ){
    data <- d3h5_to_array(f, ds)
    dlst <- c(dlst, list(data))
    rm(data)
  }
  
  h5::h5close(f)
  
  names(dlst) <- basename(ds.arr)
  dlst
} 


#' Load data from all hdf5 files in a directory, output a list
#'
#' @description
#' Load data from all hdf5 files in a directory, output a list
#'
#' @param indir character, Directory name
#' @param element character, Data element to load
#' @param pattern character, File name pattern to use
#' 
#' @return A list of data matrices
#' 
#' @examples 
#' indir <- '~/ukonkuva/projects/SeamlessCare_2015-16/analysis/ctap_branch/erp/prepro/ica/ERP_nback_allNTrg/this/figures/CTAP_plot_ERP/set6_fun3/'
#' pattern <- '.*NB01.*_ERPdata.h5'
#' element <- '/erp'
#' 
#' @export
loaddir.h5 <- function(indir, element, pattern = '*.h5'){
  
  fname.arr <- list.files(indir, pattern = pattern, full.names = F) #list hdf5 files
  if (length(fname.arr)==0){
    stop(sprintf('No files found using pattern: %s', pattern))
  }
  
  # Load data
  dl <- lapply(fname.arr, function(x){load.h5.array(file.path(indir, x), element)} )
  names(dl) <- fname.arr
  dl
}


#' Load ERP data from all hdf5 files in a directory, output a list
#'
#' @description
#' Load ERP data from all hdf5 files in a directory, output a list.
#' Uses loaddir.h5() and adds some metadata on top.
#'
#' @param indir character, Directory name
#' @param element character, Data element to load
#' @param pattern character, File name pattern to use
#' 
#' @return A list of data matrices
#' 
#' @examples 
#' indir <- '~/ukonkuva/projects/SeamlessCare_2015-16/analysis/ctap_branch/erp/prepro/ica/ERP_nback_allNTrg/this/figures/CTAP_plot_ERP/set6_fun3/'
#' pattern <- '.*NB01.*_ERPdata.h5'
#' element <- '/erp'
#' 
#' @export
loaddir.h5.erp <- function(indir, element, pattern = '*_ERPdata.h5'){
  
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


#' Transform HDF5 numeric vector to array
#'
#' @description
#' Transform HDF5 numeric vector to array
#'
#' @param h5handle HDF5 file handle, Get handle using h5::h5file(infile, 'r')
#' @param dpath character, Path to the HDF5 vector like data element
#' 
#' @return Data in 'dpath' as a numeric array
#'
#' @export
d1h5_to_array <- function(h5handle, dpath){
  
  d1ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd1ID') )
  
  data <- h5handle[dpath][]
  dimnames(data) <- list(d1ID)
  data
}


#' Transform HDF5 numeric matrix to array
#'
#' @description
#' Transform HDF5 numeric matrix to array
#'
#' @param h5handle HDF5 file handle, Get handle using h5::h5file(infile, 'r')
#' @param dpath character, Path to the HDF5 matrix like data element
#' 
#' @return Data in 'dpath' as a numeric array
#'
#' @importFrom h5 h5attr
#'
#' @export
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


#' Transform HDF5 3D numeric matrix to array
#'
#' @description
#' Transform HDF5 3D numeric matrix to array
#'
#' @param h5handle HDF5 file handle, Get handle using h5::h5file(infile, 'r')
#' @param dpath character, Path to the HDF5 3D matrix like data element
#' 
#' @return Data in 'dpath' as a numeric array
#'
#' @importFrom h5 h5attr
#'
#' @export
d3h5_to_array <- function(h5handle, dpath){
  
  d1ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd1ID') )
  d2ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd2ID') )
  d3ID <-  h5attr2dimname( h5attr(h5handle[dpath], 'd3ID') )
  
  data <- h5handle[dpath][]
  dimnames(data) <- list(d1ID, d2ID, d3ID)
  data
}


#' Convert HDF5 attribute to dimnames() compatible format
#'
#' @description
#' Convert HDF5 attribute to dimnames() combatible format
#' Note: HDF5 representation of char arrays / cellstr is user defined.
#' Using the convention of collapsing into single string with ";" 
#' separators.
#'
#' @param attrdata HDF5 attribute data, Output of h5::h5attr(object, attributename)
#'
#' @return Attribute data in dimnames() compatible format
#'
#' @export
h5attr2dimname <- function(attrdata){
  if (is.character(attrdata)){
    attrdata <- strsplit(attrdata, ';')[[1]]
  } 
  attrdata
}


#' Fetch some attribute from a HDF5 file
#'
#' @description
#' Fetch some attribute from a HDF5 file
#'
#' @param hdf5file string, Full filename of the HDF5 file, e.g. 
#' @param dgroup string, Data set group name, e.g. 
#' @param dataset string, Data set name, e.g. 
#' @param attrib string, Data attribute name
#'
#' @return Attribute data
#'
#' @importFrom h5 h5file
#' @importFrom h5 h5close
#' @importFrom h5 list.groups
#' @importFrom h5 list.datasets
#'
#' @examples
#' hdf5file = '/home/jkor/tmp/test.h5'
#' dgroup <- '/erp'
#' dataset <- 'SEAMPILOT301'
#' attrib <- 'maxdim'
#'
#' @export
h5.get.dataset.attr <- function(hdf5file, dgroup, dataset, attrib){
 
  f <- h5::h5file(hdf5file, 'r')
  
  # List available datasets in the desired group
  grps <- h5::list.groups(f)
  
  if (is.null(dgroup)){
    ds <- dataset
  } else {
    match <- grps %in% dgroup
    ds <- h5::list.datasets(f[grps[match]])
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

  h5::h5close(f)
  
  out
}
