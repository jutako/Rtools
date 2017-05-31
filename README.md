# Rtools
An R package that contains miscellaneous tools for R. I use these tools in my free time projects as well as in work. Currently the most important part are the HDF5 -wrappers that are used to read in ERP data from CTAP.

The tools are made into a proper package to make them easier to use. See chapter "howto".


# HOWTO

Follow these steps to install the package.

1. clone the repository to your machine:
```
git clone https://github.com/jutako/Rtools.git
```

2. install it in R:
```
install.packages('devtools')
require(devtools)
devtools::install('path-to-Rtools-repo')
require(Rtools)
```
