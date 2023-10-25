# Load package tidyverse
if (!require("tidyverse", quietly = TRUE))
  install.packages("tidyverse")


# Get current file location
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}




# Wrapper to install packages from CRAN
pkgLoad_cran <- function( packages ) {
  
  packagecheck <- match( packages, utils::installed.packages()[,1] )
  
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall, dependencies = TRUE )
  } else {
    print( "All requested packages already installed" )
  }
  
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
  
}

# Define R mirror
r = getOption("repos")
r["CRAN"] = "http://cran.r-project.org"  # This can be changed according to your location
options(repos = r)


# Install CRAN packages
pkgLoad_cran(packages = as.vector(unlist(read.table(file.path(gsub("src$", "environment", getCurrentFileLocation()), "requirements_cran.txt")))))
