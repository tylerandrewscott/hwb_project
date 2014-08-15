rm(list=ls())


#setwd('H:/gcs_data/univariate_shiny/')

shinyapps::setAccountInfo(
  name="tylerandrewscott", 
 token="3571572899CB844B14D1084849592AED", 
secret="gDjxjOV33KZ9BZ12hwRdVMRafTjYFSOUiZZY3YNk")

setwd('H:/gcs_data/correlation_shiny/')
deployApp()
setwd('H:/gcs_data/univariate_shiny/')
deployApp()
setwd('H:/gcs_data/covariate_shiny/')
deployApp()
runApp()


if (!require('ggvis'))
  install.packages('ggvis')
require(ggvis)

if (!require('dplyr'))
  install.packages('dplyr')
require(dplyr)

if (!require('shiny'))
  install.packages('shiny')
require(shiny)

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shinyapps")
require(shinyapps)

if (!require('RCurl'))
  install.packages('RCurl')
require(RCurl)

if (!require('data.table'))
  install.packages('data.table')
require(data.table)

if (!require('ggplot2'))
  install.packages('ggplot2')
require(ggplot2)