# Shiny Global. ################################################################

# 1) Libraries. ----------------------------------------------------------------
library(sf)                       # For working with spatial data and shapefiles.
library(grid)                     # For grid-based graphics.
library(spdep)                    # For spatial dependence analysis.
library(dplyr)                    # For data manipulation and transformation.
library(psych)                    # For creating correlation plots.
library(shiny)                    # For creating interactive web applications.
#library(rgdal)                    # For reading and writing geospatial data.
library(stringr)                  # For manipulating characters
library(shinyBS)                  # For Bootstrap Components.
library(leaflet)                  # For interactive maps.
library(ggplot2)                  # For data visualization.
library(shinyjs)                  # For enhancing Shiny web applications with JavaScript functions.
library(moments)                  # For data manipulation.
library(webshot)                  # For screenshots of interactive elements as image files.
library(magrittr)                 # For writing readable code in a pipeline style.
library(corrplot)                 # For creating a wide variety of correlograms.
library(htmltools)                # For working with HTML elements in R.
library(gridExtra)                # For arranging multiple grid-based plots.
library(patchwork)                # For combining multiple ggplots into a single plot.
library(tidyverse)                # For data manipulation.
library(ggcorrplot)               # For creating correlation plots.
library(data.table)               # For data manipulation.
library(adespatial)               # For analyzing spatial genetic data.
library(shinythemes)              # For additional themes in Shiny applications.
library(htmlwidgets)              # For creating interactive HTML widgets in R.
library(shinyWidgets)             # For additional user interface widgets in Shiny applications.
library(leaflet.extras)           # For additional functionality in the leaflet package.
library(DT)                       # For interactive datatables

options(shiny.maxRequestSize = 800 * 1024^2)

# 2) Tool Data.  ---------------------------------------------------------------
source("builtinData.R")

# 3) Functions.  ---------------------------------------------------------------
source("DataTransformCheck.R")
source("DataCheck.R")
source("MapColours.R")

# 4) User Guide.  --------------------------------------------------------------
source("UserGuide.R")
