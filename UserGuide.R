# Define the descriptions outside the server function
uploadDataDescriptions <- list(
  "Upload CSV file" = "This option facilitates data upload by allowing users to import their own CSV files. The files should contain geographical area codes (SA1: SA1_MAIN, SA2: SA2_MAIN, SA3: SA3_CODE, SA4: SA4_CODE, LGA: LGA_CODE, and State: STE_NAME) and corresponding indicators for analysis.",
  "Data Check" = "This feature performs an automated consistency check on the uploaded datasets. It identifies potential issues such as missing values, symbols, zero values, data range errors, duplicate records, data format inconsistencies, data completeness, imbalances in the data, values with commas, and white spaces.",
  "Mandatory Fields" = "Mandatory fields are highlighted in blue. These fields must be filled for the app to function properly.",
  "Geographical Resolution" = "This feature allows the selection of the desired geographical resolution, such as SA1, SA2, SA3, SA4, or LGA. The chosen resolution determines the level of detail in the generated maps.",
  "Focus Region" = "This feature enables the selection of one or more focus regions for targeted analysis. By choosing specific geographical areas, data relevant to the selected regions can be visualized.",
  "SA4 Focus Region" = "This option allows users to further narrow down their analysis by selecting specific SA4 regions for localized insights. By choosing specific SA4 regions, users can focus on particular areas of interest and gain detailed, targeted information for their analysis."
)

dataManipulationDescriptions <- list(
  "Data Dictionary" = "This section serves as a comprehensive reference guide for understanding the dataset's variables. It provides a list of variables with corresponding descriptions, offering a valuable resource for interpreting each variable's content and context. The variables in the app are denoted with a prefix \"*\".",
  "Data Transformation" = "This option allows the application of various data transformations to selected variables. These transformations include Normalization (\"_norm\"), Percentile Ranks (\"_perc\"), Logarithmic Scale (\"_log\"), and Square Root (\"_sqrt\"), catering to different data analysis needs. The number of bins in the histogram plots can be adjusted using the provided sliders.",
  "Recommendations" = "This option provides data transformation recommendations specific to the uploaded data. It offers insights and suggestions to help optimize the data transformation process for better analysis outcomes.",
  "Population-Based Data" = "This option enables the transformation of selected variables by normalizing them based on the total population variable. It provides a standardized measure that accounts for population density (\"_pop\"). The number of bins in the histogram plots can be adjusted using the provided sliders.",
  "Categorical Variable" = "This option is designed to optimize ordinal variables by replacing each category with its corresponding frequency rank (\"_rank\"). The number of bins in the histogram plots can be adjusted using the provided sliders."
)

correlationDescriptions <- list(
  "Correlation Plot" = "This option generates a plot that visualizes the correlation between different numeric variables in the dataset. The plot visualises the correlation coefficients using various techniques, including scatter plots, ellipses, and density plots."
)

mapVisualizationDescriptions <- list(
  "Parameter Selection" = "This option allows the selection of up to four parameters to be displayed on the map.",
  "View Density Plots" = "This feature generates a density plot for each of the selected parameters, providing a detailed view of data distributions.",
  "Add Markers" = "This feature allows users to add markers to the map at specified coordinates. To upload a CSV file with latitude and longitude coordinates, use the \"Upload Lat/Lon File\" button. This button enables you to select a CSV file from your local system and upload it to the application. The CSV file should include a column named \"lat\" for latitude and another named \"lon\" for longitude. 
  Make sure the coordinates are formatted correctly in decimal degrees. To add a spatial point marker to the map, specify the latitude and longitude of the point, then click the \"Add Marker\" button to place it accordingly. Click on \"Remove\" to remove a marker.",
  "Plot Map" = "This feature generates a map based on the selected parameters and geographical areas, providing a visual representation of the parameter(s) selected.",
  "Draw Map Features" = "This feature allows users to manually draw features such as polygons, circles, rectangles, and markers on the map, aiding in the creation of custom areas of analysis. The IDs of the areas inside the drawn feature will be selected and used to indicate the values of the selected parameters on density plots. You can edit the feature, including changing its size, color, borders, and shape. To remove features from the leaflet map, click on \"clear all\" using the \"bin\" icon. To remove markers from the density plot, click on \"Remove\".",
  "Refresh" = "This option allows users to reset the current map and start over, providing flexibility in iterative data analysis and visualization.",
  "Download" = "This option allows users to download the created leaflet map in \".html\" format."
)
