# Fix 'NA' entry in leaflet legend:
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

# Shiny UI. ####################################################################
ui <- bootstrapPage(
  useShinyjs(),

  HTML(html_fix),

  tags$head(
    tags$style(".leaflet-control-zoom { display: none; }"),

    tags$style(type = "text/css",
               "html, body {width:100%;height:100%}",
               ".popover{width:260px}",
               ".selectize-input {height: 39px; border-radius:0px; padding-top:8px}",
               ".selectize-input.dropdown-active {border-radius:0px}",
               ".selected {background-color:white; color:black; font-weight: bold}",
               ".wide-button { width: 100%; font-size: 12px; }",
               ".fixed-panel {box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48); min-height: 100vh;}",
               ".navbar {height: 40px; align-items: center; }",
               ".navbar-brand {font-size: 24px; width: 305px;}",
               ".nav-link {font-size: 14px;}",
               ".btn {font-size: 12px;}",
               ".data-summary {overflow-y: scroll; max-height: calc(100vh - 40px);}",
               ".option-text {font-size: 14px; line-height: 1.5;}"
    ),

    tags$script("$(document).on('shiny:connected', function(event) {
      function updateBackground(inputId, color) {
        var element = $('#' + inputId).parent();
        element.find('.dropdown-toggle').css('background-color', color);
      }

      // Add lightblue background-color immediately on startup
      updateBackground('iSHP', '2px solid lightblue');
      updateBackground('iSTE', '2px solid lightblue');

      Shiny.addCustomMessageHandler('updateBackground', function(message) {
        updateBackground(message.inputId, message.color);
      });
    });")

    ),

  # Main App Layout
  navbarPage(
    id = "tabs",
    "Indicator Translation Tool",
    theme = shinytheme("yeti"),

    # 1) FIRST TAB - Upload Data. ----------------------------------------------
    tabPanel(
      "Upload Data", fluid = TRUE,

      sidebarLayout(
        sidebarPanel(
          class = "fixed-panel",
          style = "position: fixed; top: 0px; left: 0; width: 305px; height: 100%; overflow: auto;",

          ## 1.1) CSV File. ----------------------------------------------------
          h5("Upload Data", class = "panel-heading", style = "margin-left: -14px; margin-top: 30px; margin-bottom: 0px;"),

          fileInput(
            inputId = "csv_File", label = "Choose CSV File:",
            accept = ".csv", multiple = TRUE ),

          ## 1.2) Data Check. --------------------------------------------------
          actionButton(inputId = "check_data_button", label =  "Data Check",  `title` = "Data inconsistency detailed",
                       class = "wide-button", style = "margin-top: -10px; margin-bottom: 10px;"),

          ## 1.3) Geographical Resolution. -------------------------------------
          h5("Data Resolution", class = "panel-heading", style = "margin-left: -14px; margin-top: 20px; margin-bottom: 0px;"),

          pickerInput(
            inputId = "iSHP", label = "Select Geographical Resolution:",
            choices = c("SA1", "SA2", "SA3", "SA4", "LGA"),
            options = list(`max-options` = 6, `size` = 5, `title` = "None selected"),
            multiple = FALSE),

          ## 1.4) Region. ------------------------------------------------------
          # State.
          pickerInput(
            inputId = "iSTE", label = "Select Focus Region:",
            choices = c("New South Wales", "Victoria", "Queensland", "South Australia",
                        "Western Australia", "Tasmania", "Northern Territory",
                        "Australian Capital Territory", "Other Territories", "SA4"),
            options = list(`max-options` = 9, `size` = 8, `title` = "None selected"),
            multiple = TRUE),

          # Help Text.
          tags$p(style = "font-size: 11px; margin-bottom: 16px;",
                 strong("Attention: "),
                 "Please make selections in the provided blue boxes."),

          # SA4 Area.
          pickerInput(
            inputId = 'iSA4', label = 'Select SA4 Focus Region:',
            choices = NULL,
            options = list(
              `actions-box` = TRUE, `title` = "None selected",
              `dropdown-align-right` = FALSE, `dropup` = FALSE),
            multiple = TRUE),
          div(style = "height: 60px;"),

          ## 1.5) Logos. -------------------------------------------------------
          div(style = "text-align: center;",
              img(src = "EPAlogo.png", width = "150px", height = "60px")),

          div(style = "height: 30px;"),

          div(style = "text-align: center;",
              img(src = "QUTlogo.png", width = "220px", height = "90px"))

          ),

        ## 1.5) Data Summary. --------------------------------------------------

        mainPanel(
            column(width = 12, align = "center",
                   verbatimTextOutput(outputId = "data_summary"),
                   style = "max-width: 900px; margin-top: 20px;")
          )
        )
      ),

    # 2) SECOND TAB - Data Manipulation. #######################################
    tabPanel(
      "Data Manipulation", fluid = TRUE,

      sidebarLayout(
        sidebarPanel(
          class = "fixed-panel",
          style = "position: fixed; top: 0px; left: 0; width: 305px; height: 100%; overflow: auto;",

          ## 2.1) Data Dictionary. ---------------------------------------------

          h5("Data Dictionary", class = "panel-heading", style = "margin-left: -14px; margin-top: 30px; margin-bottom: 0px;"),

          fluidRow(
            column(2, actionButton("show_sa1_vars", "SA1")),
            column(2, actionButton("show_sa2_vars", "SA2")),
            column(2, actionButton("show_sa3_vars", "SA3")),
            column(2, actionButton("show_sa4_vars", "SA4")),
            column(2, actionButton("show_lga_vars", "LGA"))
            ),
          div(style = "height: 10px;"),

          tags$p(style = "font-size: 11px; margin-bottom: 24px;",
                 "In-built dataset available. Variables in the app carry a prefix '*'. See data dictionary for details."),

          ## 2.2) Data Transformation. -----------------------------------------
          h5("Data Transformation", class = "panel-heading", style = "margin-left: -14px; margin-bottom: 0px;"),

          # Transformation Type.
          pickerInput(
            inputId = "transformOption", label = "Select Data Transformation:",
            choices = c("Normalisation", "Percentile Ranks",
                        "Logarithmic scale", "Square Root"),
            options = list(`actions-box` = TRUE, `title` = "None selected",
                           `dropdown-align-right` = FALSE, `dropup` = FALSE),
            multiple = TRUE),

          # Parameters.
          pickerInput(
            inputId = 'transform1', label = 'Select Parameters:',
            choices = NULL,
            options = list(
              `actions-box` = TRUE, `title` = "None selected",
              `dropdown-align-right` = FALSE, `dropup` = FALSE),
            multiple = TRUE),

          sliderInput("bins1", "Number of bins:", min = 1, max = 100, value = 30),
          div(style = "height: 12px;"),

          # Data Suggestions.
          div(
            class = "help-heading",
            tags$head(
              tags$style(
                HTML(
                  "
                  .custom-btn {
                  display: flex; align-items: center; justify-content: center;
                  padding: 0; border-color: none; background-color: lightgray;
                  cursor: pointer; transition: color 0.3s;
                  }
                  .custom-btn:hover {
                  color: yellow;
                  }
                  .custom-btn .icon {
                  font-size: 12px;
                  color: #6c757d;
                  }
                  .help-heading {
                  display: flex; align-items: center; margin-bottom: 20px;
                  }
                  .help-heading h5 {
                  margin: 0; font-size: 12px; margin-left: 0px;
                  }
                  "
                )
              )
            ),
            actionButton(
              inputId = "show_suggestions",
              div(class = "custom-btn", icon("lightbulb")),
              style = "border-color: none; background-color: lightgray;"
            ),
            h5(
              #strong("Click here:"),
              "Please click here for data transformation recommendations.",
              class = "panel-heading",
              style = "margin: 0; font-size: 12px;"
            )
          ),

          # Population-Based Data.
          h5("Population-Based Data", class = "panel-heading", style = "margin-left: -14px; margin-bottom: 0px;"),

          # Count Paramenter.
          pickerInput(
            inputId = 'transform2', label = 'Select Count Parameters:',
            choices = NULL,
            options = list(
              `actions-box` = TRUE, `title` = "None selected",
              `dropdown-align-right` = FALSE, `dropup` = FALSE),
            multiple = TRUE),

          # Total Population.
          pickerInput(
            inputId = 'total_pop', label = 'Select Total Population Parameter:',
            choices = NULL,
            options = list(
              `actions-box` = TRUE, `title` = "None selected",
              `dropdown-align-right` = FALSE, `dropup` = FALSE),
            multiple = TRUE),

          sliderInput("bins2", "Number of bins:", min = 1, max = 100, value = 30),
          div(style = "height: 12px;"),

          # Categorical Data.
          h5("Categorical variable"),

          pickerInput(
            inputId = 'transform3', label = 'Select Parameters:',
            choices = NULL,
            options = list(
              `actions-box` = TRUE, `title` = "None selected",
              `dropdown-align-right` = FALSE, `dropup` = FALSE),
            multiple = TRUE),

          sliderInput("bins3", "Number of bins:", min = 1, max = 100, value = 30),
          div(style = "height: 12px;"),

        ),

        ## 2.3) Graphs. --------------------------------------------------------
        mainPanel(
          style = "max-width: 900px; margin-top: 20px;",
          fluidRow(column(width = 12, verbatimTextOutput(outputId = "transformation1Message")),
                   column(width = 12, plotOutput(outputId = "histogram1")),
                   column(width = 12, verbatimTextOutput(outputId = "transformation2Message")),
                   column(width = 12, plotOutput(outputId = "histogram2")),
                   column(width = 12, verbatimTextOutput(outputId = "transformation3Message")),
                   column(width = 12, plotOutput(outputId = "histogram3")))
          )
        )
      ),

    # 3) THIRD TAB - Correlation. ##############################################
    tabPanel(
      "Correlation", fluid = TRUE,

      sidebarLayout(
        sidebarPanel(
          class = "fixed-panel",
          style = "position:fixed; top: 30px; left: 0; width: 305px; height: 100%; overflow: auto;",

          ## 3.1) Correlation Plot. --------------------------------------------

          h5("Correlation Plot"),

          pickerInput(
            inputId = 'iCorr', label = 'Select Parameters:',
            choices = NULL,
            options = list(
              `actions-box` = TRUE, `title` = "None selected",
              `dropdown-align-right` = FALSE, `dropup` = FALSE),
            multiple = TRUE)),

        ## 3.2) Graphs. --------------------------------------------------------
        mainPanel(
          div(style = "margin: 0; max-width: 800px;",
              plotOutput(outputId = "correlationplot"))
          )
        )
      ),

    # 4) FOURTH TAB - Select Parameters and Plot Map. ##########################
    tabPanel(
      "Map Visualization", fluid = TRUE,

      sidebarLayout(
        sidebarPanel(
          class = "fixed-panel",
          style = "position: fixed; top: 20px; left: 0; width: 305px; height: 100%; overflow: auto;",

          ## 4.1) Parameters. --------------------------------------------------

          h5("Parameter Selection", class = "panel-heading", style = "margin-left: -14px; margin-bottom: 0px;"),

          fluidRow(
            column(9,
              pickerInput(
                inputId = 'paramId1', label = 'Select a Parameter:',
                choices = NULL,
                options = list(
                  `actions-box` = TRUE,
                  `title` = "None selected",
                  `dropdown-align-right` = FALSE,
                  `dropup` = FALSE),
                multiple = FALSE),
              style = "padding-right: 5px;"
              ),
            column(3,
              div(style = "padding-top:25px; margin-left: 5px;",
                  checkboxInput(inputId = 'paramFlip1', label = 'Flip', value = FALSE, width = "100%"))
              )
            ),

          fluidRow(
            column(9,
                   pickerInput(
                     inputId = 'paramId2', label = 'Select a Second Parameter:',
                     choices = NULL,
                     options = list(
                       `actions-box` = TRUE,
                       `title` = "None selected",
                       `dropdown-align-right` = FALSE,
                       `dropup` = FALSE),
                     multiple = FALSE),
                   style = "padding-right: 5px;"
                   ),
            column(3,
                   div(style = "padding-top:25px; margin-left: 5px;",
                       checkboxInput(inputId = 'paramFlip2', label = 'Flip', value = FALSE, width = "100%"))
                   )
            ),

          fluidRow(
            column(9,
                   pickerInput(
                     inputId = 'paramId3', label = 'Select a Third Parameter:',
                     choices = NULL,
                     options = list(
                       `actions-box` = TRUE,
                       `title` = "None selected",
                       `dropdown-align-right` = FALSE,
                       `dropup` = FALSE),
                     multiple = FALSE),
                   style = "padding-right: 5px;"
                   ),
            column(3,
                   div(style = "padding-top:25px; margin-left: 5px;",
                       checkboxInput(inputId = 'paramFlip3', label = 'Flip', value = FALSE, width = "100%"))
                   )
            ),

          fluidRow(
            column(9,
                   pickerInput(
                     inputId = 'paramId4', label = 'Select a Fourth Parameter:',
                     choices = NULL,
                     options = list(
                       `actions-box` = TRUE,
                       `title` = "None selected",
                       `dropdown-align-right` = FALSE,
                       `dropup` = FALSE),
                     multiple = FALSE),
                   style = "padding-right: 5px;"
                   ),
            column(3,
                   div(style = "padding-top:25px; margin-left: 5px;",
                       checkboxInput(inputId = 'paramFlip4', label = 'Flip', value = FALSE, width = "100%"))
                   )
            ),

          ## 4.2) Additional Sidebar Panel. ------------------------------------
          div(
            style = "display: inline-block; width: 200px;",
            bsButton(
              inputId = "toggleSidebar",
              label = "View Density Plots",
              icon = icon("toggle-off"),
              type = "toggle",
              style = "info",
              value = FALSE,
              class = "wide-button"
            )
          ),

          ## 4.3) Input Latitude and Longitude. --------------------------------
          h5("Add Markers", class = "panel-heading", style = "margin-left: -14px; margin-top: 30px; margin-bottom: 0px;"),

          fluidRow(
            column(width = 6, align = "center",
                   numericInput(inputId = "lat", label = "Latitude:", value = NULL),
                   ),
            column(width = 6, align = "center",
                   numericInput(inputId = "lon", label = "Longitude:", value = NULL)
                   )
            ),

          div(style = "margin-bottom: -10px;",
              fileInput(inputId = "LatLon_File", label = "Upload Lat/Lon File:", accept = ".csv")),

          fluidRow(
            column(width = 6, align = "center",
                   actionButton(inputId = "addMarker", label = "Add", class = "btn-block")
                   ),
            column(width = 6, align = "center",
                   actionButton(inputId = "removeAllMarkers", label = "Remove", class = "btn-block")
                   )
            ),
          div(style = "height: 30px;"),


          ## 4.4) Plot/Reset App. ----------------------------------------------
          fluidRow(
            column(width = 8,
                   div(style = "margin-top: 24.4px;",
                       actionButton(inputId = "plot_map", label = "Plot Map", class = "btn-block"))
                   ),
            column(width = 2,
                   div(style = "margin-top: 24.4px; margin-left: -20px;",
                       actionButton(inputId = "refresh", label = NULL,
                                    title = "Refresh App", icon("refresh")))
                   ),
            column(width = 2,
                   div(style = "margin-top: 24.4px; margin-left: -20px;",
                       downloadButton(outputId = "downloadMap", label = NULL,
                                      title = "Download Map"))
                   )
            )
          ),

        ## 4.5) Map. -----------------------------------------------------------
        mainPanel(
          div(class = "outer",
              tags$style(
                type = "text/css",
                ".outer {position: fixed; top: 40px; left: 305px; right: 0; bottom: 0; overflow: hidden; padding: 0; }",
                ".map-panel {position: absolute;  top: 0; left: 0; right: 0; bottom: 0; overflow: hidden; }",
                ".plot-line {border-top: 1px solid #CCCCCC; margin-bottom: 10px; padding-top: 10px;}"),

            uiOutput(outputId = "extraSidebarStyle"),

            div(class = "map-panel",
                leafletOutput(outputId = "map", width = "100%", height = "100%")),

            div(class = "extra-sidebar",
                id = "extraSidebar",
                tags$h5("Density Plots of Selected Parameter(s)", class = "plot-heading"),
                plotOutput(outputId = "densityPlot1", height = "84px", width = "435px"),
                tags$div(class = "plot-line"),
                plotOutput(outputId = "densityPlot2", height = "84px", width = "435px"),
                tags$div(class = "plot-line"),
                plotOutput(outputId = "densityPlot3", height = "84px", width = "435px"),
                tags$div(class = "plot-line"),
                plotOutput(outputId = "densityPlot4", height = "84px", width = "435px"))
            )
          )
        )
      ),

    # 5) FIFTH TAB - User Guide. ###############################################
    tabPanel(
      "User Guide", fluid = TRUE,

      sidebarLayout(
        sidebarPanel(
          class = "fixed-panel",
          style = "position:fixed; top: 30px; left: 0; width: 305px; height: 100%; overflow: auto;",

          ## 5.1) Sidebar options for each tab. --------------------------------

          h5("User Guide Menu"),

          # Upload Data tab options.
            pickerInput(
              inputId = 'uploadDataOption',
              label = 'Upload Data Menu:',
              choices = c("Upload CSV file", "Data Check",
                          "Mandatory Fields", "Geographical Resolution",
                          "Focus Region", "SA4 Focus Region"),
              options = list(
                `actions-box` = TRUE,
                `title` = "None selected",
                `dropdown-align-right` = FALSE,
                `dropup` = FALSE),
              multiple = TRUE),

          # Data Manipulation tab options.
            pickerInput(
              inputId = 'dataManipulationOption',
              label = 'Data Manipulation Menu:',
              choices = c("Data Dictioary", "Data Transformation", "Recommendations",
                          "Population-Based Data", "Categorical Variable"),
              options = list(
                `actions-box` = TRUE,
                `title` = "None selected",
                `dropdown-align-right` = FALSE,
                `dropup` = FALSE),
              multiple = TRUE),

          # Correlation tab options.
            pickerInput(
              inputId = 'correlationOption',
              label = 'Correlation Menu:',
              choices = "Correlation Plot",
              options = list(
                `actions-box` = TRUE,
                `title` = "None selected",
                `dropdown-align-right` = FALSE,
                `dropup` = FALSE),
              multiple = TRUE),

          # Map Visualization tab options.
            pickerInput(
              inputId = 'mapVisualizationOption',
              label = 'Map Visualization Menu:',
              choices = c("Parameter Selection", "View Density Plots", "Add Markers",
                          "Plot Map", "Draw Map Features", "Refresh", "Download"),
              options = list(
                `actions-box` = TRUE,
                `title` = "None selected",
                `dropdown-align-right` = FALSE,
                `dropup` = FALSE),
              multiple = TRUE)
          ),

        ## 5.2) Main panel for displaying selected options. --------------------
        mainPanel(
          div(style = "margin-top: 40px; max-width: 800px;",
              tableOutput("uploadDataOptionTable")),
          div(style = "margin-top: 40px; max-width: 800px;",
              tableOutput("dataManipulationOptionTable")),
          div(style = "margin-top: 40px; max-width: 800px;",
              tableOutput("correlationOptionTable")),
          div(style = "margin-top: 40px; max-width: 800px;",
              tableOutput("mapVisualizationOptionTable")),
          )
        )
      )
    )
  )































