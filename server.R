# Shiny Server. ################################################################
server <- function(input, output, session) {

  # 1) FIRST TAB - Upload Data. ------------------------------------------------

  ## 1.1) Data. ----------------------------------------------------------------
  data_files <- reactive({
    req(input$csv_File)
    data_list <- lapply(input$csv_File$datapath, function(file) {
      data <- read.csv(file)
      data
    })
    names(data_list) <- input$csv_File$name
    data_list
  })

  ## 1.3) Data Summary. --------------------------------------------------------
  observe({
    req(data_files())

    if (is.null(data_files())) {
      output$data_summary <- renderPrint(NULL)
    } else {
      output$data_summary <- renderPrint({
        for (i in 1:length(data_files())) {
          cat("Data file: ", names(data_files())[i], "\n")
          glimpse(data_files()[[i]])
          cat("\n\n")
        }
      })
    }
  })

  ## 1.4) Data Check. ----------------------------------------------------
  observeEvent(data_files(), {
    data_check1 <- data_files()

    # Filter data_check1
    for (i in seq_along(data_check1)) {
      data_check1[[i]] <- data_check1[[i]][!grepl("csvId|CODE|NAME|AREASQKM", names(data_check1[[i]]))]
    }

    # Identify data problems
    data_problems <- DataCheck(data_check1)

    # Split dialogue into separate lines
    dialogue_lines <- strsplit(data_problems$dialogue, "\\.")
    dialogue_lines <- unlist(dialogue_lines)

    # Generate HTML bullet points
    dialogue_bullet_points <- paste("<li>", dialogue_lines, "</li>", sep="")
    dialogue_html <- paste("<ul>", dialogue_bullet_points, "</ul>", sep="")

    # Show modal dialog with data problems
    showModal(
      modalDialog(
        title = "Data Inconsistency Detected",
        HTML(dialogue_html),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        ),
        size = "m",
        fade = TRUE
      )
    )
  })

  observeEvent(input$check_data_button, {
    if (is.null(input$csv_File) || length(input$csv_File$datapath) == 0) {
      showModal(
        modalDialog(
          title = "No File Uploaded",
          HTML("<p>Please upload a CSV file before checking data inconsistencies.</p>"),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          ),
          size = "m",
          fade = TRUE
        )
      )
    } else {
      data_check2 <- data_files()

      # Filter data_check2
      for (i in seq_along(data_check2)) {
        data_check2[[i]] <- data_check2[[i]][!grepl("csvId|CODE|NAME|AREASQKM", names(data_check2[[i]]))]
      }

      # Identify data problems
      data_problems <- DataCheck(data_check2)

      # Show modal dialog with data problems
      if (!is.null(data_problems$problems) && nrow(data_problems$problems) > 0) {
        showModal(
          modalDialog(
            title = "Data Inconsistency",
            list(
              HTML("Data inconsistency were detected in the dataset. Please address the following issues to ensure data accuracy and reliability."),
              tags$div(
                style = "overflow-x: auto;",
                renderDataTable(data_problems$problems, options = list(scrollX = TRUE))
              )
            ),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            ),
            size = "m",
            fade = TRUE
          )
        )
      } else {
        showModal(
          modalDialog(
            title = "Data Inconsistency",
            HTML("<p>No issues were found in the dataset.</p>"),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            ),
            size = "m",
            fade = TRUE
          )
        )
      }
    }
  })

  ## 1.5) CSV ID. --------------------------------------------------------------
  csvId <- reactive({
    req(input$iSHP, input$iSTE)

    if (!is.null(input$csv_File)) {
      df_list <- data_files()
      df_list <- lapply(df_list, function(csv_File) {
        switch(input$iSHP,
               "SA1" = {
                 sa1_csvId <- grep("^SA1_", names(csv_File), value = TRUE)
                 if (any(grepl("CODE", sa1_csvId))) {
                   csv_File[['csvId']] <- csv_File[[sa1_csvId[grepl("CODE", sa1_csvId)]]]
                 }
                 csv_File <- merge(csv_File, builtinData$sa1$indicators, by.x = "csvId", by.y = "SA1_CODE", all.x = TRUE)
                 cols_sa1 <- c()



                 names(csv_File)[names(csv_File) %in% cols_sa1] <- paste0("*", cols_sa1)
                 csv_File
               },
               "SA2" = {
                 sa2_csvId <- grep("^SA2_", names(csv_File), value = TRUE)
                 if (any(grepl("CODE", sa2_csvId))) {
                   csv_File[['csvId']] <- csv_File[[sa2_csvId[grepl("CODE", sa2_csvId)]]]
                 }
                 csv_File <- merge(csv_File, builtinData$sa2$indicators, by.x = "csvId", by.y = "SA2_CODE", all.x = TRUE)
                 cols_sa2 <- c("usual_resident_population", "irsd_score", "irsd_percentile", "irsd_quintile", "pm25_weighted_mean", "pm25_percentile",
                               "pm25_quintile", "no2_weighted_mean", "no2_percentile", "no2_quintile", "age_0_5", "age_6_18",
                               "age_19_64", "age_65_over", "total", "age_0_18", "age_0_5_percent", "age_0_18_percent", "age_65_over_percent", "age_0_5_percent_percentile", "age_0_18_percent_percentile",
                               "age_65_over_percent_percentile", "age_0_5_percent_quintile", "age_0_18_percent_quintile", "age_65_over_percent_quintile",  "life_expectancy", "life_expectancy_percentile",
                               "life_expectancy_quintile")



                 names(csv_File)[names(csv_File) %in% cols_sa2] <- paste0("*", cols_sa2)
                 csv_File
               },
               "SA3" = {
                 sa3_csvId <- grep("^SA3_", names(csv_File), value = TRUE)
                 if (any(grepl("CODE", sa3_csvId))) {
                   csv_File[['csvId']] <- csv_File[[sa3_csvId[grepl("CODE", sa3_csvId)]]]
                 }
                 csv_File <- merge(csv_File, builtinData$sa3$indicators, by.x = "csvId", by.y = "SA3_CODE", all.x = TRUE)
                 cols_sa3 <- c()



                 names(csv_File)[names(csv_File) %in% cols_sa3] <- paste0("*", cols_sa3)
                 csv_File
               },
               "SA4" = {
                 sa4_csvId <- grep("^SA4_", names(csv_File), value = TRUE)
                 if (any(grepl("CODE", sa4_csvId))) {
                   csv_File[['csvId']] <- csv_File[[sa4_csvId[grepl("CODE", sa4_csvId)]]]
                 }
                 csv_File <- merge(csv_File, builtinData$sa4$indicators, by.x = "csvId", by.y = "SA4_CODE", all.x = TRUE)
                 cols_sa4 <- c()



                 names(csv_File)[names(csv_File) %in% cols_sa4] <- paste0("*", cols_sa4)
                 csv_File
               },
               "LGA" = {
                 lga_csvId <- grep("^LGA_", names(csv_File), value = TRUE)
                 if (any(grepl("CODE", lga_csvId))) {
                   csv_File[['csvId']] <- csv_File[[lga_csvId[grepl("CODE", lga_csvId)]]]
                 }
                 # For LGA, there is no data to be merged, I'm leaving it as is. Adjust this accordingly.
                 csv_File
               }
        )
      })

      df_list <- lapply(df_list, function(df) {
        df[df$STE_NAME21 %in% input$iSTE, ]
      })
    } else {
      df_list <- list() # initialize an empty list
    }
    if(length(df_list) == 0){
      df_list <- switch(input$iSHP,
                        "SA1" = {
                          if("sa1" %in% names(builtinData)) {
                            csv_File <- builtinData[["sa1"]][["indicators"]]
                            sa1_csvId <- grep("^SA1_", names(csv_File), value = TRUE)
                            if (any(grepl("CODE", sa1_csvId))) {
                              csv_File[['csvId']] <- csv_File[[sa1_csvId[grepl("CODE", sa1_csvId)]]]
                            }
                            cols_sa1 <- c()
                            names(csv_File)[names(csv_File) %in% cols_sa1] <- paste0("*", cols_sa1)
                            csv_File
                          } else {
                            data.frame() # empty data frame
                          }
                        },
                        "SA2" = {
                          if("sa2" %in% names(builtinData)) {
                            csv_File <- builtinData[["sa2"]][["indicators"]]
                            sa2_csvId <- grep("^SA2_", names(csv_File), value = TRUE)
                            if (any(grepl("CODE", sa2_csvId))) {
                              csv_File[['csvId']] <- csv_File[[sa2_csvId[grepl("CODE", sa2_csvId)]]]
                            }
                            cols_sa2 <- c("usual_resident_population", "irsd_score", "irsd_percentile", "irsd_quintile", "pm25_weighted_mean", "pm25_percentile",
                                          "pm25_quintile", "no2_weighted_mean", "no2_percentile", "no2_quintile", "age_0_5", "age_6_18",
                                          "age_19_64", "age_65_over", "total", "age_0_18", "age_0_5_percent", "age_0_18_percent", "age_65_over_percent", "age_0_5_percent_percentile", "age_0_18_percent_percentile",
                                          "age_65_over_percent_percentile", "age_0_5_percent_quintile", "age_0_18_percent_quintile", "age_65_over_percent_quintile",  "life_expectancy", "life_expectancy_percentile",
                                          "life_expectancy_quintile")
                            names(csv_File)[names(csv_File) %in% cols_sa2] <- paste0("*", cols_sa2)
                            csv_File
                          } else {
                            data.frame() # empty data frame
                          }
                        },
                        "SA3" = {
                          if("sa3" %in% names(builtinData)) {
                            csv_File <- builtinData[["sa3"]][["indicators"]]
                            sa3_csvId <- grep("^SA3_", names(csv_File), value = TRUE)
                            if (any(grepl("CODE", sa3_csvId))) {
                              csv_File[['csvId']] <- csv_File[[sa3_csvId[grepl("CODE", sa3_csvId)]]]
                            }
                            cols_sa3 <- c()
                            names(csv_File)[names(csv_File) %in% cols_sa3] <- paste0("*", cols_sa3)
                            csv_File
                          } else {
                            data.frame() # empty data frame
                          }
                        },
                        "SA4" = {
                          if("sa4" %in% names(builtinData)) {
                            csv_File <- builtinData[["sa4"]][["indicators"]]
                            sa4_csvId <- grep("^SA4_", names(csv_File), value = TRUE)
                            if (any(grepl("CODE", sa4_csvId))) {
                              csv_File[['csvId']] <- csv_File[[sa4_csvId[grepl("CODE", sa4_csvId)]]]
                            }
                            cols_sa4 <- c()
                            names(csv_File)[names(csv_File) %in% cols_sa4] <- paste0("*", cols_sa4)
                            csv_File
                          } else {
                            data.frame() # empty data frame
                          }
                        }
      )
      df_list <- list(df_list) # to maintain the list structure
      df_list <- lapply(df_list, function(df) {
        df[df$STE_NAME %in% input$iSTE, ]
      })
    }

    df_list
  })

  ## 1.6) Shapefile ID. --------------------------------------------------------
  shpId <- reactive({
    req(input$iSHP, input$iSTE)
    shp_files <- list.files(path = "data/Region Shapes - 2021", pattern = "\\.shp$", full.names = TRUE)
    shp_files <- switch(
      input$iSHP,
      "SA1" = shp_files[grep("SA1", shp_files)],
      "SA2" = shp_files[grep("SA2", shp_files)],
      "SA3" = shp_files[grep("SA3", shp_files)],
      "SA4" = shp_files[grep("SA4", shp_files)],
      "LGA" = shp_files[grep("LGA", shp_files)]
    )

    if (length(shp_files) > 0) {
      shp <- st_read(shp_files[1])
      shp <- st_transform(shp, 4326)


      shp <- switch(input$iSHP,
                    "SA1" = {
                      sa1_shpId  <- grep("^SA1_", names(shp), value = TRUE)
                      if (any(grepl("CODE", sa1_shpId))) {
                        shp[['shpId']] <- shp[[sa1_shpId[grepl("CODE", sa1_shpId)]]]
                      }
                      shp
                    },
                    "SA2" = {
                      sa2_shpId  <- grep("^SA2_", names(shp), value = TRUE)
                      if (any(grepl("CODE", sa2_shpId))) {
                        shp[['shpId']] <- shp[[sa2_shpId[grepl("CODE", sa2_shpId)]]]
                      }
                      shp
                    },
                    "SA3" = {
                      sa3_shpId  <- grep("^SA3_", names(shp), value = TRUE)
                      if (any(grepl("CODE", sa3_shpId))) {
                        shp[['shpId']] <- shp[[sa3_shpId[grepl("CODE", sa3_shpId)]]]
                      }
                      shp
                    },
                    "SA4" = {
                      sa4_shpId  <- grep("^SA4_", names(shp), value = TRUE)
                      if (any(grepl("CODE", sa4_shpId))) {
                        shp[['shpId']] <- shp[[sa4_shpId[grepl("CODE", sa4_shpId)]]]
                      }
                      shp
                    },
                    "LGA" = {
                      lga_shpId  <- grep("^LGA_", names(shp), value = TRUE)
                      if (any(grepl("CODE", lga_shpId))) {
                        shp[['shpId']] <- shp[[lga_shpId[grepl("CODE", lga_shpId)]]]
                      }
                      shp
                    }
      )
      shp <- shp[shp$STE_NAME21 %in% input$iSTE,]
      shp
    }
  })

  ## 1.7) SA4. -----------------------------------------------------------------
  observe({
    req(shpId())
    updatePickerInput(session,
                      inputId = 'iSA4',
                      choices = c("None selected", unique(shpId()$SA4_NAME21)),
                      selected = character(0))
  })

  filteredShp <- reactive({
    req(shpId())
    if (is.null(input$iSA4) || "None selected" %in% input$iSA4) {
      return(shpId())
    }
    return(shpId()[shpId()$SA4_NAME21 %in% input$iSA4,])
  })

  ## 1.8) Mandatory input selection. -------------------------------------------
  observe({
    if (is.null(input$iSHP) || input$iSHP == "") {
      session$sendCustomMessage("updateBackground", list(inputId = "iSHP", color = "lightblue"))
    } else {
      session$sendCustomMessage("updateBackground", list(inputId = "iSHP", color = ""))
    }

    if (length(input$iSTE) == 0) {
      session$sendCustomMessage("updateBackground", list(inputId = "iSTE", color = "lightblue"))
    } else {
      session$sendCustomMessage("updateBackground", list(inputId = "iSTE", color = ""))
    }
  })


  # 2) SECOND TAB - Data Manipulation. #########################################

  ## 2.1) Data Dictionary. -----------------------------------------------------

  # SA1 - Tool Variables
  observeEvent(input$show_sa1_vars, {
    output$sa1_vars_table <- renderDataTable({
      datatable(variables_sa1_df, options = list(pageLength = 10, searching = FALSE),
                class = 'cell-border stripe', escape = FALSE)
    })

    showModal(modalDialog(
      title = "SA1 - Tool Variables",
      dataTableOutput("sa1_vars_table")
    ))
  })

  # SA2 - Tool Variables
  observeEvent(input$show_sa2_vars, {
    output$sa2_vars_table <- renderDataTable({
      datatable(variables_sa2_df, options = list(pageLength = 10, searching = FALSE),
                class = 'cell-border stripe', escape = FALSE)
    })

    showModal(modalDialog(
      title = "SA2 - Tool Variables",
      dataTableOutput("sa2_vars_table")
    ))
  })

  # SA3 - Tool Variables
  observeEvent(input$show_sa3_vars, {
    output$sa3_vars_table <- renderDataTable({
      datatable(variables_sa3_df, options = list(pageLength = 10, searching = FALSE),
                class = 'cell-border stripe', escape = FALSE)
    })

    showModal(modalDialog(
      title = "SA3 - Tool Variables",
      dataTableOutput("sa3_vars_table")
    ))
  })

  # SA4 - Tool Variables
  observeEvent(input$show_sa4_vars, {
    output$sa4_vars_table <- renderDataTable({
      datatable(variables_sa4_df, options = list(pageLength = 10, searching = FALSE),
                class = 'cell-border stripe', escape = FALSE)
    })

    showModal(modalDialog(
      title = "SA4 - Tool Variables",
      dataTableOutput("sa4_vars_table")
    ))
  })

  # observeEvent(input$show_lga_vars, {
  #   showModal(modalDialog(
  #     title = "LGA - Tool Variables",
  #     HTML(variables_lga_str)
  #   ))
  # })
  observeEvent(input$show_lga_vars, {
    showModal(modalDialog(
      title = "LGA - Tool Variables",
      "No data available for LGA."
    ))
  })


  ## 2.2) Data Recommendations. ------------------------------------------------
  observeEvent(input$show_suggestions, {

    data_transformed <- NULL  # Initialize data_transformed to NULL

    if (is.null(input$csv_File) || length(input$csv_File$datapath) == 0) {
      showModal(
        modalDialog(
          title = "No File Uploaded",
          HTML("<p>Please upload a CSV file before checking data transformation recommendations.</p>"),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          ),
          size = "m",
          fade = TRUE
        )
      )
    } else {
      # Process the uploaded CSV files
      data_transformed <- lapply(data_files(), function(df) {
        data_transformed_list <- list()
        for (col in names(df)) {
          if (!grepl("csvId|CODE|NAME|AREASQKM", col)) {
            col_df <- data.frame(variable = df[[col]], stringsAsFactors = FALSE)
            names(col_df) <- col
            data_transformed_list[[col]] <- col_df
            names(data_transformed_list) <- seq_along(data_transformed_list)
          }
        }
        data_transformed_list
      })
    }

    if (!is.null(data_transformed)) {
      suggestions <- lapply(data_transformed, function(inner_list) {
        lapply(inner_list, function(df) {
          df %>% DataTransformCheck$getTransSug()
        })
      })

      all_suggestions <- do.call(rbind, lapply(suggestions, function(inner_list) {
        do.call(rbind, inner_list)
      }))

      showModal(
        modalDialog(
          title = "Data Transformation Recommendations",
          renderDataTable(all_suggestions),
          footer = modalButton("Close")
        )
      )
    }
  })

  ## 2.3) Data Transformation. -------------------------------------------------

  ### 2.3.1) Parameters.--------------------------------------------------------
  observe({
    csv_Files <- csvId()

    transform_params1 <- unique(unlist(lapply(csv_Files, function(file) {
      names(file)[grep("csvId|CODE|NAME|AREASQKM", names(file), invert = TRUE)]
    })))

    updatePickerInput(session,
                      inputId = 'transform1',
                      choices = transform_params1,
                      selected = character(0))
  })

  ### 2.3.2) Data Transformation Functions. ------------------------------------
  transformedData1 <- reactive({
    transform_params1 <- input$transform1
    if (length(transform_params1) == 0) {
      return(NULL)
    }
    req(length(transform_params1) > 0)
    csv_FileList <- csvId()

    transformed_FileList1 <- lapply(csv_FileList, function(csv_File) {
      for (param1 in transform_params1) {
        if (param1 %in% names(csv_File)) {
          if (is.character(csv_File[[param1]])) {
            csv_File[[param1]] <- as.numeric(csv_File[[param1]])
          }

          if (is.numeric(csv_File[[param1]])) {

            if ("Normalisation" %in% input$transformOption) {
              csv_File[[paste0(param1, "_norm")]] <- scale(csv_File[[param1]])
            }

            if ("Percentile Ranks" %in% input$transformOption) {
              csv_File[[paste0(param1, "_perc")]] <- percent_rank(csv_File[[param1]])
            }

            if ("Logarithmic scale" %in% input$transformOption) {
              if (any(csv_File[[param1]] == 0)) {
                csv_File[[paste0(param1, "_log")]] <- log1p(csv_File[[param1]])
              } else {
                csv_File[[paste0(param1, "_log")]] <- log(csv_File[[param1]])
              }
            }

            if ("Square Root" %in% input$transformOption) {
              csv_File[[paste0(param1, "_sqrt")]] <- sqrt(csv_File[[param1]])
            }
          }
        }
      }
      csv_File
    })

    if (is.null(input$transformOption) || input$transformOption == "") {
      output$transformation1Message <- renderText(NULL)
      return(NULL)
    }

    if (isTruthy(input$refresh)) {
      return(NULL)
    } else {
      output$transformation1Message <- renderText({
        message <- character()
        if ("Normalisation" %in% input$transformOption) {
          message <- c(message, paste("The selected parameter", input$transform1, "has been normalised."))
        }
        if ("Percentile Ranks" %in% input$transformOption) {
          message <- c(message, paste("The selected parameter", input$transform1, "has been transformed to percentile ranks."))
        }
        if ("Logarithmic scale" %in% input$transformOption) {
          message <- c(message, paste("The selected parameter", input$transform1, "has been transformed using a logarithmic scale."))
        }
        if ("Square Root" %in% input$transformOption) {
          message <- c(message, paste("The selected parameter", input$transform1, "has been transformed using the square root function."))
        }
        HTML(paste(message, collapse = "\n"))
      })
    }

    transformed_FileList1
  })


  ## 2.4) Population-Based Data. -----------------------------------------------

  ### 2.4.1) Count of population. ----------------------------------------------
  observe({
    csv_Files <- csvId()

    transform_params2 <- unique(unlist(lapply(csv_Files, function(file) {
      names(file)[grep("csvId|CODE|NAME|AREASQKM", names(file), invert = TRUE)]
    })))

    updatePickerInput(session,
                      inputId = 'transform2',
                      choices = transform_params2,
                      selected = character(0))
  })

  ### 2.4.2) Total population. -------------------------------------------------
  observe({
    csv_Files <- csvId()

    pop_params2 <- unique(unlist(lapply(csv_Files, function(file) {
      names(file)[grep("csvId|CODE|NAME|AREASQKM", names(file), invert = TRUE)]
    })))

    updatePickerInput(session,
                      inputId = 'total_pop',
                      choices = pop_params2,
                      selected = character(0))
  })

  ### 2.4.3) Data Transformation Function.--------------------------------------
  transformedData2 <- reactive({
    transform_params2 <- input$transform2
    if (length(transform_params2) == 0) {
      return(NULL)
    }
    req(length(transform_params2) > 0)
    csv_FileList <- csvId()

    transformed_FileList2 <- lapply(csv_FileList, function(csv_File) {
      for (param2 in transform_params2) {
        if (param2 %in% names(csv_File)) {
          if (is.character(csv_File[[param2]])) {
            csv_File[[param2]] <- as.numeric(csv_File[[param2]])
          }
          if (is.numeric(csv_File[[param2]])) {
            req(input$total_pop != "None selected")
            if (input$total_pop %in% names(csv_File)) {
              csv_File[[input$total_pop]] <- as.numeric(csv_File[[input$total_pop]])
              if (is.numeric(csv_File[[input$total_pop]])) {
                csv_File[[paste0(param2, "_pop")]] <- ((csv_File[[param2]]) / (csv_File[[input$total_pop]])) * 1000 # 100 (as percentage) | 1000 (per mile)
                csv_File[[paste0(param2, "_pop")]] <- replace_na(csv_File[[paste0(param2, "_pop")]], 0)
              }
            }
          }
        }
      }
      csv_File
    })

    output$transformation2Message <- renderText({
      message <- character()
      if (isTruthy(input$refresh)) {
        return(NULL)
      } else {
        if (length(transform_params2) > 0) {
          for (param2 in transform_params2) {
            message <- c(message, paste("The selected parameter", param2, "has been transformed using a population-based transformation."))
          }
        }
      }
      HTML(paste(message, collapse = "\n"))
    })

    transformed_FileList2
  })

  ## 2.5) Categorical Data. ----------------------------------------------------

  ### 2.5.1) Parameters.--------------------------------------------------------
  observe({
    csv_Files <- csvId()

    transform_params3 <- unique(unlist(lapply(csv_Files, function(file) {
      names(file)[grep("csvId|CODE|NAME|AREASQKM", names(file), invert = TRUE)]
    })))

    updatePickerInput(session,
                      inputId = 'transform3',
                      choices = transform_params3,
                      selected = character(0))
  })

  ### 2.5.2) Data Transformation Function. -------------------------------------
  transformedData3 <- reactive({
    transform_params3 <- input$transform3
    if (length(transform_params3) == 0) {
      return(NULL)
    }
    req(length(transform_params3) > 0)
    csv_FileList <- csvId()

    transformed_FileList3 <- lapply(csv_FileList, function(csv_File) {
      for (param3 in transform_params3) {
        if (param3 %in% names(csv_File)) {
          levels <- unique(csv_File[[param3]])
          csv_File[[param3]] <- factor(csv_File[[param3]], levels = levels, ordered = TRUE)
          ecdf_func <- ecdf(as.numeric(csv_File[[param3]]))
          percentile_ranks <- ecdf_func(as.numeric(csv_File[[param3]]))
          csv_File[[paste0(param3, "_rank")]] <- percentile_ranks
        }
      }
      csv_File
    })

    output$transformation3Message <- renderText({
      message <- character()
      if (isTruthy(input$refresh)) {
        return(NULL)
      } else {
        if (length(transform_params3) > 0) {
          for (param3 in transform_params3) {
            message <- c(message, paste("The selected parameter", param3, "has been transformed using ranking transformation."))
          }
        }
      }
      HTML(paste(message, collapse = "\n"))
    })

    transformed_FileList3
  })

  ## 2.6) Histogram.------------------------------------------------------------

  ### 2.6.1) Function to generate plots. ----------------------------------------
  generate_plots <- function(csv_File, transform_params, transformOptions, bins) {
    lapply(transform_params, function(param) {
      lapply(transformOptions, function(transformOption) {
        if (length(transformOption) == 1) {
          transParam <- switch(transformOption,
                               "Normalization" = paste0(param, "_norm"),
                               "Percentile Ranks" = paste0(param, "_perc"),
                               "Logarithmic scale" = paste0(param, "_log"),
                               "Square Root" = paste0(param, "_sqrt"),
                               "Population-Based Data" = paste0(param, "_pop"),
                               "Categorical Data" = paste0(param, "_rank"),
                               param)
        } else {
          transParam <- param
        }

        if (transParam %in% names(csv_File) && is.numeric(csv_File[[transParam]])) {
          p <- ggplot(csv_File, aes(x = .data[[transParam]])) +
            geom_histogram(color = "darkblue", fill = "lightblue", bins = bins) +
            xlab("Value") + ylab("Frequency") + ggtitle(paste0("Histogram of ", transParam)) +
            theme_classic() +
            theme(
              plot.title = element_text(size = 20),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14)
            )
          return(p)
        } else {
          return(NULL)
        }
      })
    })
  }

  ### 2.6.2) Histogram 1. ------------------------------------------------------
  output$histogram1 <- renderPlot({
    if (!isTruthy(transformedData1())) {
      return(NULL)
    }
    transform_params1 <- input$transform1
    if (length(transform_params1) == 0) {
      return(NULL)
    }

    csv_FileList <- transformedData1()
    plotList <- lapply(csv_FileList, function(csv_File) {
      generate_plots(csv_File, transform_params1, input$transformOption, input$bins1)
    })

    plotList <- unlist(plotList, recursive = FALSE)
    if (length(plotList) == 0) {
      return(NULL)
    }
    plotList <- unlist(plotList, recursive = FALSE)
    plotList <- plotList[sapply(plotList, function(p) class(p)[1] == "gg" || class(p)[1] == "ggplot")]

    if (length(plotList) == 0) {
      return(NULL)
    }

    grid.arrange(grobs = plotList, ncol = 2)
  })

  ### 2.6.3) Histogram 2. ------------------------------------------------------
  output$histogram2 <- renderPlot({
    if (!isTruthy(transformedData2())) {
      return(NULL)
    }
    transform_params2 <- input$transform2
    if (length(transform_params2) == 0) {
      return(NULL)
    }

    csv_FileList2 <- transformedData2()
    plotList2 <- lapply(csv_FileList2, function(csv_File) {
      generate_plots(csv_File, transform_params2, "Population-Based Data", input$bins2)
    })

    plotList2 <- unlist(plotList2, recursive = FALSE)
    if (length(plotList2) == 0) {
      return(NULL)
    }
    plotList2 <- unlist(plotList2, recursive = FALSE)
    plotList2 <- plotList2[sapply(plotList2, function(p) class(p)[1] == "gg" || class(p)[1] == "ggplot")]

    if (length(plotList2) == 0) {
      return(NULL)
    }

    grid.arrange(grobs = plotList2, ncol = 2)
  })

  ### 2.6.4) Histogram 3. ------------------------------------------------------
  output$histogram3 <- renderPlot({
    if (!isTruthy(transformedData3())) {
      return(NULL)
    }
    transform_params3 <- input$transform3
    if (length(transform_params3) == 0) {
      return(NULL)
    }

    csv_FileList3 <- transformedData3()
    plotList3 <- lapply(csv_FileList3, function(csv_File) {
      generate_plots(csv_File, transform_params3, "Categorical Data", input$bins3)
    })

    plotList3 <- unlist(plotList3, recursive = FALSE)
    if (length(plotList3) == 0) {
      return(NULL)
    }
    plotList3 <- unlist(plotList3, recursive = FALSE)
    plotList3 <- plotList3[sapply(plotList3, function(p) class(p)[1] == "gg" || class(p)[1] == "ggplot")]

    if (length(plotList3) == 0) {
      return(NULL)
    }

    grid.arrange(grobs = plotList3, ncol = 2)
  })

  ## 2.7) New data. ------------------------------------------------------------
  csv_Files <- reactive({
    csv_FilesAll <- csvId()

    for (i in 1:3) {
      transformed_Data <- get(paste0("transformedData", i))
      if (isTruthy(transformed_Data())) {
        csv_FilesAll <- c(csv_FilesAll, transformed_Data())
      }
      # print(paste0("csv_FilesAll after iteration ", i, ":"))
      # print(csv_FilesAll)
    }
    return(csv_FilesAll)

  })

  # 3) THIRD TAB - Correlation. ################################################

  ## 3.1) Parameters. ----------------------------------------------------------
  observe({
    corr_params <- unique(unlist(lapply(csv_Files(), function(file) {
      names(file)[grep("csvId|CODE|NAME|AREASQKM", names(file), invert = TRUE)]
    })))

    updatePickerInput(session,
                      inputId = 'iCorr',
                      choices = corr_params,
                      selected = character(0))
  })

  ## 3.2) Correlation Plot. ----------------------------------------------------
  output$correlationplot <- renderPlot({
    req(input$iCorr)
    correlation_params <- input$iCorr

    correlationList <- lapply(csv_Files(), function(csv_File) {
      existingParams <- correlation_params[correlation_params %in% names(csv_File)]
      if (length(existingParams) < 1) {
        return(NULL)
      }
      selectedColumns <- csv_File[, existingParams, drop = FALSE]
      selectedColumns[] <- lapply(selectedColumns, function(col){
        numeric_col <- as.numeric(as.character(col))
        if(any(is.na(numeric_col))){
          return(NULL)
        }
        return(numeric_col)
      })
      selectedColumns <- selectedColumns[sapply(selectedColumns, function(col) !is.null(col))]
      return(selectedColumns)
    })

    correlationList <- correlationList[!sapply(correlationList, is.null)]
    if(length(correlationList) == 0){
      return(NULL)
    }

    correlationData <- Reduce(function(df1, df2) {merge(df1, df2, by="row.names")}, correlationList)

    if (nrow(correlationData) > 0 & ncol(correlationData) > 1) {
      pairs.panels(correlationData,
                   smooth = FALSE,
                   scale = FALSE,
                   density = TRUE,
                   ellipses = TRUE,
                   method = "pearson",
                   pch = 21,
                   lm = FALSE,
                   cor = TRUE,
                   jiggle = FALSE,
                   factor = 2,
                   hist.col = "lightblue",
                   stars = TRUE,
                   rug = TRUE,
                   ci = TRUE,
                   cex = 1,
                   cex.axis = 1.5,
                   cex.labels = 2.5)
    }
  })

  # 4) FOURTH TAB - Select Parameters and Plot Map. ############################

  ## 4.1) Parameters Selection. ------------------------------------------------
  observe({

    allParams <- unique(unlist(lapply(csv_Files(), function(file) {
      names(file)[grep("csvId|CODE|NAME", names(file), invert = TRUE)]
    })))

    updatePickerInput(
      session = session,
      inputId = 'paramId1',
      choices = c("None selected", allParams),
      selected = "None selected"
    )

    updatePickerInput(
      session = session,
      inputId = 'paramId2',
      choices = c("None selected", allParams),
      selected = "None selected"
    )

    updatePickerInput(
      session = session,
      inputId = 'paramId3',
      choices = c("None selected", allParams),
      selected = "None selected"
    )

    updatePickerInput(
      session = session,
      inputId = 'paramId4',
      choices = c("None selected", allParams),
      selected = "None selected"
    )

  })

  ## 4.2) Merged Data. ---------------------------------------------------------
  mergedData <- reactive({
    shp <- filteredShp()

    # Check for valid data frames in csv_Files()
    if (any(sapply(csv_Files(), function(df) is.null(df) | nrow(df) == 0 | ncol(df) == 0)))
      stop("csv_Files contains null or empty data frames")

    # Function to merge two dataframes by all common columns, without duplicating them
    merge_common <- function(df1, df2){
      by_cols <- intersect(names(df1), names(df2))
      merge(df1, df2, by = by_cols, all = TRUE)
    }

    # Merge dataframes.
    common_cols <- Reduce(intersect, lapply(csv_Files(), names))
    # Use Reduce to merge all data frames in the list
    combined <- Reduce(merge_common, csv_Files())

    # Merge dataframe(s) with shapefile.
    data_cols <- intersect(names(shp), names(combined))
    combined <- dplyr::select(combined, -one_of(data_cols)) # replaced with dplyr::select()

    # Check if 'shpId' and 'csvId' are valid columns
    if (!'shpId' %in% names(shp) | !'csvId' %in% names(combined))
      stop("'shpId' or 'csvId' not found in the data frames")

    merged <- merge(shp, combined, by.x = 'shpId', by.y = 'csvId')

  })

  ## 4.3) Plot Params. ---------------------------------------------------------
  observe({
    if (input$tabs == "Map Visualization" && (input$toggleSidebar || !is.null(input$map))) {
      extraSidebarStyle <- "position: fixed; top: 40px; left: 305px; width: 500px; height: 440px; overflow: auto;
      border: 1px solid #CCCCCC; padding-left: 10px; background-color: white;"
    } else {
      extraSidebarStyle <- "display: none;"
    }

    output$extraSidebarStyle <- renderUI({
      tags$style(
        type = "text/css",
        HTML(paste0(".extra-sidebar {", extraSidebarStyle, "}"))
      )
    })

  })

  observe({
    req(mergedData())
    merged <- mergedData()

    if (!is.null(merged)) {
      output$extraSidebarVisible <- renderUI({
        tagList(
          plotOutput("densityPlot1"),
          plotOutput("densityPlot2"),
          plotOutput("densityPlot3"),
          plotOutput("densityPlot4")
        )
      })

      # A) Density Plot 1.
      output$densityPlot1 <- renderPlot({
        paramId1 <- input$paramId1

        if (!is.null(paramId1) && paramId1 != "None selected" && paramId1 %in% colnames(merged)) {
          param_data <- merged[[paramId1]]
          param_data <- as.numeric(param_data)

          if (!is.null(param_data) && length(param_data) > 1) {
            density_plot <- ggplot(data.frame(x = param_data), aes(x)) +
              geom_density(color = "darkblue", fill = "lightblue") +
              theme_classic() +
              labs(title = paramId1, x = "Value", y = "Density") +
              theme(plot.title = element_text(size = 10, hjust = 1, margin = margin(r = 0)),
                    axis.text = element_text(size = 10))

            if (isTruthy(circleDRAW())) {
              circle_shpId <- as.numeric(st_drop_geometry(circleDRAW())$shpId)
              if (!is.null(circle_shpId) && any(circle_shpId %in% merged$shpId)) {
                circle_data <- param_data[merged$shpId %in% circle_shpId]
                if (length(circle_data) > 0) {
                  circleAXIS <- as.numeric(param_data[merged$shpId %in% circle_shpId])
                  circleAXIS <- rep(circleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = circleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)
            sa1_nodo <- unique(circle_shpId)
            g <- mean(param_data)
            h <- mean(circle_data)
            i <- t.test(circle_data, param_data)
            j <- t.test(circle_data, mu = g)
            print(g)
            print(h)
            print(i)
            print(j)
#circle <- data.frame(circle_shpId, circle_data)
            #write.csv(circle, "results/result1.csv")
            write.csv(circle_shpId, "results/shpid.csv")
            write.csv(param_data, "results/baseline.csv")

            if (isTruthy(polygonDRAW())) {
              polygon_shpId <- as.numeric(st_drop_geometry(polygonDRAW())$shpId)
              if (!is.null(polygon_shpId) && any(polygon_shpId %in% merged$shpId)) {
                polygon_data <- param_data[merged$shpId %in% polygon_shpId]
                if (length(polygon_data) > 0) {
                  polygonAXIS <- as.numeric(param_data[merged$shpId %in% polygon_shpId])
                  polygonAXIS <- rep(polygonAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = polygonAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if (isTruthy(rectangleDRAW())) {
              rectangle_shpId <- as.numeric(st_drop_geometry(rectangleDRAW())$shpId)
              if (!is.null(rectangle_shpId) && any(rectangle_shpId %in% merged$shpId)) {
                rectangle_data <- param_data[merged$shpId %in% rectangle_shpId]
                if (length(rectangle_data) > 0) {
                  rectangleAXIS <- as.numeric(param_data[merged$shpId %in% rectangle_shpId])
                  rectangleAXIS <- rep(rectangleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = rectangleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if(isTruthy(markerDRAW())) {
              if(exists("shpId", where = markerDRAW())){
                markerAXIS <- as.numeric(st_drop_geometry(merged[which(merged$shpId == markerDRAW()$shpId),paramId1]))
                if (!is.na(markerAXIS) && length(markerAXIS) > 0) {
                  density_plot <- density_plot + geom_segment(
                    aes(x = x, y = 0, xend = x, yend = y),
                    data = data.frame(x = markerAXIS, y = density(param_data)$y[which.min(abs(density(param_data)$x - markerAXIS))]),
                    color = "red",
                    linetype = "dashed"
                  )
                }
              }
            }

            print(density_plot)

          }
        }
      })

      # B) Density Plot 2.
      output$densityPlot2 <- renderPlot({
        paramId2 <- input$paramId2

        if (!is.null(paramId2) && paramId2 != "None selected" && paramId2 %in% colnames(merged)) {
          param_data <- merged[[paramId2]]
          param_data <- as.numeric(param_data)

          if (!is.null(param_data) && length(param_data) > 1) {
            density_plot <- ggplot(data.frame(x = param_data), aes(x)) +
              geom_density(color = "darkblue", fill = "lightblue") +
              theme_classic() +
              labs(title = paramId2, x = "Value", y = "Density") +
              theme(plot.title = element_text(size = 10, hjust = 1, margin = margin(r = 0)),
                    axis.text = element_text(size = 10))

            if (isTruthy(circleDRAW())) {
              circle_shpId <- as.numeric(st_drop_geometry(circleDRAW())$shpId)
              if (!is.null(circle_shpId) && any(circle_shpId %in% merged$shpId)) {
                circle_data <- param_data[merged$shpId %in% circle_shpId]
                if (length(circle_data) > 0) {
                  circleAXIS <- as.numeric(param_data[merged$shpId %in% circle_shpId])
                  circleAXIS <- rep(circleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = circleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)
            write.csv(circle_data, "results/result2.csv")

            if (isTruthy(polygonDRAW())) {
              polygon_shpId <- as.numeric(st_drop_geometry(polygonDRAW())$shpId)
              if (!is.null(polygon_shpId) && any(polygon_shpId %in% merged$shpId)) {
                polygon_data <- param_data[merged$shpId %in% polygon_shpId]
                if (length(polygon_data) > 0) {
                  polygonAXIS <- as.numeric(param_data[merged$shpId %in% polygon_shpId])
                  polygonAXIS <- rep(polygonAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = polygonAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if (isTruthy(rectangleDRAW())) {
              rectangle_shpId <- as.numeric(st_drop_geometry(rectangleDRAW())$shpId)
              if (!is.null(rectangle_shpId) && any(rectangle_shpId %in% merged$shpId)) {
                rectangle_data <- param_data[merged$shpId %in% rectangle_shpId]
                if (length(rectangle_data) > 0) {
                  rectangleAXIS <- as.numeric(param_data[merged$shpId %in% rectangle_shpId])
                  rectangleAXIS <- rep(rectangleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = rectangleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if(isTruthy(markerDRAW())) {
              if(exists("shpId", where = markerDRAW())){
                markerAXIS <- as.numeric(st_drop_geometry(merged[which(merged$shpId == markerDRAW()$shpId),paramId2]))
                if (!is.na(markerAXIS) && length(markerAXIS) > 0) {
                  density_plot <- density_plot + geom_segment(
                    aes(x = x, y = 0, xend = x, yend = y),
                    data = data.frame(x = markerAXIS, y = density(param_data)$y[which.min(abs(density(param_data)$x - markerAXIS))]),
                    color = "red",
                    linetype = "dashed"
                  )
                }
              }
            }

            print(density_plot)

          }
        }
      })

      # C) Density Plot 3.
      output$densityPlot3 <- renderPlot({
        paramId3 <- input$paramId3

        if (!is.null(paramId3) && paramId3 != "None selected" && paramId3 %in% colnames(merged)) {
          param_data <- merged[[paramId3]]
          param_data <- as.numeric(param_data)

          if (!is.null(param_data) && length(param_data) > 1) {
            density_plot <- ggplot(data.frame(x = param_data), aes(x)) +
              geom_density(color = "darkblue", fill = "lightblue") +
              theme_classic() +
              labs(title = paramId3, x = "Value", y = "Density") +
              theme(plot.title = element_text(size = 10, hjust = 1, margin = margin(r = 0)),
                    axis.text = element_text(size = 10))

            if (isTruthy(circleDRAW())) {
              circle_shpId <- as.numeric(st_drop_geometry(circleDRAW())$shpId)
              if (!is.null(circle_shpId) && any(circle_shpId %in% merged$shpId)) {
                circle_data <- param_data[merged$shpId %in% circle_shpId]
                if (length(circle_data) > 0) {
                  circleAXIS <- as.numeric(param_data[merged$shpId %in% circle_shpId])
                  circleAXIS <- rep(circleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = circleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)
            write.csv(circle_data, "results/result3.csv")

            if (isTruthy(polygonDRAW())) {
              polygon_shpId <- as.numeric(st_drop_geometry(polygonDRAW())$shpId)
              if (!is.null(polygon_shpId) && any(polygon_shpId %in% merged$shpId)) {
                polygon_data <- param_data[merged$shpId %in% polygon_shpId]
                if (length(polygon_data) > 0) {
                  polygonAXIS <- as.numeric(param_data[merged$shpId %in% polygon_shpId])
                  polygonAXIS <- rep(polygonAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = polygonAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if (isTruthy(rectangleDRAW())) {
              rectangle_shpId <- as.numeric(st_drop_geometry(rectangleDRAW())$shpId)
              if (!is.null(rectangle_shpId) && any(rectangle_shpId %in% merged$shpId)) {
                rectangle_data <- param_data[merged$shpId %in% rectangle_shpId]
                if (length(rectangle_data) > 0) {
                  rectangleAXIS <- as.numeric(param_data[merged$shpId %in% rectangle_shpId])
                  rectangleAXIS <- rep(rectangleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = rectangleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if(isTruthy(markerDRAW())) {
              if(exists("shpId", where = markerDRAW())){
                markerAXIS <- as.numeric(st_drop_geometry(merged[which(merged$shpId == markerDRAW()$shpId),paramId3]))
                if (!is.na(markerAXIS) && length(markerAXIS) > 0) {
                  density_plot <- density_plot + geom_segment(
                    aes(x = x, y = 0, xend = x, yend = y),
                    data = data.frame(x = markerAXIS, y = density(param_data)$y[which.min(abs(density(param_data)$x - markerAXIS))]),
                    color = "red",
                    linetype = "dashed"
                  )
                }
              }
            }

            print(density_plot)

          }
        }
      })

      # D) Density Plot 4.
      output$densityPlot4 <- renderPlot({
        paramId4 <- input$paramId4

        if (!is.null(paramId4) && paramId4 != "None selected" && paramId4 %in% colnames(merged)) {
          param_data <- merged[[paramId4]]
          param_data <- as.numeric(param_data)

          if (!is.null(param_data) && length(param_data) > 1) {
            density_plot <- ggplot(data.frame(x = param_data), aes(x)) +
              geom_density(color = "darkblue", fill = "lightblue") +
              theme_classic() +
              labs(title = paramId4, x = "Value", y = "Density") +
              theme(plot.title = element_text(size = 10, hjust = 1, margin = margin(r = 0)),
                    axis.text = element_text(size = 10))

            if (isTruthy(circleDRAW())) {
              circle_shpId <- as.numeric(st_drop_geometry(circleDRAW())$shpId)
              if (!is.null(circle_shpId) && any(circle_shpId %in% merged$shpId)) {
                circle_data <- param_data[merged$shpId %in% circle_shpId]
                if (length(circle_data) > 0) {
                  circleAXIS <- as.numeric(param_data[merged$shpId %in% circle_shpId])
                  circleAXIS <- rep(circleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = circleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)
            write.csv(circle_data, "results/result4.csv")

            if (isTruthy(polygonDRAW())) {
              polygon_shpId <- as.numeric(st_drop_geometry(polygonDRAW())$shpId)
              if (!is.null(polygon_shpId) && any(polygon_shpId %in% merged$shpId)) {
                polygon_data <- param_data[merged$shpId %in% polygon_shpId]
                if (length(polygon_data) > 0) {
                  polygonAXIS <- as.numeric(param_data[merged$shpId %in% polygon_shpId])
                  polygonAXIS <- rep(polygonAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = polygonAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if (isTruthy(rectangleDRAW())) {
              rectangle_shpId <- as.numeric(st_drop_geometry(rectangleDRAW())$shpId)
              if (!is.null(rectangle_shpId) && any(rectangle_shpId %in% merged$shpId)) {
                rectangle_data <- param_data[merged$shpId %in% rectangle_shpId]
                if (length(rectangle_data) > 0) {
                  rectangleAXIS <- as.numeric(param_data[merged$shpId %in% rectangle_shpId])
                  rectangleAXIS <- rep(rectangleAXIS, length.out = length(density(param_data)$x))
                  density_plot <- density_plot + geom_point(
                    aes(x = x, y = 0),
                    data = data.frame(x = rectangleAXIS),
                    color = "darkgreen",
                    fill = "#33FFBD",
                    shape = 21,
                    size = 2,
                    alpha = 0.5
                  )
                }
              }
            }

            print(density_plot)

            if(isTruthy(markerDRAW())) {
              if(exists("shpId", where = markerDRAW())){
                markerAXIS <- as.numeric(st_drop_geometry(merged[which(merged$shpId == markerDRAW()$shpId),paramId4]))
                if (!is.na(markerAXIS) && length(markerAXIS) > 0) {
                  density_plot <- density_plot + geom_segment(
                    aes(x = x, y = 0, xend = x, yend = y),
                    data = data.frame(x = markerAXIS, y = density(param_data)$y[which.min(abs(density(param_data)$x - markerAXIS))]),
                    color = "red",
                    linetype = "dashed"
                  )
                }
              }
            }

            print(density_plot)

          }
        }
      })
    }
  })

  ## 4.4) Plot map. ------------------------------------------------------------
  map_rv <- reactiveValues(map = NULL)

  observeEvent(input$plot_map, {
    output$map <- renderLeaflet({
      req(mergedData(), csvId())
      merged <- mergedData()

      paramIdALL <- c(input$paramId1, input$paramId2, input$paramId3, input$paramId4)
      paramIdALL <- paramIdALL[!str_detect(paramIdALL, "None selected")]

      if (length(paramIdALL) == 0) {
        showModal(
          modalDialog(
            title = "Error: No Parameters Selected",
            HTML("<p>No parameters selected. Please select at least one parameter to visualize on the map.</p>"),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            ),
            size = "m",
            fade = TRUE
          )
        )
      } else if (length(unique(paramIdALL)) != length(paramIdALL)) {
        showModal(
          modalDialog(
            title = "Error: Duplicate Parameters Selected",
            HTML("<p>The same parameter has been selected multiple times. Please review your selections and select different parameters.</p>"),
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            ),
            size = "m",
            fade = TRUE
          )
        )

      } else {
        flipDirections <- rep(1, length(paramIdALL))
        for (i in 1:length(paramIdALL)) {
          flipId <- paste0("paramFlip", i)
          flipDirections[i] <- ifelse(!is.null(input[[flipId]]) && input[[flipId]] == TRUE, -1, 1)
        }

        merged[['combinedParamValues']] <- combineVars(merged %>% dplyr::select(all_of(paramIdALL)), flips = flipDirections)

        # A) Colour Palette.

        pal <- colorBin(
          rev(c('#b10026','#e31a1c','#fc4e2a','#fd8d3c','#feb24c','#fed976','#ffeda0','#ffffcc','#ffffff')),
          bins = 9,
          domain = range(0, 1)
        )

        # B) Zoom-in and out.

        selected_regions <- merged[merged$STE_NAME21 %in% input$iSTE,]

        if (nrow(selected_regions) == 0) {
          selected_regions <- merged
        }

        bounds <- st_bbox(selected_regions)

        # C) Leaflet Map.

        map <- leaflet() %>%
          addProviderTiles(providers$Stadia.StamenTonerLite) %>%
          addScaleBar(position = "bottomleft") %>%
          addDrawToolbar(
            targetGroup = "draw",
            position = "topright",
            polylineOptions = FALSE,
            circleMarkerOptions = FALSE,
            editOptions = editToolbarOptions(
              selectedPathOptions = selectedPathOptions()
            )
          )  %>%
          addStyleEditor(position = "topright",
                         openOnLeafletDraw = FALSE) # Modify colors of polygons on Leaflet map

        labels <- getLeafletLabels(merged %>% dplyr::select(all_of(paramIdALL), shpId, combinedParamValues))

        map <- addPolygons(
          map,
          data = merged,
          fillColor = ~pal(combinedParamValues), # the variable we want to use to fill
          fillOpacity =  0.7, # how bright are the fill colors
          color = "grey", # color of the boundaries
          weight = 0.8, # weight of the boundaries
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "black",
            bringToFront = FALSE)) %>%
          addLegend(
            position = "bottomright",
            title = "combinedParamValues",
            colors = c('#b10026','#e31a1c','#fc4e2a','#fd8d3c','#feb24c','#fed976','#ffeda0','#ffffcc','#ffffff'),
            labels = rev(c("0.00",  sprintf(seq(0.125,0.875, length.out = 7), fmt = "%#.2f"), "1.00")),
            opacity = 0.7,
            group = "legend"
          )
        map %>% fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
        map_rv$map <- map
        map
      }
    })
  })

  ## 4.5) Add Marker. ----------------------------------------------------------

  ### 4.5.1) Draw shapes on Map. -----------------------------------------------
  markerDRAW <- reactiveVal()
  circleDRAW <- reactiveVal()
  polygonDRAW <- reactiveVal()
  rectangleDRAW <- reactiveVal()

  observeEvent(input$map_draw_new_feature, {
    drawn_feature <- input$map_draw_new_feature

    if ("properties" %in% names(drawn_feature) && "feature_type" %in% names(drawn_feature$properties)) {
      feature_type <- drawn_feature$properties$feature_type

      if (feature_type == "marker") {
        # Handle marker feature
        lng <- drawn_feature$geometry$coordinates[[1]]
        lat <- drawn_feature$geometry$coordinates[[2]]
        coord <- st_sfc(st_point(c(lng, lat)), crs = 4326)
        intersected_regions <- st_intersection(shpId(), coord)
        markerDRAW(intersected_regions)

      } else if (feature_type == "circle") {
        # Handle circle feature
        lng <- drawn_feature$geometry$coordinates[[1]]
        lat <- drawn_feature$geometry$coordinates[[2]]
        radius <- drawn_feature$properties$radius
        circle_center <- st_sfc(st_point(c(lng, lat)), crs = 4326)
        circle_polygon <- st_buffer(circle_center, dist = radius)
        intersected_regions <- st_intersection(shpId(), circle_polygon)
        circleDRAW(intersected_regions)

      } else if (feature_type == "polygon") {
        # Handle polygon feature
        coords_list <- drawn_feature$geometry$coordinates[[1]]
        coords <- matrix(unlist(coords_list), ncol = 2, byrow = TRUE)
        polygon_geometry <- st_polygon(list(coords))
        polygon_sf <- st_sfc(polygon_geometry, crs = 4326)
        intersected_regions <- st_intersection(shpId(), polygon_sf)
        polygonDRAW(intersected_regions)

      } else if (feature_type == "rectangle") {
        # Handle rectangle feature
        coords <- drawn_feature$geometry$coordinates[[1]]
        min_lng <- coords[[1]][[1]]
        min_lat <- coords[[1]][[2]]
        max_lng <- coords[[3]][[1]]
        max_lat <- coords[[3]][[2]]
        rectangle_polygon <- st_polygon(list(
          matrix(c(
            min_lng, min_lat,
            max_lng, min_lat,
            max_lng, max_lat,
            min_lng, max_lat,
            min_lng, min_lat
          ), ncol = 2, byrow = TRUE)
        ))
        rectangle_sf <- st_sfc(rectangle_polygon, crs = 4326)
        intersected_regions <- st_intersection(shpId(), rectangle_sf)
        rectangleDRAW(intersected_regions)

      } else {
        showModal(modalDialog(
          title = "Error: Invalid feature type",
          HTML("<p>Please draw a valid feature type.</p>"),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          ),
          size = "m",
          fade = TRUE
        ))
      }

    } else {
      showModal(modalDialog(
        title = "Error: Missing properties or feature type.",
        HTML("<p>Please make sure to provide the necessary properties and specify the feature type.</p>"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        ),
        size = "m",
        fade = TRUE
      ))
    }
  })

  observeEvent(input$map_draw_edited_features, {
    edited_features <- input$map_draw_edited_features

    if (length(edited_features$features) > 0) {
      for (i in seq_along(edited_features$features)) {
        edited_feature <- edited_features$features[[i]]

        if ("properties" %in% names(edited_feature) && "feature_type" %in% names(edited_feature$properties)) {
          feature_type <- edited_feature$properties$feature_type

          if (feature_type == "marker") {
            # Handle marker feature
            lng <- edited_feature$geometry$coordinates[[1]]
            lat <- edited_feature$geometry$coordinates[[2]]
            coord <- st_sfc(st_point(c(lng, lat)), crs = 4326)
            intersected_regions <- st_intersection(shpId(), coord)
            markerDRAW(intersected_regions)

          } else if (feature_type == "circle") {
            # Handle circle feature
            lng <- edited_feature$geometry$coordinates[[1]]
            lat <- edited_feature$geometry$coordinates[[2]]
            radius <- edited_feature$properties$radius
            circle_center <- st_sfc(st_point(c(lng, lat)), crs = 4326)
            circle_polygon <- st_buffer(circle_center, dist = radius)
            intersected_regions <- st_intersection(shpId(), circle_polygon)
            circleDRAW(intersected_regions)

          } else if (feature_type == "polygon") {
            # Handle polygon feature
            coords_list <- edited_feature$geometry$coordinates[[1]]
            coords <- matrix(unlist(coords_list), ncol = 2, byrow = TRUE)
            polygon_geometry <- st_polygon(list(coords))
            polygon_sf <- st_sfc(polygon_geometry, crs = 4326)
            intersected_regions <- st_intersection(shpId(), polygon_sf)
            polygonDRAW(intersected_regions)

          } else if (feature_type == "rectangle") {
            # Handle rectangle feature
            coords <- edited_feature$geometry$coordinates[[1]]
            min_lng <- coords[[1]][[1]]
            min_lat <- coords[[1]][[2]]
            max_lng <- coords[[3]][[1]]
            max_lat <- coords[[3]][[2]]
            rectangle_polygon <- st_polygon(list(
              matrix(c(
                min_lng, min_lat,
                max_lng, min_lat,
                max_lng, max_lat,
                min_lng, max_lat,
                min_lng, min_lat
              ), ncol = 2, byrow = TRUE)
            ))
            rectangle_sf <- st_sfc(rectangle_polygon, crs = 4326)
            intersected_regions <- st_intersection(shpId(), rectangle_sf)
            rectangleDRAW(intersected_regions)

          }
        }
      }
    }
  })

  ### 4.5.2) Type-in Lat/Lon. --------------------------------------------------
  observeEvent(input$addMarker, {
    if(!is.null(input$lat) && !is.null(input$lon)){
      proxy <- leafletProxy("map")
      proxy %>% addCircleMarkers(
        lng = input$lon,
        lat = input$lat,
        radius = 6,
        color = "#FF5733",
        fillColor = "#33FFBD",
        fillOpacity = 0.8,
        group = "click_group"
      )
    }
  })

  ### 4.5.3) Upload File. ------------------------------------------------------
  observeEvent(input$LatLon_File, {
    req(input$LatLon_File)

    data <- read.csv(input$LatLon_File$datapath)

    if (all(c('lat', 'lon') %in% colnames(data))) {

      proxy <- leafletProxy("map")

      for (i in 1:nrow(data)) {
        proxy %>% addCircleMarkers(
          lng = data$lon[i],
          lat = data$lat[i],
          radius = 6,
          color = "#888888",
          fillColor = "#66CD00",
          fillOpacity = 0.8,
          group = "click_group",
          popup = paste0("Latitude: ", data$lat[i], "<br>Longitude: ", data$lon[i])
        )
      }
    } else {
      showModal(modalDialog(
        title = "Error: Missing Columns",
        HTML(paste0("<p>The CSV file you've uploaded is missing the required <strong>'lat'</strong> and <strong>'lon'</strong> columns.</p>",
                    "<p>Please review your file and try again. Ensure that the file contains columns named <strong>'lat'</strong> and <strong>'lon'</strong>.</p>")),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        ),
        size = "m",
        fade = TRUE
      ))
    }
  })

  ### 4.5.5) Remove Markers. ---------------------------------------------------
  observeEvent(input$removeAllMarkers, {
    leafletProxy("map") %>% clearGroup("click_group")
    leafletProxy("map") %>% clearGroup("draw_group")
    markerDRAW(NULL)
    circleDRAW(NULL)
    rectangleDRAW(NULL)
    polygonDRAW(NULL)
  })

  ### 4.5.6) Download map. -----------------------------------------------------
  output$downloadMap <- downloadHandler(
    filename = "my_map.png",
   content = function(file) {
      req(map_rv$map)
      saveWidget(map_rv$map, file = "my_map.html")
    }
  )

  ### 4.5.7) Reset All. --------------------------------------------------------

  observeEvent(input$refresh, {
    shinyjs::reset("transformOption")
    shinyjs::reset("LatLon_File")
    shinyjs::reset("transform1")
    shinyjs::reset("transform2")
    shinyjs::reset("transform3")
    shinyjs::reset("total_pop")
    shinyjs::reset("csv_File")
    shinyjs::reset("paramId1")
    shinyjs::reset("paramId2")
    shinyjs::reset("paramId3")
    shinyjs::reset("paramId4")
    shinyjs::reset("iCorr")
    shinyjs::reset("iSHP")
    shinyjs::reset("iSTE")
    shinyjs::reset("iSA4")
    shinyjs::reset("lat")
    shinyjs::reset("lon")

    output$map <- renderLeaflet({})
    output$densityPlot1 <- renderPlot({})
    output$densityPlot2 <- renderPlot({})
    output$densityPlot3 <- renderPlot({})
    output$densityPlot4 <- renderPlot({})
    output$data_summary <- renderUI({ NULL })
    output$paramSelect1 <- renderUI({ NULL })
    output$paramSelect2 <- renderUI({ NULL })
    output$paramSelect3 <- renderUI({ NULL })
    output$paramSelect4 <- renderUI({ NULL })

  })

  #  5) FIFTH TAB - User Guide. ################################################

  ## 5.1) Upload Data tab. -----------------------------------------------------
  output$uploadDataOptionTable <- renderTable({
    if (length(input$uploadDataOption) == 0) {
      NULL
    } else {
      options <- input$uploadDataOption
      descriptions <- lapply(options, function(option) {
        if (option %in% names(uploadDataDescriptions)) {
          uploadDataDescriptions[[option]]
        } else {
          paste("You selected in Upload Data Menu:", paste(option, collapse = ", "))
        }
      })
      data.frame(Option = options, Description = unlist(descriptions), stringsAsFactors = FALSE)
    }
  })

  ## 5.2) Data Manipulation tab. -----------------------------------------------
  output$dataManipulationOptionTable <- renderTable({
    if (length(input$dataManipulationOption) == 0) {
      NULL
    } else {
      options <- input$dataManipulationOption
      descriptions <- lapply(options, function(option) {
        if (option %in% names(dataManipulationDescriptions)) {
          dataManipulationDescriptions[[option]]
        } else {
          paste("You selected in Data Manipulation Menu:", paste(option, collapse = ", "))
        }
      })
      data.frame(Option = options, Description = unlist(descriptions), stringsAsFactors = FALSE)
    }
  })

  ## 5.3) Correlation tab. -----------------------------------------------------
  output$correlationOptionTable <- renderTable({
    if (length(input$correlationOption) == 0) {
      NULL
    } else {
      options <- input$correlationOption
      descriptions <- lapply(options, function(option) {
        if (option %in% names(correlationDescriptions)) {
          correlationDescriptions[[option]]
        } else {
          paste("You selected in Correlation Menu:", paste(option, collapse = ", "))
        }
      })
      data.frame(Option = options, Description = unlist(descriptions), stringsAsFactors = FALSE)
    }
  })

  ## 5.4) Map Visualization tab. -----------------------------------------------
  output$mapVisualizationOptionTable <- renderTable({
    if (length(input$mapVisualizationOption) == 0) {
      NULL
    } else {
      options <- input$mapVisualizationOption
      descriptions <- lapply(options, function(option) {
        if (option %in% names(mapVisualizationDescriptions)) {
          mapVisualizationDescriptions[[option]]
        } else {
          paste("You selected in Map Visualization Menu:", paste(option, collapse = ", "))
        }
      })
      data.frame(Option = options, Description = unlist(descriptions), stringsAsFactors = FALSE)
    }
  })

}

