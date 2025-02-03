# Load the required library
library(DT)
builtinData <- readRDS("data/builtinData.rds")

# SA1 - Tool Variables
variables_sa1_df <- data.frame(
  Variable = c("SA1_CODE"),
  Description = c("place"
  )
)

# SA2 - Tool Variables
variables_sa2_df <- data.frame(
  Variable = c("usual_resident_population", "irsd_score", "irsd_percentile", "irsd_quintile", "pm25_weighted_mean", "pm25_percentile",
               "pm25_quintile", "no2_weighted_mean", "no2_percentile", "no2_quintile", "age_0_5", "age_6_18",
               "age_19_64", "age_65_over", "total", "age_0_18", "age_0_5_percent", "age_0_18_percent", "age_65_over_percent", "age_0_5_percent_percentile", "age_0_18_percent_percentile",
               "age_65_over_percent_percentile", "age_0_5_percent_quintile", "age_0_18_percent_quintile", "age_65_over_percent_quintile",  "life_expectancy", "life_expectancy_percentile",
               "life_expectancy_quintile"),
    Description = c("usual_resident_population", "irsd_score", "irsd_percentile", "irsd_quintile", "pm25_weighted_mean", "pm25_percentile",
                    "pm25_quintile", "no2_weighted_mean", "no2_percentile", "no2_quintile", "age_0_5", "age_6_18",
                    "age_19_64", "age_65_over", "total", "age_0_18", "age_0_5_percent", "age_0_18_percent", "age_65_over_percent", "age_0_5_percent_percentile", "age_0_18_percent_percentile",
                    "age_65_over_percent_percentile", "age_0_5_percent_quintile", "age_0_18_percent_quintile", "age_65_over_percent_quintile",  "life_expectancy", "life_expectancy_percentile",
                    "life_expectancy_quintile"

  )
)

# SA3 - Tool Variables
variables_sa3_df <- data.frame(
  Variable = c("SA3_CODE"),
  Description = c("place"

  )
)

# SA4 - Tool Variables
variables_sa4_df <- data.frame(
  Variable = c("SA4_CODE"),
  Description = c("place"

  )
)
