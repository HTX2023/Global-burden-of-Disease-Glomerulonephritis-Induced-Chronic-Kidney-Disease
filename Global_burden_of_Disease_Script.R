# Set working directory
setwd('X:/New Folder (3)/0327GBD/0327GBD')

# Load required libraries
library(GlobalBurdenR)
library_required_packages()

# Load and preprocess country data
data_country <- merge_csv_files_vroom_progress('X:/New Folder (3)/0327GBD/0327GBD/204 Countries')
data_22region <- merge_csv_files_vroom_progress('X:/New Folder (3)/0327GBD/0327GBD/GBD Regions/')
data_super_region <- merge_csv_files_vroom_progress('X:/New Folder (3)/0327GBD/0327GBD/GBD Super Regions+Global/')

# Standardize column names
data_country <- gbd_rename_column(data_country)
data_22region <- gbd_rename_column(data_22region)
data_super_region <- gbd_rename_column(data_super_region)

# Combine all data sources
data_all <- rbind(data_country, data_22region, data_super_region)

# Load age-specific data
data_all_age <- merge_csv_files_vroom_progress('X:/New Folder (3)/0327GBD/0327GBD/Datas')
data_all_age <- debug_gbd_data_check(data_all_age)

# Filter data for three-line table focusing on Deaths
data <- gbd_filter(data_all,
                   measure == "Deaths",
                   rei == "All risk factors",
                   age %in% c('Age-standardized', 'All ages'))

# Export filtered data
write.csv(data, 'Glomerulonephritis-Induced Chronic Kidney Disease.csv')

# Define locations and order for analysis
location <- unique(data$location)
order <- c("Overall", "Female", "Male", "High SDI",
           "High-middle SDI", "Middle SDI",
           "Low-middle SDI", "Low SDI",
           "South-East Asia Region", "Tropical Latin America",
           "Eastern Sub-Saharan Africa", "China", 'Egypt')

# Generate GBD table
t2 <- GBD_table(
  input_path = "Glomerulonephritis-Induced Chronic Kidney Disease.csv",
  measure = "Deaths",
  locations = location,
  regions_order = order,
  year1 = 1990,
  year2 = 2021,
  output_path = 'GN-CKD Table.docx'
)
t2

# Create world map visualization for 2021 ASIR
dta <- gbd_filter(data_country,
                  measure == "Deaths",
                  rei == "All risk factors",
                  age == 'Age-standardized',
                  year == 2021,
                  sex == "Both",
                  metric == 'Rate')

map <- plot_world_map(dta,
                      only_large_map = FALSE,
                      color_palette = "Spectral",
                      legend_name = "ASIR",
                      plot_title = "Global ASIR of GN-Induced CKD in 2021")
map

# Calculate EAPC for GN-Induced CKD
dta <- gbd_filter(data_country,
                  measure == "Deaths",
                  rei == "All risk factors",
                  age == 'Age-standardized',
                  sex == "Both",
                  metric == 'Rate')

# Remove duplicate rows
duplicated_rows <- duplicated(dta)
dta <- dta[!duplicated_rows, ]

EAPC <- compute_gbd_eapc(dta)
EAPC$val <- EAPC$EAPC

# Plot EAPC world map
map3 <- plot_world_map(EAPC,
                       only_large_map = FALSE,
                       color_palette = "Spectral",
                       legend_name = "EAPC",
                       plot_title = "Global EAPC of GN-Induced CKD (1990-2021)")
map3

# Create binned color world map
dta <- gbd_filter(data_country,
                  measure == "Deaths",
                  rei == "All risk factors",
                  age == 'Age-standardized',
                  sex == "Both",
                  metric == 'Rate',
                  year == 2021)

colors <- c('#3182BD', '#6BAED6', '#9ECAE1', '#FEE5D9', '#FCAE91', '#FB6A4A', '#DE2D26', '#A50F15')
plot <- plot_world_map_D(
  data = dta,
  only_large_map = FALSE,
  breaks = c(0, 1, 2, 3, 4, 5, 6, 7),
  labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6+"),
  color_palette = colors,
  legend_name = "ASR (/10^5)",
  plot_title = "Global ASR of GN-Induced CKD with Binned Colors",
  small_titles = c("Caribbean and Central America",
                   "Persian Gulf",
                   "Balkan Peninsula",
                   "China",
                   "West Africa",
                   "Eastern Mediterranean",
                   "Northern Europe"),
  small_map_coordinates = list(
    c(-92, -60, 5, 27),
    c(45, 55, 19, 31),
    c(12, 32, 35, 53),
    c(73.0, 130.0, 3.5, 53.0),
    c(-17, -7, 7, 20),
    c(32, 37, 29, 35),
    c(5, 25, 48, 60)
  )
)
plot

# Regional map visualization
plot_world_region_map(
  dta,
  region = 'east_and_south_europe',
  legend_title = 'ASMR',
  subtitles = 'East and South Europe ASIR',
  main_title = 'East and South Europe ASIR of GN-Induced CKD'
)

# Hierarchical cluster visualization
res <- compute_gbd_eapc(dta)
visual_hierarchical_cluster_plot(res, k = 4,
                                 colors = c('#3399FF', 'lightblue', 'red', 'orange'))

# Correlation visualization between EAPC and ASIR
data2 <- gbd_filter(data_all,
                    measure == "Deaths",
                    rei == "All risk factors",
                    year == 2021,
                    age == 'Age-standardized',
                    sex == "Both",
                    metric == 'Rate')

data2 <- data2[order(data2$location), ]
res <- res[order(res$location), ]

df <- data.frame(data2$val, res$EAPC)
g <- visual_correlation_EACP(df, x_col = "data2.val", y_col = "res.EAPC")
g + xlim(c(0, 8)) + ylim(c(-2, 3))

# EAPC bar visualization
visual_eapc_bar(
  res,
  eapc_col = "EAPC",
  location_col = "location",
  x_label = "EAPC in ASIR",
  y_label = "Location",
  title = "Estimated Annual Percentage Change by Region",
  positive_color = "pink",
  negative_color = "lightblue",
  sort_ascending = TRUE
)

# Joinpoint analysis for selected countries
dt_jp <- gbd_filter(data_all,
                    measure == "Deaths",
                    rei == "All risk factors",
                    age == 'Age-standardized',
                    sex == "Both",
                    metric == 'Rate',
                    location %in% c('China', 'Viet Nam', 'Uruguay', 'Japan'))

data2 <- dt_jp
data2$SE <- (data2$upper - data2$lower) / (1.96 * 2)
data2 <- data2[order(data2$location, data2$year), ]

jp <- GlobalBurdenR::gbd_joinpoint_analysis_NIH(
  data2,
  x_var = 'year',
  y_var = 'val',
  se_var = 'SE',
  by_var = 'location',
  model = "ln",
  max_joinpoints = 5,
  n_cores = 3,
  aapc_full_range = TRUE,
  aapc_last_obs = TRUE
)

plot_joinpoint_apc_region_compare(
  jp = jp,
  title = "Mortality Trends of GN-Induced CKD (1990-2021)",
  ylab = "Age-standardized Incidence rate",
  ylim = c(0, 9),
  xlim = c(1990, 2021),
  x_breaks = 5,
  legend_pos = "right",
  show_joinpoint_labels = FALSE,
  custom_colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
)

# Sex comparison joinpoint analysis
dt_jp <- gbd_filter(data_all,
                    measure == "Deaths",
                    rei == "All risk factors",
                    age == 'Age-standardized',
                    metric == 'Rate',
                    location %in% c('China'))

data2 <- dt_jp
data2$SE <- (data2$upper - data2$lower) / (1.96 * 2)
data2 <- data2[order(data2$location, data2$year), ]

jp <- gbd_joinpoint_analysis_NIH(
  data2,
  x_var = 'year',
  y_var = 'val',
  se_var = 'SE',
  by_var = 'sex',
  model = "ln",
  max_joinpoints = 5,
  n_cores = 3,
  aapc_full_range = TRUE,
  aapc_last_obs = TRUE
)

plot_joinpoint_apc_sex_compare(
  jp = jp,
  title = "Mortality Trends of GN-Induced CKD (1990-2021)",
  ylab = "Age-standardized Incidence rate",
  ylim = c(0, 8),
  xlim = c(1990, 2021),
  x_breaks = 5,
  legend_pos = "right",
  show_joinpoint_labels = TRUE,
  custom_colors = c("#1f77b4", "#ff7f0e", "#d62728")
)

# APC analysis
data <- data_all[,-1]
duplicated_rows <- duplicated(data)
data <- data[!duplicated_rows, ]

data <- gbd_filter(data,
                   measure == 'Deaths',
                   !age %in% c('All ages', "Age-standardized"),
                   rei == 'All risk factors',
                   metric == "Number",
                   sex == 'Both',
                   location == 'Global')

data$age <- gsub(" years", "", data$age)
data$age <- gsub("-", " to ", data$age)

dt <- age_period_cohort_model(data, apcie_pop2021,
                              measure = "Deaths",
                              location_filter = "Global",
                              sex_filter = "Both",
                              start_year = 1990,
                              start_age = 20,
                              interval = 5)

# Define age order
age_levels <- c("0-4 years", "5-9 years", "10-14 years", "15-19 years",
                "20-24 years", "25-29 years", "30-34 years", "35-39 years",
                "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                "60-64 years", "65-69 years", "70-74 years", "75-79 years",
                "80-84 years", "85-89 years", "90-94 years")

# Format age groups
dt$age_group <- factor(dt$age_group, levels = age_levels, ordered = TRUE)

# Plot APC analysis results
res <- plot_apcie(dt, palette_name = "Hiroshige")

# Frontier analysis
dta <- gbd_filter(data_country,
                  measure == "Deaths",
                  rei == "All risk factors",
                  age == 'Age-standardized',
                  sex == "Both",
                  metric == 'Rate')

dta <- dta[,-1]
dta <- dta[,-6]

data('SDI2021')

results <- frontier_analysis(dta, SDI,
                             year1 = 1990,
                             year2 = 2021,
                             age = 'Age-standardized',
                             metric = 'Rate',
                             sex = 'Both',
                             measure = 'Deaths')

# Extract results
data <- results[[1]]            # Complete data for all years
data_1990 <- results[[2]]        # Data for 1990
data_2019 <- results[[3]]        # Data for 2019
data_2019$trend <- ifelse(data_2019$ASR > data_1990$ASR, "Increase", "Decrease")

# Create frontier plots
res <- plot_frontier(data, data_2019)
res$plotA
res$plotB

# Health inequality analysis
library(tidyverse)
library(data.table)
library(car)
library(MASS)
library(mgcv)
library(splines)

# Load and prepare burden data
burden <- read.csv('Glomerulonephritis-Induced Chronic Kidney Disease.csv')
mydata <- prepare_inequality_data(burden, SDI2021, GBD_popall_2021)

res <- concentration_analysis(mydata, years = c(1990, 2019),
                              label1_pos = list(x = 1.15, y = 20000),
                              label2_pos = list(x = 1.15, y = 27000),
                              key_countries = c("China"))

res$sii_plot
res$ci

# ARIMA forecasting
dta <- gbd_filter(data_all,
                  measure == "Deaths",
                  rei == "All risk factors",
                  location == 'Global',
                  age == 'Age-standardized',
                  sex == "Both",
                  metric == 'Rate')

result_2030 <- GBD_arima_predict(dta,
                                 historical_title = "Historical GN-Induced CKD Mortality",
                                 forecast_title = "Forecasted GN-Induced CKD Mortality",
                                 x_label = "Year",
                                 y_label = "ASMR",
                                 color_palette = "VanGogh2",
                                 forecast_end_year = 2050)

result_2030$forecast_data
result_2030$combined_plot

# Multi-age group forecasting
dta2 <- gbd_filter(data_all,
                   measure == "Deaths",
                   rei == "All risk factors",
                   location == 'Global',
                   sex == "Both",
                   metric == 'Rate')

result_multi <- GBD_arima_predict_multi(dta2,
                                        historical_title = "Historical Trends",
                                        forecast_title = "GN-Induced CKD Death Rate Trends and Forecasts by Age Group",
                                        x_label = "Year",
                                        y_label = "Death Rate",
                                        forecast_end_year = 2050,
                                        max_year = 2021)

result_multi$plots

# BAPC analysis
EC <- gbd_filter(data_all,
                 measure == "Deaths",
                 rei == "All risk factors")

EC <- EC[,-1]
EC <- EC[,-6]

duplicated_rows <- duplicated(EC)
EC <- EC[!duplicated_rows, ]

# Add missing age groups
EC2 <- gbd_filter(EC, age %in% c("20-24 years"))
EC2$age <- '<5 years'
EC2$val <- 0
EC2$upper <- 0
EC2$lower <- 0

EC3 <- gbd_filter(EC, age %in% c("20-24 years"))
EC3$age <- '5-9 years'
EC3$val <- 0
EC3$upper <- 0
EC3$lower <- 0

EC4 <- gbd_filter(EC, age %in% c("20-24 years"))
EC4$age <- '10-14 years'
EC4$val <- 0
EC4$upper <- 0
EC4$lower <- 0

EC5 <- gbd_filter(EC, age %in% c("20-24 years"))
EC5$age <- '15-19 years'
EC5$val <- 0
EC5$upper <- 0
EC5$lower <- 0

EC <- rbind(EC, EC2, EC3, EC4, EC5)

# Process data for BAPC analysis
EC_Male_incidence_n <- process_EC_data_BAPC(
  EC,
  sex_filter = "Male",
  metric_filter = "Number",
  measure_filter = "Deaths",
  location_filter = "Global"
)

EC_Female_incidence_n <- process_EC_data_BAPC(
  EC,
  sex_filter = "Female",
  metric_filter = "Number",
  measure_filter = "Deaths",
  location_filter = "Global"
)

EC_Both_incidence_n <- process_EC_data_BAPC(
  EC,
  sex_filter = "Both",
  metric_filter = "Number",
  measure_filter = "Deaths",
  location_filter = "Global"
)

# Process GBD data for BAPC
GBD_Global_Male_n <- process_GBD_data_BAPC(filter_years = 2020:2050,
                                           filter_location = "Global",
                                           filter_sex = 'Male')

GBD_Global_Female_n <- process_GBD_data_BAPC(filter_years = 2020:2050,
                                             filter_location = "Global",
                                             filter_sex = 'Female')

GBD_Global_Both_n <- GBD_Global_Female_n + GBD_Global_Male_n

# Prepare projection matrix
EC_pro <- matrix(data = NA, nrow = 2050 - 2021, ncol = ncol(GBD_Global_Male_n)) %>% as.data.frame()
rownames(EC_pro) <- seq(2022, 2050, 1)
colnames(EC_pro) <- names(EC_Male_incidence_n)

EC_Male_incidence_n <- rbind(EC_Male_incidence_n, EC_pro)
EC_Female_incidence_n <- rbind(EC_Female_incidence_n, EC_pro)
EC_Both_incidence_n <- rbind(EC_Both_incidence_n, EC_pro)

# Perform BAPC analysis
data(GBDstandBAPC)
results <- GBD_bapc_analysis(incidence_data = EC_Male_incidence_n,
                             global_data = GBD_Global_Male_n,
                             wstand = wstand,
                             plot_type = 'asp',
                             start_year = 1989,
                             end_year = 2050,
                             npredict = 29,
                             retro = TRUE,
                             secondDiff = FALSE,
                             verbose = TRUE)

res <- results$bapc_result

# Dual-axis age group plot
data <- gbd_filter(data_all,
                   measure == 'Deaths',
                   age %in% c("50-54 years", "55-59 years",
                              "60-64 years", "65-69 years", "70-74 years",
                              "75-79 years", "80-84 years", "85-89 years",
                              "90-94 years", "95+ years"),
                   rei == 'All risk factors',
                   sex == 'Both',
                   location == 'Global')

data <- data %>%
  mutate(year = as.numeric(as.character(year))) %>%
  mutate(age = gsub(" years", "", age)) %>%
  mutate(age = factor(age, levels = c("50-54", "55-59", "60-64", "65-69",
                                      "70-74", "75-79", "80-84", "85-89",
                                      "90-94", "95+")))

p1 <- visual_dual_axis_ageGroup(
  data = data,
  x_lab = "Year",
  y_lab_primary = "Deaths",
  y_lab_secondary = "Death rate per 100,000",
  met_palette = "Hiroshige",
  primary_limits = c(0, 180000),
  secondary_limits = c(0, 55)
)

p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dual-axis gender plot
data_in <- gbd_filter(data_all,
                      year %in% c(1990, 2019),
                      metric %in% c('Rate', 'Number'),
                      location == 'China',
                      measure == 'Deaths',
                      sex == 'Both',
                      rei == 'All risk factors',
                      age != "All ages" & age != "Age-standardized")

visual_dual_axis_GBD(data_in, colors = c('#7E57C2', '#FFC107'))

# Gender comparison plot
data_in1 <- gbd_filter(data_all,
                       year == 2019,
                       metric %in% c('Rate', 'Number'),
                       location == 'China',
                       measure == 'Deaths',
                       sex != 'Both',
                       rei == 'All risk factors',
                       age != "All ages" & age != "Age-standardized")

visual_dual_axis_GBD_sex(
  data = data_in1,
  colors = c('#7E57C2', '#FFC107', "pink", "lightblue"),
  x_lab = "Age Group",
  y_lab_primary = "Incidence Number",
  y_lab_secondary = "Crude Incidence rate per 100,000",
  legend_title_number = "Number by Sex",
  legend_title_rate = "Rate by Sex",
  secondary_limits = c(0, 60),
  rotate_x_text = TRUE,
  x_text_angle = 45
)

# Sex pyramid plot
data <- gbd_filter(data_all,
                   measure == 'Deaths',
                   !age %in% c('Age-standardized', 'All ages'),
                   sex != 'Both',
                   metric == 'Rate',
                   year == 2021,
                   location == 'Global',
                   rei == 'All risk factors')

visual_sex_data(
  data,
  x_col = "age",
  y_col = "val",
  pyramid = TRUE,
  title = "Death Rates by Age and Sex for GN-Induced CKD"
)

# Chronological risk factor comparison
data2 <- gbd_filter(data_all,
                    sex == 'Both',
                    year %in% c(1990, 2021),
                    measure == 'Deaths',
                    age == 'Age-standardized',
                    metric == 'Rate',
                    location == 'Global')

visual_chrono_risk(data2, start_year = 1990, end_year = 2021,
                   left_title = 'risk2021')

# Age trend visualization
data <- gbd_filter(data_all_age, metric == 'Rate', year == 2021)

data$location <- factor(data$location,
                        levels = c("Global", "High SDI", "High-middle SDI",
                                   "Middle SDI", "Low-middle SDI", "Low SDI"))
data$measure <- factor(data$measure,
                       levels = c("Incidence", "Deaths", "DALYs"))

age_groups <- c("<5 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years",
                "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years",
                "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years",
                "75-79 years", "80-84 years", "85-89 years", "90-94 years", "95+ years")

data$age <- factor(data$age, levels = age_groups, ordered = TRUE)

visual_age_trend(data,
                 location_select = c("Global", "High SDI", "High-middle SDI",
                                     "Middle SDI", "Low-middle SDI", "Low SDI"),
                 measure_select = c("Incidence", "Deaths", "DALYs"),
                 sex_select = c("Both"))

visual_age_trend(data,
                 location_select = c("Global", "High SDI"),
                 measure_select = c("Incidence", "Deaths", "DALYs"),
                 sex_select = c("Female", "Male"))

# Sex ratio visualization
data <- gbd_filter(data_all_age, year == 2021, metric == 'Rate')
data <- gbd_select_age_group(data, keep_original_format = TRUE)

visual_sex_ratio(
  df = data,
  measure_var = "Incidence",
  location_levels = c("Global", "High SDI", "High-middle SDI",
                      "Middle SDI", "Low-middle SDI", "Low SDI")
)

# SDI correlation analysis for 204 countries
IBD_ASMR1 <- gbd_filter(data_all,
                        age == 'Age-standardized',
                        metric == 'Rate',
                        measure == 'Deaths',
                        rei == 'All risk factors',
                        sex == "Both")

visual_204SDI_correlation(IBD_ASMR1, "ASMR", SDI_2021,
                          annotate_x = 0.2, annotate_y = 3)

# Two-year comparison dumbbell plot
data <- gbd_filter(data_all,
                   measure == "Deaths",
                   rei == 'All risk factors',
                   !age %in% c('Age-standardized', 'All ages'),
                   year %in% c(1990, 2021),
                   sex == 'Both',
                   location %in% c("Global",
                                   "High SDI",
                                   "High-middle SDI",
                                   "Middle SDI",
                                   "Low-middle SDI",
                                   "Low SDI"),
                   metric == 'Rate')

data$age <- factor(data$age)
data$year <- factor(data$year)
data$location <- factor(data$location,
                        levels = c("Global",
                                   "High SDI",
                                   "High-middle SDI",
                                   "Middle SDI",
                                   "Low-middle SDI",
                                   "Low SDI"))
data <- data[,-1]

p <- visual_2year_dumbbell(data,
                           measure = "Deaths",
                           sex = "Both")

# Gender risk comparison
df <- data_all %>%
  filter(sex != "Both", metric == "Number",
         !age %in% c("All ages", "Age-standardized"),
         measure == 'Deaths', location == 'Global',
         year == 2021) %>%
  mutate(val = val / 100000)

p1 <- visual_gender_risk(df)
p1 + xlim(c(-0.26, 0.25))

# Risk factor heatmap
data <- gbd_filter(data_super_region,
                   measure == "Deaths",
                   age %in% c('Age-standardized'),
                   year %in% c(2021),
                   sex == 'Both',
                   metric == 'Rate')

diseases <- data$rei
countries <- data$location

data_htmap <- data.frame(Disease = diseases, Country = countries)
data_htmap$val <- data$val

breaks <- quantile(data_htmap$val, probs = seq(0, 1, length.out = 11))
data_htmap$Rank <- as.numeric(cut(data_htmap$val, breaks = breaks, labels = 1:10, include.lowest = TRUE))

plot <- visual_rank_heatmap(
  data = data_htmap,
  x_var = "Disease",
  y_var = "Country",
  rank_var = "Rank",
  title = "Disease Rankings by Country",
  subtitle = "Both sexes, all ages, 2021, percent of total DALYs",
  rank_groups = list(c(1, 2, 3), c(4), c(5), c(6, 7), c(8, 9), c(10))
)

grid.newpage()
grid.draw(plot)

# Risk factor bar and facet plots
Risk_factor_2021 <- gbd_filter(data_all,
                               measure == 'Deaths',
                               sex == "Both",
                               year == 2021)

location_col <- c("Global", "High-income Asia Pacific", "High-income North America",
                  "Western Europe", "Australasia", "Andean Latin America",
                  "Tropical Latin America", "Central Latin America", "Southern Latin America",
                  "Central Europe", "Eastern Europe", "Central Asia",
                  "North Africa and Middle East", "South Asia", "Southeast Asia",
                  "East Asia", "Oceania", "Western Sub-Saharan Africa",
                  "Eastern Sub-Saharan Africa", "Central Sub-Saharan Africa",
                  "Southern Sub-Saharan Africa")

visual_risk_factor_bars(Risk_factor_2021,
                        selected_year = "2021",
                        selected_sex = "Both",
                        selected_age = "All ages",
                        selected_measure = "Deaths",
                        selected_metric = "Percent",
                        selected_rei = c("Metabolic risks",
                                         "All risk factors",
                                         "Behavioral risks",
                                         "Smoking",
                                         "Tobacco",
                                         "High body-mass index",
                                         "High fasting plasma glucose"),
                        palette_name = 'Hiroshige',
                        selected_location = location_col)

# Faceted risk factor bar plot
Risk_factor_2021 <- gbd_filter(data_all,
                               measure == 'Deaths',
                               sex == "Both",
                               year == 2021,
                               location %in% c(
                                 "Global", "High-income Asia Pacific", "High-income North America",
                                 "Western Europe", "Australasia", "Andean Latin America",
                                 "Tropical Latin America", "Central Latin America", "Southern Latin America",
                                 "Caribbean", "Central Europe", "Eastern Europe",
                                 "Central Asia", "North Africa and Middle East", "South Asia",
                                 "Southeast Asia", "East Asia", "Oceania",
                                 "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                 "Central Sub-Saharan Africa", "Southern Sub-Saharan Africa"
                               ))

visual_risk_factor_bars_facet(Risk_factor_2021,
                              selected_year = "2021",
                              selected_sex = "Both",
                              selected_age = "All ages",
                              selected_measure = "Deaths",
                              selected_metric = "Percent",
                              selected_rei = c("Metabolic risks",
                                               "All risk factors",
                                               "Behavioral risks",
                                               "Smoking",
                                               "Tobacco",
                                               "High body-mass index",
                                               "High fasting plasma glucose"),
                              palette_name = "Hiroshige")

# SDI-based GBD rate heatmap
data <- gbd_filter(data_all,
                   measure == "Deaths",
                   rei == 'Smoking',
                   !age %in% c('Age-standardized', 'All ages'),
                   sex == 'Both',
                   location %in% c("Global",
                                   "High SDI",
                                   "High-middle SDI",
                                   "Middle SDI",
                                   "Low-middle SDI",
                                   "Low SDI"),
                   metric == 'Rate')

result <- visual_SDI_GBDrateheatmap(data, ncol = 4,
                                    color_palette = "Hiroshige",
                                    legend_rows = 2)
print(result$plot)

# Risk factor percentage comparison
data <- gbd_filter(data_all,
                   measure == "Deaths",
                   rei %in% c("Smoking", "Metabolic risks",
                              "Tobacco", "High body-mass index",
                              'High fasting plasma glucose'),
                   age == 'Age-standardized',
                   year %in% c(1990, 2021),
                   sex == 'Both',
                   location %in% c("Global",
                                   "High SDI",
                                   "High-middle SDI",
                                   "Middle SDI",
                                   "Low-middle SDI",
                                   "Low SDI",
                                   "High-income Asia Pacific", "High-income North America",
                                   "Western Europe", "Australasia", "Andean Latin America",
                                   "Tropical Latin America", "Central Latin America",
                                   "Southern Latin America", "Central Europe",
                                   "Eastern Europe", "Central Asia",
                                   "North Africa and Middle East",
                                   "South Asia", "Southeast Asia", "East Asia", "Oceania",
                                   "Western Sub-Saharan Africa", "Eastern Sub-Saharan Africa",
                                   "Central Sub-Saharan Africa",
                                   "Southern Sub-Saharan Africa"),
                   metric == 'Rate')

data$risk <- data$rei

raw_data <- data
processed_data <- raw_data %>%
  group_by(location, year) %>%
  mutate(percentage = val / sum(val) * 100) %>%
  ungroup()

custom_colors <- c(
  "Smoking" = "#1a2f5f",
  "Metabolic risks" = "#4a6899",
  "Tobacco" = "white",
  "High body-mass index" = "#a6c0e0",
  "High fasting plasma glucose" = "#8B0000"
)

custom_text_colors <- c(
  "Smoking" = "white",
  "Metabolic risks" = "white",
  "Tobacco" = "black",
  "High body-mass index" = "black",
  "High fasting plasma glucose" = "white"
)

p2 <- visual_2yearCompare_GBDPercent(
  data = processed_data,
  risk_colors = custom_colors,
  text_colors = custom_text_colors,
  risk_order = c("Smoking", "Metabolic risks",
                 "Tobacco", "High body-mass index",
                 'High fasting plasma glucose'),
  bar_width = 0.8,
  text_size = 3
)
p2

# SDI-based ASR comparison curves
data <- gbd_filter(data_all,
                   rei %in% c("Smoking"),
                   age == 'Age-standardized',
                   location %in% c("Global",
                                   "High SDI",
                                   "High-middle SDI",
                                   "Middle SDI",
                                   "Low-middle SDI",
                                   "Low SDI"),
                   metric == 'Rate')

visual_SDI_GBDrate(
  data,
  selected_locations = c("Global", "High SDI", "High-middle SDI", "Middle SDI",
                         "Low-middle SDI", "Low SDI"),
  selected_measures = c("DALYs (Disability-Adjusted Life Years)", "Deaths")
)

# Gender pyramid plot
data <- gbd_filter(data_all,
                   rei %in% c("Smoking"),
                   !age %in% c('Age-standardized', 'All ages', '65-74 years'),
                   year %in% c(1990, 2021),
                   sex != 'Both',
                   location %in% c("Global"),
                   metric == 'Rate')

data$age <- gsub(" years", "", data$age)
data$age <- gsub("-", " to ", data$age)

visual_GBDrate_gender_pyramid(data,
                              descending = FALSE)