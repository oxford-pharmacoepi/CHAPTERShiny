#### PACKAGES -----
options(encoding = "UTF-8")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
# library(ggthemes)
library(plotly)
library(here)
library(scales)
library(dplyr)
library(stringr)
library(tidyr)

#### UI -----
ui <-  fluidPage(theme = shinytheme("spacelab"),

                 # title ------
                 # shown across tabs
                 titlePanel("Incidence of acute, chronic and COVID-19 related conditions before and after the COVID-19 pandemic"),
                 # set up: pages along the side -----
                 navlistPanel(

                   ## Introduction  -----
                   tabPanel("Background",
                            tags$h3("Background"),
                            tags$hr(),
                            tags$h5(
                              "This app is a companion to the study focussing on determining the incidence of different condition's diagnosis,
                              care and prognosis in the United Kingdom, South Korea, the US, Spain, Belgium, Italy and Romania from 2018 to 2023."),
                            tags$h5(
                              "In the following pages you can find information on monthly, annual and overall incidence,
 and a description of the characteristics of the study population
of the patients for all relevant conditions. All results have been stratified by age group and sex."),

                            tags$h5("The results can be found published in the following journal:"
                            ),
                            tags$ol(
                              tags$li(
                                strong("TBD"),
                                "TBD",
                                " (",
                                tags$a(href = "url", "Paper Link"),
                                ")"
                              )),
                            tags$h5("The analysis code used to generate these results can be found",
                                    tags$a(href="https://github.com/dr-you-group/CHAPTER/tree/devKim", "here"),
                            ),
                            tags$h5("Any questions regarding these studies or problems with the app please contact",
                                    tags$a(href="mailto:kim.lopez@spc.ox.ac.uk", "Kim López-Güell")),
                            tags$hr()
                   ),

                   tabPanel("Databases",
                            tags$h3("Databases"),
                            tags$h5("Some characteristics of the databases used are shown below."),
                            tags$hr(),
                            tags$h5("Snapshots") ,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Databases",
                                                 DTOutput('tbl_snapshot') %>% withSpinner()
                                        )
                            )
                   ),
                   ## Incidence ------
                   tabPanel("Population Incidence",
                            tags$h3("Incidence Estimates"),
                            tags$h5("Incidence estimates are shown below...."),
                            tags$hr(),
                            tags$h5("Database and Study Outcome"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_database_name_selector",
                                            label = "Database",
                                            choices = unique(incidence_estimates$database_name),
                                            selected = unique(incidence_estimates$database_name),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "incidence_denominator_cohort_name_selector",
                            #                 label = "Denominator",
                            #                 choices = sort(unique(incidence_estimates$denominator_target_cohort_name)),
                            #                 selected = c("all_population"),
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                            # ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_outcome_cohort_name_selector",
                                            label = "Outcome",
                                            choices = sort(unique(incidence_estimates$outcome_cohort_name)),
                                            selected = c("Asthma"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Population Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_denominator_age_group_selector",
                                            label = "Age group",
                                            choices = unique(incidence_estimates$denominator_age_group),
                                            selected = c("0 to 150"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_denominator_sex_selector",
                                            label = "Sex",
                                            choices = unique(incidence_estimates$denominator_sex),
                                            selected = "Both",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                                # ),
                                # div(style="display: inline-block;vertical-align:top; width: 150px;",
                                #     pickerInput(inputId = "incidence_denominator_days_prior_history_selector",
                                #                 label = "Days Prior History",
                                #                 choices = unique(incidence_estimates$denominator_days_prior_history),
                                #                 selected = 365,
                                #                 options = list(
                                #                   `actions-box` = TRUE,
                                #                   size = 10,
                                #                   `selected-text-format` = "count > 3"),
                                #                 multiple = TRUE)

                            ),
                            tags$hr(),
                            tags$h5("Analysis Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_start_date_selector",
                                            label = "Incidence Start Date",
                                            choices = sort(as.character(unique(incidence_estimates$incidence_start_date))),
                                            selected = sort(as.character(unique(incidence_estimates$incidence_start_date))),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_denominator_analysis_interval_selector",
                                            label = "Analysis Interval",
                                            choices = unique(incidence_estimates$analysis_interval),
                                            selected = "years",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)
                            ),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "incidence_time_at_risk_selector",
                            #                 label = "Time at risk",
                            #                 choices = unique(incidence_estimates$time_at_risk),
                            #                 selected = unique(incidence_estimates$time_at_risk)[1],
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                            # ),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Table of Estimates",
                                                 DTOutput('tbl_incidence_estimates') %>% withSpinner()),
                                        tabPanel("Plot of Estimates",
                                                 tags$hr(),
                                                 tags$h5("Plotting Options"),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_x_axis",
                                                                 label = "X axis",
                                                                 choices = c("denominator_age_group",
                                                                             "denominator_sex",
                                                                             "outcome_cohort_name",
                                                                             "database_name",
                                                                             "incidence_start_date"),
                                                                 selected = "incidence_start_date",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_plot_facet",
                                                                 label = "Facet by",
                                                                 choices = c("denominator_age_group",
                                                                             "denominator_sex",
                                                                             #"denominator_days_prior_history",
                                                                             "outcome_cohort_name",
                                                                          #   "denominator_target_cohort_name",
                                                                             "database_name",
                                                                             "incidence_start_date"),
                                                                 selected = c("database_name"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_plot_group",
                                                                 label = "Colour by",
                                                                 choices = c("denominator_age_group",
                                                                             "denominator_sex",
                                                                             #"denominator_days_prior_history",
                                                                             "outcome_cohort_name",
                                                                        #     "denominator_target_cohort_name",
                                                                             "database_name",
                                                                             "incidence_start_date"),
                                                                 selected = c("outcome_cohort_name"),
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = TRUE)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_plot_scale",
                                                                 label = "Scales",
                                                                 choices = c("fixed",
                                                                             "free"),
                                                                 selected = "fixed",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE)
                                                 ),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_plot_ribbon",
                                                                 label = "Show CIs",
                                                                 choices = c("yes",
                                                                             "no"),
                                                                 selected = "yes",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE)
                                                 ),
                                                 plotlyOutput('plot_incidence_estimates', height = "800px") %>% withSpinner() ),
                                        tabPanel("Attrition table",
                                                 DTOutput('tbl_incidence_attrition') %>% withSpinner()),
                                        tabPanel("Attrition plot for COVID-19 infection",
                                                 grVizOutput('plot_infection_attrition', height = "800px") %>% withSpinner())
                            )
                   ) ,
                   ## Incidence comparison SI------
                   tabPanel("Population Incidence with Stringency Index",
                            tags$h3("Incidence Estimates"),
                            tags$h5("Incidence estimates are shown below. Data from COVID-19 Stringency Index for comparison has been extracted from https://ourworldindata.org/covid-stringency-index ."),
                            tags$hr(),
                            tags$h5("Database and Study Outcome"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_comp_database_name_selector",
                                            label = "Database",
                                            choices = unique(incidence_estimates$database_name),
                                            selected = unique(incidence_estimates$database_name),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_comp_outcome_cohort_name_selector",
                                            label = "Outcome",
                                            choices = sort(unique(incidence_estimates$outcome_cohort_name)),
                                            selected = c("Asthma"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Population Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_comp_denominator_age_group_selector",
                                            label = "Age group",
                                            choices = unique(incidence_estimates$denominator_age_group),
                                            selected = c("0 to 150"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_comp_denominator_sex_selector",
                                            label = "Sex",
                                            choices = unique(incidence_estimates$denominator_sex),
                                            selected = "Both",
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tags$hr(),
                            tags$h5("Analysis Settings"),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "incidence_comp_start_date_selector",
                                            label = "Incidence Start Date",
                                            choices = sort(as.character(unique(incidence_estimates$incidence_start_date))),
                                            selected = sort(as.character(unique(incidence_estimates$incidence_start_date))),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = TRUE)
                            ),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Plot of Estimates",
                                                 tags$hr(),
                                                 tags$h5("Plotting Options"),
                                                 div(style="display: inline-block;vertical-align:top; width: 150px;",
                                                     pickerInput(inputId = "incidence_comp_sma",
                                                                 label = "Smooth incidence",
                                                                 choices = c("yes",
                                                                             "no"),
                                                                 selected = "no",
                                                                 options = list(
                                                                   `actions-box` = TRUE,
                                                                   size = 10,
                                                                   `selected-text-format` = "count > 3"),
                                                                 multiple = FALSE)
                                                 ),
                                                 plotlyOutput('plot_incidence_comparison', height = "700px") %>% withSpinner() )
                            )
                   ) ,
                   ## Population characteristics ------
                   tabPanel("Population Characteristics",
                            tags$h3("Study Population Characteristics"),
                            tags$h5("The population characteristics are shown below. For all conditions unless otherwise specified this was obtained looking at any time in history before diagnosis."),
                            tags$hr(),
                            # tags$h5("Study outcome") ,
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "table1_outcome_cohort_name_selector",
                            #                 label = "Outcome",
                            #                 choices = sort(unique(characterisation_index$Outcome)),
                            #                 selected = c("All population"),
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                            # ),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "table1_sex_selector",
                            #                 label = "Sex",
                            #                 choices = sort(unique(table_one_results$Sex)),
                            #                 selected = c("Both"),
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                            # ),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "table1_age_selector",
                            #                 label = "Age group",
                            #                 choices = sort(unique(table_one_results$Age_group)),
                            #                 selected = c("0:150"),
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = TRUE)
                            # ),

                            tabsetPanel(type = "tabs",
                                        tabPanel("Study Population Characteristics",
                                                 DTOutput('tbl_table_one') %>% withSpinner()
                                        )
                            )
                   ),
                   ## Time Series Analysis ------
                   tabPanel("Time Series Analysis: SR",
                            tags$h3("Time Series Analysis: Segmented Regression"),
                            tags$h5("Segmented regression analysis is shown below. The timepoints correspond to the first lockdown for each country:
                                    The UK - 23/03/2020
                                    Spain - 14/03/2020
                                    The US - From 22/03/2020 to 12/04/2020
                                    Belgium -14/03/2020
                                    Italy - 09/03/2020
                                    Romania - 24/03/2020
                                    France - 17/03/2020.

                                    Should we add COVID waves? Or a simple March 2020 date for all?

                                    As the modeling is done in quarters (of the year), the difference between the majority of the dates are unimportant.
                                    We therefore only include first quarter, represented by 01-03-2020, and second quarter, 01-04-2020"),
                            tags$hr(),
                            tags$h5("Study outcome") ,
                            #div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "tsa_denominator_cohort_name_selector",
                            #                 label = "Outcome",
                            #                 choices = sort(unique(incidence_estimates$denominator_target_cohort_name)),
                            #                 selected = c("all_population"),
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = FALSE)
                            # ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "tsa_outcome_cohort_name_selector",
                                            label = "Outcome",
                                            choices = sort(unique(incidence_estimates$outcome_cohort_name)),
                                            selected = c("Asthma"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "tsa_database_name_selector",
                                            label = "Database",
                                            choices = sort(unique(incidence_estimates$database_name)),
                                            selected = c("CPRDGOLD"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "tsa_age_selector",
                                            label = "Age group",
                                            choices = sort(unique(incidence_estimates$denominator_age_group)),
                                            selected = c("0 to 150"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "tsa_sex_selector",
                                            label = "Sex",
                                            choices = sort(unique(incidence_estimates$denominator_sex)),
                                            selected = c("Both"),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 150px;",
                                pickerInput(inputId = "tsa_timepoint_selector",
                                            label = "Timepoint",
                                            choices = sort(unique(tsa$Timepoint)),
                                            selected = c(as.Date("2020-03-09")),
                                            options = list(
                                              `actions-box` = TRUE,
                                              size = 10,
                                              `selected-text-format` = "count > 3"),
                                            multiple = FALSE)
                            ),
                            # div(style="display: inline-block;vertical-align:top; width: 150px;",
                            #     pickerInput(inputId = "tsa_time_at_risk_selector",
                            #                 label = "Time at risk",
                            #                 choices = sort(unique(incidence_estimates$time_at_risk)),
                            #                 selected = unique(incidence_estimates$time_at_risk)[1],
                            #                 options = list(
                            #                   `actions-box` = TRUE,
                            #                   size = 10,
                            #                   `selected-text-format` = "count > 3"),
                            #                 multiple = FALSE)
                            # ),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Segmented Regression",
                                                 DTOutput('tbl_segmented_regression') %>% withSpinner()),
                                        tabPanel("Segmented Regression diagnostics",
                                                 DTOutput('tbl_segmented_regression_diagnostics') %>% withSpinner()),
                                        tabPanel("Plot of Estimates",
                                                 plotlyOutput('plot_segmented_regression', height = "800px") %>% withSpinner()),
                                        tabPanel("Plot of Residuals",
                                                 plotOutput('plot_segmented_regression_residuals', height = "600px") %>% withSpinner()),
                                        tabPanel("Segmented Regression monthly",
                                                 DTOutput('tbl_segmented_regression_monthly') %>% withSpinner())

                            )
                   )

                   # close -----
                 ))
