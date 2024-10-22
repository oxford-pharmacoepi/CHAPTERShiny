# load packages -----
library(shiny)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(PatientProfiles)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(scales)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(pbs)
library(stats)
library(DiagrammeR)
library(forcats)

formatEstimateName <- function(result,
                               estimateNameFormat = NULL,
                               keepNotFormatted = TRUE,
                               useFormatOrder = TRUE) {
   # format estimate
  if (!is.null(estimateNameFormat)) {
    resultFormatted <- formatEstimateNameInternal(
      result = result, format = estimateNameFormat,
      keepNotFormatted = keepNotFormatted, useFormatOrder = useFormatOrder
    )
  } else {
    resultFormatted <- result
  }

  return(resultFormatted)
}

formatEstimateNameInternal <- function(result, format, keepNotFormatted, useFormatOrder) {
  # if no format no action is performed
  if (length(format) == 0) {
    return(result)
  }
  # correct names
  if (is.null(names(format))) {
    nms <- rep("", length(format))
  } else {
    nms <- names(format)
  }
  nms[nms == ""] <- gsub("<|>", "", format[nms == ""])
  # format
  ocols <- colnames(result)
  cols <- ocols[
    !ocols %in% c("estimate_name", "estimate_type", "estimate_value")
  ]

  # start formatting
  result <- result |>
    dplyr::mutate("formatted" = FALSE, "id" = dplyr::row_number()) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
    dplyr::mutate(group_id = min(.data$id)) |>
    dplyr::ungroup()

  resultF <- NULL
  for (k in seq_along(format)) {
    nameK <- nms[k]
    formatK <- format[k] |> unname()
    keys <- result[["estimate_name"]] |> unique()
    keysK <- regmatches(formatK, gregexpr("(?<=\\<).+?(?=\\>)", formatK, perl = T))[[1]]
    format_boolean <- all(keysK %in% keys)
    len <- length(keysK)
    if (len > 0 & format_boolean) {
      formatKNum <- getFormatNum(formatK, keysK)
      res <- result |>
        dplyr::filter(!.data$formatted) |>
        dplyr::filter(.data$estimate_name %in% .env$keysK) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(cols))) |>
        dplyr::filter(dplyr::n() == .env$len) |>
        dplyr::mutate("id" = min(.data$id))
      resF <- res |>
        dplyr::ungroup() |>
        dplyr::select(-"estimate_type") |>
        tidyr::pivot_wider(
          names_from = "estimate_name", values_from = "estimate_value"
        ) |>
        evalName(formatKNum, keysK) |>
        dplyr::mutate(
          "estimate_name" = nameK,
          "formatted" = TRUE,
          "estimate_type" = "character"
        ) |>
        dplyr::select(dplyr::all_of(c(ocols, "id", "group_id", "formatted")))
      result <- result |>
        dplyr::anti_join(
          res |> dplyr::select(dplyr::all_of(c(cols, "estimate_name"))),
          by = c(cols, "estimate_name")
        ) |>
        dplyr::union_all(resF)
    } else {
      if (len > 0) {warning(paste0(formatK, " has not been formatted."), call. = FALSE)
      } else {warning(paste0(formatK, " does not contain an estimate name indicated by <...>"), call. = FALSE)}
    }
  }
  #useFormatOrder
  if (useFormatOrder) {
    new_order <- dplyr::tibble(estimate_name = nms, format_id = 1:length(nms)) |>
      dplyr::union_all(result |>
                         dplyr::select("estimate_name") |>
                         dplyr::distinct() |>
                         dplyr::filter(!.data$estimate_name %in% nms) |>
                         dplyr::mutate(format_id = length(format) + dplyr::row_number()))
    result <- result |>
      dplyr::left_join(new_order,
                       by = "estimate_name")
    result <- result[order(result$group_id, result$format_id, decreasing = FALSE),] |>
      dplyr::select(-c("id", "group_id", "format_id"))
  } else {
    result <- result |>
      dplyr::arrange(.data$id) |>
      dplyr::select(-"id", -"group_id")
  }
  # keepNotFormated
  if (!keepNotFormatted) {
    result <- result |> dplyr::filter(.data$formatted)
  }
  # result
  result <- result |> dplyr::select(-"formatted")
  return(result)
}
getFormatNum <- function(format, keys) {
  ik <- 1
  for (k in seq_along(keys)) {
    format <- gsub(
      pattern = keys[k], replacement = paste0("#", ik, "#"), x = format
    )
    ik <- ik + 1
  }
  return(format)
}
evalName <- function(result, format, keys) {
  for (k in seq_along(keys)) {
    format <- gsub(
      pattern = paste0("<#", k, "#>"),
      replacement = paste0("#x#.data[[\"", keys[k], "\"]]#x#"),
      x = format
    )
  }
  format <- strsplit(x = format, split = "#x#") |> unlist()
  format <- format[format != ""]
  id <- !startsWith(format, ".data")
  format[id] <- paste0("\"", format[id], "\"")
  format <- paste0(format, collapse = ", ")
  format <- paste0("paste0(", format, ")")
  result <- result |>
    dplyr::mutate(
      "estimate_value" =
        dplyr::if_else(
          dplyr::if_any(dplyr::all_of(keys), ~ is.na(.x)),
          NA_character_,
          eval(parse(text = format))
        )
    )
  return(result)
}

splitAll <- function(result,
                     keep = FALSE,
                     fill = "overall",
                     overall = lifecycle::deprecated()) {
  if (lifecycle::is_present(overall)) {
    lifecycle::deprecate_warn("0.1.0", "splitAll(overall)")
  }
  result |>
    splitGroup(keep = keep, fill = fill) |>
    splitStrata(keep = keep, fill = fill) |>
    splitAdditional(keep = keep, fill = fill)
}

splitAdditional <- function(result,
                            keep = FALSE,
                            fill = "overall",
                            overall = lifecycle::deprecated()) {
  if (lifecycle::is_present(overall)) {
    lifecycle::deprecate_warn("0.1.0", "splitAdditional(overall)")
  }
  splitNameLevel(
    result = result,
    name = "additional_name",
    level = "additional_level",
    keep = keep,
    fill = fill
  )
}

splitStrata <- function(result,
                        keep = FALSE,
                        fill = "overall",
                        overall = lifecycle::deprecated()) {
  if (lifecycle::is_present(overall)) {
    lifecycle::deprecate_warn("0.1.0", "splitStrata(overall)")
  }
  splitNameLevel(
    result = result,
    name = "strata_name",
    level = "strata_level",
    keep = keep,
    fill = fill
  )
}

splitGroup <- function(result,
                       keep = FALSE,
                       fill = "overall",
                       overall = lifecycle::deprecated()) {
  if (lifecycle::is_present(overall)) {
    lifecycle::deprecate_warn("0.1.0", "splitGroup(overall)")
  }
  splitNameLevel(
    result = result,
    name = "group_name",
    level = "group_level",
    keep = keep,
    fill = fill
  )
}

splitNameLevel <- function(result,
                           name = "group_name",
                           level = "group_level",
                           keep = FALSE,
                           fill = "overall",
                           overall = lifecycle::deprecated()) {
  if (lifecycle::is_present(overall)) {
    lifecycle::deprecate_warn("0.1.0", "splitNameLevel(overall)")
  }

  newCols <- getColumns(result = result, col = name)
  id <- which(name == colnames(result))

  nameValues <- result[[name]] |> strsplit(" and | &&& ")
  levelValues <- result[[level]] |> strsplit(" and | &&& ")
  if (!all(lengths(nameValues) == lengths(levelValues))) {
    cli::cli_abort("Column names and levels number does not match")
  }

  present <- newCols[newCols %in% colnames(result)]
  if (length(present) > 0) {
    cli::cli_warn(
      "The following columns will be overwritten:
      {paste0(present, collapse = ', ')}."
    )
  }
  for (k in seq_along(newCols)) {
    col <- newCols[k]
    dat <- lapply(seq_along(nameValues), function(y) {
      res <- levelValues[[y]][nameValues[[y]] == col]
      if (length(res) == 0) {
        return(as.character(NA))
      } else {
        return(res)
      }
    }) |>
      unlist()
    result[[col]] <- dat
  }

  if (!keep) {
    result <- result |> dplyr::select(-dplyr::all_of(c(name, level)))
    colskeep <- character()
  } else {
    colskeep <- c(name, level)
  }

  # move cols
  if (id == 1) {
    result <- result |> dplyr::relocate(dplyr::any_of(newCols))
  } else {
    id <- colnames(result)[id - 1]
    result <- result |>
      dplyr::relocate(
        dplyr::any_of(c(colskeep, newCols)), .after = dplyr::all_of(id)
      )
  }

  # use fill
  if (!is.na(fill)) {
    result <- result |>
      dplyr::mutate(dplyr::across(
        dplyr::any_of(newCols),
        ~ dplyr::if_else(is.na(.x), .env$fill, .x)
      ))
  }

  return(result)
}

getColumns <- function(result, col) {

  # extract columns
  x <- result |>
    dplyr::pull(dplyr::all_of(col)) |>
    unique() |>
    lapply(strsplit, split = " and | &&& ") |>
    unlist() |>
    unique()

  # eliminate overall
  x <- x[x != "overall"]

  return(x)
}

# printing numbers with 1 decimal place and commas
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(floor(x),
                big.mark=",", nsmall = 0, scientific=FALSE))}

# read results from data folder ----
results<-list.files(here("data"), full.names = TRUE, recursive = TRUE)

# snapshots
snapshots <- results[stringr::str_detect(results, "snapshot")]
table_snap <- list()
for(i in seq_along(snapshots)){
  table_snap[[i]]<-readr::read_csv(snapshots[[i]],
                                  show_col_types = FALSE) %>%
    dplyr::mutate(cdm_version = as.character(cdm_version))
}
table_snap <- dplyr::bind_rows(table_snap)

# incidence ------
incidence_files <- results[stringr::str_detect(results, "Incidence")]
incidence_files <- incidence_files[stringr::str_detect(incidence_files, ".csv")]

# Print attrition in incidence code too!
incidence_files_at <- incidence_files[stringr::str_detect(incidence_files, "attrition")]
incidence_files_est <- incidence_files[stringr::str_detect(incidence_files, "attrition", negate = T)]

incidence <- list()
incidence_at <- list()

for(i in seq_along(incidence_files_at)){
  incidence_at[[i]]<-readr::read_csv(incidence_files_at[[i]],
                                     show_col_types = FALSE)
}
for(i in seq_along(incidence_files_est)){
  incidence[[i]]<-readr::read_csv(incidence_files_est[[i]],
                                  show_col_types = FALSE) %>%
    dplyr::mutate(
      time_at_risk = dplyr::if_else(
        grepl("tar30",incidence_files_est[[i]]),
        "30",
        dplyr::if_else(
          grepl("tar365",incidence_files_est[[i]]),
                "365", "Inf")
        )
      )
}

incidence_estimates <- dplyr::bind_rows(incidence) %>%
  dplyr::rename(
    "incidence" = "incidence_100000_pys",
    "incidence_95CI_lower" = "incidence_100000_pys_95CI_lower",
    "incidence_95CI_upper" = "incidence_100000_pys_95CI_upper"
  )

incidence_attrition <- dplyr::bind_rows(incidence_at)

# tidy incidence data frame
incidence_estimates <- incidence_estimates %>%
  dplyr::mutate(cdm_name = dplyr::if_else(
    cdm_name == "The National Health Insurance Service?National Sample Cohort",
    "YUHS",
    cdm_name
  )) %>%
  mutate( year_index = lubridate::year( incidence_start_date),
          month_index = lubridate::month( incidence_start_date),
          year_month = paste(year_index, sprintf("%02d", month_index), sep = "-")) %>%
  select(-"...1") %>%
  rename("database_name" = "cdm_name") %>%
  dplyr::filter(incidence_start_date < as.Date("2023-06-01")) %>%
  dplyr::mutate(denominator_target_cohort_name =
                  dplyr::if_else(is.na(denominator_target_cohort_name), "all_population",
                                 denominator_target_cohort_name))


# tableOne ------
characterisation_files<-results[stringr::str_detect(results, "tableOne")]

charac <- list()
for(i in seq_along(characterisation_files)){
  charac[[i]]<-readr::read_csv(characterisation_files[[i]],
                                               show_col_types = FALSE) %>%
    dplyr::select(-"...1") %>%
    dplyr::mutate(estimate_value = as.character(estimate_value),
                  result_id = 1)
}

denominator_cohort_info <- incidence_estimates %>%
  select(denominator_cohort_name, denominator_sex, denominator_age_group) %>%
  distinct()

characterisation_index <- dplyr::bind_rows(charac)  %>%
  dplyr::mutate(cdm_name = dplyr::if_else(
    cdm_name == "The National Health Insurance Service?National Sample Cohort",
    "YUHS",
    cdm_name
  )) %>%
  filter(estimate_type %in% c("integer", "numeric")) %>%
  filter(estimate_name %in% c("count", "percentage", "mean", "min", "max")) %>%
  mutate(estimate_value = dplyr::if_else(
    !is.na(estimate_value),
    as.character(round(as.numeric(estimate_value), 2)),
    estimate_value)) %>%
  formatEstimateName(
    #estimateNameFormat = c("<count> (<percentage>%)", "<mean> (<min> to <max>)"),
    estimateNameFormat = c("<count>", "<mean> (<min> to <max>)"),
    keepNotFormatted = FALSE) %>%
  splitAll() %>%
  mutate(variable_name = dplyr::if_else(
    !is.na(variable_level),
    paste0(variable_name, ": ", variable_level),
    variable_name
  )) %>%
  select(-"variable_level", -"estimate_type", -"estimate_name") %>%
  rename("Database" = "cdm_name",
         "Outcome" = "cohort_name",
         "Variable" = "variable_name") %>%
  tidyr::pivot_wider(names_from = "Database",
                     values_from = "estimate_value") %>%
  left_join(denominator_cohort_info, by = c("Outcome" = "denominator_cohort_name")) %>%
  rename("Sex" = "denominator_sex",
         "Age group" = "denominator_age_group") %>%
  mutate(Outcome = dplyr::if_else(
         grepl("denominator", Outcome),
         "All population",
         Outcome)
  ) %>%
  select(-"result_id") %>%
  arrange("Variable") %>%
  dplyr::filter(!(grepl("Sex", Variable) & grepl("population",Outcome)))

# Remove dates for which THIN data has bad quality (= all year 2023?) ASK
incidence_estimates <- incidence_estimates %>%
  dplyr::filter(!(grepl("THIN", database_name) & incidence_start_date > "2023-01-01"))

# make it pretty, colours and logos and names of cohorts!

# Add more DATES (K)?
# tsa <- dplyr::tibble(
#   Database = c("CPRDGOLD", "THINBE", "THINES", "THINFR",  "THINIT", "THINRO", "PharMetrics", "PharMetrics"),
#   Timepoint = c(as.Date("2020-03-23"), as.Date("2020-03-14"), as.Date("2020-03-14"), as.Date("2020-03-17"),
#                 as.Date("2020-03-09"),  as.Date("2020-03-24"),  as.Date("2020-03-22"), as.Date("2020-04-12"))
# )

tsa <- dplyr::tibble(
     Database = c("AllFirstQuarter", "AllSecondQuarter"),
     Timepoint = c(as.Date("2020-03-01"), as.Date("2020-04-01"))
   )

# As in TSA modeling is done in quarters (of the year), the difference between the majority of the dates there are unimportant.
# We only need to include March (first quarter) and April (second quarter). We could add others.

# Tidy cohort names
incidence_estimates <- incidence_estimates %>%
  mutate( outcome_cohort_name = forcats::fct_recode( outcome_cohort_name,
                                            "COVID-19 infection" = "covid_infection",
                                            "Influenza" = "influenza",
                                            "Asthma" = "asthma",
                                            "Breast cancer" = "breastcancer",
                                            "Colorectal cancer" = "colorectalcancer",
                                            "Dementia" = "dementia",
                                            "Hypertension" = "hypertension",
                                            "Hypothyroidism" = "hypothyroidism",
                                            "Iflammatory bowel disease" = "ibd",
                                            "Post-acute COVID-19 condition code" = "longcovid_code",
                                            "Osteoporosis" = "osteoporosis",
                                            "Rheumatoid arthritis" = "rheumatoidarthritis",
                                            "Atrial fibrillation" = "atrial_fibrillation",
                                            "Systemic lupus" = "systemiclupus",
                                            "ER and hospital admissions" = "er_and_admissions",
                                            "Outpatient visits" = "outpatient_visits",
                                            "Post-acute COVID-19 one symptom" = "long_covid_one_symptom",
                                            "Post-acute COVID-19 two symptoms" = "long_covid_two_symptoms",
                                            "Post-acute COVID-19 three symptoms" = "long_covid_three_symptoms",
                                            "Post-acute COVID-19 one complication" = "pasc_one_event",
                                            "Post-acute COVID-19 two complications" = "pasc_two_events",
                                            "Post-acute COVID-19 three complications" = "pasc_three_events",
                                            "Deep vein thrombosis" = "deep_vein_thrombosis",
                                            "Heart failure" = "heart_failure",
                                            "Ischemic stroke" = "ischemic_stroke",
                                            "Myocardial infraction" = "myocardial_infarction",
                                            "Myocarditis and pericarditis" = "myocarditis_pericarditis", # is it or?
                                            "Pulmonary embolism" = "pulmonary_embolism",
                                            "Transient ischemic attack" = "transient_ischemic_attack",
                                            "Venous thromboembolism" = "venous_thrombosembolism",
                                            "Ventricular arrythmia and cardiac arrest" = "ventricular_arrhythmia_cardiac_arrest", # and or or?
                                            "Haemorrhagic stroke" = "haemorrhagic_stroke"))

incidence_attrition <- incidence_attrition %>%
  mutate( outcome_cohort_name = forcats::fct_recode( outcome_cohort_name,
                                                     "COVID-19 infection" = "covid_infection",
                                                     "Influenza" = "influenza",
                                                     "Asthma" = "asthma",
                                                     "Breast cancer" = "breastcancer",
                                                     "Colorectal cancer" = "colorectalcancer",
                                                     "Dementia" = "dementia",
                                                     "Hypertension" = "hypertension",
                                                     "Hypothyroidism" = "hypothyroidism",
                                                     "Iflammatory bowel disease" = "ibd",
                                                     "Post-acute COVID-19 condition code" = "longcovid_code",
                                                     "Osteoporosis" = "osteoporosis",
                                                     "Rheumatoid arthritis" = "rheumatoidarthritis",
                                                     "Atrial fibrillation" = "atrial_fibrillation",
                                                     "Systemic lupus" = "systemiclupus",
                                                     "ER and hospital admissions" = "er_and_admissions",
                                                     "Outpatient visits" = "outpatient_visits",
                                                     "Post-acute COVID-19 one symptom" = "long_covid_one_symptom",
                                                     "Post-acute COVID-19 two symptoms" = "long_covid_two_symptoms",
                                                     "Post-acute COVID-19 three symptoms" = "long_covid_three_symptoms",
                                                     "Post-acute COVID-19 one complication" = "pasc_one_event",
                                                     "Post-acute COVID-19 two complications" = "pasc_two_events",
                                                     "Post-acute COVID-19 three complications" = "pasc_three_events",
                                                     "Deep vein thrombosis" = "deep_vein_thrombosis",
                                                     "Heart failure" = "heart_failure",
                                                     "Ischemic stroke" = "ischemic_stroke",
                                                     "Myocardial infraction" = "myocardial_infarction",
                                                     "Myocarditis and pericarditis" = "myocarditis_pericarditis", # is it or?
                                                     "Pulmonary embolism" = "pulmonary_embolism",
                                                     "Transient ischemic attack" = "transient_ischemic_attack",
                                                     "Venous thromboembolism" = "venous_thrombosembolism",
                                                     "Ventricular arrythmia and cardiac arrest" = "ventricular_arrhythmia_cardiac_arrest", # and or or?
                                                     "Haemorrhagic stroke" = "haemorrhagic_stroke"))

characterisation_index <- characterisation_index %>%
  mutate( Outcome = forcats::fct_recode( Outcome,
                                                     "COVID-19 infection" = "covid_infection",
                                                     "Influenza" = "influenza",
                                                     "Asthma" = "asthma",
                                                     "Breast cancer" = "breastcancer",
                                                     "Colorectal cancer" = "colorectalcancer",
                                                     "Dementia" = "dementia",
                                                     "Hypertension" = "hypertension",
                                                     "Hypothyroidism" = "hypothyroidism",
                                                     "Iflammatory bowel disease" = "ibd",
                                                     "Post-acute COVID-19 condition code" = "longcovid_code",
                                                     "Osteoporosis" = "osteoporosis",
                                                     "Rheumatoid arthritis" = "rheumatoidarthritis",
                                                     "Atrial fibrillation" = "atrial_fibrillation",
                                                     "Systemic lupus" = "systemiclupus",
                                                     "ER and hospital admissions" = "er_and_admissions",
                                                     "Outpatient visits" = "outpatient_visits",
                                                     "Post-acute COVID-19 one symptom" = "long_covid_one_symptom",
                                                     "Post-acute COVID-19 two symptoms" = "long_covid_two_symptoms",
                                                     "Post-acute COVID-19 three symptoms" = "long_covid_three_symptoms",
                                                     "Post-acute COVID-19 one complication" = "pasc_one_event",
                                                     "Post-acute COVID-19 two complications" = "pasc_two_events",
                                                     "Post-acute COVID-19 three complications" = "pasc_three_events",
                                                     "Deep vein thrombosis" = "deep_vein_thrombosis",
                                                     "Heart failure" = "heart_failure",
                                                     "Ischemic stroke" = "ischemic_stroke",
                                                     "Myocardial infraction" = "myocardial_infarction",
                                                     "Myocarditis and pericarditis" = "myocarditis_pericarditis", # is it or?
                                                     "Pulmonary embolism" = "pulmonary_embolism",
                                                     "Transient ischemic attack" = "transient_ischemic_attack",
                                                     "Venous thromboembolism" = "venous_thrombosembolism",
                                                     "Ventricular arrythmia and cardiac arrest" = "ventricular_arrhythmia_cardiac_arrest", # and or or?
                                                     "Haemorrhagic stroke" = "haemorrhagic_stroke"))

# Get cohort attrition for infected individuals

att_inf<-results[stringr::str_detect(results, "attrition_infection")]

att_infection <- list()
for(i in seq_along(att_inf)){
  att_infection[[i]]<-readr::read_csv(att_inf[[i]],
                               show_col_types = FALSE) %>%
    dplyr::select(-"...1")
}

att_infection <- dplyr::bind_rows(att_infection)


# plot Attrition function
plotAttHere <- function (result) {
  rlang::check_installed("DiagrammeR")
  colsAttr <- omopgenerics::cohortColumns("cohort_attrition")
  if (inherits(result, "data.frame") && all(colsAttr %in% colnames(result))) {
    result <- summariseAttrition(dplyr::select(result, dplyr::all_of(colsAttr)))
  }
  nCohorts <- length(unique(result$group_level))
  if (nCohorts > 1) {
    return(emptyTable(paste0(nCohorts, " present in the reuslt object, please subset to just one of them")))
  }
  x <- dplyr::mutate(dplyr::arrange(dplyr::mutate(dplyr::select(visOmopResults::pivotEstimates(visOmopResults::splitAll(result %>%
                                                                                                                          dplyr::mutate(additional_level = as.character(additional_level))),
                                                                                               pivotEstimatesBy = c("variable_name", "estimate_name"),
                                                                                               nameStyle = "{variable_name}"), "reason_id", "reason",
                                                                "number_records", "number_subjects", "excluded_records",
                                                                "excluded_subjects"), reason_id = as.numeric(.data$reason_id)),
                                    .data$reason_id), dplyr::across(dplyr::everything(),
                                                                    as.character))
  xn <- CohortCharacteristics:::createLabels(x)
  y <- CohortCharacteristics:::selectLabels(xn)
  xn <- y$xn
  att <- y$att
  n <- nrow(x)
  xg <- DiagrammeR::create_graph()
  w1 <- CohortCharacteristics:::getWidthMainBox(xn)
  if (nrow(x) == 1) {
    xg <- getSingleNode(xg, xn, w1)
  } else {
    att <- CohortCharacteristics:::validateReason(att)
    h2 <- CohortCharacteristics:::getHeightMiddleBox(att)
    p1 <- CohortCharacteristics:::getPositionMainBox(xn, n, h2)
    w2 <- CohortCharacteristics:::getWidthMiddleBox(att)
    p2 <- CohortCharacteristics:::getPositionMiddleBox(p1)
    xg <- CohortCharacteristics:::getNodes(xn, att, n, xg, h2, w1, p1, w2, p2)
  }
  return(DiagrammeR::render_graph(xg))
}
