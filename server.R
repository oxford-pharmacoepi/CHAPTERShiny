#### SERVER ------
server <-	function(input, output, session) {
  # snapshots
  get_snapshot <- reactive({
    table <- table_snap  %>%
      dplyr::mutate(cdm_name = dplyr::if_else(
        cdm_name == "The National Health Insurance Service?National Sample Cohort",
        "YUHS",
        cdm_name
      )) %>%
      select(-c("...1", "cdm_source_name", "cdm_description"))
    table
  })
  output$tbl_snapshot <- DT::renderDataTable({
    table <- get_snapshot()
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "incidence_estimates"))
              ))
  })

  # incidence estimates and attrition
  get_incidence_estimates<-reactive({

    table<-incidence_estimates %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                cohort_obscured,
                analysis_outcome_washout,
                result_obscured,
                outcome_cohort_id,
                analysis_repeated_events,
                denominator_cohort_id,
                analysis_min_cell_count,
                denominator_cohort_name,
                denominator_target_cohort_definition_id,
                denominator_target_cohort_name)) %>%
      filter(database_name %in% input$incidence_database_name_selector)  %>%
   #   filter(denominator_target_cohort_name %in% input$incidence_denominator_cohort_name_selector)  %>%
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector)  %>%
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>%
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>%
      #filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)   %>%
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector)  %>%
      filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector)

 #   print(table)

 #   table <- table %>%
 #      filter(time_at_risk %in% as.character(input$incidence_time_at_risk_selector))

#    print(table)

    table
  })
  output$tbl_incidence_estimates<-  DT::renderDataTable({

    table<-get_incidence_estimates()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>%
      mutate(incidence=nice.num2(incidence)) %>%
      mutate(incidence_95CI_lower=nice.num2(incidence_95CI_lower)) %>%
      mutate(incidence_95CI_upper=nice.num2(incidence_95CI_upper)) %>%
      mutate(incidence= ifelse(!is.na(incidence),
                                          paste0(incidence, " (",
                                                 incidence_95CI_lower," to ",
                                                 incidence_95CI_upper, ")"))) %>%
      select(!c("incidence_95CI_lower", "incidence_95CI_upper",
                "denominator_days_prior_observation",
                "time_at_risk", "year_index", "month_index", "year_month")) %>%
      mutate(n_persons=nice.num.count(n_persons)) %>%
      mutate(n_events=nice.num.count(n_events)) %>%
      mutate(person_years=nice.num.count(person_years)) %>%
      relocate(incidence_start_date) %>%
      relocate(incidence_end_date, .after = incidence_start_date) %>%
      relocate(person_years, .after = n_persons) %>%
      rename(`Start Date` = incidence_start_date,
             `End Date` = incidence_end_date,
             `Persons (n)` = n_persons,
             `Person Years`= person_years,
             `Events (n)` = n_events,
             `Incidence (100000 pys)` = incidence,
             Outcome = outcome_cohort_name,
      #       Denominator = denominator_target_cohort_name,
             `Time Interval` = analysis_interval,
             Age = denominator_age_group,
             Sex = denominator_sex,
             Database = database_name)
      #       `Time at risk` = time_at_risk)

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "incidence_estimates"))
              ))
  } )
  output$plot_incidence_estimates<- renderPlotly({

    table<-get_incidence_estimates()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    if(is.null(input$incidence_plot_group)){
      if(!is.null(input$incidence_plot_facet)){
        if(input$incidence_plot_scale == "free") {
          if(input$incidence_plot_ribbon == "yes") {
            p<-table %>%
              unite("facet_var",
                    c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
              ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                                ymin = "incidence_95CI_lower",
                                ymax = "incidence_95CI_upper")) +
              geom_point(position=position_dodge(width=1), size = 0.5)+
              geom_errorbar(width=0) +
              facet_wrap(vars(facet_var),ncol = 2, scales = "free")+
              scale_y_continuous(
                limits = c(0, NA)
              ) +
              theme_bw()
          } else {
            p<-table %>%
              unite("facet_var",
                    c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
              ggplot(aes_string(x=input$incidence_x_axis, y="incidence")) +
              geom_point(position=position_dodge(width=1), size = 0.5)+
              facet_wrap(vars(facet_var),ncol = 2, scales = "free")+
              scale_y_continuous(
                limits = c(0, NA)
              ) +
              theme_bw()
          }
        } else {
          if(input$incidence_plot_ribbon == "yes") {
            p<-table %>%
              unite("facet_var",
                    c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
              ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                                ymin = "incidence_95CI_lower",
                                ymax = "incidence_95CI_upper")) +
              geom_point(position=position_dodge(width=1), size = 0.5)+
              geom_errorbar(width=0) +
              facet_wrap(vars(facet_var),ncol = 2)+
              scale_y_continuous(
                limits = c(0, NA)
              ) +
              theme_bw()
          } else {
            p<-table %>%
              unite("facet_var",
                    c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
              ggplot(aes_string(x=input$incidence_x_axis, y="incidence")) +
              geom_point(position=position_dodge(width=1), size = 0.5)+
              facet_wrap(vars(facet_var),ncol = 2)+
              scale_y_continuous(
                limits = c(0, NA)
              ) +
              theme_bw()
          }
        }

      } else{
        if(input$incidence_plot_ribbon == "yes") {
          p<-table %>%
            ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                              ymin = "incidence_95CI_lower",
                              ymax = "incidence_95CI_upper")) +
            geom_point(position=position_dodge(width=1), size = 0.5)+
            geom_errorbar(width=0) +
            scale_y_continuous(
              limits = c(0, NA)
            ) +
            theme_bw()
        } else {
          p<-table %>%
            ggplot(aes_string(x=input$incidence_x_axis, y="incidence")) +
            geom_point(position=position_dodge(width=1), size = 0.5)+
            scale_y_continuous(
              limits = c(0, NA)
            ) +
            theme_bw()
        }
      }
    }


    if(!is.null(input$incidence_plot_group) ){

      if(is.null(input$incidence_plot_facet) ){
        if(input$incidence_plot_ribbon == "yes") {
          p<-table %>%
            unite("Group",
                  c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                              ymin = "incidence_95CI_lower",
                              ymax = "incidence_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1), size = 0.5)+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            theme_bw()
        } else {
          p<-table %>%
            unite("Group",
                  c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1), size = 0.5)+
            theme_bw()
        }
      }

      if(!is.null(input$incidence_plot_facet) ){
        if(!is.null(input$incidence_plot_group) ){
          if(input$incidence_plot_scale == "free") {
            if(input$incidence_plot_ribbon == "yes") {
              p<-table %>%
                unite("Group",
                      c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
                unite("facet_var",
                      c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
                ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                                  ymin = "incidence_95CI_lower",
                                  ymax = "incidence_95CI_upper",
                                  group="Group",
                                  colour="Group")) +
                geom_point(position=position_dodge(width=1), size = 0.5)+
                geom_errorbar(width=0, position=position_dodge(width=1)) +
                facet_wrap(vars(facet_var),ncol = 2, scales = "free")+
                scale_y_continuous(
                  limits = c(0, NA)
                )  +
                theme_bw()
            } else {
              p<-table %>%
                unite("Group",
                      c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
                unite("facet_var",
                      c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
                ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                                  group="Group",
                                  colour="Group")) +
                geom_point(position=position_dodge(width=1), size = 0.5)+
                facet_wrap(vars(facet_var),ncol = 2, scales = "free")+
                scale_y_continuous(
                  limits = c(0, NA)
                )  +
                theme_bw()
            }
          } else {
            if(input$incidence_plot_ribbon == "yes") {
              p<-table %>%
                unite("Group",
                      c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
                unite("facet_var",
                      c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
                ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                                  ymin = "incidence_95CI_lower",
                                  ymax = "incidence_95CI_upper",
                                  group="Group",
                                  colour="Group")) +
                geom_point(position=position_dodge(width=1), size = 0.5)+
                geom_errorbar(width=0, position=position_dodge(width=1)) +
                facet_wrap(vars(facet_var),ncol = 2)+
                scale_y_continuous(
                  limits = c(0, NA)
                )  +
                theme_bw()
            } else {
              p<-table %>%
                unite("Group",
                      c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>%
                unite("facet_var",
                      c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>%
                ggplot(aes_string(x=input$incidence_x_axis, y="incidence",
                                  group="Group",
                                  colour="Group")) +
                geom_point(position=position_dodge(width=1), size = 0.5)+
                facet_wrap(vars(facet_var),ncol = 2)+
                scale_y_continuous(
                  limits = c(0, NA)
                )  +
                theme_bw()
            }
          }
        }
      }

    }

    p

  })

  get_incidence_attrition<-reactive({

    table<-incidence_attrition %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id,
                #analysis_interval,
                analysis_repeated_events,
                analysis_outcome_washout,
                analysis_complete_database_intervals,
                denominator_target_cohort_definition_id,
                denominator_target_cohort_name,
                denominator_start_date,
                denominator_end_date)) %>%
      filter(cdm_name %in% input$incidence_database_name_selector)    %>%
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>%
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>%
      #filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)     %>%
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector) %>%
      filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector)
    table
  })
  output$tbl_incidence_attrition<-  DT::renderDataTable({

    table<-get_incidence_attrition()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>%
      dplyr::select(!(c("...1", "reason_id", "outcome_cohort_id", "denominator_cohort_id", "denominator_cohort_name",
                        "denominator_days_prior_observation", "analysis_min_cell_count"))) %>%
      rename('Number records' = number_records,
             'Number subjects' = number_subjects,
             `Excluded records`= excluded_records,
             `Excluded subjects`= excluded_subjects,
             Reason = reason,
             Outcome = outcome_cohort_name,
             Age = denominator_age_group,
             Sex = denominator_sex,
             `Time Interval` = analysis_interval,
             Database = cdm_name ) %>%
      dplyr::relocate("Reason", .before = "Outcome")

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "incidence_attrition"))
              ))
  })

  # infection attrition
  get_infection_attrition <- reactive({
    table <- att_infection %>%
      dplyr::filter(cdm_name == input$incidence_database_name_selector)

    table
  })

  get_incidence_comp <- reactive({
    table<-incidence_estimates %>%
      # first deselect settings which did not vary for this study
      select(!c(analysis_id,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date,
                cohort_obscured,
                analysis_outcome_washout,
                result_obscured,
                outcome_cohort_id,
                analysis_repeated_events,
                denominator_cohort_id,
                analysis_min_cell_count,
                denominator_cohort_name,
                denominator_target_cohort_definition_id,
                denominator_target_cohort_name)) %>%
      filter(database_name %in% input$incidence_comp_database_name_selector)  %>%
      filter(as.character(incidence_start_date) %in% input$incidence_comp_start_date_selector)  %>%
      filter(denominator_age_group %in% input$incidence_comp_denominator_age_group_selector)     %>%
      filter(denominator_sex %in% input$incidence_comp_denominator_sex_selector)     %>%
      filter(outcome_cohort_name %in% input$incidence_comp_outcome_cohort_name_selector)  %>%
      filter(analysis_interval %in% "months")

    table
  })
  output$plot_incidence_comparison <- renderPlotly({
    table <- get_incidence_comp()

    # Average by month of SI
    covid_si_working_avg <- covid_si %>%
      dplyr::filter(country == countriesDb[[input$incidence_comp_database_name_selector]]) %>%
      dplyr::mutate(month = month(date), year = year(date)) %>%
      dplyr::group_by(month, year) %>%
      dplyr::summarise(si = mean(si)) %>%
      dplyr::mutate(date = as.Date(paste0(year,"-",month,"-01")))

    table <- table %>%
      dplyr::left_join(covid_si_working_avg,
                       by = c("incidence_start_date" = "date")) %>%
      dplyr::mutate(si = dplyr::if_else(is.na(si), 0, si))

    table <- table %>%
      dplyr::filter(!is.na(incidence))

    scale_fct <- (table %>% dplyr::filter(si == max(si)) %>% dplyr::pull(si))/(table %>% dplyr::filter(incidence == max(incidence)) %>% dplyr::pull(incidence))
    scale_fct <- scale_fct[1]

    table <- table %>%
      dplyr::mutate(si = si/scale_fct)

    if(input$incidence_comp_sma == "yes") {
      table <- table %>%
        dplyr::group_by(outcome_cohort_name) %>%
        dplyr::mutate(incidence = zoo::rollmean(incidence,5, fill = NA)) %>%
        dplyr::mutate(incidence_95CI_lower = zoo::rollmean(incidence_95CI_lower,5, fill = NA)) %>%
        dplyr::mutate(incidence_95CI_upper = zoo::rollmean(incidence_95CI_upper,5, fill = NA)) %>%
        dplyr::filter(!is.na(incidence))
    }

    p <- table %>%
      unite("Group",
            c(all_of("outcome_cohort_name")), remove = FALSE, sep = "; ") %>%
      ggplot(aes_string(x= "incidence_start_date", y="incidence",
                        ymin = "incidence_95CI_lower",
                        ymax = "incidence_95CI_upper")) +
      geom_line(aes_string(colour = "Group")) +
      geom_ribbon(aes_string(fill = "Group"), alpha = 0.2) +
      geom_ribbon(aes_string(ymin = 0, ymax = "si"), fill = "lightgray", alpha = 0.4) +
      scale_y_continuous("Incidence", sec.axis = sec_axis(~.*scale_fct, name = "Stringency Index")) +
      theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1),
                         legend.position = "bottom") + labs(x = "Date") +
      scale_x_date(date_breaks = "4 months")

    # calculate the range of the secondary y-axis
    y2_range <- ggplot_build(p)$layout$panel_params[[1]]$y.range*scale_fct

    # get plotly object
    pp <- ggplotly(p)
    # change manually wrong names of grouping (legend)
    n_cases <- length(unique(table$outcome_cohort_name))
    for (i in 1:n_cases) {
      pp$x$data[[i]]$name <- gsub("\\)","",gsub("\\(","",gsub(',1','',pp$x$data[[i]]$name)))
      pp$x$data[[i]]$legendgroup <- gsub("\\)","",gsub("\\(","",gsub(',1','',pp$x$data[[i]]$legendgroup)))
      pp$x$data[[i + n_cases]]$name <- gsub("\\)","",gsub("\\(","",gsub(',1','',pp$x$data[[i + n_cases]]$name)))
      pp$x$data[[i + n_cases]]$legendgroup <- gsub("\\)","",gsub("\\(","",gsub(',1','',pp$x$data[[i + n_cases]]$legendgroup)))
      pp$x$data[[i + n_cases]]$showlegend <- FALSE
    }

    # add the secondary axis back in
    pp %>%
      add_trace(x=~incidence_start_date, y=~si, yaxis="y2",
                data=table, showlegend=FALSE, inherit=FALSE, mode = "line") %>%
      layout(yaxis2 = list(overlaying = "y", side = "right",
                           title = list(text = "Stringency Index", font = list(size = 14)),
                           range = y2_range, zeroline = FALSE,
                           ticks = "outside", tickfont = list(size = 12)),
             legend = list(x = 1.1, y = 0))

  })

  get_infection_attrition <- reactive({
    table <- att_infection %>%
      dplyr::filter(cdm_name == input$incidence_database_name_selector)

    table
  })
  output$plot_infection_attrition <- renderGrViz({
    validate(need(length(input$incidence_database_name_selector) == 1,
                  "Only one database can be selected at a time."))
    table <- get_infection_attrition()
    attr(table, "class") <- c("summarised_result", attr(table, "class"))
    suppressWarnings(p <- table %>% plotAttHere())
    p
  })

  # table one
  get_table_one <-reactive({

    table<-characterisation_index %>%
    #  filter(Outcome %in% input$table1_outcome_cohort_name_selector)
      filter(Outcome  == "All population")

    table
  })
  output$tbl_table_one<-  DT::renderDataTable({

    table<-get_table_one()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "table_one"))
              ))
  } )

  # SR
  get_data_sr <-reactive({
    # enforce outcome only 1
    x <-incidence_estimates %>%
      filter(outcome_cohort_name %in% input$tsa_outcome_cohort_name_selector,
             analysis_interval == "months",
             denominator_sex %in% input$tsa_sex_selector,
             denominator_age_group %in% input$tsa_age_selector,
             database_name %in% input$tsa_database_name_selector
        #     denominator_target_cohort_name %in% input$tsa_denominator_cohort_name_selector,
        #     time_at_risk %in% input$tsa_time_at_risk_selector
             ) %>%
      mutate(quarter=quarter(incidence_start_date),
             year=year(incidence_start_date)) %>%
      mutate(n_events = if_else(is.na(n_events), 1, n_events)) %>% # think what to do here, could also remove NA but then get many 0s that are not 0s? or drop? (K)
      group_by(year, quarter, database_name)%>%
      summarise(
        n_events_sum = sum(n_events),
        person_years_sum = sum(person_years),
        .groups = 'drop'
      ) %>%
      mutate(incidence_start_date = ymd(paste(year, quarter * 3 - 2, "01", sep = "-")))%>%
      arrange(incidence_start_date)%>%
      mutate(time = row_number())%>%
      mutate(intrv = ifelse(incidence_start_date >= input$tsa_timepoint_selector, 1, 0)) %>%
      filter(incidence_start_date != input$tsa_timepoint_selector)

    if(!(all(x %>% dplyr::pull("n_events_sum") == 0))) {
      x <- x[min(which(x$n_events_sum != 0)):dim(x)[1],]
      x <- x[!is.na(x$person_years_sum),]
      x[is.na(x$n_events_sum),]$n_events_sum <- 5
      x[x$n_events_sum==0,]$n_events_sum<-5
      x$time <- 1:dim(x)[1]
      step <- as.numeric(x$incidence_start_date >= input$tsa_timepoint_selector)
    } else {
      x <- x %>%
        dplyr::filter(n_events_sum != 0)
    }
    x
  })
  get_data_sr_month <-reactive({
    # enforce outcome only 1
    x <-incidence_estimates %>%
      filter(outcome_cohort_name %in% input$tsa_outcome_cohort_name_selector,
             analysis_interval == "months",
             denominator_sex %in% input$tsa_sex_selector,
             denominator_age_group %in% input$tsa_age_selector,
             database_name %in% input$tsa_database_name_selector
             #     denominator_target_cohort_name %in% input$tsa_denominator_cohort_name_selector,
             #     time_at_risk %in% input$tsa_time_at_risk_selector
      ) %>%
      mutate(month=month(incidence_start_date),
             year=year(incidence_start_date),
             quarter =quarter(incidence_start_date)) %>%
      mutate(n_events = if_else(is.na(n_events), 1, n_events)) %>% # think what to do here, could also remove NA but then get many 0s that are not 0s? or drop? (K)
      group_by(year, quarter, month, database_name)%>%
      summarise(
        n_events_sum = sum(n_events),
        person_years_sum = sum(person_years),
        .groups = 'drop'
      ) %>%
      mutate(incidence_start_date = ymd(paste(year, month, "01", sep = "-")))%>%
      arrange(incidence_start_date)%>%
      mutate(time = row_number())%>%
      mutate(intrv = ifelse(incidence_start_date >= input$tsa_timepoint_selector, 1, 0)) %>%
      filter(incidence_start_date != input$tsa_timepoint_selector)

    if(!(all(x %>% dplyr::pull("n_events_sum") == 0))) {
      x <- x[min(which(x$n_events_sum != 0)):dim(x)[1],]
      x <- x[!is.na(x$person_years_sum),]
      x[is.na(x$n_events_sum),]$n_events_sum <- 5
      x[x$n_events_sum==0,]$n_events_sum<-5
      x$time <- 1:dim(x)[1]
      step <- as.numeric(x$incidence_start_date >= input$tsa_timepoint_selector)
    } else {
      x <- x %>%
        dplyr::filter(n_events_sum != 0)
    }
    x
  })

  get_data_sr_raw <- reactive({
    x <-incidence_estimates %>%
      filter(outcome_cohort_name %in% input$tsa_outcome_cohort_name_selector,
             analysis_interval == "months",
             denominator_sex %in% input$tsa_sex_selector,
             denominator_age_group %in% input$tsa_age_selector,
             database_name %in% input$tsa_database_name_selector
        #     denominator_target_cohort_name %in% input$tsa_denominator_cohort_name_selector,
        #     time_at_risk %in% input$tsa_time_at_risk_selector
      )
    x
  })
  output$tbl_segmented_regression <- renderDataTable({
    validate(need(!grepl("COVID-19",input$tsa_outcome_cohort_name_selector),
                  "No comparison pre-intervention possible with post-COVID related outcomes."))
    x <- get_data_sr()
    validate(need(x %>% dplyr::tally() != 0,
                  "No records available for this outcome."))
    # Model with intervention
    model_with_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + lag(n_events_sum) +
                              intrv*time, family=quasipoisson, data=x)
    model_with_intrv <- update(model_with_intrv, . ~ . + pbs(as.numeric(x$quarter), df=4))
    x_without_intrv <- x %>% dplyr::filter(incidence_start_date <= as.Date(input$tsa_timepoint_selector))
    model_without_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + lag(n_events_sum) +
                                 time + pbs(as.numeric(quarter), df=4), family=quasipoisson, data=x_without_intrv)
    # Model with only step and slope
    model_simple <- glm(n_events_sum ~ offset(log(person_years_sum)) + intrv*time, family=quasipoisson, data=x)
    # Calculate the critical value for the 95% confidence interval
    critical_value <- qnorm(0.975)
    # Calculate predictions and SEs for the model with intervention
    pred_simple <- predict(model_simple, type="response", se.fit = TRUE)
    # Ensure element-wise operation for the calculation of CI
    ci_simple_lower <- pred_simple$fit - critical_value * pred_simple$se.fit
    ci_simple_upper <- pred_simple$fit + critical_value * pred_simple$se.fit
    # Calculate predictions and SEs for the model with intervention
    pred_with_intrv <- predict(model_with_intrv, type="response", se.fit = TRUE)
    # Ensure element-wise operation for the calculation of CI
    ci_with_intrv_lower <- pred_with_intrv$fit - critical_value * pred_with_intrv$se.fit
    ci_with_intrv_upper <- pred_with_intrv$fit + critical_value * pred_with_intrv$se.fit
    # Calculate predictions and SEs for the model without intervention
    pred_without_intrv <- predict(model_without_intrv, type="response", se.fit = TRUE, newdata = x)
    ci_without_intrv_lower <- pred_without_intrv$fit - critical_value * pred_without_intrv$se.fit
    ci_without_intrv_upper <- pred_without_intrv$fit + critical_value * pred_without_intrv$se.fit

    # Combine forecasts with CIs
    forecasts_simple <- data.frame(
      Date = x$incidence_start_date,
      Forecast = round(c(pred_simple$fit) * 100000/x$person_years_sum, digits = 2),  #lagged term included so first term no pred
      Lower_95_CI = round(c(ci_simple_lower) * 100000/x$person_years_sum, digits = 2),
      Upper_95_CI = round(c(ci_simple_upper) * 100000/x$person_years_sum, digits = 2),
      Model = "Step and slope only",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    # Combine forecasts with CIs
    forecasts_with_intrv <- data.frame(
      Date = x$incidence_start_date,
      Forecast = round(c(NA, pred_with_intrv$fit) * 100000/x$person_years_sum, digits = 2),  #lagged term included so first term no pred
      Lower_95_CI = round(c(NA, ci_with_intrv_lower) * 100000/x$person_years_sum, digits = 2),
      Upper_95_CI = round(c(NA, ci_with_intrv_upper) * 100000/x$person_years_sum, digits = 2),
      Model = "With Intervention",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    forecasts_without_intrv <- data.frame(
      Date = x$incidence_start_date,
      Forecast = round(pred_without_intrv$fit * 100000/x$person_years_sum, digits = 2),#lagged term included so remove first obs, as no pred
      Lower_95_CI = round(ci_without_intrv_lower * 100000/x$person_years_sum, digits = 2),
      Upper_95_CI = round(ci_without_intrv_upper * 100000/x$person_years_sum, digits = 2),
      Model = "Remove Intervention",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    forecasts <- rbind(forecasts_with_intrv, forecasts_without_intrv, forecasts_simple)
    forecasts <- as_tibble(forecasts)

    datatable(forecasts,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "segmented_regression"))
              ))

  })
  get_diagnostics <- reactive({
    x <-incidence_estimates %>%
      filter(outcome_cohort_name %in% input$tsa_outcome_cohort_name_selector,
             analysis_interval == "months",
             denominator_sex %in% input$tsa_sex_selector,
             denominator_age_group %in% input$tsa_age_selector,
             database_name %in% input$tsa_database_name_selector
       #      denominator_target_cohort_name %in% input$tsa_denominator_cohort_name_selector,
      #       time_at_risk %in% input$tsa_time_at_risk_selector
      ) %>%
      mutate(quarter=quarter(incidence_start_date),
             year=year(incidence_start_date)) %>%
      mutate(n_events = if_else(is.na(n_events), 1, n_events)) %>% # think what to do here, could also remove NA but then get many 0s that are not 0s? or drop? (K)
      group_by(year, quarter, database_name)%>%
      summarise(
        n_events_sum = sum(n_events),
        person_years_sum = sum(person_years),
        .groups = 'drop'
      ) %>%
      mutate(incidence_start_date = ymd(paste(year, quarter * 3 - 2, "01", sep = "-")))%>%
      arrange(incidence_start_date)%>%
      mutate(time = row_number())%>%
      mutate(intrv = ifelse(incidence_start_date >= input$tsa_timepoint_selector, 1, 0)) %>%
      filter(incidence_start_date != input$tsa_timepoint_selector)

    if(!(all(x %>% dplyr::pull("n_events_sum") == 0))) {
      x <- x[min(which(x$n_events_sum != 0)):dim(x)[1],]
      x <- x[!is.na(x$person_years_sum),]
      x[is.na(x$n_events_sum),]$n_events_sum <- 5
      x[x$n_events_sum==0,]$n_events_sum<-5
      x$time <- 1:dim(x)[1]
      step <- as.numeric(x$incidence_start_date >= input$tsa_timepoint_selector)
    } else {
      x <- x %>%
        dplyr::filter(n_events_sum != 0)
    }
    validate(need(x %>% dplyr::tally() != 0,
                  "No records available for this outcome."))
    model_with_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + #lag(n_events_sum) +
                              intrv*time, family=quasipoisson, data=x)
    model_with_intrv <- update(model_with_intrv, . ~ . + pbs(as.numeric(x$quarter), df=4))
    x_without_intrv <- x %>% dplyr::filter(incidence_start_date <= as.Date(input$tsa_timepoint_selector))
    model_without_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + #lag(n_events_sum) +
                                 time + pbs(as.numeric(quarter), df=4), family=quasipoisson, data=x_without_intrv)

    # Extract and combine diagnostics for both models, including coefficients and p-values
    diagnostics <- data.frame(
      Model = c(rep("With Intervention", nrow(summary(model_with_intrv)$coefficients)),
                rep("Remove Intervention", nrow(summary(model_without_intrv)$coefficients))),
      Parameter = c(rownames(summary(model_with_intrv)$coefficients), rownames(summary(model_without_intrv)$coefficients)),
      Estimate = round(c(summary(model_with_intrv)$coefficients[, "Estimate"], summary(model_without_intrv)$coefficients[, "Estimate"]), digits = 2),
      "Std.Error" = round(c(summary(model_with_intrv)$coefficients[, "Std. Error"], summary(model_without_intrv)$coefficients[, "Std. Error"]), digits = 2),
      Statistics = round(c(summary(model_with_intrv)$coefficients[, "t value"], summary(model_without_intrv)$coefficients[, "t value"]), digits = 2),
      "P-value" = round(c(summary(model_with_intrv)$coefficients[, "Pr(>|t|)"], summary(model_without_intrv)$coefficients[, "Pr(>|t|)"]), digits = 2)
    )

    diagnostics$deviance <- round(c(rep(deviance(model_with_intrv),
                                  nrow(summary(model_with_intrv)$coefficients)),
                              rep(deviance(model_without_intrv),
                                  nrow(summary(model_without_intrv)$coefficients))), digits = 2)

    diagnostics
    })
  output$tbl_segmented_regression_diagnostics <- DT::renderDataTable({
    validate(need(!grepl("COVID-19",input$tsa_outcome_cohort_name_selector),
                  "No comparison pre-intervention possible with post-COVID related outcomes."))
    diagnostics <- get_diagnostics()
    diagnostics <- as_tibble(diagnostics)

    datatable(diagnostics,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "segmented_regression_diagnostics"))
              ))
  })
  output$plot_segmented_regression_residuals <- renderPlot({
    validate(need(!grepl("COVID-19",input$tsa_outcome_cohort_name_selector),
                  "No comparison pre-intervention possible with post-COVID related outcomes."))
    x <- get_data_sr()
    validate(need(x %>% dplyr::tally() != 0,
                  "No records available for this outcome."))
    # Model with intervention
    model_with_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + #lag(n_events_sum) +
                              intrv*time, family=quasipoisson, data=x)
    model_with_intrv <- update(model_with_intrv, . ~ . + pbs(as.numeric(x$quarter), df=4))
    residuals <- resid(model_with_intrv, type = "deviance")

    # Should we add residuals for the other models? (K)

    par(mfrow = c(1,2))
    stats::acf(residuals, main = "")
    stats::pacf(residuals, main = "")
#    title(paste0("Segemented regression diagnostics ", input$tsa_database_name_selector,
#                 "\n",input$tsa_age_selector,"_",input$tsa_sex_selector,
#                 "_",input$table1_outcome_cohort_name_selector,"_",input$tsa_timepoint_selector), outer = TRUE)
  })
  output$plot_segmented_regression <- renderPlotly({
    validate(need(!grepl("COVID-19",input$tsa_outcome_cohort_name_selector),
                  "No comparison pre-intervention possible with post-COVID related outcomes."))
    x <- get_data_sr()
    validate(need(x %>% dplyr::tally() != 0,
                  "No records available for this outcome."))
    # Model with intervention
    model_with_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + #lag(n_events_sum) +
                              intrv*time, family=quasipoisson, data=x)
    model_with_intrv <- update(model_with_intrv, . ~ . + pbs(as.numeric(x$quarter), df=4))
    x_without_intrv <- x %>% dplyr::filter(incidence_start_date <= as.Date(input$tsa_timepoint_selector))
    model_without_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + #lag(n_events_sum) +
                                 time + pbs(as.numeric(quarter), df=4), family=quasipoisson, data=x_without_intrv)
    # Model with only step and slope
    model_simple <- glm(n_events_sum ~ offset(log(person_years_sum)) + intrv*time, family=quasipoisson, data=x)
    # Calculate the critical value for the 95% confidence interval
    critical_value <- qnorm(0.975)
    # Calculate predictions and SEs for the model with intervention
    pred_simple <- predict(model_simple, type="response", se.fit = TRUE)
    # Ensure element-wise operation for the calculation of CI
    ci_simple_lower <- pred_simple$fit - critical_value * pred_simple$se.fit
    ci_simple_upper <- pred_simple$fit + critical_value * pred_simple$se.fit
    # Calculate predictions and SEs for the model with intervention
    pred_with_intrv <- predict(model_with_intrv, type="response", se.fit = TRUE)
    # Ensure element-wise operation for the calculation of CI
    ci_with_intrv_lower <- pred_with_intrv$fit - critical_value * pred_with_intrv$se.fit
    ci_with_intrv_upper <- pred_with_intrv$fit + critical_value * pred_with_intrv$se.fit
    # Calculate predictions and SEs for the model without intervention
    pred_without_intrv <- predict(model_without_intrv, type="response", se.fit = TRUE, newdata = x)
    ci_without_intrv_lower <- pred_without_intrv$fit - critical_value * pred_without_intrv$se.fit
    ci_without_intrv_upper <- pred_without_intrv$fit + critical_value * pred_without_intrv$se.fit

    # Combine forecasts with CIs
    forecasts_simple <- data.frame(
      Date = x$incidence_start_date,
      Forecast = c(pred_simple$fit) * 100000/x$person_years_sum,  #lagged term included so first term no pred
      Lower_95_CI = c(ci_simple_lower) * 100000/x$person_years_sum,
      Upper_95_CI = c(ci_simple_upper) * 100000/x$person_years_sum,
      Model = "Step and slope only",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    # Combine forecasts with CIs
    # forecasts_with_intrv <- data.frame(
    #   Date = x$incidence_start_date,
    #   Forecast = c(pred_with_intrv$fit) * 100000/x$person_years_sum,  #lagged term included so first term no pred
    #   Lower_95_CI = c(NA, ci_with_intrv_lower) * 100000/x$person_years_sum,
    #   Upper_95_CI = c(NA, ci_with_intrv_upper) * 100000/x$person_years_sum,
    #   Model = "With Intervention",
    #   age_group = input$tsa_age_selector,
    #   sex = input$tsa_sex_selector,
    #   outcome = input$tsa_outcome_cohort_name_selector,
    #   timepoint = input$tsa_timepoint_selector,
    #   cdm_name = input$tsa_database_name_selector
    # )
    forecasts_with_intrv <- data.frame(
      Date = x$incidence_start_date,
      Forecast = c(pred_with_intrv$fit) * 100000/x$person_years_sum,  # remove lag term if no lag(n_events_sum) in model
      Lower_95_CI = c(ci_with_intrv_lower) * 100000/x$person_years_sum,
      Upper_95_CI = c(ci_with_intrv_upper) * 100000/x$person_years_sum,
      Model = "With Intervention",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    forecasts_without_intrv <- data.frame(
      Date = x$incidence_start_date,
      Forecast = pred_without_intrv$fit * 100000/x$person_years_sum, #lagged term included so remove first obs, as no pred
      Lower_95_CI = ci_without_intrv_lower * 100000/x$person_years_sum,
      Upper_95_CI = ci_without_intrv_upper * 100000/x$person_years_sum,
      Model = "Remove Intervention",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    rawdata <- get_data_sr_raw() %>%
      dplyr::select(
        "Forecast" = "incidence",
        "Lower_95_CI" = "incidence_95CI_lower",
        "Upper_95_CI" = "incidence_95CI_upper",
        "Date" = "incidence_start_date",
        "age_group" = "denominator_age_group",
        "sex" = "denominator_sex",
        "cdm_name" = "database_name",
        "outcome" = "outcome_cohort_name") %>%
      dplyr::mutate(
        Model = "Raw incidence",
        "timepoint" = input$tsa_timepoint_selector
      ) %>%
      dplyr::select(Date, Forecast, Lower_95_CI, Upper_95_CI, Model, age_group,
                    sex, outcome, timepoint, cdm_name) %>%
      dplyr::filter(!is.na(Forecast))

    forecasts <- rbind(forecasts_with_intrv, forecasts_without_intrv, forecasts_simple, rawdata)

    # ignore last 5 data points
    forecasts <- forecasts %>%
      dplyr::filter(!(Date %in% tail(sort(unique(forecasts$Date)), 5)))

     ggplot(data=forecasts, aes(y=Forecast, x=Date, group=Model, colour=Model,
                              fill=Model)) +
      geom_point() +
      geom_line() +
      geom_ribbon(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), alpha=.3, linetype=0) +
      theme_minimal()

  })

  # SR monthly for resilience
  output$tbl_segmented_regression_monthly <- renderDataTable({
    validate(need(!grepl("COVID-19",input$tsa_outcome_cohort_name_selector),
                  "No comparison pre-intervention possible with post-COVID related outcomes."))
    x <- get_data_sr_month()
    validate(need(x %>% dplyr::tally() != 0,
                  "No records available for this outcome."))
    # Model with intervention
    model_with_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + lag(n_events_sum) +
                              intrv*time, family=quasipoisson, data=x)
    model_with_intrv <- update(model_with_intrv, . ~ . + pbs(as.numeric(x$quarter), df=4))
    x_without_intrv <- x %>% dplyr::filter(incidence_start_date <= as.Date(input$tsa_timepoint_selector))
    model_without_intrv <- glm(n_events_sum ~ offset(log(person_years_sum)) + lag(n_events_sum) +
                                 time + pbs(as.numeric(quarter), df=4), family=quasipoisson, data=x_without_intrv)
    # Model with only step and slope
    model_simple <- glm(n_events_sum ~ offset(log(person_years_sum)) + intrv*time, family=quasipoisson, data=x)
    # Calculate the critical value for the 95% confidence interval
    critical_value <- qnorm(0.975)
    # Calculate predictions and SEs for the model with intervention
    pred_simple <- predict(model_simple, type="response", se.fit = TRUE)
    # Ensure element-wise operation for the calculation of CI
    ci_simple_lower <- pred_simple$fit - critical_value * pred_simple$se.fit
    ci_simple_upper <- pred_simple$fit + critical_value * pred_simple$se.fit
    # Calculate predictions and SEs for the model with intervention
    pred_with_intrv <- predict(model_with_intrv, type="response", se.fit = TRUE)
    # Ensure element-wise operation for the calculation of CI
    ci_with_intrv_lower <- pred_with_intrv$fit - critical_value * pred_with_intrv$se.fit
    ci_with_intrv_upper <- pred_with_intrv$fit + critical_value * pred_with_intrv$se.fit
    # Calculate predictions and SEs for the model without intervention
    pred_without_intrv <- predict(model_without_intrv, type="response", se.fit = TRUE, newdata = x)
    ci_without_intrv_lower <- pred_without_intrv$fit - critical_value * pred_without_intrv$se.fit
    ci_without_intrv_upper <- pred_without_intrv$fit + critical_value * pred_without_intrv$se.fit

    # Combine forecasts with CIs
    forecasts_simple <- data.frame(
      Date = x$incidence_start_date,
      Forecast = round(c(pred_simple$fit) * 100000/x$person_years_sum, digits = 2),  #lagged term included so first term no pred
      Lower_95_CI = round(c(ci_simple_lower) * 100000/x$person_years_sum, digits = 2),
      Upper_95_CI = round(c(ci_simple_upper) * 100000/x$person_years_sum, digits = 2),
      Model = "Step and slope only",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    # Combine forecasts with CIs
    forecasts_with_intrv <- data.frame(
      Date = x$incidence_start_date,
      Forecast = round(c(NA, pred_with_intrv$fit) * 100000/x$person_years_sum, digits = 2),  #lagged term included so first term no pred
      Lower_95_CI = round(c(NA, ci_with_intrv_lower) * 100000/x$person_years_sum, digits = 2),
      Upper_95_CI = round(c(NA, ci_with_intrv_upper) * 100000/x$person_years_sum, digits = 2),
      Model = "With Intervention",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    forecasts_without_intrv <- data.frame(
      Date = x$incidence_start_date,
      Forecast = round(pred_without_intrv$fit * 100000/x$person_years_sum, digits = 2),#lagged term included so remove first obs, as no pred
      Lower_95_CI = round(ci_without_intrv_lower * 100000/x$person_years_sum, digits = 2),
      Upper_95_CI = round(ci_without_intrv_upper * 100000/x$person_years_sum, digits = 2),
      Model = "Remove Intervention",
      age_group = input$tsa_age_selector,
      sex = input$tsa_sex_selector,
      outcome = input$tsa_outcome_cohort_name_selector,
      timepoint = input$tsa_timepoint_selector,
      cdm_name = input$tsa_database_name_selector
    )

    forecasts <- rbind(forecasts_with_intrv, forecasts_without_intrv, forecasts_simple)
    forecasts <- as_tibble(forecasts)

    datatable(forecasts,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "segmented_regression"))
              ))

  })

}
