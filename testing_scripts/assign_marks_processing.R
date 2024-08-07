
# Setup -------------------------------------------------------------------

rcanvas::set_canvas_domain("https://canvas.sussex.ac.uk")

# get_submissions function ------------------------------------------------

get_submissions <- function (course_id, type, type_id) 
{
  if (!type %in% c("quizzes", "assignments")) 
    stop("type must be 'quizzes' or 'assignments'")
  url <- sprintf("%scourses/%s/%s/%s/submissions", "https://canvas.sussex.ac.uk/api/v1/", 
                 course_id, type, type_id)
  args <- list(access_token = rcanvas:::check_token(), per_page = 100)
  rcanvas:::process_response(url, args) %>% dplyr::bind_rows() %>% dplyr::mutate(course_id = course_id)
}

# Date fx for Reasons -----------------------------------------------------

get_time_remaining <- function(assign_return) {
  #MUST load lubridate for this to work otherwith the funky operator doesn't exist
  time_remaining <- lubridate::today() %--% lubridate::as_date(assign_return) %>% lubridate::as.period()
  return(time_remaining)
}

# Importing Data from Canvas------------------------------------------------

### Practice values

get_marking_info  <- function(course_id = course_id, assign_id = assign_id, 
                              assign_return = assign_return){
  
  message("Creating dataset...")
  
  ##Student info: names, id numbers, email
  students <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", course_id, "/students"),
    list(per_page = 100), "GET") %>%
    rcanvas:::paginate() %>%
    purrr::map(httr::content, "text") %>% 
    purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
    dplyr::bind_rows()
  
  students <- students[!duplicated(students), ] %>% 
    dplyr::mutate(cand_no = gsub("Candidate No : ", "", sortable_name))
  
  students <- students %>% 
    dplyr::select(-created_at, -sortable_name, -short_name, -integration_id, -pronouns) %>% 
    dplyr::mutate(cand_no = as.numeric(cand_no)) %>% 
    dplyr::filter(!is.na(cand_no))
  
  ##Submission info: id numbers, workflow state, score
  submissions <- get_submissions(course_id, type = "assignments", assign_id)
  
  submissions <- submissions %>% 
    dplyr::select(user_id, score, workflow_state)
  
  submissions <- submissions %>% 
    rename(id = user_id)
  
  ##Section info: marking groups
  sections <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", course_id, "/sections"),
    list(per_page = 100, `include[]` = c("students")), "GET") %>%
    rcanvas:::paginate() %>%
    purrr::map(httr::content, "text") %>% 
    purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
    dplyr::bind_rows() %>% 
    dplyr::filter(grepl("^marking", name, ignore.case = T)) %>%
    tidyr::unnest()
  
  # Generates values to use in the output/display on the dashboard
  # assess_course <- unique(sections$sis_course_id)
  # module_code <- stringr::str_sub(assess_course, 1, 5)
  # assess_year <- stringr::str_sub(assess_course, nchar(assess_course)-4, nchar(assess_course)) %>% 
  #   sub("_", "/", .)
  # assess_term <- stringr::str_sub(assess_course, 7, 8) %>% 
  #   sub("(.)(.)", "\\1erm \\2", .)
  
  sections <- sections %>% 
    dplyr::mutate(marking_group = gsub("^Marking\\s*(\\d+).*", "\\1", name)) %>% 
    dplyr::select(sis_user_id, marking_group)
  #round brackets identify a particular element
  #"\\1" subs in that element for the whole string
  
  canvas_data <- students %>% 
    left_join(sections) %>% 
    left_join(submissions) %>% 
    dplyr::mutate(marking_group = as.numeric(marking_group)) %>% 
    dplyr::filter(!is.na(marking_group)) %>% 
    dplyr::arrange(marking_group)
  
  message("Datasets created! Generating summaries...")
  
  # Counts ------------------------------------------------------------------
  
  ## Submitted
  
  n_submitted <- sum(canvas_data$workflow_state != "unsubmitted", na.rm = T)
  perc_submitted <- round(n_submitted/nrow(canvas_data)*100, 2)
  
  n_marked <- sum(canvas_data$workflow_state == "graded", na.rm = T)
  perc_marked <- (n_marked/n_submitted*100) %>% round(2)
  
  ## Fails
  n_fails <- canvas_data %>% dplyr::filter(score < 40) %>% nrow()
  perc_fails  <- round(n_fails/n_submitted*100, 2)
  
  backpack <- list(
    summary_values = list(
      n_submitted = n_submitted,
      perc_submitted = perc_submitted,
      n_marked = n_marked,
      perc_marked = perc_marked,
      n_fails = n_fails,
      perc_fails = perc_fails
    )
  )
  
  # ##Create info about markers
  # markers <- tibble(
  #   marker = marker_names,
  #   marking_group = as.character(1:length(marker))
  # )
  # 
  # ##Final dataset
  # data <- canvas_data %>% 
  #   dplyr::left_join(markers) %>%
  #   dplyr::filter(!is.na(marker)) %>% 
  #   as_tibble()
  
  
  
  
  # Summaries of Marks -------------------------------------------------------
  
  time_remaining <- get_time_remaining(assign_return = assign_return)
  
  backpack$time_remaining <- time_remaining

  ## Pass-only dataset
  pass_data <- canvas_data %>% dplyr::filter(score >= 40)
  
  fail_data <- canvas_data %>% dplyr::filter(score < 40)
  
  backpack$data <- list(
    pass_data = pass_data,
    fail_data = fail_data,
    canvas_data = canvas_data
  )
  
  message("Summaries created! Generating visualisations...")

# Visualisations ----------------------------------------------------------

  ## Histogram of all marks
  col_fail <- "#e51a1e"
  col_bars <- "#2177b4"
  col_bars_outline <- "#aad8fa"
  col_grand_mean <- "#ff7f01"
  col_wo_fails <- "#ffbf72"
  text_nudge = 0.5
  fail_boundary = 40
  
  marking_levels <- c(0,10,20,30,35,38,42,45,48,52,55,58,62,65,68,72,75,78, 82, 88, 95)
  
  overall_marks_hist <- canvas_data |> 
    dplyr::filter(!is.na(score)) %>% 
    # {if (assign_type != "tap") dplyr::mutate(score = forcats::as_factor(score, levels = marking_levels))
    #   else .} |> 
    ggplot2::ggplot(aes(x = score)) + 
    
    annotate("rect", 
             xmin = fail_boundary, xmax = -Inf, 
             ymin = 0, ymax = Inf,
             alpha = .10, fill = col_fail) +
    
    geom_vline(xintercept = mean(canvas_data$score, na.rm = T),
               linetype = "dashed", colour = col_grand_mean, size = 0.95) +
    
    annotate(geom = "text", label = "Grand Mean", 
             x = mean(canvas_data$score, na.rm = T)-text_nudge, 
             y = Inf, 
             vjust = 1,
             hjust = 1, 
             color = col_grand_mean,
             fontface = "bold"
    ) +
    
    geom_vline(xintercept = mean(pass_data$score, na.rm = T),
               linetype = "dashed", colour = col_wo_fails, size = 0.95) +
    
    annotate(geom = "text", label = "Grand Mean (Excluding Fails)", 
             x = mean(pass_data$score, na.rm = T)+text_nudge, 
             y = Inf,
             vjust = 1, 
             hjust = 0, 
             color = col_wo_fails, 
             fontface = "bold"
    ) +
    
    geom_histogram(binwidth = 1, colour = col_bars_outline, fill = col_bars, alpha = 1) + 
    
    coord_cartesian(xlim = c(0, 100)) +
    scale_y_continuous(name = "Frequency", breaks = seq(0, 100, 5)) + 
    scale_x_continuous(name = "Assessment Mark", breaks = seq(0, 100, 10)) +
    
    theme_minimal()
  
  ## FIX THIS ONE TOO SOMEDAY
  overall_marks_hist_student <- canvas_data %>% 
    dplyr::filter(!is.na(score)) %>% 
    # {if (assign_type != "tap") dplyr::mutate(score = forcats::as_factor(score, levels = c(0,10,20,30,35,38,42,45,48,52,55,58,62,65,68,72,75,78)))
    #   else .} %>%
    ggplot2::ggplot(aes(x = score)) +
    ggplot2::geom_histogram(colour = "black", fill = "grey", binwidth =  1) +
    ggplot2::geom_vline(xintercept = mean(canvas_data$score, na.rm = T),
                        linetype = "dashed", colour = "orange") +
    ggplot2::annotate(geom = "text",
                      label="Grand Mean", x = mean(canvas_data$score, na.rm = T)-8, y = Inf, vjust = 1,
                      color = "orange"
    ) +
    ggplot2::scale_y_continuous(name = "Frequency") + #figure out how to change the breaks
    ggplot2::scale_x_continuous(name = "Assessment Mark", 
                                breaks = seq(from = 0, 
                                             to = max(canvas_data$score, na.rm = T) %>% plyr::round_any(., 10), 
                                             by = 10)) +
    theme_minimal()

  
  
  backpack$plots <- list(
    overall_marks_hist = overall_marks_hist,
    overall_marks_hist_student = overall_marks_hist_student
  )
  
  ## For lab report marks (with categorical bins) - doesn't work atm because of the if statement
  
  # overall_marks_hist <- data %>% 
  #   dplyr::filter(!is.na(score)) %>% 
  #   # {if(assign_type != "tap") {
  #   #   dplyr::mutate(score = factor(score, 
  #   #                       levels = c(0,10,20,30,35,38,42,45,48,52,55,58,62,65,68,72,75,78)))}
  #   # } %>% 
  #   ggplot2::ggplot(aes(x = score)) +
  #   ggplot2::geom_bar(colour = "black", fill = "grey", drop = FALSE) +
  #   ggplot2::geom_vline(xintercept = mean(data$score, na.rm = T),
  #                       linetype = "dashed", colour = "magenta") +
  #   ggplot2::geom_vline(xintercept = mean(pass_data$score, na.rm = T),
  #                       linetype = "dashed", colour = "red") +
  #   ggplot2::scale_y_continuous(name = "Frequency") +
  #   ggplot2::scale_x_discrete(name = "Assessment Mark", drop = FALSE) +
  #   theme_minimal()
  
  ## Raincloud plot of marks by marker (using ALL data)
  raincloud_marks <- pass_data |> 
    ggplot2::ggplot(aes(x = factor(marking_group), 
                        y = score,
                        fill = factor(marking_group))) +
    ggdist::stat_halfeye( # create the half-distributions
      adjust = 0.5,
      justification = -.2,
      .width = 0,
      point_colour = NA,
      scale = .75
    ) +
    geom_boxplot( # add boxplots
      width = .12,
      outlier.color = NA,
      alpha = 0.5,
      position = position_dodge()
    ) +
    ggdist::stat_dots(
      side = "left",
      justification = 1.1,
      binwidth = .25,
      position = position_dodge(width = .5)
    ) +
    tidyquant::scale_fill_tq() + # makes it nice but not-rainbow colours
    tidyquant::theme_tq() +
    labs(
      x = "Marking Group Number",
      y = "Score"
    ) + 
    scale_x_discrete(drop = FALSE) +
    geom_hline(yintercept = mean(pass_data$score, na.rm = T),
               linetype = "dashed") +
    stat_summary(fun = "mean",
                 shape = 23) +
    theme(legend.position = "none")
  
  backpack$plots$raincloud_marks <- raincloud_marks

  message("Visualisations created! Generating summary tables...")
  
# Tables ------------------------------------------------------------------

  progress_summary <- canvas_data %>% 
    dplyr::group_by(marking_group) %>% 
    dplyr::summarise(
      scripts_submitted = sum(workflow_state != "unsubmitted", na.rm = T),
      scripts_marked = sum(workflow_state == "graded", na.rm = T),
      scripts_mean = round(mean(score, na.rm = T), 2),
      scripts_perc = round(100*(scripts_marked/scripts_submitted),2),
      avg_per_day = round((scripts_submitted - scripts_marked)/time_remaining$day, 2)
      )
  
  backpack$summaries <- list(
    progress_summary_data = progress_summary
  )

  progress_summary <- progress_summary %>%
    dplyr::mutate(
      avg_per_day = ifelse(avg_per_day < 7, formattable::color_tile("lightgreen", "transparent")(avg_per_day),
                           formattable::color_tile("transparent", "red")(avg_per_day)),
      scripts_perc = formattable::color_bar("lightblue",
                                            fun = \(x){x/100})(scripts_perc))
  
  backpack$summaries$progress_summary <- progress_summary
  
  return(backpack)
}

## Tinkering with the progress summary with fake data
# set.seed(420)

# progress_summary_fake <- tibble(
#   marker = marker_names,
#   scripts_submitted = rep(60, times = length(marker_names)),
#   scripts_marked = sample(1:60, size = length(marker_names)),
#   scripts_perc = round(100*(scripts_marked/scripts_submitted),2),
#   avg_per_day = round((scripts_submitted - scripts_marked)/time_remaining$day, 2)
# )
# 
# progress_summary_fake %>% 
#   dplyr::mutate(
#     avg_per_day = ifelse(avg_per_day < 7, formattable::color_tile("lightgreen", "transparent")(avg_per_day),
#                          formattable::color_tile("transparent", "red")(avg_per_day)),
#     scripts_perc = formattable::color_bar("lightblue")(scripts_perc)
#   ) %>% 
#   kable("html", escape = FALSE, align = c("c","c", "c", "l", "c"),
#         col.names = c("Marker", "Submitted", "Marked", "Progress (%)", "Mean Per Day")
#         ) %>% 
#   kable_styling()


stuff_to_check <- function() {
## Summary of marks, including fails
incl_fails_summary <- canvas_data %>% 
  dplyr::filter(!is.na(marking_group)) %>% 
  dplyr::group_by(marking_group) %>% 
  dplyr::summarise(
    #scripts_submitted = sum(workflow_state != "unsubmitted", na.rm = T),
    scripts_marked = sum(workflow_state == "graded", na.rm = T),
    #scripts_perc = round(100*(scripts_marked/scripts_submitted),2),
    mark_min = min(score, na.rm = T),
    mark_max = max(score, na.rm = T),
    mark_mean_fails = mean(score, na.rm = T),
    mark_sd_fails = sd(score, na.rm = T),
    n_fails = sum(score < 40, na.rm = T)
  )

backpack$incl_fails_summary <- incl_fails_summary

## Summary of marks, EXCLUDING fails
pass_summary <- canvas_data %>% 
  dplyr::filter(score > 40) %>% 
  dplyr::group_by(marking_group) %>% 
  dplyr::summarise(
    mark_mean = mean(score, na.rm = T),
    mark_sd = sd(score, na.rm = T),
    ci_low = mean_cl_normal(score)$ymin,
    ci_hi = mean_cl_normal(score)$ymax,
    mark_disc = mark_mean - round(mean(pass_data$score, na.rm = T), 2)
  )

backpack$pass_summary <- pass_summary

## Table for printing
marks_by_marker_table <- left_join(incl_fails_summary, pass_summary %>% dplyr::select(-starts_with("ci"))) %>% 
  kableExtra::kbl(col.names = c("Marking Group", "*N*~Marked~", "Min", "Max", 
                      "*M*~Overall~", "*SD*~Overall~", "*N*~Fails~", "*M*~Pass~", "*SD*~Pass~", "Diff"),
        digits = 2, 
        caption = "*Table 1* Descriptives by Marker") %>% 
  kableExtra::kable_styling()

backpack$marks_by_marker_table <- marks_by_marker_table

## Histograms of marks by marker - lab reports
# marker_hist <- pass_data %>% 
#   # dplyr::mutate(score = as_factor(score, 
#   #                        levels = c(0,10,20,30,35,38,42,45,48,52,55,58,62,65,68,72,75,78))) %>% 
#   ggplot(aes(x = score)) +
#   geom_bar(colour = "black", fill = "grey") +
#   #geom_vline(xintercept = 40, linetype = "dashed", colour = "red") +
#   facet_wrap(~marker) +
#   scale_y_continuous(name = "Frequency") +
#   scale_x_discrete(name = "Marker", breaks = c("45", "55", "65", "75"),
#                    labels = c("3rd", "2.2", "2.1", "1st")) +
#   theme_minimal()

## Histograms of marks by marker - lab reports
marker_hist <- pass_data %>% 
  ggplot(aes(x = score)) +
  geom_bar(colour = "black", fill = "grey") +
  #geom_vline(xintercept = 40, linetype = "dashed", colour = "red") +
  facet_wrap(~marking_group, scales = "free_x") +
  scale_y_continuous(name = "Frequency") +
  scale_x_continuous(name = "Marking Group", 
                     limits = c(40, 90),
                     breaks = seq(from = 40, to = 90, by = 10)) +
  theme_bw() +
  theme(panel.border=element_blank(), axis.line=element_line(),
        axis.text.x = element_text(angle = 45, size = 8)) +
  lemon::coord_capped_cart(bottom='both', left='both')

backpack$marker_hist <- marker_hist

## Means plot of marks by marker ##
## figure out how to add the x axis labels
marker_means_plot <- pass_summary %>% 
  ggplot(aes(x = marking_group, y = mark_mean)) +
  geom_errorbar(aes(ymin = mark_mean - mark_sd,
                    ymax = mark_mean + mark_sd),
                width = .1, colour = "blue") +
  geom_errorbar(aes(ymin = ci_low,
                    ymax = ci_hi),
                width = .2) +  
  geom_point(size= 3, pch = 23, fill = "grey") +
  scale_x_discrete(name = "Marking Group") +
  scale_y_continuous(name = "Mean Mark",
                     limits = c(40, 80),
                     breaks = seq(from = 0, to = 100, by = 10)) +
  geom_hline(yintercept = mean(pass_data$score, na.rm = T),
             linetype = "dotted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

backpack$marker_means_plot <- marker_means_plot

# ggplot2::ggsave(here::here("marking/mean_before_remarking.jpg"), marker_means_plot)

# Marks Analysis ----------------------------------------------------------

marker_lm <- lm(score ~ marking_group, data = pass_data)

marker_diffs_p <- marker_lm %>% 
  anova() %>% 
  broom::tidy() %>% 
  dplyr::filter(term == "marker") %>% 
  pull(p.value)

marker_diffs_p_print <- ifelse(marker_diffs_p < .001, "< .001", paste0("= ", weights::rd(marker_diffs_p, 3)))
marker_diffs_sig <- ifelse(marker_diffs_p < .05, TRUE, FALSE)

sig_marker_diffs <- modelbased::estimate_contrasts(marker_lm) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(p < .05)

is_marker_diffs <- nrow(sig_marker_diffs) > 0

# String Manipulations ----------------------------------------------------

## My clunky version - but it works!
# m <- as.vector(rbind(marker_names, rep(", ", times = length(marker_names))))
# m <- m[-length(m)]
# m <- append(m, "and ", after = length(m) - 1)
# marker_names_print  <- stringr::str_c(m, collapse = "")

## Milan's slick version
# paste(marker_names[-length(marker_names)], collapse = ", ")
## and even slicker!!
marker_names_print <- sub("(.*),(.*)", "\\1, and\\2", paste(marker_names, collapse = ", "))
# This says:
# paste together the marker names with a comma between each, first thing
# the sub-boobs capture any number of characters (because of the asterisks I think?)
# because of "greedy matching" the first boob captures EVERYTHING up until the last comma
# (because then there's only one comma left so it has to stop there)
# and then the second argument replaces the stuff between those two groups with the and etc.
# nifty!!!


# Marking Tracker ---------------------------------------------------------

marker_info <- tibble::tibble(
  marker = marker_names, 
  marking_group = 1:length(marker_names))

tracker <- readr::read_csv(here::here("marking/marking_tracker.csv")) %>% 
  dplyr::mutate(marker = factor(marker, levels = marker_names),
                reason = forcats::as_factor(reason)
  )

n_checked <- nrow(tracker)
perc_checked <- (n_checked/n_submitted*100) %>% round(2)

table_checked <- tracker %>% 
  dplyr::filter(!is.na(marker)) %>% 
  dplyr::group_by(marker, reason, .drop = FALSE) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  pivot_wider(names_from = reason, values_from = n) %>%
  dplyr::mutate(across(c(acmisc, check, fail), ~tidyr::replace_na(.x, 0))) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total = sum(c_across(c(acmisc, check, fail)), na.rm = T)) %>% 
  right_join(marker_info, .) 

n_scripts_checked <- sum(table_checked$check)

table_checked_printing <- table_checked %>% 
  dplyr::mutate(
    total = factor(total, levels = as.character(0:max(total))),
    total = formattable::color_tile("lightpink", "transparent")(total) # makes bg more red for FEWER checks
  ) %>%
  kableExtra::kbl("html", escape = FALSE,
        col.names = c("Marker", "Marking Group", "Fail", "Check", "Misconduct", "Total"),
        align = "lccccc") %>% 
  kableExtra::add_header_above(c(" " = 2, "Reason for Review" = 3, " " = 1)) %>% 
  kableExtra::kable_styling()

fails <- canvas_data %>% 
  dplyr::filter(score < 40)

fails_done <- left_join(fails %>% dplyr::select(cand_no, marker, marking_group), 
                        tracker %>% dplyr::select(cand_no, marker, done)) %>% 
  dplyr::mutate(done = replace_na(done, 0))

fails_checked <- sum(fails_done$done)

fails_not_done <- fails_done %>% 
  dplyr::filter(done < 1) %>% 
  dplyr::select(-done) %>% 
  kableExtra::kbl(align = "c", col.names = c("Candidate Number", "Marker", "Marking Group")) %>% 
  kableExtra::kable_styling()
}
