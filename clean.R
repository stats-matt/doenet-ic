# attempt_items
clean_attempt_items <- function(attempt_items) {
  # remove us
  attempt_items <-
    attempt_items %>%
    filter(
      !userId %in% c(
        "MJ2cQCzOU7Z17nBuq6sgl",
        "cTYBjc9mvmn3JIaaMHDCa",
        "txmteW6wcZk09kxRz1lAp",
        "2CWpWnEp3XaLhGO3iUgG7"
      )
    )
  
  # define versions
  attempt_items <-
    attempt_items %>%
    mutate(
      version =
        case_when(
          assignmentId == "kXhVAtL0zDjJI7p3ZpUOq" ~ "A",
          assignmentId == "LF3_ONicEdcWgbuSqgxTa" ~ "B",
        )
    )
  
  attempt_items <-
    attempt_items %>%
    dplyr::select(-assignmentId,-creditOverride,-generatedVariant,-viewedSolution,-viewedSolutionDate)
  return(attempt_items)
}

#########################################
# attempts
clean_attempts <- function(attempts) {
  attempts <-
    attempts %>%
    filter(
      !userId %in% c(
        "MJ2cQCzOU7Z17nBuq6sgl",
        "cTYBjc9mvmn3JIaaMHDCa",
        "txmteW6wcZk09kxRz1lAp",
        "2CWpWnEp3XaLhGO3iUgG7"
      )
    )
  
  attempts <-
    attempts %>%
    mutate(
      version =
        case_when(
          assignmentId == "kXhVAtL0zDjJI7p3ZpUOq" ~ "A",
          assignmentId == "LF3_ONicEdcWgbuSqgxTa" ~ "B",
        )
    )
  
  # clean up columns
  attempts <-
    attempts %>%
    dplyr::select(-assignmentId,-creditOverride,-assignedVariant,-generatedVariant,-contentId) %>%
    mutate(credit = as.numeric(credit),
           version = as.factor(version))
  
  # difference in credit (in attempts)
  attempts <-
    attempts %>%
    mutate(credit = as.numeric(credit),
           version = as.factor(version))
  return(attempts)
}

#####################################
# events
clean_events <- function(events) {
  events <-
    events %>%
    filter(
      !userId %in% c(
        "MJ2cQCzOU7Z17nBuq6sgl",
        "cTYBjc9mvmn3JIaaMHDCa",
        "txmteW6wcZk09kxRz1lAp",
        "2CWpWnEp3XaLhGO3iUgG7"
      )
    )
  
  events <-
    events %>%
    mutate(
      version =
        case_when(
          assignmentId == "kXhVAtL0zDjJI7p3ZpUOq" ~ "A",
          assignmentId == "LF3_ONicEdcWgbuSqgxTa" ~ "B",
        )
    )
  
  events <-
    events %>%
    dplyr::select(-assignmentId,-attemptNumber, -variant, -deviceName)
  
  # summarize events page
  events <-
    events %>%
    group_by(userId) %>%
    mutate(timestamp=anytime(timestamp)) %>% 
    mutate(time = timestamp - min(timestamp))
  
  events <-
    events %>%
    mutate(new = map(context, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(new) %>%
    rename(
      credit.prob.1 = X1,
      credit.prob.2 = X2,
      credit.prob.3 = X3
    )
  
  events <-
    events %>%
    mutate(new = map(object, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(new)
  
  events <-
    events %>%
    separate(componentName, into = c(NA, "section", "answer", "type"))
  
  events <-
    events %>%
    filter(!is.na(documentCreditAchieved))
  return(events)
}
