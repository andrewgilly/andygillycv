formatCurrentEmployment <- function(employmentHistory, showTibble = FALSE){
  
  tibbleData <- employmentHistory %>% 
    mutate(print_data = map(data, function(x) {
      current <- filter(x, jobEnd == "present")
      previous <- filter(x, jobEnd != "present")
      
      roles <- x %>%
        select(title:jobEnd) %>%
        separate(roleStart, c("roleStart_year", "roleStart_month"), "-", convert = TRUE) %>%
        separate(roleEnd, c("roleEnd_year", "roleEnd_month"), "-", convert = TRUE,
                 fill = "right") %>%
        mutate(roleStart_month = month.name[roleStart_month],
               roleEnd_month = map_chr(roleEnd_month, function(x) {
                 if (is.na(x)) return(NA_character_)
                 month.name[x]
               }),
               label = glue("{title} | {team}")) %>% 
        # mutate(roleStart_month = month.name[roleStart_month],
        #        roleEnd_month = map_chr(roleEnd_month, function(x) {
        #          if (is.na(x)) return(NA_character_)
        #          month.name[x]
        #        }),
        #        label = glue("{title} | {team} | {roleStart_month} {roleStart_year}--present")) %>%
        pull(label) %>%
        str_replace_all("NA ", "") %>%
        as.character() %>%
        list()
      
      history <- employmentHistory$data[[1]] %>%
        pivot_longer(contains("description"), names_to = "description_num",
                     values_to = "description") %>%
        filter(!is.na(description)) %>%
        group_by(title) %>%
        mutate(descriptions = list(description)) %>%
        separate(roleStart, c("roleStart_year", "roleStart_month"), "-", convert = TRUE) %>%
        separate(roleEnd, c("roleEnd_year", "roleEnd_month"), "-", convert = TRUE, fill = "right") %>%
        separate(jobStart, c("jobStart_year", "jobStart_month"), "-", convert = TRUE) %>%
        separate(jobEnd, c("jobEnd_year", "jobEnd_month"), "-", convert = TRUE, fill = "right") %>%
        mutate(roleStart_month = month.name[roleStart_month],
               roleEnd_month = map_chr(roleEnd_month, function(x) {
                 if (is.na(x)) return(NA_character_)
                 month.name[x]
               })) %>%
        ungroup() %>%
        filter(description_num == "description_1") %>%
        mutate(full_start = min(x$roleStart)) %>% 
        separate(full_start, c("roleStart_year", "roleStart_month"), convert = TRUE) %>%       
        mutate(timeline = glue("Present - {month.name[jobStart_month]} {jobStart_year}"),
               timeline = as.character(timeline),
               description_bullets = paste("**Responsibilities**\n\n",
                                           map_chr(descriptions, ~paste("-", .x, collapse = "\n"))),
               roles =  paste("**Positions**\n\n",
                              map_chr(roles, ~paste("-", .x, collapse = "\n")))) %>%
        select(title, timeline, leadingText, description_bullets, roles)
      
    })) %>%
    select(-data) %>%
    unnest(cols = c(print_data)) 
  
  output <- tibbleData %>% 
    glue_data(
      "### {org}",
      "\n\n",
      "{title}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}",
      "\n\n",
      "{leadingText}",
      "\n\n",
      "{description_bullets}",
      "\n\n",
      "{ifelse(is.na(roles), '', roles)}",
      "\n\n",
      "\n\n\n")
  
  if(showTibble){
    tibbleData
  }else{
    output
  }
  
}


formatFormerEmployment <- function(employmentHistory, 
                                   customTitle = NULL,
                                   showTibble = FALSE){
  
  org <- employmentHistory$org %>%  unique()
  loc <- employmentHistory$loc %>%  unique()
  
  employmentHistory <- employmentHistory$data[[1]]
  
  if(is.null(customTitle)){
    title <- employmentHistory$title[1]
  } else {
    title <- customTitle
  }

  leadingText <- employmentHistory %>%
    drop_na(leadingText) %>% 
    select(leadingText) %>%
    unique() %>% 
    as.vector()%>% 
    as.character()
  
  timeline <- employmentHistory %>% 
    select(jobStart:jobEnd) %>% 
    unique() %>% 
    separate(jobStart, c("jobStart_year", "jobStart_month"), "-", convert = TRUE) %>%
    separate(jobEnd, c("jobEnd_year", "jobEnd_month"), "-", convert = TRUE, fill = "right") %>%
    mutate(timeline = glue("{month.name[jobEnd_month]} {jobEnd_year} - {month.name[jobStart_month]} {jobStart_year}"),
           timeline = as.character(timeline)) %>% 
    select(timeline) %>% 
    as.vector() %>% 
    as.character()
  
  roleLabels <- employmentHistory %>%
    select(title:jobEnd) %>%
    separate(roleStart, c("roleStart_year", "roleStart_month"), "-", convert = TRUE) %>%
    separate(roleEnd, c("roleEnd_year", "roleEnd_month"), "-", convert = TRUE,
             fill = "right") %>%
    mutate(roleStart_month = month.name[roleStart_month],
           roleEnd_month = map_chr(roleEnd_month, function(x) {
             if (is.na(x)) return(NA_character_)
             month.name[x]
           }),
           label = glue("{title} | {team}")) %>% 
    # mutate(roleStart_month = month.name[roleStart_month],
    #        roleEnd_month = map_chr(roleEnd_month, function(x) {
    #          if (is.na(x)) return(NA_character_)
    #          month.name[x]
    #        }),
    #        label = glue("{title} | {team} | {roleStart_month} {roleStart_year}--{roleEnd_month} {roleEnd_year}")) %>%
    mutate(label = paste(map_chr(label, ~paste("-", .x, collapse = "\n")))) %>% 
    select(label) %>%
    unique() 
  
  roles <- paste0("**Positions**\n\n ",  
                  paste(paste0(roleLabels$label, "\n"), collapse="")) %>%
    substr(.,1,nchar(.)-1)
  
  tibbleData <- tibble(org,
                      title,
                      loc, 
                      timeline,
                      leadingText,
                      roles)
  
  output <- tibbleData %>% 
    glue_data("### {org}",
              "\n\n",
              "{title}",
              "\n\n",
              "{loc}",
              "\n\n",
              "{timeline}",
              "\n\n",
              "{leadingText}",
              "\n\n",
              "{ifelse(is.na(roles), '', roles)}",
              "\n\n",
              "\n\n\n")
  
  if(showTibble){
    tibbleData
  }else{
    output
  }

}





