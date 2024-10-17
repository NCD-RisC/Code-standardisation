

interactive_scatter <- function(subset, xvar, yvar, size_var, my_sex, my_age_group, plot.year = NULL, dynamic = TRUE){
  # if plot.year = FALSE need dynamic = true
  
  subset_level <- subset %>% 
    filter(sex == my_sex & age_group == my_age_group) %>%
    filter(variable == xvar | variable == yvar | variable == size_var)
  
  
  xmax <- ceiling(max(subset_level$mean[which(subset_level$variable == xvar)])/10)*10
  ymax <- ceiling(max(subset_level$mean[which(subset_level$variable == yvar)])/10)*10
  
  
  if(!(dynamic)){
    subset_level <- subset_level %>% filter(year == plot.year)
  }
  
  subset_level <- subset_level %>%
    select(-c(l,u,se)) %>%
    filter(variable == xvar | variable == yvar | variable == size_var) %>%
    mutate(variable = if_else(variable == xvar, "xvar", if_else(variable == yvar, "yvar", "size_var"))) %>%
    pivot_wider(names_from = variable, values_from = mean)
  
  if(size_var == xvar){
    subset_level <- subset_level %>% mutate(size_var = xvar)
  } else if (size_var == yvar){
    subset_level <- subset_level %>% mutate(size_var = yvar)
  }
  
  xlab    <- get_var_longname(xvar)
  ylab    <- get_var_longname(yvar)
  size_lab <- get_var_longname(size_var)
  
  xaxislab <- str_to_sentence(paste0(xlab, " (%)"))
  yaxislab <- str_to_sentence(paste0(ylab, " (%)"))
  
  year_longname <- ifelse(dynamic, paste0(min(subset_level$year), "-", max(subset_level$year)), plot.year)
  
  if(age_type == "adult"){
    sex.longname <- ifelse(my_sex == "female", "women", "men")
  } else{
    sex.longname <- ifelse(my_sex == "female", "girls", "boys")
  }
  
  age_group.longname <- switch(my_age_group,
                               "ageStd" = ifelse(age_type == "adult", "20+ years old", "5-19 years old"),
                               "young" = "20-39 years old",
                               "mid" = "40-64 years old",
                               "old" = "65+ years old")
  
  plot.title <- paste0(str_to_title(sex.longname), ", ", age_group.longname)
  
  
  if (!(dynamic) & !(size_var == xvar | size_var == yvar)){
    ##### CASE ONE - STATIC AND WANT TO PRINT SIZEVAR
    p <- plot_ly(subset_level,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~xvar,
                 y = ~yvar, 
                 size = ~ sizevar,
                 color = ~Superregion,
                 colors = sregion_col,
                 text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                                '<br>Superregion: ', Superregion,
                                "<br>", str_to_sentence(xlab), ": ", round(xvar,1), "%",
                                "<br>", str_to_sentence(ylab), ": ", round(yvar,1),"%",
                                "<br>", str_to_sentence(size_lab), ": ", round(size_var,1), "%"))
  }
  
  if (!(dynamic) & (size_var == xvar | size_var == yvar)){
    ##### CASE TWO - STATIC AND DON'T WANT TO PRINT SIZEVAR
    p <- plot_ly(subset_level,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~xvar,
                 y = ~yvar, 
                 size = ~ size_var,
                 color = ~Superregion,
                 colors = sregion_col,
                 text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>", str_to_sentence(xlab), ": ", round(xvar,1), "%",
                                 "<br>", str_to_sentence(ylab), ": ", round(yvar,1),"%"))
  }
  if (dynamic & !(size_var == xvar | size_var == yvar)){
    ##### CASE THREE- DYNAMIC AND WANT TO PRINT SIZEVAR
    p <- plot_ly(subset_level,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~xvar,
                 y = ~yvar, 
                 frame = ~year, 
                 size = ~ size_var,
                 color = ~Superregion,
                 colors = sregion_col,
                 legendgroup = ~Superregion,
                 text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>", str_to_sentence(xlab), ": ", round(xvar,1), "%",
                                 "<br>", str_to_sentence(ylab), ": ", round(yvar,1),"%",
                                 "<br>", str_to_sentence(size_lab), ": ", round(size_var,1), "%")) %>%
      animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))
  }
  
  if (dynamic & (size_var == xvar | size_var == yvar)){
    ##### CASE FOUR - DYNAMIC AND DON'T WANT TO PRINT SIZEVAR
    p <- plot_ly(subset_level,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~xvar,
                 y = ~yvar, 
                 frame = ~year, 
                 size = ~ size_var,
                 color = ~Superregion,
                 legendgroup = ~Superregion,
                 colors = sregion_col,
                 text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>", str_to_sentence(xlab), ": ", round(xvar,1), "%",
                                 "<br>", str_to_sentence(ylab), ": ", round(yvar,1),"%")) %>%
      animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))
  }
  
  
  p <- p %>% layout(title = plot.title, yaxis = list(title = yaxislab, range = list(0, ymax)), 
                    xaxis = list(title = xaxislab, range = list(0, xmax)),legend =list(font = list(size = 15)))
    
  
  
  return(p)
  
  
}


interactive_scatter_age_v_age <- function(subset, xvar, yvar, my_sex, xage_group, yage_group){
  # if plot.year = FALSE need dynamic = true
  
  subset_level <- subset %>% 
    filter(sex == my_sex) %>%
    filter((variable == xvar & age_group == xage_group)| (variable == yvar & age_group == yage_group)) %>%
    mutate(variable = ifelse(variable == xvar & age_group == xage_group, "xvar", "yvar"))
  
  xmax <- ceiling(max(subset_level$mean[which(subset_level$variable == "xvar")])/10)*10
  ymax <- ceiling(max(subset_level$mean[which(subset_level$variable == "yvar")])/10)*10
  
  subset_level <- subset_level %>%
    select(-c(l,u,se, age_group)) %>%
    pivot_wider(names_from = variable, values_from = mean)
  
  xlab    <- paste0(xvar, " ", xage_group)
  ylab    <- paste0(yvar, " ", yage_group)
  
  xaxislab <- str_to_sentence(paste0(xlab, " (%)"))
  yaxislab <- str_to_sentence(paste0(ylab, " (%)"))
  
  year_longname <- paste0(min(subset_level$year), "-", max(subset_level$year))
  
  if(age_type == "adult"){
    sex.longname <- ifelse(my_sex == "female", "women", "men")
  } else{
    sex.longname <- ifelse(my_sex == "female", "girls", "boys")
  }
  
  plot.title <- paste0(str_to_title(sex.longname))
  
  p <- plot_ly(subset_level,
               type = "scatter",
               mode = 'markers',
               hoverinfo="text",
               x = ~xvar,
               y = ~yvar, 
               frame = ~year, 
               color = ~Superregion,
               colors = sregion_col,
               legendgroup = ~Superregion,
               text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                               '<br>Superregion: ', Superregion,
                               "<br>", str_to_sentence(xlab), ": ", round(xvar,1), "%",
                               "<br>", str_to_sentence(ylab), ": ", round(yvar,1),"%")) %>%
    
    animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red"))) %>% 
    
    layout(title = plot.title, 
           yaxis = list(title = yaxislab, range = list(0, ymax)), 
           xaxis = list(title = xaxislab, range = list(0, xmax)),
           legend =list(font = list(size = 15)),
           shapes = list(list(
             type = "line",
             x0 = 0,
             x1 = max(xmax,ymax),
             xref = "x",
             y0  = 0,
             y1  =max(xmax,ymax),
             yref = "y",
             line = list(color = "grey50", dash = "dot"))))
  
  
  
  return(p)
  
  
}


interactive_scatter_ranking <- function(subset, xvar, yvar, my_sex, my_age_group, plot.year = NULL, dynamic = TRUE){
  # if plot.year = FALSE need dynamic = true
  
  subset_level <- subset %>% 
    filter(sex == my_sex & age_group == my_age_group)
  
  if(!(dynamic)){
    subset_level <- subset_level %>% filter(year == plot.year)
  }
  
  subset_level <- subset_level %>%
    select(-c(l,u,se)) %>%
    filter(variable == xvar | variable == yvar) %>%
    mutate(variable = if_else(variable == xvar, "xvar", "yvar")) %>%
    pivot_wider(names_from = variable, values_from = c(ranking, mean))
  
  xlab    <- get_var_longname(xvar)
  ylab    <- get_var_longname(yvar)
  
  xaxislab <- str_to_sentence(paste0("Country ranked by ", xlab))
  yaxislab <- str_to_sentence(paste0("Country ranked by ", ylab))
  
  year_longname <- ifelse(dynamic, paste0(min(subset_level$year), "-", max(subset_level$year)), plot.year)
  
  
  if(age_type == "adult"){
    sex.longname <- ifelse(my_sex == "female", "women", "men")
  } else{
    sex.longname <- ifelse(my_sex == "female", "girls", "boys")
  }
  
  age_group.longname <- switch(my_age_group,
                               "ageStd" = ifelse(age_type == "adult", "20+ years old", "5-19 years old"),
                               "young" = "20-39 years old",
                               "mid" = "40-64 years old",
                               "old" = "65+ years old")
  
  plot.title <- paste0(str_to_title(sex.longname), ", ", age_group.longname)  
  
  if (dynamic){
    p <- plot_ly(subset_level,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~ranking_xvar,
                 y = ~ranking_yvar, 
                 frame = ~year, 
                 color = ~Superregion,
                 colors = sregion_col,
                 legendgroup = ~Superregion,
                 text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>", str_to_sentence(paste0("Rank ",xlab)), ": ", 201-ranking_xvar,
                                 "<br>", str_to_sentence(xlab), ": ", round(mean_xvar,1), "%",
                                 "<br>", str_to_sentence(paste0("Rank ",ylab)), ": ", 201-ranking_yvar,
                                 "<br>", str_to_sentence(ylab), ": ", round(mean_yvar, 1),"%")) %>%
      animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))
  }
 
  
  p <- p %>% layout(title = plot.title, yaxis = list(title = yaxislab, range = list(0, 201)), 
                    xaxis = list(title = xaxislab, range = list(0, 201)),legend =list(font = list(size = 15)),
                    shapes = list(list(
                    type = "line",
                    x0 = 0,
                    x1 = 201,
                    xref = "x",
                    y0 = 0,
                    y1 = 201,
                    yref = "y",
                    line = list(color = "grey50", dash = "dot"))))
  
  
  
  return(p)
  
  
}


interactive_scatter_timechange_v_timechange <- function(subset, xvar, yvar, my_sex, my_age_group, plot.start.year, plot.end.year){
  # interactive scatter of timechange in two variables against eachother
  
  plot_data <- subset %>%
    filter(age_group == my_age_group) %>%
    filter(variable == xvar | variable == yvar) %>%
    filter(start.year == plot.start.year & end.year == plot.end.year)
  
  
  xmax <- ceiling(max(plot_data$mean[which(plot_data$variable == xvar)])/10)*10
  ymax <- ceiling(max(plot_data$mean[which(plot_data$variable == yvar)])/10)*10
  xmin <- floor(min(plot_data$mean[which(plot_data$variable == xvar)])/10)*10
  ymin <- floor(min(plot_data$mean[which(plot_data$variable == yvar)])/10)*10
  
  
  plot_data <- plot_data %>%
    filter(sex == my_sex) %>%
    select(c(Country, Superregion, age_group, start.year, end.year, variable, iso, Region, mean)) %>%
    filter(variable == xvar | variable == yvar) %>%
    mutate(variable = if_else(variable == xvar, "xvar", if_else(variable == yvar, "yvar", "size_var"))) %>%
    pivot_wider(names_from = variable, values_from = mean)
  
  xlab     <-  paste0("change in ", get_var_longname(xvar))
  ylab     <-  paste0("change in ", get_var_longname(yvar))
  
  xaxislab <- str_to_sentence(paste0(xlab, " ", plot.start.year, "-", plot.end.year, " (percentage points)"))
  yaxislab <- str_to_sentence(paste0(ylab, " ", plot.start.year, "-", plot.end.year, " (percentage points)"))

  if(age_type == "adult"){
    sex.longname <- ifelse(my_sex == "female", "women", "men")
  } else{
    sex.longname <- ifelse(my_sex == "female", "girls", "boys")
  }
  
  age_group.longname <- switch(my_age_group,
                               "ageStd" = ifelse(age_type == "adult", "20+ years old", "5-19 years old"),
                               "young" = "20-39 years old",
                               "mid" = "40-64 years old",
                               "old" = "65+ years old")
  
  plot.title <- paste0(str_to_title(sex.longname), ", ", age_group.longname)
  
  p <- plot_ly(plot_data,
               type = "scatter",
               mode = 'markers',
               hoverinfo="text",
               x = ~xvar,
               y = ~yvar, 
               color = ~Superregion,
               colors = sregion_col,
               text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                               '<br>Superregion: ', Superregion,
                               "<br>", str_to_sentence(xlab), ": ", round(xvar,1),
                               "<br>", str_to_sentence(ylab), ": ", round(yvar,1))) %>%
    layout(title = plot.title, yaxis = list(title = yaxislab, range = list(ymin, ymax)),
                  xaxis = list(title = xaxislab, range = list(xmin, xmax)),legend =list(font = list(size = 15)))
  
  
  return(p)
}



interactive_scatter_timechange_sex_v_sex <- function(subset, xsex, ysex, my_variable, my_age_group, plot.start.year, plot.end.year){
  # interactive scatter of timechange in two variables against eachother
  
  plot_data <- subset %>%
    filter(age_group == my_age_group) %>%
    filter(variable == my_variable) %>%
    filter(start.year == plot.start.year & end.year == plot.end.year)
  
  xmax <- ceiling(max(plot_data$mean)/10)*10
  ymax <- ceiling(max(plot_data$mean)/10)*10
  xmin <- floor(min(plot_data$mean)/10)*10
  ymin <- floor(min(plot_data$mean)/10)*10
  
  plot_data <- plot_data %>%
    select(c(Country, Superregion, sex, age_group, start.year, end.year, variable, iso, Region, mean)) %>%
    mutate(sex = ifelse(sex == ysex, "yvar", "xvar")) %>%
    pivot_wider(names_from = sex, values_from = mean)
  
  if(age_type == "adult"){
    xsex.longname <- ifelse(xsex == "female", "women", "men")
    ysex.longname <- ifelse(ysex == "female", "women", "men")
  } else{
    xsex.longname <- ifelse(xsex == "female", "girls", "boys")
    ysex.longname <- ifelse(ysex == "female", "girls", "boys")
    
  }
  
  xlab     <-  paste0("change in ", get_var_longname(my_variable)," ",  xsex.longname)
  ylab     <-  paste0("change in ", get_var_longname(my_variable), " ", ysex.longname)
  
  xaxislab <- str_to_sentence(paste0(xlab, " ", plot.start.year, "-", plot.end.year, " (percentage points)"))
  yaxislab <- str_to_sentence(paste0(ylab, " ", plot.start.year, "-", plot.end.year, " (percentage points)"))
  
  
  plot.title <- paste0(str_to_sentence(my_age_group), " change in ", get_var_longname(my_variable), " ",plot.start.year, "-", plot.end.year)
  
  p <- plot_ly(plot_data,
               type = "scatter",
               mode = 'markers',
               hoverinfo="text",
               x = ~xvar,
               y = ~yvar, 
               color = ~Superregion,
               colors = sregion_col,
               text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                               '<br>Superregion: ', Superregion,
                               "<br>", str_to_sentence(xlab), ": ", round(xvar,1),
                               "<br>", str_to_sentence(ylab), ": ", round(yvar,1))) %>%
    layout(title = plot.title, yaxis = list(title = yaxislab, range = list(ymin, ymax)),
           xaxis = list(title = xaxislab, range = list(xmin, xmax)),legend =list(font = list(size = 15))) %>%
    
    layout(shapes = list(list(
      type = "line",
      x0 = min(xmin, ymin),
      x1 = max(xmax, ymax),
      xref = "xvar",
      y0 = min(xmin, ymin),
      y1 = max(xmax, ymax),
      yref = "yvar",
      line = list(color = "grey50", dash = "dot")
    )))
  
  
  return(p)
}


interactive_scatter_timechange_v_level <- function(subset_level, subset_timechange, level_var, timechange_var, my_sex, my_age_group, plot.start.year, plot.end.year, level.year, corrAnnotation = F){
  # interactive scatter of timechange from plot.start.year to plot.end.year against level in level.year 
  
  subset_level <- subset_level %>%
    filter(age_group == my_age_group,
           variable == level_var,
           year == level.year) %>%
    dplyr::rename(level = mean) %>%
    select(-c(l,u,se))
  
  subset_timechange <- subset_timechange %>%
    filter(age_group == my_age_group,
           variable == timechange_var,
           start.year == plot.start.year, 
           end.year == plot.end.year) %>%
    dplyr::rename(change = mean) %>%
    select(-c(l,u,se))
  
  plot_data <- left_join(subset_level, subset_timechange, by = c("iso", "sex", "Country", "Region", "Superregion"))
  
  xmax <- ceiling(max(plot_data$level)/10)*10
  xmin <- floor(min(plot_data$level)/10)*10
  ymax <- ceiling(max(plot_data$change)/10)*10
  ymin <- floor(min(plot_data$change)/10)*10
  
  plot_data <- plot_data %>%
    filter(sex == my_sex)
  
  xlab     <-  paste0("level of ", get_var_longname(level_var), " in ", level.year)
  ylab     <-  paste0("change in ", get_var_longname(timechange_var))
  
  xaxislab <- str_to_sentence(paste0(xlab, " (%)"))
  yaxislab <- str_to_sentence(paste0(ylab, " ", plot.start.year, "-", plot.end.year, " (percentage points)"))
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex, " ", plot.start.year, "-", plot.end.year)
  
  p <- plot_ly(plot_data,
               type = "scatter",
               mode = 'markers',
               hoverinfo="text",
               x = ~level,
               y = ~change, 
               color = ~Superregion,
               colors = sregion_col,
               text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                               '<br>Superregion: ', Superregion,
                               "<br>", str_to_sentence(xlab), ": ", round(level,1), "%",
                               "<br>", str_to_sentence(ylab), ": ", round(change,1)," percentage points",
                               "<br>PP increase: ", round(PP.increase*100,1),"%",
                               "<br>PP decrease: ", round(100-PP.increase*100, 1),"%")) %>%
    
    layout(title = plot.title, yaxis = list(title = yaxislab, range = list(min(c(ymin,-xmax)), ymax)),
           xaxis = list(title = xaxislab, range = list(xmin, xmax)),legend =list(font = list(size = 15)))
  
  if(timechange_var == "prev_bmi_l185" | timechange_var == "prev_bmi_neg2sd"){
    p <- p %>%

      layout(shapes = list(list(
        type = "line",
        x0 = 0,
        x1 = xmax,
        xref = "x",
        y0 = 0,
        y1 = -xmax,
        yref = "y",
        line = list(color = "grey50", dash = "dot")
      )))
    }
  
  if(corrAnnotation){
    p <- p %>%
      add_annotations(
        xref = "x", yref = "y",
        x = 20, 
        y = 20, 
        text = paste0("<i>R</i> = ", round(cor(plot_data$level, plot_data$change),2),
                      "<br>",
                      "<i>P</i> = ", formatC(cor.test(plot_data$level, plot_data$change)$p.value,
                                             format="e", digits=2)),
        showarrow = F,   # Does not show an arrow indicating the text position
        align = "left")
  }
  
  return(p)
}


interactive_scatter_level_v_level_comparison <- function(subset, xvar, yvar, my_sex, my_age_group, xyear, yyear){
  # interactive scatter of variable xvar in year xyear (x axis) vs variable yvar in yyear
  
  plot_data <- subset %>%
    filter(sex == my_sex & age_group == my_age_group) %>%
    filter((variable == xvar & year == xyear) | (variable == yvar & year == yyear)) 
  
  xmax <- ceiling(max(plot_data$mean[which(plot_data$variable == xvar & plot_data$year == xyear)])/10)*10
  xmin <- floor(min(plot_data$mean[which(plot_data$variable == xvar & plot_data$year == xyear)])/10)*10
  
  ymax <- ceiling(max(plot_data$mean[which(plot_data$variable == yvar & plot_data$year == yyear)])/10)*10
  ymin <- floor(min(plot_data$mean[which(plot_data$variable == yvar & plot_data$year == yyear)])/10)*10

  plot_data <- plot_data %>%
    filter(sex == my_sex & age_group == my_age_group) %>%
    select(c(Country, Superregion, age_group, year, variable, iso, Region, mean)) %>%
    mutate(cat = ifelse(variable == xvar & year == xyear, "x", "y")) %>%
    select(-c(variable, year)) %>% 
    pivot_wider(names_from = cat, values_from = mean)
  
  xlab     <-  paste0(get_var_longname(xvar), " in ", xyear)
  ylab     <-  paste0(get_var_longname(yvar), " in ", yyear)
  
  xaxislab <- str_to_sentence(paste0(xlab, " (%)"))
  yaxislab <- str_to_sentence(paste0(ylab, " (%)"))
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex)
  
  p <- plot_ly(plot_data,
               type = "scatter",
               mode = 'markers',
               hoverinfo="text",
               x = ~x,
               y = ~y, 
               color = ~Superregion,
               colors = sregion_col,
               text = ~ paste0("Country: ", Country, '<br>Region: ', Region,
                               '<br>Superregion: ', Superregion,
                               "<br>", str_to_sentence(xlab), ": ", round(x,1), "%",
                               "<br>", str_to_sentence(ylab), ": ", round(y,1),"%")) %>%
  
    layout(title = plot.title, 
           yaxis = list(title = yaxislab, range = list(ymin, ymax)),
           xaxis = list(title = xaxislab, range = list(xmin, xmax)),
           legend = list(font = list(size = 15)),
           shapes = list(type='line', line = list(color = "grey", linetype = "dashed"), x0=0, x1=max(xmax, ymax), y0=0, y1=max(xmax,ymax)))
  
  
  return(p)
}



interactive_level <- function(subset, yvar, my_sex, my_age_group, plot.year = NULL, dynamic = FALSE, ordertype = "country", data_type = "level"){
  # if plot.year = FALSE need dynamic = TRUE
  # gives uncertainty and level which changes with time
  # order takes country_order or level
  
  plot_data <- subset %>% 
    filter(sex == my_sex & age_group == my_age_group) %>%
    filter(variable == yvar)
  
  
  if (ordertype == "country") {
    country.list.arranged <- countrylist
    country.list.arranged$Region[country.list.arranged$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
    country.list.arranged$Region[country.list.arranged$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
    country.list.arranged <- country.list.arranged %>%
      arrange(match(Region, region_order), Country) %>%
      mutate(pos = seq(1:length(Country)))
    
    plot_data <- left_join(plot_data, country.list.arranged %>% select(Country,pos))
    
  } else if (ordertype == "level") {
    d_order <- plot_data %>%
      group_by(year) %>%
      arrange(mean)%>%
      mutate(pos = 1:length(Country)) %>%
      ungroup() 
    
    plot_data <- left_join(plot_data, d_order)
  }
  
  ymax <- ceiling(max(plot_data$u/10))*10
  if(data_type == "level"){
    ymin <- 0
  } else if (data_type == "velocity"){
    ymin <- floor(min(plot_data$l/10))*10
  }
  
  if(!(dynamic)){
    plot_data <- plot_data %>% filter(year == plot.year)
  }
  
  ylab <- get_var_longname(yvar)
  if(data_type == "level"){
    yaxislab <- str_to_sentence(paste0(ylab, " (%)"))
  } else if (data_type == "velocity"){
    yaxislab <- str_to_sentence(paste0("Velocity of ", ylab, " (%)"))
  }
  
  xaxislab <- "Country"
  
  year_longname <- ifelse(dynamic, paste0(min(plot_data$year), "-", max(plot_data$year)), plot.year)
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex, " ", year_longname)
  
  
  if (!(dynamic) & ordertype == "country"){
    p <- plot_ly(plot_data %>% filter(year == plot.year),
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~pos,
                 y = ~mean, 
                 size = ~mean,
                 color = ~Region,
                 legendgroup = ~Region,
                 colors = region_col,
                 text = ~ paste0("Country: ", Country, 
                                 '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), ": ", round(mean,1) , "% (", round(l,1),"-", round(u,1),")"))
      add_segments(plot_data %>% filter(year == plot.year),
                   x = ~pos, xend = ~pos, 
                   y = ~l, yend = ~u, 
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F)
  }
  if (dynamic & ordertype == "country"){
    p <- plot_ly(plot_data,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~pos,
                 y = ~mean, 
                 frame = ~year,
                 size  = ~mean,
                 color = ~Region,
                 colors = region_col,
                 text = ~ paste0("Country: ", Country, 
                                 '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), ": ", round(mean,1) , "% (", round(l,1),"-", round(u,1),")")) %>%
      add_segments(plot_data,
                   x = ~pos, xend = ~pos, 
                   y = ~l, yend  = ~u, 
                   frame = ~year, 
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F) %>%
      animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))
  }
  if (!(dynamic) & ordertype == "level"){
    p <- plot_ly(plot_data %>% filter(year == plot.year),
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~pos,
                 y = ~mean, 
                 color = ~Region,
                 legendgroup = ~Region,
                 colors = region_col,
                 text = ~ paste0("Country: ", Country, 
                                '<br>Region: ', Region,
                                '<br>Superregion: ', Superregion,
                                "<br>Rank: ", 201-pos,
                                "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), ": ", round(mean,1) , "% (", round(l,1),"-", round(u,1),")")) %>%
      add_segments(plot_data %>% filter(year == plot.year),
                   x = ~pos, xend = ~pos, 
                   y = ~l, yend = ~u, 
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F)
  }
  if (dynamic & ordertype == "level"){
    p <- plot_ly(plot_data,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~pos,
                 y = ~mean, 
                 frame = ~year,
                 legendgroup = ~Region,
                 color = ~Region,
                 colors = region_col,
                 text = ~ paste0("Country: ", Country, 
                                 '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>Rank: ", 201-pos,
                                 "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), ": ", round(mean,1) , "% (", round(l,1),"-", round(u,1),")")) %>%
      add_segments(plot_data,
                   x = ~ pos, xend = ~pos, 
                   y = ~ l, yend  = ~u, 
                   frame = ~year, 
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F) %>%
      animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="red")))
  }
  
  p <- p %>% layout(title = plot.title, yaxis = list(title = yaxislab, range = list(ymin, ymax)), 
                    xaxis = list(title = xaxislab, showticklabels = F),legend =list(font = list(size = 10)))
  
  
  
  return(p)
  
  
}



interactive_timechange_arrow <- function(subset, yvar, my_sex, my_age_group, start.year, end.year, ordertype = "country", data_type = "level"){
  # gives interactive time change arrow
  # order takes country_order (ordered by country, coloured by region), or level (ordered by level 1990, colored by magnitude of change)
  
  subset$Region[subset$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
  subset$Region[subset$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
  
  plot_data <- subset %>% 
    filter(sex == my_sex & age_group == my_age_group) %>%
    filter(variable ==  yvar) %>%
    filter(year == start.year | year == end.year) %>%
    mutate(year = ifelse(year ==start.year, "start", "end")) %>%
    select(-c(l,u,se))
  
  ymax <- ceiling(max(plot_data$mean/10))*10
  if(data_type == "level"){
    ymin <- 0
  } else if (data_type == "velocity"){
    ymin <- floor(min(plot_data$mean/10))*10
  }
  
  plot_data <- plot_data %>%
    pivot_wider(names_from = year, values_from = mean)
  
  
  if (ordertype == "country") {
    country.list.arranged <- countrylist
    country.list.arranged$Region[country.list.arranged$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
    country.list.arranged$Region[country.list.arranged$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
    country.list.arranged <- country.list.arranged %>%
      arrange(match(Region, region_order), Country) %>%
      mutate(pos = seq(1:length(Country)))
    
    plot_data <- left_join(plot_data, country.list.arranged %>% select(Country,pos))
    
  } else if (ordertype == "level") {
    d_order <- plot_data %>%
      arrange(end)%>%
      mutate(pos = 1:length(Country)) %>%
      ungroup()
    
    plot_data <- left_join(plot_data, d_order)
  }
  
  plot_data <- plot_data %>% mutate(year = end.year)
  
  ylab <- get_var_longname(yvar)
  if(data_type == "level"){
    yaxislab <- str_to_sentence(paste0(ylab, " (percentage points)"))
  } else if (data_type == "velocity"){
    yaxislab <- str_to_sentence(paste0("Velocity of ", ylab, " (Percentage points)"))
  }
  
  xaxislab <- "Country"
  
  year_longname <-  paste0(plot.start.year, "-",end.year)
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex, " ", year_longname)
  
  arrow_size <- (ymax-ymin)/200
  
  if (ordertype == "country"){
    p <- plot_ly(plot_data) %>%
      add_segments(plot_data,
                   x = ~pos, xend = ~pos, 
                   y = ~start, yend = ~end, 
                   hoverinfo = "text",
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   text = ~ paste0("Country: ", Country, 
                                   '<br>Region: ', Region,
                                   '<br>Superregion: ', Superregion,
                                   "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", start.year, ": ", round(start, 2),
                                   "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", end.year, ": ",   round(end, 2),
                                   ifelse(data_type == "level",
                                          paste0("<br>Absolute change ",  start.year, "-", end.year,": ", round(end-start,2), "%", ifelse(data_type == "velocity", "/year", ""),"<br>Relative change ", start.year, "-", end.year,": ", round((end/start-1)*100,2), "%",ifelse(data_type == "velocity", "/year", "")),
                                          ""))) %>%
      add_segments(plot_data,
                   x = ~pos, xend = ~pos, 
                   y = ~end +(start-end)/100, yend = ~end,
                   hoverinfo = "text",
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F,
                   text = ~ paste0("Country: ", Country, 
                                   '<br>Region: ', Region,
                                   '<br>Superregion: ', Superregion,
                                   "<br>",str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", start.year, ": ", round(start, 2),
                                   "<br>",str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", end.year, ": ",   round(end, 2), 
                                   ifelse(data_type == "level",
                                          paste0("<br>Absolute change ",  start.year, "-", end.year,": ", round(end-start,2), "%", ifelse(data_type == "velocity", "/year", ""),"<br>Relative change ", start.year, "-", end.year,": ", round((end/start-1)*100,2), "%",ifelse(data_type == "velocity", "/year", "")),
                                          ""))) %>%
      
      add_segments(plot_data, 
                   x = ~pos-.5, xend = ~pos, 
                   y = ~end - sign(end-start)*arrow_size, yend =  ~end,
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F,
                   hoverinfo = "none")%>%
      
      add_segments(plot_data, 
                   x = ~pos+.5, xend = ~pos, 
                   y = ~end - sign(end-start)*arrow_size, yend =  ~end,
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F,
                   hoverinfo = "none")
      
  }
  
  
  if (ordertype == "level"){
    p <- plot_ly(plot_data) %>%
      add_segments(plot_data,
                   x = ~pos, xend = ~pos, 
                   y = ~start, yend = ~end, 
                   hoverinfo = "text",
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   text = ~ paste0("Country: ", Country, 
                                   '<br>Region: ', Region,
                                   '<br>Superregion: ', Superregion,
                                   "<br>Rank: ",end.year, ": ", 201-pos,
                                   "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", start.year, ": ", round(start, 2),
                                   "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", end.year, ": ",   round(end, 2),
                                   ifelse(data_type == "level",
                                          paste0("<br>Absolute change ",  start.year, "-", end.year,": ", round(end-start,2), "%", ifelse(data_type == "velocity", "/year", ""),ifelse(data_type == "velocity", "/year", "")),
                                          ""))) %>%
      add_segments(plot_data,
                   x = ~pos, xend = ~pos, 
                   y = ~end +(start-end)/100, yend = ~end,
                   hoverinfo = "text",
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F,
                   text = ~ paste0("Country: ", Country, 
                                   '<br>Region: ', Region,
                                   '<br>Superregion: ', Superregion,
                                   "<br>Rank: ",end.year, ": ", 201-pos,
                                   "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", start.year, ": ", round(start, 2),
                                   "<br>", str_to_sentence(paste0(ifelse(data_type == "velocity", "Velocity ", ""),ylab)), " ", end.year, ": ",   round(end, 2), 
                                   ifelse(data_type == "level",
                                          paste0("<br>Absolute change ",  start.year, "-", end.year,": ", round(end-start,2), "%", ifelse(data_type == "velocity", "/year", "")),
                                          ""))) %>%
      
      add_segments(plot_data, 
                   x = ~pos-.5, xend = ~pos, 
                   y = ~end - sign(end-start)*arrow_size, yend =  ~end,
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F,
                   hoverinfo = "none")%>%
      
      add_segments(plot_data, 
                   x = ~pos+.5, xend = ~pos, 
                   y = ~end - sign(end-start)*arrow_size, yend =  ~end,
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F,
                   hoverinfo = "none")
  }
  
  
  p <- p %>% 
    layout(title = plot.title, yaxis = list(title = yaxislab, range = list(ymin, ymax)), 
                    xaxis = list(title = xaxislab,showticklabels = F), legend =list(font = list(size = 10)))
  
  
  
  return(p)
}

interactive_timechange <- function(subset, var, my_sex, my_age_group, plot.start.year, plot.end.year, ordertype = "level"){
  # if plot.year = FALSE need dynamic = TRUE
  # gives uncertainty and level which changes with time
  # order takes country_order or level
  
  subset$Region[subset$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
  subset$Region[subset$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
  
  plot_data <- subset %>% 
    filter(sex == my_sex,
           age_group == my_age_group,
           start.year == plot.start.year,
           end.year == plot.end.year, 
           variable == var)
           
  
  
  if (ordertype == "country") {
    country.list.arranged <- countrylist
    country.list.arranged$Region[country.list.arranged$Region == "North Africa and Middle East"]        <- "Middle East and North Africa"
    country.list.arranged$Region[country.list.arranged$Region == "Southern and Tropical Latin America"] <- "Southern and Latin America"
    country.list.arranged <- country.list.arranged %>%
      arrange(match(Region, region_order), Country) %>%
      mutate(pos = seq(1:length(Country)))
    
    plot_data <- left_join(plot_data, country.list.arranged %>% select(Country,pos))
    
  } else if (ordertype == "level") {
    d_order <- plot_data %>%
      arrange(mean)%>%
      mutate(pos = 1:length(Country)) %>%
      ungroup()
    
    plot_data <- left_join(plot_data, d_order)
  }
  
  ymax <- ceiling(max(plot_data$u/10))*10
  ymin <- floor(min(plot_data$l/10))*10
  
  ylab <- get_var_longname(var)
  yaxislab <- str_to_sentence(paste0("Change in ", ylab," ",plot.start.year, " - ", plot.end.year, " (percentage points)"))
  xaxislab <- "Country"
  
  plot.title <- paste0(str_to_sentence(my_age_group), " ", my_sex)
  
  
  if (ordertype == "country"){
    p <- plot_ly(plot_data %>% filter(year == plot.year),
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~pos,
                 y = ~mean, 
                 size = ~mean,
                 color = ~Region,
                 legendgroup = ~Region,
                 colors = region_col,
                 text = ~ paste0("Country: ", Country, 
                                 '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>", str_to_sentence(paste0("Change in ", ylab," ",plot.start.year, " - ", plot.end.year, ": ", round(mean,1) , "% (", round(l,1),"-", round(u,1),")"))))  %>%
      add_segments(plot_data,
                   x = ~pos, xend = ~pos, 
                   y = ~l, yend = ~u, 
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F)
  }
  
  if (ordertype == "level"){
    p <- plot_ly(plot_data,
                 type = "scatter",
                 mode = 'markers',
                 hoverinfo="text",
                 x = ~pos,
                 y = ~mean, 
                 color = ~Region,
                 legendgroup = ~Region,
                 colors = region_col,
                 text = ~ paste0("Country: ", Country, 
                                 '<br>Region: ', Region,
                                 '<br>Superregion: ', Superregion,
                                 "<br>Rank: ", 201-pos,
                                 "<br>", str_to_sentence(paste0("Change in ", ylab," ",plot.start.year, " - ", plot.end.year, ": ", round(mean,1) , "% (", round(l,1),"-", round(u,1),")")))) %>%
      add_segments(plot_data,
                   x = ~pos, xend = ~pos, 
                   y = ~l, yend = ~u, 
                   legendgroup = ~Region,
                   color = ~Region,
                   colors = region_col,
                   showlegend = F)
  }
  
  p <- p %>% layout(title = plot.title, yaxis = list(title = yaxislab, range = list(ymin, ymax)), 
                    xaxis = list(title = xaxislab, showticklabels = F),legend =list(font = list(size = 10)))
  
  
  
  return(p)
  
  
}
