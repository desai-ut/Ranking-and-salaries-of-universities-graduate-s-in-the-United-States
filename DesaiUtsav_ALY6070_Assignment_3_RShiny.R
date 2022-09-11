# install.packages(c("shiny", "dplyr","ISLR", "car", "caret", "ggplots", "gridExtra", "pROC", "RColorBrewer", "corrplot", "plotly", "shinycssloaders", "ggiraph", "fmsb", "shinyjs", "tidyr", "tidyverse"), repos = "http://cran.us.r-project.org")

# install.packages('rsconnect')
rsconnect::setAccountInfo(name='rshinydashboard', token='F9CB2B96ABCC8D3E6C4449916E7CDD3D', secret='kCmAD12+9VQA8ExReIkeT3Sn17pxAgvl7rRGtNNi')

library(rsconnect)
library(maps)
library(ggmap)
library(mapproj)
library(shiny)
library(dplyr)
library(ISLR)
library(car)
library(caret)
library(ggplot2)
library(gridExtra)
library(pROC)
library(RColorBrewer)
library(corrplot)
library(plotly)
library(ggiraph)
library(shinycssloaders)
library(fmsb)
library(shinyjs)
library(tidyr)
library(tidyverse)

percent_bar <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings <- subset(UniversityRankings, year == filteryear)
  
  UniversityRankings_Region_Table <- table(UniversityRankings$region)
  UniversityRankings_Region_Table <- sort(UniversityRankings_Region_Table, decreasing = TRUE)
  
  max <- max(UniversityRankings_Region_Table) + 50
  barp <- barplot(UniversityRankings_Region_Table,
                  beside = TRUE,
                  main = "University Count By Region",
                  ylab = "No.of Universities", 
                  col = c("#A4D146", "#B8DC6F", "#CCE698", "#D6EBAD", "#F5FAEB", "#F5FAEB"),
                  border = 0,
                  ylim = c(0, max),
                  cex.names = 0.8)
  text(barp, 
       sort(UniversityRankings_Region_Table, decreasing = TRUE) + 10, 
       sort(UniversityRankings_Region_Table, decreasing = TRUE), 
       font = 1, 
       col =  c("#79A251", "#79A251", "#79A251", "#79A251", "#79A251", "#79A251"))
  print(barp)
}

percent_map <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings <- subset(UniversityRankings, year == filteryear)
  
  WorldData <- map_data('world') %>% fortify
  
  UniversityRankings_Table <- table(UniversityRankings$country)
  U_Map <- data.frame(UniversityRankings_Table, stringsAsFactors = FALSE)
  colnames(U_Map) <- c("country", "value")
  U_Map <- subset(U_Map, value != 0)
  U_Map <- U_Map[order(U_Map$value), ]
  
  p <- ggplot() +
    geom_map(data = WorldData, map = WorldData,
             aes(x = long, y = lat, group = group, map_id=region),
             fill = "white", colour = "#7f7f7f", size=0.5) + 
    geom_map(data = U_Map, map=WorldData,
             aes(fill = value, map_id = country),
             colour = "#7F7F7F", size = 0.5) +
    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
    scale_fill_continuous(low = "#F5FAEB", high = "#6B8E23", guide = "colorbar") +
    scale_y_continuous(breaks = c()) +
    scale_x_continuous(breaks = c()) +
    labs(fill = "legend", title = "Top 150 University Count By Country", x="", y="") +
    theme_bw()
  print(p)
}

percent_donut <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings <- subset(UniversityRankings, year == filteryear)
  
  donut_data <- data.frame(type = c("Public", "Private"), 
                           value = count(UniversityRankings, type)$n) %>%
    mutate(
      percentage = value / sum(value),
      hover_text = paste0(type, ": ", value)
    ) %>%
    mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))
  
  if (donut_data$value[2] > donut_data$value[2]) {
    donut_plot <- ggplot(donut_data, aes(y = value, fill = type)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.1,
        stat = "identity",
        show.legend = TRUE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_data[["percentage_label"]][donut_data[["type"]] == "Public"],
        size = 20,
        color = "#FFBC41"
      ) +
      scale_fill_manual(values = c(Private = "#0067A5", Public = "#FFBC41")) +
      coord_polar(theta = "y") +
      theme_void()
  } else {
    donut_plot <- ggplot(donut_data, aes(y = value, fill = type)) +
      geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.1,
        stat = "identity",
        show.legend = TRUE
      ) +
      annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_data[["percentage_label"]][donut_data[["type"]] == "Private"],
        size = 20,
        color = "#72AAA8"
      ) +
      scale_fill_manual(values = c(Private = "#72AAA8", Public = "#FFBC41")) +
      coord_polar(theta = "y") +
      theme_void()
  }
  
  donut_plot
}

percent_bar_rank <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings <- subset(UniversityRankings, year <= filteryear)
  
  UniversityRankings <-
    UniversityRankings %>%
    group_by(country, year) %>%
    summarize(median_rank = median(rank_display, na.rm = TRUE))
  
  UniversityRankings_2017 <- subset(UniversityRankings, year == 2017, select = c("country", "median_rank"))
  UniversityRankings_2017 <- UniversityRankings_2017[order(UniversityRankings_2017$country), ]
  colnames(UniversityRankings_2017) <- c("country", "median_rank_2017")
  
  UniversityRankings_filteryear <- subset(UniversityRankings, year == filteryear, select = c("country", "median_rank"))
  UniversityRankings_filteryear <- UniversityRankings_filteryear[order(UniversityRankings_filteryear$country), ]
  colnames(UniversityRankings_filteryear) <- c("country", "median_rank_filteryear")
  
  UniversityRankings <- merge(UniversityRankings_2017, UniversityRankings_filteryear, all = TRUE)
  
  if (filteryear == 2017) {
    UniversityRankings <- data.frame(country = UniversityRankings$country) %>%
      mutate(
        change = UniversityRankings$median_rank_filteryear
      )
    
  } else {
    UniversityRankings <- data.frame(country = UniversityRankings$country) %>%
      mutate(
        change = UniversityRankings$median_rank_filteryear - UniversityRankings$median_rank_2017
      )
  }
  
  UniversityRankings <- na.omit(UniversityRankings)
  UniversityRankings <- data.frame(country = UniversityRankings$country,
                                   change = UniversityRankings$change) %>%
    mutate(
      percentage = change / sum(change),
    )
  
  rank_bar <- ggplot(UniversityRankings, aes(x = reorder(country, percentage), y = percentage)) +
    geom_bar(stat = "identity",
             show.legend = FALSE,
             aes(fill = percentage),  # Background color
             color = "gray30") + # Border color
    xlab("Country") +
    ylab("Change in Median Rank") +
    scale_fill_gradient2(low = "#842A25",
                         mid = "#D0E7C9",
                         high = "#568660") +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  print(rank_bar)
}

percent_region_bar_is <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$size <- as.factor(UniversityRankings$size)
  UniversityRankings$international_students <- gsub(",", "", UniversityRankings$international_students)
  UniversityRankings$international_students <- as.numeric(UniversityRankings$international_students)
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings <- subset(UniversityRankings, year == filteryear)
  
  UniversityRankings <- UniversityRankings %>% 
    group_by(region) %>% 
    summarize(mean_international_student = mean(international_students, na.rm = TRUE))
  
  gg <- ggplot(UniversityRankings, 
               aes(y = mean_international_student,
                   x = reorder(region, -mean_international_student),
                   fill = region)) +
    scale_fill_brewer(palette="PrGN") +
    geom_bar(stat="identity", position=position_dodge()) +
    ylab("International Students") + 
    labs(title = "International Students By Region",
         fill = "Region") +
    geom_text(aes(label = round(mean_international_student)),
              position = position_dodge(width = 1),
              vjust = -0.5, size = 2) +
    ylim(c(0, 9000)) +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  print(gg)
}

percent_region__bar_us <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$size <- as.factor(UniversityRankings$size)
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings <- subset(UniversityRankings, year == filteryear)
  
  UniversityRankings <- UniversityRankings %>% 
    group_by(region, size) %>% 
    summarize(size_count = n())
  
  gg <- ggplot(UniversityRankings, 
               aes(y = size_count,
                   x = reorder(region, -size_count),
                   fill = factor(size, level = c('S', 'M', 'L', 'XL')))) +
    scale_fill_manual(values = c("#C7C7FF", "#9999FF", "#6B6BFF", "#3D3DFF")) +
    geom_bar(stat="identity", position=position_dodge()) +
    xlab("University Size") +
    ylab("International Students") + 
    labs(title = "Size-wise University Count By Region",
         fill = "University Size") +
    geom_text(aes(label = size_count),
              position = position_dodge(width = 1),
              vjust = -0.5, size = 2) +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  print(gg)
}

percent_region__bar_ro <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$size <- as.factor(UniversityRankings$size)
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings$research_output <- gsub("Very high", "Very High", UniversityRankings$research_output)
  UniversityRankings <- subset(UniversityRankings, year == filteryear)
  
  UniversityRankings <- UniversityRankings %>% 
    group_by(region, research_output) %>% 
    summarize(ro_count = n())
  
  gg <- ggplot(UniversityRankings, 
               aes(y = ro_count,
                   x = reorder(region, -ro_count),
                   fill = factor(research_output, level = c('Low', 'Medium', 'High', 'Very High')))) +
    scale_fill_manual(values = c("#998766", "#B38F4D", "#CC9633", "#E69E1A")) +
    geom_bar(stat="identity", position=position_dodge()) +
    xlab("University Size") +
    ylab("International Students") + 
    labs(title = "Researchoutput-wise University Count By Region",
         fill = "Research Output") +
    geom_text(aes(label = ro_count),
              position = position_dodge(width = 1),
              vjust = -0.5, size = 2) +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  print(gg)
}

percent_country_bar_sf <- function(filteryear) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$size <- as.factor(UniversityRankings$size)
  UniversityRankings$student_faculty_ratio  <- as.numeric(UniversityRankings$student_faculty_ratio)
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings <- subset(UniversityRankings, year == filteryear)
  
  UniversityRankings <- UniversityRankings %>% 
    group_by(country, region) %>% 
    summarize(mean_student_faculty_ratio  = median(student_faculty_ratio , na.rm = TRUE))
  UniversityRankings$mean_student_faculty_ratio  <- as.numeric(UniversityRankings$mean_student_faculty_ratio)
  
  gg <- ggplot(UniversityRankings, 
               aes(y = mean_student_faculty_ratio,
                   x = reorder(country, -mean_student_faculty_ratio),
                   fill = region)) +
    scale_fill_brewer(palette="PrGN") +
    geom_bar(stat = "identity",
             show.legend = FALSE,
             aes(fill = region),  # Background color
             color = "gray30") + 
    xlab("Country") + 
    ylab("Student Faculty Ratio") + 
    labs(title = "Median Student Faculty Ratio By Country") +
    geom_text(aes(label = mean_student_faculty_ratio),
              position = position_dodge(width = 1),
              vjust = -0.5, size = 2) +
    theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))
  
  print(gg)
}

percent_radar <- function(my_i) {
  UniversityRankings <- read.csv("https://raw.githubusercontent.com/desai-ut/Ranking-and-salaries-of-universities-graduate-s-in-the-United-States/main/UniversityRankings_top500.csv", 
                                 na.strings = "0")
  UniversityRankings$country <- gsub("United States", "USA", UniversityRankings$country)
  UniversityRankings$country <- gsub("United Kingdom", "UK", UniversityRankings$country)
  UniversityRankings_Name <- subset(UniversityRankings, (year == 2017 & rank_display == my_i), select = c("university"))
  UniversityRankings <- subset(UniversityRankings, (university == UniversityRankings_Name$university), select = c("university", "year", "rank_display"))
  
  UniversityRankings <- data.frame(rbind(rep(10, 6), rep(0, 6), 
                                         spread(UniversityRankings, year, rank_display)))
  UniversityRankings <- subset(UniversityRankings, select = -c(university))
  colnames(UniversityRankings) <- c("2017", "2018", "2019", "2020", "2021", "2022")
  Rank <- paste("2017 Rank:", my_i)
  radar <- radarchart(UniversityRankings,
                      cglty = 1, cglcol = "gray",
                      pcol = 4, plwd = 2,
                      pfcol = rgb(0, 0.4, 1, 0.25),
                      title = paste(UniversityRankings_Name$university, Rank, sep="\n"))
  
  print(radar)
}

# Define UI for application that draws a histogram
ui <-
  fluidPage(
    
    navbarPage("",
      tabPanel("Overview",
               fluidRow(column(12, offset = 4, titlePanel("World top-500 University Ranking"))),
               sidebarLayout(
                  fluidRow(
                    column(12, offset = 4,
                    radioButtons("Year1", "Select Year:",
                                 c("2017" = "2017",
                                   "2018" = "2018",
                                   "2019" = "2019",
                                   "2020" = "2020",
                                   "2021" = "2021",
                                   "2022" = "2022"), 
                                 selected = "2022",
                                 inline=T)
                    )),
                  
                  mainPanel(width=12,
                    fluidRow(
                      column(12,
                             column(8, withSpinner(plotOutput("bar"))),
                             column(4, withSpinner(plotOutput("donut")))
                             )
                      ),
                    fluidRow(
                      column(12,
                             column(6, withSpinner(plotOutput("map"))),
                             column(6, withSpinner(plotOutput("bar_rank")))
                             )
                      ),
                    )
                  )
                ),
              tabPanel("Details",
                       sidebarLayout(
                         fluidRow(
                           column(12, offset = 4,
                                  radioButtons("Year2", "Select Year:",
                                               c("2017" = "2017",
                                                 "2018" = "2018",
                                                 "2019" = "2019",
                                                 "2020" = "2020",
                                                 "2021" = "2021",
                                                 "2022" = "2022"), 
                                               selected = "2022",
                                               inline=T)
                           )),
                         
                         mainPanel(width=12,
                                   fluidRow(
                                     column(12,
                                            column(4, withSpinner(plotOutput("region_bar_ro"))),
                                            column(4, withSpinner(plotOutput("region_bar_is"))),
                                            column(4, withSpinner(plotOutput("region_bar_us")))
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                            withSpinner(plotOutput("country_bar_sf")))
                                   ),
                         )
                       )
              ),
              tabPanel("Top-10",
                       fluidRow(column(12, offset = 4, titlePanel("World Top-10 University Ranking"))),
                       mainPanel(width=12,
                                 fluidRow(uiOutput('loopplots'))
                                 )
                       )
      )
    )

server <-
  function(input, output) {
    output$bar <- renderPlot({
      percent_bar(filteryear = input$Year1)
    })
    output$map <- renderPlot({
      percent_map(filteryear = input$Year1)
    })
    output$donut <- renderPlot({
      percent_donut(filteryear = input$Year1)
    })
    output$bar_rank <- renderPlot({
      percent_bar_rank(filteryear = input$Year1)
    })
    output$region_bar_ro <- renderPlot({
      percent_region__bar_ro(filteryear = input$Year2)
    })
    output$region_bar_is <- renderPlot({
      percent_region_bar_is(filteryear = input$Year2)
    })
    output$region_bar_us <- renderPlot({
      percent_region__bar_us(filteryear = input$Year2)
    })
    output$country_bar_sf <- renderPlot({
      percent_country_bar_sf(filteryear = input$Year2)
    })
    output$loopplots <- renderUI({
      plot_output_list <- lapply(1:10, function(i) {
        plotname <- paste0("loopplots", i)
        column(3, withSpinner(plotOutput(plotname)))
      })
      do.call(tagList, plot_output_list)
    })
    for(i in 1:10) {
      local({
        my_i <- i
        plotname <- paste0("loopplots", my_i)
        
        output[[plotname]] <- renderPlot({
          percent_radar(my_i)
        })
      })
    }
  }

shinyApp(ui = ui, server = server)