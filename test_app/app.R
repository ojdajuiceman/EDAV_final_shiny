#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

# Prepare 3 dataframes for agency spending:
# 1) as % of outlays (default)
# 2) as % of GDP
# 3) in 2009 dollars

# Method that will transpose data:
transpose_df <- function(df) {
  tmp <- df
  # Remove commas from all columns:
  tmp <- data.frame(lapply(tmp, function(x) { gsub(",", "", x) }))
  tmp <- as.tibble(tmp)
  # Transpose logic
  tmp <- as_tibble(cbind(nms = names(tmp), t(tmp)))
  colnames(tmp) <- tmp[1, ] 
  tmp <- tmp[-1, ] 
  # Remove the X from your first column
  #tmp$X1 <- gsub("X", "", tmp$X1)
  tmp[[1]] <- gsub("X", "", tmp[[1]])
  # Give each column a unique name
  x <- colnames(tmp)
  y <- as.character(1:length(x))
  z <- paste0(x, "_", y)
  colnames(tmp) <- z
  colnames(tmp)[1] <- 'year'
  #Convert remaining columns to numeric. 
  tmp <- tmp %>% mutate_each(funs(as.numeric), -year)
  tmp <- tmp %>% filter(year != "TQ")
  return(tmp)
}

# Download data and choose agencies:
tt_4_2 <- transpose_df(read_csv("tt_4_2_copy.csv", col_names = TRUE, skip = 1))
tmp <- tt_4_2 %>% select("year",
                         "Legislative Branch" = "Legislative Branch_2",
                         "Judicial Branch" = "Judicial Branch_3",
                         "Agriculture" = "Department of Agriculture_4",
                         "Commerce" = "Department of Commerce_5",
                         "Defense" = "Department of Defense--Military Programs_6",
                         "Education" = "Department of Education_7",
                         "Energy" = "Department of Energy_8",
                         "Health and Human Services" = "Department of Health and Human Services_9",
                         "Homeland Security" = "Department of Homeland Security_10",
                         "Housing UD" = "Department of Housing and Urban Development_11",
                         "Interior" = "Department of the Interior_12",
                         "Justice" = "Department of Justice_13",
                         "Department of Labor" = "Department of Labor_14",
                         "State Department" = "Department of State_15",
                         "Transportation" = "Department of Transportation_16",
                         "Treasury" = "Department of the Treasury_17",
                         "Veterans Affairs" = "Department of Veterans Affairs_18",
                         "Corps of Engineers" = "Corps of Engineers--Civil Works_19",
                         "Other Defense Civil Programs" = "Other Defense Civil Programs_20",
                         "EPA" = "Environmental Protection Agency_21",
                         "Executive Office" = "Executive Office of the President_22",
                         "General Services Administration" = "General Services Administration_23",
                         "International Assistance" = "International Assistance Programs_24",
                         "NASA" = "National Aeronautics and Space Administration_25",
                         "NSF" = "National Science Foundation_26",
                         "Personnel Management" = "Office of Personnel Management_27",
                         "Small Business" = "Small Business Administration_28",
                         "Social Security" = "Social Security Administration (Off-Budget)_30",
                         "Infrastructure" = "Infrastructure Initiative_31",
                         "Total outlay" = "Total outlays_38"
)
party <- read_csv('control_party_copy.csv', col_names = TRUE)
party$year <- as.character(party$year)
agencies_of_interest <- c("year","Defense","Social Security","Health and Human Services","Treasury","Department of Labor","Transportation")

# Prepare dataframes for plotting:
# First dataframe (note: no longer selecting only agencies of interest):
shin1 <- tmp %>% inner_join(party, by = "year") %>% select(-Senate, -House) %>% mutate(pres_party = as.factor(pres_party))
shin1$year <- as.Date(paste0(shin1$year, "-01-01"), "%Y-%m-%d")
shin1

# Tidy the data (gather, remove bad data, convert characters to date):
shin1 <- shin1 %>% gather(key = "Agency", value = "Percent", -year) %>% filter(!grepl("estimate",year)) %>% filter(Agency != "pres_party") %>% mutate(Percent = as.numeric(Percent))

tmp2 <-tmp %>% inner_join(party, by = "year") %>% select(-Senate, -House)
tmp2$year <- as.Date(paste0(tmp2$year, "-01-01"), "%Y-%m-%d")
tmp2
# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
  titlePanel("Distribution of US Federal Spending Across Agencies"),
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      # Slider for year
      sliderInput(inputId = "year",
                  label = "Select Year:",
                  min = as.Date("1962-01-01","%Y-%m-%d"),
                  max = as.Date("2016-01-01","%Y-%m-%d"),
                  value = as.Date("1962-01-01") , timeFormat="%Y"),
      
      checkboxGroupInput(inputId = "checkAgencies", 
                         label = "Agencies", 
                         choices = list(
                           "Legislative Branch" = "Legislative Branch", 
                           "Judicial Branch" = "Judicial Branch", 
                           "Agriculture" = "Agriculture", 
                           "Commerce" = "Commerce", 
                           "Defense" = "Defense", 
                           "Education" = "Education", 
                           "Energy" = "Energy", 
                           "Health and Human Services" = "Health and Human Services", 
                           "Homeland Security" = "Homeland Security", 
                           "Housing and Urban Dev." = "Housing UD", 
                           "Interior" = "Interior", 
                           "Justice" = "Justice", 
                           "Department of Labor" = "Department of Labor", 
                           "State Department" = "State Department", 
                           "Transportation" = "Transportation", 
                           "Treasury" = "Treasury", 
                           "Veterans Affairs" = "Veterans Affairs", 
                           "Corps of Engineers" = "Corps of Engineers", 
                           #"Other Defense Civil Programs" = "Other Defense Civil Programs", 
                           "EPA" = "EPA", 
                           "Executive Office" = "Executive Office", 
                           "General Services Administration" = "General Services Administration", 
                           "International Assistance" = "International Assistance", 
                           "NASA" = "NASA", 
                           "National Science Foundation" = "NSF", 
                           "Personnel Management" = "Personnel Management", 
                           "Small Business" = "Small Business", 
                           "Social Security" = "Social Security", 
                           "Infrastructure" = "Infrastructure"
                         ),
                         selected = c("Treasury", "Defense", "Social Security", "Health and Human Services")
      )
    ),
    mainPanel(
      plotOutput(outputId = "barChart"),
      br(),
      plotOutput(outputId = "barChart2"),
      helpText("Note: For the first figure, data is displayed only for the selected year. For the second figure, data is displayed for all years including the selected year through present day. For example, for the second plot, selecting 1980 will produce an average of all years between 1980 and 2016, inclusive.")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$barChart <- renderPlot({
    # Barchart
    to_plot <- shin1 %>% 
      filter(format(year, "%Y") == format(input$year, "%Y")) %>% 
      filter(Agency %in% input$checkAgencies) %>%
      select(-year)
    
    ggplot(to_plot, aes(x = Agency, y = Percent)) + geom_col() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 50) +
      xlab("Federal Agency") +
      ylab("Percent of Federal Spending") +
      ggtitle("Percent of Federal Spending DURING Selected Year, by Agency") 
  })
  
  output$barChart2 <- renderPlot({
    # Barchart
    zz_late <- tmp2 %>% 
      filter(format(year, "%Y") >= format(input$year, "%Y")) %>% 
      select( append(input$checkAgencies,  c("year","pres_party") )) %>%
      #select(append(c("Treasury", "Defense", "EPA", "Social Security", "Education"), c("year","pres_party"))) %>%
      select(-year) %>%
      group_by(pres_party) %>% 
      summarize_all(funs(mean)) %>% 
      gather(key = "Agency", value = "Mean_Allocation", -pres_party) %>%
      filter(Mean_Allocation < 99)
    
    ggplot(zz_late, aes(Agency, Mean_Allocation)) +   
      geom_bar(aes(fill = pres_party), position = "dodge", stat="identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("Federal Agency") +
      ylab("Average Percent of Federal Spending") +
      ggtitle("Average Percent of Federal Spending Between Selected Year and 2016, \nby Agency and Party") +
      scale_fill_manual(values=c("#56B4E9","#FF0000"))
  })  

}

# Run the application 
shinyApp(ui = ui, server = server)

