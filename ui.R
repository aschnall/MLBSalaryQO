library(rio)
library(shiny)
library(xml2)
library(shinyMatrix)

#importing data, extracting the salaries, cleaning up data
salaryData <- import('https://questionnaire-148920.appspot.com/swe/data.html', format='html')
salaries <- salaryData$V2
salaries <- gsub(",", "", salaryData$V2)
salaries <- gsub("\\$", "", salaries)
salaries <- as.numeric(salaries)
salaries <- na.omit(salaries)

#sorting salaries in descending order
orderedSalaries <- sort(salaries, decreasing=TRUE)
orderedSalariesM <- as.numeric(orderedSalaries/1000000)
orderedSalariesLen <- length(orderedSalaries)

#storing top 125 salaries into a new vector
#calculating statistics such as mean (qualifying offer), median, and number of players that made more than the QO
topSalaries <- orderedSalaries[1:125]
qo <- mean(topSalaries)
medianSal125 <- median(topSalaries)
topSalariesM <- as.numeric(topSalaries/1000000)
topSalariesLen <- length(topSalaries)
numPlayersAboveQO <- length(salaries[salaries > qo])

#calculating mean and median from list of all salaries
medianSal <- median(salaries)
meanSal <- mean(salaries)

#extracting data of non-minimum salaries and calculating statistics
#more than half the observations are the minimum so it could be insightful to look at the data without them included
nonMinSalaries <- salaries[salaries != 507500]
nonMinMedian <- median(nonMinSalaries)
nonMinMean <- mean(nonMinSalaries)
nonMinSalariesM <- as.numeric(nonMinSalaries/1000000)
nonMinSalariesLen <- length(nonMinSalaries)

#calculating the percentile of the qualifying offer relative to all salaries and non-guaranteed salaries
#citing the following stack overflow link for  assistance with this calculation
#https://stackoverflow.com/questions/34031704/finding-percentile-of-a-particular-input-in-r
qoPercentileAll <- ecdf(salaries)(qo) * 100
qoPercentileNonGTD <- ecdf(nonMinSalaries)(qo) * 100
qoPercentileTop125 <- ecdf(topSalaries)(qo) * 100

#formatting various statistics so they can be presented as currency rather than unformatted numbers
#citing the following stack overflow link as an assistant with the number formatting
#https://stackoverflow.com/questions/23065748/how-to-convert-numeric-data-into-currency-in-r
qoFormat <- paste('$',formatC(qo, big.mark=',', digits=0, format = 'f'))
medianSalFormat <- paste('$',formatC(medianSal, big.mark=',', digits=0, format = 'f'))
meanSalFormat <- paste('$',formatC(meanSal, big.mark=',', digits=0, format = 'f'))
medianSal125Format <- paste('$',formatC(medianSal125, big.mark=',', digits=0, format = 'f'))
nonMinMeanSalFormat <- paste('$',formatC(nonMinMean, big.mark=',', digits=0, format = 'f'))
nonMinMedianSalFormat <- paste('$',formatC(nonMinMedian, big.mark=',', digits=0, format = 'f'))
orderedSalariesLenFormat <- paste('',formatC(orderedSalariesLen, big.mark=',', digits=0, format = 'f'))
qoPercentileAllFormat <- paste('',formatC(qoPercentileAll, big.mark=',', digits=0, format = 'f'))
qoPercentileNonGTDFormat <- paste('',formatC(qoPercentileNonGTD, big.mark=',', digits=0, format = 'f'))
qoPercentileTop125Format <- paste('',formatC(qoPercentileTop125, big.mark=',', digits=0, format = 'f'))


#creating matrix table of calculated statistics to display
salTab <- matrix(c(medianSalFormat, nonMinMedianSalFormat, medianSal125Format, meanSalFormat, nonMinMeanSalFormat, qoFormat, orderedSalariesLenFormat, nonMinSalariesLen, topSalariesLen), 3, 3, dimnames = list(c('All', 'Non-Minimum', 'Top 125'),c('Median','Mean','Players')))

ui <- fluidPage(
  mainPanel(
    
    fluidRow(h1("Qualifying Offer:", qoFormat), align="center"),
    fluidRow(p(strong(numPlayersAboveQO), strong("players"), " had a higher salary than the qualifying offer in 2016. The qualifying offer is in the ", strong(qoPercentileAllFormat), strong("percentile"), " of all MLB salaries, the ", strong(qoPercentileNonGTDFormat), strong("percentile"), " of non-minimum MLB salaries, and the ", strong(qoPercentileTop125Format), strong("percentile"), " of the top 125 MLB salaries in 2016."), align="left"), width = 12),
  
  #inputting matrix table to display additional statistics
  #citing following resource as a help for utilizing matrix tables for shiny apps
  #https://www.inwt-statistics.com/read-blog/shinymatrix-matrix-input-for-shiny-apps.html
  matrixInput(
    "salaryTable",
    value = salTab,
    rows = list(
    ),
    cols = list(
      #names = TRUE
    )
  ),
  plotOutput('allHist'),
  plotOutput('nonMinHist'),
  plotOutput('topHist')
)

#rendering a histogram of salaries to display
#citing the following stack overflow link used to help with rendering histogram in a shiny app
#https://stackoverflow.com/questions/52611963/shiny-r-histogram
server <- function(input, output, session) {
  output$allHist <- renderPlot({
    hist(orderedSalariesM, 
         main="Histogram of All Salaries",
         xlab="Salaries in $M",
    )
  }) 
  
  output$nonMinHist <- renderPlot({
    hist(nonMinSalariesM, 
         main="Histogram of Non-Minimum Salaries",
         xlab="Salaries in $M",
    )
  }) 
  
  output$topHist <- renderPlot({
    hist(topSalariesM, 
         main="Histogram of Top 125 Salaries",
         xlab="Salaries in $M",
    )
  }) 
}
shinyApp(ui, server, options = list(height = 750))

