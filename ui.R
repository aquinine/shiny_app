
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
        
        # Application title
        titlePanel("Automated Exploratory Data Analysis and Preiminary Fits of MASS Datasets"),
        
        # Sidebar with controls to select a dataset and specify the
        # number of observations to view
        sidebarLayout(
                sidebarPanel(
                        helpText("Choose a data set from the pull down list above.",
                                 "The application will not work properly until a",
                                 "dataset is chosen.  Note that not all datasets",
                                 "are chosen because not all of them fit the structure",
                                 "of this type of analysis."),
                        
                        selectInput("dataset", "Choose a dataset:", 
                                    choices = c("Aids2", "beav1", "beav2", "birthwt", "Boston", "Cars93", "immer", 
                                                "Insurance", "Melanoma", "npk", "oats", "OME", "petrol", "Pima.te", 
                                                "Pima.tr", "Pima.tr2", "ships", "shuttle",
                                                "Sitka", "Sitka89", "Traffic", "UScrime", "VA")),
                        
                        helpText("Choose the number of lines to include in the head",
                                 "output below."),
                        
                        numericInput("obs", "Number of observations to view:", 5),
                        
                        helpText("To complete the preliminary prediction, a variable",
                                 "for the outcome needs to be chosen below.  Note",
                                 "that the names of the columns will not populate",
                                 "until a dataset is chosen."),
                        
                        conditionalPanel("input.dataset",
                                         selectInput("outcome", "Choose an outcome", c("Variables"=""), multiple=FALSE)
                        ),
                        
                        helpText("To complete the preliminary prediction with multiple",
                                 "variables the number of predictors needs to chosen below.",
                                 "Note that the names of the columns will not populate",
                                 "until a dataset is chosen and the number must be larger",
                                 "than 1 and smaller than the number of columns."),
                        
                        conditionalPanel("input.dataset", 
                                         numericInput("numPreds", "How many predictors to combine", 2)
                        ),
                        
                        helpText("The information in the two tabs can be downloaded as", 
                                 "a knitr file that can be used in a later report.  The ",
                                 "download will not work correctly in RStudio"),
                        
                        conditionalPanel("input.outcome", 
                                         downloadButton('downloadRMd', 'Download Knitr Output')
                        )
                ),
                
                # Show a summary of the dataset and an HTML table with the 
                # requested number of observations
                mainPanel(
                        tabsetPanel(
                                tabPanel("Exploratory Data Analysis", 
                                         br(),
                                         h2("Dimensions of the dataset"),
                                         verbatimTextOutput("dims"),
                                         h2("Names of the columns"),
                                         verbatimTextOutput("names"),
                                         h2("Dataset Summary"),
                                         verbatimTextOutput("summary"),
                                         h2("Dataset Head"),
                                         tableOutput("view"),
                                         h2("Dataset Pairs Plot"),
                                         plotOutput("pairs")
                                ),
                                tabPanel("Preliminary Fits", 
                                         br(),
                                         h2("Covariance of Variables"),
                                         verbatimTextOutput("covars"),
                                         h2("P-values for each variable vs. outcome"),
                                         verbatimTextOutput("pvals"),
                                         h2("Formula for multiple variables vs. outcome"),
                                         verbatimTextOutput("formula"),
                                         h2("P-values for multiple variables vs. outcome"),
                                         verbatimTextOutput("multiple_pvals")
                                )
                        )
                )
        )
))
