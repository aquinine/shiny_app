
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(MASS)

max_plots <- 20

shinyServer(function(input, output, session) {
        
        # Return the requested dataset
        datasetInput <- reactive({
                switch(input$dataset,
                       "Aids2" = Aids2,
                       "Boston" = Boston,
                       "Cars93" = Cars93,
                       "Insurance" = Insurance,
                       "Melanoma" = Melanoma,
                       "OME" = OME,
                       "Pima.te" = Pima.te,
                       "Pima.tr" = Pima.tr,
                       "Pima.tr2" = Pima.tr2,
                       "Sitka" = Sitka,
                       "Sitka89" = Sitka89,
                       "Traffic" = Traffic,
                       "UScrime" = UScrime,
                       "VA" = VA,
                       "beav1" = beav1,
                       "beav2" = beav2,
                       "birthwt" = birthwt,
                       "immer" = immer,
                       "npk" = npk,
                       "oats" = oats,
                       "petrol" = petrol,
                       "ships" = ships,
                       "shuttle" = shuttle)
        })
        
        observe({
                nms <- if (is.null(datasetInput())) character(0) else {
                        names(datasetInput())
                }
                stillSelected <- isolate(input$outcome)
                updateSelectInput(session, "outcome", choices = nms,
                                  selected = stillSelected)
        })
        
        #get the dims
        output$dims <- renderPrint({
                dim(datasetInput())
        })
        
        #get the names for the list of outcomes
        output$names <- renderPrint( {
                names(datasetInput())
        })
        
        # Generate a summary of the dataset
        output$summary <- renderPrint({
                summary(datasetInput())
        })
        
        # Show the first "n" observations
        output$view <- renderTable({
                head(datasetInput(), n = input$obs)
        })
        
        #show the pairs plot
        output$pairs <- renderPlot({
                pairs(datasetInput())
        })
        
        #show the covariance of the y and x variables
        output$covars <- renderPrint({
                if (input$outcome != "") {
                        dataset <- datasetInput()
                        x <- dataset[ , -which(names(dataset) %in% c(input$outcome))]
                        y <- dataset[,which(names(dataset) %in% c(input$outcome))]
                        
                        p <- ncol(x)
                        covvalues <- numeric(p)
                        
                        if (is.numeric(y) || is.logical(y)) {
                                for(i in seq_len(p)) {
                                        if (is.numeric(x[,i]) || is.logical(x[,i])) {
                                                covvalues[i] <- cov(y, x[,i])
                                        }
                                        else {
                                                cat(c("Variable", names(x)[i], "is not logical or numerical, setting to NA\n"), sep = " ")
                                                covvalues[i] <- "NA"
                                        }
                                }
                                
                                covarsDF <- data.frame("names"=names(x), "Covariance Values"= covvalues)
                                covarsDF
                        }
                        else{
                                cat(c("Outcome", input$outcome, "is not logical or numerical"), sep = " ")
                        }
                }
                else {
                        "Outcome is not set"
                }
        })
        
        # Show the pvals
        output$pvals <- renderPrint({
                if (input$outcome != "") {
                        dataset <- datasetInput()
                        x <- dataset[ , -which(names(dataset) %in% c(input$outcome))]
                        y <- dataset[,which(names(dataset) %in% c(input$outcome))]
                        if (class(y) == "factor") {
                                print("Cannot do a linear fit with a dependent factor")
                        }
                        else {
                                #print(head(x))
                                p <- ncol(x)
                                pvalues <- numeric(p)
                                for(i in seq_len(p)) {
                                        fit <- lm(y ~ x[, i])
                                        summ <- summary(fit)
                                        pvalues[i] <- summ$coefficients[2,4]
                                }
                                
                                pvalsDF <- data.frame("names"=names(x), "P-Values"= pvalues)
                                pvalsDF
                        }
                }
                else {
                        "Outcome is not set"
                }
        })
        
        # Insert the right number of plot output objects into the web page
        output$plots <- renderUI({
                dataset <- datasetInput()
                x <- dataset[ , -which(names(dataset) %in% c(input$outcome))]
                y <- dataset[,which(names(dataset) %in% c(input$outcome))]
                
                p <- ncol(x)
                plot_output_list <- lapply(1:p, function(i) {
                        plotname <- paste("plot", i, sep="")
                        plotOutput(plotname, height = 280, width = 250)
                })
                
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
        })
        
        plotInput <- reactive({
                dataset <- datasetInput()
                x <- dataset[ , -which(names(dataset) %in% c(input$outcome))]
                y <- dataset[,which(names(dataset) %in% c(input$outcome))]
                
                p <- ncol(x)
                
                n_plot <- p
                #total_data <- lapply(1:n_plot, function(i){x[,i]})
                return (list("n_plot"=n_plot, "y" = y, "x" = x, y_name = input$outcome, 
                             x_names = names(x)))
        })
        
        # Call renderPlot for each one. Plots are only actually generated when they
        # are visible on the web page.
        observe({
                #x_temp <- as.data.frame(plotInput$x)
                lapply(1:plotInput()$n_plot, function(i){
                        output[[paste("plot", i, sep="") ]] <- renderPlot({
                                plot(plotInput()$y ~ plotInput()$x[,i], xlab = plotInput()$x_names[i], ylab = plotInput()$y_name)
                                abline(lm(plotInput()$y ~ plotInput()$x[, i]), col="red")
                        })
                })
        })
        
        # Show the formula
        output$formula <- renderPrint({
                if (input$outcome != "" || input$numPreds != "") {
                        dataset <- datasetInput()
                        x <- dataset[ , -which(names(dataset) %in% c(input$outcome))]
                        y <- dataset[,which(names(dataset) %in% c(input$outcome))]
                        if (ncol(x) >= input$numPreds && input$numPreds > 1) {
                                if (class(y) == "factor") {
                                        print("Cannot do a linear fit with a dependent factor")
                                }
                                else {
                                        #print(head(x))
                                        p <- ncol(x)
                                        pvalues <- numeric(p)
                                        for(i in seq_len(p)) {
                                                fit <- lm(y ~ x[, i])
                                                summ <- summary(fit)
                                                pvalues[i] <- summ$coefficients[2,4]
                                        }
                                        
                                        ord <- order(pvalues)
                                        x_smaller <- x[,ord[1:input$numPreds]]
                                        
                                        formula <- paste(input$outcome, " ~", sep="")
                                        for (i in seq_len(input$numPreds)) {
                                                if (i != ncol(x_smaller)) {
                                                        formula <- paste(formula, paste(names(x_smaller)[i], "+", sep = " "), sep = " ")
                                                }
                                                else {
                                                        formula <- paste(formula, names(x_smaller)[i], sep = " ")
                                                }
                                        }
                                        formula
                                }
                        }
                        else {
                                ""
                        }
                }
                else {
                        ""
                }
        })
        
        # Show the pvals of a formula of values
        output$multiple_pvals <- renderPrint({
                if (input$outcome != "" || input$numPreds != "") {
                        dataset <- datasetInput()
                        x <- dataset[ , -which(names(dataset) %in% c(input$outcome))]
                        y <- dataset[,which(names(dataset) %in% c(input$outcome))]
                        if (ncol(x) >= input$numPreds && input$numPreds > 1) {
                                if (class(y) == "factor") {
                                        print("Cannot do a linear fit with a dependent factor")
                                }
                                else {
                                        #print(head(x))
                                        p <- ncol(x)
                                        pvalues <- numeric(p)
                                        for(i in seq_len(p)) {
                                                fit <- lm(y ~ x[, i])
                                                summ <- summary(fit)
                                                pvalues[i] <- summ$coefficients[2,4]
                                        }
                                        
                                        ord <- order(pvalues)
                                        x_smaller <- x[,ord[1:input$numPreds]]
                                        
                                        formula <- paste(input$outcome, " ~", sep="")
                                        for (i in seq_len(input$numPreds)) {
                                                if (i != ncol(x_smaller)) {
                                                        formula <- paste(formula, paste(names(x_smaller)[i], " + ", sep = ""), sep = "")
                                                }
                                                else {
                                                        formula <- paste(formula, paste(names(x_smaller)[i], sep=""), sep = " ")
                                                }
                                        }
                                        
                                        #print(formula)
                                        
                                        fit <- lm(formula, data=dataset)
                                        summ <- summary(fit)
                                        pvalues <- summ$coefficients
                                        
                                        pvalues
                                }
                        }
                        else {
                                if (input$numPreds > 1) {
                                        cat(c("Number of predictors in larger than the number of variables:", ncol(x)), sep = " ")
                                }
                                else {
                                        "Number of predictors must be larger than 1"
                                }
                        }
                }
                else {
                        if (input$outcome == "") { 
                                "Outcome is not set"
                        }
                        
                        if (input$numPreds == "") {
                                "Number of predictors to use is not set"
                        }
                }
        })
        
        
        markdown_output <- reactive ({
                text <- paste("---\n",
                              "title: \"Exploratory Data Analysis and Preliminary Prediction Analysis of ",
                              input$dataset,
                              "\"\n",
                              "author: \"\"\n",
                              "date: \"",
                              Sys.Date(),
                              "\"\n",
                              "output: html_document\n",
                              "---\n",
                              "```{r}\n",
                              "library(MASS)\n",
                              "d <- ",
                              input$dataset,
                              "\n",
                              "```\n",
                              sep = ""
                )
                
                text <- paste(text,
                              "\n##Introduction\n\n",
                              "This markdown file was created by an automated tool that does basic ",
                              "preliminary analysis of MASS datasets.  This particular analysis was ",
                              "done for the ",
                              input$dataset,
                              "dataset.  More information about this dataset can be done by executing the ",
                              "help command in R.  This section should be removed and replaced with ",
                              "information about the dataset and the analysis you complete with it ",
                              "beyond this preliminary analysis.",
                              sep = ""
                )
                
                text <- paste(text,
                              "\n\n##Exploratory Data Analysis\n\n",
                              "In this section, we present basic information about the ",
                              input$dataset,
                              ".  This dataset has the dimensions of `r dim(d)`.  ",
                              "The names of the columns are: \n",
                              "```{r}\n",
                              "names(d)\n",
                              "```\n",
                              "The summary for the dataset is shown below: \n",
                              "```{r}\n",
                              "summary(d)\n",
                              "```\n",
                              "The first ",
                              input$obs,
                              " are shown below: \n",
                              "```{r}\n",
                              "head(d, n = ",
                              input$obs,
                              ")\n",
                              "```\n",
                              "The pairs plot is shown below: \n",
                              "```{r}\n",
                              "pairs(d)\n",
                              "```\n",
                              sep = ""
                )
                
                text <- paste(text,
                              "\n\n##Preliminary Prediction Analysis\n\n",
                              "In this section we will do preliminary data analysis.  ",
                              "We focus on univariate linear analysis and covarible analysis.  ",
                              "This analysis is done in an automated fashion, although the ",
                              "final predictive analysis is completed using the number of prediction ",
                              "values input by the user.  In this analysis, we use ",
                              input$numPreds,
                              " for this final analysis.\n\n",
                              "The covariable analysis is shown below: \n",
                              "```{r}\n",
                              "x <- d[ , -which(names(d) %in% c(\"",
                              input$outcome,
                              "\"))]\n",
                              "y <- d[,which(names(d) %in% c(\"",
                              input$outcome,
                              "\"))]\n",
                              "\n",
                              "p <- ncol(x)\n",
                              "covvalues <- numeric(p)\n",
                              "\n",
                              "if (is.numeric(y) || is.logical(y)) {\n",
                              "\tfor(i in seq_len(p)) {\n",
                              "\t\tif (is.numeric(x[,i]) || is.logical(x[,i])) {\n",
                              "\t\t\tcovvalues[i] <- cov(y, x[,i])\n",
                              "\t\t}\n",
                              "\t\telse {\n",
                              "\t\t\tcat(c(\"Variable\", names(x)[i], \"is not logical or numerical, setting to NA\\n\"), sep = \" \")\n",
                              "\t\t\tcovvalues[i] <- \"NA\"\n",
                              "\t\t}\n",
                              "\t}\n",
                              "\n",
                              "\tcovarsDF <- data.frame(\"names\"=names(x), \"Covariance Values\"= covvalues)\n",
                              "\tcovarsDF\n",
                              "} else{\n",
                              "\tcat(c(\"Outcome \",\"", 
                              input$outcome,
                              "\", \"is not logical or numerical\"), sep = \" \")\n",
                              "}\n",
                              "```\n\n",
                              sep = ""
                )
                
                text <- paste(text,
                              "Below we show the predicted relationship of each variable with the selected outcome:\n",
                              "```{r}\n",
                              "x <- d[ , -which(names(d) %in% c(\"",
                              input$outcome,
                              "\"))]\n",
                              "y <- d[,which(names(d) %in% c(\"",
                              input$outcome,
                              "\"))]\n",
                              "if (class(y) == \"factor\") {\n",
                              "\tprint(\"Cannot do a linear fit with a dependent factor\")\n",
                              "} else {\n",
                              "\tp <- ncol(x)\n",
                              "\tpvalues <- numeric(p)\n",
                              "\tfor(i in seq_len(p)) {\n",
                              "\t\tfit <- lm(y ~ x[, i])\n",
                              "\t\tsumm <- summary(fit)\n",
                              "\t\tpvalues[i] <- summ$coefficients[2,4]\n",
                              "\t\tplot(y ~ x[, i], xlab=names(x)[i], ylab=\"",
                              input$outcome,
                              "\")\n",
                              "\t\tabline(fit, col=\"red\")\n",
                              "\t}\n",
                              "\n",
                              "\tpvalsDF <- data.frame(\"names\"=names(x), \"P-Values\"= pvalues)\n",
                              "\tpvalsDF\n",
                              "}\n",
                              "```\n",
                              sep = ""
                )
                
                text <- paste(text,
                              "Below we show the predicted relationship with the number of variables chosen by the user.\n",
                              "```{r}\n",
                              "x <- d[ , -which(names(d) %in% c(\"",
                              input$outcome,
                              "\"))]\n",
                              "y <- d[,which(names(d) %in% c(\"",
                              input$outcome,
                              "\"))]\n",
                              "if (ncol(x) >= ",
                              input$numPreds,
                              " && ",
                              input$numPreds,
                              " > 1) {\n",
                              "\tif (class(y) == \"factor\") {\n",
                              "\t\tprint(\"Cannot do a linear fit with a dependent factor\")\n",
                              "\t} else {\n",
                              "\t\tp <- ncol(x)\n",
                              "\t\tpvalues <- numeric(p)\n",
                              "\t\tfor(i in seq_len(p)) {\n",
                              "\t\t\tfit <- lm(y ~ x[, i])\n",
                              "\t\t\tsumm <- summary(fit)\n",
                              "\t\t\tpvalues[i] <- summ$coefficients[2,4]\n",
                              "\t\t}\n",
                              "\n",
                              "\t\tord <- order(pvalues)\n",
                              "\t\tx_smaller <- x[,ord[1:",
                              input$numPreds,
                              "]]\n",
                              "\n",         
                              "\t\tformula <- \"y ~\"\n",
                              "\t\tfor (i in seq_len(",
                              input$numPreds,
                              ")) {\n",
                              "\t\t\tif (i != ncol(x_smaller)) {\n",
                              "\t\t\t\tformula <- paste(formula, paste(names(x_smaller)[i], \"+\", sep = \" \"), sep = \" \")\n",
                              "\t\t\t}\n",
                              "\t\t\telse {\n",
                              "\t\t\t\tformula <- paste(formula, names(x_smaller)[i], sep = \" \")\n",
                              "\t\t\t}\n",
                              "\t\t}\n",
                              "\n",
                              "\t\tfit <- lm(formula, data=x_smaller)\n",
                              "\t\tsumm <- summary(fit)\n",
                              "\t\tpvalues <- summ$coefficients\n",
                              "\n",
                              "\t\tpvalues\n",
                              "\t}\n",
                              "}\n",
                              "```\n",
                              sep = ""
                )
                
                text
                
        })
        
        output$downloadRMd_text <- renderPrint({
                text <- markdown_output()
                print(text)
        })
        
        output$downloadRMd <- downloadHandler(
                filename = function() { 
                        paste(input$dataset, '_analysis', '.Rmd', sep='') 
                },
                content = function(file) {
                        text <- markdown_output()
                        print(text)
                        write(text, file)
                }
        )
        
})
