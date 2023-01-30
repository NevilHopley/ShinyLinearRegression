library(shiny)
library(ggplot2)
library(dplyr) # for case_when function



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Bivariate Data - Sampling from a Population"),
    
    ## Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            wellPanel(
                fluidRow("Population Linear Equation",
                         splitLayout(
                             numericInput(
                                 inputId = "intercept",
                                 label = "Intercept, α",
                                 value = 10,
                                 step = ,
                                 min = 0,
                                 width = "80%"
                             ),
                             numericInput(
                                 inputId = "gradient",
                                 label = "Gradient, β",
                                 value = 5,
                                 step = 0.1,
                                 width = "80%"
                             )
                         )
                ),
                sliderInput(
                    inputId = "domain",
                    label = 'Population Domain',
                    min = 0,
                    max = 20,
                    value = c(3,17)
                ),
                sliderInput(
                    inputId = "scatter",
                    label = "Amount of Scatter",
                    min = 0,
                    max = 3,
                    value = 2,
                    step = 0.1
                ),
                actionButton(
                    inputId = "popn_new",
                    label = "Generate New Population"
                ),
                textOutput("rho",
                           inline = FALSE)
            ),
            sliderInput(
                inputId = "samp_popn_size",
                label = "Sample Size | Population Size",
                min = 10,
                max = 50,
                value = c(10,50),
                step = 5,
                dragRange = FALSE
            ),
            actionButton(
                inputId = "samp_new",
                label = "Select New Sample"
            )
            
        ), # end of side bar panel function
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    )
) # end fluidpage function

# Define server logic required to draw scatterplot with regression line
server <- function(input, output) {
    
    # define reactive values that are triggered by the action buttons being pressed
    popn = reactiveValues(x = NULL, y = NULL)
    samp = reactiveValues(n = NULL, x = NULL, y = NULL)
    
    # generate random x-coordinates for population within domain limits when new population button is pressed, or when population size is changed
    observeEvent(input$popn_new
                 | input$samp_popn_size[2]
                 | input$scatter
                 | input$domain
                 | input$gradient 
                 | input$intercept,{
                     popn$x = runif(
                         min = input$domain[1],
                         max = input$domain[2],
                         n = input$samp_popn_size[2]
                     )
                     
                     # generate random normal residuals
                     residual = rnorm(
                         mean = 0,
                         sd = input$scatter * input$gradient,
                         n = input$samp_popn_size[2]
                     ) 
                     
                     # generate corresponding y-coordinates of fitted values + residuals
                     popn$y = input$gradient * popn$x + input$intercept + residual
                 })
    
    # generate sample from population when new sample button is pressed or when now population button is pressed or when sample size is changed
    observeEvent(input$samp_new
                 | input$popn_new
                 | input$samp_popn_size[1]
                 | input$scatter
                 | input$domain
                 | input$gradient 
                 | input$intercept,{
                     
                     # identify which data points from population will be in the sample
                     samp$n = sample.int(
                         n = input$samp_popn_size[2],
                         size = input$samp_popn_size[1],
                         replace = FALSE
                     )
                     
                     # generate sample of points from population
                     samp$x = popn$x[samp$n]
                     samp$y = popn$y[samp$n]
                 })
    
    # create text to display population rho in sidebar
    output$rho = renderText({
        paste0("Population Correlation, ρ = ",
        round(cor(x = popn$x,
                  y = popn$y),
              2))
    })
    
    output$plot = renderPlot({
        
        popn_df = data.frame(popn$x, popn$y)
        samp_df = data.frame(samp$x, samp$y)
        
        # calculate least squares regression line using the sample data
        samp_model = lm(formula = samp$y ~ samp$x, 
                        data = samp_df)
        
        # prepare caption text about sample least squares regression line equation and r
        samp_equation = paste0(model_equation(samp_model, digits = 2, trim = TRUE),
                               "\n\nPearson's Correlation Coefficient = ",
                               round(cor(x = samp$x,
                                         y=samp$y),
                                     2)
                               )
        
        ggplot() +
            theme_minimal() +
            # plot blue population data points
            geom_point(data = popn_df,
                       mapping = aes(
                           x = popn$x,
                           y = popn$y
                       ),
                       colour = "blue",
                       shape = 4) +
            # overplot red sampled data points
            geom_point(data = samp_df,
                       mapping = aes(
                           x = samp$x,
                           y = samp$y
                       ),
                       colour = "red",
                       shape = 16,
                       size = 2) +
            
            # axes' labels, oritentation and limites
            xlab("x") +
            ylab("y") +
            theme(axis.title.y = element_text(angle = 0,
                                              vjust = 0.5)) +
            lims(x = c(0, 20),
                 y = c(0, roundUpNice(max(popn$y))) ) + 
            
            # add in line for population
            geom_abline(
                intercept = input$intercept,
                slope = input$gradient,
                colour = "blue"
            ) +
            
            # add in least squares regression line from sample
            geom_smooth(data = samp_df,
                        mapping = aes(x = samp$x,
                                      y = samp$y),
                        method = "lm",
                        formula = "y ~ x",
                        colour = "red",
                        se = FALSE
            ) +
            # add in equation of least squares regression line
            labs(caption = samp_equation) + 
            theme(plot.caption = element_text(hjust=0,
                                              colour = "red",
                                              size = 18,
                                              face = "bold"
                                              ))
        # add in equation of population equation
        
    }) # end renderPlot
} # end server function


# FUNCTIONS used in server code, listed above

# roundUpNice function creates nice rounded values for y-axis of scatterplot
roundUpNice <- function(x, nice = c(1,2,4,5,6,8,10)) {
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

# function to extract coefficients from sample's least squares reqression model
# source: https://stats.stackexchange.com/questions/63600/how-to-translate-the-results-from-lm-to-an-equation
model_equation <- function(model, ...) {
    format_args <- list(...)
    model_coeff <- model$coefficients
    format_args$x <- abs(model$coefficients)
    model_coeff_sign <- sign(model_coeff)
    model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                    model_coeff_sign == 1 ~ " + ",
                                    model_coeff_sign == 0 ~ " + ")
    model_eqn <- paste("Sample's Least Squares Regression Equation: y =",
                       paste(if_else(model_coeff[1]<0, "- ", ""),
                             do.call(format, format_args)[1],
                             paste(model_coeff_prefix[-1],
                                   do.call(format, format_args)[-1],
                                   "x",
                                   sep = "", collapse = ""),
                             sep = ""))
    return(model_eqn)
}

# Run the application 
shinyApp(ui = ui, server = server)
