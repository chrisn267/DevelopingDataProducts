# app.R

library(shiny)

ui <- fluidPage(
    
    titlePanel("Exploring Sinusoidal Waves"),
    
    sidebarLayout(
        
        sidebarPanel(
            h3("Parameters:"),
            h3(""),
            sliderInput(inputId = "amp", "Amplitude:", min = 0, max = 5, value = 1, step = 0.1),
            sliderInput(inputId = "freq", "Frequency:", min = 0, max = 5, value = 1, step = 0.1),
            sliderInput(inputId = "phase", "Phase Shift:", min = -180, max = 180, value = 0, step = 15),
            sliderInput(inputId = "vertical", "Vertical Shift:", min = -2, max = 2, value = 0, step = 0.1),
            checkboxInput(inputId = "basic", "Show Reference Plot", value = TRUE)
        ),
        
        mainPanel(
    
            plotOutput(outputId = "plot", height = "500px")
        )
    )    
)

server <- function(input, output, session) {
    output$plot <- renderPlot({
        t = seq(-720, 720, 0.01)
        y = input$amp * sin((input$freq * t - input$phase) * 2 * pi / 360) + input$vertical
        z = sin(2 * pi * t / 360)
        plot(t, y, 
            type = 'l', 
            lwd = 4,
            col = 'dodgerblue3',
            xaxp = c(-720, 720, 8),
            xlim = c(-720, 720), 
            ylim = c(-2.5, 2.5),
            xlab = "phase",
            ylab = "amplitude",
            main = "Sinusoidal Wave Plot")
        if(input$basic) {
            lines(t, z, 
                type = 'l',
                lwd = 2,
                col = 'darkgrey')
        }
        abline(v = 0, lwd = 2, col = 'lightgrey')
        abline(h = 0, lwd = 2, col = 'lightgrey')
        abline(v = seq(-720, 720, 180), lwd = 1, lty = 2, col = 'lightgrey')
        abline(h = seq(-2, 2, 1), lwd = 1, lty = 2, col = 'lightgrey')
    })
}

shinyApp(ui = ui, server = server)

