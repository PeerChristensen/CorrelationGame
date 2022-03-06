#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(bslib)
library(ggplot2)
library(shinyjs)

#thematic::thematic_shiny(font = "auto")
#p("regular text",style = "font-family: 'Press Start 2P'")
theme <- bs_theme(
    bg = "#4142e7", fg = "white", primary = "white",
    base_font = font_google("Press Start 2P"),
    #base_font = font_google("Electrolize"),
    code_font = font_google("Press Start 2P"),
    heading_font = font_google("Press Start 2P"),
    font_scale=1.6
)

# Define UI for application that draws a histogram
ui <- fluidPage( theme = theme,
    titlePanel(h1("The Correlation Game", align = "center",style='background-color:coral;')),
    fluidRow(h5("Instructions",align="center", style='padding:25px;')),
    fluidRow(p("Guess the correlation between the variables in the plot. The closer you get, the more points you earn. A correct guess is defined as less than 10% deviance from the correct answer. Get three wrong and its game over!",
    align="center",style = "font-family: 'Press Start 2P';padding-left: 200px;padding-right: 200px;")),
    fluidRow(
        column(width=2),
        column(width=7,style="padding-left: 200px;align:'center';",
            plotOutput("plot",width="99%",height="800px")
            ),
        column(width=3,
               tags$style("#score {font-size:25px;font-family:'Press Start 2P';text-align: center}"),
            h5("Your score", align="center",style='padding:25px;'),
            textOutput("score"),
            h5("Guess",style='padding-top:125px;'),
            fluidRow(
                tags$style("#guess {font-family:'Press Start 2P'}"),
                    numericInput("guess","", value = 0, min = 0, max = 1, step = 0.01)
                    ),
            fluidRow(
                actionButton("submit", label = "Submit",
                             style="font-family:'Press Start 2P';width: 200px;margin-left:12px;")
                ),
            hidden(
                div(id='correlation_header',
                    textOutput("corr"),
                    style="padding-top:50px;font-family:'Press Start 2P';"
                    )
                ),
            div(id='correlation_value',
                textOutput("correct"),
                style="padding-top:25px;font-family:'Press Start 2P';"
                ),
            fluidRow(uiOutput("btn"))
        )
)
)


server <- function(input, output) {
    #bs_themer()

    cor_picked <- round(runif(1),2)   
    x = rnorm(200, 100, 1)
    y = rnorm(200, cor_picked*x, sqrt(1-cor_picked^2))
    corr_exact = round(cor(x,y),2)

    #diff <- eventReactive(input$submit,{
    #   round(abs((corr_exact-input$guess)/((corr_exact+input$guess)/2)*100))
    #    })
    
    output$score <- renderText({0})
    
    observeEvent(input$submit,{
        output$correct <- renderText({ corr_exact })
        output$score <- renderText({ 
            round(abs((corr_exact-input$guess)/((corr_exact+input$guess)/2)*100))})
        
        toggle('correlation_header')
        output$corr <- renderText({"Correlation"})
        
        output$btn <- renderUI({
            tagList(
                actionButton("nxt", "Next",
                             style="font-family:'Press Start 2P';width: 200px;margin-top:25px;")

            )
        })
            
        })
    



    
    
    output$plot <- renderPlot({

        df <- tibble(x,y)
        df %>%
            ggplot(aes(x,y)) +
            geom_point(size=2.5, alpha=.8) +
            theme(
                panel.background = element_rect(fill="#4142e7",colour = "white", size=2.5),
                axis.title=element_text(colour="#4142e7"),
                axis.text = element_text(colour="#4142e7")
                )
            })

}

# Run the application 
shinyApp(ui = ui, server = server)
