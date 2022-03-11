
library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(shinyjs)

blue <- "#4142e7"
font <- "Press Start 2P"

theme <- bs_theme(
    bg = blue, 
    fg = "white", 
    primary = "white",
    base_font = font_google(font),
    code_font = font_google(font),
    heading_font = font_google(font),
    font_scale = 1.6
)

cor_vals <- function(x) {
    cor_picked <- round(runif(1),2)   
    x <- rnorm(200, 100, 1)
    y <- rnorm(200, cor_picked * x, sqrt(1 - cor_picked^2))
    corr_exact <- round(cor(x,y),2)
    return(list("x"=x, "y"=y, "corr_exact"=corr_exact))
}

# --------- UI --------------------------------------------------------------


ui <- fluidPage(theme = theme, useShinyjs(),
                
    titlePanel(
        h1("The Correlation Game", 
           align = "center",
           style = 'background-color:Tomato;padding:30px;font-size:80px;')
        ),
        h5("Instructions",
           align = "center",
           style = 'padding:25px;')
        ,
    fluidRow(
        p("Guess the correlation between the variables in the plot. The closer you get, the more points you earn. A correct guess is defined as less than .1 from the correct answer. Three wrong guesses and it's GAME OVER!",
            align = "center",
            style = "font-family: 'Press Start 2P';padding-left: 200px;padding-right: 200px;")
        ),
    br(),
    fluidRow(
        column(width=2),
        column(
            width = 7,
            style = "padding-left: 200px;align:'center';",
            plotOutput("plot", width = "99%", height = "800px"),
            h1(textOutput("game_over"),
               align = "center",
               style = 'padding:100px;font-size:120px;'),
            h1(textOutput("retry"),
               align = "center",
               style = 'padding:50px;font-size:40px;'),
            ),
        column(width=3,
               tags$style("#score {font-size:30px;font-family:'Press Start 2P';text-align: center}"),
               h3(icon("heart"),"x",textOutput("lives_left",inline = T), align="center"),
               h5("Your score", 
                   align = "center",
                   style = 'padding:25px;'),
               textOutput("score"),
               h5("Guess",
                  style = 'padding-top:125px;'),
               fluidRow(
                   tags$style("#guess {font-family:'Press Start 2P'}"),
                   numericInput("guess","", value = 0, min = 0, max = 1, step = 0.01)
                    ),
               fluidRow(
                   actionButton("submit", 
                                label = "Submit",
                                style = "font-family:'Press Start 2P';width: 200px;margin-left:12px;margin-top:25px;")
                   ),
               hidden(
                   div(id='correlation_header',
                       textOutput("corr"),
                       style = "padding-top:50px;font-family:'Press Start 2P';"
                    )
                   ),
               div(id = 'correlation_value',
                   textOutput("correct"),
                   style = "padding-top:25px;font-family:'Press Start 2P';"
                   ),
               fluidRow(uiOutput("next_btn"))
               )
        )
)


# --------- SERVER --------------------------------------------------------------

server <- function(input, output) {

    vals = cor_vals() 
    
    rv <- reactiveValues(
        score = 0, 
        lives = 3,
        corr_exact = vals$corr_exact
    )

    output$score <- renderText({rv$score})
    output$lives_left <- renderText({rv$lives})
    
    output$plot <- renderPlot({
        
        df <- tibble(x=vals$x, y=vals$y)
        df %>%
            ggplot(aes(x, y)) +
            geom_point(size = 3.5, alpha = .8, colour = "white") +
            theme(
                plot.background = element_rect(fill = blue, colour = blue),
                panel.background = element_rect(fill = blue, colour = "white", size=2.5),
                axis.title = element_blank(),
                axis.text = element_blank(),
                panel.grid = element_blank()
            )
    })
    
    observeEvent(input$submit, {
        
        hide("submit")

        diff <- abs(rv$corr_exact-input$guess)
        
        if (diff > .1) {
            rv$score = rv$score
            rv$lives = rv$lives - 1

            if (rv$lives < 1) {
                #rv$score = 0
                hide("plot")
                output$game_over <- renderText({"GAME OVER"})
                output$retry <- renderText({"Refresh your browser to try again"})
                
                show("game_over")
            }
            
            
        } else {
            rv$score = rv$score + 100 + (1000 * (.1 - diff))
        }
        
        output$lives_left <- renderText({rv$lives})
        
        print(paste("corr:", rv$corr_exact))
        print(paste("guess:", input$guess))
        print(paste("diff:", diff))
        print(paste("score:",rv$score))
        print(paste("lives:",rv$lives))
        
        output$score   <- renderText({rv$score})
        output$corr    <- renderText({"Correlation"})
        output$correct <- renderText({rv$corr_exact})
        
        show("next_clicked")
        show('correlation_header')
        show('correlation_value')
        
        output$next_btn <- renderUI({
            tagList(
                actionButton("next_clicked", "Next",
                             style="font-family:'Press Start 2P';width: 200px;margin-top:25px;")
                )
            })
        })
        
    observeEvent(input$next_clicked,{

        vals = cor_vals()    
        
        rv$corr_exact = vals$corr_exact
        if (rv$lives < 1) {
            rv$lives = 3
        }
        
        show("submit")
        hide('correlation_header')
        hide("correlation_value")
        hide("next_clicked")

        output$plot <- renderPlot({
            
            df <- tibble(x=vals$x, y=vals$y)
            df %>%
                ggplot(aes(x, y)) +
                geom_point(size = 3.5, alpha = .8, colour = "white") +
                theme(
                    plot.background = element_rect(fill = blue, colour = blue),
                    panel.background = element_rect(fill = blue, colour = "white", size=2.5),
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    panel.grid = element_blank()
                )
        })

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
