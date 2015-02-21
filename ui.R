library(shiny)

###
## ShinyUI() provides a browser based interface for the interactions between the
## user and the server.
###
shinyUI(fluidPage(
        titlePanel("Next Word Prediction App", "Samantha Lui's Data Science Capstone Project"),
        sidebarLayout(
                sidebarPanel(
                        h4("This app suggests the next word for you according to
                           the word(s) you entered."),
                        helpText("Please enter a phrase in the box below, 
                                 then press the SUBMIT button. The result, up
                                 to the top 4 suggestions, will be displayed as 
                                 a word cloud on the right with the top suggestion
                                 appearing the biggest.",
                                 style = "color:purple"),
                        
                        textInput('textbox', h5("Input:"), "Hello"),
                        actionButton("submit","Submit"),
                        br(),
                        br(),
                        
                        helpText("Now there are some suggestions for you! To 
                                 continue, please either go on typing in the 
                                 textbox or select an option from the pull-down 
                                 menu and then press the PROCESS button.",
                                 style = "color:purple"),
                        br(),
                        
                        h5(a("Learn more about the Next Word Prediction App",
                             href="http://rpubs.com/Samantha_Lui/57441")),
                        br(),
                        
                        h5("Contact"),
                        a("Email Samantha Lui", href = "mailto:sluimathematics@gmail.com")
                        
                        ),
                
                mainPanel(
                        h4('You entered:'),
                        h5(textOutput("inputValue"), style = "color:blue"),
                        h4('Suggestions for the next word:'), 
                        plotOutput("plot", width="100%", height="250px"),
                        #hr(),
                        hr(),
                        
                        h4("What would you like to do next?"),
                        fluidRow(
                                column(7,uiOutput('columns')),
                                column(1,actionButton("go", "Process"))
                        )
                ))))