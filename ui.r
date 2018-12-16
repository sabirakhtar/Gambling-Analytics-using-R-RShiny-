ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Bet Exchange"),
                    
                    dashboardSidebar(width = 350,
                                     tags$head(
                                       tags$style(HTML("
                                                       .sidebar { height: 90vh; overflow-y: auto; }
                                                       
                                                       " ))),
                                     
                                     sidebarMenu(
                                       menuItem("Data", tabName = "data", icon = icon("dashboard")),
                                       menuItem("Menu", tabName = "Files", icon=icon("scale", lib = 'glyphicon'),
                                                menuItem("Demographic Insight ", tabName = "da1", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Activity by Gender", tabName = "da2", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Gambling Frequency of Users", tabName = "da3", icon=icon("triangle-right", lib = 'glyphicon'),
                                                         menuItem("For Betting ", tabName = "da4", icon=icon("globe", lib = 'glyphicon')),
                                                         menuItem("For Poker ", tabName = "da5", icon=icon("globe", lib = 'glyphicon'))),
                                                menuItem("Top 5 Applications", tabName = "da7", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Top 10 Players ", tabName = "da8", icon=icon("triangle-right", lib = 'glyphicon')),
                                                menuItem("Length of Subscription", tabName = "da6", icon=icon("triangle-right", lib = 'glyphicon')))
                                       
                                
                                     
                                     
                                       )),
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName="data",
                                tabsetPanel(
                                  tabPanel("Data Files",
                                           wellPanel(div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
                                           ))
                                  
                                )),
                        
                        # FirST tab content
                        
                        tabItem(tabName = "da4",
                                # FirST tab content
                                tabPanel("Product/User",
                                         wellPanel(fluidRow(plotlyOutput("graph1")))
 
                                )
                        ),
                        tabItem(tabName = "da2",
                                tabPanel("one",
                                         wellPanel(fluidRow(plotlyOutput("graph3")))

                                )
                        ),
                        tabItem(tabName = "da1",
                                tabPanel("one",
                                         wellPanel(fluidRow(plotlyOutput("graph5")))
                                         
                                )
                        ),
                        tabItem(tabName = "da6",
                                tabPanel("one",
                                         wellPanel(fluidRow(plotlyOutput("graph6")))
                                         
                                )
                        ),
                        
                        tabItem(tabName = "da7",
                                tabPanel("one",
                                         wellPanel(fluidRow(plotOutput("graph7")))

                                )

                        ),
                        tabItem(tabName = "da4",
                                # FirST tab content
                                
                                tabPanel("one",
                                         wellPanel(fluidRow(plotlyOutput("graph10")))
                                )
                        ),
                        tabItem(tabName = "da8",
                                # FirST tab content
                                
                                tabPanel("one",
                                         wellPanel(fluidRow(DT::dataTableOutput("graph8")))
                                )
                        ),
                        tabItem(tabName = "da5",
                                
                                tabPanel("one",
                                         wellPanel(fluidRow(plotlyOutput("graph2")))
                                
                                )
                        ))
                      
                    ))
