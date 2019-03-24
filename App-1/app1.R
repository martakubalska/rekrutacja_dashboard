library(shiny)
library(shinydashboard)
library(plotly)
library(MASS)
library(DT)
library(dplyr)
library(scales)


setwd("C:\\Users\\mkuba\\Desktop\\Payback")
simpson <- read.csv("simpson.csv", header = TRUE)[,-1]
example <- read.csv2("simpson_example.csv", header = TRUE)
example <- data.frame(example)
example$rownum <- c("Total", "Group1", "Group2", "Group3")
example <-example %>% as_tibble() %>% tibble::column_to_rownames("rownum")
full <- read.csv("full.csv", header = TRUE)[,-1]
statistics <- read.csv("stats.csv", header = TRUE)[,-1]
stats <- read.csv("stats2.csv", header = TRUE)[,-1]

ui <- dashboardPage(
  dashboardHeader(title = "PAYBACK dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Correlation", tabName="correlation", icon = icon("chart-line")),
      menuItem("Simpson's Paradox", tabName="simpson", icon = icon("layer-group")),
      menuItem("Analysis", tabName='analysis', icon = icon("chart-bar")),
      menuItem("Description", tabName = 'descr', icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "correlation",
    
              fluidRow(
                 box(width = 12, height = 700, status = "primary",
                   plotOutput("scatter1")
                   )
                )
                 ,
                 box(
                     title = "Controls", status = "primary", solidHeader = TRUE,
                     sliderInput("slider","Number of observations:",10,100,80,10)
                  ),
              box(
                title = "Controls", status = "primary", solidHeader = TRUE,
                sliderInput("slider2","Correlation coefficient:",-1,1,0.95, 0.05)
              )
    )
  ,
  
  tabItem(tabName = "simpson",
          fluidRow(column(width = 9,
            box(width = 12, height = 600, status = "danger", solidHeader = TRUE, title = "Simppson's paradox",
              plotOutput("scatter2")
            ),
            box(width = 12, title = "Example", status = "primary", solidHeader = TRUE,
                dataTableOutput('table'))
          ),
            column(width=3,
            box(width = 12, title = "Scatter plot controls", status = "danger", solidHeader = TRUE,
              radioButtons("groups_smp", "Correlation",
                           choices = list("In groups" = "1", "Combined" = "2"),selected = "1")),
              
            box(width = 12, title = "Pie chart controls", status = "primary", solidHeader = TRUE,
                radioButtons("pie", "Proportions", choices = list("Control" = 1, "Test" = 2))),
            
            
            box(width =12, title = "Proportions", status = "primary", solidHeader = TRUE,
                plotOutput("pie1"))
              
            )
          )),
  tabItem(tabName = "analysis", fluidRow(
    column( width = 6,
          box(width = 12, solidHeader = TRUE, status = "danger", title = "Amounts of purchases made",
              plotOutput("hist1")
            ),
          box(width = 6, height = 420, status = "danger", solidHeader = TRUE, title = "Response rate in groups",
              plotOutput("bar1")
          ),

          
          box( width = 6, height = 420 ,solidHeader = TRUE, status = "danger", title = "Average amount spent per client",
            plotOutput("colchart"))
            
          ),
    column(width = 6,
           
           
           infoBoxOutput(width = 4, "testBox5"),
           infoBoxOutput(width = 4,"testBox1"),
           infoBoxOutput("testBox6", width = 4),
           
           box(width = 4, status = "danger", solidHeader = TRUE, title = "Density plot controls",
               radioButtons("groups_hist", "Distribution of amount of purchase in groups",
                            choices = list("Control" = "1", "Sent" = "2", "Both" = "3"),selected = "1")
           ),
          
           

          box(width = 12, title = "Statistisc for 1. January 2018 - 14. January 2018", status = "primary", solidHeader = TRUE,
            dataTableOutput('table2')),
          
          infoBoxOutput("testBox3", width = 4),
          infoBoxOutput("testBox4", width = 4),
          infoBoxOutput("testBox2", width = 4)
    )
  )
    
  ),
  
  tabItem(tabName = "descr",
          fluidRow(
            column(width = 6,
                   box(width = 12, title = "Report preparation steps", status = "danger", solidHeader = TRUE, 
                       HTML("<font size=4><p>1. Upload the data to RStudio.</p>
                                          <p>2. Change the column <i>data_zakupu</i> type to <i>date</i> and filter only
                                              observations within the analysed period (1 February 2018 - 14 February 2018).</p>
                                          <p>3. Left join <i>history</i> table to <i>transactions</i> table on <i>id_klienta</i>.</p>
                                          <p>4. For clients who did not make any purchase insert 0 in the column <i>kwota_zakupu</i>.</p>
                                          <p>5. Create analogical columns as in points 2.-4. for period 1 January 2018 - 14 January 2018.</p>
                                          <p>6. If a client made more than 1 purchase in one of periods, sum them to receive one row 
                                                per client and total amount spent by one client.</p>
                                          <p>7. Calculate basic statistics in grups created by variable <i>grupa</i> - response rate, mean and median amounts spent
                                              in total and only for clients, who made a purchase (for February data).</p>
                                          <p>8. Use Wilcoxon (Mann-Whitney) and Chi2 tests to test whether the differences 
                                                between groups are statistically significat.</p>
                                          <p>9. Check if the groups are homogenous regarding purchases made in January - calculate
                                                basic statistics and test whether differences between groups are statistically significant.</p>
                                          <p>10. Prepare visualizations.</p></font>")
                       
                   )),
                   column(width = 6,
                          box(width = 12, title = "Possible additional reports", status = "danger", solidHeader = TRUE,
                              HTML("<font size=4><p>1. Using additional characteristics of clients divide them into groups
                                                    and check their response to the campaign within these groups.</p>
                                                <p>2. Using clients' transactions history for the same period of year 2017
                                                      compare their buying behaviour before Valentine's Day.</p>
                                                <p>3. Using data about products purchased by clients in this period analyse if
                                                      the marketing campaign increased sales of a particular category of products.</p</font>")
                            
                          ),
                          box(width = 12, title = "Source code", status = "primary", solidHeader = TRUE, 
                              h2(a(href="https://github.com/martakubalska/rekrutacja_dashboard", "github.com/martakubalska/rekrutacja_dashboard")))
                   )
                   
            
          )
          
  )

  )
  )
)


server <- function(input, output) { 
  output$scatter1 <- renderPlot({
    
    r = input$slider2
    
    corr = mvrnorm(n=100, mu=c(10, 10), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
    data <- corr[seq_len(input$slider),]
    data <- data.frame(data)
    
    ggplot(data, aes(x=X1, y=X2)) + geom_point(size = 5, color = '#CF1B39') + theme(legend.position="none") + 
      geom_text(x = 7.7, y=12, label=r, size = 15, color = "gray40") + geom_smooth(method=lm, se=FALSE, color = "gray40") +
      xlim(7.5,12.5) + ylim(7.5,12.5)
    
    
  }, height = 670)
  
  output$scatter2 <- renderPlot({
    
    if (input$groups_smp == "1"){
    ggplot(simpson, aes(x=X1, y=X2, color = group, shape = group)) + geom_point(size = 4) + 
      geom_smooth(method=lm, se=FALSE, color = "black") + scale_color_manual(values=c('#CF1B39', '#0F5CA5', "gray50")) + 
      theme(legend.position="none") } else{
        
        ggplot(simpson, aes(x=X1, y=X2)) + geom_point(size = 4) + 
          geom_smooth(method=lm, se=FALSE, color = "black") + scale_color_manual(values=c('#CF1B39', '#0F5CA5', 'gray50')) + 
          geom_smooth(aes(color = group), method=lm, se=FALSE) + theme(legend.position="none")
        
      }
    
  }, height = 520, width = 1130)
  

  output$table <- DT::renderDataTable(example, options = list(searching = FALSE, paging = FALSE, rownames = TRUE))
  
  output$pie1 <- renderPlot({
    data <- example[2:4,]
    Group <- rownames(data)
    data <- cbind(Group, data.frame(data, row.names=NULL))
    v <- as.integer(input$pie) + 1
    
    ggplot(data, aes(x="", y=data[,v], fill=Group)) + geom_bar(width = 1, stat = "identity") + theme_minimal() +
      coord_polar("y", start=0) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position = "bottom") + 
      scale_fill_manual(values=c('#CF1B39', '#0F5CA5', 'gray50'))
  })
  
  output$bar1 <- renderPlot({
    ggplot(full, aes(grupa, fill = zakup)) + geom_bar(position = "fill") + 
      scale_y_continuous(limits = c(0.0, 0.5),oob = rescale_none) + scale_fill_manual(values=c('#CF1B39', '#0F5CA5')) + 
      theme(axis.title.x=element_blank()) + 
      geom_text(x = 1, y=0.07, label="13.7%", size = 7, color = "white") + 
      geom_text(x = 2, y=0.09, label="16.9%", size = 7, color = "white") + guides(fill=guide_legend(title="Purchase")) +
      labs(y ="Response rate")
  }, height = 350)
  
  output$testBox1 <- renderInfoBox({
    infoBox(
      "Proportions test", "p-value  = 0.242", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  output$hist1 <- renderPlot({
  
    if (input$groups_hist == "1") {
      
      df_plot <- full %>% filter(kwota_zakupu > 0) %>% filter(grupa == "Control")
      
      ggplot(df_plot, aes(x=kwota_zakupu, fill=grupa, color=grupa)) + 
        geom_density(alpha=0.2) + scale_fill_manual(values=c('#CF1B39', '#0F5CA5')) +
        scale_color_manual(values=c('#CF1B39', '#0F5CA5')) +
        geom_vline(aes(xintercept = 84.9), color="#CF1B39", size = 1) + 
        geom_vline(aes(xintercept=91), color="#CF1B39", linetype="dashed", size =1) +
        labs(x = "Amount of purchase", y ="Density") + theme(legend.position = "none") + 
        geom_text(x = 72, y=0.004, label="mean = 84,9", size = 7, color = "black") + 
        geom_text(x = 105, y=0.003, label="median = 91", size = 7, color = "black")
      
      
    } else if (input$groups_hist == "2") {
      
      df_plot <- full %>% filter(kwota_zakupu > 0) %>% filter(grupa == "Sent")
      
      ggplot(df_plot, aes(x=kwota_zakupu, fill=grupa, color=grupa)) + 
        geom_density(alpha=0.2) + scale_fill_manual(values=c('#0F5CA5', '#CF1B39')) +
        scale_color_manual(values=c('#0F5CA5', '#CF1B39')) +
        geom_vline(aes(xintercept = 92.1), color="#0F5CA5", size = 1) + 
        geom_vline(aes(xintercept=96.8), color="#0F5CA5", linetype="dashed", size = 1) + 
        labs(x = "Amount of purchase", y ="Density") + theme(legend.position = "none")+ 
        geom_text(x = 67, y=0.004, label="mean = 92.1", size = 7, color = "black") + 
        geom_text(x = 127, y=0.003, label="median = 96.8", size = 7, color = "black")
      
    } else {
    
      df_plot <- full %>% filter(kwota_zakupu > 0)
    
    ggplot(df_plot, aes(x=kwota_zakupu, fill=grupa, color=grupa)) + 
      geom_density(alpha=0.2) + scale_fill_manual(values=c('#CF1B39', '#0F5CA5')) +
      geom_vline(aes(xintercept = 84.9), color="#CF1B39", size = 1) + 
      geom_vline(aes(xintercept=91), color="#CF1B39", linetype="dashed", size =1) +
      geom_vline(aes(xintercept = 92,1), color="#0F5CA5", size = 1) + 
      geom_vline(aes(xintercept=96.8), color="#0F5CA5", linetype="dashed", size = 1) + 
      labs(x = "Amount of purchase", y ="Density") }
  
    
    })
  
  output$testBox2 <- renderInfoBox({
    infoBox(
      HTML("Average total </br> amount test"), "p-value  = 0.864", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  
  
  output$testBox6 <- renderInfoBox({
    infoBox(
      HTML("Average total </br> amount test"), "p-value  = 0.182", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  output$testBox3 <- renderInfoBox({
    infoBox(
      HTML("Average purchase </br> amount test"), "p-value  = 0.556", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  
  output$testBox4 <- renderInfoBox({
    infoBox(
      "Proportions test", "p-value  = 0.985", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue", fill = TRUE
    )
  })
  
  output$testBox5 <- renderInfoBox({
    infoBox(
      HTML("Average purchase </br>amount test"), "p-value  = 0.423", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  output$colchart <- renderPlot({
    
    ggplot(stats, aes(x = grupa, y = avg_total, fill = grupa)) + geom_col() + 
      labs(x = "Group", y = "Average amount spent by client") + scale_fill_manual(values=c('#CF1B39', '#0F5CA5')) + 
      theme(legend.position = "none")
  }, height = 350)
  
  output$table2 <- DT::renderDataTable(statistics, options = list(searching = FALSE, paging = FALSE, rownames = TRUE))
  
  }

shinyApp(ui, server)