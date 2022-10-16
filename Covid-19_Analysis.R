library(jsonlite)
library(dplyr)
library(data.table)
library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinythemes)
library(shinyjs)
library(forecast)
library("TTR") 
library(stringr)

base_stats = fread('covid19/base_stats.csv')
base_stats2 = base_stats
base_stats_temp = base_stats[-c(1)]
base_stats_temp$daily_inc = diff(base_stats$Confirmed)

new_dates = strptime(as.character(base_stats$Dates), "%m/%d/%Y")
base_stats$Dates = format(new_dates, "%Y-%m-%d")
base_stats$Dates = as.Date(str_replace(base_stats$Dates, '00', '20'))

base_stats2$Dates = as.Date(base_stats2$Dates,format="%m/%d/%Y")
base_stats2$week = format(base_stats2$Dates, "%y-%W")
base_stats2$month = format(base_stats2$Dates, "%y-%m")

graphList = c("active", "confirmed", "deaths", "recovered")

ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  useShinyjs(),
  navbarPage(
    theme = "cerulean",  
    "COVID-19 Aanalysis",
    tabPanel("Data Exploration",
      sidebarLayout(
        sidebarPanel(
          pickerInput(
            inputId = "graphOptions", 
            label = "Select graph:",
            choices = graphList,
            selected = "active",
            options = list('actions-box' = T),
            multiple = F
          ),
          submitButton(
            text = "Submit", 
            icon = icon("sync")
          ),
          width = 2
        ),
        mainPanel(
          h4("Covid19 Treemap of Countries The Treemap shows the number of Cases in Different coutries and their percent of total cases worldwide"),
          plotlyOutput("distPlot"),
          h4("Country wise Visualization on a map"),
          plotlyOutput("distPlot6"),
          h4("Cases over time across the Globe (Simple Moving Averages)"),
          plotlyOutput("distPlot4"),
          h4("Daily/Weekly/Monthly Statistics"),
          plotlyOutput("distPlot5"),
          width = 10
        )
      )
    ),
    tabPanel("Forecast",
             mainPanel(
               plotOutput("distPlot2"),
               plotOutput("distPlot3"),
               width = 10
             )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$graphOptions, {
    if (is.null(input$graphOptions)) {
      updatePickerInput(
        session,
        inputId = "graphOptions",
        selected = graphList
      )
    }
  })

  observeEvent(input$graphOptions, {
    output$distPlot2 <- renderPlot({
      base_stats_ts = ts(data = base_stats_temp$daily_inc, frequency = 365, start = c(2020, 1, 22), end = c(2022, 5, 11))
      HW1 = HoltWinters(base_stats_ts, seasonal='multiplicative')
      HW1_for = forecast(HW1, h=90, level=0.95)
      lines(HW1_for$fitted, lty=2, col="blue")
      plot(HW1_for, ylab="Daily Increase")
      legend("topleft",lty=1, pch=1, col=c('black', 'blue', 'dark blue'), c("data","Holt Winters"))
    })
    
    sub_data <- base_stats_temp$Active
    sub_data2 <- base_stats$Active
    sub_data3 <- base_stats2$Active
    distPlot5Label <- 'Active'
    graphAtt <- 'multiplicative'
    if (input$graphOptions == 'confirmed') {
      sub_data <- base_stats_temp$Confirmed
      sub_data2 <- base_stats$Confirmed
      sub_data3 <- base_stats2$Confirmed
      distPlot5Label <- 'Confirmed'
    } else if (input$graphOptions == 'deaths') {
      sub_data <- base_stats_temp$Deaths
      sub_data2 <- base_stats$Deaths
      sub_data3 <- base_stats2$Deaths
      distPlot5Label <- 'Deaths'
    } else if (input$graphOptions == 'recovered') {
      sub_data <- base_stats_temp$Recovered
      sub_data2 <- base_stats$Recovered
      sub_data3 <- base_stats2$Recovered
      graphAtt <- 'additive' 
      distPlot5Label <- 'Recovered'
    }
    
    output$distPlot3 <- renderPlot({
      base_stats_ts = ts(data = sub_data, frequency = 365, start = c(2020, 1, 22), end = c(2022, 5, 11))
      HW2 = HoltWinters(base_stats_ts, seasonal=graphAtt)
      HW2_for = forecast(HW2, h=90, level=0.95)
      plot(HW2_for, ylab=input$graphOptions)
      lines(HW2_for$fitted, lty=2, col="blue")
      legend("topleft",lty=1, pch=1, col=c('black', 'blue', 'dark blue'), c("data","Holt Winters"))
    })
    
    print(input$graphOptions)
    output$distPlot <- renderPlotly({
      case_type = input$graphOptions
      file_path = sprintf("covid19/%s_country.csv",case_type)
      covid_country = read.csv(file_path)
      covid_cases = select(covid_country,Country.Region,c(ncol(covid_country)))
      colnames(covid_cases)[2] = 'cases'
      
      plot_ly(
        data = covid_cases,
        type= "treemap",
        values = ~cases,
        labels= ~Country.Region,
        parents=  "",
        domain = list(column=0),
        name = "Confirmed",
        textinfo="percent root+label+value+text") %>%
        layout()
    })
    output$distPlot4 <- renderPlotly({
      trace1 = list(
        line = list(
          dash = "dash", 
          color = "rgba(55, 128, 191, 1.0)"
        ), 
        mode = "lines", 
        name = "7 Day MA", 
        text = "", 
        type = "scatter", 
        x=as.Date(base_stats$Dates),
        y=runMean(sub_data2,7)
      )
      
      trace2 = list(
        name = "Original Data", 
        text = "", 
        type = "bar", 
        #color = "rgba(55, 128, 191, 1.0)",
        x=as.Date(base_stats$Dates),
        y=sub_data2
      )
      
      layout = list(
        legend = list(
          font = list(color = "#4D5663"), 
          bgcolor = "#F5F6F9"
        ), 
        xaxis1 = list(
          title = "", 
          showgrid = TRUE, 
          tickfont = list(color = "#4D5663"), 
          gridcolor = "#E1E5ED", 
          title = list(color = "#4D5663"), 
          zerolinecolor = "#E1E5ED"
        ), 
        yaxis1 = list(
          title = "", 
          showgrid = TRUE, 
          tickfont = list(color = "#4D5663"), 
          gridcolor = "#E1E5ED", 
          title = list(color = "#4D5663"), 
          zerolinecolor = "#E1E5ED"
        ), 
        title = list(color = "#4D5663"), 
        plot_bgcolor = "#F5F6F9", 
        paper_bgcolor = "#F5F6F9"
      )
      p = plot_ly()
      p = add_trace(p, line=trace1$line, mode=trace1$mode, name=trace1$name, text=trace1$text, type=trace1$type, x=trace1$x, y=trace1$y)
      p = add_trace(p, line=trace2$line, mode=trace2$mode, name=trace2$name, text=trace2$text, type=trace2$type, x=trace2$x, y=trace2$y)
      p = layout(p, title=layout$title, legend=layout$legend, xaxis1=layout$xaxis1, yaxis1=layout$yaxis1, titlefont=layout$titlefont, plot_bgcolor=layout$plot_bgcolor, paper_bgcolor=layout$paper_bgcolor)
    })
    
    output$distPlot5 <- renderPlotly({
      daily = setNames(select(base_stats2, Dates, distPlot5Label),c("date","count"))
      weekly = setNames(aggregate(sub_data3, list(base_stats2$week), sum),c("week","count"))
      monthly = setNames(aggregate(sub_data3, list(base_stats2$month), sum),c("month","count"))
      
      fig1 <- plot_ly(daily, type = 'bar', text= ~paste('Day: ', daily$date,'Count:', count))%>%
        add_trace(x=1:nrow(daily), y = ~count, name = 'Daily')%>%
        layout(showlegend = F) 
      
      fig2 <- plot_ly(weekly, type = 'bar', text= ~paste('Day: ', weekly$week,'Count:', count))%>%
        add_trace(x=1:nrow(weekly), y = ~count, name = 'Weekly')%>%
        layout(showlegend = F) 
      
      fig3 <- plot_ly(monthly, type = 'bar', text= ~paste('Day: ', monthly$month,'Count:', count))%>%
        add_trace(x=1:nrow(monthly), y = ~count, name = 'Monthly')%>%
        layout(showlegend = F) 
      
      fig <- subplot(fig1, fig2, fig3)%>% 
       layout(xaxis = list(zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
               yaxis = list(zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
               plot_bgcolor='#e5ecf6')
    })
    
    output$distPlot6 <- renderPlotly({
      case_type = input$graphOptions
      file_path = sprintf("covid19/%s_country.csv",case_type)
      covid_country = read.csv(file_path)
      covid_cases = select(covid_country,Country.Region,c(ncol(covid_country)))
      colnames(covid_cases)[2] = 'cases'
      
      fig <- plot_ly(covid_cases, 
                     type='choropleth', 
                     locationmode='country names', 
                     locations=covid_cases$Country.Region, 
                     z=covid_cases$cases, 
                     text=covid_cases$Country.Region,
                     colorscale="mercator")
    })
  })
}

shinyApp(ui = ui, server = server)