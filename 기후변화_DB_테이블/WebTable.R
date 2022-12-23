library(stringr)
library(htmlwidgets)
library(shiny)
library(DT)

data <- read.csv("./data.csv",stringsAsFactors = FALSE)
data$value <- as.numeric(data$value)
data$기준연도 <- as.numeric(data$기준연도)
data <- na.omit(data)
data <- as.tibble(data)
data <- data[data$기준연도 >= 2030,]
category_list <- unique(data$부문)
energy_list <- unique(data$에너지원)


# 테스트
#data <- read.csv("./data.csv",stringsAsFactors = FALSE)
#data$value <- as.numeric(data$value)
#data$기준연도 <- as.numeric(data$기준연도)
#data <- na.omit(data)
#data <- as.tibble(data)
#result <- data %>% filter(부문=='전환') %>% filter(에너지원=='수소')
#result <- result %>% group_by(기준연도,변수) %>% summarise(sum = sum(value)) %>% spread(기준연도,sum,sep='')
# 각 분야별 Rank로 정렬

ui <- fluidPage(
  # App title ----
  titlePanel("혁신기술 DB 비용분석 정보 제공 서비스"),
  title = "혁신기술 DB 비용분석 정보 제공 서비스",
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "category", "부문",  choices=category_list),
      selectInput(inputId = "energy", "에너지원",  choices=energy_list),
      actionButton("do", "비용분석정보 도출")
    ),
    mainPanel(
      output$text,
      tableOutput("r1")
    )
  )
)

server <- function(input, output){
  observeEvent(input$do, {
    output$r1 <- DT::renderDataTable({
      category <- input$category
      energy <- input$energy
      text = paste(category, ' 부문 ', energy,'에 대한 비용분석 정보', sep="");

      result <- data %>% filter(부문==category) %>% filter(에너지원==energy) %>% filter(기술=='수소')
      result <- result %>% group_by(기준연도,변수) %>% summarise(sum = sum(value)) %>% spread(기준연도,sum,sep='')
      DT::datatable(result,caption = text, filter = "none", selection="none")
    })
    output$r1 <- renderTable(iris)
  })
}

shinyApp(ui, server)