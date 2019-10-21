WV_RD <- readRDS("survey_data.rds")

library(tidyverse)
library(magrittr)
da = WV_RD
da %<>% select(V1:V250)

# all negative value represents NA

da = data.frame(apply(da,MARGIN = 2,function(x){x = ifelse(x<0, NA, x)}))

# Calculate the NA value of each variables
NA_NUM = data.frame(da %>% apply(MARGIN = 2,function(x){round(sum(is.na(x))/length(x),2)}))
NA_NUM["name"] = rownames(NA_NUM)
colnames(NA_NUM) = c("Prop","name")

# most of the Variables miss less than 10%

# delete variables with nore than 10% NA
NA_NUM %<>% filter(Prop<=0.1)

da %<>% select(NA_NUM[,2])

# V1,V2A,V3 are code for questionaire
da %<>% select(-V1,-V2A,-V3)

#delete NA observations
da = na.omit(da)


##########################################################################################
##########################################################################################
library(shiny)

ui <- fluidPage(
  
  titlePanel("Cross Table Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("V1", "Select a Variable as Va1", colnames(da)[2:215]),
      selectInput("V2", "Select a Variable as Va2", colnames(da)[2:215])
    ),
    
    mainPanel(
      tableOutput("table"),
      plotOutput("Plot", click = "plot_click", hover = "plot_hover"),
      verbatimTextOutput("info")
    )
  )
)


server <- function(input, output, session) {
  
  output$table = renderTable({
    data = da[,c(input$V1,input$V2)]
    colnames(data) = c("Va1","Va2")
    cross = xtabs(data = data,~Va1+Va2)
    crossdf = data.frame(cross)
    cross1 =crossdf %>% pivot_wider(names_from = Va1, values_from = Freq)
    ind = unique(data$Va1)
    ind = ind[order(ind)]
    colnames(cross1) = c("Va2",paste("Va1=",ind,sep = ""))
    cross1
  })
  
  output$Plot = renderPlot({
    data = da[,c(input$V1,input$V2)]
    colnames(data) = c("Va1","Va2")
    cross = xtabs(data = data,~Va1+Va2)
    prop.cross = data.frame(prop.table(cross,margin = 1))
    ggplot(prop.cross,aes(y = Freq, x = Va1, fill=Va2)) + geom_col(position = "dodge") + ylab("Proportion")
  })

  
}

shinyApp(ui = ui, server = server)
