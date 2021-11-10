
library(ISLR)
library(ggplot2)
library(plotly)
library(stringr)
library(shinythemes)
library(RWeka)
library(rpart.plot)
library(rJava)
library(corrplot)
library(tidyr)
library(psych)
library(partykit)
library(randomForest)
library(dplyr)
library(caret)

#https://www.kaggle.com/tigganeha4/diabetes-dataset-2019
df.dm2019 <- read.csv("diabetes_dataset__2019.csv",na.strings = c("n/a","NA"," ",""))
df.dm2019$BPLevel <- str_replace_all(df.dm2019$BPLevel, c("High" = "high", "Low" = "low", "normal " = "normal"))
df.dm2019$RegularMedicine[df.dm2019$RegularMedicine=='o'] <- "no"
df.dm2019$Diabetic[df.dm2019$Diabetic==' no'] <- "no"

df.dm2019 <- dplyr::select(df.dm2019, -Pdiabetes)

# Remove missing data
df.dm2019 <- df.dm2019 %>% filter(!is.na(Diabetic))
df.dm2019 <- df.dm2019 %>% drop_na()

#Convert features to factor
df.dm2019$Diabetic <- factor(df.dm2019$Diabetic)
df.dm2019$Age <- factor(df.dm2019$Age, ordered = T, levels = c("less than 40", "40-49", "50-59", "60 or older"))
df.dm2019$Gender <- factor(df.dm2019$Gender)
df.dm2019$Family_Diabetes <- factor(df.dm2019$Family_Diabetes)
df.dm2019$highBP <- factor(df.dm2019$highBP)
df.dm2019$PhysicallyActive <- factor(df.dm2019$PhysicallyActive, ordered = T, levels = c("none", "less than half an hr",
                                                                            "more than half an hr", "one hr or more"))
df.dm2019$Smoking <- factor(df.dm2019$Smoking)
df.dm2019$Alcohol <- factor(df.dm2019$Alcohol)
df.dm2019$RegularMedicine <- factor(df.dm2019$RegularMedicine)
df.dm2019$JunkFood <- factor(df.dm2019$JunkFood, ordered = T, levels = c("occasionally", "often", "very often", "always"))
df.dm2019$Stress <- factor(df.dm2019$Stress, ordered = T, levels = c("not at all", "sometimes", "very often", "always"))
df.dm2019$BPLevel <- factor(df.dm2019$BPLevel, ordered = T, levels = c("low", "normal", "high"))
df.dm2019$UriationFreq <- factor(df.dm2019$UriationFreq, ordered = T, levels = c("not much", "quite often"))

# ## Min-Max Scaling
# process <- preProcess(as.data.frame(df.dm2019), method=c("range"))
# df.dm2019 <- predict(process, as.data.frame(df.dm2019))
# df.dm2019

ybox.vars <- c("BMI", "Sleep", "SoundSleep", "Pregancies")
xbox.vars <- c("Diabetic", "Age", "Gender", "Family_Diabetes", "highBP", "PhysicallyActive", "Smoking", "Alcohol", "RegularMedicine", "JunkFood", "Stress", "BPLevel", "UriationFreq")

countcategory.vars <- c("Diabetic", "Age", "Gender", "Family_Diabetes", "highBP", "PhysicallyActive", "Smoking", "Alcohol", "RegularMedicine", "JunkFood", "Stress", "BPLevel", "UriationFreq")
fill.vars <- c("Diabetic", "Age", "Gender", "Family_Diabetes", "highBP", "PhysicallyActive", "Smoking", "Alcohol", "RegularMedicine", "JunkFood", "Stress", "BPLevel", "UriationFreq")

ypoint.vars <- c("BMI", "Sleep", "SoundSleep", "Pregancies")
xpoint.vars <- c("BMI", "Sleep", "SoundSleep", "Pregancies")

predictor.vars <- c("Age", "Gender", "Family_Diabetes", "BMI", "Sleep", "SoundSleep", "highBP", "PhysicallyActive", "Smoking", "Alcohol", "RegularMedicine", "JunkFood", "Stress", "BPLevel", "UriationFreq", "Pregancies")


ui <- tagList(
    shinythemes::themeSelector(),
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        "Diabetes App",
       
         ## Overview ##
        tabPanel("Overview",
                 sidebarPanel(
                     h4("Dataset Details:"),
                     p("This dataset was collected by Neha Prerna Tigga and Dr. Shruti Garg of the Department of Computer Science and Engineering, BIT Mesra, Ranchi-835215, for research, non-commercial purposes only. This diabetes database was collected as a part of a research project.")
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Data",
                                  tabPanel('Length menu', DT::dataTableOutput('tb'))
                         ),
                         tabPanel("Structure",
                                  verbatimTextOutput("structTable")
                         ),
                         tabPanel("Summary",
                                  verbatimTextOutput("summaryTable")
                         )
                     )
                 )
        )
        ## end ##
        ,
        
        ### Visualization ###
        tabPanel("Visualization", 
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Boxplot",
                                  sidebarPanel(
                                      # Select x axis
                                      selectInput(inputId = "xboxvar", label = strong("X variable:"),
                                                  choices = xbox.vars,
                                                  selected = "Diabetic"),
                                      selectInput(inputId = "yboxvar", label = strong("Y variable:"),
                                                  choices = ybox.vars,
                                                  selected = "BMI")
                                  ),
                                  mainPanel(
                                      h4("Boxplot"),
                                      plotOutput("distPlot")
                                  )
                         ),
                         tabPanel("Barplots", 
                                  sidebarPanel(
                                      # Select x axis
                                      selectInput(inputId = "countcategoryvar", label = strong("Category:"),
                                                  choices = countcategory.vars,
                                                  selected = "Age"),
                                      selectInput(inputId = "fillvar", label = strong("Fill:"),
                                                  choices = fill.vars,
                                                  selected = "Diabetic"),
                                      selectInput(inputId = "positionvar", label = strong("Position:"),
                                                  choices = c("fill", "stack", "dodge"),
                                                  selected = "fill")
                                  ),
                                  mainPanel(
                                      h4("Barplots"),
                                      plotOutput("countPlot")
                                  )
                         ),
                         tabPanel("Scatter plot",
                                  sidebarPanel(
                                      # Select x axis
                                      selectInput(inputId = "xpointvar", label = strong("X variable:"),
                                                  choices = xpoint.vars,
                                                  selected = "BMI"),
                                      selectInput(inputId = "ypointvar", label = strong("Y variable:"),
                                                  choices = ypoint.vars,
                                                  selected = "Sleep")
                                  ),
                                  mainPanel(
                                      h4("Scatter plot"),
                                      plotOutput("pointPlot")
                                  )
                         ),
                         tabPanel("Density plot",
                                  sidebarPanel(
                                      # Select x axis
                                      selectInput(inputId = "xdenvar", label = strong("X variable:"),
                                                  choices = xpoint.vars,
                                                  selected = "BMI"),
                                      selectInput(inputId = "dencolvar", label = strong("Color variable:"),
                                                  choices = countcategory.vars,
                                                  selected = "Diabetic")
                                  ),
                                  mainPanel(
                                      h4("Density plot"),
                                      plotOutput("densityPlot")
                                  )
                         ),
                         tabPanel("Correlation",
                                  
                                  mainPanel(
                                      h4("Correlation"),
                                      plotOutput("corPlot"),
                                      plotOutput("pairPlot")
                                  ) )
                         
                         #Important feature by Random Forest
                          ,
                          tabPanel("Important features",
                          
                                   mainPanel(
                                       h4("Important feature by Random Forest"),
                                       verbatimTextOutput("importantFeature")
                                   ) )
                         
                     )
                 )
        )
        ### end ###
        ,
        
        ## Model creation ##
        tabPanel("Model creation", 
                 sidebarPanel(
                     checkboxGroupInput(inputId = "predictorVar", 
                                        label = "Predictors: ", 
                                        choices=predictor.vars,
                                        selected = c("RegularMedicine", "Age", "Sleep", "BMI", "highBP", "PhysicallyActive", "JunkFood", "Stress", "Family_Diabetes"),
                                        inline = FALSE,
                                        width = NULL),
                     actionButton("createModelBtn", "Create Model", 
                                  class = "btn-primary")
                 ),
                 mainPanel(
                     h4("Model Evaluation"),
                     plotOutput("modelPlot"),
                     verbatimTextOutput("tree_summary"),
                 )
        )
        ## end decision tree c4.5 ##
        
       
        
    )
)

server <- function(input, output) {
    
    df <- df.dm2019
    
    tree_c45_prune <-
        J48(Diabetic ~ RegularMedicine+Age+BMI+Family_Diabetes+Sleep+highBP+PhysicallyActive+JunkFood+Stress,
            data = df, control = Weka_control(R = T))
    # rpart.plot (for plot model)
    tree_rpart <- rpart(Diabetic~RegularMedicine+Age+BMI+Family_Diabetes+Sleep+highBP+PhysicallyActive+JunkFood+
                            Stress, data= df, cp = .02, xval = 10, parms = list(split = "information")) 
    
    output$txtout <- renderText({
        paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$distPlot <- renderPlot({
        pl <- ggplot(df,aes_string(y=input$yboxvar, x=input$xboxvar)) + geom_boxplot(aes_string(fill=input$xboxvar)) + theme_bw()
        pl
    })
    output$countPlot <- renderPlot({
        pcount <- ggplot(df) + geom_bar(aes_string(x = input$countcategoryvar, fill = input$fillvar), position = input$positionvar) + theme_bw()
        pcount
    })
    output$pointPlot <- renderPlot({
        ppoint <- ggplot(data = df,
                         aes_string(x = input$xpointvar, y = input$ypointvar)) +
            geom_point(aes(color=Diabetic)) 
        ppoint
    })
    
    #################Overall###################
    output$tb <- DT::renderDataTable(
        DT::datatable(
            df.dm2019, options = list(
                lengthMenu = list(c(5,10, 15, -1), c('5', '10', '15', 'All')),
                pageLength = 10
            )
        )
    )
    output$densityPlot <- renderPlot({
        p1 <- ggplot(df, aes_string(x=input$xdenvar, color=input$dencolvar)) + geom_density()
        p1
        
    })
    output$catPlot <- renderPlot({
        ExpCatViz(df)
    })
    
    observeEvent(input$createModelBtn, {
        form <- paste('Diabetic~', paste(isolate(input$predictorVar), collapse = '+'))
        tree_c45_prune <- J48(as.formula(form), data = df, control = Weka_control(R = T))
        tree_rpart <- rpart(as.formula(form), data= df, cp = .02, xval = 10, parms = list(split = "information"))
        
        output$modelPlot <- renderPlot({
            #plot(tree_c45_prune)
            rpart.plot(tree_rpart, type=5, extra=101, shadow.col="gray", nn=TRUE)
        })
        
        output$tree_summary <- renderPrint(
            evaluate_Weka_classifier(tree_c45_prune, numFolds = 5, complexity = T,
                                     class = T, seed = 1234)
        )
    })
    output$structTable <- renderPrint(str(df))
    output$summaryTable <- renderPrint(summary(df))
    
    output$choose_x <- renderUI({
        x_choices <- names(df)[!names(df) %in% 'Diabetic']
        checkboxGroupInput('choose_x', label = 'Choose Predictors', choices = x_choices)
    })
    
    output$corPlot <- renderPlot({
        df.corplot <- df.dm2019
        df.corplot[] <- lapply(df.corplot,as.integer)
        corrplot(cor(df.corplot), type="upper", order="hclust", tl.col="black", tl.srt=45)
        #corrplot(cor(df.corplot), type="lower")
    })
    output$pairPlot <- renderPlot({
        pairs.panels(df.dm2019)
    }, height = 800, width = 1200)
    
    #Show important Feature by Random Forest
    tree_RF2 <- randomForest(Diabetic~., data=df, na.action = na.omit, importance=TRUE, ntree=170)
     output$importantFeature <- renderPrint(importance(tree_RF2) )
     
     ##show plot tree
    output$modelPlot <- renderPlot({
        #plot(tree_c45_prune)
        rpart.plot(tree_rpart, type=5, extra=101, shadow.col="gray", nn=TRUE)
    })
    ##show summary of DT
    output$tree_summary <- renderPrint(
        #summary(tree_c45_prune)
        
        evaluate_Weka_classifier(tree_c45_prune, numFolds = 5, complexity = T,
                                 class = T, seed = 1234)
    )
  
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)