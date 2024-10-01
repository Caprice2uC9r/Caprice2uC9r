#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)


setwd("~/DATA SCIENCE/Cours R/Dash_board")
data= read.csv("Cancert_2.csv", header=TRUE, sep = ",")

model= readRDS("Cancer_logistique.rds")

# Define UI for application that draws a histogram

ui <- dashboardPage(
  
      dashboardHeader(title= "prédiction_Cancert"),
      
      dashboardSidebar(
        sidebarUserPanel("Auteur : Wilga MBANI",
                         subtitle = a(href = "http://www.facebook.com/ECOSTATmachinelearnia", icon("circle", class = "text-success"), "Online"),
                         # Image file should be in www/ subdir
                         image = "icone.png"
        ),
      
        
        sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
        sidebarMenu(
          # Setting id makes input$tabs give the tabName of currently-selected tab
          id = "tabs",
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                   badgeColor = "green")
          )
        ),

      
      dashboardBody(
        tabName = "features",
        fluidRow(box(valueBoxOutput("cancert_prédiction")),
                 box(numericInput("var1", label = "l'age du patient",
                                  value= 20, min= 15))),
        fluidRow(box(selectInput("var2", label = "sexe du patient",
                                 choices = c("M", "F"))),
                 box(selectInput("var3", label= "Type de Douleur Thoracique",
                                 choices = c("ATA", "NAP", "ASY", "TA")))),
        fluidRow(box(numericInput("var4", label= "Tension au Repos",
                                  value = 100, min= 0)),
                 box(numericInput("var5", label= "cholesterol",
                                  value = 100, min= 0))),
        fluidRow(box(numericInput("var6", label= "Glycemie à jeune (valeur entre 0 et 1)",
                                  value = 0.4, min= 0, max= 1)),
                 box(selectInput("var7", label= "Electrocardiogramme au repos",
                                 choices = c("Normal", "ST", "LVH")))),
        fluidRow(box(numericInput("var8", label= "Frequence cardiaque maximum",
                                  value = 150, min= 60)),
                 box(selectInput("var9", label= "Angine exercice",
                                 choices = c("N", "Y")))),
        fluidRow(box(numericInput("var10", label= "Depression",
                                  value = 2.5, min= -4.0, step= 0.1)),
                 box(selectInput("var11", label= "aspect de l'électrocardiogramme",
                                 choices = c("Up", "Flat", "Down"))))
        
        
      )
      
      
)


server <- function(input, output) {
  prediction= reactive({
    predict(
      model,
      data.frame(
        "Age"= input$var1,          
        "Sexe"= input$var2,            
        "TypeDouleurThoracique"= input$var3, 
        "TensionRepos"= input$var4,    
        "Cholesterol"= input$var5,     
        "GlycemieAJeun"= input$var6,  
        "ECGRepos"= input$var7,        
        "FreqCardiaqueMax"= input$var8,
        "AngineExercice"= input$var9,  
        "DepressionST"= input$var10,    
        "PenteST"= input$var11       
      ),
      
      type= "link"
    )
    
  })
  prediction_label= reactive({
    ifelse(prediction()<= 0.50, "Non-Malade", "Malade")
  })
  
  
  


  prediction_prob= reactive({
    predict(
      model,
      data.frame(
        "Age"= input$var1,          
        "Sexe"= input$var2,            
        "TypeDouleurThoracique"= input$var3, 
        "TensionRepos"= input$var4,    
        "Cholesterol"= input$var5,     
        "GlycemieAJeun"= input$var6,  
        "ECGRepos"= input$var7,        
        "FreqCardiaqueMax"= input$var8,
        "AngineExercice"= input$var9,  
        "DepressionST"= input$var10,    
        "PenteST"= input$var11       
      ),
      
      type= "response"
      
    )
    
    
  })
  
  prediction_color= reactive({
    ifelse(prediction()<= 0.50, "green", "red")
  })
  

  
  output$cancert_prédiction= renderValueBox({
    
    valueBox(
      
      value = paste(round(100 * prediction_prob(),0),"%"),
      subtitle = prediction_label(),
      color = prediction_color()
     
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
