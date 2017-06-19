library(shiny)
ob_patients<- read.csv("ob_patients.csv")
dim(ob_patients)
EIB = subset(ob_patients, gdelta_FEV == 'mild' | gdelta_FEV == 'moderate' | gdelta_FEV == 'servere' )
dim(EIB)
########### Four Groups
EIB$HxEIBAsthma = "Excl"
EIB$HxEIBAsthma[EIB$HxEIB == 1 & EIB$HxAsthma == 1 ] = 0
EIB$HxEIBAsthma[EIB$HxEIB == 0 & EIB$HxAsthma == 0 ] = 1
EIB$HxEIBAsthma[EIB$HxEIB == 1 & EIB$HxAsthma == 0 ] = 2
EIB$HxEIBAsthma[EIB$HxEIB == 0 & EIB$HxAsthma == 1 ] =3
EIB$HxEIBAsthma<- factor(EIB$HxEIBAsthma, levels=0:3)
levels(EIB$HxEIBAsthma)<-c("Asthma and EIB", "None","EIB only","Asthma only")
summary(EIB$HxEIBAsthma)
table(EIB$HxEIBAsthma)
interactive()
sums<- c(9,12,5,10,9,9,9,12)
k<- matrix(sums, nrow=2, byrow= TRUE)
colnames(k)<- c("None","History of asthma","History of EIB","History of both")
rownames(k)<- c("Males","Females")

ui<- fluidPage(
  titlePanel("Asthma and EIB analysis"),
  sidebarLayout(sidebarPanel(
  selectInput("Factors", "Choose one", (choose= c("Gender", "History of EIB","Race", "Asthma Severity"))),
  selectInput("Factors2", "Choose one", (choose= c("History of Asthma", "History of EIB", "History of Asthma and EIB")))),
  mainPanel(selectInput("Fact","Choose one",(choose= rownames(k))))),


  uiOutput("ui1"),
  plotOutput("variables", click="plot_click"),
  verbatimTextOutput("info"),
  verbatimTextOutput("lol")
)
server<- function(input,output){
  EIB$Gender<- 0
  EIB$Gender[EIB$gender=="female"]= 1
  EIB$HxEIBAsthma
  sum(EIB$HxEIBAsthma== "None"& EIB$Gender==0 )
  sum(EIB$HxEIBAsthma== "Asthma only"& EIB$Gender==0 )
  sum(EIB$HxEIBAsthma== "EIB only"& EIB$Gender==0 )
  sum(EIB$HxEIBAsthma== "Asthma and EIB"& EIB$Gender==0 )
  sum(EIB$HxEIBAsthma== "None"& EIB$Gender==1 )
  sum(EIB$HxEIBAsthma== "Asthma only"& EIB$Gender==1 )
  sum(EIB$HxEIBAsthma== "EIB only"& EIB$Gender==1 )
  sum(EIB$HxEIBAsthma== "Asthma and EIB"& EIB$Gender==1 )
  w<- reactive(input$Factors)
  x<- reactive(input$Factors2) 
  
  
  output$variables<- renderPlot({barplot(k[input$Fact, ], beside= TRUE, width= .2, col= c(79,115,100,56), ylim= c(0,18))})
  output$info<- renderText({paste(k[input$Fact,], input$Fact)})
  
  output$lol<- renderText({
    n<-EIB$Race
    o<- EIB$HxEIBAsthma
    {if (w()== "Gender"){n<- EIB$gender}
      else if (w()== "Race"){n<- EIB$Race}
      else if (w()== "History of EIB"){n<- EIB$HxEIB}
      else if (w()== "Asthma Severity"){n<- EIB$Asthma.Severity.1}}
    {if (x()== "History of Asthma and EIB"){o<- EIB$HxEIBAsthma}
      else if (x()== "History of Asthma"){o<- EIB$HxAsthma}
      else if (x()== "History of EIB"){o<- EIB$HxEIB}}
    results<- chisq.test(n,o)
    print(results)
    paste("p-value:",results$p.value)
  })
}
shinyApp(ui<-ui, server<-server)
