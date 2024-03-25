
library(shiny);library(shinythemes)

ui <- fluidPage(theme=shinytheme("flatly"),

    titlePanel("No Reflow Risk Score for Acute Coronary Syndromes"),
                
    sidebarLayout(
        sidebarPanel(
            radioButtons("flow","Coronary Flow Pre-PCI",choices=c("TIMI 3 (0 points)"=0,"TIMI 2 (3 points)"=1.436,"TIMI 0 or 1 (4 points)"=2.021)),
            radioButtons("diameter","Vessel diameter (mm)",choices=c(">2.5mm (0 points)"=0,"2.5mm (1 points)"=0.513,"<2.5mm (3 points)"=1.345)),
                        radioButtons("length","Estimated stent length (mm)",choices=c("<20mm (0 points)"=0,"≥20mm (2 points)"=1.004)),
                                    radioButtons("shock","Cardiogenic shock",choices=c("No (0 points)"=0,"Yes (2 points)"=0.833)),
                                                radioButtons("graft","Left main or bypass graft PCI",choices=c("No (0 points)"=0,"Yes (2 points)"=0.992)),
                                                            radioButtons("stemi","STEMI and >195 minutes since symptom onset",choices=c("No (0 points)"=0,"Yes (1 points)"=0.482))
        ),

        mainPanel(htmlOutput("pred1"),
                  uiOutput("risk_panel")
        )
    ),
    
    fluidRow(column(width=12,align="left","The No-Reflow Risk Score was developed using data from the Melbourne Interventional Group registry (Australia) and externally validated using data from the British Cardiovascular Intervention Society (BCIS) registry (United Kingdom). The output % estimates above are derived from the regression coefficients for the variables selected in the final regression model in the MIG cohort. Discrimination in the development cohort was excellent in the development cohort (C-statistic 0.81) and good in the validation cohort (C-statistic 0.75). The score is designed for use following angiography and prior to percutaneous coronary intervention (PCI) to assist in risk stratification of patients and guide future trials for preventative treatments. Further details can be found in the original publication at https://www.ahajournals.org/doi/10.1161/CIRCINTERVENTIONS.123.013738."))
)

server <- shinyServer(function(input, output) {
    
    linpred <- reactive({
        (-5.637 +
             as.numeric(input$flow) +
             as.numeric(input$diameter) +
             as.numeric(input$length) +
             as.numeric(input$shock) +
             as.numeric(input$graft) +
             as.numeric(input$stemi))
    })
    
    p1 <- reactive({
        round((plogis(linpred())*100), digits = 1)
    })
    
    output$pred1 <- renderUI({
        HTML(paste("<b><font size='8'>Risk of No Reflow =<font/></b>", p1(), "%","</font>"))
    })
    
    output$risk_panel <- renderUI({
        tags$div(class = "panel",
                 tags$h5("Low Risk", style = ifelse(p1() <= 5, "font-size: 60px; color:green;", "display:none")),
                 tags$h5("Moderate Risk", style = ifelse(p1() > 5 && p1() <= 10, "font-size: 60px; color:orange;", "display:none")),
                 tags$h5("High Risk", style = ifelse(p1() > 10, "font-size: 60px; color:red;", "display:none"))
        )
    })
    
})

shinyApp(ui = ui, server = server)
