library(shiny)
library(plotly)
library(ggplot2)


##### SERVER #####

# Define server logic for random distribution application
server <- function(input, output) {
    
    #Generate values
    BF = function(p,r,n,y) {
        bscall = ((pi*p*r^4)/(8*n*y))
        bsput = ((pi*p*r^4)/(8*n*y))
        res = c(bscall,bsput)
    }
    
    #Pressure value
    output$BFcall <- renderText({ 
        #Get inputs
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        res = round(BF(p,r,n,y)[1],4)
    })
    
    #Radius Value
    output$BFput <- renderText({ 
        #Get inputs
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        res = round(BF(p,r,n,y)[2],4)
    })
    
    #Viscosity Value
    output$BFvisc <- renderText({ 
        #Get inputs
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        res = round(BF(p,r,n,y)[3],4)
    })
    
    #Length Value
    output$BFlength <- renderText({ 
        #Get inputs
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        res = round(BF(p,r,n,y)[4],4)
    })
    
    #Pressure plot
    output$plotCall <- renderPlot({
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        bloodflow = NULL; vput = NULL
        pressure = seq(0.1,100)
        for (p in pressure) {
           bloodflow = c(bloodflow,BF(p,r,n,y)[1])
            vput = c(vput,BF(p,r,n,y)[2])
        }
        df = data.frame(pressure,bloodflow)
        ggplot(df,aes(x=pressure,y=bloodflow)) + geom_point(color="blue")
    }, height = 350, width = 600)
    
    #Radius plot
    output$plotPut <- renderPlot({
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        bloodflow = NULL; vput = NULL
        radius = seq(1,6)
        for (r in radius) {
            bloodflow = c(bloodflow,BF(p,r,n,y)[1])
            vput = c(vput,BF(p,r,n,y)[2])
        }
        df = data.frame(radius,bloodflow)
        ggplot(df,aes(x=radius,y=bloodflow)) + geom_point(color="blue")
    }, height = 350, width = 600)
    
    #Visc plot
    output$plotVisc <- renderPlot({
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        bloodflow = NULL; vput = NULL
        viscosity = seq(1,10)
        for (n in viscosity) {
            bloodflow = c(bloodflow,BF(p,r,n,y)[1])
            vput = c(vput,BF(p,r,n,y)[2])
        }
        df = data.frame(viscosity,bloodflow)
        ggplot(df,aes(x=viscosity,y=bloodflow)) + geom_point(color="blue")
    }, height = 350, width = 600)
    
    #Length plot
    output$plotLength <- renderPlot({
        p = input$pressure
        r = input$radius
        n = input$viscosity
        y = input$length
        bloodflow = NULL; vput = NULL
        length = seq(1,45)
        for (y in length) {
            bloodflow = c(bloodflow,BF(p,r,n,y)[1])
            vput = c(vput,BF(p,r,n,y)[2])
        }
        df = data.frame(length,bloodflow)
        ggplot(df,aes(x=length,y=bloodflow)) + geom_point(color="blue")
    }, height = 350, width = 600)
}

##### UI #####

ui <- shinyUI(fluidPage(
    
    titlePanel("Blood Flow Rate"),
    
    sidebarLayout(
        sidebarPanel(

            sliderInput('pressure','Pressure',min=0,max=200,value=1,step=10),
            sliderInput('radius','Radius',min=0.1,max=6.0,value=1,step=1),
            sliderInput('viscosity','Viscosity',min=0.0,max=9.0,value=1,step=0.5),
            sliderInput('length','Length',min=0.0,max=50.0,value=1,step=1),
            hr(),
           
        ),
        
        mainPanel(
          
            p('Flow Rate'),
            textOutput("BFput"),
            hr(),
            tabsetPanel(
                tabPanel("Pressure", plotOutput("plotCall",width="100%")), 
                tabPanel("Radius", plotOutput("plotPut",width="100%")),
                tabPanel("Viscosity", plotOutput("plotVisc",width="100%")),
                tabPanel("Length", plotOutput("plotLength",width="100%")) 
            )
        )
    )  
))

##### Run #####
shinyApp(ui = ui, server = server)
