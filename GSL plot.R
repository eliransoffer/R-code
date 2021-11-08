#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("shinyWidgets")
library("readxl")
library("ExcelFunctionsR")
library("dplyr")
library("knitr")
library("reshape2")
library("ggplot2")
library("svglite")


###################################
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
    library(doBy)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
    
    # Collapse the data
    formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
    datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
    
    # Rename columns
    names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
    names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
    names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
}
###################################
setwd("C:/Users/eliranso.WISMAIN/Dropbox (Weizmann Institute)/GSL microbiome/ES30_soyGSL sulfitobacter")
#setwd("C:/Users/conny.WISMAIN/Dropbox (Weizmann Institute)/GSL microbiome/ES28_soyGSL noGlycerol")
#setwd(choose.dir())

### Load the data ###
df <- read_excel("ES30_Calculations.xlsx",sheet = "Final Bacterial count")
df <- df[,1:6]
df <- data.frame(df)
#stringCols <- colnames(df[,1:4])
#df[stringCols] <- lapply(df[stringCols], factor)
tgc <- summarySE(df, measurevar="Bac.ml", groupvars=c("Taxon","Hour","soyGSL"))
plt <- melt(tgc, id.vars = c("Taxon","Hour","soyGSL","se"), measure.vars = c("Bac.ml"))


# Define UI
ui <- shinyUI(
    fluidPage(
        

        sidebarPanel(
            
            titlePanel("Bacterial count"),
                     checkboxGroupButtons("taxa", "Taxon",
                               choices=c("Sulfitobacter marinus",
                                         "Sulfitobacter pontiacus",
                                         "Sulfitobacter porphyrae",
                                         "D7",
                                         "Alteromonas addita"),
                               selected = c("Sulfitobacter marinus"),
                                         individual = TRUE, width = "200px" , checkIcon = list(
                                                                                    yes = icon("ok",
                                                                                    lib = "glyphicon"))
            ),
            
            checkboxGroupButtons("soy", "soyGSL", c("+","-"), selected = c("+","-")),
            
            checkboxGroupInput("logit", "Logarithmic scale",""),
            
            checkboxGroupInput("yaxis", "Fix y axis",""),
            
            checkboxGroupInput("facet", "Seperate mode","",selected = ""),
            
            downloadButton('downloadPlot','Download Plot')
            
        , width=3),
        mainPanel(tabsetPanel(
            
            tabPanel("Figure", plotOutput("plot", height = 920, width = 1300)),
            #tabPanel("Figure", plotlyOutput("distPlot", height = 920, width = 1300)),
            #tabPanel("Figure", plotOutput("plot", height = 400, width = 1300)),
            tabPanel("Table", tableOutput("test"))
            )
        )
        
)
)

# Define server
server <- function(input, output, session) {
    
    taxon <- reactive({
        subset(plt, Taxon %in% input$taxa
               & soyGSL %in% input$soy)
    })
    
    logarithm <- reactive({
        if(!is.null(input$logit)) {
            log10(taxon()$value)
        }
    })
    
        
    observeEvent(input$taxa, {
        
        output$plot <- renderPlot({
            
            plt.tmp <- taxon()
            
            if(!is.null(input$logit)){
                plt.tmp$value <- logarithm()
            }
            
            output$test <- renderTable({
                taxon()
            })
            
            #s <- m %>% group_by(Taxon, Day, soyGSL) %>% summarise_each(funs(mean))
            #output$distPlot <- renderPlotly({
            
                p <- ggplot(plt.tmp, aes(Hour, value, colour=Taxon)) +
                    theme_bw() +
                    geom_point(size=3) +
                    geom_line(aes(colour=Taxon, linetype=soyGSL), size=1) +
                    theme(legend.text=element_text(size=10),
                          legend.position = "bottom",
                          legend.title = element_text(size = 20),
                          axis.title = element_text(size=36, face="bold"),
                          axis.text = element_text(size = 24),
                          legend.box = "vertical",
                          legend.key.width = unit(1.5,"cm")) +
                    scale_x_discrete(limits = c(unique(plt.tmp$Hour)), expand = expansion(add = c(6,140))) +
                    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
                    labs(x = "hpi", y = 'Bacteria/ml')
                    
                    if(!is.null(input$logit)){
                        p <- p + ylim(3,10)
                    }else{
                        if(!is.null(input$yaxis)){
                            p <- p + ylim(0,max(plt$value)) + geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1)
                        }else{
                            p <- p + geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1)
                        }
                        
                    }
                    
                    if(!is.null(input$facet)){
                        p <- p + facet_wrap(.~Taxon) + theme(strip.text.x = element_text(size = 24)) +
                            guides(color = FALSE)
                    }
                print(p)
           # })
        })
    })
    
    down <- reactiveValues()
    down$plot <- p
    
    output$downloadPlot <- downloadHandler(
        filename = function(){
            paste("Sulfitobacter",".svg",sep='')
            },
        content = function(p){
            ggsave(p, device = "svg", width = 5.5, height = 4.5, units = "in", dpi = 320)
        }
    )
    
}

# Run the application 

shinyApp(ui = ui, server = server)
