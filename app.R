
library(shiny)
library(ggplot2)
library(DT)

source("DupRm.R")
source("EveString.R")
source("CommonPatt.R")
source("TransMx.R")
source("TransEntro.R")
source("TransEntropy.R")
source("StrDifVec.R")

TransMx_out_file_names <- c("TransMx1-Transition-Matrix", "TransMx2-Normalized-Transition-Matrix", "TransMx3-Transition-Summary")

ui <- fluidPage(
  title = "R Package GrpString",
  
  tabsetPanel(
    
    ### start tabPanel DupRm  
    tabPanel("DupRm",
             titlePanel(h1("Function DupRm of GrpString", style = "background-color: #5FA9EE; padding-left: 15px")),
             
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   helpText(
                     p("Upload a file containing a set of strings in one column,"), 
                     p("or enter a string or a set of strings separated by ',',"), 
                     p("click 'Run', successive duplicates in strings will be removed."),
                     hr(style = "border-top: 1px solid #000000;")
                   ), 
                   
                   fileInput(
                     inputId = "dupRm_file",
                     label = "Choose .csv file that stores strings to be processed"
                   ),

                   p("Another option is to enter strings in the textbox", style="font-weight:bold"),
                      
                   
                   helpText(
                     "Example:"
                   ),
                   
                   textAreaInput(inputId = "DupRm_strings", label = "",
                                 value = "aacda11, cddabb333, aadcccad, dcbaaac"),
                   
                   fluidRow(
                     column(width = 4,
                            actionButton(
                              inputId = "DupRm_run",
                              label = "Run",
                              style="font-weight:bold; background-color:powderblue;"
                            )
                     ),
                     
                     column(width = 8,
                            downloadButton('download_DupRm', 'Download Result Strings',
                                           style="font-weight:bold; background: #90EE90")
                     )
                   ), # end of fluidRow
                   
                   helpText(
                     hr(style = "border-top: 1px solid #000000;")
                   ),  

                 ) # end of sidebarLayout
               ), # end of tabPanel TransMx
               
               
               mainPanel(
                 
                 h4("Processed Strings", style="color:blue;"),
                 tableOutput("DupRm_table1")  
               )
               
             )
    ), # end of tabPanel DupRm
    
    
    ### start tabPanel EveString  
    tabPanel("EveString",
             titlePanel(h1("Function EveString of GrpString", style = "background-color: #5FA9EE; padding-left: 15px")),
             
             sidebarLayout(
               sidebarPanel(
                 fileInput(
                   inputId = "event_file",
                   label = "Choose .txt file that stores event names to be converted"
                 ),
                 helpText(
                   "Example events" 
                 ),
                 tableOutput("EveString_table1"),

                 hr(style = "border-top: 1px solid #000000;"),
                 
                 fileInput(
                   inputId = "conversion_file",
                   label = "Choose .csv file that contains the conversion key."
                 ),
                 helpText(
                   "Example conversion key" 
                 ),
                 tableOutput("EveString_table2"),
                 
                 fluidRow(
                   column(width = 4,
                          actionButton(
                            inputId = "EveString_button",
                            label = "Convert",
                            style="font-weight:bold; background-color:powderblue;"
                          )
                   ),
                   
                   column(width = 8,
                          downloadButton('Estring_download', 'Download Converted Strings',
                                         style="font-weight:bold; background: #90EE90")
                   )
                 ), # end of fluidRow
                 
                 helpText(
                   hr(style = "border-top: 1px solid #000000;"), 
                   p("Note:"), 
                   p("If at least one event name is not converted, there will be a warning message above the converted strings."),
                   p("In this case, you need to check and update the conversion key, and then rerun the function.")
                 )

               ), # end of sidebarPanel
               
               mainPanel(
                 tableOutput("EveString_tbl")
               ) # end of mainPanel
               
             ) # end of sidebarLayout
    ), # end of tabPanel EveString
   
    
    ### start tabPanel CommonPatt  
    tabPanel("CommonPatt",
             titlePanel(h1("Function CommonPatt of GrpString", style = "background-color: #5FA9EE; padding-left: 15px")),
             
             sidebarLayout(
               sidebarPanel(
                 fluidRow(
                   helpText(
                     p("Upload a file containing a set of strings in one column,"), 
                     p("or enter a string or a set of strings separated by ',',"), 
                     p("and then select the minimum percent of common patterns to be shown,"), 
                     p("finally click 'Run', table of common pattens will be displayed."),
                     hr(style = "border-top: 1px solid #000000;")
                   ), 
                   
                   fileInput(
                     inputId = "commonPatt_file",
                     label = "Choose .csv file that stores strings to be summarized"
                   ),
                   
                   p("Another option is to enter strings in the textbox", style="font-weight:bold"),
                   
                   
                   helpText(
                     "Example:"
                   ),
                   
                   textAreaInput(inputId = "commonPatt_strings", label = "",
                                 value = "ABCDdefABCDa, def123DC, 123aABCD, ACD13, AC1ABC, 3123fe"),
                   
                   sliderInput(inputId = "numLowPercent",
                               label = "Select minimum percentage of common patterns:",
                               min = 0,
                               max = 100,
                               value = 25,
                               step = 5),
                   
                   hr(style = "border-top: 1px solid #000000;"), 
                   
                   fluidRow(
                     column(width = 4,
                            actionButton(
                              inputId = "commonPatt_run",
                              label = "Run",
                              style="font-weight:bold; background-color:powderblue;"
                            )
                     ),
                     
                     column(width = 8,
                            downloadButton('download_commonPatt', 'Download common pattern table',
                                           style="font-weight:bold; background: #90EE90")
                     )
                   ), # end of fluidRow
                   
                   helpText(
                     hr(style = "border-top: 1px solid #000000;")
                   ),  
                   
                   
                 ) # end of sidebarLayout
               ), # end of tabPanel TransMx
               
               
               mainPanel(
                 
                 h4("Common Pattern Table", style="color:blue;"),
                 tableOutput("commonPatt_table1")  
               )
               
             )
    ), # end of tabPanel CommonPatt
    
    
  ### start tabPanel TransMx  
  tabPanel("Transition",
  titlePanel(h1("Functions TransEntropy and TransMx of GrpString", style = "background-color: #5FA9EE; padding-left: 15px")),

  sidebarLayout(
    sidebarPanel(
      fluidRow(
      helpText(
        p("Upload a file containing a set of strings in one column,"), 
        p("or enter a string or a set of strings separated by ',',"), 
        hr(style = "border-top: 1px solid #000000;"), 
      ),
      
      fileInput(
        inputId = "TransMx_input_file",
        label = "Choose .csv file that stores strings to be processed"
      ),
      
      p("Another option is to enter strings in the textbox", style="font-weight:bold"),
      
      textAreaInput(inputId = "TransMx_strings", label = "Example:", value = "acdac, cdab, adcad, dcbac, bdaca"),
      
      fluidRow(
        column(width = 4,
               actionButton(
                 inputId = "TransMx_run",
                 label = "Run",
                 style="font-weight:bold; background-color:powderblue;"
               )
        ),
        
        column(width = 8,
               downloadButton('downloadTransMx', 'Download Transition Tables',
                              style="font-weight:bold; background: #90EE90")
        )
      ), # end of fluidRow
      
      helpText(
        hr(style = "border-top: 1px solid #000000;")
      ),  
        
      h4("Overall Transition Entropy", style="color:blue;"),
      textOutput("entropy0"),
      
      h4("Table 0. Transition Entropies", style="color:blue;"),
      tableOutput("TransEntropy_table1")
      
      ) # end of sidebarLayout
    ), # end of tabPanel TransMx

    
    mainPanel(
      h3("Table 1. Transition Matrix", style="color:orange;"),
      tableOutput("TransMx_table1"),
      h3("Table 2. Normalized Transition Matrix", style="color:orange;"),
      tableOutput("TransMx_table2"), 
      h3("Table 3. Transition Summary", style="color:orange;"),
      tableOutput("TransMx_table3")     
    )
    
  )
), # end of tabPanel TransMx


### start tabPanel StrDifVec  
tabPanel("StrDifVec",
         titlePanel(h1("Function StrDifVec of GrpString", style = "background-color: #5FA9EE; padding-left: 15px")),
         
         sidebarLayout(
           sidebarPanel(
             fileInput(
               inputId = "strings_file1",
               label = "1. Choose .txt file that stores the first group of strings"
             ),
             tableOutput("StrDifVec_table1"),
             
             fileInput(
               inputId = "strings_file2",
               label = "2. Choose .txt file that stores the second group of strings"
             ),
             tableOutput("StrDifVec_table2"),
             
             hr(style = "border-top: 1px solid #000000;"), 
             
             sliderInput(inputId = "numPermutation",
                         label = "3. Select number of permutations:",
                         min = 0,
                         max = 5000,
                         value = 500,
                         step = 50),
             
             actionButton(
               inputId = "StrDifVec_button",
               label = "Run",
               style="font-weight:bold; background-color:powderblue;"
             ),
             
             helpText(
               hr(style = "border-top: 1px solid #000000;"), 
               p("(1) Be causious to use large number of permutation, since it can dramatically increase the running time."),
               p("(2) Meanings of the result values in the histogram:"),
               tags$ul(
                 tags$li("permutation: number of permutations"), 
                 tags$li("ld0(bw): original (or \"observed\") between-group distance"), 
                 tags$li("ld0(wi): original within-group distance"), 
                 tags$li("dif_ld0: original difference between between-group and within-group distances"), 
                 tags$li("pvalue: p value of the permutation test")
               )
             )      
           ),
           mainPanel(
             # legend coordinate
             fluidRow(
               column(width = 6,
                      sliderInput(inputId = "legend_x",
                                  label = "x coordinate of observed difference dif_ld0:",
                                  min = -1,
                                  max = 1,
                                  value = 0.045,
                                  step = 0.001)
               ),
               
               column(width = 6,
                      sliderInput(inputId = "legend_y",
                                  label = "y coordinate of observed difference dif_ld0:",
                                  min = 0,
                                  max = 500,
                                  value = 65,
                                  step = 5)
               )
             ),
             
             # text (box) coordinate
             fluidRow(
               column(width = 6,
                      sliderInput(inputId = "text_x",
                                  label = "x coordinate of result values:",
                                  min = -1,
                                  max = 1,
                                  value = 0.05,
                                  step = 0.001)
               ),
               
               column(width = 6,
                      sliderInput(inputId = "text_y",
                                  label = "y coordinate of result values:",
                                  min = 0,
                                  max = 500,
                                  value = 35,
                                  step = 5)
               )
             ),
             
             fluidRow(
               column(width = 6,
                      radioButtons("showBorder", "Show border of each bar:",
                                   choiceNames = list("No", "Yes"
                                   ),
                                   choiceValues = list("no_border", "yes_border"
                                   ),
                                   inline = TRUE
                      )
               ), 
               
               column(width = 6,
                      radioButtons("freqCount", "Show count of each bar:",
                                   choiceNames = list("No", "Yes"
                                   ),
                                   choiceValues = list("no_count", "yes_count"
                                   ),
                                   inline = TRUE
                      )
               )
             ),
             
             
             # outputs
             plotOutput(
               outputId = "my_scatter_plot"
             )
             
           )
         )
           
) # end of tabPanel StrDifVec


) # end of tabsetPanel
) # end of fluidPage


server <- function(input, output) {

  ### start of server DupRm ###
  
  s_uni.vec <- eventReactive(input$DupRm_run, {
    if(is.null(input$dupRm_file)){
      s1 <- input$DupRm_strings
      s1.list <- strsplit(s1, split = ",")
      s <- trimws(s1.list[[1]])
    }else{
      s1.df <- read.csv(input$dupRm_file$datapath, stringsAsFactors = FALSE)
      s1.vec <- as.character(s1.df[, 1])
      s <- trimws(s1.vec)
    }

    s_uni <- DupRm(s)
    return(s_uni)
  })
  
  
    output$DupRm_table1 <- renderTable({
      s_uni.vec()
    })
    

    output$download_DupRm <- downloadHandler(
      filename = function() {paste("Duplicates-removed-strings", ".csv", sep="")},
      content = function(file) {write.csv(s_uni.vec(), file, row.names=FALSE)
      }   
    ) 
  ### end of server DupRm ###
  
  
  ### start of server EveString ###
  
  s1 <- c("choiceC", "step2", "choiceA", "step1") 
  s2 <- c("step1", "choiceA", "choiceB", "") 
  s3 <- c("step1", "step2", "choiceB", "choiceA") 
 
  events.df <- data.frame(t(cbind(s1, s2, s3)))

  output$EveString_table1 <- renderTable({
    events.df
  }, 
  include.colnames=FALSE)
  
  
  event <- c("choiceA", "choiceB", "choiceC", "step1", "step2")
  char <- c("A", "B", "C", "1", "2")

  eventChar.df <- data.frame(cbind(event, char))
  
  output$EveString_table2 <- renderTable({
    eventChar.df
  })
  
  estring.vec <- eventReactive(input$EveString_button, {
    if(is.null(input$event_file) | is.null(input$conversion_file)){
      event.list <- list(s1, s2, s3)
      conversion.df <- eventChar.df
      
      s.list <- lapply(event.list, function(x){
        s <- plyr::mapvalues(x, conversion.df[, 1], conversion.df[, 2])
        s0 <- paste(s, collapse = "")
        
      })  
      s.df <- do.call(rbind, s.list)
      colnames(s.df) = "data"
      
    }else{
      event_path <- input$event_file$datapath        
      
      conversion.df <- read.csv(input$conversion_file$datapath, stringsAsFactors = FALSE)
      
      s.df <- EveString(event_path, conversion.df[, 1], conversion.df[, 2])
    }

    return(s.df)
  })
  
  output$EveString_tbl <- renderTable({
    estring.vec()
  })
  
  output$Estring_download <- downloadHandler(
    filename = function() {paste("EveString-converted-strings", ".csv", sep="")},
    content = function(file) {write.csv(estring.vec(), file, row.names=FALSE)
    }   
  ) 
  ### end of server EveString ###
  
  
  ### start of server CommonPatt ###
  
  s.df <- eventReactive(input$commonPatt_run, {
    if(is.null(input$commonPatt_file)){
      s1 <- input$commonPatt_strings
      s1.list <- strsplit(s1, split = ",")
      s <- trimws(s1.list[[1]])
    }else{
      s1.df <- read.csv(input$commonPatt_file$datapath, stringsAsFactors = FALSE)
      s1.vec <- as.character(s1.df[, 1])
      s <- trimws(s1.vec)
    }
    
    n_low <- input$numLowPercent
    
    s_comm <- CommonPatt(s, low = n_low)
    return(s_comm)
  })
  
  
  output$commonPatt_table1 <- renderTable({
    s.df()
  })
  
  
  output$download_commonPatt <- downloadHandler(
    filename = function() {paste("Common-patterns", ".csv", sep="")},
    content = function(file) {write.csv(s.df(), file, row.names=FALSE)
    }   
  ) 
  ### end of server CommonPatt ###
  
  
  ### start of server TransMx ###
  
  s.vec <- eventReactive(input$TransMx_run, {
    if(is.null(input$TransMx_input_file)){
      s1 <- input$TransMx_strings
      s1.list <- strsplit(s1, split = ",")
      s <- trimws(s1.list[[1]])
    }else{
      s1.df <- read.csv(input$TransMx_input_file$datapath, stringsAsFactors = FALSE)
      s1.vec <- as.character(s1.df[, 1])
      s <- trimws(s1.vec)
    }
    
     return(s)
  })


   observeEvent(input$TransMx_run, {
   ###  TransEntro
     transEntro <- TransEntro(s.vec())

      output$entropy0 = renderText({
        sprintf("%0.3f", transEntro)
      })
     
   ###  TransEntropy
   transEntropy <- TransEntropy(s.vec())
   
   output$TransEntropy_table1 <- renderTable({
     transEntropy$String <- format(transEntropy$String, digits = 0, scientific=F)
     transEntropy$Entropy <- format(transEntropy$Entropy, digits = 5)
     transEntropy
     })

  })

   
   ###  TransMx
   mx.list <- eventReactive(input$TransMx_run, {
     TransMx(s.vec())
   })
   
   observeEvent(input$TransMx_run, {
     output$TransMx_table1 <- renderTable(mx.list()[[1]], digits = 0, rownames = FALSE)
     output$TransMx_table2 <- renderTable(mx.list()[[2]], digits = 0, rownames = FALSE)
     output$TransMx_table3 <- renderTable(mx.list()[[3]], digits = 0, rownames = FALSE)
   })    
   
   output$downloadTransMx <- downloadHandler(
     filename = function(){
       paste0("TransMx", "-outfile.zip")
       
     },
     content = function(file){
       #go to a temp dir to avoid permission issues
       owd <- setwd(tempdir())
       on.exit(setwd(owd))
       files <- NULL;
       
       for (i in 1:3){
         # write each sheet to a csv file, save the name
         fileName <- paste(TransMx_out_file_names[i], Sys.Date(),".csv", sep = "")
         write.table(mx.list()[[i]],fileName,sep = ',', row.names = F, col.names = T)
         files <- c(fileName,files)
       }   
       #create the zip file
       zip(file,files)
     }  
   )
   ### end of server TransMx ###


   ### start of server StrDifVec ###
   StrDifVec_strings1.vec <- c("A1ABC21EF0230E03G032C30CBABGE03G0",
                               "B1G1GEG10CEF2BAC3DEBA404B5G6F6A",
                               "D21BA01FEGC23230C3E3214040B4A5G706A",
                               "DE1ADC2C0C0212CD1C23BA2EBF2BAC3D",
                               "CD1C23BA2EBAFGE4B5AG6F65A2C212CD1")
   
   output$StrDifVec_table1 <- renderTable({
     data.frame(Example_strings = isolate(StrDifVec_strings1.vec))
   })
   
   StrDifVec_strings2.vec <- c("C14C2D0D21D2123201D23D21234320431",
                               "A0A076A05A67DED0E0EFCBC07DED0",
                               "BF0D4B06060BD0D010F0F0CA0GB5G6A",
                               "7FE0EF23CBA0ABAB0B2C2632D0D3D3D",
                               "67G65FD0D4340D31530DF32030340D315")
   
   output$StrDifVec_table2 <- renderTable({
     data.frame(Example_strings = isolate(StrDifVec_strings2.vec))
   })
   
   StrDifVec.list <- eventReactive(input$StrDifVec_button, {
     
     if(is.null(input$strings_file1) | is.null(input$strings_file1)){
       strings1.df <- as.data.frame(StrDifVec_strings1.vec)
       strings2.df <- as.data.frame(StrDifVec_strings2.vec)
     }else{
       strings1.df <- read.table(input$strings_file1$datapath, stringsAsFactors = FALSE)        
       strings2.df <- read.table(input$strings_file2$datapath, stringsAsFactors = FALSE)
     }
     
     n_permu <- input$numPermutation
     
     dif.list <- StrDifVec(strings1.df[, 1], strings2.df[, 1], num_perm = n_permu)
     return(dif.list)
   })
   
   slide2 <- reactive({
     legendx <- input$legend_x
     return(legendx)
   })
   
   slide3 <- reactive({
     legendy <- input$legend_y
     return(legendy)
   })
   
   slide4 <- reactive({
     textx <- input$text_x
     return(textx)
   })
   
   slide5 <- reactive({
     texty <- input$text_y
     return(texty)
   })
   
   radio1 <- reactive({
     show_border <- input$showBorder
     return(show_border)
   })
   
   radio2 <- reactive({
     show_count <- input$freqCount
     return(show_count)
   })
   
   output$my_scatter_plot <- renderPlot({
     results.list <- StrDifVec.list()
     legend.x <- slide2()
     legend.y <- slide3()
     text.x <- slide4()
     text.y <- slide5()
     show_border <- radio1()
     n_count <- radio2()
     
     dif.vec <- results.list[[1]]
     ld_between_norm_ori <- results.list[[2]]
     ld_within_norm_ori <- results.list[[3]]
     dif_ld_norm_ori <- results.list[[4]]
     p_out <- results.list[[5]]
     
     # Number of bars
     n_permu <- length(dif.vec)
     n_break <- round(sqrt(n_permu), digits = 0)
     
     borders <- (show_border == "yes_border")
     
     h <- hist(dif.vec, breaks = n_break)
     
     # more space between the top of the highest bar and the top frame limit (max y)
     max_freq <- max(h$counts)
     ymax <- round(max_freq * 1.15, digits = 0)
     
     plot(h, ylim = c(0, ymax), xlab="Difference (dif_ld)", col = "grey", border = borders, 
          main = "Distribution of differences in average distance under H0 being true")
     
     box()
     abline(v = dif_ld_norm_ori, lwd=3, col=4)
     
     ### legend ('Observed difference') and text (p value)
     
     legend(legend.x, legend.y, c("dif_ld0"), lwd = 3, col = 4, lty = 1, seg.len = 1)
     
     text(text.x, text.y, col="red", paste("permutation = ", n_permu, "\n",
                                           "ld0(bw) = ", ld_between_norm_ori, "\n",
                                           "ld0(wi) = ", ld_within_norm_ori, "\n",
                                           "dif_ld0 = ", dif_ld_norm_ori, "\n",
                                           "pvalue = ", p_out ))
     
     if (n_count == "yes_count"){
       text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5)) # pos = 1 makes counts inside bars
     }
     
   })
   
   ### end of server TransMx ###
   
} # end of server

shinyApp(ui, server)