library(shiny)
library(DT)

Thisversion <- "version 0.79"
ThisApp <- "Analysis of Zymogen Activation"
fluidPage(

  # titlePanel (h2("Analysis of zymogen activation",  align = "right")),
  titlePanel(h2(ThisApp, align = "center")),
  sidebarLayout(
    sidebarPanel(
      style = "background: #ffffe6",
      # tags$h5("Select Data: csv, txt or xlsx"),
      fluidRow(
        column(8, fileInput("data1", label = "Select Data as csv, txt or xlsx file")),
        column(4, numericInput("sheetd", label = h5("Excel sheet"), value = 1, step = 1))
      ),
      fluidRow(
        column(7, numericInput("numrows",
          label = h5("Number of rows in plots and table"), value = 8
        )),
        column(5, uiOutput("what"))
      ),
      fluidRow(
        column(4, numericInput("delay", label = h5("Time delay"), step = 20, value = 0)),
        column(4, uiOutput("maxt")),
        column(4, numericInput("num",
          label = h5("Maximum abs"), step = 0.1,
          value = 0.4
        ))
      ),
      fluidRow(
        column(4, checkboxInput("sqr", label = "Use time squared", value = TRUE)),
        column(4, checkboxInput("zero", label = "Zero at initial absorbance", value = TRUE)),
        column(4, checkboxInput("names", label = h5("Well names", value = FALSE)), )
      ),
      # fluidRow(

      # column(6, numericInput("delay",label=h5("Time delay"), step = 20, value=0)),

      # column(4, checkboxInput("sqr", label =  "Use time squared", value = TRUE))


      # )),

      checkboxInput("pM", label = h4("Calculate rates of zymogen activation in pM/s using constants below", value = FALSE)),
      fluidRow(
        column(5, numericInput("kcat",
          label = h5("kcat (s-1) for chromogenic substrate"),
          value = 60
        )),
        column(5, offset = 1, numericInput("Km",
          label = h5("Km (mM) for chromogenic substrate"), step = 0.1,
          value = 0.26
        ))
      ),
      fluidRow(
        column(6, numericInput("Sub",
          label = h5("[S] (mM) for chromogenic substrate"), step = 0.1,
          value = 0.24
        )),
        column(5, numericInput("Ext",
          label = h5("Absorbance for 1M chromophore"), step = 100,
          value = 2500
        ))
      ),

      # some blurb and promotional stuff
      helpText(h5("Please cite this reference in publications:")),
      helpText(h6(
        "Longstaff C, Development of a Shiny app tool to simplify and standardize the analysis
                                   of hemostasis assay data:",
        tags$a(href = "https://doi.org/10.1111/jth.13656", "J Thromb Haemost, 15: 1044-6, 2017")
      )),

      # tags$i("Please contact me with issues relating to:"),
      helpText(h5(
        "Please contact me",
        tags$a(href = "mailto: drclongstaff@gmail.com", "drclongstaff@gmail.com"),
        "for issues relating to:"
      )),
      helpText(h5(
        ThisApp, Thisversion,
        " last accessed", Sys.Date()
      ), ),
      tags$a(href = "https://drclongstaff.github.io/shiny-clots/", "Links to other apps and help notes"),
      tags$br(),
      tags$a(href = "https://www.youtube.com/@colinlongstaff7270", "Youtube channel of help videos")
    ),
    mainPanel(
      tabsetPanel(
        type = "tab",
        tabPanel("Plots",
          plotOutput(outputId = "myplotAll"),
          h4(textOutput("text3")),
          tags$br(),
          tags$br(),
          h4(textOutput("text4")),
          h5(""), tableOutput("resultsTable"),
          align = "center"
        ),
        tabPanel(
          "Curve",
          plotOutput(outputId = "myplot"),
          plotOutput(outputId = "linear"),
        ),
        
        tabPanel("All results", tableOutput("tabres")),
        
        tabPanel("Raw data", DTOutput("contents")),
        # tabPanel("Raw data", dataTableOutput("contents")),


        tabPanel(
          "Help",
          tags$blockquote(h5(
            "►Load a data file in csv or txt fomat (tab separator)",
            tags$br(),
            "►If the columns have headers, check the box",
            tags$br(),
            "►Select the number of rows to organise the graphs and table",
            tags$br(),
            "►All the plots are shown or individual curves can be analysed on the next tab",
            tags$br(),
            "►Select the cut off for maximum absorbance",
            tags$br(),
            "►Choose how many data points you want to include",
            tags$br(),
            "►Tick the box if you want to zero each curve at the first absorbance reading",
            tags$br(),
            "►Select time squared to calculate the rate of zymogen activation in ⌂Abs/sec²",
            tags$br(),
            "►If time squared is not selected you can calculate simple rates of substrate hydrolysis over time",
            tags$br(),
            "►With time squared selected, you can also calculate zymogen activation rates in pM/s  if you know the constants for the enzyme on the chromogenic substrate in the boxes shown",
            tags$br(),
            "►For more details on this calculation, see for e.g. Sinnger, V. et al (1999), J Biol Chem, 274, 12414-22",
            tags$br(),
            "►Note: Data files should not contain any gaps or spaces between characters",
            tags$br(),
            "►Avoid unusual characters such as % and ' in names",
            tags$br(),
            "►Code files and detailed help notes are available in a github repository",
            tags$a(href = "https://github.com/drclongstaff/zymogenactn06Repo/blob/master/ZymogenactnCL_help_notes.pdf", "Here"),
            tags$br(),
            "►Other apps and links for reproducible analysis in haemostasis assays are available",
            tags$a(href = "https://drclongstaff.github.io/shiny-clots/", "here"),
            tags$br(),
            tags$i("►Please contact me with problems or suggestions relating to:"),
            "Zymogen activation app version", Thisversion, " last accessed", Sys.Date(), "at",
            tags$a(href = "mailto: drclongstaff@gmail.com", "drclongstaff@gmail.com"),
          )),
          tags$img(src = "screenCapSq.png", width = 600, height = 700)
        )
      )
    )
  )
)
