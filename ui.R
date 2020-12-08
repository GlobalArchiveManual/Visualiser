tagList(
  useShinyjs(),
  dashboardPage(
    dbHeader,
    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload data", tabName = "upload", icon = icon("upload")),
        menuItem("Count", tabName = "count", icon = icon("list-ol", lib="font-awesome"),
                 menuSubItem("Summary", tabName = "countsummary",icon = icon("table", lib="font-awesome")),
                 menuSubItem("Plot species trends", tabName = "countspecies",icon = icon("chart-bar", lib="font-awesome")),
                 menuSubItem("Plot metrics", tabName = "countmetrics",icon = icon("plus", lib="font-awesome"))),
        menuItem("Length", tabName = "length", icon = icon("ruler", lib="font-awesome"),
                 menuSubItem("Summary", tabName = "lengthsummary",icon = icon("table", lib="font-awesome")),
                 menuSubItem("Plot species trends", tabName = "lengthspecies",icon = icon("chart-bar", lib="font-awesome")),
                 menuSubItem("Plot metrics", tabName = "lengthmetrics",icon = icon("plus", lib="font-awesome"))),
        menuItem("Mass", tabName = "mass", icon = icon("weight-hanging", lib="font-awesome"),
                 #menuSubItem("Summary", tabName = "masssummary",icon = icon("table", lib="font-awesome")),
                 #menuSubItem("Plot species trends", tabName = "massspecies",icon = icon("chart-bar", lib="font-awesome")),
                 menuSubItem("Plot metrics", tabName = "massmetrics",icon = icon("plus", lib="font-awesome"))),
        menuItem("Acknowledgements", tabName = "acknowledgements", icon = icon("hands-helping", lib="font-awesome"))
      )
    ),
    
    
    dashboardBody(
      tabItems(
        
        
        # Upload data ----
        tabItem(tabName = "upload",
                fluidRow(tags$head(tags$style( type = 'text/css',  '.rpivotTable{ overflow-x: scroll; }')),
                         box(width=4, title = "Upload Count summary", status = "primary", solidHeader = TRUE,
                             fileInput("upload.count", ".fst file only:",
                                       accept = c("image/vnd.fst",".fst"))),
                         
                         box(width=4, title = "Upload Length summary", status = "primary",solidHeader = TRUE,
                             fileInput("upload.length", ".fst file only",
                                       accept = c("image/vnd.fst",".fst"))),
                         
                         box(width=4, title = "Upload Mass summary", status = "primary",solidHeader = TRUE,
                             fileInput("upload.mass", ".fst file only",
                                       accept = c("image/vnd.fst",".fst"))),
                         # HTML('<center><img src="globalarchive-workflow_pt2.png" width="15%">
                         #      <img src="globalarchive-workflow_pt3.png" width="15%">
                         #      <img src="globalarchive-workflow_pt4.png" width="15%">
                         #      <img src="globalarchive-workflow_pt5.png" width="15%"></center>'),
                         HTML('<center><img src="globalarchive-workflow.png" width="60%"></center>')#,
                         
                         #box(width=12,imageOutput("image.workflow"))
                )
        ),
        
        # Count summary -----
        tabItem(tabName = "countsummary",
                fluidRow(tags$head(tags$style(HTML("
    .shiny-split-layout > div {
      overflow: visible;
    }
  "))),
                box(width = 12, title = "Table 1. Campaign level summary.", collapsible = TRUE,
                    status = "primary", solidHeader = TRUE, 
                    DT::dataTableOutput('countsummary.campaigntable')),
                br(),br(),br(),
                         box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                             title = 'Table 2. Species, Target level and Trophic level summaries.',
                             splitLayout(cellArgs = list(style = "padding: 6px"),
                               htmlOutput("countsummary.campaign", multiple=FALSE),
                               selectInput("countsummary.groupby", "Summarise by:",
                                         c("Species" = "Species",
                                           "Target group" = "Target group",
                                           "Trophic group" = "Trophic group"))),
                             br(),
                             DT::dataTableOutput('countsummary.groupbytable'))
                         )
        ),
        
        # Count species -----
        tabItem(tabName = "countspecies",

                fluidRow(column(width = 3,
                                box(#width = 3,
                                  width = NULL,
                                    title = "Choose a campaign:", status = "primary", solidHeader = TRUE,
                             htmlOutput("countspecies.campaign",multiple=FALSE)),
                         
                         # Select Common name, Family, Genus and Species
                         box(#width = 3,
                           width = NULL,
                             status = "primary", solidHeader = TRUE, 
                             title = "Select a species:",
                             htmlOutput("countspecies.names")),
                         
                         # plot theme
                         box(#width = 3,
                           width = NULL,
                           title="Choose theme for plots:", status = "primary", solidHeader = TRUE, 
                           selectInput("countspecies.theme", "", 
                                       choices = c("GlobalArchive",
                                                   "Black and white"), multiple = FALSE))),
                         
                         column(width=9,
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Spatial plot",
                                    leafletOutput(outputId = "countspecies.spatial.plot", height = "500px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Status",
                                    plotOutput(outputId = "countspecies.status.plot", height = "300px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Location",
                                    plotOutput(outputId = "countspecies.location.plot", height = "300px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Site",
                                    plotOutput(outputId = "countspecies.site.plot", height = "300px")))
                )
        ),
        
        # Count metrics -----
        tabItem(tabName = "countmetrics",
                
                fluidRow(column(width = 3,
                                box(#width = 3,
                                  width = NULL,
                                  title = "Choose a campaign:", status = "primary", solidHeader = TRUE,
                                  htmlOutput("countmetrics.campaign",multiple=FALSE)),
                                
                                # Select Common name, Family, Genus and Species
                                box(#width = 3,
                                  width = NULL,
                                  status = "primary", solidHeader = TRUE, 
                                  title = "Select a metric:",
                                  selectInput("countmetrics.metric", " ", 
                                              choices = c("Total abundance",
                                                          "Family richness",
                                                          "Genus richness",
                                                          "Species richness"#,
                                                          #"Abundance by trophic group",
                                                          #"Abundance by target group"
                                              ), multiple = FALSE)),
                                
                                # plot theme
                                box(#width = 3,
                                  width = NULL,
                                  title="Choose theme for plots:", status = "primary", solidHeader = TRUE, 
                                  selectInput("countmetrics.theme", "", 
                                              choices = c("GlobalArchive",
                                                          "Black and white"), multiple = FALSE))),
                         
                         column(width=9,
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Spatial plot",
                                    leafletOutput(outputId = "countmetrics.spatial.plot", height = "500px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Status",
                                    plotOutput(outputId = "countmetrics.status.plot", height = "300px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Location",
                                    plotOutput(outputId = "countmetrics.location.plot", height = "300px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Site",
                                    plotOutput(outputId = "countmetrics.site.plot", height = "300px")))
                )
        ),
        
        # Length summary -----
        tabItem(tabName = "lengthsummary",
                fluidRow(tags$head(tags$style(HTML("
    .shiny-split-layout > div {
      overflow: visible;
    }
  "))),
                    
                         box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                             title = 'Table 1. Length summaries.',
                             splitLayout(cellArgs = list(style = "padding: 6px"),
                                         htmlOutput("lengthsummary.campaign", multiple=FALSE),
                                         selectInput("lengthsummary.groupby", "Summarise by:",
                                                     c("Species" = "Species",
                                                       "Target group" = "Target group",
                                                       "Trophic group" = "Trophic group"))),
                             br(),
                             DT::dataTableOutput('lengthsummary.table'))
                )
        ),
        # LENGTH species -----
        tabItem(tabName = "lengthspecies",
                fluidRow(column(width = 3,
                                box(#width = 3,
                                  width = NULL,
                                  title = "Choose a campaign:", status = "primary", solidHeader = TRUE,
                                  htmlOutput("lengthspecies.campaign",multiple=FALSE)),
                                
                                # Select Common name, Family, Genus and Species
                                box(#width = 3,
                                  width = NULL,
                                  status = "primary", solidHeader = TRUE, 
                                  title = "Select a species:",
                                  htmlOutput("lengthspecies.names")),
                                
                                # plot theme
                                box(#width = 3,
                                  width = NULL,
                                  title="Choose theme for plots:", status = "primary", solidHeader = TRUE, 
                                  collapsible = TRUE,
                                  selectInput("lengthspecies.theme", "", 
                                              choices = c("GlobalArchive",
                                                          "Black and white"), multiple = FALSE)),
                                box(width=NULL,
                                    title="Adjust binwidth of histogram:", status = "primary", solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    numericInput("length.binwidth","", value = 5))),
                         
                         
                         column(width=9,
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Histogram",
                                    plotOutput(outputId = "lengthspecies.histogram.plot", height = "300px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Boxplot",
                                    plotOutput(outputId = "lengthspecies.status.plot", height = "300px")))
                                
                )
        ),
        
        # LENGTH metrics -----
        tabItem(tabName = "lengthmetrics",
                
                fluidRow(column(width = 3,
                                box(width = NULL,
                                  title = "Choose a campaign:", status = "primary", solidHeader = TRUE,
                                  htmlOutput("lengthmetrics.campaign",multiple=FALSE)),
                                
                                # plot theme
                                box(width = NULL,
                                  title="Choose theme for plots:", status = "primary", solidHeader = TRUE, 
                                  collapsible = TRUE,
                                  selectInput("lengthmetrics.theme", "", 
                                              choices = c("GlobalArchive",
                                                          "Black and white"), multiple = FALSE))),
                         column(width=9,
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Target Group",
                                    plotOutput(outputId = "lengthmetrics.target.plot", height = "300px")),
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Trophic Group",
                                    plotOutput(outputId = "lengthmetrics.trophic.plot", height = "300px")))
                         
                )
        ),

        # MASS metrics -----
        tabItem(tabName = "massmetrics",
                fluidRow(column(width = 3,
                                box(width = NULL,
                                    title = "Choose a campaign:", status = "primary", solidHeader = TRUE,
                                    htmlOutput("massmetrics.campaign",multiple=FALSE)),
                                
                                # plot theme
                                box(width = NULL,
                                    title="Choose theme for plots:", status = "primary", solidHeader = TRUE, 
                                    collapsible = TRUE,
                                    selectInput("massmetrics.theme", "", 
                                                choices = c("GlobalArchive",
                                                            "Black and white"), multiple = FALSE)),
                         
                         # plot theme
                         box(width = NULL,
                             title="Choose group to plot:", status = "primary", solidHeader = TRUE, 
                             collapsible = TRUE,
                             selectInput("massmetrics.groupby", " ",
                                         c("Target group" = "Target group",
                                           "Trophic group" = "Trophic group")))),
                         
                         
                         column(width=9,
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Total mass of all fish",
                                    plotOutput(outputId = "massmetrics.all.plot", height = "300px")),
                                
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Total mass of fish greater than 200 mm",
                                    plotOutput(outputId = "massmetrics.200.plot", height = "300px")),
                                
                                box(width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    title="Total mass of fish greater than 300 mm",
                                    plotOutput(outputId = "massmetrics.300.plot", height = "300px")))
                )
        ),
        
        # Acknowledgments ----
        tabItem(tabName = "acknowledgements",
                fluidRow(box(width = 4, status = "primary", height = 800,
                             "     ",HTML('<center><img src="logos-stacked.png" width="100%"></center>')
                             
                ),
                box(width = 8, status = "primary", height = 800, title = "Acknowledgments",
                    "The Marine Biodiversity Hub is funded by the Australian Government's National Environmental Science Program.",
                    br(),br(),
                    "Montbello example stereo-BRUV data was from the benchmark survey of deepwater fish in the Montbello Marine Park. Funded by the Gorgon Barrow Island Net Conservation Benefits Fund, the Commonwealth Scientific and Industrial Research Organisation and the University of Western Australia.",
                    br(),br(),
                    "GlobalArchive and Visualiser development has been supported by the Australian Research Data Commons and the
                  National Environmental Science Program's Marine Biodiversity Hub.", br(), br())
                )
        )
        
      )
    )
  )
)