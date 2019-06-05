##### DataFinder shiny app ###### 
# This shiny app will serve as a prototype design for an update to the
# datafinder on the IS UI. It should allow users to easily see how metadata
# filters affect composition of assay data available, and the composition of
# studies and participants available.

# NOTE:  Shiny uses bootstrap v3 https://getbootstrap.com/docs/3.3/components/

library(shinythemes)

# Define UI 
fluidPage(
  # shinythemes::themeSelector(),
  shinyjs::useShinyjs(),
  
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    # Hack to prevent dropdowns from collapsing when you click on it
    tags$script(HTML("$(document).on('click', '.dropdown-menu', function (e) {
  e.stopPropagation();
});"))
  ),
  
  # title ------------------------------
  titlePanel("Data Finder (v3.0)"),
  
  # Main -------------------------------
  fluidPage(
    div(class = "tabbable",
        
        # Top-level nav tabs -----
        tags$ul(class = "nav nav-pills", "data-tabsetid" = "top",
                tags$li(class = "pull-right",
                        tags$a("Analyze",
                               href = "#tab-top-analyze",
                               "data-toggle" = "tab")),
                tags$li(class = "pull-right",
                        tags$a("Cluster",
                               href = "#tab-top-cluster",
                               "data-toggle" = "tab")),
                tags$li(class = "pull-right",
                        tags$a("Visualize",
                               href = "#tab-top-visualize",
                               "data-toggle" = "tab")),
                tags$li(class = "pull-right",
                        tags$a("Data",
                               href = "#tab-top-data",
                               "data-toggle" = "tab")),
                tags$li(class = "pull-right",
                        tags$a("Studies",
                               href = "#tab-top-studies",
                               "data-toggle" = "tab",
                               "data-value" = "Studies")),
                tags$li(class = "active pull-right",
                        tags$a("Find Participants",
                               href = "#tab-top-find",
                               "data-toggle" = "tab",
                               "data-value" = "Find Participants")),
                tags$li(tags$a("Resources", 
                               href = "#tab-top-resources", 
                               "data-toggle" = "tab", 
                               "data-value" = "Resources"))),
        

        # div(style="width:100%;background-color:#f7f7f9",
        #   h2("Participant Group"),
        #   tags$p("Information about applied filters")),
        # 
        tags$hr(),
        # Content for top-level tabs -----
        div(class = "tab-content", "data-tabsetid" = "top",
            
            # Find Participants -----
            div(class = "tab-pane active", id = "tab-top-find",
                # Filter bar -----
                fluidRow(style = "background-color:#f7f7f9",
                         fluidRow(
                           column(8,
                                  h3("Unsaved Participant Group"),
                                  textOutput("bannerSummary0")),
                           column(4,
                                  actionButton("load", "Load", class = "btn btn-default", style = "float:right;"),
                                  actionButton("save", "Save", class = "btn btn-default", style = "float:right;"),
                                  actionButton("clear_input", "Clear All", class = "btn btn-default", style = "float:right;"))
                         ),
                         fluidRow(
                           column(4,
                                  h4("Study Design"),
                                  uiOutput("studyIndicators")),
                           column(4,
                                  h4("Participant Characteristics"),
                                  uiOutput("participantIndicators")),
                           column(4,
                                  h4("Participant Data Includes"),
                                  uiOutput("sampleIndicators"))
                         )
                ),
                div(class = "row",
                    
                    # Find Participants tabs -----
                    div(class = "tabbable",
                        tags$ul(class = "nav nav-tabs", "data-tabsetid" = "find",
                                
                                # Groups
                                tags$li(class = "active",
                                        tags$a("Groups",
                                               href = "#tab-find-groups",
                                               "data-toggle"="tab",
                                               "data-value"="Groups")),
                                
                                # By Participant Data
                                tags$li(class = "pull-right",
                                        tags$a("By Available Data",
                                               href = "#tab-find-data",
                                               "data-toggle"="tab",
                                               "data-value"="Data Available")),
                                
                                # By Participant Characteristics
                                tags$li(class = "pull-right",
                                        tags$a("By Participant Characteristics",
                                               href = "#tab-find-participant",
                                               "data-toggle"="tab",
                                               "data-value"="Participant")),
                                
                                # By Study Design
                                tags$li(class = "pull-right",
                                        tags$a("By Study Design",
                                               href = "#tab-find-study",
                                               "data-toggle"="tab",
                                               "data-value"="Study")),
                                
                                # Text
                                tags$li(class = "pull-right",
                                        style = "padding: 10px 15px; font-weight: bold",
                                        div("Edit Group: "))
                        ),
                        
                        # Tab content within Find Participant tabs -----
                        div(class = "tab-content", "data-tabsetid"="find",
                            
                            # Groups
                            div(class = "tab-pane active",
                                id = "tab-find-groups",
                                
                                # Tab Contents
                                img(src = "participants_overview.png",
                                    style = "width:100%")
                            ),
                            
                            # By Study Design
                            div(class = "tab-pane", 
                                id = "tab-find-study",
                                
                                ## Tab Contents
                                fluidRow(
                                  column(3,
                                         h2("Study Characteristics"),
                                         p("Study characteristics available based on current filters", tags$br(),
                                           tags$em("View individual studies in the \"studies\" tab"))),
                                  column(3,
                                         plotOutput("speciesPlot", height = "150px")),
                                  column(3,
                                         plotOutput("studyTypePlot", height = "150px")),
                                  column(3,
                                         plotOutput("diseaseStudiedPlot", height = "150px"))
                                ),
                                tags$hr(),
                                fluidRow(
                                  column(4,
                                         h2("Selection", style = "display:inline-block"),
                                         actionButton("study-clear", "Reset"),
                                         actionButton("study-apply", "Apply"),
                                         div(textOutput("studySelection"))
                                         ),
                                  column(2,
                                         .createFilterDropdown(filter = "species", 
                                                               label = "Species", 
                                                               indicator = "speciesIndicators",
                                                               data)),
                                  column(2,
                                         .createFilterDropdown(filter = "condition",
                                                               label = "Disease Studied",
                                                               indicator = "conditionIndicators",
                                                               data)),
                                  column(2,
                                         .createFilterDropdown(filter = "exposure_material",
                                                               label = "Vaccine Studied",
                                                               indicator = "exposureMaterialIndicators",
                                                               data)),
                                  column(2,
                                         .createFilterDropdown(filter = "study",
                                                               label = "Study ID", 
                                                               indicator = "studyIdIndicator",
                                                               data))
                                  
                                )
                            ),
                            
                            # By Participant Characteristics
                            div(class = "tab-pane",
                                id = "tab-find-participant",
                                
                                ## Tab Contents
                                fluidRow(
                                  column(3,
                                         h2("Participant Characteristics"),
                                         p("Participant data available based on current filters")
                                  ),
                                  column(3,
                                         plotOutput("genderBarplot", height = "150px")),
                                  column(3,
                                         plotOutput("ageBarplot", height = "150px")),
                                  column(3,
                                         plotOutput("raceBarplot", height = "150px"))
                                ),
                                tags$hr(),
                                fluidRow(
                                  column(4,
                                         h2("Selection", style = "display:inline-block"),
                                         actionButton("participant-apply", "Apply"),
                                         actionButton("participant-clear", "Reset"),
                                         div(textOutput("participantSelection"))),
                                  column(2,
                                         .createFilterDropdown(filter = "gender",
                                                               label = "Gender",
                                                               indicator = "genderIndicators",
                                                               data)),
                                  column(2,
                                         .createFilterDropdown(filter = "age",
                                                               label = "Age",
                                                               indicator = "ageIndicators",
                                                               data)),
                                  column(2,
                                         .createFilterDropdown(filter = "race",
                                                               label = "Race",
                                                               indicator = "raceIndicators",
                                                               data))
                                )
                            ),
                            
                            # By Available Data
                            div(class = "tab-pane",
                                "data-value"="Data Available",
                                id = "tab-find-data",
                                
                                # Tab Contents
                                fluidRow(
                                  column(3,
                                         h2("Assay Data Available"),
                                         p("Participant Data available based on current filters"), tags$br(),
                                         tags$em("Click on a box in the heatmap to start building a filter")
                                  ),
                                  column(9,
                                         # Heatmap
                                         d3Output(outputId = "interactiveHeatmap", height = "250px")
                                  )),
                                tags$hr(),
                                
                                # Selection
                                fluidRow(
                                  column(4,
                                         h2("Selection", style = "display:inline-block"),
                                         actionButton("data-apply", "Apply"),
                                         actionButton("data-clear", "Reset"),
                                         textOutput("customText")),
                                  column(8,
                                         style = "margin-top:15px;",
                                         
                                         # Summary of selection
                                         div(style = "width: 20em; float:left; margin-left:25px;",
                                             
                                             div(class = "filter-indicator", 
                                                 div(id = paste0("data_indicator"),
                                                     class = paste0("filter-indicator-text sample"),
                                                     style = "width: auto;",
                                                     textOutput("selectedText")
                                                 )
                                             ),
                                             tags$br()),
                                         
                                         # dropdowns
                                         div( style = "margin-left:15px;float:left;",
                                              # Operator
                                              div(class = "form-group shiny-input-container", style = "width:6em;margin-bottom:20px;",
                                                  tags$select(id = "assay_operator",
                                                              tags$option(value = "OR", "OR (any of)"),
                                                              tags$option(value = "AND", "AND (all of)"))),
                                              
                                              
                                              # Cell type selector
                                              .createFilterDropdown(filter = "sample_type", 
                                                                    label = "Cell Type", 
                                                                    indicator = "sampleTypeIndicators",
                                                                    data = data)
                                              )
                                         
                                  ))
                                
                                
                                
                                
                                
                            )
                        )
                    )
                )
            ),
            
            # Studies -----
            div(class = "tab-pane", id = "tab-top-studies",
                # Filter bar -----
                fluidRow(style = "background-color:#f7f7f9",
                         fluidRow(
                           column(8,
                                  h3("Unsaved Participant Group"),
                                  textOutput("bannerSummary1")),
                           column(4,
                                  tags$button("Load", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Save", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Clear All", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE))
                         ),
                         fluidRow(
                           column(4,
                                  h4("Study Design"),
                                  uiOutput("1-staticstudy")),
                           column(4,
                                  h4("Participant Characteristics"),
                                  uiOutput("1-staticparti")),
                           column(4,
                                  h4("Participant Data Includes"),
                                  uiOutput("1-staticsample"))
                         )
                ),
                # Study cards
                # div(
                #   style="float:right;",
                #   p("Number of Samples"),
                #   div( style = "",
                #        studyCardLegend
                #   )
                # ),
                h1("Current Selection includes: ", textOutput("studyCount", inline = TRUE), "studies"),
                p("From here you can browse studies included in the current participant group, or \"go to \" a study of interest."),
                uiOutput("studyCards"),
                
                div()),
            
            # Data -----
            div(class = "tab-pane", "data-value" = "Data", id = "tab-top-data",
                # Filter bar -----
                fluidRow(style = "background-color:#f7f7f9",
                         fluidRow(
                           column(8,
                                  h3("Unsaved Participant Group"),
                                  textOutput("bannerSummary2")),
                           column(4,
                                  tags$button("Load", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Save", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Clear All", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE))
                         ),
                         fluidRow(
                           column(4,
                                  h4("Study Design"),
                                  uiOutput("2-staticstudy")),
                           column(4,
                                  h4("Participant Characteristics"),
                                  uiOutput("2-staticparti")),
                           column(4,
                                  h4("Participant Data Includes"),
                                  uiOutput("2-staticsample"))
                         )
                ),
                tags$img(src = "data.png", style = "width:100%;")),
            
            # Visualize -----
            div(class = "tab-pane", "data-value" = "Data", id = "tab-top-visualize",# Filter bar -----
                fluidRow(style = "background-color:#f7f7f9",
                         fluidRow(
                           column(8,
                                  h3("Unsaved Participant Group"),
                                  textOutput("bannerSummary3")),
                           column(4,
                                  tags$button("Load", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Save", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Clear All", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE))
                         ),
                         fluidRow(
                           column(4,
                                  h4("Study Design"),
                                  uiOutput("3-staticstudy")),
                           column(4,
                                  h4("Participant Characteristics"),
                                  uiOutput("3-staticparti")),
                           column(4,
                                  h4("Participant Data Includes"),
                                  uiOutput("3-staticsample"))
                         )
                ),
                tags$img(src = "data_explorer.png", style = "width:100%;")),
            
            # Cluster -----
            div(class = "tab-pane", "data-value" = "Data", id = "tab-top-cluster",
                # Filter bar -----
                fluidRow(style = "background-color:#f7f7f9",
                         fluidRow(
                           column(8,
                                  h3("Unsaved Participant Group"),
                                  textOutput("bannerSummary4")),
                           column(4,
                                  tags$button("Load", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Save", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Clear All", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE))
                         ),
                         fluidRow(
                           column(4,
                                  h4("Study Design"),
                                  uiOutput("4-staticstudy")),
                           column(4,
                                  h4("Participant Characteristics"),
                                  uiOutput("4-staticparti")),
                           column(4,
                                  h4("Participant Data Includes"),
                                  uiOutput("4-staticsample"))
                         )
                ),
                tags$img(src = "dimension_reduction.png", style = "width:100%;")),
            
            # Analyze -----
            div(class = "tab-pane", "data-value" = "Data", id = "tab-top-analyze",
                # Filter bar -----
                fluidRow(style = "background-color:#f7f7f9",
                         fluidRow(
                           column(8,
                                  h3("Unsaved Participant Group"),
                                  textOutput("bannerSummary5")),
                           column(4,
                                  tags$button("Load", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Save", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Clear All", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE))
                         ),
                         fluidRow(
                           column(4,
                                  h4("Study Design"),
                                  uiOutput("5-staticstudy")),
                           column(4,
                                  h4("Participant Characteristics"),
                                  uiOutput("5-staticparti")),
                           column(4,
                                  h4("Participant Data Includes"),
                                  uiOutput("5-staticsample"))
                         )
                ),
                tags$img(src = "modules.png", style = "width:100%;")),
            
            
            # Resources -----
            div(class = "tab-pane", "data-value" = "Data", id = "tab-top-resources",
                # Filter bar -----
                fluidRow(style = "background-color:#f7f7f9",
                         fluidRow(
                           column(8,
                                  h3("Unsaved Participant Group"),
                                  textOutput("bannerSummary6")),
                           column(4,
                                  tags$button("Load", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Save", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE),
                                  tags$button("Clear All", type = "button", class = "btn btn-default", style = "float:right;", disabled = TRUE))
                         ),
                         fluidRow(
                           column(4,
                                  h4("Study Design"),
                                  uiOutput("6-staticstudy")),
                           column(4,
                                  h4("Participant Characteristics"),
                                  uiOutput("6-staticparti")),
                           column(4,
                                  h4("Participant Data Includes"),
                                  uiOutput("6-staticsample"))
                         )
                ),
                h1("Resources"),
                p("Tours, help, etc (placeholder)")),
            
            div()
            
        )
    ),
    div(id = "heatmap-label"),
    div(class = "arrow-down")
  )
)




### THE END


#   )
#   fluidRow(style="margin:20px;padding:20px;",
#            # tabsetPanel(
#            #   tabPanel("Find Participants",
#            #            fluidRow(
#            #              # filterBar,
#            #              tabsetPanel(
#            #                tabPanel(
#            #                  "Participants",
#            #                  style = "float:right;",
#            #                  # Study Characteristics
#            #                  h2("Select Study Characteristics"),
#            #                  p("Study characteristics available based on current filters", tags$br(),
#            #                    tags$em("Click on a barplot to see details or add a filter")),
#            #                  div(plotOutput("speciesPlot", height = "100%"), class = "barplot"),
#            #                  div(plotOutput("studyTypePlot", height = "100%"), class = "barplot"),
#            #                  div(plotOutput("diseaseStudiedPlot", height = "100%"), class = "barplot"),
#            #                  div()
#            #                )
#            #              )
#            #            )
#            #   )
#            
# )

# <div class="tabbable">
#   <ul class="nav nav-tabs" data-tabsetid="1106">
#     <li class="active">
#       <a href="#tab-1106-1" data-toggle="tab" data-value="Find Participants">Find Participants</a>
#     </li>
#   </ul>
#   <div class="tab-content" data-tabsetid="1106">
#     <div class="tab-pane active" data-value="Find Participants" id="tab-1106-1">
#       <div class="row">
#         <div class="tabbable">
#           <ul class="nav nav-tabs" data-tabsetid="1571">
#             <li class="active">
#               <a href="#tab-1571-1" data-toggle="tab" data-value="Participants">Participants</a>
#             </li>
#           </ul>
#           <div class="tab-content" data-tabsetid="1571">
#             <div class="tab-pane active" data-value="Participants" style="float:right;" id="tab-1571-1">
#           </div>
#         </div>
#       </div>
#     </div>
#   </div>

# )

# ),
# div(),
# sidebarLayout(
#   
#   # LHS (filters) --------------------
#   sidebarPanel(
#     
#     span("Participant Group", style = "font-size:1.5em"),
#     actionButton("load", "Load", class = "btn btn-default", style = "float:right;"),
#     actionButton("save", "Save", class = "btn btn-default", style = "float:right;"),
#     tags$hr(),
#     div(style="margin-top:10px;",
#       span("Filters", style="font-size:1.5em"),
#       span(style="float:right;margin-top:10px;",
#            actionButton("clear_input", "Clear All"))
#       
#     ),
#     div(style="margin-bottom:10px;margin-top:10px;",
#         span("Include data from... ")
#         ),
#     div(
#       tags$input(type = "text", placeholder = "Search... ", name = "search")
#     ),
#     # Build query
#     div(
#       .filterSelector("Study Design", "study"),
#       uiOutput("studyIndicators"),
#       .filterSelector("Participant Characteristics", "subject"),
#       uiOutput("subjectIndicators"),
#       .filterSelector("Data Available", "sample"),
#       uiOutput("sampleIndicators"),
#       div()
#     ),
#     tags$hr(),
#     h2("Summary"),
#     textOutput("summaryText")
#       
#       
# 
#   ),
#   
#   # RHS (visualizations) -------------
#   mainPanel(
#     tabsetPanel(
#       tabPanel("Find",
#                
#                # Study Characteristics
#                h2("Select Study Characteristics"),
#                p("Study characteristics available based on current filters", tags$br(),
#                  tags$em("Click on a barplot to see details or add a filter")),
#                div(plotOutput("speciesPlot", height = "100%"), class = "barplot"),
#                div(plotOutput("studyTypePlot", height = "100%"), class = "barplot"),
#                div(plotOutput("diseaseStudiedPlot", height = "100%"), class = "barplot"),
#                
#                # Participant Characteristics
# h2("Select Participant Characteristics"),
# p("Participant data available based on current filters", tags$br(),
#   tags$em("Click on a barplot to see details or add a filter")),
# div(plotOutput("genderBarplot", height = "100%"), class = "barplot"),
# div(plotOutput("ageBarplot", height = "100%"), class = "barplot"),
# div(plotOutput("raceBarplot", height = "100%"), class = "barplot"),
#                
#                # Assay data
#                h2("Select Assay Data"),
#                p("Assay Data available by timepoint based on current filters", tags$br(),
#                  tags$em("Click on a grid box to see details or add that assay-timepoint combination to filters filer")),
#                div(plotlyOutput(outputId = "interactiveHeatmap", height = "250px")),
#                
#                div()
#                ),
#       
#       
#       tabPanel("Studies",
#                
#                # Study cards
#                div(
#                  style="float:right;",
#                  p("Number of Samples"),
#                  div( style = "",
#                       studyCardLegend
#                  )
#                ),
#                p(textOutput("studyCount", inline = TRUE), "studies"),
#                p("Timepoint-assay plots show which timepoints have assay data, where color ",
#                  "corresponds to number of samples."),
#                uiOutput("studyCards"),
#                
#                div()
#                ),
#       tabPanel("Data",
#                h1("Clinical and Assay Data"),
#                p("View or download data here (placeholder)")),
#       tabPanel("Visualize",
#                h1("Data Explorer"),
#                p("Perform some simple data visualizations here (placeholder)")),
#       tabPanel("Cluster",
#                h1("Dimension Reduction"),
#                p("Use dimension reduction techniques for clustering data here (placeholder)")),
#       tabPanel("Analyze",
#                h1("GE Anaylsis modules"),
#                p("(placeholder)")),
#       tabPanel("Resources", 
#                h1("Links to tutorials etc"),
#                p("placeholder"))
#       
#       
#     )
#     
#     
#   )
#   
# )
# )
