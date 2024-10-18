function(request) {
    dashboardPage(
    fullscreen = T,
    help = T,
    dashboardHeader(
        title = config$portal_name,
        shiny::includeCSS("www/datatable.css")
        ),
    dashboardSidebar(
        sidebarUserPanel(
            name = config$portal_funder_name,
            image = config$portal_funder_link
        ),
        sidebarMenu(
            id = "sidebarmenu",
            menuItem(
                "Uploader",
                tabName = "validator",
                icon = icon("upload")
            ),
            menuItem(
                "About",
                tabName = "about",
                icon = icon("sliders-h")
            ),
            menuItem(
                "Help",
                tabName = "help",
                icon = icon("question")
            )
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
            tabItem(
                tabName = "about",
                box(
                    title = "Overview",
                    p("Welcome to the Data Validator webpage. This tool allows you to validate data interactively by uploading a dataset and rules file. To get started, go to the validator tab on the left."),
                    HTML(paste0('<iframe width="560" height="315" src="',config$overview,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen require-corp credentialless (Chrome > 96)></iframe> ></iframe>')),
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = F,
                    p("Join our team to build this tool!"),
                    HTML(paste0('<a class="btn btn-info" href = "', config$github,'" role = "button" target="_blank">Github</a>')),
                    boxLayout(
                        type = "columns",
                        lapply(config$contributors, function(x){x})
                    ),
                    width = 12
                )
            ),
            tabItem(
                tabName = "validator",
                tags$script(src = "https://cdn.datatables.net/1.11.3/js/jquery.dataTables.min.js"),
                fluidRow(
                    column(4,
                           fileInput("file", NULL,
                                     placeholder = "Start Here",
                                     buttonLabel = "Upload Data",
                                     width = "100%",
                                     multiple = T,
                                     accept=c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".xlsx",
                                              ".zip")) %>%
                               bs4Dash::popover(
                                   title = "Upload CSV to validate",
                                   content = "This is where you upload the csv, zip, and/or xlsx files file that you want to validate.")
                    ),
                    column(4,
                           if(!isTruthy(config$rules_to_use)){
                               bs4Dash::popover(
                                   fileInput("file_rules", NULL,
                                             placeholder = ".csv",
                                             buttonLabel = "Rules...",
                                             width = "100%",
                                             accept=c("text/csv",
                                                      "text/comma-separated-values,text/plain")),
                                   title = "Upload rules",
                                   content = "Upload the rules csv to use to validate the data csv"
                               ) 
                           },
                           downloadButton("download_rules_excel", 
                                          "Data Template",
                                          style = "background-color: #ed6ca7;"), # Define the popover in the button's HTML using data attributes
                           tags$script(HTML("
               $(document).ready(function() {
                 $('#download_rules_excel').popover({
                   title: 'Download rules template file',
                   content: 'This is a file that can be used as a template when collecting data so that it conforms to most of the rules tested in this portal.',
                   placement: 'right',
                   trigger: 'hover'
                   });
               });
             ")),
                                          ),
                    column(4, 
                           uiOutput("alert"))),
                uiOutput("error_query"),
                uiOutput("dev_options")
            ),
            #tabItem(tabName = "item4",
            #        includeMarkdown("www/datainstructions.md")
            #),
            tabItem(
                tabName = "help",
                box(
                    title = "Tutorial",
                    p("Welcome to the Data Validator webpage. This tool allows you to validate data interactively by uploading a dataset and rules file. To get started, go to the validator tab on the left."),
                    HTML(paste0('<iframe width="560" height="315" src="',config$tutorial,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen require-corp credentialless (Chrome > 96)></iframe> ></iframe>')),
                    width = 12
                ),
                box(
                    title = "Instructions and Examples",
                    br(),
                    tags$ol(
                        tags$li("Both the data and rules files must be in .csv or .xlsx format. Examples for how to structure and query the data and rules can be found below:"),
                            downloadButton("download_rules", "Download Sample Rules", style = "background-color: #2a9fd6;"),
                        tags$script(HTML("
               $(document).ready(function(){
                 $('#download_rules').popover({
                   title: 'Download rules file',
                   content: 'This is an example file that can be used in tandem with the valid or invalid data files to test out the tool.',
                   placement: 'bottom',
                   trigger: 'hover'
                   });
               });
             ")),
                            downloadButton("download_good_sample", "Download Valid Sample Data", style = "background-color: #28a745;"),
                        tags$script(HTML("
               $(document).ready(function(){
                 $('#download_good_sample').popover({
                   title: 'Download valid example data',
                   content: 'This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that is 100% validated.',
                   placement: 'bottom',
                   trigger: 'hover'
                   });
               });
             ")),
                            downloadButton("download_sample", "Download Invalid Sample Data", style = "background-color: #e4606d;"), 
                        tags$script(HTML("
    $(document).ready(function(){
      $('#download_sample').popover({
        title: 'Download invalid example data',
        content: 'This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that isn’t 100% validated.',
        placement: 'bottom',
        trigger: 'hover' 
      });
    });
  ")),
                        
                        p(),
                        tags$li("Uploaded the data and rules file on the validator tab. NOTE: If using the examples you will need to first unzip them."),
                        tags$image(src = "upload.png", width = "50%"),
                        p(),
                        tags$li("If your data is validated a popup will appear to input your credentials and then click ok. If this is a resubmission, upload a previous certificate to override the previous submission."),
                        tags$image(src = "popup.png", width = "50%"),
                        p(),
                        tags$li("If your data is validated you may download a certificate. The certificate is proof of your submission and will allow you to update the submission. This should always be saved for any submitted data."),
                        tags$image(src = "download.png", width = "50%"),
                        p(),
                        tags$li("In the event of invalidated data, the box for the file with issues to be resolved will be red, there will be an indication of the number of rules that were successful, had warnings, or had errors. Only errors need to be resolved for the validation to be successful."),
                        tags$image(src = "error.png", width = "50%"),
                        p(),
                        tags$li("You can click on any of the descriptions in the 'Issues Raised' panel to display the rows where the issue was found in the 'Issues Selected' panel."),
                        tags$image(src = "issueselection.png", width = "50%"),
                        p(),
                        tags$li("The 'Issues Raised' and 'Issue Selected' data sheets may be copied, or downloaded as CSV, Excel, or PDF."),
                        tags$image(src = "issuedownload.png", width = "50%")
                    ),
                    width = 12
                ),
                box(
                    title = "FAQs",
                    strong("Where is my data going?"),
                    p("Data will not be shared externally unless you specify the data can be shared by inputting the security key. If a key is input, the data will go to one or more of MongoDB, CKAN, and/or S3."),
                    strong("Is this open source web tool secure?"),
                    p("The validator app is https encrypted and the source code is available on GitHub for security review."),
                    width = 12
                ),
                box(
                    title = "Contact Us",
                    p("Have any additional questions or concerns? Email us using the link below:"),
                    HTML(paste0('<a class="btn btn-info" href = "mailto:', config$contact, '" role = "button" >Contact Us</a>')),
                    p("Please include in your email:"),
                    p ("(1) What do you think the app should be doing?"),
                    p ("(2) What is the app doing instead?"),
                    width = 12
                )
            )
            
        )
        
    ),

    footer = dashboardFooter(left = fluidRow(column(1,a(href = config$twitter, target = "_blank", icon('twitter'))),
                                             column(1,a(href = config$github, target = "_blank",icon('github'))),
                                             column(1,a(href = config$license, img(src= "CC.png", width= 18, height= 18))))
    )
)
}