function(request) {
    dashboardPage(
    fullscreen = T,
    help = T,
    dashboardHeader(
        ),
    dashboardSidebar(
        sidebarUserPanel(
            name = "person"
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
    dashboardBody(),

    # footer = dashboardFooter(left = fluidRow(column(1,a(href = config$twitter, icon('twitter'))),
    #                                          column(1,a(href = config$github, icon('github'))),
    #                                          column(1,a(href = config$license, img(src= "CC.png", width= 18, height= 18))))
    # )
)
}