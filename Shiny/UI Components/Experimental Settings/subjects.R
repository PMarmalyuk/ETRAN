tabItem(tabName = "Subjects",
        fluidPage(
          fluidRow(  
            box(
              title = "Available Subjects", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              DT::dataTableOutput('subjectsList')
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 12, 
              fluidRow(
                column(3,actionButton('addSubj', label = "Add New", class = "btn btn-large btn-block")),
                column(3,actionButton('viewSubj', label = "View Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('updateSubj', label = "Update Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('delSubj', label = "Delete Selected", class = "btn btn-large btn-block"))
              )
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 12,
              textInput(inputId = 'subjCode', "Subject Code", value = ""),
              textInput(inputId = 'subjFullname', "Subject Full Name", value = ""),
              dateInput(inputId = 'subjBirthdate', "Date of Birth")
            )
          )
        )
)