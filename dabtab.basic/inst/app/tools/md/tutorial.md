library(dotenv)
library(RMySQL)
library(bcrypt)  # Load bcrypt package

if (isTRUE(getOption("dabtab.from.package"))) {
  library(dabtab)
}


## source shared functions
source(file.path(getOption("radiant.path.data"), "app/init.R"), encoding = enc, local = TRUE)
source(file.path(getOption("radiant.path.data"), "app/radiant.R"), encoding = enc, local = TRUE)



# Load .env variables
dotenv::load_dot_env()
db_host <- Sys.getenv("DB_HOSTNAME")
db_user <- Sys.getenv("DB_USERNAME")
db_password <- Sys.getenv("DB_PASSWORD")
db_database <- Sys.getenv("DB_DATABASE")
db_port <- as.integer(Sys.getenv("DB_PORT"))



#THIS IS LOGIN UI FOR LOCAL USERS
loginUI<-div(
  id = "loginpage",
  style = "width: 80%; max-width: 400px; margin: 0 auto; padding: 20px;
          background-color: #7ecc49; color: #fff; font-family: 'Helvetica Neue',
          Helvetica, Arial, sans-serif; border: 1px solid #d1d5da; border-radius: 3px;",
  tags$img(src = "github_logo.png", height = 50, width = 50, style = "margin: 0 auto; display: block;"),
  tags$h1("Sign in to DabTab", class = "h4 text-gray-dark text-center mb-4 font-weight-normal"),
  wellPanel(
    div(
      class = "form-group",
      tagList(
        "Username or email address",
        textInput("userName", "", placeholder = "Enter your username or email address"
        )
      )
    ),
    br(),
    div(
      class = "form-group",
      tagList(
        "Password",
        passwordInput("passwd", "", placeholder = "Enter your password")
      )
    ),
    br(),
    div(
      class = "form-group",
      actionButton("login", "Sign in", class = "btn btn-primary btn-block btn-lg")
    ),
    shinyjs::hidden(
      div(id = "nomatch",
          tags$p("Oops! Incorrect username or password!",
                 style = "color: red; font-weight: 600;
                      padding-top: 5px; font-size: 16px;",
                 class = "text-center"))
    )
  ),
  div(
    class = "text-center",
    tags$a(href = "#", "Forgot password?", class = "forgot-password-link text-gray-dark")
  ),
  tags$style(
    "body {
      background-image: url('https://som.edu.vn/wp-content/uploads/2022/12/khai-niem-va-thuat-ngu-data-science.jpeg');
      background-size: cover;
      background-position: center center;
    }"
  )
)



  # Define reactive value to store current page
  current_page <- reactiveVal("landingPageUI")

  # Define observer for login button
  observeEvent(input$login, {
    current_page("loginUI")
  })


#verification for local users
    credentials = data.frame(
      username_id = c("myuser", "myuser1"),
      passod   = sapply(c("mypass", "mypass1"),password_store),
      permission  = c("basic", "advanced"),
      stringsAsFactors = F
  )

    shinyjs::useShinyjs()
    login =FALSE

    USER <- reactiveValues(login=login)
    observe({
      if (USER$login == FALSE) {
        if (!is.null(input$login)) {
          if (input$login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            if(length(which(credentials$username_id==Username))==1) {
              pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)
              if(pasverify) {
                USER$login <- TRUE
              } else {
                shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          }
        }
      }
})




# Define reactive value to store current page
current_page <- reactiveVal("landingPageUI")

# Define observer for login button
observeEvent(input$login, {
  current_page("loginUI")
})

options(dabtab.path.advanced = system.file(package = "dabtab.advanced"))


options(dabtab.authenticated = FALSE)

options(dabtab.paymentverified = FALSE)


options(radiant.local = FALSE)

options(dabtab.online = FALSE)

role = "BasicUser"

# Replace with stored credentials
con <- dbConnect(MySQL(),
  host = "db_host",  
  user = "db_user", 
  password = "db_host", 
  dbname = "db_database", 
  port = "db_port")
})


 # Don't forget to disconnect from the database when the app stops
  onStop(function() {
    dbDisconnect(con)
  })
}

  iv <- InputValidator$new()
  iv$add_rule("password", sv_required())
  iv$add_rule("first_name", sv_required())
  iv$add_rule("last_name", sv_required())
  iv$add_rule("password_recover", sv_required())
  iv$add_rule("password_confirm", sv_required())
  iv$add_rule("email", sv_required())
  iv$enable()


validate_email_input <- function(email, con, session) {
    if (nchar(email) == 0) {
        sendSweetAlert(
            session = session,
            title = "Empty Email",
            text = "Please provide an email address.",
            type = "warning"
        )
        return(FALSE)
    }
    
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$", email)) {
        sendSweetAlert(
            session = session,
            title = "Invalid Email",
            text = "Please enter a valid email address.",
            type = "warning"
        )
        return(FALSE)
    }

    query <- sprintf("SELECT * FROM users WHERE email = '%s'", email)
    result <- dbGetQuery(con, query)
    if (nrow(result) > 0) {
        sendSweetAlert(
            session = session,
            title = "Duplicate Email",
            text = paste0(email, " is already registered. Please use a different email."),
            type = "warning"
        )
        return(FALSE)  # Return FALSE to indicate the email already exists
    }
    return(TRUE)  # Return TRUE to indicate the email is valid and does not exist
}



insert_credentials <- function(first_name, last_name, email, hashed_password, role, userUUID, con) {
    query <- sprintf(
        "INSERT INTO users (userUUID, first_name, last_name, email, password, role) VALUES ('%s', '%s', '%s', '%s', '%s', '%s')",
        userUUID, first_name, last_name, email, hashed_password, role
    )
    dbExecute(con, query)
}

observeEvent(input$signup, {
    req(input$first_name, input$last_name, input$email, input$password)

    # Access the input values
    first_name <- isolate(input$first_name)
    last_name <- isolate(input$last_name)
    email <- isolate(input$email)
    password <- isolate(input$password)
    hashed_password <- hash_password(password)
    role <- "BasicUser"  # Set the role to a default value, modify as needed
    userUUID <- generateUserUUID(email)  # Generating UUID for the user

    if (validate_email_input(email, con, session)) {
        insert_credentials(first_name, last_name, email, hashed_password, role, userUUID, con)
        options(dabtab.authenticated = TRUE)
    }
})

  
recover_notification <- modalDialog(
  title = "Success Notification",
  "This has been a success. Do you want to assess DABTAB now?",
  footer = tagList(
    actionButton(inputId = "proceed", label = "Proceed with DabTab"),
    actionButton(inputId = "later", label = "Later")
  )
)


# Function to hash the password
hash_password <- function(password) {
  bcrypt::hashpw(password)
}



alter_recover_psw <- function(email) { 
  # Query to fetch user details
  query <- sprintf("SELECT * FROM users WHERE email = '%s'", email)
  
  # Get the new password and confirmation from the form
  password_recover <- isolate(input$password_recover)
  password_confirm <- isolate(input$password_confirm)

  # ... Perform database connection and execute query ...

  result <- dbGetQuery(con, query)
  
  if (nrow(result) == 1) {
    # Perform the logic to check if passwords match and update the user's password
    if (password_recover == password_confirm) {
      # Proceed with updating the user's password in the database
      # Update query to set the new password for the user
      hashed_password <- hash_password(password_recover)  # Hash the new password
      update_query <- sprintf("UPDATE users SET password = '%s' WHERE email = '%s'", hashed_password, email)
      
      # Execute the update query
      dbExecute(con, update_query)
      
      # Log success or send email to the user indicating password change
      # ...

      # Clear the inputs
      updateTextInput(session, "password_recover", value = "")
      updateTextInput(session, "password_confirm", value = "")
      
      # Log successful password change or redirect user
      # ...
    } else {
      # Log error: Passwords do not match
      # ...
    }
  } else {
    # Log error: User not found
    # ...
  }
}


 alter_recover_psw(email) { 
    query <- sprintf("SELECT * FROM users", email)

    password_recover <- isolate(input$password_recover)
    password_confirm <- isolate(input$password_confirm)

           if(length(which(credentials$username_id==Username))==1) {
              pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)



    integrate logic/webhook to send to useremail


    CLEAR THE INPUTS AND LOG NOT ALLOW ALTER


    result <- dbGetQuery(con, query)

    alter password which 



    if (nrow(result) > 0) {
       result$email is exist  # Replace 'password' with the actual column name

   sendSweetAlert(
            session = session,
            title = "Fail !!",
            text = paste0(email,"already exist", "Please recheck and then sign in."),
            type = "warning"
          )

    }


      return(FALSE)
}




generateUserUUID <- function(email) {
  # Combine the email and current time to create a unique identifier
  # You can use any unique string concatenation method here
  unique_string <- paste0(email, as.character(Sys.time()))
  
  # Hash the unique string to create a unique UUID
  ussid <- digest::digest(unique_string, algo = "sha256", serialize = FALSE)
  
  # Return the generated UUID
  return(ussid)
}




options(dabtab.authenticated = TRUE)



if (getOption("dabtab.development", default = FALSE)) {
    if (nrow(result) > 0) {
      stored_password_hash <- result$password  # Replace 'password' with the actual column name
      if (bcrypt::verify(password, stored_password_hash)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }


   # Define a function to validate user credentials
  validate_credentials <- function(email, password) {
    query <- sprintf("SELECT * FROM users WHERE username = '%s'", username)
    result <- dbGetQuery(con, query)
    if (nrow(result) > 0) {
      stored_password_hash <- result$password  # Replace 'password' with the actual column name
      if (bcrypt::verify(password, stored_password_hash)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }



}


#login UI
 login_filePath <- file.path(getOption("dabtab.path.advanced"), "app/www/login.html")
 loginUI <- includeHTML(login_filePath)



ALSO FOR OFFLINE USERS THEY CAN ONLY ACCESS BASIC PACKAGE THE DEFAULT PASSWORD AND USERNAME WILL BE PROVIDED


TO USE ADVANCED FEATURE PLEASE ACCESS WEBSITE WITH DOMAIN HTTP:////

>>>WHEN USER REGISTER BY DEFAULT THEIR ROLE IS BasicUser

>>>THEY HAVE TO FILL all THE REQUIRED FIELDS INCLUDE THE required_field list

>>>IN THIS STEP PLEASE ALSO ENSCRYPT THE PASSWORD FOR LATER VERIFY WITH bcrypt

>>>NOTIFY USERS IF THERE IS NOT A MATCH 

>>>TWO SCENARIOS HERE 

ONE IS THEY ARE NOT REGISTERD YET OR THEY FORGOT PASSWORD FOR

IF NOT REGISTER THEN SIMPLY SIGN UP A NEW ACCOUNT 

IN CASE OF VERIFY PASSWORD A LINK WILL BE SEND TO THEIR EMAIL TO RECOVER WITH THIS APPROACH PULL USER INFORMATION WITH USERNAME
THEN ALTER THE PASSWORD IN DATABASE THIS WILL BE HANDLE WITH SHINYJS WITHOUT SEPERATE REST API

PROVIDE LINK TO ACCESS THE LOGIN PAGE WHEN THE RECOVER PROCESS IS COMPLETED IN FORM OF NOTIFICATION MODAL, LOCK ANY FURTHER ACTION  


>>>INSERT USER INFOR INTO MYSQL DATABASE TABLE


>>>OPTIONAL PROVIDE ACCESS TO CLOUD SERVER WHICH ACTS AS IF A BATCH STORAGE/BUCKET TO ARCHIVE USER DATA
 



    USER <- reactiveValues(login=login)
    observe({
      if (USER$login == FALSE) {
        if (!is.null(input$login)) {
          if (input$login > 0) {

            # Replace 'input$userName' and 'input$passwd' with your actual input names
            username <- isolate(input$username)
            password <- isolate(input$password)
            role <- isolate(input$passwd)
            if(length(which(credentials$username_id==Username))==1) {
              pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)
              if(pasverify) {
                USER$login <- TRUE
              } else {
                shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          }
        }
      }
    })


TAG$STYLE

<a href="">Forgot your password?</a>
shaking effect and switch color

if not then just change style of "forgot password"


    #obserEvent-START PROJECT
    observeEvent(input$start_proj,{
      req(input$start_proj)
      showModal(modalDialog(
        title = "Create Project",
        # File type
        textInput("proj_name", "Project Name:", placeholder = "Enter project name"),
        # File format
        textInput("proj_infor", "Project Information:", placeholder = "Enter file information"),
        footer = tagList(
          actionButton("initialize", "Initialize"),
          modalButton("Cancel")
        )))
    })

    #obserEvent-LOGIN VALIDATION

dabtab.data <- function(state, ...) launch(package = "dabtab.basic", run = "browser", state, ...)

dabtab.login <- function () {
  observe({(
    if (isTrue(getOption("dabtab.authenticated", default = FALSE ) && user$role == "Admin") {
        current_page("adminPageUI")
        showModal(modalDialog(
        includeHTML("intro_text_user.html"),
          easyClose = TRUE,
          footer = tagList(
            actionButton(inputId = "intro_2", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
          )))
      } else if (if (isTrue(getOption("dabtab.authenticated", default = FALSE ) && user$role == "BasicUser") {){
        current_page("openPageUI")
        showModal(modalDialog(
          includeHTML("intro_text.html"),
          easyClose = TRUE,
          footer = tagList(
            actionButton(inputId = "intro_2", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
          )))
    })


    observe({
      if (USER$login == TRUE){
        current_page("openPageUI")
        showModal(modalDialog(
          includeHTML("intro_text.html"),
          easyClose = TRUE,
          footer = tagList(
            actionButton(inputId = "intro_2", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
          )))
      }
    })

    # Define observer for project initialization
    observeEvent(input$initialize, {
      if (input$initialize && current_page() == "openPageUI" && credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced"){
        if (nchar(input$proj_name) > 0) {
          current_page("advancedUI")
          output$logoutbtn<-renderUI({actionButton('logoutbtn', 'Logout', class = 'action-button logout-btn')})
          removeModal()
        }
      }
    })

   sendSweetAlert(
            session = session,
            title = "Fail !!",
            text = paste0("Please make payment to upgrade to advanced user to access more advanced features."),
            type = "warning"
          )

    }

if isFalse(getOption(options(dabtab.paymentverified)

  modalDialog(
            easyClose = TRUE,
         

     sendSweetAlert(
            session = session,
            title = "Fail !!",
            text = paste0("Please make payment to upgrade to advanced user to access more advanced features."),
            type = "warning"
          ),
      footer = tagList(
            actionButton(inputId = "make_payment", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
          )))

    }


upgrade_user_credentialS(email, role){

}




paymentMenu <- showModal(modalDialog(
        title = "Make Payment",
        # File type
        includeHTML("payment.html"),
        # File format 
}

advancedFeatureOnline <- showModal(modalDialog(
  title = "Make Payment",
  includeHTML("advanced_featured_server.html")
))

advancedFeatureLocal <- showModal(modalDialog(
  title = "Make Payment",
  includeHTML("advanced_featured_local.html")
))



handle_payment <- function() {
  if (isPaymentMade()) {  # Assuming isPaymentMade() checks if the payment is verified
    options(paymentverified = TRUE)

  } else {
    sendSweetAlert(
      title = "Warning!",
      text = "Your payment has not been made. Please consider making a payment to upgrade and access more features.",
      type = "warning"
    )
  }
}


  # Define observer for project initialization
  observeEvent(input$upgradebtn, {
    if (getOption("server", default = FALSE) && !getOption("paymentverified", default = FALSE) && getOption("authenticated", default = FALSE)) {
      showModal(advancedFeatureOnline)
    } else if (getOption("local", default = FALSE) && getOption("paymentverified", default = FALSE) && getOption("authenticated", default = FALSE)) {
      showModal(advancedFeatureLocal)
    }
  })

  # Define observer for payment button
  observeEvent(input$paymentbtn, {
    if (getOption("server", default = FALSE) && getOption("paymentverified", default = FALSE) && getOption("authenticated", default = FALSE)) {
      # Query to fetch user details
      query <- sprintf("SELECT * FROM users WHERE email = '%s'", email)  # Please adjust this query to fetch user details based on your DB schema
      result <- dbGetQuery(con, query)  # Assuming 'con' is your database connection object
      # Further operations with the result...
    }
  })
}



    # Define observer for project initialization
    observeEvent(input$initialize, {
      if (input$initialize && current_page() == "openPageUI" && credentials[,"permission"][which(credentials$username_id==input$userName)]=="basic"){
        if (nchar(input$proj_name) > 0) {
          current_page("basicUI")
          output$logoutbtn<-renderUI({actionButton('logoutbtn', 'Logout', class = 'action-button logout-btn')})
          removeModal()
        }
      }
    })

    # Define observer for project initialization
    observeEvent(input$go_to_advanced, {
      req(input$go_to_advanced)

      if (current_page() == "processUI" && credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced"){
        current_page("advancedUI")
        output$logoutbtn<-renderUI({actionButton('logoutbtn', 'Logout', class = 'action-button logout-btn')})
        removeModal()
        updateTabsetPanel(session, "tabs_data", selected = "tab3")
      }
    })

    # Define observer for go to advanced page
    observeEvent(input$go_to_advanced_page, {
      req(input$go_to_advanced_page)
      if (current_page() == "samplingUI" && credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced"){
        current_page("advancedUI")
        output$logoutbtn<-renderUI({actionButton('logoutbtn', 'Logout', class = 'action-button logout-btn')})
        updateTabsetPanel(session, "tabs_data", selected = "tab3")
        removeModal()
      }
    })

    # Define observer for go to process page
    observeEvent(input$go_to_process, {
    req(input$go_to_process)
    #verify user has made payment and their role is indeed convert to AdvancedUser
      if (current_page() == "advancedUI" && credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced"){
        current_page("processUI")
        output$logoutbtn<-renderUI({actionButton('logoutbtn', 'Logout', class = 'action-button logout-btn')})
        removeModal()
      }
    })

    # Define observer for go to sampling page
    observeEvent(input$go_to_sampling, {
      req(input$go_to_sampling)
      if (current_page() == "processUI" && credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced"){
        current_page("samplingUI")
        current_page("samplingUI")
        current_page("samplingUI")
        removeModal()
      }
    })

    # Define observer for go back to process page
    observeEvent(input$go_to_process_page, {
      req(input$go_to_process_page)
      if (current_page() == "samplingUI" && credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced"){
        current_page("processUI")
        removeModal()
      }
    })

    # Define output for page UI
    output$pageUI <- renderUI({
      if (current_page() == "landingPageUI") {
        landingPageUI
      } else if (current_page() == "loginUI") {
        loginUI
      } else if (current_page() == "openPageUI") {
        openPageUI
      } else if (current_page() == "adminPageUI") {
        adminPageUI
      }  else if (current_page() == "basicUI") {
        basicUI
      } else if (current_page() == "advancedUI") {
        advancedUI
      }else if (current_page() == "processUI") {
        processUI
      }else if (current_page() == "samplingUI"){
        samplingUI
      }
    })


    # Define observer for logout event
    observeEvent(input$logoutbtn,{
      if (USER$login == TRUE){
        session$reload()
      }
    })
  })
}}









shinyServer(function(input, output, session) {

  summary.correlation <- radiant.basics:::summary.correlation

  enc <- getOption("radiant.encoding")


  ## source data & app tools from radiant.data
  for (file in list.files(
    c(
      file.path(getOption("radiant.path.data"), "app/tools/app"),
      file.path(getOption("radiant.path.data"), "app/tools/data")
    ),
    pattern = "\\.(r|R)$",
    full.names = TRUE)) {
    source(file, encoding = enc, local = TRUE)
  }

  ## list of radiant menu's to include
  rmenus <- c("radiant.data", "radiant.design", "radiant.basics", "radiant.model", "radiant.multivariate")

  ## packages to use for example data
  options(radiant.example.data = rmenus)

  for (i in rmenus[-1]) {
    ## 'sourcing' radiant's package functions in the server.R environment
    if (!isTRUE(getOption("radiant.from.package"))) {
      eval(parse(text = paste0("radiant.data::copy_all(", i, ")")))
      cat(paste0("\nGetting ", i, " from source ...\n"))
    }

    ## help ui
    ipath <- paste0(strsplit(i, "\\.")[[1]], collapse = ".path.")
    source(file.path(getOption(ipath), "app/help.R"), encoding = enc, local = TRUE)

    ## source analysis tools for each app
    for (file in list.files(file.path(getOption(ipath), "app/tools/analysis"), pattern = "\\.(r|R)$", full.names = TRUE))
      source(file, encoding = enc, local = TRUE)
  }

  ## ui creation functions
  source(file.path(getOption("radiant.path.model"), "app/radiant.R"), encoding = enc, local = TRUE)

  ## help ui
  output$help_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        help_data_panel,
        help_design_panel,
        help_basics_panel,
        help_model_panel,
        help_multivariate_panel,
        uiOutput("help_text"),
        width = 3
      ),
      mainPanel(
        HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
        htmlOutput("help_data"),
        htmlOutput("help_design"),
        htmlOutput("help_basics"),
        htmlOutput("help_model"),
        htmlOutput("help_multivariate")
      )
    )
  })

  ## save state on refresh or browser close
  saveStateOnRefresh(session)
})







