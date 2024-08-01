################################################################################
## function for dabtab.basic package
################################################################################

library(aws.s3)
library(dotenv)
library(magick)

# Load .env variables
dotenv::load_dot_env()
AWS_ACCESS_KEY_ID <- Sys.getenv("AWS_ACCESS_KEY_ID")
AWS_SECRET_ACCESS_KEY <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
AWS_DEFAULT_REGION <- Sys.getenv("AWS_DEFAULT_REGION")

Sys.setenv(
  AWS_ACCESS_KEY_ID = AWS_ACCESS_KEY_ID,
  AWS_SECRET_ACCESS_KEY = AWS_SECRET_ACCESS_KEY,
  AWS_DEFAULT_REGION = AWS_DEFAULT_REGION
)

# Query to fetch user details
userSession <- isolate(session$user) # Get the current user's session name
query <- sprintf("SELECT * FROM users WHERE userSessionName = '%s'", userSession)
result <- dbGetQuery(global_db$con, query)

# Define the AWS S3 access point ARN
access_point_arn <- "arn:aws:s3::036825736115:accesspoint/mum5hhigkfk3u.mrap"
access_point_arn <- "arn:aws:s3::036825736115:accesspoint/mum5hhigkfk3u.mrap"

# Create the AWS S3 client using the access point ARN
s3_host <- get_bucket(
  bucket = "dabtabsharedataset",
  access_policy_arn = access_point_arn
)

s3_share <- get_bucket(
  bucket = "dabtabsharedataset",
  access_policy_arn = access_point_arn
)


userUUIDval <- reactiveVal(NULL)

# Create a unique folder on AWS S3 for each unique user
if (isTRUE(getOption(dabtab.dabtab.authenticated))) {
  if (!result$userPermission == "Allow") {
    if (nrow(result) > 0) {
      userUUIDval <- isolate(result$userUUID)
      folder_name <- userUUIDval

      tryCatch({
        put_object(data = NULL, object = folder_name, s3 = s3_host)
        cat("Operation has been executed successfully.\n", file = "success.log", append = TRUE)
      }, error = function(e) {
        cat(paste("Error 101: Folder creation has failed for user", userUUID, "\n"), file = "error.log", append = TRUE)
      })

    } else {
      cat(paste("Error: Can't create folder for the user", userUUID, "\n"), file = "error.log", append = TRUE)
    }
  } else if (!result$userPermission == "Suspend") {
    cat(paste("Error 102: User Account ID", userUUID, "has been suspended. See more details.\n"), file = "error.log", append = TRUE)
  } else if (!result$userPermission == "Prohibit") {
    cat(paste("Error 103: User Account ID", userUUID, "has been permanently terminated. See more details.\n"), file = "error.log", append = TRUE)
  }
} else {
  cat("Unknown error occurred.\n", file = "error.log", append = TRUE)
}


#ui select file format
output$file_type<-renderUI({
    selectInput(
      inputId = "file_type",
      label = "Choose file format",
      choices = c("","csv", "rds", "rda", "rdata","clipboard", "url"),
      selected = ""
    )
  })


#ui upload menu
output$ui_upload <- renderUI({
  wellPanel(
    actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle")),
    shinyjs::hidden(uiOutput("file_type")),
    uiOutput("dataSelect"),
    actionButton("upload_file", "Upload File"),
    actionButton("upload_folder", "Upload Folder"),
    pickerInput(
      inputId = "filter_data",
      label = "Choose Data Format",
      choices = c("csv", "rds", "rda", "rdata", "txt"),
      multiple = TRUE,
      selected = "all",
      options = pickerOptions(
        actionsBox = TRUE,
        title = "Please select data formats to display"
      ),
      choicesOpt = list(
        content = sprintf(
          "<span class='label label-%s'>%s</span>",
          c("csv", "rds", "rda", "rdata", "txt"),
          paste(c("csv", "rds", "rda", "rdata", "txt"))
        )
      )
    )
})


#obser event upload file to server
observeEvent(input$uploadFileServer, {
  shinyjs::show("file_type")
  shinyjs::hide("file_progress")
  isFile = TRUE

  if (file_type() != "" & file_type() != "clipboard" & file_type() != "url") {
    path_choose <- choose_files(isolate(file_type()), multi = TRUE)

    if (length(path_choose) > 0) {
      withProgress(
        message = 'Uploading file...',
        value = 0,
        for (i in 1:15) {
          incProgress(1/15)
          Sys.sleep(0.2)
        }
      )

      accepted_formats <- c("csv", "rds", "rda", "rdata")

      filename <- basename(path_choose)
      file_ext <- tools::file_ext(filename)

      if (!file_ext %in% accepted_formats) {
        # File format is not in the accepted list
        sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = "Please select the correct file format.",
          type = "warning"
        )
        return(NULL)  # Return nothing to prevent further execution
      } else {
      new_files <- data.table(directory = file.path(dirname(path_choose), '/'), filename = filename, file_ext = file_ext)
      
      filepath$path <- rbind(filepath$path, new_files)

      n_files <- nrow(unique(new_files))

      # Upload the file to the specified folder in AWS S3
      upload_object(folder_name = userUUIDval, object = path_choose, s3 = s3)

      # Generate and store thumbnail of the file or folder
      thumbnail_img <- thumnail_generator(filename, file_ext, isFile)

      # Append the thumbnail to the thumnailList
      thumnailList(c(thumnailList(), thumbnail_img))

      # Append thumbnail list to the box element
      fileViewer(thumbnailList, userUUIDval)

        if (n_files == 1) {
          latest_file$val <- new_files$filename
          sendSweetAlert(
            session = session,
            title = "Success !!",
            text = paste0("Successfully uploaded '", unique(new_files$filename), "'."),
            type = "success"
          )
        } else {
          latest_file$val <- tail(new_files$filename, n = 1)
          sendSweetAlert(
            session = session,
            title = "Success !!",
            text = paste0("Successfully uploaded '", n_files, "' files."),
            type = "success"
          )
        }

        updateProgressBar(session, "file_progress", value = 0)
        Sys.sleep(3)
        shinyjs::hide("file_progress")
      }
    } else {
      # Assign an empty data frame to filepath$path1 if no file is selected
      new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf))
      )
      filepath$path <- rbind(filepath$path, new_files)
      sendSweetAlert(
        session = session,
        title = "Warning !!!",
        text = "You have not selected any file!",
        type = "warning"
      )
    }
    updateSelectInput(session, "file_type", selected = "")
    updateActionButton(session, "upload_file", label = "Upload File")
  }
})


# observe event upload folder to server
observeEvent(input$uploadFolderServer, {
  shinyjs::hide("file_type")
  isFile = FALSE
  path_choose <- choose.dir()
  if (is.null(path_choose)) {
    new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf)))
    filepath$path <- rbind(filepath$path, new_files)
    output$output <- renderText({
      paste0("No folder selected.")
    })
  } else {
    path_dir <- paste0(gsub("\\\\", "/", path_choose), '/')
    files <- list.files(path = path_choose)
    files <- unique(files)
    len_file <- length(files)
    file_exts <- tools::file_ext(files)
    new_files <- data.table(directory = path_dir, filename = files, file_ext = file_exts)
    if (len_file == 0) {
      new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf)))
      filepath$path <- rbind(filepath$path, new_files)
      output$output <- renderText({
        paste0("Selected folder '", path_choose, "' is empty.")
      })
    } else {
      new_files_1 <- new_files[file_ext %in% ext_pattern]
      if (nrow(new_files_1) > 0) {
        filepath$path <- rbind(filepath$path, new_files_1)
        latest_file$val<-tail(new_files$filename, n = 1)

        # Upload the file to the specified folder in AWS S3
        upload_object(folder_name = userUUIDval, object = path_choose, s3 = s3)

        # Generate and store thumbnail of the file or folder
        thumbnail_img <- thumnail_generator(path_dir, file_ext, isFile)

        # Append the thumbnail to the thumnailList
        thumnailList(c(thumnailList(), thumbnail_img))

        # Append thumbnail list to the box element
        fileViewer(thumbnailList, userUUIDval)


      output$output <- renderText({
          paste0("Successfully uploaded folder '", path_dir, "'.")
        })
      } else {
        new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf)))
        filepath$path <- rbind(filepath$path, new_files)
        output$output <- renderText({
          paste0("Selected folder '", path_choose, "' does not contain any valid files.")
        })
      }
    }
  }
  updateActionButton(session, "upload_folder", label = "Upload Folder")
})


thumnailList <- reactiveVal(NULL)

#get object from AWS S3
getAWSObject <- function (folder, object) {
  # Upload the file to the specified folder in AWS S3
  upload_object(folder_name = folder, object = object, s3 = s3)

}

#delete object from AWS S3
deleteAWSObject <- function (folder, object) {
  # Upload the file to the specified folder in AWS S3
  upload_object(folder_name = folder, object = object, s3 = s3)

}

#rename object from AWS S3
renameAWSObject <- function (folder, object) {
  # Upload the file to the specified folder in AWS S3
  upload_object(folder_name = folder, object = object, s3 = s3)

}


# Function to generate thumbnails
thumnail_generator <- function(folder, filename, file_ext, isFile) {

  thumbnail_dir <- system.file("inst/tools/www/thumbnail", package = "dabtab.basic")

  thumbnail_list <- list.files(thumbnail_dir)

  thumbnail_name <- tolower(substring(thumbnail_list, 1, nchar(thumbnail_list) - 4))

  thumbnail_path <- if (isFile && (thumbnail_name %in% tolower(file_ext))) {
    file.path(thumbnail_dir, thumbnail_list[which(thumbnail_name == tolower(file_ext))])
  } else {
    file.path(thumbnail_dir, "folder.png")
  }

  thumbnail_img <- image_scale(image_read(thumbnail_path), "100x100")  # Adjust the size as needed

  thumbnail_div <- tag$div(
    style = "text-align: center; display: inline-block;",
      "Click here for info",
    tags$img(src = thumbnail_path, width = "100px", height = "100px"),
    tags$p(filename)
  )


  v <- reactiveValues()

  onclick("coords", function(event) { alert(event) })

  ## Examples of other events we might use
  #onclick("bins", v$click <- rnorm(1))
  #onevent("hover", "bins", v$click <- rnorm(1))
  #onevent("dblclick", "bins", v$click <- rnorm(1))
  #onevent("mousedown", "bins", v$click <- rnorm(1))

  ## The actual event we have used.
  onclick("image", function(event) {v$clickX <- event$pageX
                                    v$clickY <- event$pageY
                                    })

  output$panelObjectEvent <- renderUI({

    if (!is.null(v$clickX)){
      style <- paste0("position:absolute; z-index:100; background-color: rgba(100, 245, 245, 0.85); max-width: 250px; width: 250px;",
                                          "left:", v$clickX + 2, "px; top:", v$clickY - 50, "px;")
                          # actual tooltip created as wellPanel
                          wellPanel(
                            id = "aws_panel",
                            style = style,
                            p(HTML(paste0("<b> KPI: </b>", "bla", "<br/>",
                                          "<b> Information: </b>", "aText"))),
                            actionButton("deleteBtn", "Delete"), 
                            actionButton("renameBtn", "Rename"),
                            actionButton("cancelBtn", "Cancel"),
                          )
    }
    else return(NULL)
  })

  objectSelected <- getAWSObject (folder, filename)

  observeEvent(input$deleteBtn, {
    deleteAWSObject (folder, objectSelected)
    v$clickX = NULL
  })

  observeEvent(input$renameBtn, {
    renameAWSObject (folder, objectSelected)
    v$clickX = NULL
  })

  observeEvent(input$cancelBtn, {
    return()
    v$clickX = NULL
  })

  return(thumbnail_div)
}

#representative image of the folder structure
fileViewer <- function(thumbnailList, folderName) {

  # Create a folder name tag at the top left corner
  folderNameTop <- tag$div(
    style = "position: absolute; top: 5px; left: 5px; color: black; font-size: 18px;",
    folderName
  )

  # Create a cancel button at the top right corner
  cancelTop <- tag$a(
    class = "close",
    style = "position: absolute; top: 5px; right: 5px; color: red; font-size: 24px;",
    HTML("&times;")
  )
  
  # Create a cancel button at the bottom left corner
  cancelBottom <- tag$a(
    class = "close",
    style = "position: absolute; bottom: 5px; left: 5px; color: red; font-size: 24px;",
    HTML("&times;")
  )
  
  # Combine the cancel buttons with the thumbnail elements
  viewerBox <- tag$div(
    style = "position: relative;",
    thumnailList,
    cancelTop,
    cancelBottom
  )
  
  # Observe event for cancel buttons to hide the viewerBox
  observeEvent(input$cancelTop, {
    hide("viewerBox")
  })
  observeEvent(input$cancelBottom, {
    hide("viewerBox")
  })


  observeEvent(input$viewUpload, {
    show("viewerBox")
  })
  
  return(viewerBox)
}


#save project >>> save datasets >>> save session  
#by default data will be loaded from user folder in AWS S3 bucket

# Save project >>> save datasets >>> save session
# By default data will be loaded from the user's folder in AWS S3 bucket

generateSharedTable <- function(foldername) {
  list_objects <- get_list(folder_name = foldername)
  colnames <- c('Data', 'ShareWith', 'Restrict', 'Level')
  nrows <- length(list_objects)
  tab <- matrix("", nrow = nrows, ncol = length(colnames))
  colnames(tab) <- colnames
  rownames(tab) <- list_objects

  # Create a reactiveValues to track user selections
  rv <- reactiveValues(level = rep("Base", nrows))

  output$myTable <- renderDataTable({
    tab_df <- data.frame(tab, stringsAsFactors = FALSE)
    for (i in 1:nrows) {
      tab_df$Level[i] <- selectizeInput(
        inputId = paste0("level_", i),
        label = NULL,
        choices = c("Base", "All"),
        selected = rv$level[i]
      )
      # Event to track cell click
      onclick <- sprintf('var table = $("#myTable").DataTable();\
                         table.on("click", "td", function() {\
                         var colIdx = table.cell(this).index().column;\
                         var rowIdx = table.cell(this).index().row;\
                         Shiny.setInputValue("clicked_cell", [rowIdx, colIdx]);\
                         });')
      # Execute the onclick event
      shinyjs::runjs(onclick)
    }

    # Update table based on user's selection
    observeEvent(input$myTable_cell_edit, {
      info <- input$myTable_cell_edit
      if (!is.null(info$value) && info$col == "Level") {
        row <- info$row
        level <- info$value
        rv$level[row] <- level
        if (level == "All" && isTrue(isFolder(foldername, list_objects[row]))) {
          # If "All" selected and the object is a folder, append its files
          folder_files <- get_list(folder_name = file.path(foldername, list_objects[row]))
          new_rows <- length(folder_files)
          tab_df <- rbind(tab_df, matrix("", nrow = new_rows, ncol = length(colnames)))
          rownames(tab_df)[(nrows + 1):(nrows + new_rows)] <- folder_files
          tab_df[(nrows + 1):(nrows + new_rows), "Level"] <- "Base"  # Set level for added rows
          nrows <- nrow(tab_df)  # Update the total row count
        }
      }
    })


    # Event to handle cell click
    observeEvent(input$myTable_cells_selected, {
      clicked_cell <- input$myTable_cells_selected
      # Get cell coordinates
      rowIdx <- clicked_cell[1]
      colIdx <- clicked_cell[2]

      # Check if it's a cell click in the "Data" column
      if (!is.null(rowIdx) && !is.null(colIdx)) {
        # If click in the "Data" column
        # Add logic here to show wellPanel with options: INSERT EMAIL, COPY FROM, CHOOSE FROM
        # For instance, create a wellPanel with action buttons for the required functionality
        well_panel <- wellPanel(
          h5("Options"),
          actionButton("insertEmail", "Insert Email"),
          actionButton("copyFrom", "Copy From"),
          actionButton("chooseFrom", "Choose From")
        )
        output$panelShareEvent <- renderUI({
          well_panel
        })
      }
    })

    datatable(tab_df, escape = FALSE, selection = "none")
  })
}


#observe event for insertEmail button 
email <- reactiveVal()
# Creating an empty data frame to store email information
emailDF <- data.frame(cell = NA, emailList = NA)

observeEvent(input$insertEmail, {
  textInput(id="email", placeholder = "Enter email abc@gmail.com")
  email <- isolate(input$email)  # Get the inserted email value
  # Append the email to the tag list, checking and adjusting the width and height
  # For instance, here is a hypothetical function to adjust the tag list display
  selected_cell <- input$myTable_cells_selected
  tagListUpdated <- handleTagListInsert(selected_cell, tagList, email)

  output$tagList <- renderUI({
    tagListUpdated
  })
})

#observe event for copyFrom button 
observeEvent(input$copyFrom, {
  # Logic to append a list of options from other cells, excluding the current cell
  # For instance, a hypothetical function call:
  cellOptionsUI <- getOptionsForCopy(input$myTable_cell_edit)  # Fetch options from cells
 
})

#observe event for chooseFrom button 
observeEvent(input$chooseFrom, {
  # Logic to let users choose from a predefined table share list they got
  # For example, rendering a pre-defined share list in the UI:
  chosenListUI <- generate_chooseFrom_ui(table)
  output$chosenList <- renderUI({
    chosenListUI
  })
})

#function to handle insert email list
handleTagListInsert <- function(cell, tagList, email, cellWidth, cellHeight) {
  # Append the inserted email to the tag list
  tagElements <- lapply(emailList(), function(email) {
    tag <- tags$div(class = "tag", email, tags$span(class = "close", "×"))
    shinyjs::onclick(paste0(".close:contains('", email, "')"), {
      emailList(emailList()[emailList() != email])
    })
    tag
  })

 # Update the emailDF data frame with cell position and emailList
  emailDF <- rbind(emailDF, data.frame(cell = cell, emailList = email))

  if (sum(width(tagElements)) > cellWidth) {
    cellHeight += cellHeight  # Increase the cell height
    # Append the inserted email to the next line of this cell
    do.call(tagList, rbind(cell, tagElements))
  }

  # Check and adjust the width and height if necessary
  # Logic to handle tag list width and height, adjust as required

  #return updatedTagList  # Return the modified tag list
}


#Function to handle copy from options
getOptionsForCopy <- function(df, clickedCell) {
  clicked_cell <- input$myTable_cells_selected

  # Get cell coordinates
  rowIdx <- clicked_cell[1]
  colIdx <- clicked_cell[2]

  # Extract values from the specified columns excluding the clicked cell and column name
  options_shareWith <- df[, "shareWith"]
  options_shareWith <- options_shareWith[-c(rowIdx, colIdx)]

  options_restrict <- df[, "Restrict"]
  options_restrict <- options_restrict[-c(rowIdx, colIdx)]

  # Combining unique options from both columns
  options <- unique(c(options_shareWith, options_restrict))

  # Render the options as a wellPanel with action buttons
  well_panel <- wellPanel(
    h5("Copy Options"),
    lapply(options, function(opt) {
      actionButton(opt, label = opt)
    })
  )

  output$cellOptionsUI <- renderUI({
    well_panel
  })

  observeEvent(input$opt, {
    req(input$opt)
    df[input$myTable_cells_selected] <- df[input$opt]
    emailDF <- rbind(emailDF, data.frame(cell = input$myTable_cells_selected, emailList = df[input$opt]))
  })
}

#function to handle chooseFrom UI
generate_chooseFrom_ui <- function() {
  isolate({
    if ("share_list" %in% names(input) && is.null(input$share_list)) {
      contact_tb$share_list <- NULL
    } else {
      vars <- groupable_vars()  # Initialize 'vars' here or get it from somewhere
      if (available(contact_tb$share_list) && all(contact_tb$share_list %in% vars)) {
        vars <- unique(c(contact_tb$share_list, vars))
        names(vars) <- varnames() %>%
          (function(x) x[match(vars, x)]) %>%
          names()
         emailList <- cbind(emailList, input$share_list)
         emailDF <- rbind(emailDF, data.frame(cell = cell, emailList = input$share_list))
      }
    }
  })

  selectizeInput(
    "share_list",
    label = "Share List",
    choices = vars,
    selected = state_multiple("share_list", vars, isolate(input$pvt_cvars)),
    multiple = TRUE,
    options = list(
      placeholder = "Select categorical variables",
      plugins = list("remove_button", "drag_drop")
    )
  )
}

# observe event for share data button
  observeEvent(input$sharedData, {
    sharedTable <- generateSharedTable(foldername)
    output$sharedTable_ui <- renderUI(sharedTable)
})


########
##>>>THE PURPOSE OF RESTRICT IS FOR RESTRICT USERS IN PROJECT PHASE(i.e., you might add user A at the starting phase of project 
## and later restrict them from accessing data resource)
#######
#check shared user infor
check_guest_infor <- function(df, emailDF) {
  options(dabtab.check_infor = TRUE)
  df_check <- df
  invalidInput <- character(0)
  valid_access <- character(0)
  valid_restrict <- character(0)

  for (i in 1:nrow(df)) {
    emailListshareWith <- emailDF[cell == paste0("A", i), "emailList"]
    emailListRestrict <- emailDF[cell == paste0("B", i), "emailList"]
    cellDF <- df[i, c("Restrict", "shareWidth")]

    if (!is.na(emailListshareWith) && !grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}$", emailListshareWith)) {
      invalidEmailInput <- emailListshareWith
      cat(paste("Error 106: Invalid email found for user", df[i, "shareWith"], "\n"), file = "error.log", append = TRUE)
      df_check$InvalidEmailAccess[i] <- invalidEmailInput
      invalidInput <- c(invalidInput, df[i, "shareWith"])
    } else if (is.na(emailListshareWith)) {
      emptyCellList <- ""
      showModal(modalDialog(
        paste("Register at cell ", df[i, "shareWith"]),
        footer = tagList(
          actionButton("proceed", "Proceed"),
          actionButton("", ""),
          actionButton("cancel", "Cancel")
        )
      ))
      cat(paste("Error 107: Empty list for data table at", df[i, "shareWith"], "\n"), file = "error.log", append = TRUE)
    } else {
      query <- "SELECT * FROM users"
      result <- dbGetQuery(global_db$con, query)
      
      if (!(df[i, "shareWith"] %in% result$email)) {
        userNotRegister <- df[i, "shareWith"]
        showModal(modalDialog(
          paste("Please check these users:", userNotRegister, "Remove from the share list or request these users to register to proceed."),
          footer = tagList(
            actionButton("proceed", "Proceed"),
            actionButton("", ""),
            actionButton("cancel", "Cancel")
          )
        ))
        cat(paste("Error 108: Identity checking failed for user", df[i, "shareWith"], "\n"), file = "error.log", append = TRUE)
        invalidInput <- c(invalidInput, df[i, "shareWith"])
      }
    }
  }

  if (length(invalidInput) > 0) {
    showModal(modalDialog(
      "Please check these users. Remove from the share list or request these users to register to proceed.",
      footer = tagList(
        actionButton("proceed", "Proceed"),
        actionButton("", ""),
        actionButton("cancel", "Cancel")
      )
    ))
  } else {
    showModal(modalDialog(
      "Checking has been successful for the operation",
      footer = tagList(
        actionButton("proceed", "Proceed"),
        actionButton("", ""),
        actionButton("cancel", "Cancel")
      )
    ))
    cat("Success share information checking\n", file = "success.log", append = TRUE)
    valid_access <- c(valid_access, df[i, "shareWith"])
    valid_restrict <- c(valid_restrict, df[i, "Restrict"])
  }

  valid_restrict <- unique(valid_restrict)
  valid <- setdiff(valid_access, valid_restrict)

  df_check$ValidAccess <- NA
  df_check$ValidAccess[df_check$shareWith %in% valid_access] <- df_check[df_check$shareWith %in% valid_access, "shareWith"]
  df_check$ValidRestrict <- NA
  df_check$ValidRestrict[df_check$Restrict %in% valid_restrict] <- df_check[df_check$Restrict %in% valid_restrict, "Restrict"]
  df_check$Valid <- NA
  df_check$Valid[df_check$shareWith %in% valid] <- df_check[df_check$shareWith %in% valid, "shareWith"]

  return df_check
}

df_check_result <- reactiveVal(access=NULL, deny=NULL)

#manually checking if you will
observeEvent(input$check_infor) {
  check_guest_infor(df, column)
}

# observe event for share data button
  observeEvent(input$proceed, {
    showModal(modalDialog(
      "Proceed will ignore those invalid inputs and send notifications only to those valid.",
    ))
})


# Function to get userUUID
getUserUUID <- function(email) {
  # Query to fetch user details
  query <- sprintf("SELECT * FROM users WHERE userEmail = '%s'", email)
  result <- dbGetQuery(global_db$con, query)
  return result$userUUID
}

# Function to generate folder name
generateFolderName <- function(valid_access, ownerUUID) {
  # Retrieve user UUIDs for guests
  guestUUIDs <- sapply(valid_access, getUserUUID)
  # Concatenate UUIDs into the folder name
  folderName <- paste(ownerUUID, guestUUIDs, sep = "-")
  return folderName
}

# Observe event for share data button
observeEvent(input$sumbit_share, {
  df_check_result <- check_guest_infor(df, emailDF)
  df_check_result$data <- df_check_result$Data
  df_check_result$access <- df_check_result$valid_access
  df_check_result$deny <- df_check_result$valid_restrict
  # Assuming the owner's user UUID is known (replace 'ownerUUID' with the actual user UUID)
  ownerUUID <- userUUIDval# Replace with actual owner UUID
  #call apply all to trigger share event
  applyAll( ownerUUID, df_check_result$access, check_result$deny, df_check_result$data)
})


#create pair share folder - intermediary storage
create_pair_folder <- function(folder_name, userUUID, result) {
  if (isTRUE(getOption(dabtab.dabtab.authenticated))) {
    if (nrow(result) > 0) {
      if (result$userPermission == "Allow") {
        tryCatch({
          put_object(data = NULL, object = folder_name, s3 = s3_host)
          cat("Operation has been executed successfully.\n", file = "success.log", append = TRUE)
        }, error = function(e) {
          cat(paste("Error 101: Folder creation has failed for user", userUUID, "\n"), file = "error.log", append = TRUE)
        })
      } else if (result$userPermission == "Suspend") {
        cat(paste("Error 102: User Account ID", userUUID, "has been suspended. See more details.\n"), file = "error.log", append = TRUE)
      } else if (result$userPermission == "Prohibit") {
        cat(paste("Error 103: User Account ID", userUUID, "has been permanently terminated. See more details.\n"), file = "error.log", append = TRUE)
      } else {
        cat("Unknown error occurred.\n", file = "error.log", append = TRUE)
      }
    } else {
      cat(paste("Error: Can't create folder for the user", userUUID, "\n"), file = "error.log", append = TRUE)
    }
  } else {
    cat("Authentication error occurred.\n", file = "error.log", append = TRUE)
  }

  return(folder_name)
}

# Create the policy
create_policy <- function(effect, objectARN, accessAction) {
  policy <- sprintf('{
    "Version": "2012-10-17",
    "Statement": [
      {
        "Effect": "%s",
        "Action": [
          "%s"
        ],
        "Resource": [
          "%s"
        ]
      }
    ]
  }', effect, accessAction, objectARN)
  return(policy)
}


# alternative user can use a presigned URL to grant temperary access to AWS resource
# with this approach the policy rendering invalid
generate_presigned_url <- function(bucket, object_key) {
  # Generate a presigned URL for an object in the bucket
  presigned_url <- getURL(
    object = object_key,
    bucket = bucket,
    operation = "GetObject",
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    token = Sys.getenv("AWS_SESSION_TOKEN"),  # if using temporary credentials
    method = "GET",
    expires = 3600  # URL expiration time in seconds
  )
  
  return(presigned_url)
}

# Use the function to generate the presigned URL
url <- generate_presigned_url("your-bucket-name", "your-object-key")



# Function to apply policies
applyAll <- function(ownerUUID, listAcess, listDeny, object) {
  folderName <- generateFolderName(listAcess, ownerUUID)
  create_pair_folder(folderName)

  access_point_arn <- "arn:aws:s3::036825736115:accesspoint/mum5hhigkfk3u.mrap"
  objectARN <- paste(access_point_arn, object, sep = "/")

  # Generate policies for allowing access
  policyAllow <- create_policy("Allow", objectARN, "s3:*")
  
  # Generate policies for denying access
  policyDeny <- create_policy("Deny", objectARN, "s3:*")

  # Apply Allow policy to all users in the listAcess
  sapply(listAcess, function(user) {
    s3_put_user_policy <- isolate(user_name = user, policy_arn = policyAllow)
  })

  # Apply Deny policy to all users in the listDeny
  sapply(listDeny, function(user) {
    s3_put_user_policy <- isolate(user_name = user, policy_arn = policyDeny)
  })
}

#convert matrix to table 
tab <- as.table(tab)

observeEvent(input$help_keyboard, {
  showModal(
    modalDialog(
      title = "Keyboard shortcuts",
      h4("General"),
      ## based on https://github.com/swarm-lab/editR/blob/master/inst/app/bits/keyboard.R
      tab <- matrix(c(7, 5, 14, 19, 3, 2, 17, 6, 12), ncol=3, byrow=TRUE)
      colnames(tab) <- c('colName1','colName2','colName3')
      rownames(tab) <- c('rowName1','rowName2','rowName3')
      tab <- as.table(tab)
          
      ),
      footer = modalButton("OK"),
      size = "l",
      easyClose = TRUE
    )
  )
})


#Function to pool data if owner allow as well

#the idea is given the shared folder if you as guest want to push resource onto the shared foler then notify the host for permission 
#if they allow then you can operate push object


##########################################
##>>>NOTIFICATION SYSTEM <for untriggered event use ServiceWorkers, for triggered event use WebSocket>
###############################

## function to load javascript
help_menu <- function(hlp) {
  tagList(
    navbarMenu(
      "",
      icon = icon("question-circle", verify_fa = FALSE),
      tabPanel("Help", uiOutput(hlp), icon = icon("question", verify_fa = FALSE)),
      tabPanel(actionLink("help_keyboard", "Keyboard shortcuts", icon = icon("keyboard", verify_fa = FALSE))),
      # tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
      tabPanel(tags$a(
        "",
        href = "https://radiant-rstats.github.io/docs/tutorials.html", target = "_blank",
        list(icon("film", verify_fa = FALSE), "Videos")
      )),
      tabPanel("About", uiOutput("help_about"), icon = icon("info", verify_fa = FALSE)),
      tabPanel(tags$a(
        "",
        href = "https://radiant-rstats.github.io/docs/", target = "_blank",
        list(icon("globe", verify_fa = FALSE), "Radiant docs")
      )),
      tabPanel(tags$a(
        "",
        href = "https://github.com/radiant-rstats/radiant/issues", target = "_blank",
        list(icon("github", verify_fa = FALSE), "Report issue")
      ))
    ),
    # bslib::nav_item(checkboxInput("dark_mode", label = "Dark Mode", width="100px")),
    tags$head(
      tags$script(src = "js/service-worker.js"),
      tags$script(src = "https://js.stripe.com/v3/")
      tags$script(src = "js/firebaseMessaging.js"),
      tags$script(src = "js/firebase-messaging-sw.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css"),
      tags$link(rel = "shortcut icon", href = "imgs/icon.png")
    )
  )
}


# append event metadata to a JSON file
appendEventJson <- function(msg, jsonFilePath) {
  eventJSON <- toJSON(msg)
  
  # Check if the file already exists
  if (file.exists(jsonFilePath)) {
    # If the file exists, append the event information
    writeLines( eventJSON, jsonFilePath, append = TRUE )
  } else {
    # If the file does not exist, create a new file and write the event information
    writeLines(eventJSON, jsonFilePath)
  }
}

#take event information
eventInfo <- function(jsonFilePath) {
  # Read the JSON file
  json_data <- fromJSON(jsonFilePath)

  # Extract eventType and timestamp
  eventType <- json_data$EventType
  timestamp <- json_data$Timestamp

  return(list(eventType = eventType, timestamp = timestamp))
}


# Function to insert event metadata into the database table
insert_events <- function(userSession, userUUID, timestamp, eventType, eventSource) {
    query <- sprintf(
        "INSERT INTO events (userSession, userUUID, timestamp, eventType, eventSource) VALUES ('%s', '%s', '%s', '%s', '%s')",
        userSession, userUUID, timestamp, eventType, eventSource
    )
    dbExecute(con, query)
}

# Query to fetch user details
userSession <- isolate(session$user) # Get the current user session name
query <- sprintf("SELECT * FROM users WHERE userSessionName = '%s'", userSession)
result <- dbGetQuery(global_db$con, query)

# define trigger events
triggered_events <- list("shareData", "update") 

# Function to retrieve event information from a JSON file
eventInfo <- function(jsonFilePath) {
  json_data <- fromJSON(file = jsonFilePath)
  eventType <- json_data$EventType
  timestamp <- json_data$Timestamp
  return(list(eventType = eventType, timestamp = timestamp))
}

# Function to insert event metadata into the database table
insert_events <- function(userSession, userUUID, timestamp, eventType, eventSource) {
  query <- sprintf(
    "INSERT INTO events (userSession, userUUID, timestamp, eventType, eventSource) VALUES ('%s', '%s', '%s', '%s', '%s')",
    userSession, userUUID, timestamp, eventType, eventSource
  )
  dbExecute(con, query)
}

# Query to fetch user details
userSession <- isolate(session$user) # Get the current user session name
query <- sprintf("SELECT * FROM users WHERE userSessionName = '%s'", userSession)
result <- dbGetQuery(global_db$con, query)

triggered_events <- c("shareData", "update")  # List of triggered events

if (getOption("userNotificationAllowed", default = TRUE)) {
  eventInformation <- eventInfo("event.json")
  
  recordedTimestampDB <- result$timestamp
  recordedTimestampJSON <- eventInformation$timestamp
  
  eventTypeDB <- result$eventType
  eventTypeJSON <- eventInformation$eventType
  
  for (i in 1:length(recordedTimestampDB)) {
    time_difference <- difftime(Sys.time(), as.POSIXct(recordedTimestampDB[i]))
    
    if (!is.na(recordedTimestampDB[i]) && !is.na(recordedTimestampJSON)) {
      if (time_difference <= 172800 && eventTypeJSON %in% triggered_events && eventTypeJSON != eventTypeDB) {

        session$sendCustomMessage("requestPermission", list(topic = "triggered_event", eventDetails = js_event))

        # Record the event to the database
        insert_events(userSession, userUUID, eventInformation$timestamp, eventInformation$eventType, "eventSource") 
        # Store the event details in the JSON file for appending
        writeEventJson(eventInformation, "event.json")
      }
    }
  }
}


##########################################
##>>>MESSAGE SYSTEM <for untriggered event use ServiceWorkers, for triggered event use WebSocket>
###############################

################################################################################
## function for dabtab.basic package
################################################################################

library(shiny)
library(httr) 

  chat_log <- character(0)

  observeEvent(input$send_message, {
    user_message <- input$user_message
    chat_log <- c(chat_log, paste("You:", user_message))
    
    # Make a request to the OpenAI API with the user's message
    api_url <- "https://api.openai.com/v1/engines/davinci-codex/completions"
    api_key <- "YOUR_API_KEY"
    
    response <- httr::POST(api_url, 
      httr::add_headers(Authorization = paste("Bearer", api_key)),
      body = list(prompt = user_message, max_tokens = 50)
    )
    
    # Get the chatbot's response from the API
    chatbot_response <- httr::content(response, "text")
    chat_log <- c(chat_log, paste("Chatbot:", chatbot_response))
    
    output$chat_output <- renderText({ chat_log })
  })
}


###############################
##THEME
###################################

theme_list <- c("cerulean","cosmo","cyborg","darkly","flatly","journal","lumen","paper","readable","sandstone","simplex","slate","spacelab","superhero","united","yeti")


ui_theme <- renderUI(
  wellPanel(
    tabsetPanel(
      tabPanel("Edit Live", 
               actionButton("themeEditLive", "Edit Live")
      ),
      tabPanel("Theme Select", 
               actionButton("themeSelector", "Theme Select")
      ),
      tabPanel("Theme Transfer", 
               selectInput("session", "Select Session", choices = "")
      ),
      tabPanel("Reset Theme", 
               actionButton("resetTheme", "Reset Theme")
      )
    )
  )
)

# observe event for theme live editting
observeEvent(input$themeEditLive, {
    shinythemes::themeSelector()
})
  
# observe event for theme selector
observeEvent(input$themeSelector, {
  showModal(
    modalDialog(
      selectInput("themeInput", "Choose Theme", choices = theme_list)
    )
  )
  
observeEvent(input$themeInput, {
  theme <- bslib::bs_theme(version = 4, bootswatch = isolate(input$themeInput))
     session$setCurrentTheme(theme)
  })
})


# Observe event for theme transferring between sessions
observeEvent(input$themeTransfer, {
  default_theme <- bslib::bs_theme(version = 4)
  setCurrentTheme(default_theme)
})



# Observe event for theme reset to default theme
observeEvent(input$session, {
  current_session <- session$token
  session_list <- listShinySessions() %>%
    filter(token != current_session)

  current_theme <- getCurrentTheme()

  showModal(
    modalDialog(
      selectInput("session", "Select Session", choices = session_list, multiple = TRUE)
    )
  )
  
  observeEvent(input$session, {
    selected_sessions <- session_list[input$session]
    for (sess in selected_sessions) {
      sess$setCurrentTheme(current_theme)
    }
  })
})

###############################
##SWITCH
###################################

# Observe event for switch between session
observeEvent(input$switch, {
  current_session <- session$token
  session_list <- listShinySessions() %>%
    filter(token != current_session)

  showModal(
    modalDialog(
      selectInput("session", "Select Session", "Choose Session", choices = session_list)
    )
  )

  observeEvent(input$session, {
    selected_session <- isolate(input$session)
    session$doBookmark()
    updateQueryString("session", selected_session)
  })
})



###############################
##FORMULA INFORMATION
###################################

buitinFormula <-reactiveVal(select=NULL)

observeEvent(input$checkFormula, {
  req(input$selectedFormula)
  if (input$selectedFormula %in% names(list_formula_help)) {
    showModal(modalDialog(list_formula_help[[input$selectedFormula]]))
  }
})



###############################
##FORMULA UI
###################################

formula_list <-  list_funcs(dabtab.basic)

output$ui_calculation <- renderUI({
  tagList(
    wellPanel(
      actionButton("intro_1", "Quick Intro"),
      fluidRow(
         column(width = 12,
                 actionBttn(inputId = "builtin_formula", label = "BuiltIn Formulas", style = "material-flat", color = "primary", block = TRUE,icon = icon("eraser"))
          ),
          column(width = 12,
                 textAreaInput(inputId = "argument", label = "Function Arguments", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt"))
          ),
          column(width = 12,
                 actionBttn(inputId = "custom_formula", label = "Custom Formulas", style = "material-flat", color = "royal",block = TRUE,icon = icon("pencil-alt"))
          ),
          column(width = 12,
                 textAreaInput(inputId = "range_apply", label = "Input Range", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt"))
                 actionButton(inputId = "select_input_range", label = "Select Range", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt")) 
                 actionButton(inputId = "lock_input_range", label = "Lock", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt")) 
          ),
          column(width = 12,
                 textAreaInput(inputId = "range_result", label = "Output Range", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt"))
                 actionButton(inputId = "select_output_range", label = "Select Range", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt")) 
                 actionButton(inputId = "lock_output_range", label = "Lock", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt"))

          ),
          column(width = 12,
                 actionButton(inputId = "apply_formula", label = "Apply", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt"))
          )
      ),

      div(
        id = "dropdown_menu_1",
        style = ifelse(isTRUE(values$menu[[1]]), "display: block;", "display: none;"),
        class = "well",
        uiOutput("var_1"),
        selectizeInput(
          "formulaSelect",
          label = "Formula List",
          choices = formula_list,
          selected = state_multiple(),
          multiple = TRUE,
          options = list(
            placeholder = "Select categorical variables",
            plugins = list("remove_button", "drag_drop")
          )
        ),
        actionButton("checkFormula", "Check Formula")
      ),

      div(
        id = "dropdown_formula_menu_2",
        style = ifelse(isTRUE(values$menu[[2]]), "display: block;", "display: none;"),
        class = "well",
        uiOutput("var_4"),
        tabPanel("Typeahead",
                 h4("Filter by Expression"),
                 div(class = "row-fluid",
                     div(class = "well container-fluid",
                         div(class = "container span3",
                             helpText("Type expression to filter dataset."),
                             textInput.typeahead(
                               id = "search",
                               placeholder = "Enter search term",
                               local = data.frame(name = formula_list,
                                                 info = ifelse(grepl("x|y|between", formula_list),
                                                               "You need to replace x & y with numeric value.", ""),
                                                 stringsAsFactors = FALSE),
                               valueKey = "name",
                               tokens = c(1:length(my_autocomplete_list)),
                               template = HTML("<p class='repo-name'>{{name}}</p><p class='repo-description'>{{info}}</p>")
                             )
                         ),
                         div(class = "container span6",
                             shinyalert("shinyalert3")
                         )
                     )
                 ),
        ),
        actionButton("paste", "Paste suggestion"),
        textAreaInput("expr", "Enter filter condition", placeholder = "Please enter the expression based on the above suggestion"),
        actionButton("filter_button", "Filter"),
        actionButton("clear_button", "Clear")
      )
    )
  )
})



observeEvent(input$select_input_range, {
  req(input$select_input_range)
  
  observeEvent(input$mytable_cells_selected, {
    req(input$mytable_cells_selected)
    selected_input_range <- input$mytable_cells_selected
    infoInput <- eventReactive(selected_input_range, {
      paste0(info, input$range_apply)
    })
  })
  
  observeEvent(input$lock_input_range, {
    req(input$lock_input_range)
    freezeReactiveVal(infoInput)
  })
})


observeEvent(input$select_output_range, {
  req(input$select_output_range)
  
  observeEvent(input$mytable_cells_selected, {
    req(input$mytable_cells_selected)
    selected_output_range <- input$mytable_cells_selected
    infoOutput <- eventReactive(selected_output_range, {
      paste0(info, input$range_result)
    })
  })
  
  observeEvent(input$lock_output_range, {
    req(input$lock_output_range)
    freezeReactiveVal(infoOutput)
  })
})


listFuncNotRequireArgs <- c("dabMinus", "dabAdd")
listFuncRequireArgs <- c("dabCEILING")

formula_args <- reactiveVal(param_name = NULL, param_value = NULL)

observeEvent(input$buitinFormal, {
  req(input$buitinFormal)
  if (isTRUE(input$buitinFormal > 0)) {
    for (func in input$buitinFormal) {
      if (func %in% listFuncRequireArgs) {
        functionArgs <- get_func_params(dabtab.basic, func)
        output$ui_args <- renderUI({
          tagList(
            lapply(functionArgs$params, function(param) {
              textInput(
                inputId = param$name,
                label = paste(param$name, ":", param$paramdetails)
              )
            })
          )
        })
      }
    }
  }
})

observeEvent(input$lock_args, {
  req(input$lock_args)
  if (!is.null(formula_args())) {
    # You need to replace the function call `func(...)(formula_args)` with the actual function you're using and pass arguments accordingly
    result <- func(formula_args())
    # Use the 'result' value as needed
  } else {
    # Handle if arguments are not filled
  }
})


 # Function to get details of parameters for each function
get_func_params <- function(package, funcName) {
  func_obj <- get(funcName, envir = asNamespace(package))
  if (is.function(func_obj)) {
    param_info <- names(formals(func_obj))
    cat("Function:", funcName, "\n")
    cat("Parameters:", param_info, "\n\n")
  }
}

# UI elements (This is a partial code snippet; you'll need to integrate it within your app)
navlistPanel(
  "calOpt",
  tabPanel("Row", h3("This is the first panel")),
  tabPanel("Column", h3("This is the second panel")),
  tabPanel("CROSS", h3("This is the third panel"))
)

# Observe the applyFormula event
observeEvent(input$applyFormula, {
  req(input$inputRange, input$buitinFormal, input$custom_formula, input$outputRange)

  # Your data table render (partial snippet provided)
  output$mytable <- renderDT({
    # Render your data table here using the 'data' variable
    # e.g., datatable(data, editable = TRUE, selection = 'multiple')
  })

  # Your matrix operations (partial and unclear snippet provided)
  output_matrix <- reactiveVal(NULL)
  pasteVal <- function(data, rowStart, colStart, rowDest, colDest) {
    data[rowDest, colDest] <- data[rowStart, colStart]
  }

  handleOrderApply <- function(func) {
    # Perform operations based on order apply
  }

  handleMultateApply <- function(func) {
    # Perform operations based on mutation apply
  }

  if (isTRUE(input$buitinFormal$select) && length(input$buitinFormal) > 0) {
    for (func in input$buitinFormal) {
      observeEvent(input$order_cal, {
        handleOrderApply(func)
      })

      observeEvent(input$mutation_cal, {
        handleMultateApply(func)
      })
    }
  } else {
    funcInput <- input$custom_formula  # Assuming this is the input from the custom formula

    observeEvent(input$order_cal, {
      handleOrderApply(funcInput)
    })

    observeEvent(input$mutation_cal, {
      handleMultateApply(funcInput)
    })
  }

  # Update the DataTable
  replaceData(proxy = dataTableProxy("mytable"), data, resetPaging = FALSE)
})
   


obserEvent(input$applyFormula) {
  req(input$applyFormula)

  output$mytable <- renderDT({
    datatable(data, editable = TRUE, selection = 'multiple')
  })


output_matrix <- reactiveVal(NULL)

pasteVal <- function (data, rowStart, colStart, rowDest, colDest) {data[rowDest, colDest]<-data[rowStart, colStart]}


handleOrderApply <- funtion (func)   

 if length(selected_input_rows$col) > 0

          observeEvent(input$row) {
          output_matrix <- mapply(add, selected_input_range[,1], input_matrix[, n(length(selected_input_cols$col))])

          output_matrix_transform <- mapply(pasteVal(output_matrix), output_matrix[,1], input_matrix[, n(length(selected_input_cols$col))])

          } 

          observeEvent(input$column) {
            output_matrix <- mapply(add, selected_input_range[,1], input_matrix[, n(length(selected_input_cols$col))])

            output_matrix_transform <- mapply(pasteVal(output_matrix), output_matrix[,1], input_matrix[, n(length(selected_input_cols$col))])

      }


        observeEvent(input$cross) {
              output_matrix_row <- sum(mapply(add, selected_input_range[,1], input_matrix[, n(length(selected_input_cols$col))]))

              output_matrix_col <- sum(mapply(add, selected_input_range[,1], input_matrix[, n(length(selected_input_cols$col))]))

              output_matrix <- mapply(add, output_matrix_row, output_matrix_col])


    

        } else if length(selected_input_rows$col) = 0{

          zero_vector = matrix(0, length(selected_input_rows$rows))

          output_matrix <- mapply(add, selected_input_range[,1], zero_vector)


        }

        # Update the DataTable
        replaceData(proxy = dataTableProxy("mytable"), data, resetPaging = FALSE)
      }


  if(!isna(input$inputRange) && !isna(input$buitinFormal || input$custom_formula) && !isna(input$outputRange) 

  if (isTRUE(buitinFormal$select) && length(input$buitinFormal) > 0) {
    for (func in length(input$buitinFormal)){
      observeEvent(input$order_cal) {
        handleOrderApply(func)
        # Perform calculation

          }

         observeEvent(input$mutation_cal) {
        handleMultateApply(func)
        # Perform calculation
          }




  }
} else if (isFALSE(buitinFormal$select)))

funcInput <- formula_expression(input$)custom_formula  


 observeEvent(input$order_cal) {
        handleOrderApply(funcInput)
        # Perform calculation

          }

         observeEvent(input$mutation_cal) {
        handleMultateApply(funcInput)
        # Perform calculation
          }


  }
         # Update the DataTable
        replaceData(proxy = dataTableProxy("mytable"), data, resetPaging = FALSE)


}



apply(X, MARGIN, FUN)
Here:
-x: an array or matrix
-MARGIN:  take a value or range between 1 and 2 to define where to apply the function:
-MARGIN=1`: the manipulation is performed on rows
-MARGIN=2`: the manipulation is performed on columns
-MARGIN=c(1,2)` the manipulation is performed on rows and columns
-FUN: tells which function to apply. Built functions like mean, median, sum, min, max and even user-defined functions can be applied>



# Read and evaluate the code from the file
formula_expression <- function(formula, dataframe, inputRange, outputRange) {
  expr <-apply(formula)

output_reactive <- reactive({
  shiny::exprToFunction(expr, env, quoted)
  })

  return expr
}


observeEvent(input$builtin_formula, {
buitinFormal$select <- TRUE
# check if other dropdown menus are visible
for (i in 2:5) {
  if (values$menu[[i]]) {
    # skip toggling if other dropdown menu is visible
    return(NULL)
  }
}
# toggle display of dropdown menu 1
values$menu[[1]] <- !values$menu[[1]]
})

observeEvent(input$custom_formula, {
buitinFormal$select <- FALSE
# check if other dropdown menus are visible
for (i in c(1, 3:5)) {
  if (values$menu[[i]]) {
    # skip toggling if other dropdown menu is visible
    return(NULL)
  }
}
# toggle display of dropdown menu 2
values$menu[[2]] <- !values$menu[[2]]
})


#obsere event to keep track of selected cells
  observeEvent(input$mytable_cells_selected, {
    # Get the selected cells
    selected_cells <- input$mytable_cells_selected
    
    # Display the selected cells in the console
    if (!is.null(selected_cells)) {
      cat("Selected Cells: ")
      for (cell in seq_along(selected_cells$row)) {
        cat("(Row:", selected_cells$row[cell], ", Column:", selected_cells$column[cell], ") ")
      }
      cat("\n")
    }
  })
}




###############################
## PAYPAL PAYMENT
###################################


tags$head(
    tags$script(src = "https://www.paypal.com/sdk/js?client-id=YOUR_CLIENT_ID")
  ),
  
  numericInput("amount", "Amount", value = 10, min = 1),  # User input for amount
  textInput("card_number", "Card Number", placeholder = "Enter card number"),
  textInput("expiry_date", "Expiry Date", placeholder = "MM/YY"),
  textInput("cvc", "CVC", placeholder = "CVC"),
  
  tags$script(HTML("
    $('#process_payment').click(function() {
      var amount = $('#amount').val(); // Get the user-entered amount
      var cardNumber = $('#card_number').val();
      var expiryDate = $('#expiry_date').val();
      var cvc = $('#cvc').val();
      
      paypal.Buttons({
        createOrder: function(data, actions) {
          return actions.order.create({
            purchase_units: [{
              amount: {
                value: amount  // Use the user-entered amount
              }
            }]
          });
        },
        onApprove: function(data, actions) {
          return actions.order.capture().then(function(details) {
            // Payment was successful, you can handle the success here
            alert('Transaction completed by ' + details.payer.name.given_name);
          });
        }
      }).render('#paypal-button-container');
    });
  "))
)


# Server part of your Shiny app
server <- function(input, output, session) {
  observeEvent(input$process_payment, {
    # Fetch the payment details from the input fields
    amount <- as.numeric(input$amount) * 100  # Convert to cents (Stripe requires amounts in the smallest currency unit)
    card_number <- input$card_number
    expiry_date <- input$expiry_date
    cvc <- input$cvc
    
    # Use the Stripe API to process the payment
    stripeToken <- stripe::stripeCreateToken(
      card = list(
        number = card_number,
        exp_month = as.numeric(substr(expiry_date, 1, 2)),
        exp_year = as.numeric(substr(expiry_date, 4, 5)),
        cvc = cvc
      )
    )
    
    # Use the token to make a charge
    charge <- stripe::stripeChargeCreate(
      amount = amount,
      currency = "usd",  # Change this based on your currency
      source = stripeToken$id
    )
    
    # Handle the response
    if (charge$paid) {
      # Payment successful
      showModal(modalDialog("Payment successful!"))
    } else {
      # Payment failed
      showModal(modalDialog("Payment failed. Please try again."))
    }
  })
}


###############################
##LAMDA FUNCTION AMAZON TO HANDLE SHARING DATA ON CLOUD 
###################################


###############################
##STASTICAL MODELLING
###################################


#LINEAR REGRESSION

library(caret)

# Sample data
data(iris)
model <- train(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris, method = "lm")
print(model)


#CORRELATION

library(caret)

# Sample data
data(iris)
cor_matrix <- cor(iris[, -5]) # Compute correlation matrix excluding the Species column
print(cor_matrix)


#LOGISTIC REGRESSION

library(caret)

# Sample data
data(iris)
model <- train(Species ~ Sepal.Width + Petal.Length + Petal.Width, data = iris, method = "glm", family = "binomial")
print(model)


#MULTINOMINAL LOGISTIC REGRESSION

library(nnet)

# Sample data
data(iris)
model <- multinom(Species ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
print(model)


#NAIVE BAYES

library(e1071)

# Sample data
data(iris)
model <- naiveBayes(Species ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
print(model)


#NEURONETWORK

library(nnet)

# Sample data
data(iris)
model <- nnet(Species ~ Sepal.Width + Petal.Length + Petal.Width, data = iris, size = 5)
print(model)


#TREES


#CLASSISIFICATION AND REGRESSION TREES

library(caret)

# Sample data
data(iris)
model <- train(Species ~ Sepal.Width + Petal.Length + Petal.Width, data = iris, method = "rpart")
print(model)



#RANDOM FOREST

library(randomForest)

# Sample data
data(iris)
model <- randomForest(Species ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
print(model)



#GRADIENT BOOSTED TREES


library(xgboost)

# Sample data
data(iris)
model <- xgboost(data = data.matrix(iris[, -5]), label = iris$Species, nrounds = 10)
print(model)



#EVALUATE REGRESSION


#EVALUATE CLASSIFICATION



#COLLABORATIVE FILTERING


#DECISION MAKING


#SIMULATE


#PROBABILITY CALCULATION


#CENTRAL LIMIT THEOREM

#GOODNESS OF FIT


#CROSS-TABS

#CORRELATION


#DISSIMILARITY

#ATTRIBUTES


#PRE-FACTOR


#FACTOR


#HIERARCHICAL


#K-CLUSTERING


#CONJOINT


#MONTE-CARLO METHODS




###############################
##REPORT 
###################################

 tag$HTML(tag$head(
        tag$link("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.1/css/bootstrap.min.css")
        tag$script("https://raw.githack.com/SortableJS/Sortable/master/Sortable.js")
        ))


    tags$script(HTML(
      "Sortable.create(droppable, {",
      "  forceFallback: true",
      "});",
      "$(function() {",
      "  $('.droppable-main').droppable({",
      "    drop: function(event, ui) {",
      "      var draggedElement = ui.draggable-sidebar.text();",
      "      var uiElement = find_ID(draggedElement, list_UI);",
      "      removeSidebarElement(selectorInput, uiElement);",
      "      generateMainElement(uiElement);",
      "      var currentContent = $(this).html();",
      "      $(this).html(currentContent);",
      "    }",
      "  });",
      "  $('.droppable-sidebar').droppable({",
      "    drop: function(event, ui) {",
      "      var draggedElement = ui.draggable-main;",
      "      var uiElement = find_ID(draggable-main, list_UI);",
      "      removeMainElement(uiElement);",
      "      generateSidebarElement(uiElement);",
      "      var currentContent = $(this).html();",
      "      $(this).html(currentContent);",
      "    }",
      "  });",
      "});"
    ))
  )

  #Report Tab     
ui_report <- fluidPage(
  wellPanel(
    "Reports",
    icon = icon("file-text"),  # Change the icon here
    actionButton("placeElement", "Add Element"),
    actionButton("exportReport", "Export Reports")
  ),
  mainPanel(
    h3("Drop Zone for R Markdown"),
    htmlOutput("markdown_output")
  )
)


observeEvent(input$exportReport, {
  showModal(
    modalDialog(
      fluidPage(
        fluidRow(
          column(
            width = 12,
            fluidPage(
              tabsetPanel(
                tabPanel("HTML", value = "html_report", icon = icon("file-pdf")),
                tabPanel("PDF", value = "pdf_report", icon = icon("file-pdf")),
                tabPanel("Word", value = "word_report", icon = icon("file-word")),
                tabPanel("PPTX", value = "pptx_report", icon = icon("file-powerpoint")),
                tabPanel("shinyHTML", value = "shinyHtml_report", icon = icon("file-powerpoint")),
                tabPanel("shinyPPTX", value = "shinyPptx_report", icon = icon("file-powerpoint")),
                tabPanel("More Info", icon = icon("info-circle"))
              )
            )
          )
        )
      ),
      mainPanel(
        h3("Drop Zone for R Markdown"),
        htmlOutput("markdown_output")
      )
    )
  )
})

 exportRMarkdown <- function(input_file, output_format, output_file) {
  rmarkdown::render(
    input = input_file,
    output_format = output_format,
    output_file = output_file
  )
}

insert_output <- function(name, output, list_UI) {
  list_UI[[name]] <- output
  return(list_UI)
}

find_ID <- function(uiID, list_UI) {
  for (ui in list_UI) {
    if (uiID == ui[[name]]) {
      return(uiID)
    }
  }
}

generateMainElement <- function(uiID, list_UI) {
  div_list <- div(
    list_UI[[uiID]],
    id = uiID,
    class = "draggable-",
    style = "border: 1px solid black; padding: 10px; "
  )
  return(div_list)
}

removeMainElement <- function(uiID) {
  # Assuming element refers to a parent container
  element$removeChild(uiID)
}

removeSidebarElement <- function(selectorInput, uiID) {
  # Assuming you are using shiny's removeUI function
  removeUI(
    selector = paste("div:has(#", uiID, ")")
  )
}

generateSidebarElement <- function(selectorInput, uiID) {
  insertUI(
    selector = selectorInput,
    where = "afterEnd",
    ui = div(id = uiID, class = "draggable-sidebar", draggable = "true", "Element 2")
  )
}

generateSidebarElements <- function(selectorInput, list_UI) {
  lapply(names(list_UI), function(name) {
    insertUI(
      selector = selectorInput,
      where = "afterEnd",
      ui = div(id = name, class = "draggable-sidebar", draggable = "true", "Element 2")
    )
  })
}

observeEvent(input$placeElement, {
  req(input$placeElement)
  generateSidebarElements("#placeElement", list_UIs)
})


#Function to dynamically render RMarkdown reports
observeEvent(input$export, {
  showModal(
    modalDialog(
      splitLayout(
        infoBox(
          title = "Custom Template",
          textOutput("custom_template_info")
        ),
        infoBox(
          title = "Choose Template",
          textOutput("choose_template_info")
        )
      )
    )
  )
})

   


#Function to dynamically render RMarkdown reports
   observeEvent(input$export, {
    selected_report <- input$reports
    if (is.null(selected_report)) return()
    report_file <- file.path(instDir, "app/www/reports/dabtab_html.Rmd"),
    
    report_output <- renderUI({
      exportRMarkdown(report_fil, input$reports, output_file)
      
    })  
    output$report_content <- report_output

   observeEvent(input$download_report, {
    inFile <- input$file
    outputFormat <- input$format
    if (!is.null(inFile)) {
      output_file <- sub("\\.Rmd", paste0(".", outputFormat), inFile$name)
      exportRMarkdown(inFile$datapath, outputFormat, output_file)
      if (file.exists(output_file)) {
        file.rename(output_file, "report.zip")  # Optional: Rename the file
        output$download_report = downloadHandler(
          filename = function() {
            "report.zip"  # Downloaded file will be named as report.zip
          },
          content = function(file) {
            file.copy("report.zip", file)
          }
        )
      }
    }
  })

  })


###############################
## SWITCH SESSIONS/ SAVE PROJECT/ LOAD PROJECT
###################################


###############################
## MODIFY SHORTCUTS
###################################



  
######################
##Existing code
######################
choices_list<-c("sum", "geometric mean", "harmonic mean", "coorvariance", "standard deviation", "arthematic mean")

my_autocomplete_list <- c("lowest value", "highest value", "top x% value", "bottom x% value",
                          "values that contain", "duplicate value", "unique value", "above value x",
                          "below value x", "between x and y", "contain value x", "equal to",
                          "above average", "below average", "missing value", "find type")

callback <- c(
  '$("#remove1").on("click", function(){',
  '  table.rows(".selected").remove().draw();',
  '});')

# Define the file paths

# Get the path to the 'inst' directory of your package or project
dataDir <- system.file("data", package = "dabtab.basic")
instDir <- system.file("inst", package = "dabtab.basic")


df_1<-read.csv(file.path(dataDir, "202204-divvy-tripdata_v2.csv"))
d_pkg_dependencies_path<-file.path(dataDir, "pkg_dependencies.csv")
d_pkg_title_path<-file.path(dataDir, "tile_summary.rds")
d_pkg_details_path<-file.path(dataDir, "pkg_details.rds")

path_fix<-file.path(dataDir)
path_sufix<-file.path(dataDir)
domains_path <- file.path(dataDir, "treechart_test.csv")

filename <- list.files(path = path_fix)

directory <- rep(path_sufix, length(filename))
file_ext<-tools::file_ext(filename)

filedf <- data.table(directory, filename, file_ext)

read_eval_code <- memoise(function(filepath) {
  code <- read_files(filepath)
  code <- gsub('`|\\{r\\}', '', code)
  parsed_code <- parse(text = code)
  result <- eval(parsed_code)
  list(parsed_code = parsed_code, result = result)
})



      # Read and evaluate the code from the file
      code_data <- read_eval_code(filepath)
      parsed_code <- code_data$parsed_code
      result <- code_data$result
      # Update table and print output
      result_store$val<-result
      names(result) <- trimws(names(result))


read_eval_domains <- memoise(function(filepath) {
  result <- read.csv(filepath)
  result
})

# Read and evaluate the code from the file
domains <- read_eval_domains(domains_path)

# Define a function to read the data
read_data <- function(filepath) {
  read.csv(filepath, stringsAsFactors = FALSE)
}

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


# Define the server
if (interactive()) {
  server <- function(input, output, session) {
    d_pkg_dependencies <- reactive({
      readr::read_csv(d_pkg_dependencies_path)
    })

    d_pkg_releases <- reactive({
      readr::read_csv(d_pkg_releases_path)
    })

    d_pkg_details <- reactive({
      readRDS(d_pkg_details_path)
    })

    l_tile_summary <- reactive({
      readRDS(d_pkg_title_path)
    })


    # Placeholder data
    set.seed(123)
    fopi <- data.frame(
      year = rep(2010:2019, 4),
      country = rep(c("Afghanistan", "Albania", "Algeria", "American Samoa"), each = 10),
      data = runif(40, 100, 1000)
    )


    filepath <- reactiveValues(path = NULL)

    result_store<-reactiveValues(val = NULL)

    ext_pattern<-c("csv", "rds", "rda", "rdata", "txt")

    temp_file <- tempdir()
    for (file in path_sufix) {
      file.copy(file, temp_file, overwrite = TRUE)
    }
    temp_files <- list.files(temp_file)


    output$file_type<-renderUI({
      selectInput(
        inputId = "file_type",
        label = "Choose file format",
        choices = c("","csv", "rds", "rda", "rdata","clipboard", "url"),
        selected = ""
      )
    })

    file_type<-eventReactive(input$file_type,{
      input$file_type
    })

    observeEvent(input$intro, {
      introjs(session, options = list("nextLabel"="Next",
                                      "prevLabel"="Back",
                                      "skipLabel"="Exit"))
    })

    output$ui_prepare <- renderUI({
      wellPanel(
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle")),
        shinyjs::hidden(uiOutput("file_type")),
        uiOutput("dataSelect"),
        actionButton("upload_file", "Upload File"),
        actionButton("upload_folder", "Upload Folder"),
        prettyRadioButtons(
          inputId = "add_descript",
          label = "Add Description",
          choices = c("No Description", "Upload Available", "Add Manually"),
          selected = "No Description",
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        ),
        actionButton("refresh", "Refresh Dataset"),
        actionButton("return", "Return Dataset"),
        pickerInput(
          inputId = "filter_data",
          label = "Choose Data Format",
          choices = c("csv", "rds", "rda", "rdata", "txt"),
          multiple = TRUE,
          selected = "all",
          options = pickerOptions(
            actionsBox = TRUE,
            title = "Please select data formats to display"
          ),
          choicesOpt = list(
            content = sprintf(
              "<span class='label label-%s'>%s</span>",
              c("csv", "rds", "rda", "rdata", "txt"),
              paste(c("csv", "rds", "rda", "rdata", "txt"))
            )
          )
        ),

        actionBttn(inputId = "save_data",
                   label = "Save Dataset",
                   style = "jelly",
                   color = "primary")

      )
    })

    latest_file<-reactiveValues(val=NULL)

    observeEvent(input$upload_file, {
      shinyjs::show("file_type")
      shinyjs::hide("file_progress")

      if (file_type() != "" & file_type() != "clipboard" & file_type() != "url") {
        path_choose <- choose_files(isolate(file_type()), multi = TRUE)

        if (length(path_choose) > 0) {
          withProgress(
            message = 'Uploading file...',
            value = 0,
            for (i in 1:15) {
              incProgress(1/15)
              Sys.sleep(0.2)
            }
          )

          accepted_formats <- c("csv", "rds", "rda", "rdata")

          filename <- basename(path_choose)
          file_ext <- tools::file_ext(filename)

          if (!file_ext %in% accepted_formats) {
            # File format is not in the accepted list
            sendSweetAlert(
              session = session,
              title = "Warning !!!",
              text = "Please select the correct file format.",
              type = "warning"
            )
            return(NULL)  # Return nothing to prevent further execution
          } else {
            new_files <- data.table(directory = file.path(dirname(path_choose), '/'), filename = filename, file_ext = file_ext)
            filepath$path <- rbind(filepath$path, new_files)

            n_files <- nrow(unique(new_files))

            if (n_files == 1) {
              latest_file$val <- new_files$filename
              sendSweetAlert(
                session = session,
                title = "Success !!",
                text = paste0("Successfully uploaded '", unique(new_files$filename), "'."),
                type = "success"
              )
            } else {
              latest_file$val <- tail(new_files$filename, n = 1)
              sendSweetAlert(
                session = session,
                title = "Success !!",
                text = paste0("Successfully uploaded '", n_files, "' files."),
                type = "success"
              )
            }

            updateProgressBar(session, "file_progress", value = 0)
            Sys.sleep(3)
            shinyjs::hide("file_progress")
          }
        } else {
          # Assign an empty data frame to filepath$path1 if no file is selected
          new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf))
          )
          filepath$path <- rbind(filepath$path, new_files)
          sendSweetAlert(
            session = session,
            title = "Warning !!!",
            text = "You have not selected any file!",
            type = "warning"
          )
        }

        updateSelectInput(session, "file_type", selected = "")
        updateActionButton(session, "upload_file", label = "Upload File")
      }
    })

    observeEvent(input$upload_folder, {
      shinyjs::hide("file_type")
      path_choose <- choose.dir()
      if (is.null(path_choose)) {
        new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf)))
        filepath$path <- rbind(filepath$path, new_files)
        output$output <- renderText({
          paste0("No folder selected.")
        })
      } else {
        path_dir <- paste0(gsub("\\\\", "/", path_choose), '/')
        files <- list.files(path = path_choose)
        files <- unique(files)
        len_file <- length(files)
        file_exts <- tools::file_ext(files)
        new_files <- data.table(directory = path_dir, filename = files, file_ext = file_exts)
        if (len_file == 0) {
          new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf)))
          filepath$path <- rbind(filepath$path, new_files)
          output$output <- renderText({
            paste0("Selected folder '", path_choose, "' is empty.")
          })
        } else {
          new_files_1 <- new_files[file_ext %in% ext_pattern]
          if (nrow(new_files_1) > 0) {
            filepath$path <- rbind(filepath$path, new_files_1)
            latest_file$val<-tail(new_files$filename, n = 1)
            output$output <- renderText({
              paste0("Successfully uploaded folder '", path_dir, "'.")
            })
          } else {
            new_files <- data.table(directory = rep("", length(filedf)), filename = rep("", length(filedf)), file_ext = rep("", length(filedf)))
            filepath$path <- rbind(filepath$path, new_files)
            output$output <- renderText({
              paste0("Selected folder '", path_choose, "' does not contain any valid files.")
            })
          }
        }
      }
      updateActionButton(session, "upload_folder", label = "Upload Folder")
    })

    #This is for read clipboard
    observeEvent(input$upload_file, {
      req(input$upload_file)
      if (file_type() != "" & file_type() != "csv" & file_type() != "url" & file_type() != "rds" & file_type() != "rdata" & file_type() != "txt" & file_type() != "rda" ) {
        showModal(paste_clip)
      }
    })

    # Filter data based on selected countries
    output$trend <- echarts4r::renderEcharts4r({
      req(input$country_select)
      msg <- paste0(tools::toTitleCase(input$country_select))
      fopi %>%
        dplyr::mutate(year = as.character(year)) %>%
        dplyr::arrange(year) %>%
        dplyr::filter(country %in% input$country_select) %>%
        dplyr::group_by(country) %>%
        echarts4r::e_charts(year) %>%
        echarts4r::e_line(data) %>%
        echarts4r::e_tooltip(trigger = "axis") %>%
        echarts4r::e_y_axis(inverse = TRUE) %>%
        echarts4r::e_axis_labels("Years") %>%
        echarts4r::e_title(msg) %>%
        echarts4r::e_color(
          c("#247BA0", "#FF1654", "#70C1B3", "#2f2f2f", "#F3FFBD", "#B2DBBF")
        )
    })

    observeEvent(input$summit_3, {
      req(input$clipboard,input$summit_3)
      data <- load_clip(text = input$clipboard)
      output$table <- render_custom_datatable({
        data
      })
      removeModal()
    })

    paste_clip<-modalDialog(
      textAreaInput(
        inputId="clipboard",
        label="Paste Clipboard",
        width = "auto",
        height = "1000px",
        placeholder="Please paste here"),
      footer = tagList(
        actionButton("summit_3", "Load", class = "btn-primary"),
        modalButton("Cancel")
      )
    )

    observeEvent(input$viz_data, {
      req(input$viz_data)
      if(input$viz_data=="Show Multiples"){
        shinyjs::toggle("dropdown_menu_7")
      }
    })

    values <- reactiveValues(filedf = NULL)

    file_df <- reactive({
      rbind(filedf, filepath$path)
    })

    observeEvent(input$refresh, {
      req(input$refresh)
      file_df <- reactive({
        filedf
      })
      updateSelectInput(session, "dataselect",
                        choices = file_df()[complete.cases(file_df()), "filename"] %>% unique())

    })

    observeEvent(input$return, {
      req(input$return)
      file_df <- reactive({
        rbind(filedf, filepath$path)})

      updateSelectInput(session, "dataselect",
                        choices = file_df()[complete.cases(file_df()), "filename"] %>% unique())
    })

    output$dataSelect <- renderUI({
      selectInput(
        inputId = "dataselect",
        label = "Select a dataset",
        choices = file_df()[complete.cases(file_df()), "filename"] %>% unique())
    })

    observeEvent(file_df(),{
      updateSelectInput(inputId = "dataselect",
                        label = "Select a dataset",
                        choices = file_df()[complete.cases(file_df()), "filename"] %>% unique(),
                        selected = latest_file$val)
    })

    observeEvent(input$filter_data, {
      filtered_files <- file_df()[file_df()$file_ext %in% input$filter_data, ]
      updateSelectInput(session, "dataselect", choices = filtered_files$filename)

    })

    output$slider<-renderUI({
      sliderInput("rows", "Rows to display", min=1, max=nrow(result_store$val), value=c(1, nrow(result_store$val)))
    })

    observeEvent(input$viz_run_1, {
      req(input$viz_run_1)
      means <- c(calculate_df$val$arith_mean, calculate_df$val$harm_mean, calculate_df$val$geo_mean)
      output$mean_plot<-renderPlotly({means_plot(means, calculate_df$val$arith_mean, calculate_df$val$harm_mean, calculate_df$val$geo_mean)})
    })

    calculate_df<-reactiveValues(val=NULL)
    observeEvent(input$cal_select, {
      req(input$cal_select)
      df_copy<-result_store$val
      cal_df<-caculate(df_copy, input$cal_select, input$var_select_8)
      calculate_df$val<-cal_df
      cal_tabel<-renderDataTable({render_custom_datatable({cal_df})
      })
    })

    observeEvent(input$show_sample_viz, {
      req(input$show_sample_viz)
      if (input$show_sample_viz=="TRUE"){
        shinyjs::show("histogram")

      }else{
        shinyjs::hide("histogram")

      }
    })

    n_seeds<-reactive({
      n_rows <- nrow(df_1)

      n_seeds <- floor(n_rows /input$sample_size[[2]])

      return(n_seeds)
    })

    names(df_1) <- trimws(names(df_1))
    choices<-names(df_1)
    choices_1<-c("whole dataset",choices)
    output$sampling_1<-renderUI({
      menuItem(
        "Data Sampling",
        tabName = "sampling",
        icon = icon("spinner"),
        switchInput(
          inputId = "show_sample_viz",
          label = "Display Plot",
          value = FALSE,
          onLabel = "SHOW",
          offLabel = "HIDE",
          inline = TRUE,
          size = "mini",
          width = "100%",
          offStatus = "danger"
        ),
        sliderTextInput(
          inputId = "sample_size",
          label = "Sample Size:",
          choices = c(1:50),
          selected = c(1,10),
          from_fixed = TRUE,
          dragRange = TRUE
        ),
        numericInput("obs", "Seed:", value = 1, min = 1, max = n_seeds()),
        selectInput(
          inputId = "var_select_7",
          label = "Choose variable",
          choices = choices,
          selected = ""),

        actionButton("summary", "Summary"),

        actionButton("viz_run", "Create plot", width = "80%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )

    })

    output$sampling_2<-renderUI({
      menuItem(
        "Calculation",
        tabName = "calculation",
        selectInput(
          inputId = "var_select_8",
          label = "Choose variable to calculate",
          choices = choices_1,
          selected = "Whole dataset"
        ),
        pickerInput(
          inputId = "cal_select",
          label = "Choose Functions to Apply",
          choices = choices_list,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            title = "Please select data formats to display",
            choicesOpt = list(
              sprintf(
                "<span class='label label-%s'>%s</span>",
                choices_list,
                paste(choices_list, collapse = "")
              )
            )
          )
        ),
        tags$i("This plot shows the different between different means."),
        actionButton("viz_run_1", "Create plot", width = "100%", class = "btn-success")

      )

    })

    output$progressBox <- renderInfoBox({
      infoBox(
        "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
        color = "purple"
      )
    })
    output$approvalBox <- renderInfoBox({
      infoBox(
        "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow"
      )
    })

    # Same as above, but with fill=TRUE
    output$progressBox2 <- renderInfoBox({
      infoBox(
        "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
        color = "purple", fill = TRUE
      )
    })
    output$approvalBox2 <- renderInfoBox({
      infoBox(
        "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow", fill = TRUE
      )
    })

    val<-reactive({
      input$sample_size[[2]]

    })
    # Define a reactive expression to generate a list of seeds
    seed_list <- reactive({
      set.seed(input$obs)
      n_rows <- nrow(df_1)
      n_seeds <- floor(n_rows / val())
      seeds <- sample.int(n = 10000, size = n_seeds, replace = TRUE)
      return(seeds)
    })

    # Define a reactive expression to get the sampled data
    sampled_df <- reactive({
      n_rows <- nrow(df_1)

      set.seed(seed_list()[input$obs])
      df[sample(n_rows, val(), replace = TRUE), ]
    })

    # Render the sampled data table
    output$sampled_df <- renderDataTable({
      sampled_df()
    })

    # Update the sampled data table when the numeric input is changed
    observeEvent(input$obs, {

      n_rows <- nrow(df_1)
      output$sampled_df <- renderDataTable({
        set.seed(seed_list()[input$obs])
        df[sample(n_rows, val(), replace = TRUE), ]
      })
    })

    observeEvent(input$summary, {
      req(input$summary)
      output$summary<-renderText({summary(sampled_df())})

    })

    observeEvent(input$viz_run, {
      req(input$var_select_7,input$viz_run)
      output$histogram<-renderPlotly({histogram_plot(sampled_df(),input$var_select_7)})

    })

    observeEvent(input$dataselect,{
      req(input$dataselect)
      # Find the rows in df that match the selected file names
      matches <- grepl(paste(input$dataselect, collapse = "|"), file_df()$filename)

      # Subset df to include only the matching rows
      matched_files <- file_df()[matches, ]

      # Print the full file paths for the matched files
      filepath <- file.path(matched_files$directory, matched_files$filename)

      # Read and evaluate the code from the file
      code_data <- read_eval_code(filepath)
      parsed_code <- code_data$parsed_code
      result <- code_data$result
      # Update table and print output
      result_store$val<-result
      names(result) <- trimws(names(result))

      columns <- reactive({
        names(result)
      })

      # define initial choices for var_select_1
      choices_1 <- c("", columns())
      choices_3 <- c("Whole dataset", columns())

      output$multi_select<-renderUI({
        pickerInput(
          inputId = "mul_select",
          label = "Choose Columns to Display",
          choices = c(choices_1),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            title = "Please select data formats to display",
            choicesOpt = list(
              sprintf(
                "<span class='label label-%s'>%s</span>",
                choices_1,
                paste(choices_1, collapse = "")
              )
            )
          )
        )
      })

      output$var_1 <- renderUI({
        selectInput(
          inputId = "var_select_1",
          label = "Choose variable",
          choices = choices_1,
          selected = ""
        )
      })

      output$var_3 <- renderUI({
        selectInput(
          inputId = "var_select_3",
          label = "Choose variable",
          choices = choices_1,
          selected = ""
        )
      })

      output$var_5 <- renderUI({
        selectInput(
          inputId = "var_select_5",
          label = "Choose variable",
          choices = choices_1,
          selected = ""
        )
      })

      output$var_4 <- renderUI({
        selectInput(
          inputId = "var_select_4",
          label = "Choose variable",
          choices = choices_3,
          selected = "Whole dataset"
        )
      })

      output$var_6 <- renderUI({
        selectInput(
          inputId = "var_select_6",
          label = "Choose Pre Column",
          choices = choices_3,
          selected = ""
        )
      })

      result_filtered<-render_custom_datatable(result)

      result_filtered_1<-render_custom_datatable_1(result)

      output$table <- renderDataTable({result_filtered})
      output$table_2 <- renderDataTable({result_filtered_1})

      output$tb <- renderPrint({ parsed_code })

    })

    observeEvent(input$load, {
      data <- load_clip(delim = "\t",text = input$clipboard)
      output$table <- renderDataTable({
        data

      })
      removeModal()
    })

    description <- reactiveValues(value1 = "", value2 = NULL)

    observeEvent(input$add_descript, {
      req(input$add_descript)

      if (input$add_descript == "No Description") {
        description$value1<-NULL
        description$value2<-""
      }

    })

    observeEvent(input$add_descript, {
      req(input$add_descript)

      if (input$add_descript == "Add Manually") {

        showModal(modalDialog(
          title = "Add Description",
          textAreaInput("add_descr","ADD DESCRIPTION"),
          actionButton("submit_2", "Submit")
        ))

      } else if (input$add_descript == "Upload Available") {
        path_choose <- choose_files(multi = FALSE, "txt")
        filename <- basename(path_choose)
        path <- gsub("\\\\", "/", path_choose)
        description$value2<-path
        description$value1<-""
        if (length(path_choose) > 0) {
          withProgress(
            message = 'Uploading file...',
            value = 0,
            {
              for (i in 1:15) {
                incProgress(1/15)
                Sys.sleep(0.2)
              }
            }
          )

          sendSweetAlert(
            session = session,
            title = "Success !!",
            text = paste0("Successfully uploaded '",filename, "'."),
            type = "success"
          )

          updateProgressBar(session, "file_progress", value = 0)
          Sys.sleep(3)
          shinyjs::hide("file_progress")

        } else {
          sendSweetAlert(
            session = session,
            title = "Warning !!!",
            text = "You have not selected any file!",
            type = "warning"
          )
        }
      } else {
        return()
      }
    })

    observeEvent(input$submit_2, {
      req(input$submit_2,input$add_descr)
      description$value1<-input$add_descr
      description$value2<-NULL
      removeModal()
    })

    observeEvent(input$viz_description, {
      if (input$viz_description == "Show Top") {
        shinyjs::show(id = "myrow1")
      } else if (input$viz_description == "Show Bottom") {
        shinyjs::show(id = "myrow2")
      } else {
        shinyjs::hide(id = "myrow1")
        shinyjs::hide(id = "myrow2")
      }
    })

    descr_value <- reactive({
      if (length(description$value1) > 0 & is.null(description$value2)) {
        description$value1
      } else{
        readLines(description$value2, warn = FALSE)
      }
    })

    output$descr_1 <- renderText({
      descr_value()
    })

    output$descr_2 <- renderText({
      descr_value()
    })


    dataPull <- function(filename) {
      # check if the file exists in the download directory
      if (!file.exists(file.path(tempdir(), filename))) {
        showModal(modalDialog(
          title = "File not found",
          paste("The file", filename, "does not exist in the temporary directory."),
          easyClose = TRUE
        ))
        return(NULL)
      }

      # get the file extension
      ext <- tools::file_ext(filename)
      # read the data from the temp file
      if (ext == "csv") {
        data <- read.csv(file.path(tempdir(), filename))
        return(data)
      } else if (ext == "rds" || ext == "rda" || ext == "rdata") {
        data <- readRDS(file.path(tempdir(), filename))
        return(data)
      } else if (ext == "txt" || ext == "text") {
        data <- read.table(file.path(tempdir(), filename))
        return(data)
      } else {
        showModal(modalDialog(
          title = "Unknown file format",
          "The requested file format is not supported.",
          easyClose = TRUE
        ))
        return(NULL)
      }
    }

    downloadFile <- function(filename, format, session, custom_name) {
      #print(paste("filename:", filename))
      #print(paste("format:", format))
      #print(paste("custom_name:", custom_name))
      # check if the file exists in the download directory
      download_path <- file.path(getwd(), paste0(custom_name, ".", format))
      if (!file.exists(download_path)) {
        showModal(modalDialog(
          title = "File not found",
          paste("The file", custom_name, "does not exist in the download directory."),
          easyClose = TRUE
        ))
        return(NULL)
      }

      # set the content type based on the format
      if (format == "csv") {
        contentType <- "text/csv"
      } else if (format == "rds" || format == "rda" || format == "rdata") {
        contentType <- "application/octet-stream"
      } else if (format == "text") {
        contentType <- "text/plain"
      } else {
        showModal(modalDialog(
          title = "Unknown file format",
          "The requested file format is not supported.",
          easyClose = TRUE
        ))
        return(NULL)
      }

      # read the data from the temp file
      if (format == "csv") {
        data <- read.csv(file.path(tempdir(), filename))
      } else if (format == "rds" || format == "rda" || format == "rdata") {
        data <- readRDS(file.path(tempdir(), filename))
      } else if (format == "text") {
        data <- read.table(file.path(tempdir(), filename))
      }

      # define the content to be downloaded based on the format
      content <- switch(format,
                        csv = {
                          write.csv(data, row.names = FALSE)
                        },
                        rds = {
                          saveRDS(data, file = NULL)
                        },
                        text = {
                          write.table(data, row.names = FALSE)
                        })

      # define the downloadHandler with the specified filename, content, and content type
      downloadHandler(
        filename = download_filename,
        content = content,
        contentType = contentType,
        outputArgs = list(session),
        dialog = TRUE
      )
    }

    dataModal <- function(failed = FALSE) {
      modal <- modalDialog(
        pickerInput(
          inputId = "format",
          label = "Choose Data Format",
          choices = c("csv", "rds", "rda", "rdata", "txt"),
          multiple = TRUE,
          selected = "all",
          options = pickerOptions(
            actionsBox = TRUE,
            title = "Please select data formats to download",
          ),
          choicesOpt = list(
            content = sprintf("<span class='label label-%s'>%s</span>",
                              c("csv", "rds", "rda", "rdata", "txt"),
                              paste(c("csv", "rds", "rda", "rdata", "txt"))
            )
          )
        ),
        checkboxGroupButtons(
          inputId = "keep_name",
          label = "Label",
          choices = c("Keep original name"="A", "Rename"="B"),
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o",
                        style = "color: steelblue"))
        ),
        conditionalPanel(
          condition = "input.keep_name == 'B'",
          textInput("fill_filename", "Please enter file name", placeholder = "Enter file name here"),
          actionButton("summit_7", "Summit", class = "btn-primary")
        ),

        #change this to yes/no
        checkboxGroupButtons(
          inputId = "metadata_checkbox",
          label = "Attach Metadata",
          choices = c("Yes", "No"),
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o",
                        style = "color: steelblue"))
        ),
        conditionalPanel(
          condition = "input.metadata_checkbox == 'Yes'",
          prettyRadioButtons(
            inputId = "metadata",
            label = "Choose:",
            choices = c("default", "custom"),
            icon = icon("check"),
            bigger = TRUE,
            status = "info",
            animation = "jelly"
          )
        ),
        span('(Try the name of a valid data file like " or ", then a name of a non-existent object like "abc")'),
        footer = tagList(
          actionButton("saveData", "Save", class = "btn-primary"),
          modalButton("Cancel")
        )
      )

      if (failed) {
        showModal(modal)
      } else {
        modal
      }
    }

    # Event handler for save button
    observeEvent(input$save_data, {
      req(input$save_data)
      dataModal(failed=TRUE)
    })


    add_metadata <- modalDialog(

      title = "Add Metadata",

      #Description
      textAreaInput("caption", "Caption", "Data Summary", width = "1000px"),

      # File type
      textInput("file_type", "File type:", placeholder = "Enter file type"),

      # File format
      textInput("file_format", "File format:", placeholder = "Enter file format"),


      # Ownership
      textInput("ownership", "Ownership:", placeholder = "Enter ownership"),

      # Access rights
      textInput("access_rights", "Access rights:", placeholder = "Enter access rights"),

      # Usage restrictions
      textInput("usage_restrictions", "Usage restrictions:", placeholder = "Enter usage restrictions"),

      # File location (optional)
      textInput("file_location", "File location (optional):", placeholder = "Enter file location"),

      # Relevant file
      textInput("relevant_file", "Relevant file:", placeholder = "Enter relevant file"),

      # Technical details
      textInput("technical_details", "Technical details:", placeholder = "Enter technical details"),

      footer = tagList(
        actionButton("summit_5", "Summit"),
        actionButton("cancel_3", "Cancel")
      )

    )

    observeEvent(input$cancel_3, {
      req(input$cancel_3)
      dataModal(failed=TRUE)

    })

    observeEvent(input$metadata, {
      if (input$metadata=="custom"){
        showModal(add_metadata)
      }
    })

    observeEvent(input$submit_5, {
      removeModal()
    })

    values <- reactiveValues(
      file_type = "",
      file_format = "",
      description = "",
      ownership = "",
      access_rights = "",
      usage_restrictions = "",
      file_location = "",
      relevant_file = "",
      technical_details = ""
    )

    observeEvent(input$metadata, {
      if (input$metadata == "default") {
        # Write each input to a new line in the file
        values$file_type <- "File type: hallo"
        values$file_format <- "File format: hallo"
        values$description <- "Description: hallo"
        values$ownership <- "Ownership: hallo"
        values$access_rights <- "Access rights: hallo"
        values$usage_restrictions <- "Usage restrictions: hallo"
        values$file_location <- "File location: hallo"
        values$relevant_file <- "Relevant file: hallo"
        values$technical_details <- "Technical details: hallo"
      } else {
        showModal(add_metadata)
      }
    })

    observeEvent(input$summit,{
      # Write each input to a new line in the file
      paste0("File type: ", values$file_type)
      paste0("File format: ", values$file_format)
      paste0("Description: ", values$description)
      paste0("Ownership: ", values$ownership)
      paste0("Access rights: ", values$access_rights)
      paste0("Usage restrictions: ", values$usage_restrictions)
      paste0("File location: ", values$file_location)
      paste0("Relevant file: ", values$relevant_file)
      paste0("Technical details: ", values$technical_details)
    })

    observeEvent(input$Cancel, {
      dataModal()
    })
    iv <- InputValidator$new()
    iv$add_rule("file_type", sv_required())
    iv$add_rule("caption", sv_required())
    iv$add_rule("file_format", sv_required())
    iv$add_rule("fill_filename", sv_required())
    iv$add_rule("proj_name", sv_required())
    iv$add_rule("col_add", sv_required())
    iv$enable()
    observeEvent(c(input$file_type_1, input$file_format_1, input$ownership,
                   input$access_rights, input$usage_restrictions, input$file_location,
                   input$relevant_file, input$technical_details), {
                   input_list <- list(input$file_type_1, input$file_format_1, input$ownership,
                                        input$access_rights, input$usage_restrictions, input$file_location,
                                        input$relevant_file, input$technical_details)
                     for (i in seq_along(input_list)) {
                       input_list[[i]] <- strtrim(input_list[[i]], 50)
                     }
                     values$file_type <- input_list[[1]]
                     values$file_format <- input_list[[2]]
                     values$ownership <- input_list[[3]]
                     values$access_rights <- input_list[[4]]
                     values$usage_restrictions <- input_list[[5]]
                     values$file_location <- input_list[[6]]
                     values$relevant_file <- input_list[[7]]
                     values$technical_details <- input_list[[8]]
                   })

    filename_1 <- reactiveValues(name = "")

    observeEvent(c(input$keep_name, input$fill_filename, input$summit_7, input$dataselect), {
      req(input$keep_name)
      # Check if file name is valid
      if (input$keep_name == "A") {
        filename_1$name <- input$dataselect
        print(filename_1$name)
      } else {
        req(input$summit_7)
        validate(
          need(is.null(input$fill_filename) & input$fill_filename == "" & substr(input$fill_filename, nchar(filename), nchar(filename)) == "_",
               "Please enter a valid file name.")
        )
        filename_1$name <- input$fill_filename
        print(filename_1$name)
      }
    })

    output$saveData <- downloadHandler(
      filename = function() {
        file_name_1 <- paste(filename_1()$name, input$format_1, sep = ".")
        return(file_name_1)
      },
      content = function(file) {
        data <- dataPull(input$dataselect)
        if (input$format_1 == "csv") {
          write.csv(data, file, row.names = FALSE)
        } else if (input$format_1 == "rds" || input$format_1 == "rda" || input$format_1 == "rdata") {
          saveRDS(data, file)
        } else if (input$format_1 == "txt") {
          write.table(data, file, row.names = FALSE, sep = "\t")
        } else {
          showModal(modalDialog(
            title = "Unknown file format",
            "The requested file format is not supported.",
            easyClose = TRUE
          ))
        }
      },
      contentType = function(file) {
        ext <- tools::file_ext(file)
        if (ext == "csv") {
          "text/csv"
        } else if (ext == "rds" || ext == "rda" || ext == "rdata") {
          "application/octet-stream"
        } else if (ext == "txt") {
          "text/plain"
        } else {
          showModal(modalDialog(
            title = "Unknown file format",
            "The requested file format is not supported.",
            easyClose = TRUE
          ))
        }
      }
    )

    # Event handler for download button
    observeEvent(c(input$saveData, input$metadata_checkbox), {

      withProgress({
        setProgress(message = "Downloading file...", detail = "")
        tryCatch({
          data <- paste0(file_df()$directory[filename$name == input$dataselect], input$dataselect)
          if (input$metadata_checkbox == "Yes"){
            downloadFile_1(data, filename$name, input$format)
          }else{
            metadata=values()
            downloadFile1(data, metadata,filename$name, input$format)
          }
          output$download_msg <- renderText({
            paste0("File saved successfully to ", getwd(), ".")
          })
        }, error = function(e) {
          print(paste("Oops! An error occurred during downloading:", e$message))
          return(NA)
        })
      }
      )
    })

    output$my_text <- renderText({
      "Hello, world!"
    })

    output$my_text_1 <- renderText({
      "Hello, world!"
    })

    output$my_text_5 <- renderText({
      "Hello, world!"
    })


    output$my_text_2 <- renderText({
      "Hello, world!"
    })

    output$my_text_3 <- renderText({
      "Hello, world!"
    })
    output$my_text_4 <- renderText({
      "Hello, world!"
    })
    credentials = data.frame(
      username_id = c("myuser", "myuser1"),
      passod   = sapply(c("mypass", "mypass1"),password_store),
      permission  = c("basic", "advanced"),
      stringsAsFactors = F
    )


  constructor() {
        this.db = mysql2.createPool({
              host     : process.env.RDS_HOSTNAME,
              user     : process.env.RDS_USERNAME,
              password : process.env.RDS_PASSWORD,
              port     : process.env.RDS_PORT,
              database : process.env.RDS_DATABASE

        });

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

    data <- reactive({
      req(input$datasets)
      filepath = paste0("D:/Data science/01_DS_Project/Data - Copy/file_csv/", input$datasets)
      read.csv(filepath)
    })

    map_data <- reactive({
      req(input$map_datasets)
      filepath = paste0("D:/Data science/01_DS_Project/Data - Copy/file_csv/", input$map_datasets)
      read.csv(filepath)
    })


    my_custom_theme<-myThemeServer(id="theme")

    observeEvent(input$plots,{
      output$plot1 <- renderPlotly({
        if (is.null(data())) return(NULL)
        if (input$plots == "plot 1") {
          data()[-1,] %>%
            filter(!is.na(member_casual) & !is.na(as.numeric(ride_duration_1))) %>%
            group_by(member_casual) %>%
            summarize(total_ride_duration = sum(as.numeric(ride_duration_1))) %>%
            ggplot(aes(x = member_casual, y = total_ride_duration, fill = member_casual)) +
            geom_bar(stat = "identity") +
            labs(x="Membership Type",y="Hours",title = "Ride Duration by Membership", subtitle = "My subtitle", caption = "Source: https://divvy-tripdata.s3.amazonaws.com/index.html")+
            my_custom_theme$result+
            coord_flip()
        }
      })

      output$plot2 <- renderPlotly({
        if (is.null(data())) return(NULL)
        if (input$plots == "plot 2") {
          data()[-1,] %>%
            filter(!is.na(weekday) & !is.na(ride_duration_1)) %>%
            group_by(member_casual, weekday) %>%
            summarize(total_ride_duration = sum(as.numeric(ride_duration_1))) %>%
            ggplot(aes(x = weekday, y = total_ride_duration, group = member_casual, color = member_casual)) +
            geom_line() +
            geom_point(size = 2) +
            ggtitle("Ride duration of member and casual riders by weekday") +
            xlab("Weekday") +
            ylab("Ride Duration (by hours)") +
            scale_x_discrete(labels = c("Mon.", "Tue.", "Wed.", "Thurs.", "Fri.", "Sat.", "Sun.")) +
            my_custom_theme$result+
            facet_wrap(~member_casual)
        }})


      output$my_map <- renderLeaflet({
        if (is.null(map_data())) return(NULL)
        if (input$plots == "plot 3") {
          # Find the top ten stations with the highest count values
          top_stations <- map_data() %>%
            top_n(10, count)

          # Create the leaflet map with the base tiles
          leaflet() %>%
            addTiles() %>%
            # Add circle markers for the top ten stations
            addCircleMarkers(data = top_stations,
                             ~pt_lng, ~pt_lat,
                             color = "red",
                             radius = 10,
                             stroke = FALSE,
                             fillOpacity = 1) %>%
            # Add popup text for all markers
            addMarkers(data = map_data(),
                       ~pt_lng, ~pt_lat,
                       popup = paste("Station ID: ", map_data()$station_id, "<br>",
                                     "Frequency: ", map_data()$count))


        }
      })
    })


    # Create 3D scatter plot
    output$myplot <- renderPlotly({
      plot_ly(data = mtcars, x = ~wt, y = ~disp, z = ~mpg,
              type = "scatter3d", mode = "markers", marker = list(color = "blue", size = 1.5)) %>%
        layout(scene = list(xaxis = list(title = "Weight"),
                            yaxis = list(title = "Displacement"),
                            zaxis = list(title = "Miles per gallon")),
               title = "3D Scatter Plot of Cars Data")
    })

    output$categorySelectComboTree <- renderUI({
      selectInput("selectedCategoryTree","Select a category:", sort(as.character(unique(domains$domain))))
    })

    domainsTree <- reactive({
      filtered_data <- domains[domains$domain == input$selectedCategoryTree, c("domain", "main_branch", "sub_branch")]
      # Ensure the columns are factors or character vectors
      filtered_data$domain <- as.character(filtered_data$domain)
      filtered_data$main_branch <- as.character(filtered_data$main_branch)
      filtered_data$sub_branch <- as.character(filtered_data$sub_branch)
      filtered_data
    })

    output$tree <- renderCollapsibleTree({
      collapsibleTree(
        domainsTree(),
        root = input$selectedCategoryTree,
        attribute = "sub_branch",
        hierarchy = c("domain", "main_branch", "sub_branch"),
        fill = "green",
        zoomable = FALSE
      )
    })

    # Render the username and permission tabs
    output$username_tab <- renderUI({
      if (USER$login==TRUE) {
        credentials[,"username"][which(credentials$username_id==input$userName)]
        # render username info
      }
    })

    output$permission_tab <- renderUI({
      if (USER$login==TRUE) {
        credentials[,"permission"][which(credentials$username_id==input$userName)]

      }
    })

    observeEvent(input$plots, {
      if (input$plots == "plot 3") {
        shinyjs::show("map_datasets")
        shinyjs::hide("downloadPlot")
        shinyjs::hide("datasets")
      } else {
        shinyjs::hide("map_datasets")
        shinyjs::show("downloadPlot")
        shinyjs::show("datasets")
      }
    })

    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste0("viz",input$plots, ".png")
      },
      content = function(file) {
        # Pass the plot to be downloaded to the png function
        png(file)
        # Render the appropriate plot depending on the selected option
        if (input$plots == "plot 1") {
          print(plot1(data()))
        } else if(input$plots == "plot 2") {
          print(plot2(data()))
        }
        # Close the plot device to save the plot
        dev.off()
      }
    )
    observeEvent(input$intro_1, {
      showModal(modalDialog(
        includeHTML("checklist_process.html"),
        easyClose = TRUE,
        footer = tagList(
          actionButton(inputId = "intro_btn", label = "INTRODUCTION TOUR", icon = icon("info-circle")),
        )
      ))
    })

    values <- reactiveValues(menu = list(FALSE, FALSE, FALSE, FALSE, FALSE,FALSE))

    output$ui_manipulate <- renderUI({
      wellPanel(
        actionButton("intro_1", "Quick Intro"),
        fluidRow(
          column(width = 12,
                 actionBttn(inputId = "outlier", label = "Outline Removal", style = "material-flat", color = "primary", block = TRUE,icon = icon("eraser"))
          ),
          column(width = 12,
                 actionBttn(inputId = "filter_df", label = "Filter Data", style = "material-flat", color = "danger",block = TRUE,icon = icon("filter"))
          ),
          column(width = 12,
                 actionBttn(inputId = "replace", label = "Remove/Replace Data", style = "material-flat", color = "default", block = TRUE, icon = icon("trash-alt"))
          )
          ,
          column(width = 12,
                 actionBttn(inputId = "run_check", label = "Check Spelling", style = "material-flat", color = "success",block = TRUE,icon("spell-check"))
          ),
          column(width = 12,
                 actionBttn(inputId = "run_fix", label = "Correct Typos", style = "material-flat", color = "royal",block = TRUE,icon = icon("pencil-alt"))
          ),
          column(width = 12,
                 actionBttn(inputId = "convert_df", label = "Convert Data", style = "material-flat", color = "warning",block = TRUE,icon = icon("exchange-alt"))
          )
        ),

        div(id = "dropdown_menu_1",
            style = ifelse(isTRUE(values$menu[[1]]), "display: block;", "display: none;"),
            class = "well",
            uiOutput("var_1"),
            radioGroupButtons(
              inputId = "outlier_sel",
              label = "Label",
              choices = c("Z_score",
                          "IQR", "MAD", "LOF"),
              justified = TRUE,
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"))
            ),
            uiOutput("var_2"),
            actionButton(
              inputId = "show_plot",
              label = "Show Plot"
            ),
            actionButton(
              inputId = "hide_plot",
              label = "Hide Plot"
            )
        ),
        div(id = "dropdown_menu_3",
            style = ifelse(isTRUE(values$menu[[3]]), "display: block;", "display: none;"),
            class = "well",
            uiOutput("var_3"),
            actionButton("check","Run Check")
        )
        ,
        div(id = "dropdown_menu_6",
            style = ifelse(isTRUE(values$menu[[6]]), "display: block;", "display: none;"),
            class = "well",
            h5("Remove and replace data from filtered dataset"),
            actionButton("start_fil", "Start Process"),
            uiOutput("col_selection"),
            actionButton("save_update", "Save Updated"),
            h5("Insert and Remove Data"),
            actionButton("add_col", "Insert Column"),
            actionButton("deleteRows", "Delete Rows"),
        ),
        div(id = "dropdown_menu_4",
            style = ifelse(isTRUE(values$menu[[4]]), "display: block;", "display: none;"),
            class = "well",
            verbatimTextOutput("current_value"),
            selectInput("input_type", "Choose Fix Optuons", choices = c("Auto Fix", "Manual Fix")),
            conditionalPanel(
              condition = "input.input_type == 'Auto Fix'",
              uiOutput("fix_sel")
            ),
            conditionalPanel(
              condition = "input.input_type == 'Manual Fix'",
              textInput("text_enter", "Please enter text",placeholder = "Enter corrected word")
            ),
            br(),
            br(),
            actionButton("move_next", "Next"),
            actionButton("move_back", "Back")
        )
        ,
        div(id = "dropdown_menu_5",
            style = ifelse(isTRUE(values$menu[[5]]), "display: block;", "display: none;"),
            class = "well",
            uiOutput("var_5"),
            prettyRadioButtons(
              inputId = "convert_opt",
              label = "Convert Data",
              choices = c("Convert Text to Number"="A", "Convert Number to Text"="B"),
              icon = icon("check"),
              bigger = TRUE,
              status = "info",
              animation = "jelly"
            ),
            actionButton("convert","Convert")
        ),
       div(id = "dropdown_menu_2",
            style = ifelse(isTRUE(values$menu[[2]]), "display: block;", "display: none;"),
            class = "well",
            uiOutput("var_4"),
            tabPanel("Typeahead",
                     h4("Filter by Expression ")
                     ,div(class="row-fluid ", div(class="well container-fluid",
                                                  div(class="container span3",
                                                      helpText("Type expression to filter dataset. "),
                                                      textInput.typeahead(
                                                        id="search"
                                                        ,placeholder = "Enter search term"
                                                        ,local = data.frame(name = my_autocomplete_list,
                                                                            info = ifelse(grepl("x|y|between", my_autocomplete_list),
                                                                                          "You need to replace x & y with numeric value.", ""),
                                                                            stringsAsFactors = FALSE)

                                                        ,valueKey = "name"
                                                        ,tokens=c(1:length(my_autocomplete_list))
                                                        ,template = HTML("<p class='repo-name'>{{name}}</p><p class='repo-description'>{{info}}</p>")
                                                      )
                                                  ),
                                                  div(class="container span6"
                                                      ,shinyalert("shinyalert3")
                                                  ))
                     )

            ),
            actionButton("paste", "Paste suggestion"),
            textAreaInput("expr", "Enter filter condition", placeholder = "Please enter the expression based on the above suggestion"),
            actionButton("filter_button", "Filter"),
            actionButton("clear_button", "Clear"),
        ))
    })

    observeEvent(input$outlier, {
      # check if other dropdown menus are visible
      for (i in 2:5) {
        if (values$menu[[i]]) {
          # skip toggling if other dropdown menu is visible
          return(NULL)
        }
      }
      # toggle display of dropdown menu 1
      values$menu[[1]] <- !values$menu[[1]]
    })

    observeEvent(input$filter_df, {
      # check if other dropdown menus are visible
      for (i in c(1, 3:5)) {
        if (values$menu[[i]]) {
          # skip toggling if other dropdown menu is visible
          return(NULL)
        }
      }
      # toggle display of dropdown menu 2
      values$menu[[2]] <- !values$menu[[2]]
    })


    observeEvent(input$run_check, {
      # check if other dropdown menus are visible
      for (i in c(1:2,5)) {
        if (values$menu[[i]]) {
          # skip toggling if other dropdown menu is visible
          return(NULL)
        }
      }
      # toggle display of dropdown menu 3
      values$menu[[3]] <- !values$menu[[3]]
    })


    observeEvent(input$run_fix, {
      # check if other dropdown menus are visible
      for (i in c(1:2,5)) {
        if (values$menu[[i]]) {
          # skip toggling if other dropdown menu is visible
          return(NULL)
        }
      }
      # toggle display of dropdown menu 4
      values$menu[[4]] <- !values$menu[[4]]
    })


    observeEvent(input$convert_df, {
      # check if other dropdown menus are visible
      for (i in c(1:4)) {
        if (values$menu[[i]]) {
          # skip toggling if other dropdown menu is visible
          return(NULL)
        }
      }
      # toggle display of dropdown menu 5
      values$menu[[5]] <- !values$menu[[5]]
    })

    observeEvent(input$replace, {
      # check if other dropdown menus are visible
      for (i in c(1:5)) {
        if (values$menu[[i]]) {
          # skip toggling if other dropdown menu is visible
          return(NULL)
        }
      }
      # toggle display of dropdown menu 6
      values$menu[[6]] <- !values$menu[[6]]
    })

    observeEvent(input$input_type, {
      if (input$input_type == "Auto Fix") {
        disable("text_enter")
        enable("fix_word")
      } else {
        disable("fix_word")
        enable("text_enter")
      }
    })

    observeEvent(input$check, {
      req(input$var_select_3)
      df<-result_store$val
      # Check if column is of character type
      if (!is.character(df[[input$var_select_3]])) {
        showModal(modalDialog(
          title = "Column Type Error",
          "The selected column is not of type 'character'.",
          "Do you want to convert it to character and proceed?",
          footer = tagList(
            actionButton("convert_char", "Convert"),
            modalButton("Cancel")
          )
        ))
      } else {
        # Call check_spelling function on sample dataframe
        result_hunspell <- check_spelling(df, input$var_select_3)
        if (!is.null(result_hunspell)){
          # Output highlighted dataframe
          output$highlighted_table <- DT::renderDataTable({
            DT::datatable(result_hunspell, escape = FALSE, options = list(pageLength = 15))
          })
        }else{
          output$spelling<-renderText({"Cannot find any typos"})
        }
      }
    })

    observeEvent(input$convert_char, {
      req(input$var_select_3)
      df<-result_store$val
      # Convert column to character type
      df[[input$var_select_3]] <- as.character(df[[input$var_select_3]])

      # Call check_spelling function on updated dataframe
      result_hunspell <- check_spelling(df, input$var_select_3)

      if (!is.null(result_hunspell)){
        # Output highlighted dataframe
        output$highlighted_table <- DT::renderDataTable({
          DT::datatable(result_hunspell, escape = FALSE, options = list(pageLength = 15))
        })
      }else{
        output$spelling<-renderText({"Cannot find any typos"})
      }
    })

    rv <- reactiveValues(val = 1)
    unique_values<-eventReactive(input$var_select_3,{
      req(input$var_select_3)
      result_spell <- check_spelling(result_store$val, input$var_select_3)
      unique_words <- unique(result_spell$mispelling_word)
      return(unique_words)
    })

    observe({
      # Define reactive values
      correction_df <- reactiveValues(
        data = data.frame(word =unique_values(), corrected_word = "", stringsAsFactors = FALSE)
      )
      observeEvent(input$fix_word,{
        req(input$fix_word)
        if (!is.null(input$fix_word)){
          correction_df$data$corrected_word[rv$val] <- input$fix_word
        }else{
          correction_df$data$corrected_word[rv$val] <- ""
        }
      })

      observeEvent(input$text_enter,{
        req(input$text_enter)
        if (!is.null(input$text_enter)){
          correction_df$data$corrected_word[rv$val] <- input$text_enter
        }else{
          correction_df$data$corrected_word[rv$val] <- ""
        }
      })

      output$current_value <<- renderText({
        unique_values()[rv$val]
      })

      output$fix_sel<-renderUI({
        selectInput(
          inputId = "fix_word",
          label = "Choose corrected word",
          choices = c("",setNames(hunspell_suggest(unique_values()[rv$val]), hunspell_suggest(unique_values()[rv$val]))))
      })


      # Update correction dataframe with current suggestion and move to next word
      observeEvent(input$move_next, {
        req(input$move_next)
        if (rv$val < length(unique_values())) {
          rv$val <- rv$val + 1
          updateSelectInput(
            inputId = "fix_word",
            label = "Choose corrected word",
            choices = c("",setNames(hunspell_suggest(unique_values()[rv$val]), hunspell_suggest(unique_values()[rv$val])))
          )

          updateTextInput(session, "text_enter", value = "")

          observeEvent(input$fix_word,{
            req(input$fix_word)
            if (!is.null(input$fix_word)){
              correction_df$data$corrected_word[rv$val] <- input$fix_word
            }else{
              correction_df$data$corrected_word[rv$val] <- ""
            }
          })

          observeEvent(input$text_enter,{
            req(input$text_enter)
            if (!is.null(input$text_enter)){
              correction_df$data$corrected_word[rv$val] <- input$text_enter
            }else{
              correction_df$data$corrected_word[rv$val] <- ""
            }
          })
          output$current_value <- renderText({
            unique_values()[rv$val]
          })
        } else {
          output$table_review <- DT::renderDataTable({
            render_custom_datatable(correction_df$data)
          })
          showModal(fix_final)
          rv$val<<-1
        }
      })

      observeEvent(input$move_back, {
        req(input$move_back)
        if (rv$val > 1) {
          rv$val <- rv$val - 1
          updateSelectInput(
            inputId = "fix_word",
            label = "Choose corrected word",
            choices = c("",setNames(hunspell_suggest(unique_values()[rv$val]), hunspell_suggest(unique_values()[rv$val]))))
          observeEvent(input$fix_word,{
            req(input$fix_word)
            if (!is.null(input$fix_word)){
              correction_df$data$corrected_word[rv$val] <- input$fix_word
            }else{
              correction_df$data$corrected_word[rv$val] <- ""
            }
          })
          observeEvent(input$text_enter,{
            req(input$text_enter)
            if (!is.null(input$text_enter)){
              correction_df$data$corrected_word[rv$val] <- input$text_enter
            }else{
              correction_df$data$corrected_word[rv$val] <- ""
            }
          })
          output$current_value <- renderText({
            unique_values()[rv$val]
          })
        }
      })

      observeEvent(input$cancel_fix, {
        req(input$cancel_fix)
        # reset reactive values
        rv$val <- 1
        # clear correction dataframes
        correction_df$data$corrected_word <- ""
        # remove modal
        removeModal()
      })

      observeEvent(input$fix_accept,{
        req(input$fix_accept)
        df_copy <<- result_store$val  # assign df_copy to global environment
        if (!is.na(correction_df$data$corrected_word[1])) {
          for (i in 1:nrow(correction_df$data)) {
            word <-correction_df$data$word[i]
            corrected_word <-correction_df$data$corrected_word[i]
            if (!is.na(corrected_word)) {
              # Replace all instances of the word with the corrected word in the copy dataset
              df_copy[[input$var_select_3]] <- gsub(paste0("\\b", word, "\\b"), corrected_word, df_copy[[input$var_select_3]], ignore.case = TRUE)
              sendSweetAlert(
                session = session,
                title = "Success !!",
                text = paste0("Successfully fixed typos."),
                type = "success"
              )
            } else {
              sendSweetAlert(
                session = session,
                title = "Error...",
                text = "Error !",
                type = "error"
              )
              return()
            }
          }
          removeModal()
        } else {

          return(df_copy)

        }
      })


    })

    fix_final <- modalDialog(
      title = "Preview",
      dataTableOutput("table_review"),
      footer = tagList(
        actionButton("fix_accept", "Accept corrections"),
        modalButton("Cancel")
      ))

    observeEvent(input$check, {
      req(input$check)
      output$my_output <- renderUI({
        fluidRow(
          column(
            width = 8,
            dataTableOutput("highlighted_table")
          ),
          column(
            width = 4,
            textOutput("spelling")
          )
        )
      })
    })

    observeEvent(c(input$outlier_sel,input$var_select_1),{
      req(input$var_select_1,input$outlier_sel)
      if(input$outlier_sel=="Z_score"){
        showModal(modalDialog(
          selectInput("Z_score_sel", "Please Choose Z_Threshold",

                      choices = c("2", "3")),
          footer = tagList(
            actionButton("remove_1", "Remove"),
            modalButton("Cancel")
          ))
        )
      }else if(input$outlier_sel=="IQR"){
        showModal(modalDialog(
          title="The IQR calculation exclude any data points that fall below or above the IQR value",
          footer = tagList(
            actionButton("remove_2", "Remove"),
            modalButton("Cancel")
          )
        ))
      }else if(input$outlier_sel=="MAD"){
        showModal(modalDialog(
          title = "The MAD calculation excludes any data points that fall below or above the threshold value",
          textInput("add_val", "Please add K value"),
          footer = tagList(
            actionButton("remove_3", "Remove"),
            modalButton("Cancel")
          )
        ))
      }else{
        showModal(modalDialog(
         title="The LOF calculation excludes any data points that fall outside the neighborhood",
         sliderInput("lof_outlier_num", "Choose a number:", min = 5, max = 50, step = 1, value = 5),
              footer = tagList(
                actionButton("remove_4", "Remove"),
                modalButton("Cancel")
            ))
        )
      }
    })

    observeEvent(input$paste, {
      updateTextAreaInput(session, "expr", value = input$search)
    })

    observeEvent(input$clear_button, {
      updateTextAreaInput(session, "expr", value = "")
    })

    observeEvent(input$var_select_1,{
      req(input$var_select_1)
      names(result_store$val) <- trimws(names(result_store$val))
      # define initial choices for var_select_2
      choices_2 <- reactive({c("", subset(names(result_store$val), names(result_store$val) != input$var_select_1))})
      output$var_2 <- renderUI({
        selectInput(
          inputId = "var_select_2",
          label = "Choose variable to plot against",
          choices = choices_2(),
          selected = ""
        )
      })
    })

    observeEvent(c(input$remove_1,input$var_select_1,input$var_select_2, input$Z_score_sel,input$mul_select),{
      req(input$remove_1,input$var_select_1,input$Z_score_sel)
      df_copy_1<-result_store$val
      names(df_copy_1) <- trimws(names(df_copy_1))
      df<-z_outlier(df_copy_1, input$var_select_1,input$Z_score_sel)
      all_df<-df$df2
      isolate_df<-df$df1
      multiple_df<-all_df %>%select(all_of(input$mul_select))
      names(all_df) <- trimws(names(all_df))
      all_df_table <- render_custom_datatable_1(all_df)
      isolate_df_table <- render_custom_datatable_1(isolate_df)
      multiple_df_table <- render_custom_datatable_1(multiple_df)
      output$table_all <- renderDataTable({all_df_table})
      output$table_isolate <- renderDataTable({isolate_df_table})
      output$table_multiple <- renderDataTable({multiple_df_table})
      output$whisker <- renderPlotly({box_and_whisker_plot_1(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$whisker1 <- renderPlotly({box_and_whisker_plot_1(all_df,input$var_select_2,input$var_select_1 )})
      output$density <- renderPlotly({density_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$density1 <- renderPlotly({density_plot(all_df,input$var_select_2,input$var_select_1 )})
      output$scatter <- renderPlotly({scatter_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$scatter1 <- renderPlotly({scatter_plot(all_df,input$var_select_2,input$var_select_1 )})
      removeModal()
    })

    #obserEvent-IQR OUTLIER REMOVAL 
    observeEvent(c(input$remove_2,input$var_select_1,input$var_select_2),{
      req(input$remove_2,input$var_select_1)
      df_copy_1<-result_store$val
      names(df_copy_1) <- trimws(names(df_copy_1))
      df<-iqr_outlier(df_copy_1, input$var_select_1)
      all_df<-df$df2
      isolate_df<-df$df1
      multiple_df<-all_df %>%select(all_of(input$mul_select))
      names(all_df) <- trimws(names(all_df))
      all_df_table <- render_custom_datatable_1(all_df)
      isolate_df_table <- render_custom_datatable_1(isolate_df)
      multiple_df_table <- render_custom_datatable_1(multiple_df)
      output$table_all <- renderDataTable({all_df_table})
      output$table_isolate <- renderDataTable({isolate_df_table})
      output$table_multiple <- renderDataTable({multiple_df_table})
      output$whisker <- renderPlotly({box_and_whisker_plot_1(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$whisker1 <- renderPlotly({box_and_whisker_plot_1(all_df,input$var_select_2,input$var_select_1 )})
      output$density <- renderPlotly({density_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$density1 <- renderPlotly({density_plot(all_df,input$var_select_2,input$var_select_1 )})
      output$scatter <- renderPlotly({scatter_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$scatter1 <- renderPlotly({scatter_plot(all_df,input$var_select_2,input$var_select_1 )})
      removeModal()
    })

    #obserEvent-MAD OUTLIER REMOVAL 
    observeEvent(c(input$remove_3,input$var_select_1),{
      req(input$remove_3,input$var_select_1)
      df_copy_1<-result_store$val
      names(df_copy_1) <- trimws(names(df_copy_1))
      df<-mad_outlier(df_copy_1, input$var_select_1)
      all_df<-df$df2
      isolate_df<-df$df1
      multiple_df<-all_df %>%select(all_of(input$mul_select))
      names(all_df) <- trimws(names(all_df))
      all_df_table <- render_custom_datatable_1(all_df)
      isolate_df_table <- render_custom_datatable_1(isolate_df)
      multiple_df_table <- render_custom_datatable_1(multiple_df)
      output$table_all <- renderDataTable({all_df_table})
      output$table_isolate <- renderDataTable({isolate_df_table})
      output$table_multiple <- renderDataTable({multiple_df_table})
      output$whisker <- renderPlotly({box_and_whisker_plot_1(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$whisker1 <- renderPlotly({box_and_whisker_plot_1(all_df,input$var_select_2,input$var_select_1 )})
      output$density <- renderPlotly({density_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$density1 <- renderPlotly({density_plot(all_df,input$var_select_2,input$var_select_1 )})
      output$scatter <- renderPlotly({scatter_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$scatter1 <- renderPlotly({scatter_plot(all_df,input$var_select_2,input$var_select_1 )})
      removeModal()
    })

    #obserEvent-LOF OUTLIER REMOVAL 
    observeEvent(c(input$remove_4,input$var_select_1, input$lof_outlier_num),{
      req(input$remove_4,input$var_select_1,input$lof_outlier_num)
      df_copy_1<-result_store$val
      names(df_copy_1) <- trimws(names(df_copy_1))
      df<-lof_outlier(df_copy_1, input$var_select_1, input$lof_outlier_num)
      all_df<-df$df2
      isolate_df<-df$df1
      multiple_df<-all_df %>%select(all_of(input$mul_select))
      names(all_df) <- trimws(names(all_df))
      all_df_table <- render_custom_datatable_1(all_df)
      isolate_df_table <- render_custom_datatable_1(isolate_df)
      multiple_df_table <- render_custom_datatable_1(multiple_df)
      output$table_all <- renderDataTable({all_df_table})
      output$table_isolate <- renderDataTable({isolate_df_table})
      output$table_multiple <- renderDataTable({multiple_df_table})
      output$whisker <- renderPlotly({box_and_whisker_plot_1(df_copy_1,input$var_select_2,input$
       )})
      output$whisker1 <- renderPlotly({box_and_whisker_plot_1(all_df,input$var_select_2,input$var_select_1 )})
      output$density <- renderPlotly({density_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$density1 <- renderPlotly({density_plot(all_df,input$var_select_2,input$var_select_1 )})
      output$scatter <- renderPlotly({scatter_plot(df_copy_1,input$var_select_2,input$var_select_1 )})
      output$scatter1 <- renderPlotly({scatter_plot(all_df,input$var_select_2,input$var_select_1 )})
      removeModal()
    })

    #Display filtered data table
    sel_output <- eventReactive(input$viz_data, {
      req(input$viz_data)
      switch(
        input$viz_data,
        "Show All" = dataTableOutput("table_all"),
        "Show Multiples" = dataTableOutput("table_multiple"),
        "Show Isolate" = dataTableOutput("table_isolate")
      )
    })

    #obserEvent-SHOW PLOT 
    observeEvent(input$show_plot, {
      req(input$plot_sel)
      shinyjs::show("my_output_1")
      if (isTruthy(input$show_plot)) {
        plot_list <- input$plot_sel
        plot_list_1 <- paste0(plot_list, "1")
        # Create a reactive function to generate the plotlyOutput elements
        output_list <- reactive({
          # Create a list of plotlyOutput elements
          lapply(plot_list, function(plot_id) {
            plot_id_1 <- paste0(plot_id, "1")
            splitLayout(
              cellWidths = 600,
              style = "border: 1px solid silver;",
              cellArgs = list(style = "padding: 6px"),
              conditionalPanel(
                condition = "input.show_plot",
                shinycssloaders::withSpinner(plotlyOutput(plot_id),
                                             type = 5,
                                             color = "#ffffff",
                                             size = 5)
              ),
              conditionalPanel(
                condition = "input.show_plot",
                shinycssloaders::withSpinner(plotlyOutput(plot_id_1),
                                             type = 5,
                                             color = "#ffffff",
                                           size = 5)
              )
            )
          })
        })

        # Generate the UI elements dynamically based on the selected input values
        output$my_output_1 <- renderUI({
          if (length(plot_list) == 0) {
            return(NULL)
          } else if (length(plot_list) == 1) {
            splitLayout(
              cellWidths = 600,
              style = "border: 1px solid silver;",
              cellArgs = list(style = "padding: 6px"),
              conditionalPanel(
                condition = "input.show_plot",
                shinycssloaders::withSpinner(plotlyOutput(plot_id),
                                             type = 5,
                                             color = "#ffffff",
                                             size = 5)
              ),
              conditionalPanel(
                condition = "input.show_plot",
                shinycssloaders::withSpinner(plotlyOutput(plot_id_1),
                                             type = 5,
                                             color = "#ffffff",
                                             size = 5)
              )
            )
          } else {
            # Use tagList to display the plotlyOutput elements vertically
            tagList(
              lapply(output_list(), function(output) {
                div(
                  shinycssloaders::withSpinner(output, type = 5, color = "#ffffff", size = 5),
                  style = "padding: 6px;"
                )
              })
            )
          }
        })
      } else {
        output$my_output_1 <- NULL
      }
    })

    observeEvent(input$outlier, {
      req(input$outlier)
      output$my_output <- renderUI({
        splitLayout(
          cellWidths = 600,
          cellArgs = list(style = "padding: 6px"),
          style = "border: 1px solid silver;",
          dataTableOutput("table_2"),
          sel_output()
        )
      })
    })

    #obserEvent-HIDE PLOT 
    observeEvent(input$hide_plot,{
      req(input$hide_plot)
      shinyjs::hide("my_output_1")
    })

    #obserEvent-CONVERT 
    observeEvent(input$convert, {
      req(input$convert,input$convert_opt, input$var_select_5)
      df_copy_3 <- result_store$val
      col_value <- df_copy_3[[input$var_select_5]]

      if (input$convert_opt == "A") {
        converted_data <- text_to_num(df_copy_3, input$var_select_5)
        converted_df<-converted_data$data
        showModal(modalDialog(
          title = paste0(converted_data$converted, " rows were successfully converted.\n",
                         converted_data$invalid, " rows were invalid and being kept.")
         )
        )
      } else {
        converted_data <- num_to_text(df_copy_3, input$var_select_5)
        converted_df<-converted_data$df
        showModal(
          modalDialog(
            title = paste0(converted_data$num_converted, " rows were successfully converted.\n",
                           converted_data$num_invalid, " rows were invalid and being kept.")
          )

        )
      }
      output$table_convert<-renderDataTable({
        render_custom_datatable(converted_df)
      })
      removeModal()
    })

    observeEvent(input$convert, {
      req(input$convert)
      output$my_output <- renderUI({
        fluidRow(
          dataTableOutput("table_convert")
        )
      })
    })

    observeEvent(input$filter_df, {
      req(input$filter_df)
      output$my_output <- renderUI({
        fluidRow(
          dataTableOutput("filtered_table_1")
        )
      })
    })

    observeEvent(input$replace, {
      req(input$replace)
      output$my_output <- renderUI({
        fluidRow(
          dataTableOutput("filtered_table")
        )
      })
    })

    # Create a reactiveValues object
    search_values <- reactiveValues(last_search = "", last_select = NULL, last_row = NULL)

    observeEvent(input$start_fil, {
      req(input$start_fil)

      last_filtered_data<-filtered_df(result_store$val,search_values$last_select, search_values$last_search, search_values$last_row)

      names(last_filtered_data)<-trimws(names(last_filtered_data))

      choices<-names(last_filtered_data)

      output$col_selection<-renderUI({
        pickerInput(
          inputId = "col_exclude",
          label = "Choose Columns not Edit",
          choices = choices,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE,
            title = "Please columns not to include for editing",
            choicesOpt = list(
              sprintf(
                "<span class='label label-%s'>%s</span>",
                choices,
                paste(choices, collapse = "")
              )
            )
          )
        )
      })

      # Create a copy of the original dataset
      df_copy_6 <- result_store$val

      # Create an editable dataframe
      editable_df <- reactiveVal(last_filtered_data)

      output$filtered_table <- renderDataTable({
        editable_df() %>%
          # Add a column to track changes
          mutate(changed = FALSE) %>%
          # Rename columns to remove spaces
          setNames(nm = gsub(" ", "_", names(.))) %>%
          DT::datatable(
            options = list(
              paging = FALSE,
              searching = FALSE,
              ordering = FALSE,
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                "$(this).parent().addClass('d-flex flex-row-reverse align-items-center justify-content-between');",
                "$(this).parent().siblings('.dataTables_info').addClass('mt-3 mr-auto');",
                "}"
              ),
              columnDefs = list(
                list(className = 'dt-center', targets = "_all", createdCell = JS(
                  "function (td, cellData, rowData, row, col) {
              if (rowData.changed) {
                $(td).addClass('edited-cell');
              }
            }"
                ))
              ),
              dom = '<"d-flex flex-row justify-content-between align-items-center"<"mr-2"l><"dt-pagination-wrapper flex-grow-1"p>><"table-responsive"t><"d-flex flex-row justify-content-between align-items-center mt-3"i>',
              info = TRUE
            ),
            filter = "top",
            selection = 'multiple',
            style = 'bootstrap',
            class = 'cell-border stripe',
            rownames = FALSE,
            width = "100%",
            editable = list(target = "row", disable = list(columns = input$col_exclude)))
      })

      # Handle cell editing
      observeEvent(input$filtered_table_cell_edit, {
        # Get the row and column of the edited cell
        row <- input$filtered_table_cell_edit$row
        col <- input$filtered_table_cell_edit$col

        # Update the value in the editable dataframe
        edited_data <- input$filtered_table_cell_edit$data

        # Set the 'changed' flag to TRUE for the edited row
        editable_df()[row, 'changed'] <<- TRUE
        DT::replaceData(editable_df(), edited_data, row)
      })

      # Update the copy of the original dataset based on the edited filtered_df
      observeEvent(input$save_update, {
        # Get the rows that have been changed
        changed_rows <- editable_df() %>% filter(changed == TRUE)

        # Find matches between changed rows and df_copy_6
        matches <- merge(df_copy_6, changed_rows, all = TRUE)

        # Update df_copy_6 based on the changes
        for (i in 1:nrow(matches)) {
          if (!is.na(matches[i, 'changed'])) {
            df_copy_6[matches[i, 'Row'], matches[i, 'col']] <<- matches[i, 'value']
          }
        }
        write.csv(df_copy_6, "updated_data.csv")
      })
    })

    #obserEvent-FILTER DATA FRAME
    observeEvent(c(input$filter_button,input$expr,input$var_select_4, input$rows),{
      req(input$filter_button)
      filtered_data<-filtered_df(result_store$val, input$var_select_4, input$expr,input$rows)
      search_values$last_search <- input$expr
      search_values$last_select <- input$var_select_4
      search_values$last_row <- input$rows
      output$filtered_table_1 <- renderDataTable({render_custom_datatable(filtered_data)})
    })

    #obserEvent-ADD COLUMN
    observeEvent(input$add_col,{
      showModal(
        modalDialog(
          title = "Insert Column Options",
          uiOutput("var_6"),
          textInput("col_add", "Please enter column name",placeholder = "Enter Inserted Column Name"),
          footer = tagList(
            actionButton("Insert", "Insert"),
            modalButton("Cancel")
          )))
    })

    #obserEvent-INSERT COLUMN
    observeEvent(input$insert,{
      req(input$var_select_6, input$col_add)
      fillvalues(result_store$val, input$col_add, input$var_select_6)
      removeModal()
    })
    #delterows
    observeEvent(input$deleteRows,{
      if (!is.null(input$table_rows_selected)) {
        result_store$val <- result_store$val[-as.numeric(input$table_rows_selected),]
      }
    })

    # Define reactive value to store current page
    current_page <- reactiveVal("landingPageUI")

    # Define observer for login button
    observeEvent(input$login, {
      current_page("loginUI")
    })

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


    #THIS PART IS EXTRA
    mytable_1 <- data.frame(
      ID           = integer(),
      `Location X` = numeric(),
      `Location Y` = numeric(),
      Notes        = character(),
      check.names = FALSE
    )
    ID <- reactiveVal(0L)
    Xcoords <- reactiveVal()
    Ycoords <- reactiveVal()
    #mytable
    output[["mytable_1"]] <- renderDT({
      datatable(
        mytable_1,
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = c(0L, 1L, 2L))),
        callback = JS(callback)
      )
    }, server = FALSE)

    proxy <- dataTableProxy("mytable_1")
    #bind clicks
    observeEvent(input[["plot_click"]], {
      x <- input[["plot_click"]][["x"]]
      y <- input[["plot_click"]][["y"]]
      Xcoords(c(Xcoords(), x))
      Ycoords(c(Ycoords(), y))
      newRow <- as.data.frame(list(ID() + 1L, x, y, ""))
      ID(ID() + 1L)
      addRow(proxy, newRow, resetPaging = FALSE)
    })

    #interactive plot
    output[["plot_coor"]] <- renderPlot({
      plot(c(-25, 25), c(-50, 50), type = "n", ylab = NA, xlab = NA)
      points(Xcoords(), Ycoords(), cex = 2, pch = 19, col = "black")
    })

    # remove btn
    observeEvent(input[["remove1"]], {
      req(input[["mytable_1_rows_selected"]])
      indices <- input[["mytable_1_rows_selected"]]
      Xcoords(Xcoords()[-indices])
      Ycoords(Ycoords()[-indices])
    })
    #This is for network graph
    # Global constants --------------------------------------------------------

    # number of rows in the HTML data table showing all packages
    ROWS_PER_PAGE <- 15

    ################################################################################
    # Module featured_packages
    #
    # Author: Stefan Schliebs
    # Created: 2019-03-06 08:25:53
    ################################################################################

    # Package table  ----------------------------------------------------------
    # data for the table to render
    d_package_table <- reactive({
      d_pkg_dependencies() %>%
        filter(dependency != "R") %>%
        count(dependency, sort = TRUE) %>%
        rename(package = dependency, `# refs` = n)
    })
    # render prepared package table
    output$package_table <- DT::renderDT({
      d_package_table() %>%
        DT::datatable(
          options = list(pageLength = ROWS_PER_PAGE, responsive = TRUE, dom = "ftip"),
          selection = list(mode = "single"),
          class = "display compact",
          rownames = FALSE
        )
    })

    # pre-select a package to reveal the functionality of the app
    observe({
      req(d_package_table())

      # determine the row in the package table that contains the package representing the selected node
      row_index <- which(d_package_table()$package == "png")

      DT::dataTableProxy("package_table") %>%
        DT::selectRows(row_index) %>%
        DT::selectPage(which(input$package_table_rows_all == row_index) %/% ROWS_PER_PAGE + 1)
    })

    # Ego graph ---------------------------------------------------------------

    # create network graph from d_pkg_dependencies()
    g_deps <- reactive({
      req(d_pkg_dependencies())

      d_pkg_dependencies() %>%
        filter(dependency != "R") %>%
        select(from = package, to = dependency) %>%
        igraph::graph_from_data_frame(directed = TRUE)
    })

    # update center node of the ego network when a new row is selected in the package table
    center_node <- reactive({
      req(input$package_table_rows_selected)
      d_package_table() %>%
        slice(input$package_table_rows_selected) %>%
        pull(package)
    })

    # ego graph extracted using igraph
    g_ego <- reactive({
      req(g_deps(), center_node())
      # req(g_deps(), input$order, center_node())

      # extract ego graph depending on user input
      g <-
        g_deps() %>%
        igraph::make_ego_graph(order = 3, nodes = center_node(), mode = "in") %>%
        # igraph::make_ego_graph(order = input$order, nodes = center_node(), mode = "in") %>%
        .[[1]]
      print(g)

      # set node size proportional to node degree
      node_degree <- igraph::degree(g)
      igraph::V(g)$size <- ifelse(node_degree < 50, 2 * node_degree, 100) + 10

      g
    })

    # Layout stop/start functionality -----------------------------------------

    # switch on/off network physics engine depending on button state
    observeEvent(input$graph_physics_enabled, {
      visNetworkProxy("graph") %>%
        visPhysics(enabled = input$graph_physics_enabled)
    })

    # Dependency network ------------------------------------------------------

    # javascript function to control progress bar during network layouting
    progress_js_func <- function() {
      glue(
        "function(params) {{",
        "  var maxWidth = 100;",
        "  var minWidth = 20;",
        "  var widthFactor = params.iterations / params.total;",
        "  var width = Math.max(minWidth, maxWidth * widthFactor);",
        "  document.getElementById('{'loading-bar'}').style.visibility = 'visible';",
        "  document.getElementById('{'progress-bar'}').style.width = width + 'px';",
        "  document.getElementById('{'loading-text'}').innerHTML = 'loading - ' + Math.round(widthFactor*100) + '%';",
        "}}"
      )
    }

    # javascript function to switch off physics engine after the maximum layout iterations have finished
    stabilised_js_func <- function() {
      glue(
        "function() {{",
        "  document.getElementById('{'progress-bar'}').style.width = '100px';",
        "  document.getElementById('{'loading-text'}').innerHTML = 'loading - 100%';",
        "  document.getElementById('graph{'graph'}').chart.stopSimulation();",
        "  document.getElementById('{'loading-bar'}').style.visibility = 'hidden';",
        "}}"
      )
    }

    # render the network graph
    output$graph <- renderVisNetwork({
      # convert igraph into visnetwork graph
      data <- visNetwork::toVisNetworkData(g_ego())
      # browser()

      # add node properties:
      # - colorise center node and label
      # - set label font size proportional to degree of node
      nodes <-
        data$nodes %>%
        mutate(
          font.size = 3 * size,
          color = ifelse(id == isolate(center_node()), "firebrick", "#97C2FC"),
          font.color = ifelse(id == isolate(center_node()), "firebrick", "#343434")
        ) %>%
        left_join(d_pkg_details() %>% select(id = package, title), by = "id")

      # render the network
      visNetwork(
        nodes = nodes,
        edges = data$edges
      ) %>%

        # use straight edges to improve rendering performance
        visEdges(smooth = FALSE, color = list(opacity = 0.5)) %>%
        # visInteraction(hideEdgesOnDrag = TRUE) %>%

        # configure layouting algorithm
        visPhysics(
          solver = "forceAtlas2Based",
          timestep = 0.5,
          minVelocity = 1,
          maxVelocity = 30,
          forceAtlas2Based = list(gravitationalConstant = -200, damping = 1),
          stabilization = list(iterations = 200, updateInterval = 10),
          adaptiveTimestep = TRUE
        ) %>%

        visOptions(nodesIdSelection = list(enabled = TRUE, style = "margin-bottom: -30px; visibility: hidden")) %>%

        # connect custom javascript functions to network events
        visEvents(
          stabilizationProgress = progress_js_func(),
          stabilizationIterationsDone = stabilised_js_func()
        )
    })

    # Network interaction -----------------------------------------------------

    observeEvent(input$graph_selected, {
      req(input$graph_selected)

      # determine the row in the package table that contains the package representing the selected node
      row_index <- which(d_package_table()$package == input$graph_selected)

      # select the identified row and jump to the page in the table
      DT::dataTableProxy("package_table") %>%
        DT::selectRows(row_index) %>%
        DT::selectPage(which(input$package_table_rows_all == row_index) %/% ROWS_PER_PAGE + 1)
    })
    # Package info box --------------------------------------------------------

    output$package_info <- renderUI({
      req(selected_package())
      current_version <- selected_package()$versions %>% filter(published == max(published)) %>% pull(version)
      last_update <- max(selected_package()$versions$published) %>% as.Date()
      first_published <- min(selected_package()$versions$published) %>% as.Date()
      num_updates <- nrow(selected_package()$versions) %>% format(big.mark = ",")
      tagList(
        h4(selected_package()$details$package),
        h5(selected_package()$details$title),
        h5("Version", current_version),
        p("Published: ", tags$em(first_published), style = "font-size: 9pt"),
        p("Last update: ", tags$em(last_update), style = "font-size: 9pt"),
        p(tags$em(num_updates), "updates", style = "font-size: 9pt"),
        div(
          actionBttn("show_pkg_details", label = "Details", size = "xs"),
          style = "margin-top: 15px;"
        ),
        hr()
      )
    })

    observeEvent(input$show_pkg_details, {
      sendSweetAlert(
        session = session,
        title = center_node(),
        text = fluidRow(
          column(
            width = 12,
            uiOutput("package_details"),
            style = "font-size: 9pt"
          ),
          column(
            width = 12,
            DT::DTOutput("version_table"),
            style = "font-size: 8pt"
          )
          # style = "margin-top: 50px"
        ),
        html = TRUE
      )
    })
    #This part will be expand
    # Package details ---------------------------------------------------------
    selected_package <- reactive({
      req(center_node())
      list(
        details = d_pkg_details() %>% filter(package == center_node()),
        versions = d_pkg_releases() %>% filter(package == center_node())
      )
    })

    output$package_details <- renderUI({
      req(selected_package())
      tagList(
        h5(selected_package()$details$title),
        p(selected_package()$details$description),
        h5("Publication History", style = "margin-top: 15px")
      )
    })

    output$version_table <- DT::renderDT({
      req(selected_package())
      selected_package()$versions %>%
        select(-package) %>%
        arrange(desc(published)) %>%
        DT::datatable(
          options = list(pageLength = 5, responsive = TRUE, dom = "tp"),
          selection = "single",
          class = "display compact",
          rownames = FALSE
        )
    })

}}





