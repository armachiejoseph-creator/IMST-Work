library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(markdown)

# --- 1. AUTHENTICATION (Hybrid Cloud/Local) ---
options(gargle_oauth_email = TRUE) 
options(gargle_oauth_cache = ".secrets")

# Define the local path for your testing
app_dir <- getwd()
LOCAL_AUTH <- file.path(app_dir, "service_auth.json")

# 1. Check if we are on the server (using Environment Variable)
if (Sys.getenv("GOOGLE_JSON_KEY") != "") {
  # Create a temporary file from the secret variable
  t_file <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GOOGLE_JSON_KEY"), t_file)
  
  drive_auth(path = t_file)
  gs4_auth(token = drive_token())
  
  # 2. If no variable, check for the local file (for your desktop/Posit Cloud testing)
} else if (file.exists(LOCAL_AUTH)) {
  drive_auth(path = LOCAL_AUTH)
  gs4_auth(token = drive_token())
  
} else {
  stop("CRITICAL ERROR: No authentication found. Upload service_auth.json or set GOOGLE_JSON_KEY.")
}

SHEET_ID <- "1ewnpXKjQt45mU-2-Nh9g5OlHzXGe9Uy5NzNtEvifHD0"



# --- 2. UI ---
ui <- page_navbar(
  title = "Africa CDC-WHO IMST | Results Framework Matrix",
  id = "main_nav",
  navbar_options = navbar_options(position = "fixed-top"), # Updated Line 37
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#006838",
    secondary = "#FDB913"
  ),
  
  header = tagList(
    tags$style(HTML("
      body { padding-top: 100px !important; background-color: #f8f9fa; }
      .card-pirs { border-left: 10px solid #006838; margin-bottom: 25px; font-size: 0.85rem; }
      .section-title { color: #006838; font-weight: bold; border-bottom: 1px solid #eee; margin-bottom: 15px; margin-top: 20px; }
      .card-label { font-weight: bold; color: #555; text-transform: uppercase; font-size: 0.7rem; display: block; margin-top: 5px; }
      .hist-link { font-size: 0.72rem; float: right; color: #006838; text-decoration: underline; cursor: pointer; font-weight: normal; }
      textarea { resize: both !important; min-height: 80px; width: 100%; }
      .card-body-text { white-space: pre-wrap; word-wrap: break-word; }
    ")),
    tags$script(HTML("
      $(document).on('click', '.edit-btn', function() {
        Shiny.setInputValue('card_id_trigger', this.id, {priority: 'event'});
        Shiny.setInputValue('row_trigger', $(this).attr('data-row'), {priority: 'event'});
      });
    "))
  ),
  
  nav_panel(
    "Instructions",
    div(
      class = "instruction-container",
      if (file.exists("instruction.md")) includeMarkdown("instruction.md") else h4("instruction.md not found.")
    )
  ),
  
  nav_panel(
    "Gallery Registry",
    div(
      style = "max-width: 1500px; margin: auto; padding: 15px;",
      layout_column_wrap(
        width = 1/5,
        selectInput("f_level", "Filter Level:", choices = c("All", "Outcome-long term", "Outcome", "Output")),
        selectInput("f_pillar", "Filter Pillar:", choices = c("All", "Coordination", "IPC_CM", "Laboratory", "Logistics", "RCCE", "Research", "Surveillance", "Vaccination", "WASH", "Essential service")),
        textInput("f_search", "Search:"),
        div(style = "padding-top: 32px;", actionButton("refresh_data", "Refresh", icon = icon("sync"), class = "btn-outline-primary w-100")),
        div(style = "padding-top: 32px;", actionButton("add_new", "Add New", icon = icon("plus"), class = "btn-success w-100"))
      ),
      uiOutput("gallery_ui")
    )
  ),
  
  nav_panel(
    "Editor",
    div(
      style = "max-width: 1000px; margin: auto; padding-bottom: 50px;",
      uiOutput("edit_form_ui"),
      conditionalPanel(
        condition = "!input.card_id_trigger && !input.add_new",
        h4("Select an indicator to edit.", style = "color:#999; text-align:center; margin-top:50px;")
      )
    )
  ),
  
  nav_panel(
    "Admin",
    div(
      style = "max-width:1000px; margin:auto; padding:20px;",
      card(
        card_header("Admin"),
        card_body(p("Settings."))
      )
    )
  )
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  
  editing_id <- reactiveVal(NULL)
  is_new_entry <- reactiveVal(FALSE)
  refresh_trigger <- reactiveVal(0)
  
  pirs_data <- reactive({
    refresh_trigger()
    read_sheet(SHEET_ID, sheet = "pirs_db", col_types = "c")
  })
  
  observeEvent(input$refresh_data, {
    refresh_trigger(refresh_trigger() + 1)
  })
  
  clean_val <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x)) || x[1] == "NA") "" else as.character(x[1])
  }
  
  # --- HISTORY VIEWER ---
  show_hist <- function(label, db_field) {
    req(editing_id())
    
    logs <- read_sheet(SHEET_ID, sheet = "audit_log", col_types = "c") %>%
      filter(indicator_id == editing_id(), field_name == db_field) %>%
      arrange(desc(change_time))
    
    output$history_table <- renderTable({
      logs
    })
    
    showModal(
      modalDialog(
        title = paste("History:", label),
        if (nrow(logs) == 0) "No changes found." else tableOutput("history_table"),
        easyClose = TRUE,
        size = "l"
      )
    )
  }
  
  obs_fields <- list(
    "h_level" = "results_level",
    "h_pillar" = "pillar",
    "h_area" = "results_area",
    "h_ind" = "indicator",
    "h_def" = "definition",
    "h_meth" = "method_of_measurement",
    "h_dis" = "disaggregation",
    "h_freq" = "frequency",
    "h_mov" = "mov"
  )
  
  lapply(names(obs_fields), function(id) {
    observeEvent(input[[id]], {
      show_hist(obs_fields[[id]], obs_fields[[id]])
    })
  })
  
  # --- GALLERY CARDS ---
  output$gallery_ui <- renderUI({
    data <- pirs_data()
    if (nrow(data) == 0) return(h3("No data available."))
    
    data$original_row <- 1:nrow(data) + 1
    
    if (input$f_level != "All") data <- data %>% filter(results_level == input$f_level)
    if (input$f_pillar != "All") data <- data %>% filter(pillar == input$f_pillar)
    if (nzchar(input$f_search)) {
      data <- data %>% filter(
        grepl(input$f_search, indicator, ignore.case = TRUE) |
          grepl(input$f_search, id, ignore.case = TRUE)
      )
    }
    
    lapply(1:nrow(data), function(i) {
      card(
        class = "card-pirs",
        card_header(
          div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            tags$b(data$id[i]),
            span(class = "badge bg-success", data$results_level[i])
          )
        ),
        div(
          style = "padding:10px;",
          h5(style = "color:#006838; font-weight:bold;", data$indicator[i]),
          
          layout_column_wrap(
            width = 1/4,
            div(span(class = "card-label", "Pillar"), div(class = "card-body-text", data$pillar[i])),
            div(span(class = "card-label", "Frequency"), div(class = "card-body-text", data$frequency[i])),
            div(span(class = "card-label", "MOV"), div(class = "card-body-text", data$mov[i]))
          ),
          
          hr(),
          
          span(class = "card-label", "Results Area"),
          div(class = "card-body-text", data$results_area[i]),
          
          span(class = "card-label", "Definition"),
          div(class = "card-body-text", data$definition[i]),
          
          layout_column_wrap(
            width = 1/2,
            div(
              span(class = "card-label", "Method of Measurement"),
              div(class = "card-body-text", data$method_of_measurement[i])
            ),
            div(
              span(class = "card-label", "Disaggregation"),
              div(class = "card-body-text", data$disaggregation[i])
            )
          )
        ),
        card_footer(
          actionButton(
            data$id[i],
            "Edit Details",
            class = "btn-sm btn-primary edit-btn",
            `data-row` = data$original_row[i]
          )
        )
      )
    })
  })
  
  # --- EDITOR UI ---
  output$edit_form_ui <- renderUI({
    if (is.null(editing_id()) && is_new_entry() == FALSE) return(NULL)
    
    if (is_new_entry()) {
      curr <- list(
        results_level = "",
        pillar = "",
        frequency = "",
        results_area = "",
        indicator = "",
        definition = "",
        method_of_measurement = "",
        disaggregation = "",
        mov = ""
      )
      header_text <- "Create New Indicator"
    } else {
      curr <- pirs_data() %>% filter(id == editing_id())
      header_text <- paste("Reference ID:", editing_id())
    }
    
    card(
      card_header(header_text),
      
      h6(class = "section-title", "1. Strategic Alignment"),
      layout_column_wrap(
        width = 1/3,
        div(
          if (!is_new_entry()) actionLink("h_level", "History", class = "hist-link"),
          selectInput(
            "u_level", "Level",
            choices = c("Select Level..." = "", "Outcome-long term", "Outcome", "Output"),
            selected = clean_val(curr$results_level)
          )
        ),
        div(
          if (!is_new_entry()) actionLink("h_pillar", "History", class = "hist-link"),
          selectInput(
            "u_pillar", "Pillar",
            choices = c("Select Pillar..." = "", "Coordination", "IPC_CM", "Laboratory", "Logistics", "RCCE", "Research", "Surveillance", "Vaccination", "WASH", "Essential service"),
            selected = clean_val(curr$pillar)
          )
        ),
        div(
          if (!is_new_entry()) actionLink("h_freq", "History", class = "hist-link"),
          selectInput(
            "u_freq", "Frequency",
            choices = c("Select Frequency..." = "", "Weekly", "Monthly", "Quarterly", "Annual"),
            selected = clean_val(curr$frequency)
          )
        )
      ),
      
      div(
        if (!is_new_entry()) actionLink("h_area", "History", class = "hist-link"),
        textAreaInput("u_area", "Results Area", clean_val(curr$results_area))
      ),
      
      h6(class = "section-title", "2. Technical Specification"),
      
      div(
        if (!is_new_entry()) actionLink("h_ind", "History", class = "hist-link"),
        textAreaInput("u_ind", "Indicator Name", clean_val(curr$indicator))
      ),
      
      div(
        if (!is_new_entry()) actionLink("h_def", "History", class = "hist-link"),
        textAreaInput("u_def", "Definition", clean_val(curr$definition))
      ),
      
      h6(class = "section-title", "3. Measurement"),
      
      layout_column_wrap(
        width = 1/2,
        div(
          if (!is_new_entry()) actionLink("h_meth", "History", class = "hist-link"),
          textAreaInput("u_meth", "Method of Measurement", clean_val(curr$method_of_measurement))
        ),
        div(
          if (!is_new_entry()) actionLink("h_mov", "History", class = "hist-link"),
          textAreaInput("u_mov", "MOV", clean_val(curr$mov))
        )
      ),
      
      div(
        if (!is_new_entry()) actionLink("h_dis", "History", class = "hist-link"),
        textAreaInput("u_dis", "Disaggregation", clean_val(curr$disaggregation))
      ),
      
      hr(),
      
      textInput("check_email", "Authorized Email:", value = ""),
      
      card_footer(
        div(
          style = "float:right",
          actionButton("save_changes", "Save Changes", class = "btn-success")
        )
      )
    )
  })
  
  # --- SAVE LOGIC WITH EMAIL VALIDATION ---
  observeEvent(input$save_changes, {
    user_email <- trimws(tolower(input$check_email))
    
    if (user_email == "") {
      showNotification("Please enter your authorized email.", type = "error")
      return()
    }
    
    user_df <- read_sheet(SHEET_ID, sheet = "users", col_types = "c")
    valid_user <- user_df %>% filter(tolower(trimws(email)) == user_email)
    
    if (nrow(valid_user) == 0) {
      showNotification("Unauthorized email. You are not permitted to save changes.", type = "error")
      return()
    }
    
    if (input$u_level == "" || input$u_pillar == "" || input$u_freq == "") {
      showNotification("Please complete all dropdown fields before saving.", type = "error")
      return()
    }
    
    if (is_new_entry()) {
      full_data <- read_sheet(SHEET_ID, sheet = "pirs_db", col_types = "c")
      row_idx <- nrow(full_data) + 2
      new_id <- if (nrow(full_data) > 0) max(as.numeric(full_data$id), na.rm = TRUE) + 1 else 1
      
      range_write(
        SHEET_ID,
        data = data.frame(v = as.character(new_id)),
        sheet = "pirs_db",
        range = paste0("A", row_idx),
        col_names = FALSE
      )
      
      current_id <- as.character(new_id)
      old_data <- NULL
    } else {
      row_idx <- as.numeric(input$row_trigger)
      current_id <- editing_id()
      old_data <- pirs_data() %>% filter(id == current_id)
    }
    
    col_mapping <- list(
      "B" = input$u_level,
      "C" = input$u_pillar,
      "D" = input$u_area,
      "E" = input$u_ind,
      "F" = input$u_def,
      "G" = input$u_meth,
      "H" = input$u_dis,
      "I" = input$u_freq,
      "J" = input$u_mov
    )
    
    tryCatch({
      withProgress(message = "Saving...", value = 0, {
        for (col_let in names(col_mapping)) {
          range_write(
            SHEET_ID,
            data = data.frame(v = as.character(col_mapping[[col_let]])),
            sheet = "pirs_db",
            range = paste0(col_let, row_idx),
            col_names = FALSE
          )
        }
      })
      
      # --- audit log ---
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      if (!is.null(old_data) && nrow(old_data) > 0) {
        audit_rows <- data.frame()
        
        field_map <- list(
          results_level = input$u_level,
          pillar = input$u_pillar,
          results_area = input$u_area,
          indicator = input$u_ind,
          definition = input$u_def,
          method_of_measurement = input$u_meth,
          disaggregation = input$u_dis,
          frequency = input$u_freq,
          mov = input$u_mov
        )
        
        for (fname in names(field_map)) {
          old_val <- if (fname %in% names(old_data)) as.character(old_data[[fname]][1]) else ""
          new_val <- as.character(field_map[[fname]])
          
          if (is.na(old_val)) old_val <- ""
          if (trimws(old_val) != trimws(new_val)) {
            audit_rows <- bind_rows(
              audit_rows,
              data.frame(
                indicator_id = current_id,
                field_name = fname,
                old_value = old_val,
                new_value = new_val,
                changed_by = valid_user$username[1],
                change_time = now
              )
            )
          }
        }
        
        if (nrow(audit_rows) > 0) {
          sheet_append(SHEET_ID, data = audit_rows, sheet = "audit_log")
        }
      }
      
      showNotification("Success!", type = "message")
      is_new_entry(FALSE)
      editing_id(NULL)
      refresh_trigger(refresh_trigger() + 1)
      nav_select("main_nav", "Gallery Registry")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$card_id_trigger, {
    is_new_entry(FALSE)
    editing_id(input$card_id_trigger)
    nav_select("main_nav", "Editor")
  })
  
  observeEvent(input$add_new, {
    is_new_entry(TRUE)
    editing_id(NULL)
    nav_select("main_nav", "Editor")
  })
}

shinyApp(ui, server)