library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(readxl)
library(openxlsx) 

# --- 1. DATABASE SETUP ---
db_name <- "results_framework_v1.sqlite"
con <- dbConnect(SQLite(), db_name)
cdc_colors <- list(green = "#006838", yellow = "#FDB913", bg = "#f8f9fa")

dbExecute(con, "CREATE TABLE IF NOT EXISTS pirs_db (
  id TEXT PRIMARY KEY,
  results_level TEXT,
  pillar TEXT,
  results_area TEXT,
  indicator TEXT,
  definition TEXT,
  method_of_measurement TEXT,
  dissagregation TEXT,
  frequency TEXT,
  mov TEXT,
  last_updated TEXT
)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS audit_log (
  indicator_id TEXT, field_name TEXT, old_value TEXT, new_value TEXT, changed_by TEXT, change_time TEXT
)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS users (username TEXT, role TEXT, email TEXT PRIMARY KEY)")
dbExecute(con, "INSERT OR IGNORE INTO users VALUES ('Admin', 'Admin', 'admin@moh.gov')")

# --- 2. UI ---
ui <- page_navbar(
  title = "Africa CDC-WHO IMST | Results Framework Matrix",
  id = "main_nav",
  position = "fixed-top", 
  theme = bs_theme(bootswatch = "flatly", primary = cdc_colors$green, secondary = cdc_colors$yellow),
  
  header = tagList(
    tags$style(HTML("
      body { padding-top: 100px !important; background-color: #f8f9fa; }
      .card-pirs { border-left: 10px solid #006838; margin-bottom: 25px; font-size: 0.85rem; }
      .section-title { color: #006838; font-weight: bold; border-bottom: 1px solid #eee; margin-bottom: 15px; margin-top: 20px; }
      .card-label { font-weight: bold; color: #555; text-transform: uppercase; font-size: 0.7rem; display: block; margin-top: 5px; }
      textarea { resize: both !important; min-height: 80px; width: 100%; }
      .hist-link { font-size: 0.72rem; float: right; color: #006838; text-decoration: underline; cursor: pointer; font-weight: normal; }
    ")),
    tags$script(HTML("$(document).on('click', '.edit-btn', function() { Shiny.setInputValue('card_id_trigger', this.id, {priority: 'event'}); });"))
  ),
  
  nav_panel("Gallery Registry", 
            div(style = "max-width: 1500px; margin: auto; padding: 15px;",
                layout_column_wrap(width = 1/4,
                                   selectInput("f_level", "Filter by Level:", choices = c("All","Outcome-long term", "Outcome", "Output")),
                                   selectInput("f_pillar", "Filter by Pillar:", choices = c("All","Coordination", "IPC_CM", "Laboratory", "Logistics", "RCCE","Research", "Surveillance","Vaccination","WASH", "Essential service")),
                                   textInput("f_search", "Search ID/Indicator:"),
                                   div(style="padding-top: 32px;", actionButton("add_new_ind", "Add New Indicator", class = "btn-success w-100", icon = icon("plus")))
                ),
                uiOutput("gallery_ui")
            )
  ),
  nav_panel("Editor", div(style = "max-width: 1000px; margin: auto; padding-bottom: 50px;", 
                          uiOutput("edit_form_ui"),
                          conditionalPanel("!input.card_id_trigger && !input.add_new_ind", 
                                           h4("No indicator selected. Please select one from the Gallery Registry.", style="color:#999; text-align:center; margin-top:50px;"))
  )),
  nav_spacer(),
  nav_item(actionButton("btn_admin_login", "Staff Management", class="btn-outline-secondary"))
)

# --- 3. SERVER ---
server <- function(input, output, session) {
  editing_id <- reactiveVal(NULL)
  refresh_val <- reactiveVal(0)
  is_admin <- reactiveVal(FALSE)
  
  safe_diff <- function(a, b) {
    a_clean <- trimws(as.character(ifelse(is.na(a), "", a)))
    b_clean <- trimws(as.character(ifelse(is.na(b), "", b)))
    return(a_clean != b_clean)
  }
  
  show_hist <- function(label, db_field) {
    logs <- dbGetQuery(con, "SELECT old_value, new_value, changed_by, change_time FROM audit_log WHERE indicator_id = ? AND field_name = ? ORDER BY change_time DESC", list(editing_id(), db_field))
    showModal(modalDialog(title = paste("History:", label), if(nrow(logs)==0) "No history recorded." else renderTable(logs), easyClose = TRUE, size = "l"))
  }
  
  obs_fields <- list(
    "h_level"="results_level", "h_pillar"="pillar", "h_area"="results_area", 
    "h_ind"="indicator", "h_def"="definition", "h_meth"="method_of_measurement", 
    "h_dis"="dissagregation", "h_freq"="frequency", "h_mov"="mov"
  )
  lapply(names(obs_fields), function(id) { observeEvent(input[[id]], { show_hist(obs_fields[[id]], obs_fields[[id]]) }) })
  
  observeEvent(input$add_new_ind, {
    new_id <- paste0("IND-", format(Sys.time(), "%H%M%S"))
    # Insert truly blank values for new records
    dbExecute(con, "INSERT INTO pirs_db (id, indicator, results_level, pillar, frequency) VALUES (?, ?, ?, ?, ?)", 
              list(new_id, "[New Indicator - Please Edit]", "", "", ""))
    editing_id(new_id)
    refresh_val(refresh_val() + 1)
    nav_select("main_nav", "Editor")
    showNotification(paste("Created temporary ID:", new_id), type = "message")
  })
  
  output$gallery_ui <- renderUI({
    refresh_val()
    data <- dbGetQuery(con, "SELECT * FROM pirs_db")
    if(nrow(data) == 0) return(h3("No data available."))
    
    if(input$f_level != "All") data <- data %>% filter(results_level == input$f_level)
    if(input$f_pillar != "All") data <- data %>% filter(pillar == input$f_pillar)
    if(nzchar(input$f_search)) data <- data %>% filter(grepl(input$f_search, indicator, ignore.case=T) | grepl(input$f_search, id, ignore.case=T))
    
    lapply(1:nrow(data), function(i) {
      card(class = "card-pirs",
           card_header(div(style="display:flex; justify-content:space-between; align-items:center;", 
                           tags$b(data$id[i]), span(class="badge bg-success", data$results_level[i]))),
           div(style="padding:10px;",
               h5(style="color:#006838; font-weight:bold;", data$indicator[i]),
               layout_column_wrap(width = 1/4,
                                  div(span(class="card-label", "Pillar"), data$pillar[i]),
                                  div(span(class="card-label", "Frequency"), data$frequency[i]),
                                  div(span(class="card-label", "MOV"), data$mov[i])
               ),
               hr(),
               span(class="card-label", "Results Area"), p(data$results_area[i]),
               span(class="card-label", "Definition"), p(data$definition[i]),
               layout_column_wrap(width = 1/2,
                                  div(span(class="card-label", "Method of Measurement"), data$method_of_measurement[i]),
                                  div(span(class="card-label", "Disaggregation"), data$dissagregation[i])
               )
           ),
           card_footer(actionButton(data$id[i], "Edit Details", class="btn-sm btn-primary edit-btn"))
      )
    })
  })
  
  output$edit_form_ui <- renderUI({
    req(editing_id())
    curr <- dbGetQuery(con, "SELECT * FROM pirs_db WHERE id = ?", list(editing_id()))
    if(nrow(curr) == 0) return(NULL)
    
    # Handle the selected state dynamically: if DB is empty, use ""
    sel_level <- if(is.na(curr$results_level) || curr$results_level == "") "" else curr$results_level
    sel_pillar <- if(is.na(curr$pillar) || curr$pillar == "") "" else curr$pillar
    sel_freq <- if(is.na(curr$frequency) || curr$frequency == "") "" else curr$frequency
    
    card(
      card_header(paste("Reference Sheet:", editing_id())),
      h6(class="section-title", "1. Strategic Alignment"),
      layout_column_wrap(width = 1/3, 
                         div(actionLink("h_level", "History", class="hist-link"), 
                             selectInput("u_level", "Level", 
                                         choices = c("Select Level..." = "", "Outcome-long term", "Outcome", "Output"), 
                                         selected = sel_level)), 
                         div(actionLink("h_pillar", "History", class="hist-link"), 
                             selectInput("u_pillar", "Pillar", 
                                         choices = c("Select Pillar..." = "", "Coordination", "IPC_CM", "Laboratory", "Logistics", "RCCE","Research", "Surveillance","Vaccination","WASH", "Essential service"), 
                                         selected = sel_pillar)),
                         div(actionLink("h_freq", "History", class="hist-link"), 
                             selectInput("u_freq", "Frequency", 
                                         choices = c("Select Frequency..." = "", "Weekly", "Monthly", "Quarterly", "Annual"), 
                                         selected = sel_freq))),
      div(actionLink("h_area", "History", class="hist-link"), textAreaInput("u_area", "Results Area", curr$results_area)),
      
      h6(class="section-title", "2. Technical Specification"),
      div(actionLink("h_ind", "History", class="hist-link"), textAreaInput("u_ind", "Indicator", curr$indicator)),
      div(actionLink("h_def", "History", class="hist-link"), textAreaInput("u_def", "Definition", curr$definition)),
      
      h6(class="section-title", "3. Measurement"),
      layout_column_wrap(width = 1/2, 
                         div(actionLink("h_meth", "History", class="hist-link"), textAreaInput("u_meth", "Method of Measurement", curr$method_of_measurement)), 
                         div(actionLink("h_mov", "History", class="hist-link"), textAreaInput("u_mov", "MOV", curr$mov))),
      div(actionLink("h_dis", "History", class="hist-link"), textAreaInput("u_dis", "Disaggregation", curr$dissagregation)),
      
      hr(),
      textInput("check_email", "Authorized Email:", value = "enter your credential"),
      card_footer(div(style="float:right", actionButton("save_changes", "Save & Audit", class="btn-success")))
    )
  })
  
  observeEvent(input$save_changes, {
    user <- dbGetQuery(con, "SELECT username FROM users WHERE LOWER(email) = LOWER(?)", list(trimws(input$check_email)))
    if(nrow(user) == 0) { showNotification("Unauthorized credentials", type = "error"); return() }
    
    old <- dbGetQuery(con, "SELECT * FROM pirs_db WHERE id = ?", list(editing_id()))
    now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    fields <- list(
      "results_level"="u_level", "pillar"="u_pillar", "results_area"="u_area",
      "indicator"="u_ind", "definition"="u_def", "method_of_measurement"="u_meth",
      "dissagregation"="u_dis", "frequency"="u_freq", "mov"="u_mov"
    )
    
    changes_found <- FALSE
    dbBegin(con)
    tryCatch({
      for(f in names(fields)) {
        if(safe_diff(input[[fields[[f]]]], old[[f]])) {
          changes_found <- TRUE
          dbExecute(con, "INSERT INTO audit_log VALUES (?, ?, ?, ?, ?, ?)", 
                    list(editing_id(), f, as.character(old[[f]]), as.character(input[[fields[[f]]]]), user$username[1], now))
        }
      }
      
      if(changes_found) {
        dbExecute(con, "UPDATE pirs_db SET results_level=?, pillar=?, results_area=?, indicator=?, definition=?, method_of_measurement=?, dissagregation=?, frequency=?, mov=?, last_updated=? WHERE id=?",
                  list(input$u_level, input$u_pillar, input$u_area, input$u_ind, input$u_def, input$u_meth, input$u_dis, input$u_freq, input$u_mov, now, editing_id()))
        dbCommit(con)
        showNotification("Success! Information updated.", type = "message")
      } else {
        dbRollback(con)
        showNotification("No changes detected.", type = "warning")
      }
      
      editing_id(NULL) 
      refresh_val(refresh_val() + 1)
      nav_select("main_nav", "Gallery Registry")
      
    }, error = function(e) { 
      dbRollback(con)
      showNotification(paste("Save Error:", e$message), type="error") 
    })
  })
  
  observeEvent(input$btn_admin_login, { 
    showModal(modalDialog(passwordInput("ap", "Enter Admin Code"), footer=actionButton("do_al", "Login"))) 
  })
  
  observeEvent(input$do_al, { 
    admin_secret <- Sys.getenv("PIRS_ADMIN_CODE")
    if(input$ap == admin_secret) { is_admin(TRUE); removeModal(); showNotification("Admin Access Granted") } 
    else { showNotification("Incorrect Code", type = "error") }
  })
  
  observe({ 
    if(is_admin()) nav_insert("main_nav", target="Editor", nav_panel("Admin", 
                                                                     card(card_header("Database Management"),
                                                                          layout_column_wrap(width = 1/2,
                                                                                             fileInput("m_file", "1. Upload Excel for Reset", accept = ".xlsx"),
                                                                                             downloadButton("download_backup", "2. Download Current Backup", class = "btn-info")),
                                                                          actionButton("do_reset", "Execute Hard Reset (Deletes Old Data)", class="btn-danger", width="100%")),
                                                                     card(card_header("Users"), textInput("nu_n", "Name"), textInput("nu_e", "Email"), actionButton("su", "Add User"), hr(), DTOutput("st")))) 
  })
  
  output$download_backup <- downloadHandler(
    filename = function() { paste0("Strategic_Framework_Backup_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx") },
    content = function(file) {
      export_data <- dbGetQuery(con, "SELECT * FROM pirs_db")
      cols <- c("id", "results_level", "pillar", "results_area", "indicator", "definition", "method_of_measurement", "dissagregation", "frequency", "mov")
      write.xlsx(export_data[, cols], file, rowNames = FALSE)
    }
  )
  
  observeEvent(input$do_reset, {
    req(input$m_file)
    df <- read_excel(input$m_file$datapath)
    df[] <- lapply(df, as.character)
    expected_cols <- c("id", "results_level", "pillar", "results_area", "indicator", "definition", "method_of_measurement", "dissagregation", "frequency", "mov")
    
    missing <- setdiff(expected_cols, colnames(df))
    if(length(missing) > 0) {
      showNotification(paste("Missing columns:", paste(missing, collapse=", ")), type="error")
      return()
    }
    
    dbBegin(con)
    tryCatch({
      dbExecute(con, "DELETE FROM pirs_db")
      dbWriteTable(con, "pirs_db", df[, expected_cols], append = TRUE)
      dbCommit(con); refresh_val(refresh_val()+1); showNotification("Reset Done")
    }, error = function(e) { dbRollback(con); showNotification("Reset Error", type="error") })
  })
  
  output$st <- renderDT({ refresh_val(); datatable(dbGetQuery(con, "SELECT username, email FROM users")) })
  observeEvent(input$su, { dbExecute(con, "INSERT INTO users VALUES (?, 'User', ?)", list(input$nu_n, input$nu_e)); refresh_val(refresh_val()+1) })
  observeEvent(input$card_id_trigger, { editing_id(input$card_id_trigger); nav_select("main_nav", "Editor") })
}

shinyApp(ui, server)