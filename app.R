# app_user.R
# Persists Staff ID + Name in the browser using localStorage (shinyjs)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(glue)
  library(dplyr)
  library(tibble)
  library(shinyjs)   # <-- NEW
  source("R/gs_utils.R")
})

ensure_sheets_exist()
CFG <- read_config()

ui <- page_fluid(
  theme = bs_theme(version = 5),
  useShinyjs(),  # <-- NEW: enable shinyjs
  # JS: restore saved Staff ID/Name from localStorage on connect
  tags$script(HTML("
    document.addEventListener('shiny:connected', function() {
      try {
        const sid  = localStorage.getItem('coffee_staff_id') || '';
        const name = localStorage.getItem('coffee_name') || '';
        if (sid || name) {
          Shiny.setInputValue('ls_restore', {sid: sid, name: name}, {priority: 'event'});
        }
      } catch (e) { /* ignore */ }
    });
  ")),
  titlePanel("☕ Coffee Club – Log Coffees"),
  layout_columns(
    column(6,
      card(
        card_header("Your details"),
        textInput("staff_id", "Staff ID", placeholder = "e.g. A1234"),
        textInput("name", "Name", placeholder = "First Last"),
        numericInput("coffees", "Number of coffees", value = 1, min = 1, step = 1),
        actionButton("submit", "Log Coffee(s)", class = "btn-primary"),
        div(class = "mt-2 text-muted", sprintf("Price per coffee: £%.2f", CFG$coffee_price))
      )
    ),
    column(6,
      card(
        card_header("Your balance"),
        uiOutput("balance_ui")
      ),
      card(
        card_header("Recent activity"),
        tableOutput("recent_tbl")
      )
    )
  )
)

server <- function(input, output, session) {
  # Local reactive stores (event-driven, no polling)
  recent_data  <- reactiveVal(tibble())
  balance_data <- reactiveVal(tibble())
  
  refresh_balance <- function() {
    req(nzchar(input$staff_id))
    balance_data(balance_of(trimws(input$staff_id)))
  }
  
  refresh_recent <- function() {
    req(nzchar(input$staff_id))
    recent <- read_transactions() |>
      filter(staff_id == trimws(input$staff_id)) |>
      arrange(desc(timestamp)) |>
      mutate(amount = sprintf("£%.2f", amount)) |>
      select(timestamp, type, coffees, amount, note)
    recent_data(recent)
  }
  
  # Restore saved Staff ID/Name from localStorage (sent by the JS block in UI)
  observeEvent(input$ls_restore, {
    if (isTruthy(input$ls_restore$sid)) {
      updateTextInput(session, "staff_id", value = input$ls_restore$sid)
    }
    if (isTruthy(input$ls_restore$name)) {
      updateTextInput(session, "name", value = input$ls_restore$name)
    }
    # After updating fields, refresh UI
    refresh_balance()
    refresh_recent()
  }, ignoreInit = TRUE)
  
  # Also refresh when staff_id changes manually
  observeEvent(input$staff_id, {
    if (nzchar(input$staff_id)) {
      refresh_balance()
      refresh_recent()
    }
  }, ignoreInit = FALSE)
  
  output$balance_ui <- renderUI({
    req(nzchar(input$staff_id))
    b <- balance_data()
    name <- ifelse(nrow(b) == 0 || is.na(b$name[1]), input$name, b$name[1])
    balance <- if (nrow(b) == 0) 0 else (b$balance[1] %||% 0)
    status <- if (balance < CFG$topup_threshold) {
      div(class = "alert alert-warning",
          glue("Current balance for {name} ({input$staff_id}): £{sprintf('%.2f', balance)} – please top up."))
    } else {
      div(class = "alert alert-success",
          glue("Current balance for {name} ({input$staff_id}): £{sprintf('%.2f', balance)}"))
    }
    tagList(status)
  })
  
  observeEvent(input$submit, {
    validate(
      need(nzchar(input$staff_id), "Please enter a Staff ID."),
      need(nzchar(input$name), "Please enter your name."),
      need(is.numeric(input$coffees) && input$coffees >= 1, "Coffees must be >= 1")
    )
    
    coffees <- as.integer(input$coffees)
    amount  <- -coffees * CFG$coffee_price
    
    append_transaction(
      staff_id = trimws(input$staff_id),
      name     = trimws(input$name),
      type     = "coffee",
      coffees  = coffees,
      amount   = amount,
      note     = "coffee",
      submitted_by = session$request$REMOTE_ADDR %||% "user-app"
    )
    
    # Save to localStorage for next visit (sticky identity)
    runjs(sprintf("
      try {
        localStorage.setItem('coffee_staff_id', %s);
        localStorage.setItem('coffee_name', %s);
      } catch(e) { /* ignore */ }
    ", jsonlite::toJSON(trimws(input$staff_id)), jsonlite::toJSON(trimws(input$name))))
    
    # Instant UI refresh after write
    refresh_balance()
    refresh_recent()
    
    showNotification(glue("Logged {coffees} coffee(s) = £{sprintf('%.2f', -amount)}"), type = "message")
  })
  
  output$recent_tbl <- renderTable({
    head(recent_data(), 10)
  })
}

shinyApp(ui, server)
