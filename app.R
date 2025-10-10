# app_user.R — remembers Staff ID + Name across visits using localStorage

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(glue)
  library(dplyr)
  library(tibble)
  library(shinyjs)   # for runjs
  source("R/gs_utils.R")
})

ensure_sheets_exist()
CFG <- read_config()

ui <- page_fluid(
  theme = bs_theme(version = 5),
  useShinyjs(),
  
  # JS: restore saved Staff ID/Name on connect by populating inputs directly,
  # then dispatching 'input' events so Shiny updates the reactive values.
  tags$script(HTML("
    function coffeePersist(key, val) {
      try { localStorage.setItem(key, val || ''); } catch(e) {}
    }
    function coffeeLoad(key) {
      try { return localStorage.getItem(key) || ''; } catch(e) { return ''; }
    }
    function setInputValue(id, val){
      var el = document.getElementById(id);
      if(!el) return;
      el.value = val || '';
      // Fire the input event so Shiny sees the change immediately
      el.dispatchEvent(new Event('input', {bubbles:true}));
      el.dispatchEvent(new Event('change', {bubbles:true}));
    }
    document.addEventListener('shiny:connected', function() {
      var sid  = coffeeLoad('coffee_staff_id');
      var name = coffeeLoad('coffee_name');
      if (sid)  setInputValue('staff_id', sid);
      if (name) setInputValue('name', name);
    });
    // Allow server to persist via a custom message too
    Shiny.addCustomMessageHandler('persistIdentity', function(x){
      if (x.sid !== undefined)  coffeePersist('coffee_staff_id', x.sid);
      if (x.name !== undefined) coffeePersist('coffee_name', x.name);
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
      card(card_header("Your balance"), uiOutput("balance_ui")),
      card(card_header("Recent activity"), tableOutput("recent_tbl"))
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
  
  # Persist on every change (so closing the tab still remembers)
  observeEvent(input$staff_id, {
    if (nzchar(input$staff_id)) {
      runjs(sprintf("coffeePersist('coffee_staff_id', %s);",
                    jsonlite::toJSON(trimws(input$staff_id), auto_unbox = TRUE)))
      # also refresh balance/history when the ID changes
      refresh_balance(); refresh_recent()
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$name, {
    runjs(sprintf("coffeePersist('coffee_name', %s);",
                  jsonlite::toJSON(trimws(input$name), auto_unbox = TRUE)))
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
    
    # Persist identity (redundant with on-change, but safe)
    session$sendCustomMessage("persistIdentity",
                              list(sid = trimws(input$staff_id), name = trimws(input$name)))
    
    # Instant UI refresh after write
    refresh_balance()
    refresh_recent()
    
    showNotification(glue("Logged {coffees} coffee(s) = £{sprintf('%.2f', -amount)}"),
                     type = "message")
  })
  
  output$recent_tbl <- renderTable({
    head(recent_data(), 10)
  })
}

shinyApp(ui, server)
