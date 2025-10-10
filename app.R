# app_user.R — robust "remember me" with localStorage + server-side update

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(glue)
  library(dplyr)
  library(tibble)
  library(shinyjs)
  source("R/gs_utils.R")
})

ensure_sheets_exist()
CFG <- read_config()

ui <- page_fluid(
  theme = bs_theme(version = 5),
  useShinyjs(),
  
  # Robust persist/restore script
  tags$head(tags$script(HTML("
    function c_trim(s){ return (s||'').replace(/^\\s+|\\s+$/g,''); }
    function c_set(key,val){
      try{
        val = c_trim(val);
        if(val){ localStorage.setItem(key, val); }
        else { localStorage.removeItem(key); } // don't keep blanks/spaces
      }catch(e){}
    }
    function c_get(key){
      try{ return c_trim(localStorage.getItem(key)||''); }catch(e){ return ''; }
    }
    function c_fillInput(id, val){
      var el = document.getElementById(id);
      if(!el) return;
      el.value = val || '';
      el.dispatchEvent(new Event('input', {bubbles:true}));
      el.dispatchEvent(new Event('change', {bubbles:true}));
    }

    document.addEventListener('shiny:connected', function(){
      var sid  = c_get('coffee_staff_id');
      var name = c_get('coffee_name');

      // 1) Directly populate the DOM inputs (client-side)
      if(sid)  c_fillInput('staff_id', sid);
      if(name) c_fillInput('name', name);

      // 2) Also tell the server to update (covers any timing quirks)
      if (window.Shiny) {
        Shiny.setInputValue('ls_restore', { sid: sid, name: name }, {priority:'event'});
      }
    });

    // Let server persist too (used on submit)
    Shiny.addCustomMessageHandler('persistIdentity', function(x){
      if (x.sid !== undefined)  c_set('coffee_staff_id', x.sid);
      if (x.name !== undefined) c_set('coffee_name', x.name);
    });
  "))),
  
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
  
  # Restore from localStorage via server-side update as well
  observeEvent(input$ls_restore, {
    sid  <- input$ls_restore$sid  %||% ""
    name <- input$ls_restore$name %||% ""
    if (nzchar(sid))  updateTextInput(session, "staff_id", value = sid)
    if (nzchar(name)) updateTextInput(session, "name",     value = name)
    if (nzchar(sid)) { refresh_balance(); refresh_recent() }
  }, ignoreInit = TRUE)
  
  # Save to localStorage when fields change (only non-empty, trimmed)
  observeEvent(input$staff_id, {
    sid <- trimws(input$staff_id)
    if (nzchar(sid)) {
      runjs(sprintf("c_set('coffee_staff_id', %s);", jsonlite::toJSON(sid, auto_unbox=TRUE)))
      refresh_balance(); refresh_recent()
    }
  }, ignoreInit = FALSE)
  
  observeEvent(input$name, {
    nm <- trimws(input$name)
    if (nzchar(nm)) {
      runjs(sprintf("c_set('coffee_name', %s);", jsonlite::toJSON(nm, auto_unbox=TRUE)))
    }
  }, ignoreInit = FALSE)
  
  output$balance_ui <- renderUI({
    req(nzchar(input$staff_id))
    b <- balance_data()
    name <- ifelse(nrow(b) == 0 || is.na(b$name[1]), input$name, b$name[1])
    balance <- if (nrow(b) == 0) 0 else (b$balance[1] %||% 0)
    if (balance < CFG$topup_threshold) {
      div(class="alert alert-warning",
          glue("Current balance for {name} ({input$staff_id}): £{sprintf('%.2f', balance)} – please top up."))
    } else {
      div(class="alert alert-success",
          glue("Current balance for {name} ({input$staff_id}): £{sprintf('%.2f', balance)}"))
    }
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
    
    # Persist identity redundantly (server -> client) in case fields were empty before submit
    session$sendCustomMessage("persistIdentity",
                              list(sid = trimws(input$staff_id), name = trimws(input$name)))
    
    refresh_balance(); refresh_recent()
    showNotification(glue("Logged {coffees} coffee(s) = £{sprintf('%.2f', -amount)}"), type = "message")
  })
  
  output$recent_tbl <- renderTable(head(recent_data(), 10))
}

shinyApp(ui, server)
