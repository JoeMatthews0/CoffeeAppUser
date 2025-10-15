# app_user.R — solid "remember me" via server<->client handshake

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

coffee_price <- 0.2

ui <- fluidPage(  # using classic layout (works everywhere)
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  
  # Client helpers
  tags$head(tags$script(HTML("
    // c_trim removes whitespace from inputs, avoids issues of trailing spaces after names etc causing duplicate profiles
    function c_trim(s){ return (s||'').replace(/^\\s+|\\s+$/g,''); }
    // c_set stores variables in localStorage -- needed for the name autofill feature
    function c_set(key,val){
      try{
        // Trim the val input of any whitespace
        val = c_trim(val);
        // If val exists, i.e. if there's an input to be stored, store it in localStorage
        if(val){ localStorage.setItem(key, val); }
        // If the input is blank
        else { localStorage.removeItem(key); }
      }catch(e){}
    }
    // c_get outputs the value stored in localStorage, e.g. the most recently entered name of staff_id
    // If there is nothing with that key available in localStorage, it returns ''
    function c_get(key){
      try{ return c_trim(localStorage.getItem(key)||''); }catch(e){ return ''; }
    }
    // c_fillInput populates fields (e.g. name or staff_id) with their values from LocalStorage when used alongside c_get
    function c_fillInput(id, val){
      var el = document.getElementById(id);
      if(!el) return;
      el.value = val || '';
      el.dispatchEvent(new Event('input', {bubbles:true}));
      el.dispatchEvent(new Event('change', {bubbles:true}));
    }

    // Server asks for identity -> send it back via input$ls_restore
    Shiny.addCustomMessageHandler('requestIdentity', function(x){
      // Extract staff_id from localStorage (if exists) and save as sid
      var sid  = c_get('coffee_staff_id');
      // Same for name
      var name = c_get('coffee_name');
      // also pre-fill DOM immediately just for UX
      // Populate the name/id input fields with their values from localStorage
      if(sid)  c_fillInput('staff_id', sid);
      if(name) c_fillInput('name', name);
      if (window.Shiny) {
        Shiny.setInputValue('ls_restore', { sid: sid, name: name }, {priority:'event'});
      }
    });

    // Server can persist explicitly too (e.g., after submit)
    // Store name and id in localStorage for future pulls
    Shiny.addCustomMessageHandler('persistIdentity', function(x){
      if (x.sid  !== undefined) c_set('coffee_staff_id', x.sid);
      if (x.name !== undefined) c_set('coffee_name',    x.name);
    });
  "))),
  
  titlePanel("MSP Coffee Club"),
  p("If you encounter any issues using this app, please contact Joe Matthews"),
  fluidRow(
    column(6,
           card(
             card_header("Your details"),
             textInput("staff_id", "Staff ID", placeholder = "e.g. A1234"),
             textInput("name",    "Name",     placeholder = "First Last"),
             numericInput("coffees", "Number of coffees", value = 1, min = 1, step = 1),
             actionButton("submit", "Log Coffee(s)", class = "btn-primary"),
             actionButton("balance", "Check Balance", class = "btn-primary"),
             actionButton("activity", "See Recent Activity", class = "btn-primary"),
             div(class = "mt-2 text-muted", sprintf("Price per coffee: £%.2f", coffee_price))
           )
    ),
    column(6,
           conditionalPanel('input.balance % 2 == 1',
                            card(card_header("Your balance"), uiOutput("balance_ui"))
                            ),
           conditionalPanel('input.activity % 2 == 1',
                            card(card_header("Recent activity"), 
                                 selectInput(inputId = "activityType", 
                                             label = "Which activity would you like to see?", 
                                             choices = c("All" = "all", 
                                                         "Coffees" = "cof", 
                                                         "Top-ups" = "top")), 
                                 tableOutput("recent_tbl"),
                                 p("How many transactions to display?"),
                                 selectInput(inputId = "recentLength",
                                             label = NULL,
                                             choices = c("5" = "5",
                                                         "10" = "10",
                                                         "20" = "20",
                                                         "All" = "all")
                                 )
                            )
                            )
    )
  )
)

server <- function(input, output, session) {
  recent_data  <- reactiveVal(tibble())
  balance_data <- reactiveVal(tibble())
  
  refresh_balance <- function() {
    req(nzchar(input$staff_id) & nzchar(input$name))
    balance_data(balance_of(trimws(input$staff_id), trimws(input$name)))
  }
  refresh_recent <- function() {
    req(nzchar(input$staff_id))
    recent_data(
      read_transactions() |>
        dplyr::filter(staff_id == trimws(input$staff_id)) |>
        dplyr::arrange(dplyr::desc(timestamp)) |>
        dplyr::mutate(
          timestamp = format(timestamp, "%d-%m-%y %H:%M"),   # <- pretty string
          amount    = sprintf("£%.2f", amount)
        ) |>
        dplyr::select(timestamp, type, coffees, amount, submitted_by)
    )
  }
  
  # 🔑 Ask the browser for stored values AFTER UI is bound
  session$onFlushed(function() {
    session$sendCustomMessage("requestIdentity", list())
  }, once = TRUE)
  
  # When client returns stored values, set via updateTextInput (authoritative)
  observeEvent(input$ls_restore, {
    sid  <- input$ls_restore$sid  %||% ""
    name <- input$ls_restore$name %||% ""
    if (nzchar(sid))  updateTextInput(session, "staff_id", value = sid)
    if (nzchar(name)) updateTextInput(session, "name",     value = name)
    if (nzchar(sid)) { refresh_balance(); refresh_recent() }
  }, ignoreInit = TRUE)
  
  # Persist on every edit (trim; never store blanks)
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
    if(!nzchar(input$staff_id) || !nzchar(input$name)){
      div(class="alert alert-warning",
          glue("Please enter a staff ID and name to check balance. "))
    } else{
      b <- balance_data()
      name <- ifelse(nrow(b) == 0 || is.na(b$name[1]), input$name, b$name[1])
      balance <- if (nrow(b) == 0) 0 else (b$balance[1] %||% 0)
      #if(nrow(b) == 0){
      #  div(class="alert alert-warning",
      #      glue("Current balance for {name} ({input$staff_id}): £{sprintf('%.2f', balance)} – please top up."))
      #}
      if (balance < 0) {
        div(class="alert alert-warning",
            glue("Current balance for {name} ({input$staff_id}): £{sprintf('%.2f', balance)} – please top up."))
      } else {
        div(class="alert alert-success",
            glue("Current balance for {name} ({input$staff_id}): £{sprintf('%.2f', balance)}"))
      }
    }
  })
  
  observeEvent(input$submit, {
    validate(
      need(nzchar(input$staff_id), "Please enter a Staff ID."),
      need(nzchar(input$name), "Please enter your name."),
      need(is.numeric(input$coffees) && input$coffees >= 1, "Coffees must be >= 1")
    )
    coffees <- as.integer(input$coffees)
    amount  <- -coffees * coffee_price
    
    append_transaction(
      staff_id = trimws(input$staff_id),
      name     = trimws(input$name),
      type     = "coffee",
      coffees  = coffees,
      amount   = amount,
      submitted_by = trimws(input$name)
    )
    
    # Persist identity redundantly (server -> client)
    session$sendCustomMessage("persistIdentity",
                              list(sid = trimws(input$staff_id), name = trimws(input$name))
    )
    
    refresh_balance(); refresh_recent()
    showNotification(glue("Logged {coffees} coffee(s) = £{sprintf('%.2f', -amount)}"), type = "message")
  })
  
  filtered_data <- reactive({
    switch(input$activityType,
           "all" = recent_data(),
           "cof" = recent_data() |> filter(type == "coffee"),
           "top" = recent_data() |> filter(type == "topup")
    )
  })
  
  recentRange <- reactive({
    ifelse(input$recentLength == "all", 1e6, as.numeric(input$recentLength))
  })
  
  output$recent_tbl <- renderTable(
      head(filtered_data(), recentRange())
    )
}

shinyApp(ui, server)
