library(shiny)

# ---------- Simple normalization for comparison ----------
# lowercase + collapse/trim whitespace
normalize_simple <- function(x) {
  if (is.null(x)) x <- ""
  x <- tolower(x)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)  # collapse multiple spaces
  x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x, perl = TRUE)  # trim
  x
}

# ---------- Clock drawing ----------

draw_clock <- function(hours12, minutes) {
  h12 <- hours12 %% 12
  
  minute_angle <- (minutes / 60) * 2 * pi
  hour_angle   <- ((h12 + minutes / 60) / 12) * 2 * pi
  
  par(mar = c(1, 1, 1, 1))
  plot(0, 0, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", asp = 1, bty = "n")
  
  # Clock circle
  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 4)
  
  # Numbers only (1–12), moved further out (radius 0.85), no tick marks
  for (k in 0:11) {
    ang <- (k / 12) * 2 * pi
    num <- if (k == 0) 12 else k
    xn <- 0.85 * sin(ang)
    yn <- 0.85 * cos(ang)
    text(xn, yn, labels = num, cex = 1.2)
  }
  
  mx <- 0.6 * sin(minute_angle)
  my <- 0.6 * cos(minute_angle)
  segments(0, 0, mx, my, lwd = 2, col = "blue")
  
  hx <- 0.375 * sin(hour_angle)
  hy <- 0.375 * cos(hour_angle)
  segments(0, 0, hx, hy, lwd = 4, col = "black")
  
  # Center
  points(0, 0, pch = 19)
}

# ---------- German time phrases: returns *display* vector ----------
# Uses "ein Uhr" / "ein Uhr fünf" etc. but keeps "eins" in relative forms.

german_time_options_display <- function(hours12, minutes, is_pm) {
  h12 <- ((hours12 - 1) %% 12) + 1
  numwords_1_12 <- c("eins","zwei","drei","vier","fünf","sechs",
                     "sieben","acht","neun","zehn","elf","zwölf")
  hw_12h_rel <- numwords_1_12[h12]           # for relative forms (eins)
  next_hw    <- numwords_1_12[((h12) %% 12) + 1]
  
  # For absolute 'Uhr' forms, use 'ein' instead of 'eins' when it's 1 o'clock
  hw_12h_abs <- if (hw_12h_rel == "eins") "ein" else hw_12h_rel
  
  # 24h hour number (0–23)
  h24_num <- if (!is_pm) {
    if (h12 == 12) 0 else h12
  } else {
    if (h12 == 12) 12 else h12 + 12
  }
  numwords_13_23 <- c("dreizehn","vierzehn","fünfzehn","sechzehn",
                      "siebzehn","achtzehn","neunzehn","zwanzig",
                      "einundzwanzig","zweiundzwanzig","dreiundzwanzig")
  hw_24h <- NA_character_
  if (h24_num >= 13 && h24_num <= 23) {
    hw_24h <- numwords_13_23[h24_num - 12]
  }
  
  # minute words for absolute forms (written out, 5‑minute grid)
  minute_word <- switch(
    as.character(minutes),
    "0"  = "",
    "5"  = "fünf",
    "10" = "zehn",
    "15" = "fünfzehn",
    "20" = "zwanzig",
    "25" = "fünfundzwanzig",
    "30" = "dreißig",
    "35" = "fünfunddreißig",
    "40" = "vierzig",
    "45" = "fünfundvierzig",
    "50" = "fünfzig",
    "55" = "fünfundfünfzig",
    ""
  )
  
  out <- character(0)
  
  # Helper to add absolute "X Uhr Y" variants (12h + optional 24h if defined)
  add_absolute <- function(vec) {
    if (minute_word != "") {
      vec <- c(vec, paste(hw_12h_abs, "Uhr", minute_word))
      if (!is.na(hw_24h)) {
        vec <- c(vec, paste(hw_24h, "Uhr", minute_word))
      }
    } else {
      vec <- c(vec, paste(hw_12h_abs, "Uhr"))
      if (!is.na(hw_24h)) {
        vec <- c(vec, paste(hw_24h, "Uhr"))
      }
    }
    vec
  }
  
  if (minutes == 0) {
    # No relative "eins Uhr" here; only absolute "ein Uhr" via add_absolute()
    
  } else if (minutes == 5) {
    out <- c(out, paste("fünf nach", hw_12h_rel))
    
  } else if (minutes == 10) {
    out <- c(out, paste("zehn nach", hw_12h_rel))
    
  } else if (minutes == 15) {
    out <- c(out, paste("Viertel nach", hw_12h_rel))
    
  } else if (minutes == 20) {
    out <- c(out,
             paste("zwanzig nach", hw_12h_rel),
             paste("zehn vor halb", next_hw))
    
  } else if (minutes == 25) {
    out <- c(out,
             paste("fünfundzwanzig nach", hw_12h_rel),
             paste("fünf vor halb", next_hw))
    
  } else if (minutes == 30) {
    out <- c(out, paste("halb", next_hw))
    
  } else if (minutes == 35) {
    out <- c(out, paste("fünf nach halb", next_hw))
    
  } else if (minutes == 40) {
    out <- c(out, paste("zehn nach halb", next_hw))
    
  } else if (minutes == 45) {
    out <- c(out, paste("Viertel vor", next_hw))
    
  } else if (minutes == 50) {
    out <- c(out, paste("zehn vor", next_hw))
    
  } else if (minutes == 55) {
    out <- c(out, paste("fünf vor", next_hw))
  }
  
  # Add the absolute 12h and (if applicable) 24h forms
  out <- add_absolute(out)
  
  unique(out)
}

# ---------- UI ----------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .umlaut-buttons {
        text-align: center;
      }
      .umlaut-buttons .btn {
        display: inline-block;
        margin-right: 4px;
        margin-bottom: 4px;
      }
      .center-buttons {
        text-align: center;
      }
      .center-buttons .btn {
        display: inline-block;
        margin: 4px;
      }
    ")),
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();

          // Flush current text box value to Shiny immediately (bypass debounce)
          var el = document.getElementById('answer');
          if (el && typeof Shiny !== 'undefined') {
            Shiny.setInputValue('answer', el.value, {priority: 'event'});
          }

          if ($('#check').is(':visible')) {
            $('#check').click();
          } else if ($('#next_q').is(':visible')) {
            $('#next_q').click();
          }
        }
      });

      Shiny.addCustomMessageHandler('focusAnswer', function(message) {
        var el = document.getElementById('answer');
        if (el) {
          el.focus();
        }
      });
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("answer", label = "Wie spät ist es?", value = ""),
      div(
        class = "umlaut-buttons text-center",
        actionButton("btn_ae", "ä"),
        actionButton("btn_oe", "ö"),
        actionButton("btn_ue", "ü"),
        actionButton("btn_sz", "ß")
      ),
      br(),
      div(
        class = "center-buttons",
        uiOutput("buttonsUI")
      ),
      br(), br(),
      htmlOutput("feedback")
    ),
    mainPanel(
      plotOutput("clockPlot", height = "400px"),
      div(
        style = "text-align: center; margin-top: 10px;",
        textOutput("ampmText")
      )
    )
  )
)

# ---------- SERVER ----------

server <- function(input, output, session) {
  
  current <- reactiveValues(hours12 = 2, minutes = 0, is_pm = FALSE)
  state   <- reactiveValues(mode    = "check")  # "check" or "next"
  
  # Time generator: 75% quarter-hours, 25% other multiples of 5
  new_random_time <- function() {
    quarter_minutes <- c(0, 15, 30, 45)
    all5 <- seq(0, 55, by = 5)
    other5 <- setdiff(all5, quarter_minutes)
    
    if (runif(1) < 0.75) {
      m <- sample(quarter_minutes, 1)
    } else {
      m <- sample(other5, 1)
    }
    h <- sample(1:12, 1)
    is_pm <- sample(c(FALSE, TRUE), 1)
    list(h = h, m = m, pm = is_pm)
  }
  
  # Initialize
  observe({
    nt <- new_random_time()
    current$hours12 <- nt$h
    current$minutes <- nt$m
    current$is_pm   <- nt$pm
  })
  
  # Append ä/ö/ü/ß and refocus
  append_char <- function(char) {
    cur <- input$answer
    if (is.null(cur)) cur <- ""
    updateTextInput(session, "answer", value = paste0(cur, char))
    session$sendCustomMessage("focusAnswer", list())
  }
  observeEvent(input$btn_ae, { append_char("ä") })
  observeEvent(input$btn_oe, { append_char("ö") })
  observeEvent(input$btn_ue, { append_char("ü") })
  observeEvent(input$btn_sz, { append_char("ß") })
  
  # Dynamic action buttons
  output$buttonsUI <- renderUI({
    if (state$mode == "check") {
      actionButton("check", "Check my answer")
    } else {
      actionButton("next_q", "Next question")
    }
  })
  
  # Clock + AM/PM
  output$clockPlot <- renderPlot({
    draw_clock(current$hours12, current$minutes)
  })
  output$ampmText <- renderText({
    if (current$is_pm) "PM" else "AM"
  })
  
  # Valid answers
  valid_answers_display <- reactive({
    ans <- german_time_options_display(current$hours12, current$minutes, current$is_pm)
    if (length(ans) == 0) ans <- "No text solution defined."
    ans
  })
  
  bullet_list_html <- function(vec) {
    items <- paste(sprintf("<li><em>%s</em></li>", vec), collapse = "\n")
    paste0("<ul>", items, "</ul>")
  }
  
  # Check my answer
  observeEvent(input$check, {
    req(state$mode == "check")
    
    # Normalize user input
    user_norm <- normalize_simple(input$answer)
    
    # Display valid answers and their normalized forms
    va_disp <- valid_answers_display()
    va_norm <- normalize_simple(va_disp)
    
    is_correct <- user_norm %in% va_norm
    
    if (is_correct) {
      matches <- which(user_norm == va_norm)
      correct_disp <- unique(va_disp[matches])
      others_disp  <- setdiff(va_disp, correct_disp)
      if (length(others_disp) > 0) {
        msg <- paste0(
          "✅ Correct!<br><br>Other acceptable answers are:",
          bullet_list_html(others_disp)
        )
      } else {
        msg <- "✅ Correct!"
      }
    } else {
      msg <- paste0(
        "❌ Incorrect. The correct answer is:",
        bullet_list_html(va_disp)
      )
    }
    
    output$feedback <- renderUI(HTML(msg))
    state$mode <- "next"
  })
  
  # Next question
  observeEvent(input$next_q, {
    req(state$mode == "next")
    
    nt <- new_random_time()
    current$hours12 <- nt$h
    current$minutes <- nt$m
    current$is_pm   <- nt$pm
    updateTextInput(session, "answer", value = "")
    output$feedback <- renderUI(HTML(""))
    
    state$mode <- "check"
  })
}

shinyApp(ui, server)