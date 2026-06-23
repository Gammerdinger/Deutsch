library(shiny)

# ---------- Simple normalization for comparison ----------
normalize_simple <- function(x) {
  if (is.null(x)) x <- ""
  x <- tolower(x)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x, perl = TRUE)
  x
}

# ---------- Simple HTML escape (no htmltools) ----------
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub("\"","&quot;",x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
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
  
  symbols(0, 0, circles = 1, inches = FALSE, add = TRUE, lwd = 4)
  
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
  
  points(0, 0, pch = 19)
}

# ---------- ALL valid German time phrases (absolute + special) ----------
german_time_options_display <- function(hours12, minutes, is_pm) {
  h12 <- ((hours12 - 1) %% 12) + 1
  numwords_1_12 <- c("eins","zwei","drei","vier","fünf","sechs",
                     "sieben","acht","neun","zehn","elf","zwölf")
  hw_12h_rel <- numwords_1_12[h12]
  next_hw    <- numwords_1_12[((h12) %% 12) + 1]
  
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
  
  # special/relative forms (12h only) + Mitternacht/Mittag
  if (minutes == 0) {
    if (h24_num == 0) {
      out <- c(out, "Mitternacht", "die Mitternacht")
    } else if (h24_num == 12) {
      out <- c(out, "Mittag", "der Mittag")
    }
    
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
  
  out <- add_absolute(out)
  
  unique(out)
}

# ---------- Preferred absolute forms ONLY (hour + Uhr + minutes) ----------
german_time_options_absolute_display <- function(hours12, minutes, is_pm) {
  h12 <- ((hours12 - 1) %% 12) + 1
  numwords_1_12 <- c("eins","zwei","drei","vier","fünf","sechs",
                     "sieben","acht","neun","zehn","elf","zwölf")
  hw_12h_rel <- numwords_1_12[h12]
  hw_12h_abs <- if (hw_12h_rel == "eins") "ein" else hw_12h_rel
  
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
  if (minute_word != "") {
    out <- c(out, paste(hw_12h_abs, "Uhr", minute_word))
    if (!is.na(hw_24h)) {
      out <- c(out, paste(hw_24h, "Uhr", minute_word))
    }
  } else {
    out <- c(out, paste(hw_12h_abs, "Uhr"))
    if (!is.na(hw_24h)) {
      out <- c(out, paste(hw_24h, "Uhr"))
    }
  }
  unique(out)
}

# ---------- Bullet list with TTS buttons ----------
bullet_list_html <- function(vec) {
  items <- vapply(
    vec,
    function(txt) {
      esc <- html_escape(txt)
      sprintf(
        '<li><em>%s</em> <span class="tts-de" data-text="%s" title="Play text-to-speech">🔊</span></li>',
        esc, esc
      )
    },
    character(1)
  )
  paste0("<ul>", paste(items, collapse = "\n"), "</ul>")
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
      .tts-de {
        cursor: pointer;
        user-select: none;
      }
      .app-container {
        max-width: 600px;
        margin: 0 auto;
      }
      #feedback-container ul {
        display: inline-block;
        text-align: left;
        margin: 0 auto;
        padding-left: 1.5em;
      }
    ")),
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        if (e.key === 'Enter') {
          e.preventDefault();
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
        if (el) el.focus();
      });

      function speakGerman(text) {
        if (!('speechSynthesis' in window)) {
          console.warn('tts-helper: speechSynthesis not supported in this browser');
          return;
        }
        window.speechSynthesis.cancel();
        var utter = new SpeechSynthesisUtterance(text);
        utter.lang = 'de-DE';
        utter.rate = 1.0;
        utter.pitch = 1.0;
        window.speechSynthesis.speak(utter);
      }

      document.addEventListener('click', function(e) {
        var target = e.target;
        if (target && target.classList && target.classList.contains('tts-de')) {
          e.preventDefault();
          var text = target.getAttribute('data-text');
          if (!text) {
            console.warn('tts-helper: .tts-de element has no data-text', target);
            return;
          }
          speakGerman(text);
        }
      });
    "))
  ),
  
  div(
    class = "app-container",
    
    plotOutput("clockPlot", height = "260px"),
    
    div(
      style = "text-align: center; margin-top: 5px; margin-bottom: 15px;",
      textOutput("ampmText")
    ),
    
    div(
      class = "answer-container",
      style = "display: flex; justify-content: center; margin-top: 10px;",
      div(
        style = "width: 320px; text-align: center;",
        textInput("answer", label = "Wie spät ist es?", value = "")
      )
    ),
    
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
    
    br(),
    
    div(
      id = "feedback-container",
      style = "text-align: center;",
      htmlOutput("feedback")
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  current <- reactiveValues(hours12 = 2, minutes = 0, is_pm = FALSE)
  state   <- reactiveValues(mode    = "check")
  
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
  
  observe({
    nt <- new_random_time()
    current$hours12 <- nt$h
    current$minutes <- nt$m
    current$is_pm   <- nt$pm
  })
  
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
  
  output$buttonsUI <- renderUI({
    if (state$mode == "check") {
      actionButton("check", "Check my answer")
    } else {
      actionButton("next_q", "Next question")
    }
  })
  
  output$clockPlot <- renderPlot({
    draw_clock(current$hours12, current$minutes)
  })
  output$ampmText <- renderText({
    if (current$is_pm) "PM" else "AM"
  })
  
  valid_answers_absolute_display <- reactive({
    ans <- german_time_options_absolute_display(
      current$hours12, current$minutes, current$is_pm
    )
    if (length(ans) == 0) ans <- "No text solution defined."
    ans
  })
  
  valid_answers_all_display <- reactive({
    german_time_options_display(
      current$hours12, current$minutes, current$is_pm
    )
  })
  
  observeEvent(input$check, {
    req(state$mode == "check")
    
    user_raw  <- if (is.null(input$answer)) "" else input$answer
    user_norm <- normalize_simple(user_raw)
    
    # Empty input -> ask for an answer, don't advance
    if (user_norm == "") {
      output$feedback <- renderUI(HTML("Please provide an answer."))
      return()
    }
    
    va_abs_disp <- valid_answers_absolute_display()
    va_abs_norm <- normalize_simple(va_abs_disp)
    
    va_all_disp <- valid_answers_all_display()
    va_all_norm <- normalize_simple(va_all_disp)
    
    is_preferred_correct <- user_norm %in% va_abs_norm
    is_any_correct       <- user_norm %in% va_all_norm
    
    # Mitternacht/Mittag strings
    mid_strings <- c("Mitternacht","die Mitternacht","Mittag","der Mittag")
    is_mid_special <- user_norm %in% normalize_simple(mid_strings)
    
    # For listings: preferred + any midnight/noon nouns
    mid_present <- intersect(va_all_disp, mid_strings)
    va_list_disp <- unique(c(va_abs_disp, mid_present))
    
    if (is_preferred_correct || (is_any_correct && is_mid_special)) {
      # Preferred Uhr form OR midnight/noon nouns => Correct!
      if (is_preferred_correct) {
        matches <- which(user_norm == va_abs_norm)
        correct_disp <- unique(va_abs_disp[matches])
        main_correct <- correct_disp[1]
      } else {
        main_correct <- user_raw
      }
      
      main_esc <- html_escape(main_correct)
      main_correct_html <- sprintf(
        '<em>%s</em> <span class="tts-de" data-text="%s" title="Play text-to-speech">🔊</span>',
        main_esc, main_esc
      )
      
      others_disp <- setdiff(va_list_disp, main_correct)
      if (length(others_disp) > 0) {
        msg <- paste0(
          "✅ Correct!<br>",
          "One correct answer is: ", main_correct_html, "<br>",
          "Other acceptable answers are:<br>",
          bullet_list_html(others_disp)
        )
      } else {
        msg <- paste0(
          "✅ Correct!<br>",
          "The correct answer is: ", main_correct_html
        )
      }
      
    } else if (is_any_correct) {
      # Other special/colloquial forms -> Accepted
      user_esc <- html_escape(user_raw)
      user_html <- sprintf(
        '<em>%s</em> <span class="tts-de" data-text="%s" title="Play text-to-speech">🔊</span>',
        user_esc, user_esc
      )
      preferred_list_html <- bullet_list_html(va_list_disp)
      
      msg <- paste0(
        "✅ Accepted: ", user_html, "<br>",
        "But for this exercise the preferred answer(s) are:<br>",
        preferred_list_html
      )
      
    } else {
      # Incorrect
      preferred_list_html <- bullet_list_html(va_list_disp)
      
      msg <- paste0(
        "❌ Incorrect. The correct answer(s):<br>",
        preferred_list_html
      )
    }
    
    output$feedback <- renderUI(HTML(msg))
    state$mode <- "next"
  })
  
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