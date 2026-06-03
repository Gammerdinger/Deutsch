library(shiny)

# Build German numbers 0–99 with their words and audio filenames
build_german_numbers_0_99 <- function() {
  vals <- 0:99
  word <- character(length(vals))
  
  # 0–9
  ones <- c("null", "eins", "zwei", "drei", "vier",
            "fünf", "sechs", "sieben", "acht", "neun")
  word[0:9 + 1] <- ones
  
  # 10–19
  teens <- c(
    "zehn", "elf", "zwölf", "dreizehn", "vierzehn",
    "fünfzehn", "sechzehn", "siebzehn", "achtzehn", "neunzehn"
  )
  word[10:19 + 1] <- teens
  
  # Tens (20, 30, …, 90)
  tens_vals  <- seq(20, 90, by = 10)
  tens_words <- c("zwanzig", "dreißig", "vierzig", "fünfzig",
                  "sechzig", "siebzig", "achtzig", "neunzig")
  word[tens_vals + 1] <- tens_words
  
  # Ones forms used in compounds (ein-, zwei-, drei-, …)
  ones_comp <- c("ein", "zwei", "drei", "vier",
                 "fünf", "sechs", "sieben", "acht", "neun")
  
  # 21–99, excluding pure tens and teens
  for (n in 20:99) {
    if (n %% 10 == 0) next        # 20,30,... already set
    if (n >= 10 && n <= 19) next  # teens already set
    
    tens_part <- floor(n / 10) * 10
    ones_part <- n %% 10
    tens_word <- word[tens_part + 1]
    comp_ones <- ones_comp[ones_part]
    
    word[n + 1] <- paste0(comp_ones, "und", tens_word)
  }
  
  audio <- paste0(word, ".ogg")
  
  data.frame(
    value = vals,
    audio = audio,
    stringsAsFactors = FALSE
  )
}

german_numbers <- build_german_numbers_0_99()

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
        padding: 0;
      }
      #user_answer input {
        text-align: center;
      }
    ")),
    tags$audio(id = "global-audio-player"),
    tags$script(HTML(
      "
document.addEventListener('DOMContentLoaded', function () {
  const player = document.getElementById('global-audio-player');
  if (!player) return;

  // Click on pronunciation icon
  document.body.addEventListener('click', function (e) {
    const el = e.target.closest('.pron');
    if (!el) return;

    e.preventDefault();
    const src = el.getAttribute('data-src');
    if (!src) return;

    // URL-encode umlauts etc. in the path
    player.src = encodeURI(src);
    player.load();
    player.play().catch(() => {});
  });

  // Press Enter when focused in the input
  document.addEventListener('keyup', function(e) {
    if (e.key === 'Enter') {
      const active = document.activeElement;
      if (active && active.id === 'user_answer') {
        e.preventDefault();
        const btn = document.getElementById('check_next');
        if (btn) btn.click();
      }
    }
  });
});
"
    ))
  ),
  
  div(
    style = "max-width: 600px; margin: 0 auto; text-align: center; padding-top: 5px; padding-bottom: 5px;",
    
    uiOutput("audio_button_ui"),
    
    tags$div(style = "height: 10px;"),
    
    div(
      style = "display: flex; justify-content: center;",
      div(
        style = "width: 220px; text-align: center;",
        textInput(
          "user_answer",
          label = "Enter the number (digits only)",
          value = "",
          width = "100%"
        )
      )
    ),
    
    tags$div(style = "height: 10px;"),
    
    actionButton("check_next", "Check my answer"),
    
    tags$div(style = "height: 8px;"),
    
    textOutput("feedback")
  )
)

server <- function(input, output, session) {
  
  current_number <- reactiveVal(sample(0:99, 1))
  mode <- reactiveVal("check")
  feedback_text <- reactiveVal("")
  
  output$audio_button_ui <- renderUI({
    num <- current_number()
    row <- german_numbers[german_numbers$value == num, ]
    
    audio_subdir <- if (num <= 9) {
      "0_9"
    } else {
      decade_start <- floor(num / 10) * 10
      decade_end   <- decade_start + 9
      sprintf("%d_%d", decade_start, decade_end)
    }
    
    audio_path <- sprintf("audio/numbers/%s/%s", audio_subdir, row$audio)
    
    div(
      style = "margin-top: 0;",
      tags$span(
        class = "pron",
        `data-src` = audio_path,
        style = "font-size: 3rem; cursor: pointer;",
        "🔊"
      )
    )
  })
  
  output$feedback <- renderText(feedback_text())
  
  observeEvent(input$check_next, {
    if (mode() == "check") {
      ans_raw <- trimws(input$user_answer)
      
      if (ans_raw == "") {
        feedback_text("Please enter a number using digits (e.g., 0, 1, 2, ..., 99).")
        return()
      }
      
      if (!grepl("^[0-9]+$", ans_raw)) {
        feedback_text(
          "Please enter the answer as a numeral using digits only, not as a word."
        )
        return()
      }
      
      ans <- as.integer(ans_raw)
      correct <- current_number()
      
      if (!is.na(ans) && ans == correct) {
        feedback_text("✅ Correct!")
      } else {
        feedback_text(sprintf("❌ Incorrect. The correct answer is: %d", correct))
      }
      
      mode("next")
      updateActionButton(session, "check_next", label = "Next question")
      
    } else {
      current_number(sample(0:99, 1))
      updateTextInput(session, "user_answer", value = "")
      feedback_text("")
      mode("check")
      updateActionButton(session, "check_next", label = "Check my answer")
    }
  })
}

shinyApp(ui, server)