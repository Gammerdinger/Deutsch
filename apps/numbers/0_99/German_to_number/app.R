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
  
  # Tens words (20,30,...,90)
  tens_vals  <- seq(20, 90, by = 10)
  tens_words <- c("zwanzig", "dreißig", "vierzig", "fünfzig",
                  "sechzig", "siebzig", "achtzig", "neunzig")
  word[tens_vals + 1] <- tens_words
  
  # Ones forms used in compounds (ein-, zwei-, drei-, …)
  ones_comp <- c("ein", "zwei", "drei", "vier",
                 "fünf", "sechs", "sieben", "acht", "neun")
  
  # Fill in 21–99 that are not pure tens
  for (n in 20:99) {
    if (n %% 10 == 0) next  # already have 20,30,...
    if (n >= 10 && n <= 19) next  # teens already set
    
    tens_part <- floor(n / 10) * 10
    ones_part <- n %% 10
    tens_word <- word[tens_part + 1]
    
    # ones_part is 1–9 here
    comp_ones <- ones_comp[ones_part]
    
    word[n + 1] <- paste0(comp_ones, "und", tens_word)
  }
  
  data.frame(
    value = vals,
    word  = word,
    audio = paste0(word, ".ogg"),
    stringsAsFactors = FALSE
  )
}

german_numbers <- build_german_numbers_0_99()

ui <- fluidPage(
  tags$head(
    # Center text inside the input box
    tags$style(HTML("
      #user_answer input {
        text-align: center;
      }
    ")),
    # Global audio player
    tags$audio(id = "global-audio-player"),
    # Audio helper + Enter key on keyup
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

    player.src = src;
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
  
  # Center all content; compact for embedding
  div(
    style = "max-width: 600px; margin: 0 auto; text-align: center; padding-top: 10px;",
    
    # German word + speaker
    uiOutput("german_word_ui"),
    
    br(),
    
    # Center the whole input (label + box)
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
    
    br(),
    
    actionButton("check_next", "Check my answer"),
    
    br(),
    
    textOutput("feedback")
  )
)

server <- function(input, output, session) {
  
  # Current number (integer 0–99)
  current_number <- reactiveVal(sample(0:99, 1))
  
  # Mode: "check" or "next"
  mode <- reactiveVal("check")
  
  # Feedback text
  feedback_text <- reactiveVal("")
  
  # Render the German word and its pronunciation button
  output$german_word_ui <- renderUI({
    num <- current_number()
    row <- german_numbers[german_numbers$value == num, ]
    
    # Choose audio subdirectory based on the decade
    audio_subdir <- if (num <= 9) {
      "0_9"
    } else {
      decade_start <- floor(num / 10) * 10
      decade_end   <- decade_start + 9
      sprintf("%d_%d", decade_start, decade_end)  # e.g. 20_29, 30_39, ...
    }
    
    # Raw path under www/audio/numbers/...
    audio_path_raw <- sprintf("audio/numbers/%s/%s", audio_subdir, row$audio)
    
    # URL-encode so umlauts etc. become safe in the URL
    audio_path <- URLencode(audio_path_raw, reserved = TRUE)
    
    div(
      style = "font-size: 2.5rem; margin-top: 0;",
      span(row$word),
      tags$span(
        class = "pron",
        `data-src` = audio_path,
        style = "margin-left: 0.5em; font-size: 1.5rem; vertical-align: middle; cursor: pointer;",
        "🔊"
      )
    )
  })
  
  # Feedback output
  output$feedback <- renderText({
    feedback_text()
  })
  
  observeEvent(input$check_next, {
    
    if (mode() == "check") {
      # Check the user's answer
      ans_raw <- trimws(input$user_answer)
      
      if (ans_raw == "") {
        feedback_text("Please enter a number using digits (e.g., 0, 1, 2, ..., 99).")
        return()
      }
      
      # Digits only
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
        feedback_text(
          sprintf("❌ Incorrect. The correct answer is: %d", correct)
        )
      }
      
      # Switch to next-question mode
      mode("next")
      updateActionButton(session, "check_next", label = "Next question")
      
    } else {
      # Next question
      current_number(sample(0:99, 1))
      updateTextInput(session, "user_answer", value = "")
      feedback_text("")
      
      mode("check")
      updateActionButton(session, "check_next", label = "Check my answer")
    }
  })
}

shinyApp(ui, server)