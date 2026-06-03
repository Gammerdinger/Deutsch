library(shiny)

# German numbers 0–12 and corresponding audio file names
german_numbers <- data.frame(
  value = 0:12,
  audio = c(
    "null.ogg", "eins.ogg", "zwei.ogg", "drei.ogg", "vier.ogg",
    "fünf.ogg", "sechs.ogg", "sieben.ogg", "acht.ogg", "neun.ogg",
    "zehn.ogg", "elf.ogg", "zwölf.ogg"
  ),
  stringsAsFactors = FALSE
)

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
  
  # Compact, centered layout
  div(
    style = "max-width: 600px; margin: 0 auto; text-align: center; padding-top: 5px; padding-bottom: 5px;",
    
    # Just the speaker icon (no German text)
    uiOutput("audio_button_ui"),
    
    tags$div(style = "height: 10px;"),
    
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
    
    tags$div(style = "height: 10px;"),
    
    actionButton("check_next", "Check my answer"),
    
    tags$div(style = "height: 8px;"),
    
    textOutput("feedback")
  )
)

server <- function(input, output, session) {
  
  # Current number (integer 0–12)
  current_number <- reactiveVal(sample(0:12, 1))
  
  # Mode: "check" or "next"
  mode <- reactiveVal("check")
  
  # Feedback text
  feedback_text <- reactiveVal("")
  
  # Render the speaker icon only, wired to the current audio file
  output$audio_button_ui <- renderUI({
    num <- current_number()
    row <- german_numbers[german_numbers$value == num, ]
    
    audio_subdir <- if (num <= 9) "0_9" else "10_19"
    # Files live in www/audio/numbers/...
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
        feedback_text("Please enter a number using digits (e.g., 0, 1, 2, ...).")
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
      current_number(sample(0:12, 1))
      updateTextInput(session, "user_answer", value = "")
      feedback_text("")
      mode("check")
      updateActionButton(session, "check_next", label = "Check my answer")
    }
  })
}

shinyApp(ui, server)