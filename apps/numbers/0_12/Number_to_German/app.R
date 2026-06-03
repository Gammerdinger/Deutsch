library(shiny)

# Numbers 0–12 with their German words and audio
numbers_df <- data.frame(
  value = 0:12,
  word  = c(
    "null", "eins", "zwei", "drei", "vier", "fünf",
    "sechs", "sieben", "acht", "neun", "zehn", "elf", "zwölf"
  ),
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
      .umlaut-buttons .btn {
        margin: 0 2px;
        padding: 2px 8px;
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
  
  div(
    style = "max-width: 600px; margin: 0 auto; text-align: center; padding-top: 5px; padding-bottom: 5px;",
    
    # Numeral prompt
    uiOutput("number_prompt_ui"),
    
    tags$div(style = "height: 10px;"),
    
    # Center the input (label + box)
    div(
      style = "display: flex; justify-content: center;",
      div(
        style = "width: 260px; text-align: center;",
        textInput(
          "user_answer",
          label = "Enter the number in German",
          value = "",
          width = "100%"
        )
      )
    ),
    
    # Umlaut / ß helper buttons
    div(
      class = "umlaut-buttons",
      style = "margin-top: 4px;",
      actionButton("add_ae", "ä"),
      actionButton("add_oe", "ö"),
      actionButton("add_ue", "ü"),
      actionButton("add_sz", "ß")
    ),
    
    tags$div(style = "height: 10px;"),
    
    actionButton("check_next", "Check my answer"),
    
    tags$div(style = "height: 8px;"),
    
    uiOutput("feedback_ui")
  )
)

server <- function(input, output, session) {
  
  # Current number (integer 0–12)
  current_number <- reactiveVal(sample(0:12, 1))
  
  # Mode: "check" or "next"
  mode <- reactiveVal("check")
  
  # Raw feedback HTML
  feedback_html <- reactiveVal("")
  
  # Show the numeral prompt
  output$number_prompt_ui <- renderUI({
    num <- current_number()
    
    div(
      style = "font-size: 2.4rem; margin-top: 0;",
      span(num)
    )
  })
  
  # Feedback (with possible pronunciation icon)
  output$feedback_ui <- renderUI({
    HTML(feedback_html())
  })
  
  # Helper: normalize German input (trim, lower, remove all spaces)
  normalize_input <- function(x) {
    x <- trimws(x)
    x <- tolower(x)
    x <- gsub("\\s+", "", x)
    x
  }
  
  # Helper: append a character to the current input
  append_char <- function(ch) {
    cur <- input$user_answer
    if (is.null(cur)) cur <- ""
    updateTextInput(session, "user_answer", value = paste0(cur, ch))
  }
  
  observeEvent(input$add_ae, { append_char("ä") })
  observeEvent(input$add_oe, { append_char("ö") })
  observeEvent(input$add_ue, { append_char("ü") })
  observeEvent(input$add_sz, { append_char("ß") })
  
  observeEvent(input$check_next, {
    
    if (mode() == "check") {
      # CHECK ANSWER
      ans_raw <- input$user_answer
      if (is.null(ans_raw)) ans_raw <- ""
      
      # If empty
      if (trimws(ans_raw) == "") {
        feedback_html("Please enter the number in German.")
        return()
      }
      
      # If they typed digits, direct them to give the German word
      if (grepl("^[0-9]+$", trimws(ans_raw))) {
        feedback_html("Please enter the number in German, not as digits.")
        return()
      }
      
      ans_norm <- normalize_input(ans_raw)
      
      num <- current_number()
      row <- numbers_df[numbers_df$value == num, ]
      
      correct_word <- row$word
      correct_norm <- normalize_input(correct_word)
      
      # Audio path for this number
      audio_subdir <- if (num <= 9) "0_9" else "10_19"
      audio_path <- sprintf("audio/numbers/%s/%s", audio_subdir, row$audio)
      
      pron_html <- sprintf(
        ' <span class="pron" data-src="%s" style="margin-left: 0.3em; font-size: 1.3rem; cursor: pointer;">🔊</span>',
        audio_path
      )
      
      if (ans_norm == correct_norm) {
        feedback_html(
          paste0(
            "✅ Correct! <sb",
            htmltools::htmlEscape(correct_word),
            "</b>",
            pron_html
          )
        )
      } else {
        feedback_html(
          paste0(
            "❌ Incorrect. The correct answer is: <b>",
            htmltools::htmlEscape(correct_word),
            "</b>",
            pron_html
          )
        )
      }
      
      mode("next")
      updateActionButton(session, "check_next", label = "Next question")
      
    } else {
      # NEXT QUESTION
      current_number(sample(0:12, 1))
      updateTextInput(session, "user_answer", value = "")
      feedback_html("")
      
      mode("check")
      updateActionButton(session, "check_next", label = "Check my answer")
    }
  })
}

shinyApp(ui, server)