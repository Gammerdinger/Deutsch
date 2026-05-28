library(shiny)

verbs <- read.csv("data/verbs.csv", header = TRUE, fileEncoding = "UTF-8")

pronoun_lookup <- list(
  present_singular_1st_person = "ich",
  present_singular_2nd_person = "du",
  present_singular_3rd_person = c("er", "sie (singular)", "es"),
  present_plural_1st_person   = "wir",
  present_plural_2nd_person   = "ihr",
  present_plural_3rd_person   = "sie (plural)",
  present_formal              = "Sie"
)

# Define UI for application
ui <- fluidPage(
  tags$script(HTML("
  // Enter/Return triggers check
  $(document).on('keyup', function(e) {
    if(e.which == 13 && $('#answer').is(':focus')) {
      setTimeout(function() {
        $('#check').click();
      }, 10);
    }
  });
  // Insert character at cursor, always works even if inputs are re-rendered
  function insertAtCursor(field, value) {
    if (!field) return;
    if (document.selection) {
      field.focus();
      var sel = document.selection.createRange();
      sel.text = value;
    } else if (field.selectionStart || field.selectionStart === 0) {
      var startPos = field.selectionStart;
      var endPos = field.selectionEnd;
      field.value = field.value.substring(0, startPos)
        + value
        + field.value.substring(endPos, field.value.length);
      field.selectionStart = field.selectionEnd = startPos + value.length;
    } else {
      field.value += value;
    }
    field.focus();
    // Notify Shiny to update the input value
    $(field).trigger('input');
  }
  $(document).on('click', '#add_ae', function() { insertAtCursor(document.getElementById('answer'), 'ä'); });
  $(document).on('click', '#add_oe', function() { insertAtCursor(document.getElementById('answer'), 'ö'); });
  $(document).on('click', '#add_ue', function() { insertAtCursor(document.getElementById('answer'), 'ü'); });
  $(document).on('click', '#add_sz', function() { insertAtCursor(document.getElementById('answer'), 'ß'); });
")),
  div(style = "text-align:center;",
      uiOutput("infinitive"),
      uiOutput("prompt_row"),
      # Umlaut buttons here:
      div(
        style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin: 10px 0;",
        actionButton("add_ae", "ä"),
        actionButton("add_oe", "ö"),
        actionButton("add_ue", "ü"),
        actionButton("add_sz", "ß")
      ),
      div(
        style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin-top: 10px;",
        actionButton("check", "Check my answer"),
        actionButton("newq", "Next question")
      ),
      textOutput("feedback")
  )
)

server <- function(input, output, session) {
  # Persistent storage of indices
  current_indices <- reactiveVal({
    list(row = sample(x = seq_len(nrow(verbs)), size = 1),
         col = sample(x = seq(from = 15, to = ncol(verbs)), size = 1),
         pronoun = NULL)
  })
  
  # Helper to update indices and reset UI
  new_question <- function() {
    current_indices(list(
      row = sample(x = seq_len(nrow(verbs)), size = 1),
      col = sample(x = seq(from = 15, to = ncol(verbs)), size = 1)
    ))
    updateTextInput(session, "answer", value = "")
    output$feedback <- renderText("")
  }
  
  # Show a new question when the app starts
  observe({ new_question() })
  # And whenever "New question" is pressed
  observeEvent(input$newq, { new_question() })
  
  # For convenience, use current_indices in place of your verb_indices
  verb_indices <- reactive({ current_indices() })
  
  output$infinitive <- renderUI({
    idx <- verb_indices()
    tags$h1(style = "text-align:center;", verbs$verb[idx$row])
  })
  
  output$prompt_row <- renderUI({
    idx <- verb_indices()
    colname <- colnames(verbs)[idx$col]
    pronoun_val <- pronoun_lookup[[colname]]
    if (length(pronoun_val) > 1) {
      pronoun <- sample(pronoun_val, 1)
    } else {
      pronoun <- pronoun_val
    }
    # Use a flexbox div for centering both elements together
    div(
      style = "display: flex; justify-content: center; align-items: center; gap: 15px;",
      tags$h4(style = "margin:0;", pronoun),
      textInput("answer", label = NULL, width = "200px", placeholder = "Type answer")
    )
  })
  
  observeEvent(input$check, {
    idx <- verb_indices()
    correct_answer <- verbs[idx$row, idx$col]
    user_answer <- trimws(input$answer)
    if (tolower(user_answer) == tolower(correct_answer)) {
      output$feedback <- renderText("✅ Correct!")
    } else {
      output$feedback <- renderText(paste0("❌ Incorrect. The correct answer is: ", correct_answer))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
