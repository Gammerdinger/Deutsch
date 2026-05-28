library(shiny)

verbs <- read.csv("verbs.csv", header = TRUE, fileEncoding = "UTF-8")

pronoun_cols <- c(
  "present_singular_1st_person",
  "present_singular_2nd_person",
  "present_singular_3rd_person",
  "present_plural_1st_person",
  "present_plural_2nd_person",
  "present_plural_3rd_person",
  "present_formal"
)

pronoun_lookup <- list(
  present_singular_1st_person = "ich",
  present_singular_2nd_person = "du",
  present_singular_3rd_person = c("er", "sie (singular)", "es"),
  present_plural_1st_person   = "wir",
  present_plural_2nd_person   = "ihr",
  present_plural_3rd_person   = "sie (plural)",
  present_formal              = "Sie"
)

ui <- fluidPage(
  tags$script(HTML("
  $(document).on('keyup', function(e) {
    if (e.which == 13 && $('#answer').is(':focus')) {
      setTimeout(function() {
        // If the 'Check my answer' button exists, use it.
        if ($('#check').length) {
          $('#check').click();
        // Otherwise, if the 'Next question' button exists, use it.
        } else if ($('#newq').length) {
          $('#newq').click();
        }
      }, 10);
    }
  });
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
    $(field).trigger('input');
  }
  $(document).on('click', '#add_ae', function() { insertAtCursor(document.getElementById('answer'), 'ä'); });
  $(document).on('click', '#add_oe', function() { insertAtCursor(document.getElementById('answer'), 'ö'); });
  $(document).on('click', '#add_ue', function() { insertAtCursor(document.getElementById('answer'), 'ü'); });
  $(document).on('click', '#add_sz', function() { insertAtCursor(document.getElementById('answer'), 'ß'); });

  // After clicking 'Next question' (by mouse or via Enter-triggered click),
  // refocus the answer input once the new question has rendered.
  $(document).on('click', '#newq', function() {
    setTimeout(function() {
      $('#answer').focus();
    }, 50);
  });
")),
  
  fluidRow(
    # LEFT COLUMN: enlarged sidebar-like panel (width 4) + history directly underneath
    column(
      width = 4,
      wellPanel(
        selectInput(
          "filter_type", "Verb type filter:",
          choices = c(
            "Separable" = "separable",
            "Reflexive" = "reflexive",
            "Irregular" = "present_irregular",
            "S, X, or Z stem-endings" = "sxz_ending",
            "D, T, N or M (irregular) stem-endings" = "dt_nm_ending_irregular",
            "N or M (irregular) stem-endings" = "nm_ending_irregular",
            "N or M (regular) stem-endings" = "nm_ending_regular",
            "-n instead of -en infinitive endings" = "no_en_ending",
            "a → ä stem-changing" = "a_stemchanging",
            "e → i stem-changing" = "i_stemchanging",
            "e → ie stem-changing" = "ie_stemchanging",
            "Irregular (other)" = "irregular_other"
          ),
          multiple = TRUE,
          selected = NULL
        ),
        selectInput(
          "pronoun_filter", "Pronoun(s):",
          choices = c(
            "ich" = "present_singular_1st_person",
            "du" = "present_singular_2nd_person",
            "er/sie/es" = "present_singular_3rd_person",
            "wir" = "present_plural_1st_person",
            "ihr" = "present_plural_2nd_person",
            "sie (plural)" = "present_plural_3rd_person",
            "Sie (formal)" = "present_formal"
          ),
          multiple = TRUE,
          selected = NULL
        ),
        hr(),
        checkboxInput("show_score", "Show score", value = TRUE),
        checkboxInput("show_history", "Show history", value = TRUE)
      ),
      # History directly below the wellPanel
      uiOutput("history")
    ),
    
    # RIGHT COLUMN: main content (width 8)
    column(
      width = 8,
      div(style = "text-align:center;",
          uiOutput("infinitive"),
          uiOutput("prompt_row"),
          div(
            style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin: 10px 0;",
            actionButton("add_ae", "ä"),
            actionButton("add_oe", "ö"),
            actionButton("add_ue", "ü"),
            actionButton("add_sz", "ß")
          ),
          div(
            style = "display: flex; flex-direction: column; align-items: center; gap: 4px; margin-top: 10px;",
            div(
              style = "display: flex; justify-content: center; align-items: center; gap: 10px;",
              uiOutput("buttons")  # dynamic: either Check or Next
            ),
            textOutput("score")     # score display (toggle controlled)
          ),
          textOutput("feedback"),
          # Reserve vertical space for the conjugation table so layout doesn't jump
          div(
            style = "min-height: 150px; margin-top: 10px;",
            htmlOutput("conj_table")
          )
      )
    )
  )
)

server <- function(input, output, session) {
  show_table <- reactiveVal(FALSE)
  button_state <- reactiveVal("check")  # 'check' or 'next'
  
  # Score and history storage
  rv <- reactiveValues(
    correct = 0,
    total   = 0,
    history = list()   # each element: list(pronoun, answer, correct, correct_form)
  )
  
  filtered_verbs <- reactive({
    v <- verbs
    filter_types <- input$filter_type
    if (is.null(filter_types) || length(filter_types) == 0) {
      return(v)
    }
    mask <- rep(FALSE, nrow(v))
    for (ft in filter_types) {
      if (ft == "separable") {
        mask <- mask | (v$separable == TRUE)
      } else if (ft == "reflexive") {
        mask <- mask | (v$reflexive == TRUE)
      } else if (ft == "present_irregular") {
        mask <- mask | (v$present_irregular == TRUE)
      } else if (ft == "sxz_ending") {
        mask <- mask | (v$sxz_ending == TRUE)
      } else if (ft == "dt_nm_ending_irregular") {
        mask <- mask | (v$dt_ending == TRUE | v$nm_ending_irregular == TRUE)
      } else if (ft == "nm_ending_irregular") {
        mask <- mask | (v$nm_ending_irregular == TRUE)
      } else if (ft == "nm_ending_regular") {
        mask <- mask | (v$nm_ending_regular == TRUE)
      } else if (ft == "no_en_ending") {
        mask <- mask | (v$no_en_ending == TRUE)
      } else if (ft == "a_stemchanging") {
        mask <- mask | (v$a_stemchanging == TRUE)
      } else if (ft == "i_stemchanging") {
        mask <- mask | (v$i_stemchanging == TRUE)
      } else if (ft == "ie_stemchanging") {
        mask <- mask | (v$ie_stemchanging == TRUE)
      } else if (ft == "irregular_other") {
        mask <- mask | (v$irregular_other == TRUE)
      }
    }
    v[mask, ]
  })
  
  # Store colname (not index!) and always use column names
  current_indices <- reactiveVal(list(row = 1, colname = pronoun_cols[1], pronoun = NULL))
  
  new_question <- function() {
    v <- filtered_verbs()
    # Only allow pronoun columns by name, and only those that exist in v
    if (is.null(input$pronoun_filter) || length(input$pronoun_filter) == 0) {
      eligible_colnames <- pronoun_cols
    } else {
      eligible_colnames <- intersect(pronoun_cols, input$pronoun_filter)
    }
    eligible_colnames <- eligible_colnames[eligible_colnames %in% colnames(v)]
    if (nrow(v) == 0 || length(eligible_colnames) == 0) {
      current_indices(list(row = NA, colname = NA, pronoun = NULL))
      updateTextInput(session, "answer", value = "")
      output$feedback <- renderText("No verbs/pronouns match your filter.")
    } else {
      row <- sample(seq_len(nrow(v)), 1)
      colname <- sample(eligible_colnames, 1)
      pronoun_val <- pronoun_lookup[[colname]]
      if (length(pronoun_val) > 1) {
        pronoun <- sample(pronoun_val, 1)
      } else {
        pronoun <- pronoun_val
      }
      current_indices(list(row = row, colname = colname, pronoun = pronoun))
      updateTextInput(session, "answer", value = "")
      output$feedback <- renderText("")
    }
    show_table(FALSE)
    button_state("check")  # reset to 'Check my answer'
  }
  
  observe({ new_question() })
  observeEvent(input$newq, { new_question() })
  observeEvent(input$filter_type, { new_question() })
  observeEvent(input$pronoun_filter, { new_question() })
  
  verb_indices <- reactive({ current_indices() })
  
  output$infinitive <- renderUI({
    idx <- verb_indices()
    v <- filtered_verbs()
    if (is.na(idx$row)) {
      tags$h1(style = "text-align:center;", "No verb")
    } else {
      tagList(
        tags$h1(style = "text-align:center;", v$verb[idx$row]),
        tags$div(
          style = "text-align:center; color = #888; font-size: 16px; margin-bottom: 15px;",
          v$translation[idx$row]
        )
      )
    }
  })
  
  output$prompt_row <- renderUI({
    idx <- verb_indices()
    if (is.na(idx$row) || is.na(idx$colname)) {
      return(NULL)
    }
    pronoun <- idx$pronoun
    div(
      style = "display: flex; justify-content: center; align-items: center; gap: 15px;",
      tags$h4(style = "margin:0;", pronoun),
      textInput("answer", label = NULL, width = "200px", placeholder = "Type answer")
    )
  })
  
  # Dynamic buttons
  output$buttons <- renderUI({
    if (button_state() == "check") {
      actionButton("check", "Check my answer")
    } else {
      actionButton("newq", "Next question")
    }
  })
  
  # Score display (toggle controlled)
  output$score <- renderText({
    if (!isTRUE(input$show_score)) return("")  # hide when switch off
    if (rv$total == 0) {
      "Score: --"
    } else {
      pct <- round(100 * rv$correct / rv$total)
      sprintf("Score: %d/%d (%d%% correct)", rv$correct, rv$total, pct)
    }
  })
  
  observeEvent(input$check, {
    idx <- verb_indices()
    v <- filtered_verbs()
    if (is.na(idx$row) || is.na(idx$colname)) {
      output$feedback <- renderText("No verbs/pronouns match your filter.")
      show_table(FALSE)
      return()
    }
    correct_answer <- v[idx$row, idx$colname]
    user_answer <- trimws(input$answer)
    is_correct <- (tolower(user_answer) == tolower(correct_answer))
    
    if (is_correct) {
      output$feedback <- renderText("✅ Correct!")
      rv$correct <- rv$correct + 1
    } else {
      output$feedback <- renderText(paste0("❌ Incorrect. The correct answer is: ", correct_answer))
    }
    rv$total <- rv$total + 1
    
    # Add to history (newest on top)
    display_answer <- if (nzchar(user_answer)) user_answer else "(blank)"
    rv$history <- c(
      list(list(
        pronoun      = idx$pronoun,
        answer       = display_answer,
        correct      = is_correct,
        correct_form = correct_answer
      )),
      rv$history
    )
    # Optional limit:
    # if (length(rv$history) > 50) rv$history <- rv$history[1:50]
    
    show_table(TRUE)
    button_state("next")  # switch to 'Next question'
  })
  
  output$conj_table <- renderUI({
    if (!show_table()) return(NULL)
    idx <- verb_indices()
    v <- filtered_verbs()
    if (is.na(idx$row) || is.na(idx$colname)) return(NULL)
    
    singular_pronouns <- c("present_singular_1st_person", "present_singular_2nd_person", "present_singular_3rd_person")
    plural_pronouns   <- c("present_plural_1st_person", "present_plural_2nd_person", "present_plural_3rd_person")
    singular_labels <- c("ich", "du", "er/sie/es")
    plural_labels   <- c("wir", "ihr", "sie")
    
    singular_forms <- sapply(singular_pronouns, function(p) as.character(v[idx$row, p]))
    plural_forms   <- sapply(plural_pronouns,   function(p) as.character(v[idx$row, p]))
    
    # Build table cells, bolding pronoun and form only in the tested cell
    singular_cells <- vector("list", 3)
    plural_cells <- vector("list", 3)
    for (i in 1:3) {
      if (idx$colname == singular_pronouns[i]) {
        singular_cells[[i]] <- sprintf("<b>%s %s</b>", singular_labels[i], singular_forms[i])
      } else {
        singular_cells[[i]] <- sprintf("%s %s", singular_labels[i], singular_forms[i])
      }
      if (idx$colname == plural_pronouns[i]) {
        plural_cells[[i]] <- sprintf("<b>%s %s</b>", plural_labels[i], plural_forms[i])
      } else {
        plural_cells[[i]] <- sprintf("%s %s", plural_labels[i], plural_forms[i])
      }
    }
    
    HTML(sprintf('
    <table style="border-collapse:collapse; margin-left:auto; margin-right:auto;">
      <tr>
        <th style="border:1px solid #666; text-align:center; padding:4px; background-color:#e3f1fa;">Singular</th>
        <th style="border:1px solid #666; text-align:center; padding:4px; background-color:#e3f1fa;">Plural</th>
      </tr>
      <tr>
        <td style="border:1px solid #666; text-align:left; padding:4px;">%s</td>
        <td style="border:1px solid #666; text-align:left; padding:4px;">%s</td>
      </tr>
      <tr>
        <td style="border:1px solid #666; text-align:left; padding:4px;">%s</td>
        <td style="border:1px solid #666; text-align:left; padding:4px;">%s</td>
      </tr>
      <tr>
        <td style="border:1px solid #666; text-align:left; padding:4px;">%s</td>
        <td style="border:1px solid #666; text-align:left; padding:4px;">%s</td>
      </tr>
    </table>
    ',
                 singular_cells[[1]], plural_cells[[1]],
                 singular_cells[[2]], plural_cells[[2]],
                 singular_cells[[3]], plural_cells[[3]]
    ))
  })
  
  # History list under the sidebar (toggle controlled)
  output$history <- renderUI({
    if (!isTRUE(input$show_history)) return(NULL)
    if (length(rv$history) == 0) return(NULL)
    
    items <- lapply(rv$history, function(h) {
      if (h$correct) {
        # Correct: pronoun answer ✅
        tags$li(
          style = "margin-bottom: 2px;",
          HTML(sprintf(
            "%s %s <span style='color:green;'>✅</span>",
            h$pronoun, h$answer
          ))
        )
      } else {
        # Incorrect: pronoun answer ❌ → correct_form
        tags$li(
          style = "margin-bottom: 2px;",
          HTML(sprintf(
            "%s %s <span style='color:red;'>❌</span> &rarr; %s",
            h$pronoun, h$answer, h$correct_form
          ))
        )
      }
    })
    
    tagList(
      tags$h4("History"),
      tags$ul(
        style = "list-style-type:none; padding-left:0; margin-top:5px;",
        items
      )
    )
  })
}

shinyApp(ui = ui, server = server)