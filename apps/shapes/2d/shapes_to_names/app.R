library(shiny)

blank_plot <- function(xlim = c(-2, 2), ylim = c(-2, 2)) {
  plot(0, 0, type = "n", asp = 1,
       xlim = xlim, ylim = ylim,
       xlab = "", ylab = "", axes = FALSE)
}

triangle <- function() {
  x <- c(0, 2, 1)
  y <- c(0, 0, sqrt(3))
  blank_plot(c(-1, 3), c(-1, 3))
  polygon(x, y)
}

square <- function() {
  x <- c(0, 2, 2, 0)
  y <- c(0, 0, 2, 2)
  blank_plot(c(-1, 3), c(-1, 3))
  polygon(x, y)
}

rectangle <- function() {
  x <- c(-1.5,  1.5,  1.5, -1.5)
  y <- c(-1.0, -1.0,  1.0,  1.0)
  blank_plot(c(-2, 2), c(-2, 2))
  polygon(x, y)
}

rhombus <- function() {
  x <- c(0,  1,  0, -1)
  y <- c(sqrt(3), 0, -sqrt(3), 0)
  blank_plot(c(-2, 2), c(-2, 2))
  polygon(x, y)
}

parallelogram <- function() {
  x <- c(-1,  1,  1.5, -0.5)
  y <- c( 0,  0,  1,   1)
  blank_plot(c(-2, 2), c(-1, 2))
  polygon(x, y)
}

trapezoid <- function() {
  x <- c(-1.2, 1.2,  0.8, -0.8)
  y <- c( 0,   0,    1,   1)
  blank_plot(c(-2, 2), c(-1, 2))
  polygon(x, y)
}

circle <- function() {
  center_x <- 0
  center_y <- 0
  r <- 1
  theta <- seq(0, 2*pi, length.out = 200)
  x <- center_x + r * cos(theta)
  y <- center_y + r * sin(theta)
  blank_plot(c(-1.5, 1.5), c(-1.5, 1.5))
  polygon(x, y)
}

semicircle <- function() {
  center_x <- 0
  center_y <- 0
  r <- 1
  theta <- seq(0, pi, length.out = 200)
  x <- center_x + r * cos(theta)
  y <- center_y + r * sin(theta)
  blank_plot(c(-1.5, 1.5), c(-0.2, 1.5))
  polygon(x, y)
}

oval <- function() {
  center_x <- 0
  center_y <- 0
  a <- 2
  b <- 1
  theta <- seq(0, 2*pi, length.out = 300)
  x <- center_x + a * cos(theta)
  y <- center_y + b * sin(theta)
  blank_plot(c(-2.5, 2.5), c(-1.5, 1.5))
  polygon(x, y)
}

# -------- helper for regular polygons --------
regular_polygon <- function(n, r = 1,
                            xlim = c(-1.5, 1.5),
                            ylim = c(-1.5, 1.5),
                            start_angle = pi/2) {
  theta <- seq(start_angle, start_angle + 2*pi, length.out = n + 1)[-(n + 1)]
  x <- r * cos(theta)
  y <- r * sin(theta)
  blank_plot(xlim, ylim)
  polygon(x, y)
}

pentagon <- function() {
  regular_polygon(5, r = 1, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
}

hexagon <- function() {
  regular_polygon(6, r = 1, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
}

octagon <- function() {
  regular_polygon(8, r = 1, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
}

decagon <- function() {
  regular_polygon(10, r = 1, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
}

shapes <- list(
  triangle      = triangle,
  square        = square,
  rectangle     = rectangle,
  rhombus       = rhombus,
  parallelogram = parallelogram,
  trapezoid     = trapezoid,
  circle        = circle,
  semicircle    = semicircle,
  oval          = oval,
  pentagon      = pentagon,
  hexagon       = hexagon,
  octagon       = octagon,
  decagon       = decagon
)

shapes_info_2d <- list(
  triangle = list(
    english = "triangle",
    forms = data.frame(
      noun     = c("Dreieck"),
      article  = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  
  pentagon = list(
    english = "pentagon",
    forms = data.frame(
      noun     = c("Fünfeck"),
      article  = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  
  hexagon = list(
    english = "hexagon",
    forms = data.frame(
      noun     = c("Sechseck"),
      article  = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  
  octagon = list(
    english = "octagon",
    forms = data.frame(
      noun     = c("Achteck"),
      article  = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  
  decagon = list(
    english = "decagon",
    forms = data.frame(
      noun     = c("Zehneck"),
      article  = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  
  square = list(
    english = "square",
    forms = data.frame(
      noun      = c("Quadrat",    "Viereck",   "Rechteck",     "Parallelogramm"),
      article   = c("das",        "das",       "das",          "das"),
      preferred = c(TRUE,         FALSE,       FALSE,          FALSE),
      stringsAsFactors = FALSE
    )
  ),
  
  rectangle = list(
    english = "rectangle",
    forms = data.frame(
      noun      = c("Rechteck",   "Viereck",   "Parallelogramm"),
      article   = c("das",        "das",       "das"),
      preferred = c(TRUE,         FALSE,       FALSE),
      stringsAsFactors = FALSE
    )
  ),
  
  parallelogram = list(
    english = "parallelogram",
    forms = data.frame(
      noun      = c("Parallelogramm", "Viereck"),
      article   = c("das",            "das"),
      preferred = c(TRUE,             FALSE),
      stringsAsFactors = FALSE
    )
  ),
  
  rhombus = list(
    english = "rhombus",
    forms = data.frame(
      noun      = c("Raute",      "Viereck",   "Parallelogramm"),
      article   = c("die",        "das",       "das"),
      preferred = c(TRUE,         FALSE,       FALSE),
      stringsAsFactors = FALSE
    )
  ),
  
  trapezoid = list(
    english = "trapezoid",
    forms = data.frame(
      noun      = c("Trapez", "Viereck"),
      article   = c("das",    "das"),
      preferred = c(TRUE,     FALSE),
      stringsAsFactors = FALSE
    )
  ),
  
  circle = list(
    english = "circle",
    forms = data.frame(
      noun      = c("Kreis"),
      article   = c("der"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  
  semicircle = list(
    english = "semicircle",
    forms = data.frame(
      noun      = c("Halbkreis"),
      article   = c("der"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  
  oval = list(
    english = "oval",
    forms = data.frame(
      noun      = c("Oval"),
      article   = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  )
)

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

normalize_term <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  # remove leading articles if user types them
  x <- sub("^(der|die|das)\\s+", "", x)
  x <- gsub("\\s+", "", x)
  x
}

shapes_info <- list(
  triangle = list(
    english = "triangle",
    forms = data.frame(
      noun      = c("Dreieck"),
      article   = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  pentagon = list(
    english = "pentagon",
    forms = data.frame(
      noun      = c("Fünfeck"),
      article   = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  hexagon = list(
    english = "hexagon",
    forms = data.frame(
      noun      = c("Sechseck"),
      article   = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  octagon = list(
    english = "octagon",
    forms = data.frame(
      noun      = c("Achteck"),
      article   = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  decagon = list(
    english = "decagon",
    forms = data.frame(
      noun      = c("Zehneck"),
      article   = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  square = list(
    english = "square",
    forms = data.frame(
      noun      = c("Quadrat", "Viereck", "Rechteck", "Parallelogramm", "Raute"),
      article   = c("das", "das", "das", "das", "die"),
      preferred = c(TRUE, FALSE, FALSE, FALSE, FALSE),
      stringsAsFactors = FALSE
    )
  ),
  rectangle = list(
    english = "rectangle",
    forms = data.frame(
      noun      = c("Rechteck",   "Viereck",   "Parallelogramm"),
      article   = c("das",        "das",       "das"),
      preferred = c(TRUE,         FALSE,       FALSE),
      stringsAsFactors = FALSE
    )
  ),
  parallelogram = list(
    english = "parallelogram",
    forms = data.frame(
      noun      = c("Parallelogramm", "Viereck"),
      article   = c("das",            "das"),
      preferred = c(TRUE,             FALSE),
      stringsAsFactors = FALSE
    )
  ),
  rhombus = list(
    english = "rhombus",
    forms = data.frame(
      noun      = c("Raute",      "Viereck",   "Parallelogramm"),
      article   = c("die",        "das",       "das"),
      preferred = c(TRUE,         FALSE,       FALSE),
      stringsAsFactors = FALSE
    )
  ),
  trapezoid = list(
    english = "trapezoid",
    forms = data.frame(
      noun      = c("Trapez", "Viereck"),
      article   = c("das",    "das"),
      preferred = c(TRUE,     FALSE),
      stringsAsFactors = FALSE
    )
  ),
  circle = list(
    english = "circle",
    forms = data.frame(
      noun      = c("Kreis"),
      article   = c("der"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  semicircle = list(
    english = "semicircle",
    forms = data.frame(
      noun      = c("Halbkreis"),
      article   = c("der"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  oval = list(
    english = "oval",
    forms = data.frame(
      noun      = c("Oval"),
      article   = c("das"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  )
)

for (k in names(shapes_info)) {
  df <- shapes_info[[k]]$forms
  df$norm_noun <- normalize_term(df$noun)
  shapes_info[[k]]$forms <- df
}

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
    tags$script(HTML(
      "
document.addEventListener('DOMContentLoaded', function () {
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
    style = "max-width: 650px; margin: 0 auto; text-align: center; padding-top: 5px; padding-bottom: 5px;",
    
    plotOutput("shape_plot", height = "320px"),
    
    tags$div(style = "height: 5px;"),
    
    radioButtons(
      "article_choice",
      label = "Select the correct article",
      choices = c("der", "die", "das"),
      selected = character(0),
      inline = TRUE
    ),
    
    tags$div(style = "height: 5px;"),
    
    div(
      style = "display: flex; justify-content: center;",
      div(
        style = "width: 320px; text-align: center;",
        textInput(
          "user_answer",
          label = "Enter the German name",
          value = "",
          width = "100%"
        )
      )
    ),
    
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
  current_shape <- reactiveVal(sample(names(shapes), 1))
  mode <- reactiveVal("check")
  feedback_html <- reactiveVal("")
  output$shape_plot <- renderPlot({
    key <- current_shape()
    shapes[[key]]()
  })
  
  output$feedback_ui <- renderUI({
    HTML(feedback_html())
  })
  
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
      art_raw <- input$article_choice
      
      if (trimws(ans_raw) == "") {
        feedback_html("Please enter the German name of the shape.")
        return()
      }
      
      if (is.null(art_raw) || art_raw == "") {
        feedback_html("Please select der / die / das.")
        return()
      }
      
      if (grepl("^(?i)\\s*(der|die|das)\\s+", ans_raw)) {
        feedback_html(
          "Please enter the shape name <b>without</b> der/die/das; choose the article with the radio buttons above."
        )
        return()
      }
      
      ans_norm <- normalize_term(ans_raw)
      art <- art_raw
      
      key <- current_shape()
      info <- shapes_info[[key]]
      forms <- info$forms
      
      match_idx <- which(ans_norm == forms$norm_noun)
      
      if (length(match_idx) == 0) {
        preferred <- forms[forms$preferred, , drop = FALSE]
        
        pref_parts <- paste0(
          "<b>",
          htmltools::htmlEscape(paste(preferred$article, preferred$noun)),
          "</b>"
        )
        correct_str <- paste(pref_parts, collapse = " or ")
        
        feedback_html(
          paste0(
            "❌ Incorrect. The correct answer is: ",
            correct_str,
            "."
          )
        )
        
      } else {
        match_forms <- forms[match_idx, , drop = FALSE]
        correct_article_idx <- which(match_forms$article == art)
        
        if (length(correct_article_idx) == 0) {
          correct_genders <- unique(paste0(match_forms$article, " ", match_forms$noun))
          feedback_html(
            paste0(
              "❌ The noun is right, but the article is incorrect. ",
              "For what you wrote, use: <b>",
              htmltools::htmlEscape(paste(correct_genders, collapse = " or ")),
              "</b>."
            )
          )
          
        } else {
          chosen <- match_forms[correct_article_idx[1], , drop = FALSE]
          
          user_noun_trim    <- trimws(ans_raw)
          canonical_noun    <- chosen$noun
          canonical_article <- chosen$article
          
          cap_issue <- (tolower(user_noun_trim) == tolower(canonical_noun) &&
                          user_noun_trim != canonical_noun)

          extra_note <- ""
          if (key == "tri_pyramid") {
            other <- forms[forms$noun != canonical_noun & forms$preferred, , drop = FALSE]
            if (nrow(other) == 1) {
              other_str <- paste(other$article, other$noun)
              extra_note <- paste0(
                "<br>This shape is also sometimes referred to as <b>",
                htmltools::htmlEscape(other_str),
                "</b>."
              )
            }
          }
          
          if (!chosen$preferred) {
            preferred <- forms[forms$preferred, , drop = FALSE]
            pref_text <- paste0(preferred$article, " ", preferred$noun)
            pref_str  <- paste(pref_text, collapse = " or ")
            
            if (cap_issue) {
              feedback_html(
                paste0(
                  "✅ Accepted: <b>",
                  htmltools::htmlEscape(paste(art, user_noun_trim)),
                  "</b>.<br>",
                  "Remember that German nouns are capitalized, so it should be: <b>",
                  htmltools::htmlEscape(paste(canonical_article, canonical_noun)),
                  "</b>.<br>",
                  "However, the more usual term here is: <b>",
                  htmltools::htmlEscape(pref_str),
                  "</b>.",
                  extra_note
                )
              )
            } else {
              feedback_html(
                paste0(
                  "✅ Accepted: <b>",
                  htmltools::htmlEscape(paste(chosen$article, chosen$noun)),
                  "</b>.<br>",
                  "However, the more usual term here is: <b>",
                  htmltools::htmlEscape(pref_str),
                  "</b>.",
                  extra_note
                )
              )
            }
            
          } else {
            if (cap_issue) {
              feedback_html(
                paste0(
                  "✅ Accepted: <b>",
                  htmltools::htmlEscape(paste(art, user_noun_trim)),
                  "</b>.<br>",
                  "Remember that German nouns are capitalized, so it should be: <b>",
                  htmltools::htmlEscape(paste(canonical_article, canonical_noun)),
                  "</b>.",
                  extra_note
                )
              )
            } else {
              feedback_html(
                paste0(
                  "✅ Correct! <b>",
                  htmltools::htmlEscape(paste(canonical_article, canonical_noun)),
                  "</b>.",
                  extra_note
                )
              )
            }
          }
        }
      }
      
      mode("next")
      updateActionButton(session, "check_next", label = "Next question")
      
    } else {
      current_shape(sample(names(shapes), 1))
      updateTextInput(session, "user_answer", value = "")
      updateRadioButtons(session, "article_choice", selected = character(0))
      feedback_html("")
      
      mode("check")
      updateActionButton(session, "check_next", label = "Check my answer")
    }
  })
}

shinyApp(ui, server)
