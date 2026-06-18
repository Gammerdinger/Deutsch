library(shiny)

blank_plot <- function(xlim = c(-2, 2), ylim = c(-2, 2)) {
  plot(0, 0, type = "n", asp = 1,
       xlim = xlim, ylim = ylim,
       xlab = "", ylab = "", axes = FALSE)
}

tri_prism <- function() {
  n <- 3
  theta <- seq(pi/2, pi/2 + 2*pi, length.out = n + 1)[- (n + 1)]
  r <- 1
  x_front <- r * cos(theta)
  y_front <- r * sin(theta)
  dx <- 0.7; dy <- 0.7
  x_back <- x_front + dx
  y_back <- y_front + dy
  
  blank_plot(range(c(x_front, x_back)), range(c(y_front, y_back)))
  
  draw_poly <- function(x, y, lty = 1, col = "black", lwd = 2) {
    for (i in 1:n) {
      j <- ifelse(i == n, 1, i + 1)
      segments(x[i], y[i], x[j], y[j], lwd = lwd, lty = lty, col = col)
    }
  }
  
  draw_poly(x_back,  y_back,  lty = 2, col = "grey60")
  for (i in 1:n)
    segments(x_front[i], y_front[i], x_back[i], y_back[i],
             lwd = 2, lty = 2, col = "grey60")
  draw_poly(x_front, y_front)
  solid_vert <- c(1, 3)
  for (i in solid_vert)
    segments(x_front[i], y_front[i], x_back[i], y_back[i], lwd = 2)
  segments(x_back[1], y_back[1], x_back[3], y_back[3], lwd = 2)
}

rect_prism <- function() {
  x_front <- c(-1.5,  1.5,  1.5, -1.5)
  y_front <- c(-1.0, -1.0,  1.0,  1.0)
  dx <- 0.7; dy <- 0.7
  x_back <- x_front + dx
  y_back <- y_front + dy
  
  blank_plot(range(c(x_front, x_back)), range(c(y_front, y_back)))
  
  draw_rect <- function(x, y, lty = 1, col = "black", lwd = 2) {
    n <- length(x)
    for (i in 1:n) {
      j <- ifelse(i == n, 1, i + 1)
      segments(x[i], y[i], x[j], y[j], lwd = lwd, lty = lty, col = col)
    }
  }
  
  draw_rect(x_back, y_back, lty = 2, col = "grey60")
  n <- length(x_front)
  for (i in 1:n)
    segments(x_front[i], y_front[i], x_back[i], y_back[i],
             lwd = 2, lty = 2, col = "grey60")
  draw_rect(x_front, y_front)
  
  solid_vert <- c(4, 3, 2)
  for (i in solid_vert)
    segments(x_front[i], y_front[i], x_back[i], y_back[i], lwd = 2)
  for (p in list(c(4, 3), c(3, 2))) {
    i <- p[1]; j <- p[2]
    segments(x_back[i], y_back[i], x_back[j], y_back[j], lwd = 2)
  }
}

pent_prism <- function() {
  n <- 5
  theta <- seq(pi/2, pi/2 + 2*pi, length.out = n + 1)[- (n + 1)]
  r <- 1
  x_front <- r * cos(theta)
  y_front <- r * sin(theta)
  dx <- 0.7; dy <- 0.7
  x_back <- x_front + dx
  y_back <- y_front + dy
  
  blank_plot(range(c(x_front, x_back)), range(c(y_front, y_back)))
  
  draw_pent <- function(x, y, lty = 1, col = "black", lwd = 2) {
    for (i in 1:n) {
      j <- ifelse(i == n, 1, i + 1)
      segments(x[i], y[i], x[j], y[j], lwd = lwd, lty = lty, col = col)
    }
  }
  
  draw_pent(x_back, y_back, lty = 2, col = "grey60")
  for (i in 1:n)
    segments(x_front[i], y_front[i], x_back[i], y_back[i],
             lwd = 2, lty = 2, col = "grey60")
  draw_pent(x_front, y_front)
  
  solid_vert <- c(2, 1, 5, 4)
  for (i in solid_vert)
    segments(x_front[i], y_front[i], x_back[i], y_back[i], lwd = 2)
  
  for (p in list(c(2, 1), c(1, 5), c(5, 4))) {
    i <- p[1]; j <- p[2]
    segments(x_back[i], y_back[i], x_back[j], y_back[j], lwd = 2)
  }
}

hex_prism <- function() {
  n <- 6
  theta <- seq(pi/2, pi/2 + 2*pi, length.out = n + 1)[- (n + 1)]
  r <- 1
  x_front <- r * cos(theta)
  y_front <- r * sin(theta)
  dx <- 0.7; dy <- 0.7
  x_back <- x_front + dx
  y_back <- y_front + dy
  
  blank_plot(range(c(x_front, x_back)), range(c(y_front, y_back)))
  
  draw_hex <- function(x, y, lty = 1, col = "black", lwd = 2) {
    for (i in 1:n) {
      j <- ifelse(i == n, 1, i + 1)
      segments(x[i], y[i], x[j], y[j], lwd = lwd, lty = lty, col = col)
    }
  }
  
  draw_hex(x_back, y_back, lty = 2, col = "grey60")
  for (i in 1:n)
    segments(x_front[i], y_front[i], x_back[i], y_back[i],
             lwd = 2, lty = 2, col = "grey60")
  draw_hex(x_front, y_front)
  
  solid_vert <- c(2, 1, 6, 5)
  for (i in solid_vert)
    segments(x_front[i], y_front[i], x_back[i], y_back[i], lwd = 2)
  
  for (p in list(c(2, 1), c(1, 6), c(6, 5))) {
    i <- p[1]; j <- p[2]
    segments(x_back[i], y_back[i], x_back[j], y_back[j], lwd = 2)
  }
}

cube <- function() {
  x_front <- c(-1,  1,  1, -1)
  y_front <- c(-1, -1,  1,  1)
  dx <- 0.7; dy <- 0.7
  x_back <- x_front + dx
  y_back <- y_front + dy
  
  blank_plot(range(c(x_front, x_back)), range(c(y_front, y_back)))
  
  draw_rect <- function(x, y, lty = 1, col = "black", lwd = 2) {
    n <- length(x)
    for (i in 1:n) {
      j <- ifelse(i == n, 1, i + 1)
      segments(x[i], y[i], x[j], y[j], lwd = lwd, lty = lty, col = col)
    }
  }
  
  draw_rect(x_back, y_back, lty = 2, col = "grey60")
  n <- length(x_front)
  for (i in 1:n)
    segments(x_front[i], y_front[i], x_back[i], y_back[i],
             lwd = 2, lty = 2, col = "grey60")
  draw_rect(x_front, y_front)
  
  solid_vert <- c(4, 3, 2)
  for (i in solid_vert)
    segments(x_front[i], y_front[i], x_back[i], y_back[i], lwd = 2)
  for (p in list(c(4, 3), c(3, 2))) {
    i <- p[1]; j <- p[2]
    segments(x_back[i], y_back[i], x_back[j], y_back[j], lwd = 2)
  }
}

tri_pyramid <- function() {
  x_base <- c(-1.5, 1.5, 0)
  y_base <- c(-0.8, -0.8, -0.2)
  cx <- mean(x_base); cy <- mean(y_base)
  x_apex <- cx; y_apex <- cy + 2.0
  
  x <- c(x_base, x_apex)
  y <- c(y_base, y_apex)
  blank_plot(range(x), range(y))
  
  draw_edge <- function(i, j, lty = 1, col = "black", lwd = 2) {
    segments(x[i], y[i], x[j], y[j], lwd = lwd, lty = lty, col = col)
  }
  
  draw_edge(1, 2)
  draw_edge(2, 3, lty = 2, col = "grey60")
  draw_edge(3, 1, lty = 2, col = "grey60")
  draw_edge(1, 4)
  draw_edge(2, 4)
  draw_edge(3, 4, lty = 2, col = "grey60")
}

square_pyramid <- function() {
  x_base <- c(-0.2,  1.2,  0.3, -1.1)
  y_base <- c(-0.4, -0.3,  0,   -0.1)
  cx <- mean(x_base); cy <- mean(y_base)
  x_apex <- cx; y_apex <- cy + 1.5
  x <- c(x_base, x_apex)
  y <- c(y_base, y_apex)
  
  blank_plot(range(x), range(y))
  
  draw_edge <- function(i, j, lty = 1, col = "black", lwd = 2) {
    segments(x[i], y[i], x[j], y[j], lwd = lwd, lty = lty, col = col)
  }
  
  draw_edge(1, 2)
  draw_edge(2, 3, lty = 2, col = "grey60")
  draw_edge(3, 4, lty = 2, col = "grey60")
  draw_edge(4, 1)
  draw_edge(1, 5)
  draw_edge(2, 5)
  draw_edge(3, 5, lty = 2, col = "grey60")
  draw_edge(4, 5)
}

cylinder <- function() {
  blank_plot(c(-2, 2), c(-3, 3))
  r <- 1.2; ry <- 0.4; h <- 3; n <- 200
  theta <- seq(0, 2*pi, length.out = n)
  x_top <- r * cos(theta)
  y_top <- ry * sin(theta) + h/2
  x_bot <- r * cos(theta)
  y_bot <- ry * sin(theta) - h/2
  front_idx <- theta <= pi
  back_idx  <- theta >  pi
  lines(x_top, y_top, lwd = 2)
  lines(x_bot[back_idx],  y_bot[back_idx],  lwd = 2)
  lines(x_bot[front_idx], y_bot[front_idx], lty = 2, col = "grey60", lwd = 2)
  i_left  <- which.min(abs(theta - pi))
  i_right <- which.min(abs(theta - 0))
  segments(x_top[i_left],  y_top[i_left],  x_bot[i_left],  y_bot[i_left],  lwd = 2)
  segments(x_top[i_right], y_top[i_right], x_bot[i_right], y_bot[i_right], lwd = 2)
}

cone <- function() {
  blank_plot(c(-2, 2), c(-3, 3))
  r <- 1.2; ry <- 0.4; h <- 3; n <- 200
  theta <- seq(0, 2*pi, length.out = n)
  x_base <- r * cos(theta)
  y_base <- ry * sin(theta) - h/2
  front_idx <- theta <= pi
  back_idx  <- theta >  pi
  lines(x_base[back_idx],  y_base[back_idx],  lwd = 2)
  lines(x_base[front_idx], y_base[front_idx], lty = 2, col = "grey60", lwd = 2)
  x_apex <- 0
  y_apex <- h/2 + 0.5
  i_left  <- which.min(abs(theta - pi))
  i_right <- which.min(abs(theta - 0))
  segments(x_apex, y_apex, x_base[i_left],  y_base[i_left],  lwd = 2)
  segments(x_apex, y_apex, x_base[i_right], y_base[i_right], lwd = 2)
}

sphere <- function() {
  blank_plot(c(-2, 2), c(-2, 2))
  r <- 1.2; ry <- 0.5; n <- 400
  theta <- seq(0, 2*pi, length.out = n)
  x_outer <- r * cos(theta)
  y_outer <- r * sin(theta)
  x_eq <- r * cos(theta)
  y_eq <- ry * sin(theta)
  front_idx <- theta <= pi
  back_idx  <- theta >  pi
  lines(x_outer, y_outer, lwd = 2)
  lines(x_eq[back_idx],  y_eq[back_idx],  lwd = 2)
  lines(x_eq[front_idx], y_eq[front_idx], lty = 2, col = "grey60", lwd = 2)
}

hemisphere <- function() {
  blank_plot(c(-2, 2), c(-1, 2))
  r <- 1.2; ry <- 0.5; n <- 400
  theta <- seq(0, 2*pi, length.out = n)
  theta_top <- seq(pi, 0, length.out = n)
  x_top <- r * cos(theta_top)
  y_top <- r * sin(theta_top)
  x_eq <- r * cos(theta)
  y_eq <- ry * sin(theta)
  front_idx <- theta <= pi
  back_idx  <- theta >  pi
  lines(x_eq[front_idx], y_eq[front_idx], lty = 2, col = "grey60", lwd = 2)
  lines(x_eq[back_idx],  y_eq[back_idx],  lwd = 2)
  lines(x_top, y_top, lwd = 2)
}

shapes <- list(
  tri_prism      = tri_prism,
  rect_prism     = rect_prism,
  pent_prism     = pent_prism,
  hex_prism      = hex_prism,
  cube           = cube,
  tri_pyramid    = tri_pyramid,
  square_pyramid = square_pyramid,
  cylinder       = cylinder,
  cone           = cone,
  sphere         = sphere,
  hemisphere     = hemisphere
)

shapes_info <- list(
  tri_prism = list(
    english = "triangular prism",
    forms = data.frame(
      noun      = c("dreieckige Prisma",   "dreiseitige Prisma"),
      article   = c("das",                 "das"),
      preferred = c(TRUE,                  FALSE),
      stringsAsFactors = FALSE
    )
  ),
  rect_prism = list(
    english = "rectangular prism",
    forms = data.frame(
      noun      = c("Quader",              "viereckige Prisma", "vierseitige Prisma"),
      article   = c("der",                 "das", "das"),
      preferred = c(TRUE,                  FALSE, FALSE),
      stringsAsFactors = FALSE
    )
  ),
  pent_prism = list(
    english = "pentagonal prism",
    forms = data.frame(
      noun      = c("fünfeckige Prisma",   "fünfseitige Prisma"),
      article   = c("das",                 "das"),
      preferred = c(TRUE,                  FALSE),
      stringsAsFactors = FALSE
    )
  ),
  hex_prism = list(
    english = "hexagonal prism",
    forms = data.frame(
      noun      = c("sechseckige Prisma",  "sechsseitige Prisma"),
      article   = c("das",                 "das"),
      preferred = c(TRUE,                  FALSE),
      stringsAsFactors = FALSE
    )
  ),
  cube = list(
    english = "cube",
    forms = data.frame(
      noun      = c("Würfel",              "viereckige Prisma", "vierseitige Prisma"),
      article   = c("der",                 "das", "das"),
      preferred = c(TRUE,                  FALSE, FALSE),
      stringsAsFactors = FALSE
    )
  ),
  tri_pyramid = list(
    english = "triangular pyramid",
    forms = data.frame(
      noun      = c("dreieckige Pyramide", "dreiseitige Pyramide", "Tetraeder"),
      article   = c("die",                 "die",                  "der"),
      preferred = c(TRUE,                  FALSE,                  TRUE),
      stringsAsFactors = FALSE
    )
  ),
  square_pyramid = list(
    english = "square pyramid",
    forms = data.frame(
      noun      = c("viereckige Pyramide", "vierseitige Pyramide"),
      article   = c("die",                 "die"),
      preferred = c(TRUE,                  FALSE),
      stringsAsFactors = FALSE
    )
  ),
  cylinder = list(
    english = "cylinder",
    forms = data.frame(
      noun      = c("Zylinder"),
      article   = c("der"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  cone = list(
    english = "cone",
    forms = data.frame(
      noun      = c("Kegel"),
      article   = c("der"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  sphere = list(
    english = "sphere",
    forms = data.frame(
      noun      = c("Kugel"),
      article   = c("die"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  ),
  hemisphere = list(
    english = "hemisphere",
    forms = data.frame(
      noun      = c("Halbkugel"),
      article   = c("die"),
      preferred = c(TRUE),
      stringsAsFactors = FALSE
    )
  )
)

normalize_term <- function(x) {
  x <- trimws(x)
  x <- tolower(x)
  x <- sub("^(der|die|das)\\s+", "", x)
  x <- gsub("\\s+", "", x)
  x
}

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
      .pron {
        margin-left: 4px;
        cursor: pointer;
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
    )),
    tags$audio(id = "global-audio-player"),
    tags$script(HTML(
      "
document.addEventListener('DOMContentLoaded', function () {
  console.log('audio-helper: DOMContentLoaded');
  const player = document.getElementById('global-audio-player');
  if (!player) {
    console.error('audio-helper: no #global-audio-player found');
    return;
  }

  document.body.addEventListener('click', function (e) {
    const el = e.target.closest('.pron');
    if (!el) return;

    e.preventDefault();

    const listAttr = el.getAttribute('data-src-list');
    const single = el.getAttribute('data-src');

    let sources = [];
    if (listAttr) {
      sources = listAttr.split(',').map(s => s.trim()).filter(Boolean);
    } else if (single) {
      sources = [single];
    }

    console.log('audio-helper: sources to play', sources);
    if (sources.length === 0) return;

    let idx = 0;

    if (player._sequenceHandler) {
      player.removeEventListener('ended', player._sequenceHandler);
      player._sequenceHandler = null;
    }

    const playNext = function () {
      if (idx >= sources.length) {
        console.log('audio-helper: sequence finished');
        player.removeEventListener('ended', playNext);
        player._sequenceHandler = null;
        return;
      }
      const src = sources[idx++];
      console.log('audio-helper: playing', src);
      player.src = src;
      player.load();
      player.play().catch(err => console.error('audio-helper: play error', err));
    };

    player._sequenceHandler = playNext;
    player.addEventListener('ended', playNext);

    playNext();
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
  
  audio_term_html <- function(article, noun, preferred_flag) {
    enable_audio <- !(isFALSE(preferred_flag) && grepl("seitige", noun, fixed = TRUE))
    art_esc <- htmltools::htmlEscape(article)
    noun_esc <- htmltools::htmlEscape(noun)
    base_text <- sprintf("<b>%s %s</b>", art_esc, noun_esc)
    if (!enable_audio) {
      return(base_text)
    }
    words <- strsplit(noun, "\\s+")[[1]]
    base <- "audio/3d/"
    if (length(words) == 1) {
      src <- paste0(base, words[1], ".ogg")
      sprintf(
        '%s <span class="pron" data-src="%s">🔊</span>',
        base_text,
        htmltools::htmlEscape(src)
      )
    } else {
      srcs <- paste0(base, words, ".ogg")
      attr <- paste(srcs, collapse = ",")
      sprintf(
        '%s <span class="pron" data-src-list="%s">🔊</span>',
        base_text,
        htmltools::htmlEscape(attr)
      )
    }
  }
  
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
        pref_parts <- vapply(
          seq_len(nrow(preferred)),
          function(i) audio_term_html(preferred$article[i], preferred$noun[i], preferred$preferred[i]),
          character(1)
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
          idxs <- seq_len(nrow(match_forms))
          correct_parts <- vapply(
            idxs,
            function(i) audio_term_html(
              match_forms$article[i],
              match_forms$noun[i],
              match_forms$preferred[i]
            ),
            character(1)
          )
          correct_parts <- unique(correct_parts)
          correct_str <- paste(correct_parts, collapse = " or ")
          
          base_msg <- paste0(
            "❌ The noun is right, but the article is incorrect. ",
            "For what you wrote, use: ",
            correct_str,
            "."
          )
          
          if (all(!match_forms$preferred) && any(forms$preferred)) {
            preferred <- forms[forms$preferred, , drop = FALSE]
            pref_parts <- vapply(
              seq_len(nrow(preferred)),
              function(i) audio_term_html(
                preferred$article[i],
                preferred$noun[i],
                preferred$preferred[i]
              ),
              character(1)
            )
            pref_str <- paste(pref_parts, collapse = " or ")
            
            base_msg <- paste0(
              base_msg,
              " However, the more usual term here is: ",
              pref_str,
              "."
            )
          }
          
          feedback_html(base_msg)
          
        } else {
          chosen <- match_forms[correct_article_idx[1], , drop = FALSE]
          
          if (!chosen$preferred) {
            preferred <- forms[forms$preferred, , drop = FALSE]
            pref_parts <- vapply(
              seq_len(nrow(preferred)),
              function(i) audio_term_html(
                preferred$article[i],
                preferred$noun[i],
                preferred$preferred[i]
              ),
              character(1)
            )
            pref_str <- paste(pref_parts, collapse = " or ")
            
            chosen_html <- audio_term_html(chosen$article, chosen$noun, chosen$preferred)
            
            feedback_html(
              paste0(
                "✅ Accepted: ",
                chosen_html,
                ".<br>",
                "However, the more usual term here is: ",
                pref_str,
                "."
              )
            )
          } else {
            user_noun_trim    <- trimws(ans_raw)
            canonical_noun    <- chosen$noun
            canonical_article <- chosen$article
            
            canonical_html <- audio_term_html(canonical_article, canonical_noun, TRUE)
            
            extra_note <- ""
            if (key == "tri_pyramid") {
              other <- forms[forms$noun != canonical_noun & forms$preferred, , drop = FALSE]
              if (nrow(other) == 1) {
                other_html <- audio_term_html(other$article, other$noun, other$preferred)
                extra_note <- paste0(
                  "<br>This shape is also sometimes referred to as ",
                  other_html,
                  "."
                )
              } else if (nrow(other) > 1) {
                other_parts <- vapply(
                  seq_len(nrow(other)),
                  function(i) audio_term_html(
                    other$article[i],
                    other$noun[i],
                    other$preferred[i]
                  ),
                  character(1)
                )
                other_str <- paste(other_parts, collapse = " or ")
                extra_note <- paste0(
                  "<br>This shape is also sometimes referred to as ",
                  other_str,
                  "."
                )
              }
            }
            
            if (tolower(user_noun_trim) == tolower(canonical_noun) &&
                user_noun_trim != canonical_noun) {
              feedback_html(
                paste0(
                  "✅ Accepted: <b>",
                  htmltools::htmlEscape(paste(art, user_noun_trim)),
                  "</b>.<br>",
                  "Remember that German nouns are capitalized, so it should be: ",
                  canonical_html,
                  ".",
                  extra_note
                )
              )
            } else {
              feedback_html(
                paste0(
                  "✅ Correct! ",
                  canonical_html,
                  ".",
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