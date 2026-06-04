library(shiny)

# Allowed denominators
allowed_denominators <- c(2:12, 20, 50, 100)

# Fractions we want to see less often (in simplest form)
common_fractions <- c("1/2", "1/3", "2/3", "1/4", "3/4")

# Greatest common divisor helper
gcd <- function(a, b) {
  a <- abs(a); b <- abs(b)
  while (b != 0) {
    tmp <- b
    b <- a %% b
    a <- tmp
  }
  a
}

# Basic German number words for 0–19
german_0_19 <- c(
  "null", "eins", "zwei", "drei", "vier", "fünf",
  "sechs", "sieben", "acht", "neun", "zehn",
  "elf", "zwölf", "dreizehn", "vierzehn", "fünfzehn",
  "sechzehn", "siebzehn", "achtzehn", "neunzehn"
)

# Tens words for 20, 30, ..., 90
german_tens <- c(
  "20" = "zwanzig",
  "30" = "dreißig",
  "40" = "vierzig",
  "50" = "fünfzig",
  "60" = "sechzig",
  "70" = "siebzig",
  "80" = "achtzig",
  "90" = "neunzig"
)

# Convert 0–99 to a German number word
german_number_word_0_99 <- function(n) {
  if (n < 0 || n > 99) stop("n must be between 0 and 99")
  
  if (n <= 19) {
    return(german_0_19[n + 1])
  }
  
  if (n %% 10 == 0) {
    # exact tens: 20,30,...,90
    return(german_tens[as.character(n)])
  }
  
  ones <- n %% 10
  tens <- n - ones
  
  ones_word <- german_0_19[ones + 1]
  # for 21,31,41,... we need "ein", not "eins"
  if (ones == 1) {
    ones_word <- "ein"
  }
  tens_word <- german_tens[as.character(tens)]
  
  paste0(ones_word, "und", tens_word)
}

# Numerator word: use German 0–99; replace "eins" with "ein" (for 1)
german_numerator_word <- function(n) {
  if (n < 1 || n > 99) {
    return(as.character(n))  # fallback if ever exceeded
  }
  w <- german_number_word_0_99(n)
  if (w == "eins") "ein" else w
}

# German denominator words (singular form)
denom_words <- c(
  "2"   = "Halb",
  "3"   = "Drittel",
  "4"   = "Viertel",
  "5"   = "Fünftel",
  "6"   = "Sechstel",
  "7"   = "Siebtel",
  "8"   = "Achtel",
  "9"   = "Neuntel",
  "10"  = "Zehntel",
  "11"  = "Elftel",
  "12"  = "Zwölftel",
  "20"  = "Zwanzigstel",
  "50"  = "Fünfzigstel",
  "100" = "Hundertstel"
)

# Build a German phrase like "drei Viertel", "ein Halb", "einundvierzig Fünfzigstel"
german_fraction_phrase <- function(num, den) {
  num_word <- german_numerator_word(num)
  den_str  <- as.character(den)
  
  if (!den_str %in% names(denom_words)) {
    stop("Denominator not supported in mapping.")
  }
  
  if (den == 2) {
    # With proper fractions + simplification, only 1/2 appears
    paste(num_word, "Halb")   # "ein Halb"
  } else {
    paste(num_word, denom_words[den_str])
  }
}

generate_fraction <- function() {
  repeat {
    den_raw <- sample(allowed_denominators, 1)
    
    # For 50 and 100, only use odd numerators to avoid simplifying to /25
    if (den_raw %in% c(50, 100)) {
      num_candidates <- seq(1, den_raw - 1, by = 2)  # 1,3,5,...,den_raw-1
      num_raw <- sample(num_candidates, 1)
    } else {
      # Other denominators: any proper numerator
      num_raw <- sample(1:(den_raw - 1), 1)
    }
    
    g <- gcd(num_raw, den_raw)
    num_simpl <- num_raw / g
    den_simpl <- den_raw / g
    
    if (num_simpl >= den_simpl) next  # safety, should never happen
    
    frac_str <- paste0(num_simpl, "/", den_simpl)
    
    # Down-weight some very common fractions
    if (frac_str %in% common_fractions) {
      reject_prob <- 0.7
      if (runif(1) < reject_prob) next
    }
    
    return(list(num = num_simpl, den = den_simpl))
  }
}

# Normalize German text for comparison
normalize_text <- function(x) {
  x <- tolower(x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #user_answer input {
        text-align: center;
      }
      .special-char-btn {
        margin: 0 3px;
      }
    ")),
    tags$script(HTML(
      "
document.addEventListener('DOMContentLoaded', function () {
  // Enter key triggers Check button
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

  // Insert special characters into the input
  document.body.addEventListener('click', function(e) {
    const btn = e.target.closest('.special-char-btn');
    if (!btn) return;

    const ch = btn.getAttribute('data-char');
    const input = document.getElementById('user_answer');
    if (!input || !ch) return;

    const start = input.selectionStart ?? input.value.length;
    const end   = input.selectionEnd   ?? input.value.length;
    const val   = input.value;

    input.value = val.slice(0, start) + ch + val.slice(end);
    input.focus();
    const newPos = start + ch.length;
    input.selectionStart = input.selectionEnd = newPos;

    // Notify Shiny about the change
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('user_answer', input.value, {priority: 'event'});
    }
  });
});
"
    ))
  ),
  
  div(
    style = "max-width: 600px; margin: 0 auto; text-align: center; padding-top: 10px;",
    
    # Show numeric fraction, e.g. "3 / 4"
    uiOutput("fraction_ui"),
    
    br(),
    
    div(
      style = "display: flex; justify-content: center;",
      div(
        style = "width: 320px; text-align: center;",
        textInput(
          "user_answer",
          label = "Write this fraction in German (e.g., drei Viertel)",
          value = "",
          width = "100%"
        )
      )
    ),
    
    br(),
    
    # Special character buttons
    div(
      style = "margin-bottom: 10px;",
      tags$button("ä", type = "button",
                  class = "special-char-btn btn btn-default",
                  `data-char` = "ä"),
      tags$button("ö", type = "button",
                  class = "special-char-btn btn btn-default",
                  `data-char` = "ö"),
      tags$button("ü", type = "button",
                  class = "special-char-btn btn btn-default",
                  `data-char` = "ü"),
      tags$button("ß", type = "button",
                  class = "special-char-btn btn btn-default",
                  `data-char` = "ß")
    ),
    
    actionButton("check_next", "Check my answer"),
    
    br(), br(),
    
    textOutput("feedback")
  )
)

server <- function(input, output, session) {
  
  current_fraction <- reactiveVal(generate_fraction())
  mode <- reactiveVal("check")
  feedback_text <- reactiveVal("")
  
  # Show the numeric fraction (e.g., 3/4)
  output$fraction_ui <- renderUI({
    frac <- current_fraction()
    div(
      style = "font-size: 2.5rem; margin-top: 0;",
      paste0(frac$num, " / ", frac$den)
    )
  })
  
  output$feedback <- renderText({
    feedback_text()
  })
  
  observeEvent(input$check_next, {
    
    if (mode() == "check") {
      ans_raw <- input$user_answer
      
      if (trimws(ans_raw) == "") {
        feedback_text("Please type the German phrase, e.g., drei Viertel.")
        return()
      }
      
      frac <- current_fraction()
      correct_phrase <- german_fraction_phrase(frac$num, frac$den)
      
      if (normalize_text(ans_raw) == normalize_text(correct_phrase)) {
        feedback_text("✅ Correct!")
      } else {
        feedback_text(
          sprintf(
            "❌ Incorrect. The correct answer is: \"%s\"",
            correct_phrase
          )
        )
      }
      
      mode("next")
      updateActionButton(session, "check_next", label = "Next question")
      
    } else {
      current_fraction(generate_fraction())
      updateTextInput(session, "user_answer", value = "")
      feedback_text("")
      mode("check")
      updateActionButton(session, "check_next", label = "Check my answer")
    }
  })
}

shinyApp(ui, server)