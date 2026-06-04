library(shiny)

# Allowed denominators
allowed_denominators <- c(2:12, 20, 50, 100)

# Fractions we want to see less often (in simplest form)
# Represented as character "num/den" for easy comparison
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
    return(german_tens[as.character(n)])  # exact tens
  }
  
  ones <- n %% 10
  tens <- n - ones
  
  ones_word <- german_0_19[ones + 1]
  tens_word <- german_tens[as.character(tens)]
  
  paste0(ones_word, "und", tens_word)
}

# Numerator word: use German 0–99; replace "eins" with "ein"
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

# Build a German phrase like "drei Viertel", "ein Halb", "dreizehn Zwanzigstel"
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

# Generate a random proper fraction:
# 1. Sample denominator uniformly from allowed_denominators.
# 2. For 50 and 100, use only odd numerators (to avoid /25 after simplification).
#    For others, sample numerator uniformly from 1:(den - 1).
# 3. Simplify.
# 4. Tone down very common fractions via rejection.
generate_fraction <- function() {
  repeat {
    den_raw <- sample(allowed_denominators, 1)
    
    if (den_raw %in% c(50, 100)) {
      # Odd numerators only: 1,3,5,...,den_raw-1
      num_candidates <- seq(1, den_raw - 1, by = 2)
      num_raw <- sample(num_candidates, 1)
    } else {
      num_raw <- sample(1:(den_raw - 1), 1)
    }
    
    g <- gcd(num_raw, den_raw)
    num_simpl <- num_raw / g
    den_simpl <- den_raw / g
    
    if (num_simpl >= den_simpl) next  # safety
    
    frac_str <- paste0(num_simpl, "/", den_simpl)
    
    # If in common set, keep only with probability (1 - reject_prob)
    if (frac_str %in% common_fractions) {
      reject_prob <- 0.7  # 70% of the time we redraw
      if (runif(1) < reject_prob) next
    }
    
    return(list(num = num_simpl, den = den_simpl))
  }
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #user_answer input {
        text-align: center;
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
    style = "max-width: 600px; margin: 0 auto; text-align: center; padding-top: 10px;",
    
    uiOutput("fraction_ui"),
    
    br(),
    
    div(
      style = "display: flex; justify-content: center;",
      div(
        style = "width: 260px; text-align: center;",
        textInput(
          "user_answer",
          label = "Enter the fraction in simplest form (e.g., 3/4)",
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
  
  current_fraction <- reactiveVal(generate_fraction())
  mode <- reactiveVal("check")
  feedback_text <- reactiveVal("")
  
  output$fraction_ui <- renderUI({
    frac <- current_fraction()
    phrase <- german_fraction_phrase(frac$num, frac$den)
    div(
      style = "font-size: 2.5rem; margin-top: 0;",
      phrase
    )
  })
  
  output$feedback <- renderText({
    feedback_text()
  })
  
  observeEvent(input$check_next, {
    
    if (mode() == "check") {
      ans_raw <- trimws(input$user_answer)
      
      if (ans_raw == "") {
        feedback_text("Please enter a fraction like 3/4 (numerator/denominator).")
        return()
      }
      
      ans_clean <- gsub("\\s+", "", ans_raw)
      
      if (!grepl("^[0-9]+/[0-9]+$", ans_clean)) {
        feedback_text("Please enter the answer as a fraction using digits only, e.g., 3/4.")
        return()
      }
      
      parts <- strsplit(ans_clean, "/", fixed = TRUE)[[1]]
      num_ans <- as.integer(parts[1])
      den_ans <- as.integer(parts[2])
      
      if (is.na(num_ans) || is.na(den_ans) || den_ans == 0) {
        feedback_text("Invalid fraction. Please enter something like 3/4 with a nonzero denominator.")
        return()
      }
      
      g_ans <- gcd(num_ans, den_ans)
      num_ans_simpl <- num_ans / g_ans
      den_ans_simpl <- den_ans / g_ans
      
      frac <- current_fraction()
      
      if (num_ans_simpl == frac$num && den_ans_simpl == frac$den) {
        feedback_text("✅ Correct!")
      } else {
        feedback_text(
          sprintf("❌ Incorrect. The correct answer is: %d/%d", frac$num, frac$den)
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