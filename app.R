library(shiny)
library(dplyr)

# ========== GLOBAL: load data and fit model once ==========

path <- "data/mental health social media balance dataset.csv"

if (!file.exists(path)) {
  stop("Data file not found: ", path,
       "\nMake sure it is in data/mental health social media balance dataset.csv")
}

df <- read.csv(path, stringsAsFactors = FALSE)

df <- df %>%
  rename(
    Daily_Screen_Time_hrs = `Daily_Screen_Time.hrs.`,
    Sleep_Quality         = `Sleep_Quality.1.10.`,
    Stress_Level          = `Stress_Level.1.10.`,
    Exercise_Frequency    = `Exercise_Frequency.week.`,
    Happiness_Index       = `Happiness_Index.1.10.`
  ) %>%
  mutate(
    Gender                = factor(Gender),
    Social_Media_Platform = factor(Social_Media_Platform),
    # binary outcome: 1 = low happiness (<= 5)
    Low_Happiness         = ifelse(Happiness_Index <= 5, 1, 0)
  ) %>%
  select(
    Low_Happiness,
    Age,
    Gender,
    Daily_Screen_Time_hrs,
    Sleep_Quality,
    Stress_Level,
    Days_Without_Social_Media,
    Exercise_Frequency,
    Social_Media_Platform,
    Happiness_Index
  ) %>%
  na.omit()

# logistic regression formula using all predictors
logit_formula <- Low_Happiness ~ Age + Gender + Daily_Screen_Time_hrs +
  Sleep_Quality + Stress_Level + Days_Without_Social_Media +
  Exercise_Frequency + Social_Media_Platform

logit_model <- glm(logit_formula, data = df, family = binomial())

# probabilities for all rows (used for confusion matrix & metrics)
all_probs <- predict(logit_model, type = "response")

# ========== UI ==========

ui <- fluidPage(
  titlePanel("Mental Health & Social Media – Logistic Regression"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      
      sliderInput("threshold", "Classification threshold:",
                  min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      
      hr(),
      h4("Single Prediction Input"),
      
      numericInput("new_age", "Age:", value = 30, min = 10, max = 80),
      selectInput("new_gender", "Gender:", choices = levels(df$Gender)),
      numericInput("new_screen", "Daily screen time (hrs):",
                   value = 5, min = 0, max = 24),
      sliderInput("new_sleep", "Sleep quality (1–10):",
                  min = 1, max = 10, value = 7),
      sliderInput("new_stress", "Stress level (1–10):",
                  min = 1, max = 10, value = 5),
      numericInput("new_days_off", "Days without social media:",
                   value = 2, min = 0, max = 30),
      numericInput("new_exercise", "Exercise freq (per week):",
                   value = 3, min = 0, max = 14),
      selectInput("new_platform", "Social media platform:",
                  choices = levels(df$Social_Media_Platform))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          h3("Project Description"),
          p("This Shiny app uses the 'Mental Health Social Media Balance' dataset."),
          p("We create a binary outcome Low_Happiness (Happiness_Index ≤ 5), and fit a logistic regression model using age, gender, daily screen time, sleep quality, stress level, days without social media, exercise frequency, and primary social media platform as predictors.")
        ),
        tabPanel("Model Summary", verbatimTextOutput("model_summary")),
        tabPanel(
          "Performance",
          h4("Confusion Matrix"),
          tableOutput("conf_mat"),
          h4("Performance Metrics"),
          verbatimTextOutput("metrics")
        ),
        tabPanel(
          "Predict New Case",
          h3("Predicted Probability of Low Happiness (Happiness ≤ 5)"),
          verbatimTextOutput("new_prediction_text")
        )
      )
    )
  )
)

# ========== SERVER ==========

server <- function(input, output, session) {
  
  # ---- Model summary ----
  output$model_summary <- renderPrint({
    summary(logit_model)
  })
  
  # ---- Confusion matrix ----
  output$conf_mat <- renderTable({
    thr  <- input$threshold
    pred <- ifelse(all_probs >= thr, 1, 0)
    
    tab <- table(Predicted = pred, Actual = df$Low_Happiness)
    as.data.frame.matrix(tab)
  }, rownames = TRUE)
  
  # ---- Simple performance metrics ----
  output$metrics <- renderPrint({
    thr    <- input$threshold
    pred   <- ifelse(all_probs >= thr, 1, 0)
    actual <- df$Low_Happiness
    
    acc <- mean(pred == actual)
    
    tp <- sum(pred == 1 & actual == 1)
    tn <- sum(pred == 0 & actual == 0)
    fp <- sum(pred == 1 & actual == 0)
    fn <- sum(pred == 0 & actual == 1)
    
    sens <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
    spec <- ifelse(tn + fp == 0, NA, tn / (tn + fp))
    
    cat("Accuracy :", round(acc, 3), "\n")
    cat("Sensitivity (TPR):", round(sens, 3), "\n")
    cat("Specificity (TNR):", round(spec, 3), "\n")
    cat("TP:", tp, " FP:", fp, " FN:", fn, " TN:", tn, "\n")
  })
  
  # ---- Predict new case ----
  output$new_prediction_text <- renderPrint({
    new_row <- data.frame(
      Age                       = input$new_age,
      Gender                    = factor(input$new_gender,
                                         levels = levels(df$Gender)),
      Daily_Screen_Time_hrs     = input$new_screen,
      Sleep_Quality             = input$new_sleep,
      Stress_Level              = input$new_stress,
      Days_Without_Social_Media = input$new_days_off,
      Exercise_Frequency        = input$new_exercise,
      Social_Media_Platform     = factor(input$new_platform,
                                         levels = levels(df$Social_Media_Platform))
    )
    
    prob  <- predict(logit_model, newdata = new_row, type = "response")
    class <- ifelse(prob >= input$threshold, 1, 0)
    
    cat(sprintf("Predicted probability of LOW happiness (Happiness ≤ 5): %.3f\n", prob))
    cat("Threshold =", input$threshold, "→ Predicted class:",
        ifelse(class == 1, "LOW happiness (1)", "Moderate/High happiness (0)"),
        "\n")
  })
}

shinyApp(ui = ui, server = server)