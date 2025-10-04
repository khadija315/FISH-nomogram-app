# -----------------------------
# Function to predict recurrence risk for new patients
# -----------------------------
predict_recurrence <- function(newdata, cox_model, horizon = 365) {
  # Ensure factors match levels in training data
  newdata <- newdata %>%
    mutate(
      gender = factor(gender, levels = levels(df_lm3$gender)),
      smoking_status = factor(smoking_status, levels = levels(df_lm3$smoking_status)),
      pre_GD_stage = factor(pre_GD_stage, levels = levels(df_lm3$pre_GD_stage))
    )
  
  # Linear predictor
  lp <- predict(cox_model, newdata = newdata, type = "lp")
  
  # Baseline cumulative hazard
  base_haz <- basehaz(cox_model, centered = FALSE)
  H0_t <- base_haz$hazard[which.min(abs(base_haz$time - horizon))]
  
  # Recurrence risk at specified horizon
  risk <- 1 - exp(-H0_t * exp(lp))
  
  # Return
  return(risk)
}


# Single patient
patient1 <- data.frame(
  FISH_Abn = 0.12,
  `Age GD` = 65,
  gender = "Male",
  smoking_status = "Never Smoker",
  pre_GD_stage = "TaHG",
  `Days to Induction` = 15,
  `GD treatment duration SL` = 30
)

predict_recurrence(patient1, cox_rms, horizon = 365)



# Multiple patients
patients <- data.frame(
  FISH_Abn = c(0.12, 0.25),
  `Age GD` = c(65, 70),
  gender = c("Male", "Female"),
  smoking_status = c("Never Smoker", "Current Smoker"),
  pre_GD_stage = c("TaHG", "T1HG"),
  `Days to Induction` = c(15, 20),
  `GD treatment duration SL` = c(30, 45)
)

predict_recurrence(patients, cox_rms, horizon = 365)


# This will return the predicted 1-year recurrence risk for each patient. 
# You can change horizon = 730 or 1095 to get 2- or 3-year recurrence risk, respectively.

# ---------------------------------------------------------------------------------------------------------------------

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# A simple Shiny app that replicates your nomogram: 
# Users enter patient covariates, and it returns predicted recurrence risk at 1, 2, and 3 years
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# -----------------------------
# Shiny app for Cox nomogram predictions
# -----------------------------
library(shiny)
library(rms)
library(dplyr)

# -----------------------------
# Load or define your cox model (cox_rms) and df_lm3
# -----------------------------
# Assuming cox_rms and df_lm3 are already in your environment

# Function to predict recurrence risk
predict_recurrence <- function(newdata, cox_model, horizon) {
  newdata <- newdata %>%
    mutate(
      gender = factor(gender, levels = levels(df_lm3$gender)),
      smoking_status = factor(smoking_status, levels = levels(df_lm3$smoking_status)),
      pre_GD_stage = factor(pre_GD_stage, levels = levels(df_lm3$pre_GD_stage))
    )
  
  lp <- predict(cox_model, newdata = newdata, type = "lp")
  base_haz <- basehaz(cox_model, centered = FALSE)
  H0_t <- base_haz$hazard[which.min(abs(base_haz$time - horizon))]
  
  risk <- 1 - exp(-H0_t * exp(lp))
  return(risk)
}

# -----------------------------
# Define UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Bladder Cancer Recurrence Risk Calculator (Nomogram)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("FISH_Abn", "FISH Abnormality (proportion)", value = 0.1, min = 0, max = 1, step = 0.01),
      numericInput("Age_GD", "Age at GD", value = 65, min = 18, max = 100, step = 1),
      selectInput("gender", "Gender", choices = levels(df_lm3$gender)),
      selectInput("smoking_status", "Smoking Status", choices = levels(df_lm3$smoking_status)),
      selectInput("pre_GD_stage", "Pre GD Stage", choices = levels(df_lm3$pre_GD_stage)),
      numericInput("Days_to_Induction", "Days to Induction", value = 15, min = 0, max = 365),
      numericInput("GD_treatment_duration_SL", "GD Treatment Duration (SL)", value = 30, min = 0, max = 365),
      actionButton("calc", "Calculate Risk")
    ),
    mainPanel(
      h3("Predicted Recurrence Risk"),
      tableOutput("risk_table")
    )
  )
)

# -----------------------------
# Define server
# -----------------------------
server <- function(input, output) {
  
  observeEvent(input$calc, {
    # Collect patient info
    new_patient <- data.frame(
      FISH_Abn = input$FISH_Abn,
      `Age GD` = input$Age_GD,
      gender = input$gender,
      smoking_status = input$smoking_status,
      pre_GD_stage = input$pre_GD_stage,
      `Days to Induction` = input$Days_to_Induction,
      `GD treatment duration SL` = input$GD_treatment_duration_SL
    )
    
    # Compute risks
    risk_1yr <- predict_recurrence(new_patient, cox_rms, 365)
    risk_2yr <- predict_recurrence(new_patient, cox_rms, 730)
    risk_3yr <- predict_recurrence(new_patient, cox_rms, 1095)
    
    # Render table
    output$risk_table <- renderTable({
      data.frame(
        Horizon = c("1 Year", "2 Years", "3 Years"),
        Recurrence_Risk = c(round(risk_1yr, 3), round(risk_2yr, 3), round(risk_3yr, 3))
      )
    })
  })
}

# -----------------------------
# Run the app
# -----------------------------
shinyApp(ui = ui, server = server)