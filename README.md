# Mental Health & Social Media — Logistic Regression Shiny App

This repository contains an interactive **R Shiny application** that analyzes the relationship between social media habits and mental well-being using logistic regression. The project is part of **DS501 – Case Study 3**, focusing on the data science lifecycle, machine learning modeling, and building an interactive web application.
The fully deployed Shiny app is available here: https://cmcourville.shinyapps.io/ds501-hw6/
---

## Dataset

The dataset used in this project is the:

**Mental Health Social Media Balance Dataset**  
Source: https://www.kaggle.com/datasets  

(Direct link if needed: https://www.kaggle.com/datasets/suchintan/mental-health-social-media-balance-dataset)

This dataset includes variables such as:
- Age  
- Gender  
- Daily Screen Time  
- Sleep Quality  
- Stress Level  
- Days Without Social Media  
- Exercise Frequency  
- Social Media Platform  
- Happiness Index  

The goal of the dataset is to explore the relationship between online behavior, lifestyle habits, and overall happiness.

---

## Project Goal

The main objective of this project is to:

1. **Load and clean the dataset**  
2. **Create a binary mental health outcome**  
   - `Low_Happiness = 1` if Happiness ≤ 5  
   - `Low_Happiness = 0` otherwise  
3. **Build a Logistic Regression model** to predict the likelihood of low happiness based on social media behavior and lifestyle factors  
4. **Develop an interactive Shiny App** where users can:  
   - View model summary  
   - Adjust the classification threshold  
   - View a confusion matrix and performance metrics  
   - Input hypothetical user characteristics to generate predictions  
5. **Deploy the app online** via shinyapps.io  

This project demonstrates the complete **data science lifecycle**:  
data ingestion → cleaning → modeling → evaluation → communication.

---

## Machine Learning Model

The app implements a **Logistic Regression** model trained with:

- Age  
- Gender  
- Daily Screen Time  
- Sleep Quality  
- Stress Level  
- Days Without Social Media  
- Exercise Frequency  
- Social Media Platform  

The model predicts the probability that a person has **Low Happiness (≤ 5)**.

---

## Shiny App Features

The application includes:

### About Page
- Description of dataset  
- Overview of methodology  
- Explanation of the logistic regression model  

### Model Summary
- Full regression output  
- Coefficients, significance, and diagnostics  

### Performance Tab
- Adjustable classification threshold  
- Confusion matrix  
- Accuracy, sensitivity, specificity  

### Predict New Case
- User enters characteristics  
- App predicts probability of low happiness  

---

## Repository Structure
ds501-hw6/
├── app.R
├── README.md
├── data/
│   └── mental health social media balance dataset.csv
├── model/                
└── additional_docs/      
---

## Running the App Locally

### **1. Clone the repository**
```bash
git clone https://github.com/cmcourville/ds501-hw6.git
cd ds501-hw6
```

### **2. Open RStudio and install required packages
```bash
install.packages(c("shiny", "dplyr"))
```

### **3. Run the app
```bash
shiny::runApp()
```
