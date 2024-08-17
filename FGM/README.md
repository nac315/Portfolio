# Exploring Factors Determining Support for FGM in Somalia

This repository contains R scripts for analyzing survey data related to Female Genital Mutilation (FGM). The analysis focuses on data cleaning, visualization, and predictive modeling to understand factors associated with FGM practices.

## Table of Contents

1. [Data Cleaning](#data-cleaning)
2. [Data Visualization](#data-visualization)
3. [Predictive Modeling](#predictive-modeling)
4. [Regression Analysis](#regression-analysis)
5. [Model Evaluation](#model-evaluation)
6. [Dependencies](#dependencies)
7. [License](#license)

## Data Cleaning

The data cleaning process involves:
- **Reading Data**: Loading the data from a CSV file.
- **Filtering Data**: Removing entries where FGM status is "No" and those with an age of 12 or younger.
- **Creating Factor Variables**: Converting categorical variables related to location, education level, and support for FGM into factors for analysis.

## Data Visualization

The visualization section includes:
- **Boxplots**: Comparing the age of FGM by support practice and location.
- **Heat Maps**: Visualizing the relationships between education level and support for FGM, and location and support for FGM.
- **Scatterplots with Spline**: Plotting age against support for FGM and fitting a smooth spline to observe trends.

## Predictive Modeling

Various modeling approaches are used to predict support for FGM:
- **Random Forest**: Builds a Random Forest model to identify the most influential predictors.
- **Rpart Decision Trees**: Constructs decision trees with different configurations to evaluate model performance.
- **Conditional Inference Trees (Party Trees)**: Uses Conditional Inference Trees to model and visualize the relationship between predictors and support for FGM.

## Regression Analysis

- **Logistic Regression**: Models the probability of supporting FGM based on predictors such as education level, location, and age. Provides insights into how changes in these predictors influence the likelihood of supporting FGM.

## Model Evaluation

Models are evaluated based on:
- **Accuracy**: Calculating how often the predictions match the actual values.
- **Confusion Matrices**: Comparing predicted values against actual values to assess model performance.

## Dependencies

Ensure the following R packages are installed:
- `randomForest`
- `rpart`
- `party`
- `tidyverse`
- `RColorBrewer`
- `rattle`
- `rpart.plot`
- `stats`


