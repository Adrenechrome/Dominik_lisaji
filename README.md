# Dominik lisaji

R Shiny application for exploring the Idrija dataset stored in Excel workbooks. The app loads `vsi_podatki_3_changes.xlsx` and provides interactive views for linear regression, variable distributions, sample comparisons, ANOVA, and Tukey post-hoc results.

## Features

- Interactive scatter plots for numeric variable pairs.
- Optional linear or LOESS regression lines with confidence intervals.
- Distribution plots for selected response and predictor variables.
- Per-location regression summaries using R-squared and p-value coloring.
- Boxplots for comparing numeric variables across location/group factors.
- Tukey HSD comparison plots, with an option to show all comparisons or only statistically significant ones.

## Project Structure

```text
.
├── app.R                       # Shiny application
├── vsi_podatki_3_changes.xlsx  # Main workbook used by the app
├── vsi_podatki_2.xlsx          # Additional dataset/workbook
├── vsi_podatki.xlsx            # Additional dataset/workbook
├── vsi_pod.xlsx                # Additional dataset/workbook
├── Dominik_lisaji.Rproj        # RStudio project file
└── .gitignore
```

## Requirements

Use a recent R installation with the following packages installed:

```r
install.packages(c(
  "shiny",
  "shinymanager",
  "ggplot2",
  "dplyr",
  "lubridate",
  "plotly",
  "fmsb",
  "DT",
  "tidyverse",
  "shinythemes",
  "shinyBS",
  "shinydashboard",
  "shinyWidgets",
  "openxlsx"
))
```

## Running the App

From RStudio:

1. Open `Dominik_lisaji.Rproj`.
2. Open `app.R`.
3. Click **Run App**.

From an R console in the project directory:

```r
shiny::runApp()
```

The app expects `vsi_podatki_3_changes.xlsx` to be present in the project root. If the workbook is renamed or moved, update the `read.xlsx(...)` call near the top of `app.R`.

## Main Views

### Linearna regresija

Use this tab to choose a numeric response variable, a numeric predictor variable, and an optional grouping/location column. The tab displays distribution plots, a scatter plot, optional regression smoothing, and a regression summary plot by group.

### Primerjava vzorcev

Use this tab to compare a numeric variable across selected groups. The tab displays a boxplot and Tukey HSD comparison plot based on an ANOVA model.

## Development Notes

- The application is currently implemented as a single-file Shiny app in `app.R`.
- UI labels are primarily in Slovenian.
- Generated RStudio state is ignored through `.gitignore`.
