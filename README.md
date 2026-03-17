# Housing Market Dashboard (R Shiny)

Interactive dashboard for analyzing the Polish housing market using real-world data from NBP and GUS.

## Overview

This project delivers an end-to-end data analysis pipeline and interactive tool for exploring housing prices, affordability, and macroeconomic factors across Poland.

## Features

* Analysis of housing prices on primary and secondary markets
* Inflation-adjusted price calculations
* Housing affordability indicator (months of work required per m²)
* Regional analysis using geospatial data
* Interactive visualizations (time series, maps, comparisons)
* Analysis of relationships between housing prices, wages, inflation, and migration

## Data Usage

All data used in this project comes from publicly available sources:

* GUS (Statistics Poland)
* NBP (National Bank of Poland)
* PRG (administrative boundaries)

Data is used for educational and non-commercial purposes only.

## Tech Stack

* R (Shiny, ggplot2, sf)
* Data processing and feature engineering
* Interactive UI with reactive components

## How to Run

1. Clone the repository
2. Open the project in RStudio
3. Run `app.R`

## Project Structure

```
├── app.R          # Shiny application
├── data.R         # Data processing and feature engineering
├── data/          # Raw datasets
```

## Author

Iwo Ceglarek
[GitHub](https://github.com/iwoceglarek)
[LinkedIn](https://linkedin.com/in/iwoceglarek/)
