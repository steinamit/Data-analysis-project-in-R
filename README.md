# Demand Forecasting in R

> Comparing trend regression, smoothing and Holt-Winters on a 10-year monthly demand series.

![R](https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white)

## Highlights

- Time-series objects at monthly, quarterly and yearly resolution.
- Five forecasting approaches compared on accuracy and a holdout period.
- Decomposition and residual-noise analysis.

## Methods

- Linear trend regression (`tslm`) with accuracy measures
- Moving averages of order 3 and 6
- Simple exponential smoothing with α of 0.1, 0.5, 0.9 and an optimized α
- Holt-Winters with set and optimized parameters, tested on the last 5 periods
- Classical decomposition and residual analysis

## Repository structure

| Path | Description |
|---|---|
| `__DATA_code file.R` | Analysis script |
| `DATA.csv` | Demand series, 120 periods |

## Context

Course assignment, Production Management, Ariel University. Team project by Amit Stein and Noa Rubin.
