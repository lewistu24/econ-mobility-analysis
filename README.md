# Economic Mobility Analysis

An analysis of economic mobility and social capital across census tracts, with a focus on Belmont and Middlesex County in Massachusetts.

## Project Structure

```
├── data/           # Stata datasets (. dta files)
├── analysis/       # R Markdown analysis files
├── figures/        # Generated visualizations
└── docs/           # Final report (PDF)
```

## Data Sources

- `atlas.dta` - Opportunity Atlas data
- `social_capital_zip. dta` - Social capital metrics by ZIP code
- `zip_tracts_xwalk.dta` - ZIP code to census tract crosswalk

## Analysis

The analysis is split into two parts:
- **Part 1**: Initial data exploration and hypothesis generation
- **Part 2**: Hypothesis testing and conclusion

## Requirements

- R (with `haven`, `tidyverse`, and related packages)

## Usage

Open and knit the `. Rmd` files in RStudio to reproduce the analysis.
