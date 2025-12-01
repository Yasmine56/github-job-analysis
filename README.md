# github-job-analysis
Analysis of public GitHub projects (2025) and job market trends using web scraping and R Shiny dashboards.

# GitHub × Welcome to the Jungle × Google Trends Analysis 

This repository contains an R Shiny application to analyze whether GitHub projects created in 2025 reflect the skills demanded on the job market, as represented by job postings on Welcome to the Jungle (WTTJ). It also integrates Google Trends data to compare:

- Popularity of programming languages among developers
- Languages requested by recruiters
- Public interest via search trends

## Application

- `dashboard.R` – Generates a dashboard which analyzes whether public projects created on GitHub in 2025 reflect the skills required in the job market, as represented by the offers published on the Welcome to the Jungle platform. 

The interactive Shiny app allows you to visualize:

- Histograms of programming languages on GitHub
- Languages demanded in job postings
- Temporal evolution
- Cross-visualizations between GitHub, WTTJ, and Google Trends
- Analysis and conclusion

Main file: app/dashboard.R

## Dependencies

- R >= 4.3.1
- Shiny
- dplyr
- ggplot2

You can install the required packages in R with :

```bash
install.packages(c("shiny", "dplyr", "ggplot2"))
```

## Usage

```bash
dashboard.R
```

## Notes

- Webscrapping python scripts are not included. 


## Author

[Yasmine Aissa / Yasmine56] 
