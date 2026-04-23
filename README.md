# epi_changepoints

Repository for data and code for the article: "Detecting changepoints in dynamical systems: Modelling time-varying transmission of seasonal influenza", Oza, AN, O'Brien, KM, Gleeson, JP; PNAS, 2026

Currently this repository shows how to intialise the data in an R project. Data courtesy of HSE-Health Protection Surveillance Centre. See note on the data below.

### How to run

After downloading/cloning, go to the script 101_examples_import_data.R and source. object `df_infected_all_seasons` should have the data as used in the article.

Regarding Git or R projects, you may find the following resources helpful:

-   **Downloading or cloning repositories from GitHub (GitHub Docs)**\
    <https://docs.github.com/en/get-started/start-your-journey/downloading-files-from-github>

-   **Using GitHub with R and RStudio (Happy Git and GitHub for the useR)**\
    <https://happygitwithr.com/rstudio-git-github.html>

-   **Cloning a GitHub repository into RStudio (step-by-step)**\
    <https://oxford-ihtm.io/ihtm-handbook/clone-repository-rstudio.html>

### Note on the data:

De-identified laboratory confirmed influenza case-based data notified on Ireland’s Computerised Infectious Disease Reporting (CIDR) system as collated by Health Service Executive - Health Protection Surveillance Centre (HSE-HPSC), were extracted on 05/05/2025. Only data from samples that were taken from hospital inpatients were considered for this study. Cases that were confirmed as influenza A only were included for influenza season 2019/2020 (pre-COVID-19 pandemic) and three seasons following the COVID-19 pandemic (2022/2023, 2023/2024, 2024/2025) are included. The period under consideration started on Sunday of week 37 (approximately mid-September) in the epidemiological calendar and extended to the end of week 9 (approximately late February/early March) of the following calendar year (Saturday). Thus, each season had days numbered 1 to 175. Daily reports were created by seven-day moving averages of case numbers by day for each season; data one week prior to day 1 were used as burn-in period and are not included in this repository (or the analysis in the paper). Furthermore, reports were rounded to integer values. In addition, for the purposes of this repository, any reported numbers fewer than five were removed from the 'raw' series. The script provided in this repository imputes integer values of 0 to 4 for any missing data.

HSE-HPSC. Respiratory Virus Notification Data Hub. 2025 [cited 2025 16/10/2025]; Available from: <https://respiratoryvirus.hpsc.ie/>.
