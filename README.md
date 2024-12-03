# Injustice

## Overview

This project aims to investigate the problem of racial disparities on gun shot likelihood in police-involved deaths in Canada since the year 2000 based on Tracking (in)justice Website open database.
Bayesian logistic regression models conducted at three levels: Canada Overall, Ontario province and Toronto city. The findings of the study shows significance of racial disparities on gun shot likelihood that odds of likelihood blacks is over six times higher than that for whites (OR = 7.13). 


## File Structure

The repo is structured as:

-   `data/raw_data` contains the raw data as obtained from Tracking (in)justice Website open database https://trackinginjustice.ca/explore-the-data.
-   `data/analysis_data` contains the cleaned dataset constructed by this study.
-   `data/simulated_data` contains the a simulation version of the cleaned data set with same size but different data entries, note that, this simulated data might not be appropriate for analysis due to simulation process.
-   `models` contains the fitted  bayesian logistic regression models for three levels - Canada Overall, Ontario province and Toronto city.
-   `other/sketches` contains sketches of data tables and plots.
-   `other/llm_usage` contains details about LLM chat interactions.
-   `other/literature` contains literature.
-   `other/shinyapp` contains a shinyapp dashboard for this project.
-   `paper` contains Quarto document of paper and PDF of paper as well as  reference bibliography.
-   `scripts` contains the R scripts for cleaning data, simulation of data, tests of data, exploratory data analysis, models and a piece of replication sample to replicate the map from simulated data.


## Statement on LLM usage

Some aspects of facts of Canada were written with the help of ZeroGPT and the entire chat history is available in other/llm_usage/usage.txt.



