# Online Retail II Data Analysis in R

## Project Overview

This project analyses the Online Retail II transactional dataset using Base R. The workflow covers data quality assessment, data cleaning, exploratory data analysis, customer-level analysis, product analysis, multivariate outlier detection, and validation of the final tidy dataset.

The analysis was developed as a reproducible Base R workflow, with automated outputs including summary tables, CSV files, visualisations, and validation results.

## Objectives

The main objectives of the project are to:

- Assess the quality of the raw transactional data
- Identify missing values, duplicate records, cancellations, and invalid values
- Develop a purpose-aware data cleaning workflow
- Explore transaction patterns over time and across countries
- Analyse customer purchasing behaviour
- Examine product-level sales patterns
- Identify potential multivariate outliers
- Compare different data-cleaning approaches
- Validate the final tidy dataset
- Produce reproducible analysis outputs

## Dataset

The project uses the **Online Retail II** transactional dataset.

The dataset contains retail transaction records with information including:

- Invoice number
- Stock code
- Product description
- Quantity
- Unit price
- Invoice date
- Customer ID
- Country

The raw dataset is not included in this repository.

## Data Quality and Cleaning

The workflow begins with an audit of the raw data and evaluates data quality issues including:

- Missing customer IDs
- Missing product descriptions
- Exact duplicate records
- Cancellation transactions
- Negative quantities
- Zero or negative prices

The cleaning process distinguishes between **sales transactions and cancellation transactions**, allowing cancellations to be retained separately rather than treating all negative quantities as invalid records.

The workflow also creates validation checks to assess the quality of the cleaned data.

## Exploratory Data Analysis

The project performs exploratory analysis of:

- Transaction activity over time
- Monthly transaction patterns
- Country-level activity
- Customer purchasing behaviour
- Customer net spending
- Product sales value
- Product units sold

Visualisations are generated using Base R and saved automatically to the `outputs` directory.

## Customer Analysis

Customer-level analysis is performed for customers with known customer IDs.

The analysis includes:

- Purchase invoice count
- Gross spending
- Gross units purchased
- Product diversity
- Net spending after cancellations
- Net units after cancellations
- Cancellation invoice count
- Recency of customer purchases

Customers with positive net spending are used for further customer analysis.

## Product Analysis

Product-level analysis aggregates sales by stock-code and product-description combinations.

The analysis examines:

- Gross sales value
- Units sold
- Top-performing product combinations
- The contribution of the highest-value product combinations to total sales

## Multivariate Outlier Analysis

The project investigates unusual transaction patterns using two approaches:

1. **Interquartile Range (IQR) rules**
2. **Mahalanobis distance**

The multivariate analysis considers transaction characteristics including:

- Quantity
- Unit price
- Line value
- Purchase hour

A reproducible sample of up to 50,000 valid sales transaction lines is used for the multivariate outlier analysis.

The results compare the number of observations identified by each method and examine the agreement between the approaches.

## Comparison of Data-Cleaning Approaches

The project compares a stricter study-style cleaning approach with the purpose-aware cleaning pipeline developed for the analysis.

The comparison examines:

- Number of transaction lines retained
- Known-customer sales
- Anonymous sales
- Cancellation transactions retained separately
- Gross paid-sales value
- Net transaction value after cancellations
- The treatment of customer returns

This comparison demonstrates how different preprocessing decisions can affect the amount of information retained for analysis.

## Final Dataset Validation

The final tidy dataset is validated using checks including:

- Raw input file size
- Number of final rows
- Number of final columns
- Remaining missing values
- Unknown customer IDs
- Zero or negative prices
- Remaining exact duplicate records

The final tidy dataset is also saved in RDS format.

## Outputs

The `outputs` directory contains generated analysis results, including:

- CSV summary tables
- Data-quality results
- Customer analysis results
- Product analysis results
- Outlier analysis results
- Data-cleaning comparisons
- Validation results
- Text summaries
- Visualisations
- Final tidy dataset output

## Tools and Techniques

- R
- Base R
- Data Cleaning
- Exploratory Data Analysis
- Data Quality Assessment
- Statistical Analysis
- Data Visualisation
- Customer Behaviour Analysis
- Product Analysis
- IQR Outlier Detection
- Mahalanobis Distance
- Data Validation

## Project Structure

```text
OnlineRetailAnalysis/
│
├── README.md
│
├── CW1_online_retail_analysis_baseR.R
│
└── outputs/
    ├── Analysis result files
    ├── Visualisations
    ├── Text summaries
    └── Validation outputs
