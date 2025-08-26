# Text-Based Economic Forecasting with Topics, Sentiment, and Uncertainty  

This repository contains the code and supporting materials for the paper *Text-Based Economic Forecasting with Topics, Sentiment, and Uncertainty* (Okuneva, 2025).  
The project examines whether text-based indicators extracted from a large corpus of German news articles can improve nowcasts of GDP, investment, and consumption.  

## Repository Structure  

- **`data/`**  
  Construction of real-time vintages for the out-of-sample forecasting experiment,  
  as well as correlation analysis between text-based indicators and macroeconomic series.  

- **`dfm/`**  
  Dynamic factor model (DFM) experiments for the out-of-sample forecasting exercise.  

- **`topics/`**  
  Estimation of unadjusted topics via Latent Dirichlet Allocation (LDA).  

- **`sentiment/`**  
  Adjustment of topics using sentiment lexicons, including BPW, SentiWS, and the Business Cycle Conditions (BCC) sentiment measure.  

- **`uncertainty/`**  
  Adjustment of topics based on the share of uncertainty-related terms.  

## Reference  

If you use this code, please cite:  

> Okuneva, M. (2025). *Text-Based Economic Forecasting with Topics, Sentiment, and Uncertainty*.  
