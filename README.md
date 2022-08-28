# Prediction of Audit Fees
_An ACCT337 Statistical Programming Project_

## Context
Audit Fees refer to the amount that the Big 4 Accounting companies charge to their cilents for their Audit Services.

## Files in this Repo
### 📜 1. <u>[Final R Project Script.R](https://github.com/cherylperyl/prediction-of-audit-fees/blob/main/Final%20R%20Project%20Script.R)</u>
In this file, the given data was analysed and meticulously cleaned. Following that, Regression Analysis was performed to see which factors would help to give a more accurate prediction of Audit Fees. Techniques used:
1. Feature Engineering
2. Feature Transformation
3. Forward/ Backward Selection (for Regression)
4. Stepwise Regression (for Regression)
5. Classification Tree

### 📂 2. <u>[Text Analysis folder](https://github.com/cherylperyl/prediction-of-audit-fees/tree/main/text%20analysis)</u>
In this file, Naïve Bayes algorithm was used to analyse the 'Company Description' coloumn. Steps:
1. Create dummy column for each word occurence (Data was too large for R to handle so this step is performed by the python script)
2. Naïve Bayes algorithmis then applied to the word counts data to find out which words generally appear in companies with Audit Fees below/above the 50th percentile.

### 🤾🏻‍♀️ 3. <u>[Playground](https://github.com/cherylperyl/prediction-of-audit-fees/blob/main/playground.ipynb) and [Visualisations](https://github.com/cherylperyl/prediction-of-audit-fees/tree/main/visualisations)</u>
Data analysis and visualisations files.

### 📽 4. <u>[Final Presentation Slides](https://github.com/cherylperyl/prediction-of-audit-fees/blob/main/Final%20R%20Project%20Slide%20Deck.pdf)</u>
Brings our entire project together into a succint pitch!