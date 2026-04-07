# Cor Coffee Spring Syrup Analysis (Feb - Mar 2026)

This repository contains a data-driven analysis of seasonal syrup sales at **Cor Coffee**, a non-profit Catholic coffee shop in Chicago. The project evaluates the performance and customer reception of three spring syrups: **Ube**, **Lavender**, and **Pistachio**.

## Project Overview
Using transaction data from February and March 2026, this study answers key business questions regarding menu optimization, customer preferences, and revenue impact.

### Key Research Questions
* Which seasonal syrup is the most popular?
* Is there a significant relationship between syrup choice and milk or temperature preference?
* How do spring syrups contribute to total revenue?
* Can customer behavior (like previous orders) predict which syrup they will choose?

---

## Tech Stack
* **Language:** R
* **Libraries:**
  * `tidyverse`: Data manipulation and visualization
  * `patchwork`: Combining multiple plots for comparative analysis
  * `nnet`: Statistical modeling (Multinomial Logistic Regression)
* **Data Source:** Square Point-of-Sale (POS) exports (~3,002 observations)

---

## Methodology
The analysis followed a rigorous data science workflow:
1. **Data Cleaning:** Extracted specific modifiers (syrups, milk types) from raw POS strings and converted currency strings into numeric data.
2. **Exploratory Data Analysis (EDA):** Visualized sales volume by date, time of day, and drink category.
3. **Statistical Testing:**
   * **Fisher’s Exact Test:** To determine if milk choice is independent of syrup flavor.
   * **Chi-Square Test:** To evaluate the relationship between drink temperature (Hot vs. Iced) and syrup choice.
   * **ANOVA/Kruskal-Wallis:** To compare transaction values between syrup and non-syrup orders.
4. **Predictive Modeling:** Built a Multinomial Logistic Regression model to predict customer syrup selection based on order characteristics.

---

## Key Findings
* **The "Ube" Effect:** Ube was the most versatile syrup, appearing in 11 different drink types. It showed the highest growth rate, peaking at 30 orders in a single day.
* **Temperature Trends:** Customers overwhelmingly preferred **Iced** preparations (>60% across all spring syrups).
* **Milk Independence:** A p-value of **0.4015** from Fisher's Exact Test indicates that a customer's choice of milk (Whole, Oat, etc.) is independent of the syrup flavor they choose.
* **Revenue Impact:** Seasonal syrups accounted for **15.4% of total revenue** during the period. Orders with these syrups had a significantly higher mean transaction value.
* **Customer Retention:** While Lavender is a recurring favorite, its retention rate has slightly declined as customers migrated to the new Ube flavor.

---

## Recommendations
1. **Ube as a Staple:** Due to its massive popularity and consistent growth, Ube should be considered for the permanent menu.
2. **Strategic Rotation:** Since Pistachio and Lavender share similar sales profiles, they should be rotated sequentially rather than sold simultaneously to avoid "cannibalizing" each other's sales.
3. **Incentive Programs:** Implement a "Double Punch" reward on seasonal drinks to increase retention and encourage customers to try new flavors.

---

