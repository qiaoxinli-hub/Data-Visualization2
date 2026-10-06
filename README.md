# Gender Labor Force Participation Gaps and Economic Development

## Overview

This project examines how gender differences in labor force participation have changed across countries over the past three decades and how these differences are related to economic development.

The main question is:

> **How does economic development relate to the gender labor force participation gap, and why do countries with similar income levels experience very different outcomes?**

The analysis uses World Bank data from 1990 to 2024 and combines country-level trends, cross-sectional comparisons, and income-group averages. The results are interpreted using the **U-shaped Female Labor Force Participation Hypothesis**, which suggests that the relationship between economic development and female labor supply may be non-linear.

## Key Findings

Three main patterns emerge from the analysis:

1. **Gender participation gaps have generally narrowed in higher-income economies.**
   Countries such as Germany, the United Kingdom, Japan, and South Korea experienced substantial declines in their gender participation gaps between 1990 and 2024.

2. **GDP per capita is negatively associated with the gender gap, but the relationship is far from uniform.**
   The 2024 cross-country data show a correlation of **-0.28**. Higher-income countries tend to have smaller gaps, but countries with similar income levels can have very different outcomes.

3. **The development process is non-linear.**
   Some low-income countries have relatively small gender gaps because women participate in agricultural, family, or informal work out of economic necessity. A small gap therefore does not necessarily imply greater gender equality or better economic opportunities for women.

Overall, the findings suggest that **economic development matters, but GDP alone cannot explain gender labor force participation. Institutions, labor market structures, and social norms also play an important role.**

## Data

The analysis uses indicators from the **World Bank World Development Indicators (WDI)**.

The main variables are:

* **Female labor force participation rate**
* **Male labor force participation rate**
* **GDP per capita**
* **Country income group**
* **year**

The gender labor force participation gap is calculated as:

$$
\text{Gender Gap} =
\text{Male Labor Force Participation Rate}-
\text{Female Labor Force Participation Rate}
$$

A larger value indicates a larger difference between male and female labor force participation.

The analysis covers countries with available data from **1990 to 2024**.

## Methods

The project uses descriptive data analysis and visualization rather than causal estimation.

Three types of comparisons are used:

### 1. Country-level trends

The first figure tracks the gender participation gap over time for six major economies:

* China
* Germany
* Japan
* Republic of Korea
* United Kingdom
* United States

This allows us to compare how gender gaps have evolved within individual countries.

### 2. Cross-country comparison

The second figure uses 2024 data to examine the relationship between:

* GDP per capita
* Gender labor force participation gap

A fitted linear regression line is included to show the overall direction of the relationship. The correlation coefficient is also calculated to summarize the strength of the association.

### 3. Income-group comparison

The third figure aggregates countries into four World Bank income groups:

* High income
* Upper middle income
* Lower middle income
* Low income

This provides a broader view of how gender participation gaps have changed at different stages of economic development.

## Main Figures

### Figure 1: Gender Participation Gap Over Time

Shows changes in the gender labor force participation gap from 1990 to 2024 across six selected economies.

### Figure 2: GDP per Capita and Gender Participation Gap

Shows the cross-sectional relationship between GDP per capita and the gender participation gap across countries in 2024.

### Figure 3: Gender Gap by Income Group

Shows the average gender participation gap over time for four World Bank income groups.

## Interpretation

The findings are broadly consistent with the **U-shaped hypothesis of female labor force participation**.

In poorer agricultural economies, women may have relatively high labor force participation because household income is low and family labor is economically necessary. As economies industrialize, women may leave agricultural or family work before gaining access to formal employment, potentially widening the gender gap. At higher levels of development, improvements in education, employment opportunities, childcare, and labor market institutions can contribute to higher female participation and a narrower gender gap.

However, the data also show that economic development does not produce the same outcome everywhere. China's relatively small gender participation gap in the early 1990s, despite its lower level of economic development at the time, illustrates how **historical labor policies and social institutions can shape female labor supply alongside economic conditions**.


## Repository Structure

```text
blog-post-4-gender-gap/
├── README.md
├── data/
│   ├── raw/
│   │   └── wdi_gender_labor_raw.csv
│   └── processed/
│       ├── gender_labor_wdi_clean.csv
│       ├── selected_country_trends.csv
│       ├── cross_section_2024.csv
│       ├── gender_gap_change_1990_2024.csv
│       └── selected_gap_change_1990_2024.csv
├── src/
│   ├── 01_data_processing.R
│   └── 02_data_visualization.R
└── results/
    └── figures/
        ├── figure1.png
        ├── figure2.png
        └── figure3.png
```

## Reproducibility

All data cleaning, transformation, analysis, and figure generation are performed programmatically in **R**.

To reproduce the analysis:

1. Obtain the raw World Bank data and place it in `data/raw/`.
2. Run `data_processing.R` to clean and transform the data.
3. Run `data_visualization.R` to generate the three figures.
4. The processed datasets will be stored in `data/processed/`, and the figures will be saved in `results/figures/`.

Because the analysis is fully scripted, the figures can be regenerated from the underlying data without manual editing.

## Conclusion

The evidence suggests that economic development is generally associated with smaller gender labor force participation gaps, particularly among higher-income economies. However, the large variation across countries shows that **income alone is not enough to explain female labor force participation**.

Understanding gender gaps therefore requires looking beyond GDP to the broader institutional, economic, and social environment in which women make labor market decisions.

