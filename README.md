#Broad Insititute Data Science Summer 2024 Internship 
# TCGA & DepMap expression and order parameter analysis

This project involves training and evaluating models at both the lineage and subtype levels using TCGA and DepMap datasets. The primary goal is to evaluate wether we can  use TCGA to classify depmap?

## Key Features

### Lineage-Level Analysis
- **Model Training**: Trained models at the lineage level.
- **OncoTree Annotation**: Updated the `name` column in the Oncotree DataFrame by appending tissue information for names with more than one unique OncoTree code.
- **Subtype Corrections**: Refined subtypes within each lineage.
- **Correlation Threshold**: Identified subtypes with a correlation > 0.9.

### Getting Scores and Median Correlation
- **OncoTree Code Assignment**: Filled in OncoTree codes for samples with missing codes based on subtype names.
- **Derived Lineage and Subtype**: Added derived lineage and subtype based on OncoTree codes.
- **Subtype Renaming**: Renamed subtypes containing 'adeno' or 'squamous' as 'adeno' or 'squamous', excluding lung, colon, and prostate adenocarcinomas.
- **Model Training**: Trained models at the derived subtype level and calculated median correlations and scores for the top subtypes.

### High Correlation Analysis
- **Correlation Threshold**: Filtered subtypes with a correlation > 0.91 within the same lineage.
- **Lineage Filtering**: Excluded subtypes with more than one unique lineage.
- **Subtype Mapping**: Mapped highly correlated subtypes.
- **Top 5 Subtypes**: Identified the top 5 subtypes for each annotated subtype, updated `new_subtype` in `tcga_meta` by finding common parents with the lowest level.

### Subtype-Level Model
- **Model Training**: Trained models at the subtype level.
- **Median Correlation**: Calculated median correlations and scores for annotated subtypes.

### DepMap Mode
- **Model Training**: Trained models using TCGA expressions at the subtype level.
- **DepMap Subtype Renaming**: Aligned DepMap subtype names with those in TCGA, mapping squamous and adenoid subtypes while excluding lung, colon, and prostate adenocarcinomas.
- **Brain Lineage Mapping**: Mapped all subtypes in the brain lineage except medulloblastoma.
- **Subtype Correlations**: Found correlations between subtypes in both TCGA and DepMap.

### DepMap and TCGA Correlation Analysis
- **Correlation Analysis**: Identified correlations of TCGA subtypes that are also present in DepMap.
- **Median Correlation Comparison**: Compared median correlations of unique subtypes in both TCGA and DepMap to determine a threshold for DepMap subtypes.
- **High-Correlation Subtypes**: Identified DepMap subtypes with a correlation > 0.88.

### DepMap Subtype Model
- **Model Training**: Trained models with TCGA at the subtype level and evaluated them using DepMap.
- **Projections**: Obtained projections and filtered subtypes with more than 10 samples in DepMap and more than 25 samples in TCGA.
- **Histogram Analysis**: Graphed histograms of subtype scores.

### DepMap Lineage Model
- **Model Training**: Trained models with TCGA at the lineage level and evaluated them using DepMap.
- **Projections**: Obtained projections and filtered subtypes with more than 10 samples in DepMap and more than 25 samples in TCGA.
- **Boxplot Analysis**: Created boxplots comparing subtype and lineage scores.

