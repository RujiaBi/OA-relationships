# oa_relationships

# Authors

Friedrich Keppler, Rujia Bi, Juliano Palacios, Olaf Jensen, et al...



# Time line

- 9/1, Preliminary results sent out to group
- 9/15, Deadline for comments/suggestions from larger group
- 10/27, Revised results sent out to group
- 11/3, Hybrid meeting: discuss results and work on ms.
- 12/31, Target submission deadline

# Steps to take

1) Gather time series of total or spawning biomass for all RAM Legacy stocks (**Check**)

2) Identify the closest associated trawl survey-species combination in
FishGlob matching each of these stocks (**Juliano, Zoe, Rujia**)

4) For each stock and each matching year of the survey and biomass data,
calculate two metrics of occupancy:
  - the area of the minimum convex polygon representing all occurrences and
  - the 90th percentile density distribution based on an interpolated map of trawl CPUE. (**Fred**)

5) Gather a variety of taxonomic (e.g., genus, family, and order) and
life history (e.g., age at maturity, habitat (benthic/pelagic), and
trophic level) covariates for each of the stocks.  FishLife would be a
good place to go for this.

6) Fit the model in eq 1 of the proposal: Rt = αBt^ß  where Rt is one of
the two metrics of occupancy in step 3 and Bt is one of the two biomass
metrics in step 1.  t is the year index and alpha and beta are shape
parameters.  I suggest that the base model be a hierarchical Bayesian
model where alpha is shared among stocks and alpha is estimated
independently for each stock.  Variations will include various taxonomic
hierarchies and life history covariates. (**Rujia**)

7) The outputs will include a model comparison table and figures will
include (1) a map figure like Fig 1 in the proposal and (2) parameter
estimates from the best model.

# Data

- RAMLDB stock boundary folder: https://drive.google.com/drive/folders/1MYlnK-9PsaBwcBrQPGpH4lKWaZ7dWrzT?usp=share_link. Please note that the shapefiles are labeled by “assess id”, instead of “stockid”. Please refer to the “ramldb_id_formatted.xlsx” to find the corresponding “stockid”.
