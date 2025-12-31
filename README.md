# (In)sensitivity to surface-level heuristics: A case from Turkish verbal attractors

This repository contains the data, analysis scripts, and manuscript source materials for the paper "(In)sensitivity to surface-level heuristics: A case from Turkish verbal attractors".

## Abstract

Linguistic illusion literature debates what information accesses memory representations. Prior work tests whether structural, semantic, or discourse cues guide subject-verb dependencies; however, it remains unclear whether native speakers rely on surface level heuristics, such as phonological information during dependency resolution. 

We test whether phonological overlap or association with controllerhood elicits erroneous agreement in Turkish. Turkish provides a critical test: both verbal and nominal elements can surface as subjects and the plural morpheme `-lAr` marks number in both of them, but only nominal plural `-lAr` controls verbal agreement. Two speeded acceptability studies show no attraction from plural-marked verbs but robust attraction from genitive plural nouns. We report a first-of-its-kind dissociation under minimal manipulation: verbal attractors that can surface as subjects yet cannot control agreement do not induce attraction, whereas genitive plural nouns—which can be subjects and control in other environments—do. This pattern constrains retrieval processes by tying attraction to abstract controller features rather than surface phonology.

## Repository Structure

*   **`submission_package/`**: Contains the anonymized LaTeX manuscript and compiled PDF for the submission.
*   **`hsp/`**: R scripts used for statistical analysis used in HSP submission.
    *   `hsp.R`, `hsp_nested.R`, `hsp_another.R`: Main analysis scripts.
    *   `002_functions.R`: Helper functions.
*   **`data/`**: Experimental result data files.
*   **`models/`**: Saved Bayesian model objects (`.rds` files) to avoid re-running computationally expensive models.
*   **`figures/`**: Generated plots and figures referenced in the paper.
*   **`paper/`**: Quarto source files for the paper, it also includes the statistical analysis in the paper.qmd file.

## Reproduction

To reproduce the analysis, ensure relevant R packages are installed and run the scripts in the `hsp/` directory as well as the `paper.qmd` file in the `paper/` directory. Note that the models are pre-saved in `models/` to facilitate quick inspection of results.

## Word Count

Approximate word count of the manuscript: 3912 words.
