# Computational Models of Cue-Based Retrieval

This directory contains R scripts implementing ACT-R cue-based retrieval models to simulate agreement attraction effects in Czech and Slovak, following the experimental design in Lacina et al. (2025, *Journal of Memory and Language*), "Only case-syncretic nouns attract: Czech and Slovak gender agreement attraction in comprehension."

## Background

The paper investigates **gender agreement attraction** in Czech/Slovak comprehension, where an intervening attractor noun can interfere with subject-verb gender agreement processing. A key manipulation is whether the attractor's case form is **syncretic** (ambiguous between nominative and another case) or **non-syncretic** (unambiguously marked). The models here use the **interACT** framework (an extension of ACT-R's declarative memory retrieval) to generate predictions for six experimental conditions crossing verb grammaticality (grammatical vs. ungrammatical) with attractor type (gender-matching, non-syncretic mismatch, syncretic mismatch).

## Files

### `interACT_2features.R`
Core model engine using **2 retrieval cues** (gender + case). Implements:
- ACT-R global parameters (latency factor, decay, noise, mismatch penalty, spreading activation, etc.)
- Cue weighting functions with normalization for 2-cue setup
- A 6-condition design encoding target and distractor match vectors as pairs `(gender, case)`
- A `distortion()` function supporting optional feature percolation (probabilistic misrepresentation of features)
- `create_param_matrix()`: builds a full factorial parameter matrix across conditions and iterations
- `run()`: the main retrieval engine — computes base-level activations, match quality, spreading activation, mismatch penalties, final activations, retrieval latencies, and accuracy
- Low-level ACT-R functions: `activation()`, `spreading_act()`, `match_quality()`, `latency()`, noise generation, etc.

### `interACT_3features.R`
Same architecture as the 2-feature model but uses **3 retrieval cues** (syntactic category + gender + case). Key differences:
- Match vectors are triplets `(syntactic, gender, case)` — the syntactic cue always matches the target and always mismatches the distractor
- Cue weight normalization is adjusted for 3 cues (weights sum to 3 instead of 2)
- `match_quality()` uses a 3-element confusion matrix
- The `weights` matrix in `run()` selects 3 columns (`weights1`, `weights2`, `weights3`)

### `Retrieval-only.R`
Sampling script that generates model predictions by:
1. Sourcing the 2-feature (or optionally 3-feature) model engine
2. Setting ACT-R parameters to specific values (e.g., `mp = 0.15`, `mas = 1.5`, `ans = 0.2`, `bll = 0.5`)
3. Drawing 2,000 samples of the latency factor from a truncated normal prior (`mean = 0.15`, `sd = 0.05`, bounds `[0.05, 1]`)
4. For each sample, running the model across all 6 conditions (1,000 iterations each) and recording predicted reading times
5. Saving the resulting prediction samples to an `.Rda` file (e.g., `ACTR_predictions_2features_full_match_mp0.15.Rda`)

### `simulations.R`
Post-processing and visualization of model predictions:
- Loads saved `.Rda` prediction files for both 2-feature and 3-feature models under full-match and half-match assumptions for syncretic case
- Reshapes data into long format with labeled conditions (Grammatical/Ungrammatical × Matching gender / Mismatching non-syncretic / Mismatching syncretic)
- Adds a 250 ms offset for response planning and button pressing
- Generates pointrange plots of condition means (saved as `img/simulations_2features.pdf` and `img/simulations_3features.pdf`)
- Computes predicted ungrammaticality effects (d−a, e−b, f−c), attraction effect differences, and inhibitory interference contrasts using `tidybayes::mean_qi()`

### `estimate_plotting.R`
Comparative visualization script that:
- Reads pre-computed estimates from `data/predictions_plotting_long.csv`
- Plots model predictions alongside empirical results (Experiments 1–3) as a faceted pointrange plot by grammaticality
- Produces two output plots:
  - `img/Estimate_comparison.pdf`: combined empirical + simulation estimates
  - `img/Predictions.pdf`: simulation predictions only (2- and 3-feature models × full/half match)

## Six Experimental Conditions

| Condition | Verb          | Attractor Type              | Example (Czech)                        |
|-----------|---------------|-----------------------------|----------------------------------------|
| a         | Grammatical   | Gender match, case match    | Náklad na parník překvapivě byl        |
| b         | Grammatical   | Gender mismatch, non-syncr. | Náklad na střech-u překvapivě byl      |
| c         | Grammatical   | Gender mismatch, syncretic  | Náklad na loď překvapivě byl           |
| d         | Ungrammatical | Gender match, case match    | *Náklad na parník překvapivě byl-a     |
| e         | Ungrammatical | Gender mismatch, non-syncr. | *Náklad na střech-u překvapivě byl-a   |
| f         | Ungrammatical | Gender mismatch, syncretic  | *Náklad na loď překvapivě byl-a        |

## Syncretic Case Modeling

Syncretic nouns have identical forms for nominative and another case (e.g., accusative), making their case marking ambiguous. Two assumptions are modeled:
- **Full match**: syncretic case is treated as a full match to the nominative retrieval cue (match value = 1)
- **Half match**: syncretic case is treated as a partial match (match value = 0.5)

---

## Phonological Similarity Extension

The following files extend the cue-based retrieval framework to model **phonological similarity** effects on number agreement attraction. The idea: a word phonologically similar to another word with a different number feature (e.g., *cruise* [sg] ~ *crews* [pl]) partially activates the homophone's number, creating retrieval interference analogous to case syncretism.

### `interACT_2features_phon.R`
Model engine using **2 retrieval cues** (structural role + number) with an **8-condition design** (2×2×2):
- **Grammaticality**: grammatical (verb agrees with subject) vs. ungrammatical
- **Attractor number**: matches vs. mismatches the subject's number
- **Phonological similarity**: attractor is/isn't phonologically similar to an opposite-number word

The `phon_activation` parameter controls how strongly the homophone's number feature is activated:
- `1` = full activation (number mismatch → full match, like full case syncretism)
- `0.5` = half activation (number mismatch → partial match, like half syncretism)
- `0` = no activation (no phonological effect)

Phonological similarity is modeled **asymmetrically** (like syncretism): it only increases match values for distractors that would otherwise mismatch the number cue. Distractors that already match are unaffected.

#### Eight conditions

| Cond | Grammaticality | Attractor # | Phon Sim | Target match | Distractor match | Example |
|------|---------------|-------------|----------|--------------|------------------|---------|
| a | Grammatical | Match | No | (1,1) | (0,1) | "The key to the table was..." |
| b | Grammatical | Match | Yes | (1,1) | (0,1) | "The key to the cruise was..." |
| c | Grammatical | Mismatch | No | (1,1) | (0,0) | "The key to the tables was..." |
| d | Grammatical | Mismatch | Yes | (1,1) | (0,φ) | "The key to the crews was..." |
| e | Ungrammatical | Match | No | (1,0) | (0,0) | "The key to the table were..." |
| f | Ungrammatical | Match | Yes | (1,0) | (0,φ) | "The key to the cruise were..." |
| g | Ungrammatical | Mismatch | No | (1,0) | (0,1) | "The key to the tables were..." |
| h | Ungrammatical | Mismatch | Yes | (1,0) | (0,1) | "The key to the crews were..." |

φ = `phon_activation` (1 for full, 0.5 for half). Conditions b and h are unchanged by phon sim because the distractor already matches the number cue.

### `Retrieval-phon.R`
Sampling script (analogous to `Retrieval-only.R`) that:
1. Sources `interACT_2features_phon.R`
2. Samples 2,000 latency factor values from a truncated normal prior
3. Runs the model across all 8 conditions for each sample
4. Saves predictions for both full and half phonological activation to `.Rda` files

### `fit_slioussar.R`
Generates interACT predictions for **error rates** from Slioussar (2018, JML) "Forms and features: The role of syncretism in number agreement attraction," following the Lacina et al. (2025) methodology. Targets:

| Attractor | Exp 1 (Prod) | Exp 2 (Comp) | Syncretism |
|---|---|---|---|
| Acc.Sg | 0% | 4.8% | No (baseline) |
| Acc.Pl (=Nom.Pl) | 15.3% | 23.4% | Yes (number + form) |
| Gen.Sg (=Nom.Pl) | 4.1% | 14.5% | Yes (form only!) |
| Gen.Pl | 0.6% | 9.5% | No (number only) |

The model's `Miss` rate (distractor retrieved instead of target) maps to the agreement error rate. The script:
1. **Fixes all ACT-R parameters** to Engelmann et al. (2019) defaults (`mp=0.15`, `mas=1.5`, `ans=0.2`, `bll=0.5`, `rth=-1.5`) — no parameter fitting
2. **Samples only `lf`** (latency factor) from a truncated normal prior (`mean=0.15`, `sd=0.05`, bounds `[0.05, 1]`), 2000 draws
3. Defines a 4-condition design matching Slioussar's singular-head conditions, where syncretism is modeled as partial structural/nominative cue match
4. Runs under **two structural assumptions**: `phon_activation = 1.0` (full syncretism) and `0.5` (half syncretism)
5. Compares predicted error distributions to Slioussar's observed rates via RMSD
6. Produces two plots: (a) observed vs. predicted error rates for both assumptions, (b) density distributions of predicted error rates across `lf` samples
7. Saves prediction samples and summaries to `forms_features/*.Rda`

### `forms_features/`
Supplementary data from Slioussar (2018): CSV files with reading times by region (Experiments 2–3) and supplementary PDF.

## Dependencies

- `tidyverse` (`dplyr`, `tidyr`, `ggplot2`)
- `tidybayes`
- `LaplacesDemon`
- `truncnorm`
- `parallel`
- `ggh4x`
- A custom theme file sourced from `../helper/mytheme.R`

## Reference

Lacina, R., et al. (2025). Only case-syncretic nouns attract: Czech and Slovak gender agreement attraction in comprehension. *Journal of Memory and Language*, 143, 104623.
