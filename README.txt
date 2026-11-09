HumanAIproject_MDA
Supplementary materials for: Does humanizing AI-generated text make it
human? A multidimensional analysis of register in argumentative essays
(anonymised for double-blind review)

======================================================================
1. OVERVIEW
======================================================================

This repository contains the data, scripts and outputs for a
multidimensional analysis (MDA) of argumentative essays in three
conditions:

  - Student:   essays by native-speaker students, from the Louvain
               Corpus of Native English Essays (LOCNESS)
  - GPT-4:     essays generated with GPT-4.1 on the same topics (first
               used in AUTHOR1 et al. 2026; see Section 3.2)
  - Humanised: the GPT-4 essays after processing with an AI
               "humanizer" tool (see Section 3.3)

The three conditions form matched triplets: for each student essay there
is a GPT-4 essay on the same topic and a humanised version of that
GPT-4 essay. Each corpus has 145 essays (435 texts). Dimension scores
are computed over all 435 texts; the condition comparisons use the 432
texts in the 144 complete triplets (see Section 4).

Research questions:
  RQ1  Does humanisation change the register of GPT-4 essays?
  RQ2  Does it move them towards student writing?
  RQ3  Does it change how features co-occur, not only how often
       they occur?

Notes on names:
  - File and folder names use "Humanised"; the paper uses "humanized".
    They refer to the same condition.
  - As in the paper, "GPT-4" is used as a short label for the GPT-4.1
    condition (see Section 3.2). The folder data/texts/GPT4/ and the
    label LOCNESS_GPT refer to the same essays.

The LOCNESS texts are NOT included, because the LOCNESS licence does not
allow redistribution. Only the feature counts derived from them are
included (see Sections 2 and 3.1).

======================================================================
2. FOLDER GUIDE
======================================================================

data/texts/GPT4/
    The GPT-4 essays as plain-text files, one essay per file.

data/texts/Humanised/
    The humanised versions of the GPT-4 essays, one essay per file.

data/mfte_counts/
    MFTE "Statistics" output for each of the three corpora, tagged
    separately with the same MFTE version and settings. Feature counts
    only; no text.
    GPTcorpus_normed.csv
    Humanisedcorpus_normed.csv
    Human_LOCNESS_normed.csv
        Feature frequencies normalised per 100 words (input to
        feature_selection_HumanAIproject_V2.R).
    GPTcorpus_counts_raw.csv
    Humanisedcorpus_counts_raw.csv
    HumanLOCNESS_counts_raw.csv
        Raw counts, including word counts per text (input to
        Text-length-impact.R).

data/pooled/
    EFA-readyHumanAIprojectRegistersADDED_V2.csv
        The three corpora combined, after feature selection; one row per
        text (435 texts), with filename and Register columns and the 80
        features
        that passed feature selection (Section 5, Step 3).
    dimension_scores_HumanAIproject_V2.csv
        Scores on dimensions D1-D6 for all 435 texts in the pooled
        file.

data/crosswalk/
    item_crosswalk_HumanAIproject.csv
        Links each text (filename + Register) to its triplet (item_id).
        See Section 4.
    crosswalk_incomplete_items_HumanAIproject.csv
        Items without a complete triplet (excluded from all analyses).

scripts/
    R scripts for every step after tagging. See Section 6 for the run
    order.

jasp/
    JASP files for the exploratory factor analysis (factor solutions
    and the final six-factor model).
    EFA-6F-HumanAIproject_V2.jasp
        The JASP analysis file for the six-factor EFA.
    6F-Forced-V2.xlsx
        [CONFIRM description, e.g. factor loadings of the forced
        six-factor solution]
    Parellel Analysis table for screeplot.csv
        The parallel analysis table exported from JASP (input to
        scree_plot.R).

results/
    Output tables produced by the scripts.

    Feature selection (Step 3)
      surviving_cols_final_HumanAIproject.csv
          The 80 features remaining after NZV and KMO/MSA screening.
      removed_nzv_HumanAIproject.csv
          Features removed for near-zero variance.
      removed_low_msa_HumanAIproject.csv
          Features removed for low sampling adequacy, in order of
          removal, with their MSA at removal.
      overall_KMO_HumanAIproject.csv
          Overall KMO of the final feature set.

    Factor loadings (Step 4)
      in jasp/6F-Forced-V2.xlsx

    Dimension scores (Step 5)
      EFA_zscored_HumanAIproject_V2.csv
      all_shapiro_results_HumanAIproject-6D_V2.csv

    Condition comparisons (Step 6)
      friedman_DIMENSIONS_HumanAIproject_V2.csv
      wilcoxon_posthoc_DIMENSIONS_HumanAIproject_V2.csv
      wilcoxon_effsizes_DIMENSIONS_HumanAIproject_V2.csv
      wilcoxon_combined_table4_HumanAIproject_V2.csv
      condition_medians_HumanAIproject_V2.csv
      Table4_HumanAIproject_V2.csv  (Table 4 in the paper)
      friedman_wilcoxon_combined_table_HumanAIproject-V2.csv

    Further analyses (Step 7)
      feature_medians_D1_D2_D4_D5_HumanAIproject_V2.csv
      matrix_similarity_pairs_HumanAIproject_2026-10-19.csv
      matrix_similarity_differences_HumanAIproject_2026-10-19.csv
      matrix_similarity_splithalf_HumanAIproject_2026-10-19.csv
      length_effect_correlations_HumanAIproject.csv
      length_effect_triplet_data_HumanAIproject.csv
      representative_triplets_by_dimension_HumanAIproject_V2.csv
      representative_triplets_multidimension_HumanAIproject_V2.csv
      top_bottom5_by_dimension_MATCHED_HumanAIproject_V2.xlsx

results/figures/
    boxplot_v2_D1.png ... boxplot_v2_D6.png
        Dimension scores by condition, with compact letters from the
        Wilcoxon post-hoc tests.
    figure_captions_HumanAIproject_v2.txt
        Caption drafts for the boxplots.
    scree_plot.png
        Scree plot with parallel analysis.

sessionInfo/
    R sessionInfo() records documenting the R and package versions
    used. See Section 6.

======================================================================
3. CORPUS CREATION
======================================================================

3.1 Student essays (LOCNESS)
----------------------------
Source: Louvain Corpus of Native English Essays (LOCNESS), Centre for
English Corpus Linguistics (CECL), Universite catholique de Louvain,
Belgium.

Subset used: All British
pupils' A level essays, British university students' essays (31 of 33 in total) 
Availability: LOCNESS is available from CECL, Universite catholique de
Louvain, on request and under licence, for non-commercial research only.
The licence does not permit redistribution, so the texts are not in this
repository. Researchers who obtain LOCNESS from CECL can match their
files to this repository through the crosswalk (data/crosswalk/).
Corpus page: https://www.uclouvain.be/en/research-institutes/ilc/cecl/locness

Citation: Granger, S. (1998). The computer learner corpus: A versatile
new source of data for SLA research. In S. Granger (Ed.), Learner
English on Computer (pp. 3-18). Addison Wesley Longman.

3.2 GPT-4 essays (GPT-4.1)
--------------------------
The GPT-4 essays were generated with OpenAI's GPT-4.1 model, accessed
through ChatGPT, and were first used in AUTHOR1 et al. (2026). They are
on the same topics as the LOCNESS essays, with the same number of
essays per topic as in LOCNESS.

Prompt (word for word):
  You are a competent senior high school student writer of English
  texts for academic purposes. Write 16 argumentative essays with a
  persuasive writing style on the topic of [topic]. Each essay is about
  500 words long.

The number in the prompt ("16") was replaced with the LOCNESS count for
each topic.

Model:      GPT-4.1, via ChatGPT
Access:     ChatGPT web interface
Date(s):    Late November 2025
Settings:   Default. Sampling settings such as temperature cannot be
            changed in the ChatGPT web interface.
Essays per topic:
            Same as the number of LOCNESS essays on that topic (varies
            by topic)

3.3 Humanised essays
--------------------
Each of the 145 GPT-4 essays was processed with:

Tool:       Grammarly AI Humanizer
Access:     Web interface, paid version
Date(s):    June 2026
Settings:   Default
Procedure:  Each essay was pasted in whole and processed in one pass;
            the output was saved as a plain .txt file without editing.

3.4 Tagging
-----------
All texts were tagged with the Multi-Feature Tagger of English (MFTE),
Python version 1.0 (Le Foll and Shakir 2023; described and evaluated
in Le Foll and Shakir 2025). Each corpus was tagged separately with
identical settings:

  - Normalisation: frequencies per 100 words
  - Type-token ratio: computed over the first 150 words of each text

MFTE tagged 141 features. The MFTE output for each corpus is in
data/mfte_counts/.

  Le Foll, E., & Shakir, M. (2023). MFTE Python (Version 1.0)
    [Computer software]. https://github.com/mshakirDr/MFTE
  Le Foll, E., & Shakir, M. (2025). The Multi-Feature Tagger of English
    (MFTE): Rationale, description and evaluation. Research in Corpus
    Linguistics, 13(2), 63-93. https://doi.org/10.32714/ricl.13.02.03

======================================================================
4. MATCHED-TRIPLET CROSSWALK
======================================================================

File names differ between the three corpora for the same topic and essay
(e.g., "Foxhunting1" in the GPT-4 corpus, "Fox1" in the humanised
corpus, "Fox hunting1" in LOCNESS). The crosswalk
(data/crosswalk/item_crosswalk_HumanAIproject.csv) assigns every text a
common item_id (topic + number), so that the three versions of each
essay can be paired.

Columns: filename, Register, topic_raw, topic_family, number, item_id

The crosswalk depends only on file names and register labels, so it is
unaffected by the type-token ratio recalculation (V1 to V2).

Register labels (set in feature_selection_HumanAIproject_V2.R and used
in all later files):
  LOCNESS_Human      student essays
  LOCNESS_GPT        GPT-4 essays
  LOCNESS_Humanised  humanised essays

The "LOCNESS_" prefix marks all three conditions as matched to the
LOCNESS topics. Only the LOCNESS_Human texts come from LOCNESS.

Excluded items: two items lack a complete triplet and are excluded from
all analyses: computerandlife_14 and vitro_28.
Each corpus has 145 essays: LOCNESS includes vitro_28 but not
computerandlife_14, and the GPT-4 and humanised corpora include
computerandlife_14 but not vitro_28.
Result: 144 complete triplets (432 texts).

======================================================================
5. ANALYSIS STEPS
======================================================================

Step 1. Tagging (MFTE)
    See Section 3.4. MFTE tagged 141 features.

Step 2. Feature exclusion
    Composite tags (which duplicate their component tags) and tags not
    relevant to written argumentative essays were removed.
    Script: feature_selection_HumanAIproject_V2.R (list of removed tags
    in section 3 of the script)

Step 3. Near-zero variance (NZV) and sampling adequacy (KMO/MSA)
    Features with near-zero variance were removed, and features with low
    sampling adequacy were then removed iteratively (KMO/MSA). 80
    features remained.
    Script: feature_selection_HumanAIproject_V2.R
    NZV criteria: caret::nearZeroVar() with default settings
    (freqCut = 95/5, uniqueCut = 10). A feature was removed when the
    ratio of its most common value to its second most common value
    exceeded 19 (95/5) and fewer than 10% of its values were distinct.
    30 features were removed at this step.
    MSA threshold: >= .60 (psych::KMO()); the feature with the lowest
    MSA was removed and KMO recalculated until all MSA values were at
    or above .60.
    Outputs: data/pooled/EFA-readyHumanAIprojectRegistersADDED_V2.csv;
    results/surviving_cols_final_HumanAIproject.csv,
    removed_nzv_HumanAIproject.csv, removed_low_msa_HumanAIproject.csv,
    overall_KMO_HumanAIproject.csv

Step 4. Exploratory factor analysis (JASP)
    Software: JASP 0.97 (JASP Team, 2026)
    Method: principal axis factoring, promax rotation.
    Factor retention: parallel analysis, scree plot and comparison of
    forced solutions (4-16 factors); six factors retained.
    Feature refinement: in preliminary EFAs, a feature with uniqueness
    >= .90 (communality <= .10) and no loading >= |.30| was removed,
    lowest first, and the EFA was re-run. This reduced the set to 58
    features. Four features with no salient loading (time adverbials,
    yes/no questions, activity verbs, time-related adjectives) did not
    meet the removal criterion (communalities .140-.315) and were
    retained in the EFA but excluded from interpretation.
    Salient loading cut-off: |.30|. A feature loading on more than one
    factor was assigned to the factor with the higher loading.
    Final model fit: RMSEA = .055, SRMR = .043, TLI = .738,
    CFI = .793; cumulative variance 34.1%.
    Files: jasp/EFA-6F-HumanAIproject_V2.jasp
           jasp/6F-Forced-V2.xlsx
    Scree plot: scree_plot.R, from the parallel analysis table
    exported from JASP (results/figures/scree_plot.png)

Step 5. Dimension scores
    Feature frequencies were standardised (z-scores across all 435
    texts in the pooled file, including the two items without a
    complete triplet). Each text's score on a dimension is the sum of
    its z-scores on the positive-loading features minus the sum on the
    negative-loading features of that factor.
    Script: zscore_Dimensions_shapiro_HumanAIproject-V2.R
    Output: data/pooled/dimension_scores_HumanAIproject_V2.csv
    The script also runs Shapiro-Wilk tests on each dimension
    (results/all_shapiro_results_HumanAIproject-6D_V2.csv).

    Dimensions:
      D1 Informational density vs stance modality
      D2 Personal-experiential involvement
      D3 Narrative specificity vs possibility/evaluation
      D4 Communicative attribution vs event occurrence
      D5 Comparative/quantitative emphasis vs necessity
      D6 Collective technical stance

Step 6. Condition comparisons
    Friedman test per dimension across the 144 triplets, with Kendall's
    W as effect size. For dimensions with a significant Friedman test,
    pairwise Wilcoxon signed-rank tests (paired by item_id),
    Benjamini-Hochberg-adjusted across the three contrasts per
    dimension, with effect size r = Z / sqrt(n), n = 144 pairs.
    Direction: condition medians, median within-triplet differences
    and the number of triplets in which each condition scored higher.
    The script checks each Wilcoxon statistic against base R's
    wilcox.test() on item-aligned data and stops if they differ.
    Script: friedman_wilcoxon_HumanAIproject_V2.R
    Figures: wilcoxon_letter_boxplots_v2_HumanAIproject.R
    (results/figures/boxplot_v2_D1-D6.png)

Step 7. Further analyses
    - Feature medians per condition for D1, D2, D4 and D5
      (feature_medians_by_dimension_HumanAIproject_V2.R)
    - Feature co-occurrence structure: correlation matrices of the 58
      features per condition, compared by matrix correlation (Pearson;
      Spearman as robustness check), bootstrap of differences over
      triplets (B = 5000), split-half baseline with Spearman-Brown
      correction, Mantel tests
      (matrix_similarity_HumanAIproject_V2.R)
    - Length check: log ratio of humanised to GPT-4 word counts
      correlated with score change per dimension (Spearman, BH-adjusted)
      (Text-length-impact.R)
    - Selection of representative matched triplets (median-difference
      rule) (representative_triplets_HumanAIproject_V2.R)
    - Top and bottom five texts on each dimension
      (extract_top_bottom_texts_MATCHED-V2.R)
    - [CONFIRM: include only if the paper shows highlighted excerpts]
      Highlighting of each dimension's features in an MFTE-tagged text
      (highlight_dimension_features_HumanAIproject.R). This script needs
      MFTE-tagged texts, which are not included (LOCNESS licence); it
      can be run on texts tagged by the user.

======================================================================
6. HOW TO REPRODUCE
======================================================================

Software
    MFTE        Python version 1.0
    JASP        0.97
    R           4.5.0 (2025-04-11 ucrt). One run, on 4 November 2026
                (Friedman/Wilcoxon analysis), used R 4.5.2.
    R packages  readr, dplyr, stringr, tidyr, caret, psych, rstatix,
                coin, vegan, ggplot2, multcompView, readxl, openxlsx;
                officer (highlighting script only)
    Exact package versions are recorded in sessionInfo/.

Before running
    1. The scripts read and write files by name in the R working
       directory. Put the input files for each script (listed in the
       run order below) in the working directory, or add their folder
       paths to the file names in the scripts.
    2. Each script writes sessionInfo() records to the working
       directory at its start and end.
    3. Text-length-impact.R reads word counts from the three
       *_counts_raw.csv files in data/mfte_counts/ (names set in
       wc_files at the top of the script).

Run order
     1. feature_selection_HumanAIproject_V2.R
          in:  data/mfte_counts/*_normed.csv
     2. crosswalk_mismatchitem_HumanAIproject.R
          in:  data/pooled/EFA-readyHumanAIprojectRegistersADDED_V2.csv
     3. EFA in JASP (jasp/), exporting the loadings and the parallel
        analysis table; scree_plot.R for the scree plot
     4. zscore_Dimensions_shapiro_HumanAIproject-V2.R
          in:  EFA-ready file (data/pooled/)
     5. friedman_wilcoxon_HumanAIproject_V2.R
          in:  dimension scores, crosswalk
     6. wilcoxon_letter_boxplots_v2_HumanAIproject.R
          in:  dimension scores, crosswalk, Friedman and Wilcoxon
               results from step 5
     7. feature_medians_by_dimension_HumanAIproject_V2.R
          in:  EFA-ready file, crosswalk
     8. matrix_similarity_HumanAIproject_V2.R
          in:  EFA-ready file, crosswalk
     9. Text-length-impact.R
          in:  dimension scores, crosswalk, data/mfte_counts/
               *_counts_raw.csv
    10. representative_triplets_HumanAIproject_V2.R
          in:  dimension scores, crosswalk
    11. extract_top_bottom_texts_MATCHED-V2.R
          in:  dimension scores, crosswalk

Reproducing without the LOCNESS texts
    Only tagging (Section 3.4) needs the original texts. Every script in
    the run order uses files included in this repository, so the whole
    analysis after tagging can be reproduced without the LOCNESS texts.
    The optional highlighting script is the only exception.

Random seeds
    matrix_similarity_HumanAIproject_V2.R uses set.seed(2026).

sessionInfo records
    The files in sessionInfo/ are named by run date
    (sessionInfo_YYYY-MM-DD.txt at the start of a run,
    sessionInfo_end_YYYY-MM-DD.txt at the end), not by script. When
    several scripts were run on the same day, the last run's record
    replaced the earlier ones; the packages listed in each record show
    which analysis it belongs to. End-of-run records are available for
    runs from 16 to 19 October 2026.

======================================================================
7. LICENCE
======================================================================

Code (scripts/): MIT License. See LICENSE.

Data (data/, jasp/, results/): Creative Commons Attribution 4.0
International (CC BY 4.0), except as noted below.

Exceptions:
  - LOCNESS texts are not included and are not covered by this licence;
    they are available only from CECL, Universite catholique de Louvain,
    under its own licence (non-commercial research use, no
    redistribution).


======================================================================
8. HOW TO CITE
======================================================================

Paper:
    [INSERT citation after acceptance]

This repository:
    [INSERT repository citation]. DOI: [INSERT DOI]

LOCNESS:
    Please credit the Centre for English Corpus Linguistics (CECL),
    Universite catholique de Louvain, Belgium, in any work that uses
    LOCNESS, and cite:
    Granger, S. (1998). The computer learner corpus: A versatile new
    source of data for SLA research. In S. Granger (Ed.), Learner
    English on Computer (pp. 3-18). Addison Wesley Longman.
