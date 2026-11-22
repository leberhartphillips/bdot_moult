# Banded dotterel molt study

## Repeatable ornament expression and limited evidence for breeding-related plasticity in molt timing

Luke Eberhart-Hertel, Bashar Jarayseh, Ailsa McGilvary-Howard, Ted Howard,
Tony Habraken, Emma M. Williams, Colin F. J. O'Donnell, and Bart Kempenaers

This repository contains the commented script, data and saved models accompanying
the revised manuscript. Repeated photographs of marked banded dotterels allow us
to estimate baseline ornament expression and post-breeding molt midpoint together.
We compare individual consistency in these components and assess breeding-related
plasticity by separating among- and within-individual associations in confidently
after-first-year (AFY) birds. In this study, adult includes both first-year and AFY
birds.

For correspondence, contact Luke Eberhart-Hertel: `luke.eberhart@bi.mpg.de`.

## Start here

- [Commented analysis script](banded_dotterel_moult.qmd): Quarto source with the
  equations, their R implementations, sample definitions, results and diagnostics.
- [Rendered analysis](banded_dotterel_moult.html): download this self-contained HTML
  file and open it in a browser. GitHub's file viewer does not display the full
  rendered document.

The HTML includes code folding and a Code menu for viewing the source. Rendering
uses the eight saved models in `out/`; **it does not refit any model**. The
model-fitting calls are shown in blocks marked `eval=FALSE` for optional refitting.

## Manuscript outputs

| Output | Contents |
| --- | --- |
| Table 1 | Sex, age-class, migratory-status, season and repeatability results |
| Table 2 and Figure 4 | Among- and within-individual breeding–molt associations for three breeding-date measures |
| Table S1 | Sex interactions with the AFY mean and deviation in last observed breeding date, and with migratory status |
| Figures 1–3 | Scoring examples, nonlinear trajectory schematic and state-specific trajectories |
| Figure S1 | Six panels of raw observations by sex and season, ordered and colour-coded by migratory status |
| Figure S2 | Approximate power to detect a resident–migrant midpoint difference |
| Supporting summaries | Observer agreement, breeding-date ranges, recorded attempts, first-year breeding records, Fisher's exact tests and final local plumage scores |

The main AFY analysis uses **47 birds across 86 bird-seasons and 1,045 scored
observations**. Twenty-eight birds contributed repeated AFY seasons. Each breeding
date is fitted separately. Means and deviations are calculated once per retained
bird-season, after restricting the sample to confidently AFY observations. The
models estimate a shared within-individual slope, rather than individual plasticity
slopes. No covariate effects on the shared transition-steepness parameter are tested.

## Render from a fresh clone

1. Clone or download the repository and keep its directory structure intact. Run R
   from the repository root.
2. Install R and the Quarto command-line tool. Install the R packages listed in the
   setup section of `banded_dotterel_moult.qmd`; the setup checks for missing
   packages. Figure 4 uses the bundled **Lato Regular** font and the font-rendering
   packages listed in that setup section.
3. Run:

   ```bash
   quarto render banded_dotterel_moult.qmd --to html
   ```

The supplied data and fitted models are read during rendering. Tables and plots
are regenerated, and derived CSV and figure files are written to `out/` and
`figs/`. These generated files are not all tracked. The HTML embeds its images,
styles, scripts and mathematical notation so it can be read without the source
folders or an internet connection.

A working `cmdstanr` and CmdStan installation is required only for refitting.
All eight reported models use four chains of 10,000 iterations, with 5,000 warmup
iterations per chain and the `cmdstanr` backend. The document checks these settings
against the saved fits and reports diagnostics and the render environment.

## Data, models and helper scripts

Only the inputs and supporting code needed for the reported analyses are included.
Rendering and refitting start from the supplied analysis-ready `.rds` data. Local
manuscript drafts, historical preparation files, retired fits and exploratory
analyses are excluded.

- `data/*.rds`: analysis-ready photographic, bird-season and nest data.
- `data/Scoring - test.xlsx`: original observer-agreement worksheet.
- The three `Kaikoura_Dotterel_*.xlsx` workbooks listed in the document: nest-visit
  inputs used to count recorded breeding attempts.
- `R/scoring_validation_correlations.R`: parsing and observer-agreement calculations.
- `assets/fonts/`: Lato Regular and its SIL Open Font License, included so Figure 4
  does not depend on a locally installed font. Source: [Google Fonts](https://github.com/google/fonts/tree/main/ofl/lato).
- `scripts/run_afy_within_individual_breeding_phenology_models.R`: optional refitting
  and centering checks for the three main AFY models.
- `scripts/run_afy_sex_last_breeding_interaction_2026-11-18.R`: optional refitting and
  centering checks for the supplementary AFY sex interaction.
- `scripts/export_figure_s1_panels_2026-11-20.R`: regeneration of the six Figure S1
  PNGs and combined PDF.
- `figs/Figure_S1_for_Word_2026-11-20/`: the six PNG panels displayed in Figure S1.
  The export script regenerates these panels and their combined PDF from the data.

The saved models used by the document are:

| File in `out/` | Model |
| --- | --- |
| `sex_mod_.rds` | Sex and season |
| `sex_age_class_mod_2.rds` | Sex, age class and season |
| `sex_migratory_status_mod_.rds` | Sex, migratory status and season |
| `sex_first_breed_within_individual_afy_mod_.rds` | AFY first observed breeding date |
| `sex_last_nest_within_individual_afy_mod_.rds` | AFY last nest date |
| `sex_last_breed_within_individual_afy_mod_.rds` | AFY last observed breeding date |
| `afy_sex_last_breeding_interaction_2026-11-18/model.rds` | AFY sex-by-mean and sex-by-deviation interactions |
| `sex_migratory_interaction_mod_.rds` | Sex-by-migratory-status interaction |

Each saved model contains `fitted_model` (a `brmsfit`) and its analysis `data`.
AFY models also retain the predictor, photograph-date scaling reference and
midpoint-to-days conversion. See the document's reproducibility section for the
complete input manifest, fitting calls, seeds and guidance for applying the
approach to another system.

## Plot corrections still pending

This repository preserves the fitted models and the numerical analyses. The
following plotting corrections remain to be completed before final submission:

- Correct calendar-date back-transformations in Figures 1 and 3 and use a consistent
  reference season for Figure 3.
- Reconcile the 2,285 raw records currently supplied to plotting layers with the
  1,646 distinct scored observations used in the full sex-specific model.
- Update Figure 2's illustrative midpoint and steepness values to the current
  posterior medians and revise its caption accordingly.

The same limitations are identified beside the relevant figures in the analysis.
Two generated filenames retain earlier numbering:
`figs/Figure5_afy_components_revision3.pdf` and its PNG show the current Figure 4,
and `figs/FigS3.pdf` shows the current Figure S2. These names do not denote
additional manuscript figures. Figure S1 already has consistent panel widths and
both legends.

## AI use

AI tools assisted with R code, model implementation, documentation and parts of
the writing. The author retained responsibility for the analytical decisions,
interpretation and final wording. The full disclosure is included in the analysis
companion and manuscript.
