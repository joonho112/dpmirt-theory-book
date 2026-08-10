# Bayesian Semiparametric Item Response Modelling for Person-Specific Latent Traits

*Theory, Identification, and the Literature Behind the Estimators*

**JoonHo Lee** · The University of Alabama · <jlee296@ua.edu>

A web monograph on what it takes to report a person's latent trait honestly:
the measurement models and their identification, information and the reliability
coefficients that summarise it, hierarchical shrinkage and the empirical-Bayes
tradition it comes from, Dirichlet process mixture priors on the latent-trait
distribution, and the decision-theoretic case that three inferential goals —
an individual's score, a ranking, and the distribution itself — imply three
different posterior summaries rather than one set of numbers.

📖 **Read it:** <https://joonho112.github.io/dpmirt-theory-book/>

## What is here

The book runs to 29 chapters in nine parts, plus seven appendices.

| Part | What it does |
|---|---|
| **I–II** | The estimation problem; the Rasch and 2PL models, identification, and item/person estimation |
| **III** | Information, measurement error, and the reliability coefficients — including why several defensible coefficients disagree |
| **IV** | The Bayesian hierarchical model, the conditional posterior, shrinkage, and its empirical-Bayes lineage |
| **V** | What happens when normality is relaxed: flexible and semiparametric alternatives, the Dirichlet process, and DPM priors |
| **VI** | Three goals, three losses: posterior means, constrained Bayes, and triple-goal estimation |
| **VII** | Positioning against the nearest neighbour in the literature, scope, and open problems |
| **VIII** | What changes under the 2PL — information, reliability, and identification with free discriminations |
| **IX** | The realized evidence: what a companion simulation settled, what real tests look like, and what remains open |

The title is a **Rasch-centred umbrella**, not a claim of parallel depth across
the two models: Parts I–VII form the theory spine and Part VIII is a focused
two-parameter extension of the mechanisms for which free discriminations
materially change the argument.

## How the book is put together

Two disciplines shape the repository, and both are auditable.

**Provenance.** Every numbered result carries a tag — *restated*, *adapted*, or
*derived here* — together with a source locator, and `manifest/` holds the
registers that make this checkable: the result register, the notation register,
the corrections to the manuscript this book supports, the reviewer claim map,
and the source-reading receipts. A `derived-here` tag asserts no priority.

**Generated artifacts.** Chapters read frozen tables under `tables/` and figures
under `book/figures/`; they compute nothing. Where the book quotes a companion
volume, `manifest/evidence-claim-register.csv` binds each imported claim to its
source edition, snapshot locator, and field. Appendix G states exactly what that
machinery guarantees and what it does not.

| Path | Contents |
|---|---|
| `book/` | Quarto sources, figures, and bibliography. The rendered site is published from the `gh-pages` branch |
| `code/R/` | The build pipeline: table and figure generators, the bibliography harvester, and the verifiers |
| `manifest/` | Provenance registers |
| `tables/` | Frozen `.rds` artifacts with CSV mirrors under `tables/supplement/` |
| `refs/` | The bibliography and its acquisition ledger |
| `verification/` | Verifier outputs from the release build |

## Reproducing

**Rendering the book needs only this repository:**

```bash
quarto render book
```

The rendered site is served from the `gh-pages` branch; `main` carries the
sources that produce it.

**The full verification pipeline does not.** `Rscript code/R/14-build-all.R`
regenerates every artifact and runs the verifiers, but it reads inputs that are
deliberately not published: nine reference libraries of licensed third-party
PDFs that back the citation audit, the companion simulation and case-study
repositories, and two Item Response Warehouse corpus studies. Point
`DPMIRT_PROJECT_ROOT` at a directory holding those alongside this repository and
the pipeline runs; without them it will stop at the first missing input, by
design. The verifier outputs from the release build are included under
`verification/` so the result of that run can be inspected without re-running it.

Environment: R 4.6.0, Quarto 1.9.x.

## Companion volumes

This is the **theory** volume of a three-volume project. The simulation volume
owns realized outcomes under known truth; the case-study volume owns what
changes on real tests and issues no correctness claims of its own. Where this
book cites either, it names the volume and the frozen artifact it read.

## Citing

See `CITATION.cff`, or:

> Lee, JoonHo (2026). *Bayesian Semiparametric Item Response Modelling for
> Person-Specific Latent Traits: Theory, Identification, and the Literature
> Behind the Estimators.*

## Licence

Text, figures, and tables under `book/` are CC BY 4.0; code under `code/` is
MIT. Cited sources are the copyright of their publishers and are not
distributed here. See `LICENSE`.
