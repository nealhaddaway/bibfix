
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bibfix <img src="man/figures/bibfix hex.png" align="right" width="20%"/>

<!-- badges: start -->
<!-- badges: end -->

`bibfix` is an R package and a Shiny app that helps users repair and enrich their bibliographic data. It does so through a suite of functions that request bibliographic data from API services including OpenAlex and The Lens.org. Repaired fields can include titles, abstract, unique identifiers (e.g. DOIs), publication years, etc. In addition, bibliographic records can be supplemented with additional information, including internal identifiers for common databases (e.g. The Lens.org, PubMed, Microsoft Academic) that facilitate deduplication, and information such as author affiliations and identifiers (e.g. ORCIDs).

The package is a work in progress and is part of the <a href="https://www.eshackathon.org/" target="_blank">Evidence Synthesis Hackathon</a>.


To install the latest version of the package:

```
remotes::install_github("nealhaddaway/bibfix")
```


Please cite this package as: Haddaway NR, Grainger MJ, Jones ML and
Stuart A (2021). bibfix: An R package and Shiny app for repairing and
enriching bibliographic data.
<a href="https://github.com/nealhaddaway/bibfix" target="_blank">https://github.com/nealhaddaway/bibfix</a>.
