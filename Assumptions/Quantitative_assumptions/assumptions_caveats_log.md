# Assumptions and caveats log

This log contains a list of assumptions and caveats used in this analysis.

## Definitions

Assumptions are red, amber or green (RAG) rated according to the following definitions for quality and impact[^1]:

| RAG rating | Assumption quality | Assumption impact |
|------------|--------------------|-------------------|
| GREEN | Reliable assumption, well understood and/or documented; anything up to a validated & recent set of actual data. | Marginal assumptions; their changes have no or limited impact on the outputs.  |
| AMBER | Some evidence to support the assumption; may vary from a source with poor methodology to a good source that is a few years old. | Assumptions with a relevant, even if not critical, impact on the outputs. |
| RED   | Little evidence to support the assumption; may vary from an opinion to a limited data source with poor methodology. | Core assumptions of the analysis; the output would be drastically affected by their change. |

[^1]: With thanks to the Home Office Analytical Quality Assurance team for these definitions.

## Assumptions and caveats

Assumptions and caveats last updated: 23/03/2023

This analysis contains the following assumptions and caveats:

### Assumption 1: House price should always be a positive number.

* Location: `Quantitative_assumptions/Example_script.py`
* **Quality**: RED
* **Impact**: AMBER

Detailed description: We assume that no house is sold for a negative amount or for no value.

### Assumption 2: Current trends in price will continue.

* Location: `Quantitative_assumptions/Example_script.py`
* **Quality**: RED
* **Impact**: AMBER

Detailed description: We assume that the mean sale price of each dwelling type in a local authority in time 't' should be less than time 't+1'.

### Assumption 3: There is no change in average house prices by local authority.

* Location: `Quantitative_assumptions/Example_script.py`
* **Quality**: RED
* **Impact**: AMBER

Detailed description: We assume that there is no regional variation in house prices.

Currently no caveats in this analysis.
