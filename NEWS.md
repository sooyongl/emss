# emss 0.1.0

* First release. `emss` implements Embedded Standard Setting (ESS): it estimates
  cut scores from panelists' item–level alignments and item locations.

* Cut scores can be estimated with either criterion: ESS-Count (number of
  inconsistent items) or ESS-Weight (summed distance of inconsistent items).

* Results are given for each panelist and aggregated across panelists (modal,
  median and mean), by grade, round and, optionally, by domain.

* Diagnostics include panelist agreement, item review (consistent, essentially
  consistent and inconsistent items) and impact data from examinee scores.

* `update()` evaluates manually set cut scores, and `bootstrapping()` gives
  standard errors for the estimated cuts.

* Input can come from an Excel workbook or from data frames and CSV files via
  `ess_data()`. `report()` and `plot()` produce tables and figures of the results.