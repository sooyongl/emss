# Changelog

## emss 0.1.0

- First release. `emss` implements Embedded Standard Setting (ESS): it
  estimates cut scores from panelists’ item–level alignments and item
  locations.

- Cut scores can be estimated with either criterion: ESS-Count (number
  of inconsistent items) or ESS-Weight (summed distance of inconsistent
  items).

- Results are given for each panelist and aggregated across panelists
  (modal, median and mean), by grade, round and, optionally, by domain.

- Diagnostics include panelist agreement, item review (consistent,
  essentially consistent and inconsistent items) and impact data from
  examinee scores.

- [`update()`](https://rdrr.io/r/stats/update.html) evaluates manually
  set cut scores, and
  [`bootstrapping()`](https://sooyongl.github.io/emss/reference/bootstrapping.ESS.md)
  gives standard errors for the estimated cuts.

- Input can come from an Excel workbook or from data frames and CSV
  files via
  [`ess_data()`](https://sooyongl.github.io/emss/reference/ess_data.md).
  [`report()`](https://sooyongl.github.io/emss/reference/report.ESS.md)
  and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) produce
  tables and figures of the results.
