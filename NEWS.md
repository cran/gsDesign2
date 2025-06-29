# gsDesign2 1.1.5

## Bug fixes

- The spending of the WLR design has been corrected. 
The default spending of a WLR design is information fraction. (#557, thanks to @LittleBeannie).


# gsDesign2 1.1.4

## New functions

- A new function, `gs_cp_npe()`, is now available for calculating simple conditional power under NPH. 
A vignette has been published on the pkgdown website (#510, #539, #545, thanks to @LittleBeannie).
- A new design summary function `gs_bound_summary()` is available with similar output structure as `gsDesign::gsBoundSummary()` and
supporting for multiple alpha (#468, #522, #537, thanks to @jdblischak).
- Textual summary of AHR designs are available via the `text_summary()` (#526, thanks to @LittleBeannie).

## User interface improvements

- The integer event is rounded in IAs and rounded up in FA as defaults in `to_integer()` (#483, thanks to @LittleBeannie).
- The integer sample size is rounded to the nearest multiple of randomization ratio + 1 when `round_up_final = TRUE`
(#488, thanks to @LittleBeannie)..
- The updated design can be derived with the input of events per analysis per interval (#499, thanks to @LittleBeannie).
- Bound functions and spending functions can be provided through their names (character strings) now, 
e.g., `gs_design_ahr(..., upper = "gs_spending_bound", upar = list(sf = "sfLDOF", ...))` (#509, thanks to @yihui).
- The `footnote` argument of `as_gt()` can take the value `FALSE` to disable footnotes (#514, thanks to @yihui).
- Optimized the functions `expected_accural()`, `expected_time()`, `gs_design_ahr()`,  `gs_design_combo()`, 
`gs_design_npe()`, `gs_design_wlr()`, `pw_info()`, `ppwe()`, `s2pe()`, and `gs_bound()` (#528, thanks to @yihui).
- Weights of a WLR design functions can be provided as string now (#533, thanks to @LittleBeannie).

## Bug fixes

- Correct the statistical information of WLR-integer designs in `to_integer()` (#478, thanks to @LittleBeannie).
- Correct the calculation `info_frac` in `gs_design_wlr()` when `info_scale = "h0_info"` (#485, #486, thanks to @LittleBeannie).
- Add the `h1_spending` argument to `gs_power_ahr()` (#518, thanks to @LittleBeannie)

## Testing

- Add the developer tests of `gs_cp_npe()` (#519, thanks to @shiyuskaya)

# gsDesign2 1.1.3

## Bug fixes

- Fix the bug of `gs_design_rd()` when `info_scale = "h0_h1_info"` (#402, thanks to @LittleBeannie).
- Fix the bug of `gs_spending_combo()` to enable HSD spending function (#444, thanks to @LittleBeannie).
- Fix the bug of `fixed_design_maxcombo()` regarding the upper bounds (#445, thanks to @elong0527).
- Fix the bug of `gs_design_wlr()` when the design is driven by information fraction only (#446, thanks to @LittleBeannie).
- Fix the bug of `pw_info()` when there are many piecewise HRs (#460, thanks to @LittleBeannie).

## Statistical improvements

- The `gs_update_ahr()` function (test version) is updated to

  + Enable unchanged futility bounds when it is fixed at the original design (#408, thanks to @LittleBeannie).
  + Allow boundary updates when only certain interim analysis data is available (#436, thanks to @LittleBeannie).
  + Allow boundary updates when alpha changes by keeping the same `info_scale` as the original design (#470, @LittleBeannie).

- Rounding of integer design is updated (#488, #484, #486, thanks to @LittleBeannie).
- Integer design (i.e., integer sample size and events) is updated to ensure exact integer sample size and events  (#452, #460, thanks to @LittleBeannie and @yihui).
- Change the information fraction displayed at the summary-gt table from under H1 to H0 for logrank tests (#439, thanks to @LittleBeannie).
- Add the sample size as the output of `ahr()` and `pw_info()` (#427, #433, thanks to @LittleBeannie).

## User interface improvements

- Enable passing named vector of `col_decimals` and `analysis_decimals` to `summary.gs_design()` (#403, #431, @jdblischak).

## Coding practice improvements

- Add robust check if `lower` is equivalent to `gs_b` (#413, thanks to @jdblischak )
- The `summary()`, `as_gt()` `as_rtf()`, and `to_integer()` functions are refactored (#448, #449, #450, #465, #461, thanks to @yihui).
- Remove the `full_alpha` argument from `as_rtf.gs_design()` (#458, thanks to @yihui).
- Other coding practice improvement worth mentioning (#409, #412, #429, #437, #440, #447, #453, #464, #467, $475, thanks to @yihui, @nanxstats, and @jdblischak).

## Documentation

- Update the documentation of `gs_b()` (#415, @jdblischak)

## Testing

- More developer tests of `gs_power_ahr()` are added (#420, @LittleBeannie).
- More developer tests of `summary()` are added (#422, #426, thanks to @yuliasidi, @jdblischak and @LittleBeannie).
- Independent tests of `ahr_blinded()` are added (#435, thanks to @DMuriuki).
- More developer tests of `to_integer()` are added (#476, thanks to @LittleBeannie).

# gsDesign2 1.1.2

## New features

- The `gs_update_ahr()` function is now available for efficacy and futility
  boundary update based on blinded estimation of treatment effect (#370).

## Bug fixes

- Fix the accrual parameters bugs in `gs_design_wlr()` depending on npsurvSS (#344, #356).
- Fix `gs_design_ahr()` to incorporate information fraction driven design when number of analyses >= 4 (#358).

## Improvements

- Zero failure rate in some but not all intervals is acceptable as input (#360).
- Study with duration > 100 units are executable when event accrual is slow (#368).

## Documentation

- A new [vignette](https://merck.github.io/gsDesign2/articles/story-update-boundary.html)
  introducing how to do the boundary update is available (#278, #364, #366).
- A new [vignette](https://merck.github.io/gsDesign2/articles/story-seven-test-types.html)
  bridging gsDesign2 to the 6 test types of gsDesign is available.
- The pkgdown website is re-organized to providing better view for users (#341).

## Testing

- Independent testing of `as_gt()` is added (#337).
- Restructure tests to make them self-contained (#347).

# gsDesign2 1.1.1

## New features

- The `as_rtf()` method is now available for `fixed_design` and `gs_design`
  objects for generating RTF table outputs (#278).

## Bug fixes

- `gs_power_wlr()` and `to_integer()` now check and convert integer
  sample size more rigorously (#322).
- `gs_design_*()` now handle exceptions explicitly when all hazard ratio
  is set to 1 throughout the study (#301).
- `fixed_design_rd()` will not generate warnings due to the previous
  default value change of `h1_spending` (#296).

## Improvements

- `gs_power_ahr()` now runs twice as fast by using data.table and other
  performance optimizations (#295), enhanced by similar
  improvements in `gs_info_ahr()` and `pw_info()` (#300).
- Enrollment and failure rate input constructors and validators
  are refactored to check only the format instead of the class.
  This change reduces the number of warning messages and catches
  real exceptions as errors properly (#316).
- Nested functions are refactored into reusable internal functions,
  to improve code rigor, avoid potential scoping pitfalls, and
  facilitate debugging (#235).
- For fixed designs, the variable names of the table outputs from
  `to_integer()` and `summary()` are updated (#292).

## Documentation

- Add a new vignette
  [statistical information under null and
  alternative hypothesis](https://merck.github.io/gsDesign2/articles/story-info-formula.html)
  (#289).
- Improve `define_enroll_rate()` and `define_fail_rate()` documentation by
  adding detailed descriptions and improving code examples (#302).
- The function reference page now has dedicated sections for piecewise
  exponential distributions and computing trial events (#258).
- Use the four trailing dashes convention to standardize code comment
  section format (#308).

## Namespace and testing

- Tidy up namespace by removing rlang from and adding stats to `Imports` (#307, #325).
- Qualify namespaces in tests to avoid `library()` calls (#332).
- Fortify the GitHub Actions workflows by limiting the token usage only when
  necessary and enabling manual trigger of workflow runs (#326).
- Update GitHub Actions workflows to the latest versions from upstream
  (#330).

# gsDesign2 1.1.0

## Breaking changes

- Split `fixed_design()` into a group of `fixed_design_*()` functions for enhanced modularity (#263).
- `gs_design_rd()` and `gs_power_rd()` now have updated options of weighting for stratified design (#276).
- `ppwe()` now accepts two arguments `duration` and `rate` instead of a data frame `fail_rate` (#254).
- Unexport helper functions `gridpts()`, `h1()`, and `hupdate()` (#253).

## New features

- Introduce `define_enroll_rate()` and `define_fail_rate()` as new input constructor functions to replace the tibble inputs (#238).
- Add a new function `pw_info()` which calculates the statistical information under the piecewise model (#262).

## Improvements

- Add a [vignette](https://merck.github.io/gsDesign2/articles/story-canonical-h0-h1.html) showing the canonical joint distribution of Z-score and B-values under null and alternative hypothesis for the AHR test (#246).
- Refactor `expected_event()` to improve computational performance (@jdblischak, #250).
- Move the source code of the legacy version from `inst/` to `tests/testthat/` as developer tests (#269).

# gsDesign2 1.0.9

## Improvements

- Add CRAN download counts badge (#215).
- Update documentation of `gs_design_rd()` (#220).
- Format footnote numbers using decimal notation (#222).
- Split C++ functions into individual `.cpp` and header files (#224).

## Bug fixes

- Fix the digits display in `summary()` (#231).

# gsDesign2 1.0.8

## Improvements

- Update the calculation of upper/lower bounds at the final analysis in MaxCombo tests (#217).
- Update the `fixed_design()` function in the application of stratified design when using the Lachin and Foulkes method (#211).
- Correct the `fixed_design()` function in the application of `rmst` (#212).
- Rename the `info_scale` argument options from `c(0, 1, 2)` to `c("h0_h1_info", "h0_info", "h1_info")` to be more informative and make the default value (`"h0_h1_info"`) clear (#203).
- Add missing global functions/variables (#213).
- Fix outdated argument names and use canonical style for text elements in `README.md` (#198).
- Add a CRAN downloads badge to `README.md` to show the monthly downloads (#216).

## Bug fixes

- Fix the calculation of the futility bounds in `gs_power_ahr()` (#202).

# gsDesign2 1.0.7

## Improvements

- Move imported dependencies from `Suggests` to `Imports`.
- Remove redundant dependencies from `Suggests`.
- Update the GitHub Actions workflows to their latest versions from upstream.
- Add a rule to `.gitattributes` for GitHub Linguist to keep the repository's
  language statistics accurate.

# gsDesign2 1.0.6

## Improvements

- Export functions `gridpts()`, `h1()`, `hupdate()`, and `gs_create_arm()`
  to avoid the use of `:::` in code examples.
- Fix the write path issue by moving the test fixture generation script to
  `data-raw/` which is not included in the package.

# gsDesign2 1.0.5

First submission to CRAN in March 2023.

## Breaking changes

- Passes lintr check for the entire package (#150, #151, #171).
- Improve the documentation (#161, #163, #168, #176).

## Bug fixes

- `check_fail_rate()` when only 1 number in `fail_rate` is > 0 (#132).
- `gs_power_ahr()` when study duration is > 48 months (#141).
- `fixed_design()` for event-based design (#143).
- `gs_design_combo()` when test only applies to part of the analysis (#148).
- `gs_info_rd()` for variance calculation (#153).
- `summary()` for capitalized first letter in the summary header (#164).

# gsDesign2 1.0.0

GitHub release in December 2022.

## Breaking changes

- Merges [gsDesign2 v0.2.1](https://github.com/Merck/gsDesign2/tree/v0.2.1)
  and [gsdmvn](https://github.com/Merck/gsdmvn).
- Updates API to follow the new style guide in `vignette("style")`.
  See the detailed mapping between the old API and new API in #84.

## New features

- Supports organized summary tables and gt tables.
- Power/sample size calculation for risk difference.
- Integer sample size support (#116, #125).
- Adds `fixed_design()` to implement different methods for power/sample size calculation.
- Adds `info_scale` arguments to `gs_design_*()` and `gs_power_*()`.
- Adds RMST and milestone methods to fixed design.

## Bug fixes

- `expected_accrual()` for stratified population.
- `gs_spending_bound()` when IA is close to FA (#40).
- `gs_power_bound()` when applied in the MaxCombo test (#62).
- `gs_design_npe()` for type I error (#59).

## Minor improvements

- Adds and re-organizes vignettes.

# gsDesign2 0.2.1

GitHub release in August 2022.

- The release before merging with `Merck/gsdmvn`.

# gsDesign2 0.2.0

GitHub release in May 2022.

- Supports the _Biometrical Journal_ paper "A unified framework for weighted parametric group sequential design" by Keaven M. Anderson, Zifang Guo, Jing Zhao, and Linda Z. Sun.

# gsDesign2 0.1.0

GitHub release in May 2021.

- Updated AHR vignette to introduce average hazard ratio concept properly.
- Added arbitrary distribution vignette to demonstrate `s2pwe()`.
- Corrected calculations in `AHR()` when using stratified population.
- Release for Regulatory/Industry Symposium training.

# gsDesign2 0.0.0.9006

GitHub release in December 2019.

- Added vignette for `eEvents_df()` explaining the methods thoroughly.
- Updated `eEvents_df()` to simplify output under option `simple = FALSE`.

# gsDesign2 0.0.0.9005

GitHub release in December 2019.

- Updated `docs/` directory to correct the reference materials on the website.
- Minor fixes in `eAccrual()`.

# gsDesign2 0.0.0.9004

GitHub release in November 2019.

- Moved new simulation functions to the simtrial package
  (`simfix()`, `simfix2simPWSurv()`, `pMaxCombo()`).

# gsDesign2 0.0.0.9003

GitHub release in November 2019.

- Tried to make `AHR()` and `simfix()` more compatible with each other.
- Improved vignette for group sequential design.
- Added pkgdown website for documentation and vignettes.
- Added support functions for to support approximation using and visualization
  of the piecewise model.

# gsDesign2 0.0.0.2

GitHub release in October 2019.

- Update `AHR()` to output trial duration, expected events and average hazard ratio in a tibble.
- Vignette AHRvignette demonstrating sample size computations for fixed design under non-proportional hazards assumptions.
- Vignette gsNPH demonstrating sample size computations for group sequential design under non-proportional hazards assumptions.
- Initial implementation of `pMaxCombo()` to compute p-value for MaxCombo test; pMaxComboVignette demonstrates this capability.

# gsDesign2 0.0.0.1

GitHub release in September 2019.

- Computations based on piecewise constant enrollment and piecewise exponential failure rate.
- Expected event count calculation for each different hazard ratios in `eEvents_df()`.
- Average hazard ratio computation based on expected event counts in `AHR()`.
- Vignette demonstrating fixed sample size computation with simulation to verify power.
