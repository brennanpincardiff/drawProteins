# testing coverage
library(covr)
# https://cran.r-project.org/web/packages/covr/index.html

# 20170818
x <- package_coverage()
report(x)
# shows lines tested and not tested.
# useful!
# tells me which functions still need tests.
# also how many tests per line!

package_coverage()
# run: 201708181503
# drawProteins Coverage: 49.47%
# R/plotting_functions.R: 0.00%
# R/geoms.R: 56.52%
# R/extract_from_api.R: 57.50%
# R/get_features.R: 90.00%


package_coverage()
# run: 201708181455
# drawProteins Coverage: 44.74%
# R/get_features.R: 0.00%
# R/plotting_functions.R: 0.00%
# R/geoms.R: 56.52%
# R/extract_from_api.R: 57.50%

package_coverage()
# run: 201708181425
# drawProteins Coverage: 38.42%
# R/get_features.R: 0.00%
# R/plotting_functions.R: 0.00%
# R/geoms.R: 44.93%
# R/extract_from_api.R: 52.50%

function_coverage(get_features)
# run: 201708181425
# Coverage: 0.00%
# /Users/paulbrennan/Documents/drawProteins/R/get_features.R: 0.00%






