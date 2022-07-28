# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("shinythemes")
usethis::use_package("turner")
usethis::use_package("DT")
usethis::use_package("dplyr")
usethis::use_package("shinyjqui")
usethis::use_package("numbers")
usethis::use_package("blocksdesign")
usethis::use_package("shinycssloaders")
usethis::use_package("ggplot2")
usethis::use_package("plotly")
usethis::use_package("viridis")
usethis::use_package("shinyalert")
usethis::use_package("desplot")
usethis::use_package("shinyjs")
usethis::use_package("rlang", "Suggests")
usethis::use_pipe()
usethis::use_spell_check()

## Add modules ----
## Create a module infrastructure in R/
############ PARTIALLY REPLICATED #################
golem::add_module(name = "Diagonal")
golem::add_module(name = "Optim")
golem::add_module(name = "RCBD_augmented")
golem::add_module(name = "pREPS")
############# Lattice Designs ####################
golem::add_module(name = "Square_Lattice")
golem::add_module(name = "Rectangular_Lattice")
golem::add_module(name = "Alpha_Lattice")
############# CLASSIC DESIGNS ##################### 
golem::add_module(name = "CRD")# Name of the module
golem::add_module(name = "RCBD")# Name of the module
golem::add_module(name = "LSD")# Name of the module
golem::add_module(name = "FD")# Name of the module
golem::add_module(name = "SPD")# Name of the module
golem::add_module(name = "SSPD")# Name of the module
golem::add_module(name = "IBD")# Name of the module
golem::add_module(name = "RowCol")# Name of the module
golem::add_module(name = "STRIPD")# Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
#### Creates ftc_*
golem::add_fct("partially_replicated")
golem::add_fct("CRD")
golem::add_fct("RCBD")
golem::add_fct("latin_square")
golem::add_fct("full_factorial")
golem::add_fct("split_plot")
golem::add_fct("split_split_plot")
golem::add_fct("strip_plot")
golem::add_fct("RCDB_augmented")
golem::add_fct("incomplete_blocks")
golem::add_fct("row_column")
golem::add_fct("square_lattice")
golem::add_fct("rectangular_lattice")
golem::add_fct("alpha_lattice")
golem::add_fct("diagonal_arrangement")
golem::add_fct("optimized_arrangement")
golem::add_fct("split_families")
golem::add_fct("plot_layout")
#### Creates utils_*
golem::add_utils("load_file")
golem::add_utils("norm_trunc")
golem::add_utils("paste_by_row")
golem::add_utils("lsq")
golem::add_utils("split_name")
golem::add_utils("scrolY")
golem::add_utils("continue_plot_Cart")
golem::add_utils("continue_plot_Serp")
golem::add_utils("automatically_cuts")
golem::add_utils("available_percent")
golem::add_utils("random_checks")
golem::add_utils("seriePlot_numbers")
golem::add_utils("export_design")
golem::add_utils("diagonals_checks")
golem::add_utils("get_random")
golem::add_utils("split_vectors")
golem::add_utils("plot_number_fillers")
golem::add_utils("plot_number")
golem::add_utils("paste_by_col")
golem::add_utils("ARCBD_name")
golem::add_utils("ARCBD_plot_number")
golem::add_utils("ibd_plot_numbers")
golem::add_utils("names_diagonal")
golem::add_utils("pREP")
golem::add_utils("get_DBrandom")
golem::add_utils("names_layout")
golem::add_utils("serpentinelayout")
golem::add_utils("concurrence_matrix")
golem::add_utils("order_ls")
golem::add_utils("ZST")
golem::add_utils("AR1xAR1_simulation")
golem::add_utils("plot_number_splits")
golem::add_utils("NO_Random")
golem::add_utils("infoPrint")
golem::add_utils("S3_methods")
golem::add_utils("plot_RCBD")
golem::add_utils("plot_CRD")
golem::add_utils("plot_iblocks")
golem::add_utils("plot_latinSQ")
golem::add_utils("planter_transform")
golem::add_utils("plot_splitPlots")
golem::add_utils("factor_subsets")

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "shinybusy" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "style" )
golem::add_css_file( "mobile" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )
usethis::use_test("testing utils functions")

# Documentation

## Vignette ----
usethis::use_vignette("fieldhub")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

