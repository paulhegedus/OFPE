#' Training data for the nitrogen use efficiency model.
#'
#' A dataset containing soil and plant biomass sampling data, remotely sensed 
#' covariates, and NUE calculations for four fields across two years. This data 
#' is used to train the NUE model used to optimize efficiency on maximized profit 
#' and minimized risk of N loss. 
#'
#' @format A data frame with 270 rows and 75 variables:
#' \describe{
#'   \item{samplePnt}{Sample number, out of 75 potential points in the field.}
#'   \item{field}{Name of the field sampled.}
#'   \item{farm}{Name of the farm that the field is part of.}
#'   \item{year}{Year sampling occured.}
#'   \item{fid}{Unique identifier for observation within field.}
#'   \item{mean_vwc.august}{Mean VWC across the top 30cm measured in August.}
#'   \item{mean_vwc.march}{Mean VWC across the top 30cm measured in March.}
#'   \item{sum_soil_N_kgm2.august}{Sum of the soil total N across the top 30cm in August in kg/m2.}
#'   \item{sum_soil_N_kgm2.march}{Sum of the soil total N across the top 30cm in March in kg/m2.}
#'   \item{mean_pct_TN.august}{Mean percent total N across the top 30cm in August.}
#'   \item{mean_pct_TN.march}{Mean percent total N across the top 30cm in March.}
#'   \item{mean_pct_TC.august}{Mean percent total C across the top 30cm in August.}
#'   \item{mean_pct_TC.march}{Mean percent total C across the top 30cm in March.}
#'   \item{mean_bd_gcm3.august}{Mean bulk density across the top 30cm in August in g/cm3.}
#'   \item{mean_bd_gcm3.march}{Mean bulk density across the top 30cm in March in g/cm3.}
#'   \item{sum_Nmin_kgm2}{Sum of the mineralized N (kg/m2) across the top 30cm, averaged across March and August measurements of percent TN according to Vigil et al. 2002.}
#'   \item{crop_pct_TN}{Percent total N measured in the crop's above ground biomass.}
#'   \item{crop_pct_TC}{Percent total C measured in the crop's above ground biomass.}
#'   \item{x}{The x coordinate projected into UTM zone 12.}
#'   \item{y}{The y coordinate projected into UTM zone 12.}
#'   \item{rx_N_lbsac}{The prescribed N rate in lbs/ac.}
#'   \item{ssurgo}{The SSURGO soil texture class.}
#'   \item{WHC_OG}{The WHC classification sampling was stratified on.}
#'   \item{yld}{Grain yield in bushels/acre.}
#'   \item{aa_n}{As-applied N rates in lbs/ac.}
#'   \item{slope}{The slope at the observed point.}
#'   \item{elev}{The elevation (m) at the observed point.}
#'   \item{tpi}{The topographic position index at the observed point.}
#'   \item{prec_cy_d}{Precipitation (mm) measured from Daymet V3 (1km) from Nov 1 of the prior year to Mar 30 of the harvest year.}
#'   \item{prec_py_d}{Precipitation (mm) measured from Daymet V3 (1km) from Nov 1 of two years prior to Oct 31 of the year prior to harvest.}
#'   \item{gdd_cy_d}{Growing degree days measured from Daymet V3 (1km) from Jan 1 of the harvest year to Mar 30 of the harvest year.}
#'   \item{gdd_py_d}{Growing degree days measured from Daymet V3 (1km) from Jan 1 of prior year to Dec 31 of the year prior to harvest.}
#'   \item{prec_cy_g}{Precipitation (mm) measured from GridMET (4km) from Nov 1 of the prior year to Mar 30 of the harvest year.}
#'   \item{prec_py_g}{Precipitation (mm) measured from GridMET (4km) from Nov 1 of two years prior to Oct 31 of the year prior to harvest.}
#'   \item{gdd_cy_g}{Growing degree days measured from GridMET (4km) from Jan 1 of the harvest year to Mar 30 of the harvest year.}
#'   \item{gdd_py_g}{Growing degree days measured from GridMET (4km) from Jan 1 of prior year to Dec 31 of the year prior to harvest.}
#'   \item{ndvi_cy_s}{Normalized difference vegetation index (NDVI) measured from Sentinel 2 (10m) from Jan 1 of the harvest year to Mar 30 of the harvest year.}
#'   \item{ndvi_py_s}{Normalized difference vegetation index (NDVI) measured from Sentinel 2 (10m) from Jan 1 of the prior year to Dec 31 of the prior year before harvest.}
#'   \item{ndvi_2py_s}{Normalized difference vegetation index (NDVI) measured from Sentinel 2 (10m) from Jan 1 two years prior to Dec 31 two years prior to the harvest year.}
#'   \item{ndvi_cy_l}{Normalized difference vegetation index (NDVI) measured from Landsat (30m) from Jan 1 of the harvest year to Mar 30 of the harvest year.}
#'   \item{ndvi_py_l}{Normalized difference vegetation index (NDVI) measured from Landsat (30m) from Jan 1 of the prior year to Dec 31 of the prior year before harvest.}
#'   \item{ndvi_2py_l}{Normalized difference vegetation index (NDVI) measured from Landsat (30m) from Jan 1 two years prior to Dec 31 two years prior to the harvest year.}
#'   \item{ndwi_cy_s}{Normalized difference water index (NDWI) measured from Sentinel 2 (10m) from Jan 1 of the harvest year to Mar 30 of the harvest year.}
#'   \item{ndwi_py_s}{Normalized difference water index (NDWI) measured from Sentinel 2 (10m) from Jan 1 of the prior year to Dec 31 of the prior year before harvest.}
#'   \item{ndwi_2py_s}{Normalized difference water index (NDWI) measured from Sentinel 2 (10m) from Jan 1 two years prior to Dec 31 two years prior to the harvest year.}
#'   \item{ndwi_cy_l}{Normalized difference water index (NDWI) measured from Landsat (30m) from Jan 1 of the harvest year to Mar 30 of the harvest year.}
#'   \item{ndwi_py_l}{Normalized difference water index (NDWI) measured from Landsat (30m) from Jan 1 of the prior year to Dec 31 of the prior year before harvest.}
#'   \item{ndwi_2py_l}{Normalized difference water index (NDWI) measured from Landsat (30m) from Jan 1 two years prior to Dec 31 two years prior to the harvest year.}
#'   \item{aspect_cos}{Aspect/exposure measured in the E/W direction.}
#'   \item{aspect_sin}{Aspect/exposure measured in the N/S direction.}
#'   \item{musym}{USDA soil texture classification.}
#'   \item{bulkdensity}{Soil bulk density (g/m3) gathered from OpenLandMap (250m) and averaged over 200cm.}
#'   \item{claycontent}{Percent clay gathered from OpenLandMap (250m) and averaged over 200cm.}
#'   \item{sandcontent}{Percent sand gathered from OpenLandMap (250m) and averaged over 200cm.}
#'   \item{phw}{The pH of soil water gathered from OpenLandMap (250m) and averaged over 200cm.}
#'   \item{watercontent}{Percent water gathered from OpenLandMap (250m) and averaged over 200cm.}
#'   \item{carboncontent}{Percent soil organic carbon gathered from OpenLandMap (250m) and averaged over 200cm.}
#'   \item{pro}{Measured grain protein concentration (%).}
#'   \item{dep_N_kgha}{Deposition of atmospheric N (kg/ha).}
#'   \item{sim_crop_N_kgm2}{Simulated crop N (kg/m2) from Jackson et al. 2005.}
#'   \item{crop_N_kgm2}{Measured crop N (kg/m2).}
#'   \item{yld_kgha}{Measured crop yield (kg/ha).}
#'   \item{rx_N_kgha}{Prescribed N fertilizer (kg/ha).}
#'   \item{aa_N_kgha}{As-applied N fertilizer (kg/ha).}
#'   \item{aa_N_kgm2}{As-applied N fertilizer (kg/m2).}
#'   \item{sum_soil_N_kgha.march}{Sum of the soil total N across the top 30cm in March in kg/ha.}
#'   \item{sum_soil_N_kgha.august}{Sum of the soil total N across the top 30cm in August in kg/ha.}
#'   \item{sum_Nmin_kgha}{Sum of the mineralized N (kg/ha) across the top 30cm, averaged across March and August measurements of percent TN according to Vigil et al. 2002.}
#'   \item{dep_N_kgm2}{Deposition of atmospheric N (kg/m2).}
#'   \item{sim_crop_N_kgha}{Simulated crop N (kg/ha) from Jackson et al. 2005.}
#'   \item{crop_N_kgha}{Measured crop N (kg/ha).}
#'   \item{FUE}{Fertilizer use efficiency, calculated as Ncrop / Nfert.}
#'   \item{NUE}{Nitrogen use efficiency, calculated as (Ncrop + Naugust) / (Nfert + Ndep + Nmin + Nmarch).}
#'   \item{NUE_noTN}{Nitrogen use efficiency without soil TN measures, calculated as (Ncrop) / (Nfert + Ndep + Nmin).}
#'   \item{WHC}{The WHC classification used as a covariate.}
#' }
"NUE_TrnDat"
