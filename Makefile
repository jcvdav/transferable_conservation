output: results/output_data/bau_and_mkt_otucomes_global_30.rds results/output_data/gains_from_trade_bubbles.rds results/output_data/data_for_science_figures.csv
input: results/processed_data/master_costs_and_benefits.rds results/processed_data/supply_curves/with_mpas/ecoregion_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/ecoregion_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/global_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/global_supply_curve_with_mpas.rds results/processed_data/supply_curves/with_mpas/hemisphere_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/hemisphere_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/province_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/province_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/realm_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/realm_supply_curves_with_mpas.rds
dag: makefile-dag.png

# Combine all source data layers
results/processed_data/master_costs_and_benefits.rds: scripts/02_processing/07_combine_all_layers.R clean_data/suitability.tif clean_data/revenue_raster.tif clean_data/eez_raster.tif clean_data/rlm_raster.tif clean_data/pro_raster.tif clean_data/eco_raster.tif clean_data/hemispheres.tif clean_data/mpa_raster.tif
		cd $(<D);Rscript $(<F)

# Build conservation supply curves
results/processed_data/supply_curves/with_mpas/ecoregion_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/ecoregion_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/global_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/global_supply_curve_with_mpas.rds results/processed_data/supply_curves/with_mpas/hemisphere_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/hemisphere_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/province_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/province_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/realm_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/realm_supply_curves_with_mpas.rds: scripts/02_processing/08_build_supply_curves_with_MPAs.R results/processed_data/master_costs_and_benefits.rds
		cd $(<D);Rscript $(<F)

# Initial analysis for global 30X30 (figures were moved to supplements)
results/output_data/bau_and_mkt_otucomes_global_30.rds: scripts/04_analysis/01_bau_and_mkt_outcomes_global_30.R results/processed_data/supply_curves/with_mpas/global_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/global_supply_curve_with_mpas.rds
		cd $(<D);Rscript $(<F)
		
# Full analysis (all bubble policies, all conservation targets)
results/output_data/gains_from_trade_bubbles.rds: scripts/04_analysis/02_all_segments_all_scenarios.R results/processed_data/supply_curves/with_mpas/ecoregion_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/ecoregion_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/global_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/global_supply_curve_with_mpas.rds results/processed_data/supply_curves/with_mpas/hemisphere_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/hemisphere_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/province_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/province_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/realm_eez_supply_curves_with_mpas.rds results/processed_data/supply_curves/with_mpas/realm_supply_curves_with_mpas.rds
		cd $(<D);Rscript $(<F)

# Minimal CSV file for Science to reproduce the figures we had created for the main text
results/output_data/data_for_science_figures.csv: scripts/05_figures_and_tables/00_minimal_figure_reproduction_for_science.R results/output_data/gains_from_trade_bubbles.rds
		cd $(<D);Rscript $(<F)

makefile-dag.png: Makefile
	LANG=C make -pBnd | python3 make_p_to_json.py | python3 json_to_dot.py | dot -Tpng -Gdpi=300 -o makefile-dag.png