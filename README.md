# A Global Market for Marine Conservation


## Repository structure 

```
-- json_to_dot.py
-- make_p_to_json.py
-- README.md
-- renv
-- renv.lock
   |__activate.R
   |__library
      |__R-3.6
      |__R-4.0
      |__R-4.1
      |__R-4.2
   |__local
   |__settings.dcf
   |__staging
-- results
   |__img
      |__30_by_segment
      |__abs_gain_from_trade_segmented_market_plot.pdf
      |__abs_gain_from_trade_segmented_market_plot.png
      |__benefit_and_cost_maps
      |__change_in_area.pdf
      |__change_in_area.png
      |__correlogram_exvessel_price.pdf
      |__correlogram_exvessel_price.png
      |__dummy_plots
      |__equilibrum_supply_curves_hem.pdf
      |__equilibrum_supply_curves_hem.png
      |__equilibrum_supply_curves_rlm.pdf
      |__equilibrum_supply_curves_rlm.png
      |__equilibrum_supply_curves.pdf
      |__equilibrum_supply_curves.png
      |__gain_from_trade_segmented_market_plot.pdf
      |__gain_from_trade_segmented_market_plot.png
      |__gains_from_trade_panel.pdf
      |__gains_from_trade_panel.png
      |__hsi_hist.pdf
      |__hsi_hist.png
      |__map_contrasting_scenarios_global.pdf
      |__map_contrasting_scenarios_global.png
      |__map_contrasting_scenarios_rlm.pdf
      |__map_contrasting_scenarios_rlm.png
      |__map_of_trade_hem.pdf
      |__map_of_trade_hem.png
      |__map_of_trade_rlm.pdf
      |__map_of_trade_rlm.png
      |__map_of_trade.pdf
      |__map_of_trade.png
      |__rel_gain_from_trade_segmented_market_plot.pdf
      |__rel_gain_from_trade_segmented_market_plot.png
      |__savings_30.pdf
      |__savings_30.png
      |__savings_map.pdf
      |__savings_map.png
      |__segment_gains.pdf
      |__segment_gains.png
      |__supply_curves
      |__trading_units
   |__output_data
      |__bau_and_mkt_otucomes_global_30.rds
      |__gains_from_trade_bubbles.rds
      |__trade_outcomes
   |__processed_data
      |__master_costs_and_benefits.rds
      |__supply_curves
   |__tab
      |__gains_from_trade_hem.tex
      |__gains_from_trade_rlm.tex
      |__gains_from_trade.tex
      |__pro_gains_from_trade.tex
      |__trading_prices_hem.tex
      |__trading_prices_pro.tex
      |__trading_prices_rlm.tex
      |__trading-prices.tex
-- scripts
   |__00_run_all.R
   |__00_setup
      |__01_setup.R
      |__02_functions.R
   |__01_cleaning
      |__00_run_all_cleaning.R
      |__01_clean_EEZs.R
      |__02_clean_MEOWs.R
      |__03_clean_world_seas.R
      |__04_create_base_raster.R
      |__05_create_ocean_mask.R
      |__06_create_hemisphere_raster.R
      |__07_create_hemisphere_geopackage.R
      |__08_clean_mpa_atlas.R
      |__09_download_thredds_aquamaps.R
   |__02_processing
      |__00_run_all_processing.R
      |__01_generate_pixel_benefits.R
      |__02_combine_catch_and_prices.R
      |__03_generate_pixel_costs.R
      |__04_intersect_eez_and_meow.R
      |__05_rasterize_eez_and_meow.R
      |__06_rasterize_mpas.R
      |__07_combine_all_layers.R
      |__08_build_supply_curves_no_MPAs.R
      |__09_build_supply_curves_with_MPAs.R
   |__03_descriptive_content
      |__00_run_all_descriptive.R
      |__02_plot_trading_units.R
      |__03_plot_global_benefits_and_costs.R
      |__04_mex_us_example_benefits_costs_bcr.R
      |__05_plot_hsi_histogram.R
   |__04_analysis
      |__01_bau_and_mkt_outcomes_global_30.R
      |__02_all_segments_all_scenarios.R
   |__05_figures
      |__01_plot_supply_curves.R
      |__02_savings_map_global_30.R
      |__03_sources_of_efficiency_global_30.R
      |__04_savings_by_bubble_multiple_targets.R
      |__05_two_states_map_global.R
      |__06_change_in_area.R
   |__99_others
      |__01_draw_dummy_supply_curves.R
      |__meeting_pct_test.R
-- transferable_conservation.Rproj
```

---------
