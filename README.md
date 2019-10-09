# Post-fire forest management
A project to evaluate the effects of management practices (particularly tree planting) on post-fire forest recovery.

This repository contains the code and data associated with the quantitative/analytical aspects of the project. Large data files that cannot be synced to this repository are located here: https://ucdavis.box.com/v/pfm-non-synced. Once the GitHub repo is cloned to a local computer, the "non-synced" Box folder can simply be copied into the "data" folder.

Files not related to analytical components of the project (e.g., field visit notes and presentations) are stored in the project Box folder: https://ucdavis.box.com/v/postfire-management. Contact me for access.

### Summary of repository organization, key files, and data details

The data files for analysis are in `data/field-processed/compiled-processed/`. These data files have been cleaned and pre-processed based on raw data (e.g., fuels data has been converted from counts to Mg/ha; density of subsampled trees has been increased to account for subsampling). The raw entered data are in `data/field-raw`. The scripts to process and compile the raw data are `scripts/field-data-carpentry/compile_field_data.R` and then `scripts/field-data-carpentry/extract_gis_data_at_plots.R`.

There are multiple .csv data files to accommodate the nested structure of the data. Tables can be loaded and joined in R using the column PlotID.

Important aspects of plot, tree, and seedling data:
* Plot area has been computed based on slope area (what we measured on the ground) and horizontal area (what you see from above). The area is different for seedlings vs. pre-fire trees due to the way we masured distances for each. Therefore, we should work with densities, not counts.
* Plot type can be "treated", "untreated", or "internal". Internal plots may be planted ("treated") or unplanted and are not paired with other plots. They exist primarily on the Cottonwood Fire.
* Climate and topoclimate data (extracted by script `scripts/field-data-carpentry/extract_gis_data_at_plots.R`) are in the processed plots data file (`data/field-processed/compiled-processed/plots_w_gis_data.R`).
* The seedling data file to use is `seedlings_plot.csv`. The other has seedlings that were <25 cm and thus only sampled in the transects.
* Each row of seedling data represents a seedling surveyed in the plot. However, because some plots were subsampled, we need to interpret seedlings as densities, and when aggregating seedling values to a plot-level summary, we need to use those density values rather than counts. Each seedlings has two density values, one computed based on the slope-area of the plot (the amount of ground area), and one computed based on the horizontal area (the area from above). For seedlings, I think slope area makes the most sense.
* Examples of some basic analysis (including dealing with seedling densities as opposed to counts) is in the script `scripts/field-data-analysis/field_data_analysis.R`

Spatial data (plot perimeters and stem maps) are in `data/field-processed/spatial/`

<details><summary>Detailed description of plot selection-related scripts and data files</summary>
<p>

The list below describes how files are organized in this repo, with **_bold italic_** text indicating folders and **bold** text indicating files.

  * **_data_**: data files used by and produced by scripts/analysis
  
    * **_site-selection_**: data used for evaluating managmenet history and selection study sites
  
      * **_analysis-parameters_**: global parameters to be used by all scripts (e.g., focal fires)
      
        * **focal_fires.csv**: list of fires (named by USFS VB_ID) to evaluate for management history and candidate plots
        
      * **_input_**: datasets used purely as input; not created through scripts/analysis
      
      * **_output_**: datasets produced by scripts/analysis
      
        * **_aggregated-management-history_**: summaries of management history based on FACTS data (including text-based and geospatial)
        
          * **_shapefiles_**: geospatial representation of management history
            
            * **management_history.gpkg**: FACTS management for focal fires summarized in a flattened layer of management "slices" in which the entire area had the same management history and no features (polygons) overlap. This allows evaluation of the management applied to a given area without having to look at all FACTS polygons that overlap the area. For example, two FACTS polygons (planting and salvage) that partially overlap would be converted in to three "slices": one that is planting-only, one that is salvage-only, and on that is planting-and-slavage. Created by script aggregate_postfire_management_spatial.R
            
            * **management_history_summarized.gpkg**: same as above file but with additional columns derived from the columns in the above file (e.g. a logical column indicating whether a fire was planted but not salvaged). Created by script summarize_aggregated_postfire_management_spatial.R
            
          * **aggregated_management_history.csv**: Summaries of each management "slice" (e.g., number of times and years planted, slavaged, released, etc.). Management "slices" are defined above. Created by script aggregate_postfire_management_spatial.R
          
          * **aggregated_management_history_summarized.csv**: Summaries of management history at the fire level. Created by script "summarize_aggregated_postfire_management.R"
        
        * **_candidate-plots_**: geospatial layers and statistical summaries of candidate study plots
        
        * **_fire-names_**: exported list of fire names in the USFS R5 fire perimeter database
        
        * **_salvage-overlap-planting_**: geospatial layers of planting units and salvage units that overlap them

    * **_non-synced_**: data files that are too large to sync on GitHub. These files are stored in the project's Box folder "non-synced repo files": https://ucdavis.box.com/v/pfm-non-synced (contact me for access). The file structure can be copied directly from Box into this folder "non-synced" and it should work for all scripts


* **_scripts_**

  * **_site-selection_**: Scripts related to selection of plots to survey
  
    * **_data-carpentry_**: Scripts to prepare data from existing (public) sources and produce datasets that can be used directly for project-related purposes
    
      * **merge_pseudo-FACTS.R**: Take the multiple shapefiles (each representing different mangement trajectories) of FACTS management (some with different column names) from here: (add URL) and merge into a single shapefile
      
      * **aggregate_postfire_management_spatial.R**: Take the merged pseudo-FACTS layer (produced by the above script) and summarize it in a flattened layer of management "slices" in which the entire area had the same management history and no features (polygons) overlap. The output file is "management_history.gpkg" (see more detailed description of this file above) and "aggregated_management_history.csv" which is text (not spatial)-based data containing the attributes (management history and focal fire) of each FACTS slice.
      
      * **summarize_aggregated_postfire_management.R**: Take the "aggregated_management_history.csv" file produced by the above script and summarize, at the fire level, the proportion of area that was salvaged, planted, and the number of times and years that different management was applied. Output file is "aggregated_management_history_summarized.csv"
      
      * **summarize_aggregated_postfire_management_spatial.R**: Add additional fields (derived from operations on the original fields) to the aggregated management history geospatial layer (management_history.gpkg). Example derived field is "not salvaged but planted". Output file is "management_history_summarized.gpkg"
      
    * **paired_plot_selection_spatial.R**: Take the summarized (flattened) layer of FACTS "slices" as well as other input layers such as fire perimeters, severity, DEM, and ownership and produce sets of paired candidate sampling points that are on opposite sides of a planting boundary but otherwise comparable. Output file (geospatial) is "candidate_plots_paired.gpkg"

</p>
</details>

### Data compilation workflow

This section describes the scripts used to compile and prepare the data for analysis. These scripts produce the data stored in `data/field-processed/compiled-processed` and `data/field-processed/spatial`. The scripts for compiling/preparing the data are in `scripts/field-data-carpentry`.

1. `compile_field_data.R`: Loads raw entered field data (from `data/field-raw`). Calculates distances and areas (slope and horizontal). Stores an intermediate set of data files in `data/field-processed/compiled-uncleaned` but this should never be needed for direct use. Cleans data by correcting typos (fixes are hard-coded into the script). Stores another intermediate set of data files in `data/field-processed/compiled-cleaned` but this should also never be needed for direct use. Processes data by calculating fuel loads, calculating tree densities, calculating pre-fire oak distances. Store status flags like "drone subsample" and "internal", "treatment", or "control", names of fires, etc. Write processed data (for analysis) to `data/field-processed/compiled-processed`. Creates and writes spatial data (seedling stem maps, plot perimeters, plot points) to `data/field-processed/spatial`.

2. `extract_gis_data_at_plots.R`: At each plot location, extracts data from GIS layers, including elevation, slope, aspect, solar radiation, precip, management history. Uses the helper script `extract_FACTS_species.R` to obtain the species that were planted in each plot according to FACTS records. Summarizes management history. Writes augmented plot data (which can be used for analysis) as `data/field-processed/compiled-processed/plots_w_gis_data.csv`.
