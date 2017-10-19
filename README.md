# Post-fire forest management
A project to evaluate the outcomes of management practices (particularly tree planting) on post-fire forest recovery

This repository contains the code and data associated with the quantitative aspects of the project (currently, selection of candidate study sites). Large data files that cannot be synced to this repository are located here: https://ucdavis.box.com/v/pfm-non-synced. Once the GitHub repo is cloned to a local computer, the "non-synced" Box folder can simply be copied into the "data" folder.

Files not related to analytical components of the project (e.g., field visit notes and presentations) are stored in the project Box folder: https://ucdavis.box.com/v/postfire-management. Contact me for access.

### Description of GitHub repository file structure
The list below describes how files are organized in this repo, with **_bold italic_** text indicating folders and **bold** text indicating files.

  * **_data_**: data files used by and produced by scripts/analysis
  
    * **_site-selection_**: data used for evaluating managmenet history and selection study sites
  
      * **_analysis-parameters_**: global parameters to be used by all scripts (e.g., focal fires)
      
        * **focal_fires.csv**: list of fires (named by USFS VB_ID) to evaluate for management history and candidate plots
        
      * **_input_**: datasets used purely as input; not created through scripts/analysis
      
      * **_output_**: datasets produced by scripts/analysis
      
        * **_aggregated-management-history_**: summaries of management history based on FACTS data (including text-based and geospatial)
        
          * **_shapefiles_**: geospatial representation of management historu
            
            * **management_history.gpkg**: FACTS management for focal fires summarized in a flattened layer of management "slices" in which the entire area had the same management history and no features (polygons) overlap. This allows evaluation of the management applied to a given area without having to look at all FACTS polygons that overlap the area. For example, two FACTS polygons (planting and salvage) that partially overlap would be converted in to three "slices": one that is planting-only, one that is salvage-only, and on that is planting-and-slavage. Created by script aggregate_postfire_management_spatial.R
            
            * **management_history_summarized.gpkg**: same as above file but with additional columns derived from the columns in the above file (e.g. a logical column indicating whether a fire was planted but not salvaged). Created by script summarize_aggregated_postfire_management_spatial.R
            
          * **aggregated_management_history.csv**: Summaries of each management "slice" (e.g., number of times and years planted, slavaged, released, etc.). Management "slices" are defined above. Created by script aggregate_postfire_management_spatial.R
          
          * **_aggregated_management_history_summarized_**: Summaries of management history at the fire level. Created by script "summarize_aggregated_postfire_management.R"
        
        * **_candidate-plots_**: geospatial layers of candidate study plots
        
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