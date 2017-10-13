# postfire-management

## Repo file structure:

  * *_data_*: data files used by and produced by scripts/analysis
  
    * *_site-selection_*: data used for evaluating managmenet history and selection study sites
  
      * *_analysis-parameters_*: global parameters to be used by all scripts (e.g., focal fires)
      
        * *focal_fires.csv*: list of fires (named by USFS VB_ID) to evaluate for management history and candidate plots
        
      * *_input_*: datasets used purely as input; not created through scripts/analysis
      
      * *_output_*: datasets produced by scripts/analysis
      
        * *_aggregated-management-history_*: summaries of management history based on FACTS data (including text-based and geospatial)
        
          * *_shapefiles_*: geospatial representation of management historu
            
            * *management_history.gpkg*: FACTS management for focal fires summarized in a flattened layer of management "slices" in which the entire area had the same management history. This allows evaluation of the management applied to a given area without having to look at all FACTS polygons that overlap the area. For example, two FACTS polygons (planting and salvage) that partially overlap would be converted in to three "slices": one that is planting-only, one that is salvage-only, and on that is planting-and-slavage. Created by script aggregate_postfire_management_spatial.R
            
            * *management_history_summarized.gpkg*: same as above file but with additional columns derived from the columns in the above file (e.g. a logical column indicating whether a fire was planted but not salvaged). Created by script summarize_aggregated_postfire_management_spatial.R
            
          * *aggregated_management_history.csv*: Summaries of each management "slice" (e.g., number of times and years planted, slavaged, released, etc.). Management "slices" are defined above. Created by script aggregate_postfire_management_spatial.R
          
          * *_aggregated_management_history_summarized_*: Summaries of management history at the fire level. Created by script "summarize_aggregated_postfire_management.R"
        
        * *_candidate-plots_*: geospatial layers of candidate study plots
        
        * *_fire-names_*: exported list of fire names in the USFS R5 fire perimeter database
        
        * *_salvage-overlap-planting_*: geospatial layers of planting units and salvage units that overlap them

    * *_non-synced_*: data files that are too large to sync on GitHub. These files are stored in the project's Box folder "non-synced repo files": https://ucdavis.box.com/v/pfm-non-synced (contact me for access). The file structure can be copied directly from Box into this folder "non-synced" and it should work for all scripts











Scripts:

salvage-planting-overlap.R: Use FACTS data to distinguish planted areas based on whether there was any salvage logging associated with them
