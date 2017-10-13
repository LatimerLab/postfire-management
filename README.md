# postfire-management

## Repo file structure:

  * data: data files used by and produced by scripts/analysis
  
    * site-selection: data used for evaluating managmenet history and selection study sites
  
      * analysis-parameters: global parameters to be used by all scripts (e.g., focal fires)
      
        * focal_fires.csv: list of fires (named by USFS VB_ID) to evaluate for management history and candidate plots
        
      * input: datasets used purely as input; not created through scripts/analysis
      
      * output: datasets produced by scripts/analysis
      
        * aggregated-management-history: summaries of management history based on FACTS data (including text-based and geospatial)
        
          * shapefiles: geospatial representation of management historu
            
            * management_history.gpkg: FACTS management for focal fires summarized in a flattened layer of management "slices" in which the entire area had the same management history. This allows evaluation of the management applied to a given area without having to look at all FACTS polygons that overlap the area. For example, two FACTS polygons (planting and salvage) that partially overlap would be converted in to three "slices": one that is planting-only, one that is salvage-only, and on that is planting-and-slavage. Created by script aggregate_postfire_management_spatial.R
            
            * management_history_summarized.gpkg: same as above file but with additional columns derived from the columns in the above file (e.g. a logical column indicating whether a fire was planted but not salvaged). Created by script summarize_aggregated_postfire_management_spatial.R
            
          * aggregated_management_history.csv: Summaries of each management "slice" (e.g., number of times and years planted, slavaged, released, etc.). Management "slices" are defined above. Created by script aggregate_postfire_management_spatial.R
          
          * aggregated_management_history_summarized: Summaries of management history at the fire level. Created by script "summarize_aggregated_postfire_management.R"
        
        * candidate-plots: geospatial layers of candidate study plots
        
        * fire-names: exported list of fire names in the USFS R5 fire perimeter database
        
        * salvage-overlap-planting: geospatial layers of planting units and salvage units that overlap them

    * non-synced: data files that are too large to sync on GitHub. These files are stored in the project's Box folder "non-synced repo files": https://ucdavis.box.com/v/pfm-non-synced (email for)
    
      * existing-datasets: datasets taken directly from (or minimally-processed from) public sources











Scripts:

salvage-planting-overlap.R: Use FACTS data to distinguish planted areas based on whether there was any salvage logging associated with them
