library(sf)

#### Load necessary layers ####

# load FACTS planting history slices of focal fires


# extract names of focal fires


# load fire database and thin to focal fires


# load full FACTS (not just planting) and clip to focal fires
#    we need this to avoid putting plots in areas that were managed


# load fire severity layers and thin to focal fires

# load DEM and comput slope and aspect




#### Place points ####

# Buffer out the planting units by 0.5 m so they merge back together

# Buffer in by 20 m and place points along the resulting perimeters to establish the candidate set of "treated" plots

# Buffer out by 20m and place points along the resulting perimeters to estalish the candidate set of "control" plot
# Remove candidate control plots that are in an area with any FACTS management history (beyond planting)

# Get slope, aspect, elevation, and fire severity of all control and treated plots

# include only candidate plots that have high severity surrounding them

# exclude plot pairs that did not match in terms of slope, aspect, elevation, and fire severity

# if multiple plot pairs from the same planting slice are within X distance, randomly remove them until they are > X distance

# summarize the planting pairs: planted-conttrol both salvaged or not, or different? replanted, shrub control, etc?



