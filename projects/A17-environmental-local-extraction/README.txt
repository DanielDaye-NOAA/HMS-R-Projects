R/projects/A17-environmental-local-extraction/

- Contains all of the R data files required to run the environmental extraction and association
- Some additional files include analysis on the environmental data (summarizing by species, life stage), or environmental analysis of the data provided by GULFSPAN contributors

-----------------------------------

/batch-outfiles/ contains output files (.txt) that were generated during the environmental extraction/association process. Not important to keep but were used as a visual check to see how far along the process the extractions were, as R no longer prints to the console when running in parallel.

/data/ contains a variety of data files - mostly .xlsx or .csv, some shapefiles. Used in some R scripts for comparisons to point data or to summarize data from GULFSPAN surveys (some of this is used in the HMS indicators development for 2025 SAFE report).

/envdata/ contains a few NetCDF files that were used to develop the R scripts, but the whole date range of environmental data was stored elsewhere.

/extractions/ contains observational data that has been associated with remote environmental data. Each file corresponds to a batch of data that was extracted at once. There are a few files that are consolidated across batches. Names should be self-explanatory. Saved as both .xlsx and .rds files.

/figures/ contains bar and whisker plots for all of the environmental data that was associated with EFH data. Also include a bathymetry map and rugosity map as references for the datasets.

/GULFSPAN-summaries/ Some additional follow-up analysis was run separately on GULFSPAN data that we received for each region. This folder contains the summaries of GULFSPAN data by species and life stage.

/html/ HTML files produced by R Quarto documents.

/output/ Some product files produced by R scripts.

/quarto/ Contains a quarto doc used to produce some monthly visualizations for EFH data.

/r-files/ contains all R scripts that were included as this part of A17. There are comments which provide a better explanation within each script
- BatchEnvAssign.R is the function used to associate environmental data with EFH observations
- ConvertListToDF.R is a helper function to help transform List data produced by BatchEnvAssign into a long data frame
- GenerateTextDescriptions.R is the function used to generate preliminary text descriptions for EFH species / life stages from environmental data
- Grubbs-GULFSPAN-summary.R and Hendon-GULFSPAN-summary.R are the files used to do analysis on specific GULFSPAN regions (in-situ environmental data)
- env-boxplots.R contains code to generate the figures in /figures/
- env-data-consolidation.R just contains the code that combines extractions into data frames and specific year ranges
- environmental-data-association-script.R contains all the code relevant to EFH extractions. This had to be done in batches so it was a way to organize and keep track of the overall progress of the extraction process
- gulf-blacktip-juvadu-example.R contains the code needed to generate the figures in the EFH methods appendix which walk through the process of creating EFH boundaries from observational data
- summarizing-data.R has the scripts which generate the final outputs for text descriptions (GenerateTextDescriptions) and tabular summaries