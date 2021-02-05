# Data & Code: “The Total Return and Risk to Residential Real Estate”

_by Piet Eichholtz, Matthijs Korevaar, Thies Lindenthal and Ronan Tallec_

For questions and clarifications about the files and routines, please contact korevaar@ese.eur.nl

There are three separate folders:
* [figures](figures) contains the main figures that are used in the paper, both in the main body of the paper and in the appendix,
* [data](data) contains data files, and
* [code](code) all code.

## General series

The main return series in the paper for both Paris and Amsterdam are provided in the files [TimeSeriesParisAnalysis.csv](data/TimeSeriesParisAnalysis.csv) and [TimeSeriesAmsAnalysis.csv](data/TimeSeriesAmsAnalysis.csv). The data used in the paper only covers the 1809-1943 period (Paris) and the 1900-1979 period (Amsterdam).

The main datafiles with processed raw data are [DataParisAnalysis.csv](data/DataParisAnalysis.csv) (Paris) and [DataAmsterdamInput.csv](data/DataAmsterdamInput.csv) (Amsterdam). These are used to created the yield series.

Finally, the code [TableStatsAll.R](code/TableStatsAll.R) computes dozens of different summary statistics on the series, most of which are used in the paper. The [Graphs.R](data/Graphs.R) construct the various return graphs. Other graphs are constructed in individual files, such as [yields.nbh.ams.R](code/yields.nbh.ams.R) or [yields.nbh.par.R](code/yields.nbh.par.R), which produce the yield maps for Paris and Amsterdam.

## City-Specific Code and Data

### Paris

* [ParisSelData.R](code/ParisSelData.R) transforms the raw input file [DataParisInput.csv](data/DataParisInput.csv) into the file [DataParisAnalysis.csv](data/DataParisAnalysis.csv) which will be used for most of the analysis, and transformed in other files (note that running this overwrites the final [DataParisAnalysis.csv](data/DataParisAnalysis.csv) ).

* [ParisIndexConstruction.R](code/ParisIndexConstruction.R) contains the routines to estimate the price indices for capital gains (Section 3) and rent prices (Appendix B)

* [ParisYieldConstruction.R](code/ParisIndexConstruction.R) contains the routines to estimate the various gross yield series in the paper, as well as the robustness analyses for these, and various inputs. It modifies the [DataParisAnalysis.csv](data/DataParisAnalysis.csv).

* [TimeSeriesParisAnalysis.csv](data/TimeSeriesParisAnalysis.csv) contains the final time series used in the paper, after processing all the data, as well as existing series. These existing series are used to remove outliers in the novel series.
* [ParisTax18551926.csv](data/ParisTax18551926.csv) and [ParisTax18101860.csv](data/ParisTax18101860.csv) contain the source data used to estimate the tax properties. These are constructed in the file [ParisYieldConstruction.R](code/ParisIndexConstruction.R)

### Amsterdam

* [AmsIndexConstruction.R](code/AmsIndexConstruction.R) computes the Amsterdam capital gains index. It uses [DataAmsRepeatSales.csv](data/DataAmsRepeatSales.csv) as input file as well as the [AmsRPI.csv](data/AmsRPI.csv) which contains the rent index.

* [AmsYieldConstruction.R](code/AmsYieldConstruction.R) computes the Amsterdam yields, as well as the tax fractions and the cost analysis. It uses [DataAmsterdamInput.csv](data/DataAmsterdamInput.csv) as main input file. For the cost data, it uses “AmsCostData.csv”

* [AmsIdioRisk.R](code/AmsIdioRisk.R) computes the idiosyncratic risk computations in the main paper, as well as all the robustness checks in the appendix. It uses [DataAmsterdamInput.csv](data/DataAmsterdamInput.csv) as main data input file. It also uses [geocoded.addresses.amsterdam.csv](data/geocoded.addresses.amsterdam.csv) as input file to the neigborhoodlevel analysis.
