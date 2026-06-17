# How to retrieve data from the INBOVEG database

## Introduction

The Flemish vegetation database, INBOVEG, is an application developed to
provide a repository of relevés and makes the relevés available for
future use. More information? Check out
<https://www.vlaanderen.be/inbo/en-gb/data-applications/inboveg/>

INBOVEG supports different types of recordings: `BioHab` recordings
(protocol of Natura 2000 monitoring) and the classic relevés. The
classic relevés can stand alone, be an element of a collection or
element of a chain where the linkage is used to give information about
the relative position of recording within a series. Ample selection and
export functions toward analysis tools are provided. It also provides
standardized lists of species, habitats, life forms, scales … Original
observations are preserved and a full history of subsequent
identifications is saved.

More information? Check out
[INBOVEG-website](https://www.vlaanderen.be/inbo/en-gb/data-applications/inboveg/)

## Aim

We make functions available to query data directly from the INBOVEG
SQL-server database. This avoids writing your own queries or to
copy/paste them from the access-frontend for INBOVEG.

We have provided functions to query

- survey (INBOVEG-projects)
- metadata of recordings (header info)
- recordings (vegetation relevés)
- layer data (cover or qualifiers)
- classification (Natura2000 or local classification like BWK)
- qualifiers (management and site characteristics)
- relations between recordings (relation information on recordings for
  one or more surveys based on Parent (classic-chain/bucket) and Child
  (classic) relationships)

## Packages and connection

The main functions that we will use in this tutorial all start with
`get_inboveg_*`. These functions are made available by loading the
`inbodb` package.

``` r

library(inbodb)
```

These functions will only work for people with access to the INBO
network. As an INBO employee, you should make sure you have
reading-rights for CYDONIA, otherwise place an ICT-call.

The following R-code can be used to establish a connection to INBOVEG by
using `connect_inbo_dbase` of the inbodb-package with the database
‘Cydonia’ on the `inbo-sql07-prd` server:

``` r

con <- connect_inbo_dbase("D0010_00_Cydonia")
```

## Functionality

### Survey information

The function `get_inboveg_survey` queries the INBOVEG database for
survey information (metadata about surveys) for one or more survey(s) by
the name of the survey.

Three examples are given, this can be used as base to continue selecting
the data you require.

Get information of a specific survey and collect data.

``` r

survey_info <- get_inboveg_survey(
  con,
  survey_name = "OudeLanden_1979",
  collect = TRUE
)
```

``` r

survey_info
#> # A tibble: 1 × 5
#>      Id Name            Description                                Owner creator
#>   <int> <chr>           <chr>                                      <chr> <chr>  
#> 1   172 OudeLanden_1979 Verlinden A, Leys G en Slembrouck J (1979… NA    els_de…
```

Get information of all surveys. This time we will not use
`collect = TRUE`, which will return a [lazy
query](https://dbplyr.tidyverse.org/reference/tbl.src_dbi.html):

``` r

allsurveys <- get_inboveg_survey(con)
```

``` r

class(allsurveys)
#> [1] "tbl_Microsoft SQL Server" "tbl_dbi"                 
#> [3] "tbl_sql"                  "tbl_lazy"                
#> [5] "tbl"
```

If only a part of the survey name is known, you can make use of
wildcards such as `%`.

``` r

partsurveys <- get_inboveg_survey(
  con,
  survey_name = "%MILKLIM%",
  collect = TRUE
)
```

``` r

partsurveys
#> # A tibble: 31 × 5
#>       Id Name                                    Description       Owner creator
#>    <int> <chr>                                   <chr>             <chr> <chr>  
#>  1     4 MILKLIM_Alopecurion                     Standplaatsonder… MILK… maud_r…
#>  2     5 MILKLIM_WZ_AalstTeralfene               Opnamen van PQ's… MILK… floris…
#>  3     6 MILKLIM_Hei(schraal)herstel             PQ's in kader va… MILK… floris…
#>  4     7 MILKLIM_Heide                           Standplaatsonder… MILK… jan_wo…
#>  5     8 MILKLIM_W&Z_Leiebermen_2010_2013        Evaluatie maaibe… MILK… maud_r…
#>  6     9 MILKLIM_W&Z_Geraardsbergen              Vegetatieopnames… MILK… luc_va…
#>  7    10 MILKLIM_Heischraal2012                  Vegetatieopnames… MILK… cecile…
#>  8    11 MILKLIM_W&Z_Varia                       Losse opnamen in… MILK… floris…
#>  9    12 MILKLIM_W&Z_Bermen_AfleidingskanaalLeie Ecologische opvo… MILK… els_de…
#> 10    14 MILKLIM_W&Z_OeversLeie                  Oeveropnamen lan… Maud… luc_va…
#> # ℹ 21 more rows
```

### Header information

The function `get_inboveg_header` queries the INBOVEG database for
header information (metadata for a vegetation-relevé) for one or more
survey by the name of the survey(s) and the recorder type (`classic`,
`classic-emmer`, `classic-ketting`, `biohab`, `ppa`). All relevés are
selected, also those with ‘needs work’ indicated (0 = no work needed, 1
= needs work). It is important to realize that the relevé is not
finished yet, species lists can be incomplete, or not all species have
been identified yet. For certain analysis these relevés can better be
excluded (by using dplyr -\> filter).

Three examples are given, this can be used as base to continue selecting
the data you require.

Get header information from a specific survey and a specific recording
type and collect the data:

``` r

header_info <- get_inboveg_header(
  con,
  survey_name = "OudeLanden_1979",
  rec_type = "Classic",
  collect = TRUE
)
```

``` r

head(header_info, 10)
#> # A tibble: 10 × 17
#>    RecordingGivid     NeedsWork SurveyName   UserReference Observer LocationCode
#>    <chr>                  <int> <chr>        <chr>         <chr>    <chr>       
#>  1 IV2016021012140632         0 OudeLanden_… 5             Alex Ve… Ekeren      
#>  2 IV2016021014064327         0 OudeLanden_… 1             Alex Ve… Ekeren      
#>  3 IV2016021014244819         0 OudeLanden_… 2             Alex Ve… Ekeren      
#>  4 IV2016021015310781         0 OudeLanden_… 4             Alex Ve… Ekeren      
#>  5 IV2016021015515026         0 OudeLanden_… 81            Alex Ve… Ekeren      
#>  6 IV2016021016074544         0 OudeLanden_… 20            Alex Ve… Ekeren      
#>  7 IV2016021016140673         0 OudeLanden_… 17            Alex Ve… Ekeren      
#>  8 IV2016021016201725         0 OudeLanden_… 19            Alex Ve… Ekeren      
#>  9 IV2016021016335063         0 OudeLanden_… 18            Alex Ve… Ekeren      
#> 10 IV2016021016484882         0 OudeLanden_… 23            Alex Ve… Ekeren      
#> # ℹ 11 more variables: Latitude <dbl>, Longitude <dbl>, Area <dbl>,
#> #   Length <int>, Width <int>, VagueDateType <chr>, VagueDateBegin <chr>,
#> #   VagueDateEnd <chr>, SurveyId <int>, RecTypeID <int>, RecTypeName <chr>
```

Get header information from several specific surveys by using
`multiple = TRUE`.

``` r

header_severalsurveys <- get_inboveg_header(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  multiple = TRUE
)
```

Get header information of all surveys without collecting the data:

``` r

all_header_info <- get_inboveg_header(con)
```

### Recording information

The function
[`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md)
queries the INBOVEG database for relevé information (which species were
recorded in which plots and in which vegetation layers with which cover)
for one or more surveys.

Several examples are given, this can be used as base to continue
selecting the data you require.

First, we show how to get the relevés from one survey and collect the
data:

``` r

recording_heischraal2012 <- get_inboveg_recording(
  con,
  survey_name = "MILKLIM_Heischraal2012",
  collect = TRUE
)
```

``` r

recording_heischraal2012
#> # A tibble: 3,288 × 13
#>    Name            RecordingGivid UserReference LayerCode CoverCode OriginalName
#>    <chr>           <chr>          <chr>         <chr>     <chr>     <chr>       
#>  1 MILKLIM_Heisch… IV20120816113… HS_1001       M         5         Rhytidiadel…
#>  2 MILKLIM_Heisch… IV20120816113… HS_1001       M         5         Pseudoscler…
#>  3 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Juncus acut…
#>  4 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Nardus stri…
#>  5 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Potentilla …
#>  6 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Anthoxanthu…
#>  7 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Molinia cae…
#>  8 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Lysimachia …
#>  9 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Luzula mult…
#> 10 MILKLIM_Heisch… IV20120816113… HS_1001       K         90        Carex pilul…
#> # ℹ 3,278 more rows
#> # ℹ 7 more variables: ScientificName <chr>, TaxonGroupCode <chr>,
#> #   PhenologyCode <chr>, Comment <chr>, CoverageCode <chr>, PctValue <dbl>,
#> #   RecordingScale <chr>
```

Here is how to get the relevé for a specific `RecordingGivid`:

``` r

recording_specific_givid <- get_inboveg_recording(
  con,
  recording_givid = "IV2012080609161322",
  collect = TRUE
)
```

Or for multiple `RecordingGivid`:

``` r

recording_specific_givid <- get_inboveg_recording(
  con,
  recording_givid = c("IV2012080609161322", "IV2012081611384756"),
  collect = TRUE
)
```

The above is very useful in tandem with
[`get_inboveg_header()`](https://inbo.github.io/inbodb/reference/get_inboveg_header.md)
to first search for all relevés belonging to one or more surveys and
further filter these records to only the relevés you require based on
the relevé metadata (such as date, observer, …).

Or based on a user reference:

``` r

recording_specific_userref <- get_inboveg_recording(
  con,
  user_reference = "HS_1001",
  collect = TRUE
)
```

Get all recordings from MILKLIM surveys (partial matching), without
collecting the data:

``` r

recording_milkim <- get_inboveg_recording(
  con,
  survey_name = "%MILKLIM%"
)
```

Get recordings from several specific surveys:

``` r

recording_severalsurveys <- get_inboveg_recording(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  collect = TRUE
)
```

Get all relevés of all surveys, without collecting the data:

``` r

allrecordings <- get_inboveg_recording(con)
```

The function
[`get_inboveg_ppa()`](https://inbo.github.io/inbodb/reference/get_inboveg_ppa.md)
is similar in use as
[`get_inboveg_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_recording.md),
but can be used specifically to get point to plant distance type data
(PPA) from the INBOVEG database.

``` r

allppa <- get_inboveg_ppa(con)
```

### Classification information

The function `get_inboveg_classification` queries the INBOVEG database
for information on the field classification (N2000 or BWK-code) of the
relevé for one or more survey(s) by the name of the survey.

Two examples are given, this can be used as base to continue selecting
the data you require.

Get a specific classification from a survey and collect the data:

``` r

classif_info <- get_inboveg_classification(
  con,
  survey_name = "MILKLIM_Heischraal2012",
  classif = "4010",
  collect = TRUE
)
```

``` r

classif_info
#> # A tibble: 1 × 9
#>   RecordingGivid     Name       Classif ActionGroup ListName LocalClassification
#>   <chr>              <chr>      <chr>   <chr>       <chr>    <chr>              
#> 1 IV2013031814425352 MILKLIM_H… 4010    N2k         Habitat… NA                 
#> # ℹ 3 more variables: Habitattype <chr>, Cover <chr>, PctValue <dbl>
```

Get the classification from several specific surveys

``` r

classif_info <- get_inboveg_classification(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  multiple = TRUE
)
classif_info
```

Get all surveys, all classifications without collecting the data:

``` r

all_codes <- get_inboveg_classification(con)
```

### Qualifiers information

The function
[`get_inboveg_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_qualifier.md)queries
the INBOVEG database for qualifier information on recordings for one or
more surveys. These qualifiers give information on management
(management qualifier `MQ`) or location description (site qualifier
`SQ`).

Get the qualifiers from one survey:

``` r

qualifiers_heischraal2012 <- get_inboveg_qualifier(
  con,
  survey_name = "MILKLIM_Heischraal2012"
)
```

``` r

head(qualifiers_heischraal2012)
#>                     Name     RecordingGivid UserReference
#> 1 MILKLIM_Heischraal2012 IV2012081615083167        HS_008
#> 2 MILKLIM_Heischraal2012 IV2012081613133274        HS_009
#> 3 MILKLIM_Heischraal2012 IV2013041614525228        HS_035
#> 4 MILKLIM_Heischraal2012 IV2012081609450300        HS_036
#> 5 MILKLIM_Heischraal2012 IV2012081610204607        HS_037
#> 6 MILKLIM_Heischraal2012 IV2012081610393743        HS_044
#>                                           Observer QualifierType Q1Code
#> 1                   Cécile Herr en Robin Guelinckx            MQ      A
#> 2                   Cécile Herr en Robin Guelinckx            MQ      A
#> 3 Cécile Herr, Patrik Oosterlynck, Robin Guelinckx            MQ      A
#> 4                   Cécile Herr en Robin Guelinckx            MQ      A
#> 5 Cécile Herr, Robin Guelinckx, Patrik Oosterlynck            MQ      A
#> 6                   Cécile Herr en Robin Guelinckx            MQ      A
#>   Q1Description Q2Code Q2Description   Q3Code Q3Description Elucidation NotSure
#> 1          <NA>  PBuis    Peilbuizen GUPP042A          <NA>                   0
#> 2          <NA>  PBuis    Peilbuizen GUPP043B          <NA>                   0
#> 3        Active  PBuis    Peilbuizen WALP161X          <NA>                   0
#> 4          <NA>  PBuis    Peilbuizen WALP157X          <NA>                   0
#> 5          <NA>  PBuis    Peilbuizen WALP117X          <NA>                   0
#> 6          <NA>  PBuis    Peilbuizen WALP162X          <NA>                   0
#>   ParentID  QualifierResource
#> 1       NA               <NA>
#> 2       NA               <NA>
#> 3       NA RS2012060811060080
#> 4       NA               <NA>
#> 5       NA               <NA>
#> 6       NA               <NA>
```

Get all site qualifiers (SQ) from MILKLIM surveys (partial matching):

``` r

qualifiers_milkim <- get_inboveg_qualifier(
  con,
  survey_name = "%MILKLIM%",
  qualifier_type = "SQ"
)
```

Get qualifiers from several specific surveys with `multiple = TRUE`:

``` r

qualifiers_severalsurveys <- get_inboveg_qualifier(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  multiple = TRUE
)
```

Get all qualifiers of all surveys:

``` r

allqualifiers <- get_inboveg_qualifier(con)
```

### Layer information

The function `get_inboveg_layer_cover` queries the INBOVEG database for
cover information per layer on recordings for one or more surveys.

Get the layer cover information from one survey

``` r

layerinfo_heischraal2012 <- get_inboveg_layer_cover(
  con,
  survey_name = "MILKLIM_Heischraal2012"
)
```

Get all layer cover information from MILKLIM surveys (partial matching)

``` r

layerinfo_milkim <- get_inboveg_layer_cover(
  con,
  survey_name = "%MILKLIM%"
)
```

Get layer cover information from several specific surveys

``` r

layerinfo_severalsurveys <- get_inboveg_layer_cover(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  multiple = TRUE
)
```

Get all layer cover information of all surveys

``` r

all_layerinfo <- get_inboveg_layer_cover(con)
```

### Layer qualifiers

The function
[`get_inboveg_layer_qualifier()`](https://inbo.github.io/inbodb/reference/get_inboveg_layer_qualifier.md)queries
the INBOVEG database for layer qualifier information on recordings for
one or more surveys.

Get the layer qualifiers from one survey

``` r

layerqualifier_gagealutea <- get_inboveg_layer_qualifier(
  con,
  survey_name = "GageaLutea_1980"
)
```

Get all layer qualifiers from MILKLIM surveys (partial matching)

``` r

layerqualifiers_milkim <- get_inboveg_layer_qualifier(
  con,
  survey_name = "%MILKLIM%"
)
```

Get layer qualifiers from several specific surveys

``` r

layerqualifiers_severalsurveys <- get_inboveg_layer_qualifier(
  con,
  survey_name = c("MILKLIM_Heischraal2012", "NICHE Vlaanderen"),
  multiple = TRUE
)
```

Get all layer qualifiers of all surveys

``` r

alllayerqualifiers <- get_inboveg_layer_qualifier(con)
```

### Parent-child relationships between recordings

The function
[`get_inboveg_relation_recording()`](https://inbo.github.io/inbodb/reference/get_inboveg_relation_recording.md)
queries the INBOVEG database for relational information on recordings
for one or more surveys based on parent (classic-chain/bucket) and Child
(classic) relationships.

Get all parent-child-relations from `N2000meetnet` surveys (partial
matching):

``` r

relations_n2000meetnet <- get_inboveg_relation_recording(
  con,
  survey_name = "%N2000meetnet%"
)

relations_n2000meetnet
```

    #> # Source:   SQL [?? x 7]
    #> # Database: Microsoft SQL Server 13.00.6419[INBO\frederic_piesschaer@INBO-SQL07-PRD\LIVE/D0010_00_Cydonia]
    #>    Name              RecordingId Child_GIVID Child_UserRef ParentId Parent_GIVID
    #>    <chr>                   <int> <chr>       <chr>            <int> <chr>       
    #>  1 N2000meetnet_Dui…       44283 IV20190218… 13594417_2170    44282 IV201902180…
    #>  2 N2000meetnet_Dui…       44285 IV20190218… 10334001_2190    44284 IV201902181…
    #>  3 N2000meetnet_Dui…       44287 IV20190218… 10186545_213…    44286 IV201902181…
    #>  4 N2000meetnet_Dui…       44290 IV20190218… 10186545_2170    44289 IV201902181…
    #>  5 N2000meetnet_Dui…       44292 IV20190218… 13561649_2170    44291 IV201902181…
    #>  6 N2000meetnet_Dui…       44294 IV20190218… 11595569_2170    44293 IV201902181…
    #>  7 N2000meetnet_Dui…       44296 IV20190218… 1240881_2170     44295 IV201902181…
    #>  8 N2000meetnet_Dui…       44298 IV20190218… 13823793_2170    44297 IV201902181…
    #>  9 N2000meetnet_Dui…       44300 IV20190218… 12644145_2170    44299 IV201902181…
    #> 10 N2000meetnet_Dui…       44302 IV20190218… 10260273_213…    44301 IV201902181…
    #> # ℹ more rows
    #> # ℹ 1 more variable: Parent_UserRef <chr>

Get the parent-child-relations from several specific surveys:

``` r

relations_severalsurveys <- get_inboveg_relation_recording(
  con,
  survey_name =  c("DeBlankaart-1985-Beheer", "N2000meetnet_Grasland"),
  multiple = TRUE
)
```

## Processing the retrieved data

When you have retrieved the data from INBOVEG using one of the `inbodb`
functions in the previous section, you can easily work with these data
in R or export the data.

We recommend to use the [`dplyr`](https://dplyr.tidyverse.org/) package
for basic as well as advanced data wrangling. For instance:

- [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  adds new variables that are functions of existing variables

- [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  picks variables based on their names.

- [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  picks cases (rows) based on their values.

- [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  reduces multiple values down to a single summary.

- [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
  changes the ordering of the rows.

For exporting the data we can make use of the
[`readr`](https://readr.tidyverse.org/) package.

Here is a simple example:

``` r

library(inbodb)
library(dplyr)
library(readr)

# get the data
con <- connect_inbo_dbase("D0010_00_Cydonia")

recording_heischraal2012 <- get_inboveg_recording(
  con,
  survey_name = "MILKLIM_Heischraal2012",
  collect = TRUE
)

# close the connection
dbDisconnect(con)
rm(con)

# do some data wrangling with dplyr
# here we remove one column
output_heischraal2012 <- recording_heischraal2012 %>%
  select(-OriginalName)

# export the data to a csv file that can be easily read by excel
write_excel_csv2(
  x = output_heischraal2012,
  file = "data/output_heischraal2012.csv"
)
```

## More complex queries

These functions give most of the relevant basic information that is
available in INBOVEG. In the future additional functions can be added to
this package to help the INBOVEG-users.

## Closing the connection

Close the connection when done

``` r

dbDisconnect(con)
rm(con)
```
