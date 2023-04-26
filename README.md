Creating Factors to Conflate Census Demographics to TAZ Demographics for
Equity Analyses
================
Sophie Fox and Steven Andrews
2023-02-23

## General Goal

The TDM outputs results at a TAZ level. However, for equity analyses, we
need to allocated census demographics to those TAZs–the model does not
output results based on minority status, income status, or any other
status that might be interesting for an equity analysis (ages or
limited-English proficiency, perhaps disability status, and others).

Census geometry does NOT fit neatly into TAZ geometry. Some TAZs are
contained wholly within census geography, some is partially covered by
multiple geographies, some is *almost* congruent with census geography
but has slivers of overlaps.

We would like to allocate pieces of census demographics to surrounding
TAZs to create demographics of the TAZs. We need to create a table that
we can use to allocate portions of census geometry into TAZs.

**Problems:**

-   TAZs are mostly made up of census blocks, but they’re not exactly
    made up of blocks.
-   There are small tweaks to the TAZ geometry throughout the model
    region that mean lines do not overlap exactly.
-   Census blocks are NOT necessarily the best starting point because
    the differential privacy features of the 2020 census have rendered
    them unreliable.
-   TAZs may be smaller than a census geometry.

**Constraints:**

-   TAZ geometry may change over time. The process should be repeatable
    for arbitrary TAZ geometry (this essentially means for ANY
    geometry).

-   We want the process to be maintainable over multiple ACS iterations.

**Other Goals**

-   Create multiple methods to compare the sensitivity to methodology.

**Potential Solutions**

When joining 2010 census blocks to TAZs, Paul Reim performed a series of
intersections to identify where blocks were split. Where a TAZ split a
block, he counted rooftops alongside Google StreetView to estimate how
many rooftops were in each TAZ. It is not desirable to maintain such a
process–the reproducibility is limited and its highly manual. We can
replicate the methodology using the rooftops layer–forgoing a visual
inspection methodology entirely. It is unclear exactly which dataset and
how manual the process was.

Potential Solutions

1.  Rooftops
    1.  Allocate based on where the largest piece of the rooftop is
        **(chosen)**
    2.  Where the centroid of the roof is–could be a more scalable
        method–assuming intersecting points and polygons is more
        efficient that polygons and polygons and calculating shares in
        each, but would want a method to keep the centroid within the
        geometry.
    3.  Allocate proportionally based on the rooftop area-seems overly
        complicated
2.  Land Area
    1.  Find the share of the overlap removing inland water. Coastlines
        are clipped to match.
3.  Total Area
    1.  Find the share of the overlap without removing inland water.
        Coastlines are clipped to match.
4.  Dasymetric Mapping (not completed yet)

### Useful references

-   massgis census:
    <https://www.mass.gov/info-details/massgis-data-2020-us-census>

-   MassGIS rooftops:
    <https://www.mass.gov/info-details/massgis-data-building-structures-2-d>

-   Microsoft Rooftops:
    <https://github.com/Microsoft/USBuildingFootprints>

-   Dasymetric Mapping:
    <https://www.nature.com/articles/s41597-022-01603-z>

-   Paul’s thoughts on the topic: [TAZs and Census
    Geography](https://docs.google.com/presentation/d/1MDErn39ACXf1nyaW1dcCB9o_aeXUcY9tLi2yhBLjQXw/edit#slide=id.gcc3b1fded3_0_121)

## Running the Script

The work for this project is contained in a quarto doc named
`tract_to_TAZ_2020 - Final.qmd` in the main directory. The document is
intended to be run from top to bottom without much hand holding.
Throughout the script resource-intensive operations are stored as `.rds`
files. These statements should be turned off for a fresh run. A search
for `saveRDS` is a way to find these places.  The functions used in this 
document are contained in `Census_to_TAZ_functions.R` in the functions folder.

The user will need to download the MassGIS rooftops file and load it
into the `./data/base/` folder.

The folder structure is basically a data folder with “base” untouched
data, “processed” data that is a modified version of the base data or
large, typically modified–datasets stored as serialized `.rds` files.

    +-- blkgrp_to_TAZ_2020Update.html
    +-- blkgrp_to_TAZ_2020Update.qmd
    +-- data
    |   +-- base
    |   \-- processed
    +-- images
    +-- output
    |   \-- data
    +-- README.md
    \-- README.qmd (generates readme.md)

## Results

The final results are found in the `./output/data` folder:

Area based methods: `./output/data/area_allocation_BosMPO.csv`

Rooftop based method:
`./output/data/rooftop_allocation_MPO_largest_in_each.csv`

All methods: `./output/data/allocation_results_BosMPO.csv`

## Comparison of results:

The three methods tend to generate very similar results at a percentage
level (there is a little more “fuzz” at the total levels). There are
dozens of interesting lines that are not on the 1:1 line that we can
explore in more detail.

![](images/PctMinorityComparison.png)
