# Transforme des données de tableau de format Constellation en format de trame de données tibble R.

Transforme des données de tableau de format Constellation en format de
trame de données tibble R.

## Usage

``` r
donnéesTableauÀTrame(données)
```

## Arguments

- données:

  Les données provenant de Constellation

## Value

Une trame de données en format tibble R

## Examples

``` r
données <- jsonlite::fromJSON("{
  \"données\":[{
    \"col1\":123},{\"col2\":\"abc\",\"col1\":456}
   ]}"
  , simplifyDataFrame = FALSE
 )
td <- donnéesTableauÀTrame(données["données"])
```
