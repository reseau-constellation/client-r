# Transforme des données de nuée de format Constellation en format de trame de données tibble R.

Transforme des données de nuée de format Constellation en format de
trame de données tibble R.

## Usage

``` r
donnéesNuéeÀTrame(données)
```

## Arguments

- données:

  Les données provenant de Constellation

## Value

Une trame de données en format tibble R

## Examples

``` r
données <- jsonlite::fromJSON(
  "{\"données\":[
     {\"col1\":123,
     \"auteur\": \"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"},
     {\"col2\":\"abc\",\"col1\":456,
     \"auteur\": \"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"
     }]}",
  simplifyDataFrame = FALSE
 )
td <- donnéesTableauÀTrame(données["données"])
```
