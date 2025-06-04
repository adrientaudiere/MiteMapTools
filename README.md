
# MiteMapTools

The goal of MiteMapTools is to …

[Masier et
al. 2022](https://onlinelibrary.wiley.com/doi/10.1002/jez.2651)

<https://github.com/LR69/MiteMap/tree/MiteMap.v6>

## Installation

You can install the development version of MiteMapTools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("adrientaudiere/MiteMapTools")
#> ! Using bundled GitHub PAT. Please add your own PAT using `gitcreds::gitcreds_set()`.
#> ℹ Loading metadata database
#> ✔ Loading metadata database ... done
#> 
#> 
#> → Will install 99 packages.
#> → Will update 1 package.
#> → All 100 packages (66.03 MB) are cached.
#> + askpass               1.2.1   [bld][cmp]
#> + backports             1.5.0   [bld][cmp]
#> + base64enc             0.1-3   [bld][cmp]
#> + bit                   4.6.0   [bld][cmp]
#> + bit64                 4.6.0-1 [bld][cmp]
#> + blob                  1.2.4   [bld]
#> + broom                 1.0.8   [bld]
#> + bslib                 0.9.0   [bld]
#> + cachem                1.1.0   [bld][cmp]
#> + callr                 3.7.6   [bld]
#> + cellranger            1.1.0   [bld]
#> + cli                   3.6.5   [bld][cmp]
#> + clipr                 0.8.0   [bld]
#> + conflicted            1.2.0   [bld]
#> + cpp11                 0.5.2   [bld]
#> + crayon                1.5.3   [bld]
#> + curl                  6.2.3   [bld][cmp]
#> + data.table            1.17.4  [bld][cmp]
#> + DBI                   1.2.3   [bld]
#> + dbplyr                2.5.0   [bld]
#> + digest                0.6.37  [bld][cmp]
#> + dplyr                 1.1.4   [bld][cmp]
#> + dtplyr                1.3.1   [bld]
#> + evaluate              1.0.3   [bld]
#> + fansi                 1.0.6   [bld][cmp]
#> + farver                2.1.2   [bld][cmp]
#> + fastmap               1.2.0   [bld][cmp]
#> + fontawesome           0.5.3   [bld]
#> + forcats               1.0.0   [bld]
#> + fs                    1.6.6   [bld][cmp]
#> + gargle                1.5.2   [bld]
#> + generics              0.1.4   [bld]
#> + ggplot2               3.5.2   [bld]
#> + glue                  1.8.0   [bld][cmp]
#> + googledrive           2.1.1   [bld]
#> + googlesheets4         1.1.1   [bld]
#> + gtable                0.3.6   [bld]
#> + haven                 2.5.5   [bld][cmp]
#> + highr                 0.11    [bld]
#> + hms                   1.1.3   [bld]
#> + htmltools             0.5.8.1 [bld][cmp]
#> + httr                  1.4.7   [bld]
#> + ids                   1.0.1   [bld]
#> + isoband               0.2.7   [bld][cmp]
#> + jquerylib             0.1.4   [bld]
#> + jsonlite              2.0.0   [bld][cmp]
#> + knitr                 1.50    [bld]
#> + labeling              0.4.3   [bld]
#> + lifecycle             1.0.4   [bld]
#> + lubridate             1.9.4   [bld][cmp]
#> + magrittr              2.0.3   [bld][cmp]
#> + memoise               2.0.1   [bld]
#> + mime                  0.13    [bld][cmp]
#> + MiteMapTools  0.0.1 → 0.0.1   [bld][cmp] (GitHub: e4e12b8)
#> + modelr                0.1.11  [bld]
#> + openssl               2.3.3   [bld][cmp]
#> + pillar                1.10.2  [bld]
#> + pkgconfig             2.0.3   [bld]
#> + prettyunits           1.2.0   [bld]
#> + processx              3.8.6   [bld][cmp]
#> + progress              1.2.3   [bld]
#> + ps                    1.9.1   [bld][cmp]
#> + purrr                 1.0.4   [bld][cmp]
#> + R6                    2.6.1   [bld]
#> + ragg                  1.4.0   [bld][cmp]
#> + rappdirs              0.3.3   [bld][cmp]
#> + RColorBrewer          1.1-3   [bld]
#> + readr                 2.1.5   [bld][cmp]
#> + readxl                1.4.5   [bld][cmp]
#> + rematch               2.0.0   [bld]
#> + rematch2              2.1.2   [bld]
#> + reprex                2.1.1   [bld]
#> + rlang                 1.1.6   [bld][cmp]
#> + rmarkdown             2.29    [bld]
#> + rstudioapi            0.17.1  [bld]
#> + rvest                 1.0.4   [bld]
#> + sass                  0.4.10  [bld][cmp]
#> + scales                1.4.0   [bld]
#> + selectr               0.4-2   [bld]
#> + stringi               1.8.7   [bld][cmp]
#> + stringr               1.5.1   [bld]
#> + sys                   3.4.3   [bld][cmp]
#> + systemfonts           1.2.3   [bld][cmp]
#> + textshaping           1.0.1   [bld][cmp]
#> + tibble                3.2.1   [bld][cmp]
#> + tidyr                 1.3.1   [bld][cmp]
#> + tidyselect            1.2.1   [bld][cmp]
#> + tidyverse             2.0.0   [bld]
#> + timechange            0.3.0   [bld][cmp]
#> + tinytex               0.57    [bld]
#> + tzdb                  0.5.0   [bld][cmp]
#> + utf8                  1.2.5   [bld][cmp]
#> + uuid                  1.2-1   [bld][cmp]
#> + vctrs                 0.6.5   [bld][cmp]
#> + viridisLite           0.4.2   [bld]
#> + vroom                 1.6.5   [bld][cmp]
#> + withr                 3.0.2   [bld]
#> + xfun                  0.52    [bld][cmp]
#> + xml2                  1.3.8   [bld][cmp]
#> + yaml                  2.3.10  [bld][cmp]
#> ℹ No downloads are needed, 100 pkgs (66.03 MB) are cached
#> ℹ Building backports 1.5.0
#> ℹ Building base64enc 0.1-3
#> ℹ Building bit 4.6.0
#> ℹ Building cli 3.6.5
#> ℹ Building clipr 0.8.0
#> ℹ Building cpp11 0.5.2
#> ✔ Built backports 1.5.0 (545ms)
#> ✔ Built base64enc 0.1-3 (541ms)
#> ✔ Built bit 4.6.0 (472ms)
#> ℹ Building crayon 1.5.3
#> ✔ Built clipr 0.8.0 (409ms)
#> ℹ Building curl 6.2.3
#> ℹ Building data.table 1.17.4
#> ✔ Built cli 3.6.5 (764ms)
#> ✔ Built cpp11 0.5.2 (568ms)
#> ✔ Built crayon 1.5.3 (395ms)
#> ℹ Building DBI 1.2.3
#> ℹ Building digest 0.6.37
#> ℹ Building evaluate 1.0.3
#> ✔ Built curl 6.2.3 (605ms)
#> ℹ Building fansi 1.0.6
#> ℹ Building farver 2.1.2
#> ✔ Built digest 0.6.37 (520ms)
#> ✔ Built evaluate 1.0.3 (371ms)
#> ℹ Building fastmap 1.2.0
#> ℹ Building fs 1.6.6
#> ✔ Built DBI 1.2.3 (798ms)
#> ℹ Building generics 0.1.4
#> ✔ Built fansi 1.0.6 (683ms)
#> ℹ Building glue 1.8.0
#> ✔ Built fastmap 1.2.0 (568ms)
#> ℹ Building isoband 0.2.7
#> ✔ Built generics 0.1.4 (418ms)
#> ✔ Built glue 1.8.0 (339ms)
#> ℹ Building jsonlite 2.0.0
#> ✔ Built data.table 1.17.4 (1.8s)
#> ℹ Building labeling 0.4.3
#> ℹ Building magrittr 2.0.3
#> ✔ Built fs 1.6.6 (1s)
#> ℹ Building mime 0.13
#> ✔ Built labeling 0.4.3 (334ms)
#> ℹ Building pkgconfig 2.0.3
#> ✔ Built magrittr 2.0.3 (479ms)
#> ℹ Building prettyunits 1.2.0
#> ✔ Built mime 0.13 (556ms)
#> ✔ Built pkgconfig 2.0.3 (395ms)
#> ℹ Building ps 1.9.1
#> ✔ Built jsonlite 2.0.0 (1s)
#> ℹ Building R6 2.6.1
#> ℹ Building rappdirs 0.3.3
#> ✔ Built farver 2.1.2 (2.2s)
#> ✔ Built isoband 0.2.7 (1.4s)
#> ℹ Building RColorBrewer 1.1-3
#> ✔ Built prettyunits 1.2.0 (453ms)
#> ℹ Building rematch 2.0.0
#> ✔ Built R6 2.6.1 (384ms)
#> ℹ Building rlang 1.1.6
#> ✔ Built ps 1.9.1 (662ms)
#> ✔ Built rappdirs 0.3.3 (491ms)
#> ℹ Building rstudioapi 0.17.1
#> ✔ Built RColorBrewer 1.1-3 (446ms)
#> ✔ Built rematch 2.0.0 (359ms)
#> ℹ Building stringi 1.8.7
#> ℹ Building sys 3.4.3
#> ✔ Built rstudioapi 0.17.1 (303ms)
#> ℹ Building utf8 1.2.5
#> ℹ Building uuid 1.2-1
#> ℹ Building viridisLite 0.4.2
#> ✔ Built sys 3.4.3 (395ms)
#> ℹ Building withr 3.0.2
#> ✔ Built rlang 1.1.6 (958ms)
#> ✔ Built uuid 1.2-1 (392ms)
#> ℹ Building xfun 0.52
#> ℹ Building yaml 2.3.10
#> ✔ Built viridisLite 0.4.2 (457ms)
#> ✔ Built utf8 1.2.5 (821ms)
#> ✔ Installed MiteMapTools 0.0.1 (github::adrientaudiere/MiteMapTools@e4e12b8) (149ms)
#> ✔ Built withr 3.0.2 (596ms)
#> ✔ Built xfun 0.52 (500ms)
#> ✔ Installed backports 1.5.0  (166ms)
#> ✔ Installed base64enc 0.1-3  (98ms)
#> ✔ Installed bit 4.6.0  (104ms)
#> ℹ Building bit64 4.6.0-1
#> ✔ Built yaml 2.3.10 (568ms)
#> ✔ Installed cli 3.6.5  (151ms)
#> ✔ Installed clipr 0.8.0  (28ms)
#> ✔ Installed cpp11 0.5.2  (46ms)
#> ℹ Building timechange 0.3.0
#> ℹ Building tzdb 0.5.0
#> ✔ Built bit64 4.6.0-1 (494ms)
#> ✔ Installed bit64 4.6.0-1  (113ms)
#> ✔ Installed crayon 1.5.3  (139ms)
#> ✔ Built stringi 1.8.7 (2.3s)
#> ✔ Installed curl 6.2.3  (98ms)
#> ✔ Installed data.table 1.17.4  (88ms)
#> ✔ Installed DBI 1.2.3  (75ms)
#> ✔ Installed digest 0.6.37  (70ms)
#> ✔ Built timechange 0.3.0 (892ms)
#> ✔ Installed evaluate 1.0.3  (133ms)
#> ✔ Installed fansi 1.0.6  (89ms)
#> ✔ Installed farver 2.1.2  (57ms)
#> ✔ Installed fastmap 1.2.0  (58ms)
#> ✔ Installed fs 1.6.6  (84ms)
#> ✔ Installed generics 0.1.4  (88ms)
#> ✔ Installed glue 1.8.0  (57ms)
#> ✔ Installed isoband 0.2.7  (58ms)
#> ✔ Installed jsonlite 2.0.0  (56ms)
#> ✔ Built tzdb 0.5.0 (1.2s)
#> ✔ Installed labeling 0.4.3  (105ms)
#> ✔ Installed magrittr 2.0.3  (80ms)
#> ✔ Installed mime 0.13  (54ms)
#> ✔ Installed pkgconfig 2.0.3  (51ms)
#> ✔ Installed prettyunits 1.2.0  (73ms)
#> ✔ Installed ps 1.9.1  (84ms)
#> ✔ Installed R6 2.6.1  (55ms)
#> ℹ Building processx 3.8.6
#> ✔ Installed rappdirs 0.3.3  (95ms)
#> ✔ Installed RColorBrewer 1.1-3  (16ms)
#> ✔ Installed rematch 2.0.0  (16ms)
#> ✔ Installed rlang 1.1.6  (1s)
#> ℹ Building cachem 1.1.0
#> ✔ Built processx 3.8.6 (1.3s)
#> ℹ Building htmltools 0.5.8.1
#> ℹ Building lifecycle 1.0.4
#> ✔ Built cachem 1.1.0 (232ms)
#> ℹ Building xml2 1.3.8
#> ✔ Built htmltools 0.5.8.1 (265ms)
#> ✔ Installed cachem 1.1.0  (49ms)
#> ℹ Building memoise 2.0.1
#> ✔ Built lifecycle 1.0.4 (254ms)
#> ✔ Installed htmltools 0.5.8.1  (149ms)
#> ℹ Building fontawesome 0.5.3
#> ℹ Building jquerylib 0.1.4
#> ✔ Built memoise 2.0.1 (341ms)
#> ℹ Building sass 0.4.10
#> ✔ Built xml2 1.3.8 (719ms)
#> ✔ Installed lifecycle 1.0.4  (18ms)
#> ℹ Building gtable 0.3.6
#> ✔ Built fontawesome 0.5.3 (478ms)
#> ℹ Building systemfonts 1.2.3
#> ✔ Built jquerylib 0.1.4 (584ms)
#> ✔ Built gtable 0.3.6 (262ms)
#> ℹ Building vctrs 0.6.5
#> ✔ Installed fontawesome 0.5.3  (46ms)
#> ✔ Installed gtable 0.3.6  (36ms)
#> ✔ Installed jquerylib 0.1.4  (1.1s)
#> ✔ Built sass 0.4.10 (1.8s)
#> ✔ Built systemfonts 1.2.3 (1.5s)
#> ✔ Built vctrs 0.6.5 (1.3s)
#> ✔ Installed memoise 2.0.1  (1.3s)
#> ℹ Building conflicted 1.2.0
#> ✔ Installed processx 3.8.6  (274ms)
#> ℹ Building callr 3.7.6
#> ✔ Built conflicted 1.2.0 (180ms)
#> ✔ Installed rstudioapi 0.17.1  (64ms)
#> ✔ Installed sass 0.4.10  (91ms)
#> ℹ Building bslib 0.9.0
#> ✔ Built callr 3.7.6 (639ms)
#> ✔ Installed conflicted 1.2.0  (496ms)
#> ✔ Installed callr 3.7.6  (19ms)
#> ✔ Installed stringi 1.8.7  (59ms)
#> ✔ Installed sys 3.4.3  (14ms)
#> ℹ Building askpass 1.2.1
#> ✔ Installed systemfonts 1.2.3  (55ms)
#> ℹ Building textshaping 1.0.1
#> ✔ Built askpass 1.2.1 (301ms)
#> ✔ Installed askpass 1.2.1  (15ms)
#> ℹ Building openssl 2.3.3
#> ✔ Installed utf8 1.2.5  (29ms)
#> ✔ Installed timechange 0.3.0  (124ms)
#> ✔ Installed tzdb 0.5.0  (140ms)
#> ✔ Installed uuid 1.2-1  (81ms)
#> ℹ Building lubridate 1.9.4
#> ✔ Installed vctrs 0.6.5  (102ms)
#> ℹ Building blob 1.2.4
#> ℹ Building hms 1.1.3
#> ✔ Built openssl 2.3.3 (718ms)
#> ✔ Built lubridate 1.9.4 (421ms)
#> ✔ Built blob 1.2.4 (269ms)
#> ℹ Building pillar 1.10.2
#> ℹ Building purrr 1.0.4
#> ✔ Built bslib 0.9.0 (1.7s)
#> ✔ Built textshaping 1.0.1 (1.2s)
#> ✔ Built hms 1.1.3 (302ms)
#> ℹ Building stringr 1.5.1
#> ✔ Installed blob 1.2.4  (27ms)
#> ✔ Installed hms 1.1.3  (19ms)
#> ℹ Building progress 1.2.3
#> ✔ Built pillar 1.10.2 (452ms)
#> ✔ Built purrr 1.0.4 (391ms)
#> ✔ Installed bslib 0.9.0  (242ms)
#> ✔ Built stringr 1.5.1 (342ms)
#> ✔ Installed lubridate 1.9.4  (51ms)
#> ✔ Built progress 1.2.3 (193ms)
#> ✔ Installed openssl 2.3.3  (94ms)
#> ℹ Building httr 1.4.7
#> ✔ Installed pillar 1.10.2  (126ms)
#> ℹ Building ids 1.0.1
#> ℹ Building tibble 3.2.1
#> ✔ Built httr 1.4.7 (258ms)
#> ✔ Built ids 1.0.1 (211ms)
#> ✔ Installed httr 1.4.7  (18ms)
#> ✔ Installed ids 1.0.1  (14ms)
#> ✔ Installed progress 1.2.3  (16ms)
#> ✔ Installed purrr 1.0.4  (20ms)
#> ✔ Installed stringr 1.5.1  (25ms)
#> ℹ Building selectr 0.4-2
#> ✔ Installed textshaping 1.0.1  (87ms)
#> ℹ Building ragg 1.4.0
#> ✔ Built tibble 3.2.1 (866ms)
#> ✔ Built selectr 0.4-2 (583ms)
#> ✔ Installed selectr 0.4-2  (17ms)
#> ✔ Installed tibble 3.2.1  (1s)
#> ℹ Building cellranger 1.1.0
#> ℹ Building forcats 1.0.0
#> ℹ Building rematch2 2.1.2
#> ✔ Built cellranger 1.1.0 (197ms)
#> ✔ Installed cellranger 1.1.0  (16ms)
#> ℹ Building readxl 1.4.5
#> ✔ Built forcats 1.0.0 (323ms)
#> ✔ Built rematch2 2.1.2 (230ms)
#> ✔ Installed forcats 1.0.0  (55ms)
#> ✔ Installed rematch2 2.1.2  (14ms)
#> ✔ Installed viridisLite 0.4.2  (23ms)
#> ℹ Building scales 1.4.0
#> ✔ Installed withr 3.0.2  (21ms)
#> ℹ Building gargle 1.5.2
#> ℹ Building tidyselect 1.2.1
#> ✔ Built scales 1.4.0 (296ms)
#> ✔ Installed xfun 0.52  (58ms)
#> ℹ Building highr 0.11
#> ✔ Installed scales 1.4.0  (108ms)
#> ℹ Building ggplot2 3.5.2
#> ✔ Built gargle 1.5.2 (495ms)
#> ✔ Built tidyselect 1.2.1 (398ms)
#> ℹ Building tinytex 0.57
#> ✔ Built highr 0.11 (357ms)
#> ✔ Installed gargle 1.5.2  (25ms)
#> ℹ Building googledrive 2.1.1
#> ✔ Installed highr 0.11  (42ms)
#> ✔ Built readxl 1.4.5 (1.5s)
#> ✔ Built tinytex 0.57 (408ms)
#> ✔ Installed tidyselect 1.2.1  (72ms)
#> ℹ Building dplyr 1.1.4
#> ✔ Installed readxl 1.4.5  (162ms)
#> ℹ Building vroom 1.6.5
#> ✔ Built ggplot2 3.5.2 (1.1s)
#> ✔ Built googledrive 2.1.1 (732ms)
#> ✔ Installed ggplot2 3.5.2  (63ms)
#> ✔ Installed googledrive 2.1.1  (44ms)
#> ℹ Building googlesheets4 1.1.1
#> ✔ Built dplyr 1.1.4 (738ms)
#> ✔ Installed dplyr 1.1.4  (108ms)
#> ℹ Building dtplyr 1.3.1
#> ℹ Building tidyr 1.3.1
#> ✔ Built googlesheets4 1.1.1 (559ms)
#> ✔ Installed googlesheets4 1.1.1  (56ms)
#> ✔ Built dtplyr 1.3.1 (399ms)
#> ✔ Installed tinytex 0.57  (138ms)
#> ✔ Installed xml2 1.3.8  (116ms)
#> ℹ Building rvest 1.0.4
#> ✔ Installed dtplyr 1.3.1  (120ms)
#> ✔ Installed yaml 2.3.10  (43ms)
#> ℹ Building knitr 1.50
#> ✔ Built rvest 1.0.4 (483ms)
#> ✔ Built tidyr 1.3.1 (970ms)
#> ✔ Installed rvest 1.0.4  (91ms)
#> ✔ Installed tidyr 1.3.1  (49ms)
#> ℹ Building broom 1.0.8
#> ℹ Building dbplyr 2.5.0
#> ✔ Built knitr 1.50 (542ms)
#> ✔ Installed knitr 1.50  (167ms)
#> ℹ Building rmarkdown 2.29
#> ✔ Built broom 1.0.8 (1s)
#> ✔ Built dbplyr 2.5.0 (924ms)
#> ✔ Installed broom 1.0.8  (99ms)
#> ℹ Building modelr 0.1.11
#> ✔ Installed dbplyr 2.5.0  (78ms)
#> ✔ Built modelr 0.1.11 (399ms)
#> ✔ Installed modelr 0.1.11  (24ms)
#> ✔ Built rmarkdown 2.29 (1.2s)
#> ✔ Built ragg 1.4.0 (8s)
#> ✔ Installed rmarkdown 2.29  (108ms)
#> ℹ Building reprex 2.1.1
#> ✔ Installed ragg 1.4.0  (257ms)
#> ✔ Built vroom 1.6.5 (4.7s)
#> ✔ Built reprex 2.1.1 (258ms)
#> ✔ Installed reprex 2.1.1  (26ms)
#> ✔ Installed vroom 1.6.5  (146ms)
#> ℹ Building readr 2.1.5
#> ✔ Built readr 2.1.5 (1.6s)
#> ✔ Installed readr 2.1.5  (62ms)
#> ℹ Building haven 2.5.5
#> ✔ Built haven 2.5.5 (922ms)
#> ✔ Installed haven 2.5.5  (39ms)
#> ℹ Building tidyverse 2.0.0
#> ✔ Built tidyverse 2.0.0 (224ms)
#> ✔ Installed tidyverse 2.0.0  (20ms)
#> ✔ 1 pkg + 104 deps: kept 5, upd 1, added 99 [33.3s]
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MiteMapTools)
#> Le chargement a nécessité le package : tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.4     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
#> Le chargement a nécessité le package : readxl

MM_filtered_centered <- filter_mitemap(MM_example, center_x = -20, center_y = -20)
#> Row removed when clearing the first secondes: 536
#> Row removed when clearing bad x range: 16860
#> Row removed when clearing bad y range: 0
#> Row removed when clearing bad x values: 24537
#> Row removed when clearing bad y values: 93
#> Row removed when clearing for run with times sup to maximum_time: 814
vioplot_mitemap(MM_filtered_centered)
```

<img src="man/figures/README-example-1.png" width="100%" />

A input folder for MiteMapTools consist of a - a metadata file (in xlsx
or csv) with 8 columns: - Run number - File name (must be present in the
list of zip files) - Date - Start time - Farm - MiteMap number - Bag -
Modality

- a list of zip files with 4 files compressed inside
  - A raw data file with 4 columns
    - The time in second (position is recorded every 0.2s)
    - The position in x (in mm)
    - The position in y (in mm)
    - Boolean variable indicating if the individual has remained
      immobile since the last record (1 if immobile)
  - A png file figuring the position of the individual using an heatmap
  - A processed data file called “formeD” (also called HH) compute the
    time spent in an area defined by the half side of the arena (see
    figure @ref(fig:cars-plot)).
    - The date and hour of the experiment
    - The name of the MiteMap run
    - Total time spent in the half containing the odor source (second)
    - Total time spent in the opposite half (second)
    - Time spent immobile in the half containing the odor source
      (second)
    - Time spent immobile in the opposite half (second)
    - Total distance traveled in the half containing the odor source
      (mm)
    - Total distance traveled in the opposite half (mm)
  - A processed data file called “formeC” (also called CH) compute the
    time spent in an area defined by a circle center on the odor source
    (see figure @ref(fig:cars-plot)) encompassing half of the arena
    surface.
    - The date and hour of the experiment
    - The name of the MiteMap run
    - Total time spent in the half containing the odor source (second)
    - Total time spent in the opposite half (second)
    - Time spent immobile in the half containing the odor source
      (second)
    - Time spent immobile in the opposite half (second)
    - Total distance traveled in the half containing the odor source
      (mm)
    - Total distance traveled in the opposite half (mm)

<img src="man/figures/README-shape-1.png" width="100%" />
