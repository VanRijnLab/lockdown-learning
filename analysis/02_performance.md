SlimStampen Performance During Lockdown
================
Maarten van der Velde
Last updated: 2021-05-19

# Setup

``` r
library(data.table)
library(DBI)
library(ggplot2)
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     ggsave

``` r
library(grid)
library(lme4)
```

    ## Loading required package: Matrix

``` r
library(lmerTest)
```

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
Sys.setlocale("LC_TIME", "en_US.UTF-8") # Print English date format
```

    ## [1] "en_US.UTF-8"

``` r
# Sys.setlocale("LC_TIME", "nl_NL.UTF-8") # Print Dutch date format

number_format <- scales::number_format(big.mark = ",", decimal.mark = ".") # Print English number format
# number_format <- scales::number_format(big.mark = ".", decimal.mark = ",") # Print Dutch number format

theme_paper <- theme_classic(base_size = 12) + 
  theme(axis.text = element_text(colour = "black"),
        panel.grid.major.y = element_line(colour = "grey92"))
```

School closure and opening
    dates

Sources:

  - <https://www.rijksoverheid.nl/actueel/nieuws/2020/03/15/aanvullende-maatregelen-onderwijs-horeca-sport>
  - <https://www.rijksoverheid.nl/actueel/nieuws/2020/05/19/onderwijs-gaat-stap-voor-stap-open>

<!-- end list -->

``` r
date_schools_closed <- as.POSIXct("2020-03-16")
date_schools_opened <- as.POSIXct("2020-06-02")
```

Handle database connections

``` r
db_connect <- function() {
  db <- dbConnect(RSQLite::SQLite(), file.path("..", "data", "noordhoff.sqlite"))
  return(db)
}

db_disconnect <- function(db) {
  dbDisconnect(db)
}
```

# Data

The database contains all SlimStampen data collected via Noordhoff’s
platform in three courses: *Stepping Stones* (English), *Grandes Lignes*
(French), and *Neue Kontakte* (German).

Trial-level response data are stored in the `responses` table. Book
information, such as the course year, book title, and chapter, are
stored in the `book_info`
table.

## `responses`

| Column                 | Type | Explanation                                   |
| ---------------------- | ---- | --------------------------------------------- |
| date                   | int  | UNIX time stamp \[s\]                         |
| user\_id               | chr  | unique user identifier                        |
| method                 | chr  | course                                        |
| start\_time            | int  | elapsed time since session start \[ms\]       |
| rt                     | int  | response time \[ms\]                          |
| duration               | int  | trial duration \[ms\]                         |
| fact\_id               | int  | unique fact identifier (within chapter)       |
| correct                | int  | response accuracy                             |
| answer                 | chr  | user’s response                               |
| choices                | int  | number of answer choices (1 == open response) |
| backspace\_used        | dbl  | user pressed backspace during trial           |
| backspace\_used\_first | dbl  | user erased first character of response       |
| study                  | int  | trial was a study trial                       |
| answer\_language       | chr  | language of the answer                        |
| subsession             | int  | identifies part within learning session       |
| book\_info\_id         | chr  | unique identifier of book information         |

## `book_info`

| Column         | Type | Explanation                             |
| -------------- | ---- | --------------------------------------- |
| book\_info\_id | chr  | unique identifier of book information   |
| method\_group  | chr  | year and edition                        |
| book\_title    | chr  | book title (incl. year, level, edition) |
| book\_type     | chr  | type of book                            |
| chapter        | chr  | chapter number and title                |

Preview first 10 rows

``` r
db <- db_connect()
responses_top <- dbGetQuery(db, "SELECT * FROM responses_noduplicates LIMIT 10")
responses_top
```

    ##          date                              user_id         method
    ## 1  1574814365 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 2  1574812553 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 3  1574814365 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 4  1574814365 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 5  1574812553 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 6  1574814365 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 7  1574814365 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 8  1574814365 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 9  1574812553 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ## 10 1574814365 000046f0-8731-42cd-95e6-ef427b76b156 Grandes Lignes
    ##    start_time    rt duration fact_id correct           answer choices
    ## 1          23  1569     4956       9       1     avoir raison       1
    ## 2          33  4698     6967       6       1           minuit       1
    ## 3          34  2319    10978      10       1  l'enfant unique       1
    ## 4         224  1489     1490       7       1           waarom       4
    ## 5         266  9249     9249       5       1            mardi       4
    ## 6         267  1420     1420       2       1          op tijd       4
    ## 7         269 55438    55438       5       1     bient&#244;t       4
    ## 8         301  2358     2358       4       1 snel, binnenkort       4
    ## 9         334  2039     2039       8       1      de afspraak       4
    ## 10       2733  1736     1736       2       1          op tijd       4
    ##    backspace_used backspace_used_first study answer_language subsession
    ## 1               0                    0     0               X          2
    ## 2               0                    0     0               X          2
    ## 3               1                    1     0               X          2
    ## 4              NA                   NA     1              NL          1
    ## 5              NA                   NA     0               X          3
    ## 6              NA                   NA     1              NL          1
    ## 7              NA                   NA     0               X          3
    ## 8              NA                   NA     1              NL          1
    ## 9              NA                   NA     1              NL          1
    ## 10             NA                   NA     0              NL          1
    ##                        book_info_id
    ## 1  b7fd0c6450a543df0abce795286cc0d9
    ## 2  b7fd0c6450a543df0abce795286cc0d9
    ## 3  b7fd0c6450a543df0abce795286cc0d9
    ## 4  b7fd0c6450a543df0abce795286cc0d9
    ## 5  b7fd0c6450a543df0abce795286cc0d9
    ## 6  b7fd0c6450a543df0abce795286cc0d9
    ## 7  b7fd0c6450a543df0abce795286cc0d9
    ## 8  b7fd0c6450a543df0abce795286cc0d9
    ## 9  b7fd0c6450a543df0abce795286cc0d9
    ## 10 b7fd0c6450a543df0abce795286cc0d9

``` r
book_info_top <- dbGetQuery(db, "SELECT * FROM book_info LIMIT 10")
book_info_top
```

    ##                        book_info_id        method_group
    ## 1  2562c0861746d69665bc006db57d61d2 Leerjaar 3 (5e Ed.)
    ## 2  2562c0861746d69665bc006db57d61d2 Leerjaar 3 (5e Ed.)
    ## 3  2562c0861746d69665bc006db57d61d2 Leerjaar 3 (5e Ed.)
    ## 4  669e672f2c250b5b93c2b7cbe13f89ee Leerjaar 1 (5e Ed.)
    ## 5  669e672f2c250b5b93c2b7cbe13f89ee Leerjaar 1 (5e Ed.)
    ## 6  fa534b16f415fa69e378fd112acfa6e0 Leerjaar 1 (5e Ed.)
    ## 7  fa534b16f415fa69e378fd112acfa6e0 Leerjaar 1 (5e Ed.)
    ## 8  d144843118b70c545e1c85c91634a1c6 Leerjaar 1 (5e Ed.)
    ## 9  d144843118b70c545e1c85c91634a1c6 Leerjaar 1 (5e Ed.)
    ## 10 d144843118b70c545e1c85c91634a1c6 Leerjaar 1 (5e Ed.)
    ##                book_title          book_type              chapter
    ## 1      3 vmbo gt (5e Ed.)               <NA>                 <NA>
    ## 2      3 vmbo gt (5e Ed.) Activiteitenboek A       2 To the limit
    ## 3      3 vmbo gt (5e Ed.)          Hoofdboek      2 To the limit!
    ## 4  1 vmbo b/lwoo (5e Ed.)               <NA>                 <NA>
    ## 5  1 vmbo b/lwoo (5e Ed.)          Hoofdboek 1 Family and friends
    ## 6  1 vmbo b/lwoo (5e Ed.)               <NA>                 <NA>
    ## 7  1 vmbo b/lwoo (5e Ed.)          Hoofdboek        2 School life
    ## 8  1 vmbo b/lwoo (5e Ed.)               <NA>                 <NA>
    ## 9  1 vmbo b/lwoo (5e Ed.) Activiteitenboek A        2 School life
    ## 10 1 vmbo b/lwoo (5e Ed.)          Hoofdboek        2 School life

``` r
db_disconnect(db)
```

# Performance

There are several measures of learning performance we can look at. The
most straight-forward of these are response accuracy and response time.

Important factors to keep in mind: question type (multiple choice or
open answer) and language. Note that we cannot distinguish between NL-X
and X-X, since we only know the language of the answer.

## Response accuracy

### Whole population

``` r
db <- db_connect()
correct <- dbGetQuery(db, 
                      "SELECT r.method AS 'method',
                      DATE(r.date + 3600, 'unixepoch') AS 'doy',
                      r.user_id AS 'user',
                      r.choices > 1 AS 'mcq',
                      r.correct AS 'correct',
                      COUNT(*) AS 'n'
                      FROM 'responses_noduplicates' r
                      WHERE r.study == 0
                      GROUP BY r.method,
                      DATE(r.date + 3600, 'unixepoch'),
                      r.user_id,
                      r.choices > 1,
                      r.correct"
)
setDT(correct)
db_disconnect(db)
```

Fill in missing rows (where all trials on a day were
correct/incorrect):

``` r
correct <- tidyr::complete(correct, tidyr::nesting(method, doy, user, mcq), correct, fill = list(n = 0))
setDT(correct)
```

``` r
correct[, mcq := as.logical(mcq)]
```

``` r
accuracy <- correct[, .(accuracy = n[correct == 1]/sum(n), n = sum(n)), by = .(method, doy, user, mcq)]
```

Add a school year column (cutoff date: 1 August):

``` r
accuracy[, doy_posix := as.POSIXct(doy)]
accuracy[, school_year := ifelse(doy_posix < "2019-08-01", "18/19", "19/20")]
```

Add sensible course
names:

``` r
accuracy[, course := ifelse(method == "Grandes Lignes", "French", ifelse(method == "Stepping Stones", "English", "German"))]
```

Align school
years:

``` r
accuracy[school_year == "18/19", doy_posix_aligned := as.POSIXct(doy_posix + 365*24*60*60, origin = "1970-01-01")]
accuracy[school_year == "19/20", doy_posix_aligned := doy_posix]
```

Use cut.Date() to bin dates by week. Each day is assigned the date of
the most recent Monday.

``` r
accuracy[, doy_posix_week := cut.POSIXt(doy_posix, "week")]
accuracy[, doy_posix_aligned_week := cut.POSIXt(doy_posix_aligned, "week")]
```

``` r
accuracy_by_week_and_user <- accuracy[, .(accuracy = sum(accuracy*n)/sum(n)), by = .(course, school_year, doy_posix_aligned_week, user, mcq)]
accuracy_by_week <- accuracy_by_week_and_user[, .(accuracy_mean = mean(accuracy, na.rm = TRUE),
                              accuracy_se = sd(accuracy, na.rm = TRUE)/sqrt(.N), n = .N), by = .(course, school_year, doy_posix_aligned_week, mcq)]
```

Add question type
labels:

``` r
accuracy_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

Plot response accuracy by week (mean +/- 1
SE).

``` r
p_acc <- ggplot(accuracy_by_week[(course == "English" & mcq == TRUE) | course == "French",],
            aes(x = as.POSIXct(doy_posix_aligned_week), y = accuracy_mean, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(. ~ course) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1.05, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = accuracy_mean - accuracy_se, ymax = accuracy_mean + accuracy_se, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(limits = c(.7, 1), labels = scales::percent_format(accuracy = 1)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Accuracy",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper
p_acc
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggsave("../output/acc_by_question_type.pdf", width = 9, height = 3)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

``` r
ggsave("../output/acc_by_question_type.eps", width = 9, height = 3)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/acc_by_question_type.png", width = 9, height = 3)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

### By level and year

``` r
db <- db_connect()
correct_strat <- dbGetQuery(db, 
                      "SELECT r.method AS 'method',
                      r.book_info_id AS 'book_info_id',
                      DATE(r.date + 3600, 'unixepoch') AS 'doy',
                      r.user_id AS 'user',
                      r.choices > 1 AS 'mcq',
                      r.correct AS 'correct',
                      COUNT(*) AS 'n'
                      FROM 'responses_noduplicates' r
                      WHERE r.study == 0
                      GROUP BY r.method,
                      r.book_info_id,
                      DATE(r.date + 3600, 'unixepoch'),
                      r.user_id,
                      r.choices > 1,
                      r.correct"
)
setDT(correct_strat)
db_disconnect(db)
```

Fill in missing rows (where all trials on a day were
correct/incorrect):

``` r
correct_strat <- tidyr::complete(correct_strat, tidyr::nesting(method, book_info_id, doy, user, mcq), correct, fill = list(n = 0))
setDT(correct_strat)
```

``` r
correct_strat[, mcq := as.logical(mcq)]
```

Add book information:

``` r
db <- db_connect()
book_info <- dbGetQuery(db, "SELECT DISTINCT * FROM 'book_info'")
db_disconnect(db)

setDT(book_info)
```

``` r
correct_strat[book_info[book_type == "Hoofdboek",], on  = "book_info_id", c("method_group", "book_title") := .(i.method_group, i.book_title)]
```

Add sensible course
names:

``` r
correct_strat[, course := ifelse(method == "Grandes Lignes", "French", ifelse(method == "Stepping Stones", "English", "German"))]
```

Add a school year column (cutoff date: 1 August):

``` r
correct_strat[, doy_posix := as.POSIXct(doy)]
correct_strat[, school_year := ifelse(doy_posix < "2019-08-01", "18/19", "19/20")]
```

Simplify level names:

``` r
# Keep all distinctions
correct_strat[, book_title_simple := stringr::str_sub(book_title, 3, -10)]
correct_strat[, book_title_simple := factor(book_title_simple, levels = c("vmbo b/lwoo", "vmbo b", "vmbo bk", "vmbo k", "vmbo kgt", "vmbo-gt", "vmbo gt", "vmbo-gt/havo", "vmbo (t)hv", "havo", "havo vwo", "vwo"))]

# Simplify to three levels
correct_strat[, level := dplyr::case_when(
  grepl( "hv", book_title) ~ "General secondary\n(havo)",
  grepl("vmbo", book_title) ~ "Pre-vocational\n(vmbo)",
  grepl("havo", book_title) ~ "General secondary\n(havo)",
  grepl("vwo", book_title) ~ "Pre-university\n(vwo)",
  TRUE ~ "Other")]
correct_strat[, level := factor(level, levels = c("Other", "Pre-vocational\n(vmbo)", "General secondary\n(havo)", "Pre-university\n(vwo)"))]
```

Simplify year names:

``` r
correct_strat[, year := dplyr::case_when(
  method_group == "Leerjaar 1 (5e Ed.)" ~ "Year 1",
  method_group == "Leerjaar 2 (5e Ed.)" ~ "Year 2",
  method_group == "Leerjaar 3 (5e Ed.)" ~ "Year 3",
  method_group == "Leerjaar 3/4 (5e Ed.)" ~ "Year 3/4",
  method_group == "Leerjaar 4 (5e Ed.)" ~ "Year 4",
  method_group == "Tweede Fase (6e Ed.)" ~ "Tweede Fase",
  TRUE ~ "Other")]
```

Consolidate by
day:

``` r
accuracy_strat <- correct_strat[, .(accuracy = n[correct == 1]/sum(n), n = sum(n)), by = .(school_year, doy_posix, course, level, year, user, mcq)]
```

Align school
years:

``` r
accuracy_strat[school_year == "18/19", doy_posix_aligned := as.POSIXct(doy_posix + 365*24*60*60, origin = "1970-01-01")]
accuracy_strat[school_year == "19/20", doy_posix_aligned := doy_posix]
```

Use cut.Date() to bin dates by week. Each day is assigned the date of
the most recent Monday.

``` r
accuracy_strat[, doy_posix_week := cut.POSIXt(doy_posix, "week")]
accuracy_strat[, doy_posix_aligned_week := cut.POSIXt(doy_posix_aligned, "week")]
```

``` r
accuracy_strat_by_week_and_user <- accuracy_strat[, .(accuracy = sum(accuracy*n)/sum(n)), by = .(course, school_year, doy_posix_aligned_week, level, year, user, mcq)]
accuracy_strat_by_week <- accuracy_strat_by_week_and_user[, .(accuracy_mean = mean(accuracy, na.rm = TRUE),
                              accuracy_se = sd(accuracy, na.rm = TRUE)/sqrt(.N), n = .N), by = .(course, school_year, doy_posix_aligned_week, level, year, mcq)]
```

Add question type
labels:

``` r
accuracy_strat_by_week_and_user[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
accuracy_strat_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

How many unique users per
group?

``` r
accuracy_strat_by_week_and_user[, .(unique_users = length(unique(user))),  by = .(course, level, year, school_year, question_type)]
```

    ##      course                     level        year school_year
    ##  1:  French    Pre-vocational\n(vmbo)    Year 3/4       18/19
    ##  2:  French    Pre-vocational\n(vmbo)    Year 3/4       18/19
    ##  3:  French    Pre-vocational\n(vmbo)    Year 3/4       19/20
    ##  4:  French    Pre-vocational\n(vmbo)    Year 3/4       19/20
    ##  5:  French General secondary\n(havo)      Year 1       18/19
    ##  6:  French General secondary\n(havo)      Year 1       18/19
    ##  7:  French General secondary\n(havo)      Year 1       19/20
    ##  8:  French General secondary\n(havo)      Year 1       19/20
    ##  9:  French General secondary\n(havo)    Year 3/4       18/19
    ## 10:  French General secondary\n(havo)    Year 3/4       18/19
    ## 11:  French General secondary\n(havo)    Year 3/4       19/20
    ## 12:  French General secondary\n(havo)    Year 3/4       19/20
    ## 13:  French     Pre-university\n(vwo)      Year 1       18/19
    ## 14:  French     Pre-university\n(vwo)      Year 1       18/19
    ## 15:  French     Pre-university\n(vwo)      Year 1       19/20
    ## 16:  French     Pre-university\n(vwo)      Year 1       19/20
    ## 17:  French     Pre-university\n(vwo)    Year 3/4       18/19
    ## 18:  French     Pre-university\n(vwo)    Year 3/4       18/19
    ## 19:  French     Pre-university\n(vwo)    Year 3/4       19/20
    ## 20:  French     Pre-university\n(vwo)    Year 3/4       19/20
    ## 21:  French General secondary\n(havo)      Year 2       18/19
    ## 22:  French General secondary\n(havo)      Year 2       18/19
    ## 23:  French General secondary\n(havo)      Year 2       19/20
    ## 24:  French General secondary\n(havo)      Year 2       19/20
    ## 25:  French     Pre-university\n(vwo)      Year 2       18/19
    ## 26:  French     Pre-university\n(vwo)      Year 2       18/19
    ## 27:  French     Pre-university\n(vwo)      Year 2       19/20
    ## 28:  French     Pre-university\n(vwo)      Year 2       19/20
    ## 29:  French    Pre-vocational\n(vmbo)      Year 1       18/19
    ## 30:  French    Pre-vocational\n(vmbo)      Year 1       18/19
    ## 31:  French    Pre-vocational\n(vmbo)      Year 1       19/20
    ## 32:  French    Pre-vocational\n(vmbo)      Year 1       19/20
    ## 33:  French    Pre-vocational\n(vmbo)      Year 2       18/19
    ## 34:  French    Pre-vocational\n(vmbo)      Year 2       18/19
    ## 35:  French    Pre-vocational\n(vmbo)      Year 2       19/20
    ## 36:  French    Pre-vocational\n(vmbo)      Year 2       19/20
    ## 37:  German General secondary\n(havo) Tweede Fase       18/19
    ## 38:  German General secondary\n(havo) Tweede Fase       18/19
    ## 39:  German     Pre-university\n(vwo) Tweede Fase       18/19
    ## 40:  German     Pre-university\n(vwo) Tweede Fase       18/19
    ## 41:  German     Pre-university\n(vwo) Tweede Fase       19/20
    ## 42:  German     Pre-university\n(vwo) Tweede Fase       19/20
    ## 43:  German General secondary\n(havo) Tweede Fase       19/20
    ## 44:  German General secondary\n(havo) Tweede Fase       19/20
    ## 45: English    Pre-vocational\n(vmbo)      Year 1       18/19
    ## 46: English    Pre-vocational\n(vmbo)      Year 1       19/20
    ## 47: English    Pre-vocational\n(vmbo)      Year 3       18/19
    ## 48: English    Pre-vocational\n(vmbo)      Year 3       19/20
    ## 49: English General secondary\n(havo)      Year 2       18/19
    ## 50: English General secondary\n(havo)      Year 2       19/20
    ## 51: English     Pre-university\n(vwo)      Year 3       18/19
    ## 52: English     Pre-university\n(vwo)      Year 3       19/20
    ## 53: English    Pre-vocational\n(vmbo)      Year 4       18/19
    ## 54: English    Pre-vocational\n(vmbo)      Year 4       19/20
    ## 55: English     Pre-university\n(vwo)      Year 1       18/19
    ## 56: English     Pre-university\n(vwo)      Year 1       19/20
    ## 57: English                     Other      Year 2       18/19
    ## 58: English                     Other      Year 2       19/20
    ## 59: English General secondary\n(havo)      Year 3       18/19
    ## 60: English General secondary\n(havo)      Year 3       19/20
    ## 61: English                     Other      Year 3       18/19
    ## 62: English                     Other      Year 3       19/20
    ## 63: English    Pre-vocational\n(vmbo)      Year 2       18/19
    ## 64: English    Pre-vocational\n(vmbo)      Year 2       19/20
    ## 65: English                     Other      Year 1       18/19
    ## 66: English                     Other      Year 1       19/20
    ## 67: English     Pre-university\n(vwo)      Year 2       18/19
    ## 68: English     Pre-university\n(vwo)      Year 2       19/20
    ## 69: English General secondary\n(havo)      Year 1       18/19
    ## 70: English General secondary\n(havo)      Year 1       19/20
    ## 71: English     Pre-university\n(vwo)      Year 1       18/19
    ## 72: English     Pre-university\n(vwo)      Year 1       19/20
    ## 73: English                     Other      Year 1       18/19
    ## 74: English                     Other      Year 1       19/20
    ##      course                     level        year school_year
    ##        question_type unique_users
    ##  1:     Open\nanswer          227
    ##  2: Multiple\nchoice          345
    ##  3:     Open\nanswer          492
    ##  4: Multiple\nchoice          610
    ##  5:     Open\nanswer         5819
    ##  6: Multiple\nchoice         6021
    ##  7:     Open\nanswer         5450
    ##  8: Multiple\nchoice         5588
    ##  9:     Open\nanswer         1049
    ## 10: Multiple\nchoice         1110
    ## 11:     Open\nanswer         2023
    ## 12: Multiple\nchoice         2104
    ## 13:     Open\nanswer         3053
    ## 14: Multiple\nchoice         3145
    ## 15:     Open\nanswer         2709
    ## 16: Multiple\nchoice         2778
    ## 17:     Open\nanswer          976
    ## 18: Multiple\nchoice         1015
    ## 19:     Open\nanswer         2006
    ## 20: Multiple\nchoice         2059
    ## 21:     Open\nanswer         2998
    ## 22: Multiple\nchoice         3208
    ## 23:     Open\nanswer         4744
    ## 24: Multiple\nchoice         4935
    ## 25:     Open\nanswer         2061
    ## 26: Multiple\nchoice         2122
    ## 27:     Open\nanswer         3251
    ## 28: Multiple\nchoice         3325
    ## 29:     Open\nanswer         3025
    ## 30: Multiple\nchoice         3160
    ## 31:     Open\nanswer         2840
    ## 32: Multiple\nchoice         2922
    ## 33:     Open\nanswer         1319
    ## 34: Multiple\nchoice         1447
    ## 35:     Open\nanswer         1994
    ## 36: Multiple\nchoice         2094
    ## 37:     Open\nanswer          437
    ## 38: Multiple\nchoice          439
    ## 39:     Open\nanswer          382
    ## 40: Multiple\nchoice          379
    ## 41:     Open\nanswer          616
    ## 42: Multiple\nchoice          617
    ## 43:     Open\nanswer          421
    ## 44: Multiple\nchoice          421
    ## 45: Multiple\nchoice         9650
    ## 46: Multiple\nchoice         9043
    ## 47: Multiple\nchoice         9741
    ## 48: Multiple\nchoice        11094
    ## 49: Multiple\nchoice         5674
    ## 50: Multiple\nchoice         6288
    ## 51: Multiple\nchoice         1550
    ## 52: Multiple\nchoice         1896
    ## 53: Multiple\nchoice         5900
    ## 54: Multiple\nchoice         6246
    ## 55: Multiple\nchoice         3041
    ## 56: Multiple\nchoice         3099
    ## 57: Multiple\nchoice          262
    ## 58: Multiple\nchoice          232
    ## 59: Multiple\nchoice         2901
    ## 60: Multiple\nchoice         3209
    ## 61: Multiple\nchoice          125
    ## 62: Multiple\nchoice          163
    ## 63: Multiple\nchoice         9347
    ## 64: Multiple\nchoice        10908
    ## 65: Multiple\nchoice          312
    ## 66: Multiple\nchoice          215
    ## 67: Multiple\nchoice         2553
    ## 68: Multiple\nchoice         2876
    ## 69: Multiple\nchoice         8090
    ## 70: Multiple\nchoice         7333
    ## 71:     Open\nanswer          106
    ## 72:     Open\nanswer           64
    ## 73:     Open\nanswer            4
    ## 74:     Open\nanswer            8
    ##        question_type unique_users

Plot response accuracy by week (mean +/- 1
SE).

``` r
p_acc_level_year <- ggplot(accuracy_strat_by_week[course == "French" & level != "Other",],
            aes(x = as.POSIXct(doy_posix_aligned_week), y = accuracy_mean, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(level ~ year) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1.05, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = accuracy_mean - accuracy_se, ymax = accuracy_mean + accuracy_se, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(.4, 1)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Accuracy",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_acc_level_year
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
ggsave("../output/acc_by_question_type_french_level_year.pdf", width = 9, height = 5)
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

``` r
ggsave("../output/acc_by_question_type_french_level_year.eps", width = 9, height = 5)
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/acc_by_question_type_french_level_year.png", width = 9, height = 5)
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

``` r
p_acc_level_year <- ggplot(accuracy_strat_by_week[course == "English" & level != "Other" & question_type == "Multiple\nchoice",],
            aes(x = as.POSIXct(doy_posix_aligned_week), y = accuracy_mean, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(level ~ year) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1.05, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = accuracy_mean - accuracy_se, ymax = accuracy_mean + accuracy_se, colour = NULL), alpha = 0.2) +
  geom_line() +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(.4, 1)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Accuracy",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_acc_level_year
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
ggsave("../output/acc_by_question_type_english_level_year.pdf", width = 9, height = 5)
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

``` r
ggsave("../output/acc_by_question_type_english_level_year.eps", width = 9, height = 5)
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/acc_by_question_type_english_level_year.png", width = 9, height = 5)
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

### By level

``` r
accuracy_level_by_week_and_user <- accuracy_strat[, .(accuracy = sum(accuracy*n)/sum(n)), by = .(course, school_year, doy_posix_aligned_week, level, user, mcq)]

accuracy_level_by_week <- accuracy_level_by_week_and_user[, .(accuracy_mean = mean(accuracy, na.rm = TRUE),
                              accuracy_se = sd(accuracy, na.rm = TRUE)/sqrt(.N), n = .N), by = .(course, school_year, doy_posix_aligned_week, level, mcq)]
```

Add question type
labels:

``` r
accuracy_level_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

How many users in each
group?

``` r
accuracy_level_by_week_and_user[, .(unique_users = length(unique(user))),  by = .(course, level, school_year, mcq)]
```

    ##      course                     level school_year   mcq unique_users
    ##  1:  French    Pre-vocational\n(vmbo)       18/19 FALSE         4568
    ##  2:  French    Pre-vocational\n(vmbo)       18/19  TRUE         4949
    ##  3:  French    Pre-vocational\n(vmbo)       19/20 FALSE         5311
    ##  4:  French    Pre-vocational\n(vmbo)       19/20  TRUE         5610
    ##  5:  French General secondary\n(havo)       18/19 FALSE         9853
    ##  6:  French General secondary\n(havo)       18/19  TRUE        10326
    ##  7:  French General secondary\n(havo)       19/20 FALSE        12196
    ##  8:  French General secondary\n(havo)       19/20  TRUE        12605
    ##  9:  French     Pre-university\n(vwo)       18/19 FALSE         6078
    ## 10:  French     Pre-university\n(vwo)       18/19  TRUE         6270
    ## 11:  French     Pre-university\n(vwo)       19/20 FALSE         7945
    ## 12:  French     Pre-university\n(vwo)       19/20  TRUE         8139
    ## 13:  German General secondary\n(havo)       18/19 FALSE          437
    ## 14:  German General secondary\n(havo)       18/19  TRUE          439
    ## 15:  German     Pre-university\n(vwo)       18/19 FALSE          382
    ## 16:  German     Pre-university\n(vwo)       18/19  TRUE          379
    ## 17:  German     Pre-university\n(vwo)       19/20 FALSE          616
    ## 18:  German     Pre-university\n(vwo)       19/20  TRUE          617
    ## 19:  German General secondary\n(havo)       19/20 FALSE          421
    ## 20:  German General secondary\n(havo)       19/20  TRUE          421
    ## 21: English    Pre-vocational\n(vmbo)       18/19  TRUE        34520
    ## 22: English    Pre-vocational\n(vmbo)       19/20  TRUE        37146
    ## 23: English General secondary\n(havo)       18/19  TRUE        16593
    ## 24: English General secondary\n(havo)       19/20  TRUE        16806
    ## 25: English     Pre-university\n(vwo)       18/19  TRUE         7132
    ## 26: English     Pre-university\n(vwo)       19/20  TRUE         7858
    ## 27: English                     Other       18/19  TRUE          698
    ## 28: English                     Other       19/20  TRUE          610
    ## 29: English     Pre-university\n(vwo)       18/19 FALSE          106
    ## 30: English     Pre-university\n(vwo)       19/20 FALSE           64
    ## 31: English                     Other       18/19 FALSE            4
    ## 32: English                     Other       19/20 FALSE            8
    ##      course                     level school_year   mcq unique_users

Plot response accuracy by week (mean +/- 1
SE).

``` r
p_acc_level <- ggplot(accuracy_level_by_week[((course == "English" & question_type == "Multiple\nchoice") | course == "French") & level != "Other",],
            aes(x = as.POSIXct(doy_posix_aligned_week), y = accuracy_mean, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(level ~ course) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1.05, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = accuracy_mean - accuracy_se, ymax = accuracy_mean + accuracy_se, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(.6, 1)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Accuracy",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_acc_level
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
ggsave("../output/acc_by_question_type_level.pdf", width = 9, height = 5)
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

``` r
ggsave("../output/acc_by_question_type_level.eps", width = 9, height = 5)
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/acc_by_question_type_level.png", width = 9, height = 5)
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

### By year

``` r
accuracy_year_by_week_and_user <- accuracy_strat[, .(accuracy = sum(accuracy*n)/sum(n)), by = .(course, school_year, doy_posix_aligned_week, year, user, mcq)]

accuracy_year_by_week <- accuracy_year_by_week_and_user[, .(accuracy_mean = mean(accuracy, na.rm = TRUE),
                              accuracy_se = sd(accuracy, na.rm = TRUE)/sqrt(.N), n = .N), by = .(course, school_year, doy_posix_aligned_week, year, mcq)]
```

Add question type
labels:

``` r
accuracy_year_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

How many users in each
group?

``` r
accuracy_year_by_week_and_user[, .(unique_users = length(unique(user))),  by = .(course, year, school_year, mcq)]
```

    ##      course        year school_year   mcq unique_users
    ##  1:  French    Year 3/4       18/19 FALSE         2249
    ##  2:  French    Year 3/4       18/19  TRUE         2467
    ##  3:  French    Year 3/4       19/20 FALSE         4517
    ##  4:  French    Year 3/4       19/20  TRUE         4767
    ##  5:  French      Year 1       18/19 FALSE        11856
    ##  6:  French      Year 1       18/19  TRUE        12281
    ##  7:  French      Year 1       19/20 FALSE        10919
    ##  8:  French      Year 1       19/20  TRUE        11204
    ##  9:  French      Year 2       18/19 FALSE         6353
    ## 10:  French      Year 2       18/19  TRUE         6750
    ## 11:  French      Year 2       19/20 FALSE         9963
    ## 12:  French      Year 2       19/20  TRUE        10328
    ## 13:  German Tweede Fase       18/19 FALSE          818
    ## 14:  German Tweede Fase       18/19  TRUE          817
    ## 15:  German Tweede Fase       19/20 FALSE         1036
    ## 16:  German Tweede Fase       19/20  TRUE         1037
    ## 17: English      Year 1       18/19  TRUE        20831
    ## 18: English      Year 1       19/20  TRUE        19461
    ## 19: English      Year 3       18/19  TRUE        14280
    ## 20: English      Year 3       19/20  TRUE        16312
    ## 21: English      Year 2       18/19  TRUE        17620
    ## 22: English      Year 2       19/20  TRUE        20125
    ## 23: English      Year 4       18/19  TRUE         5900
    ## 24: English      Year 4       19/20  TRUE         6246
    ## 25: English      Year 1       18/19 FALSE          110
    ## 26: English      Year 1       19/20 FALSE           72
    ##      course        year school_year   mcq unique_users

Plot response accuracy by week (mean +/- 1
SE).

``` r
p_acc_year <- ggplot(accuracy_year_by_week[((course == "English" & question_type == "Multiple\nchoice") | course == "French") & year != "Other",],
            aes(x = as.POSIXct(doy_posix_aligned_week), y = accuracy_mean, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(year ~ course) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1.05, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = accuracy_mean - accuracy_se, ymax = accuracy_mean + accuracy_se, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(.6, 1)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Accuracy",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_acc_year
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
ggsave("../output/acc_by_question_type_year.pdf", width = 9, height = 5)
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

``` r
ggsave("../output/acc_by_question_type_year.eps", width = 9, height = 5)
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/acc_by_question_type_year.png", width = 9, height = 5)
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

### Regression model

Fit a mixed effects model to the daily accuracy data:

``` r
accuracy[, period := dplyr::case_when(
  doy_posix_aligned >= date_schools_opened ~ "post-lockdown",
  doy_posix_aligned >= date_schools_closed & doy_posix_aligned < date_schools_opened ~ "during-lockdown",
  doy_posix_aligned < date_schools_opened ~ "pre-lockdown"
)]

# Reorder factor levels so that intercept is pre-lockdown open answer in 19/20
accuracy[, period := factor(period, levels = c("pre-lockdown", "during-lockdown", "post-lockdown"))]
accuracy[, school_year := factor(school_year, levels = c("19/20", "18/19"))]
accuracy[, mcq := factor(mcq, levels = c(FALSE, TRUE))]
```

Since we know the number of trials per day and the proportion correct
(accuracy), we can use a binomial GLMM:

``` r
if(!file.exists("../output/m_acc_fit.rds")) {
  m_acc <- glmer(accuracy ~ period*school_year*mcq + (1 | user) + (1 | course),
                 data = accuracy[(course == "English" & mcq == TRUE) | course == "French",],
                 family = "binomial", 
                 weights = n,
                 nAGQ = 0,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))
  saveRDS(m_acc, "../output/m_acc_fit.rds")
} else {
  m_acc <- readRDS("../output/m_acc_fit.rds")
}

m_acc_summary <- summary(m_acc)
m_acc_summary
```

    ## Generalized linear mixed model fit by maximum likelihood (Adaptive
    ##   Gauss-Hermite Quadrature, nAGQ = 0) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: accuracy ~ period * school_year * mcq + (1 | user) + (1 | course)
    ##    Data: 
    ## accuracy[(course == "English" & mcq == TRUE) | course == "French",      ]
    ## Weights: n
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+06))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  4934770  4934930 -2467371  4934742   668527 
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -22.1127  -0.9205   0.1614   1.0939  11.4403 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  user   (Intercept) 0.34971  0.5914  
    ##  course (Intercept) 0.05504  0.2346  
    ## Number of obs: 668541, groups:  user, 133419; course, 2
    ## 
    ## Fixed effects:
    ##                                                 Estimate Std. Error
    ## (Intercept)                                     1.631889   0.165913
    ## periodduring-lockdown                           0.264728   0.002760
    ## periodpost-lockdown                             0.159475   0.004190
    ## school_year18/19                                0.116285   0.002580
    ## mcqTRUE                                         0.959425   0.002109
    ## periodduring-lockdown:school_year18/19         -0.316933   0.004432
    ## periodpost-lockdown:school_year18/19           -0.251656   0.006437
    ## periodduring-lockdown:mcqTRUE                  -0.333631   0.002913
    ## periodpost-lockdown:mcqTRUE                    -0.286879   0.004562
    ## school_year18/19:mcqTRUE                        0.004370   0.002681
    ## periodduring-lockdown:school_year18/19:mcqTRUE  0.384829   0.004778
    ## periodpost-lockdown:school_year18/19:mcqTRUE    0.344909   0.007078
    ##                                                 z value Pr(>|z|)    
    ## (Intercept)                                       9.836   <2e-16 ***
    ## periodduring-lockdown                            95.917   <2e-16 ***
    ## periodpost-lockdown                              38.065   <2e-16 ***
    ## school_year18/19                                 45.073   <2e-16 ***
    ## mcqTRUE                                         454.977   <2e-16 ***
    ## periodduring-lockdown:school_year18/19          -71.502   <2e-16 ***
    ## periodpost-lockdown:school_year18/19            -39.095   <2e-16 ***
    ## periodduring-lockdown:mcqTRUE                  -114.525   <2e-16 ***
    ## periodpost-lockdown:mcqTRUE                     -62.879   <2e-16 ***
    ## school_year18/19:mcqTRUE                          1.630    0.103    
    ## periodduring-lockdown:school_year18/19:mcqTRUE   80.540   <2e-16 ***
    ## periodpost-lockdown:school_year18/19:mcqTRUE     48.730   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                   (Intr) prddr- prdps- sc_18/19 mcTRUE prdd-:_18/19
    ## prddrng-lck       -0.008                                           
    ## prdpst-lckd       -0.005  0.336                                    
    ## schl_y18/19       -0.009  0.453  0.290                             
    ## mcqTRUE           -0.010  0.534  0.344  0.596                      
    ## prdd-:_18/19       0.005 -0.631 -0.212 -0.506   -0.343             
    ## prdp-:_18/19       0.003 -0.228 -0.655 -0.347   -0.233  0.239      
    ## prddr-:TRUE        0.007 -0.825 -0.271 -0.414   -0.649  0.520      
    ## prdps-:TRUE        0.004 -0.263 -0.827 -0.259   -0.408  0.166      
    ## s_18/19:TRU        0.007 -0.411 -0.264 -0.838   -0.701  0.451      
    ## prdd-:_18/19:TRUE -0.004  0.510  0.167  0.440    0.408 -0.854      
    ## prdp-:_18/19:TRUE -0.003  0.175  0.536  0.296    0.272 -0.189      
    ##                   prdp-:_18/19 prdd-:TRUE prdp-:TRUE s_18/19:
    ## prddrng-lck                                                  
    ## prdpst-lckd                                                  
    ## schl_y18/19                                                  
    ## mcqTRUE                                                      
    ## prdd-:_18/19                                                 
    ## prdp-:_18/19                                                 
    ## prddr-:TRUE        0.183                                     
    ## prdps-:TRUE        0.541        0.307                        
    ## s_18/19:TRU        0.308        0.483      0.303             
    ## prdd-:_18/19:TRUE -0.193       -0.617     -0.190     -0.524  
    ## prdp-:_18/19:TRUE -0.844       -0.204     -0.647     -0.355  
    ##                   prdd-:_18/19:TRUE
    ## prddrng-lck                        
    ## prdpst-lckd                        
    ## schl_y18/19                        
    ## mcqTRUE                            
    ## prdd-:_18/19                       
    ## prdp-:_18/19                       
    ## prddr-:TRUE                        
    ## prdps-:TRUE                        
    ## s_18/19:TRU                        
    ## prdd-:_18/19:TRUE                  
    ## prdp-:_18/19:TRUE  0.220

Save coefficients as a table for in the paper:

``` r
m_acc_coef <- as.data.frame(m_acc_summary$coefficients)
setDT(m_acc_coef, keep.rownames = TRUE)
m_acc_coef$rn <- c("Intercept \\small{(Period: pre-lockdown, School year: 19/20, Question type: open answer)}",
                   "Period: lockdown",
                   "Period: post-lockdown",
                   "School year: 18/19",
                   "Question type: multiple choice",
                   "Period: lockdown $\\times$ School year: 18/19",
                   "Period: post-lockdown $\\times$ School year: 18/19",
                   "Period: lockdown $\\times$ Question type: multiple choice",
                   "Period: post-lockdown $\\times$ Question type: multiple choice",
                   "School year: 18/19 $\\times$ Question type: multiple choice",
                   "Period: lockdown $\\times$ School year: 18/19 $\\times$ Question type: multiple choice",
                   "Period: post-lockdown $\\times$ School year: 18/19 $\\times$ Question type: multiple choice")

# Format p-values
m_acc_coef$`Pr(>|z|)` <- format.pval(m_acc_coef$`Pr(>|z|)`, eps = .001, digits = 3, flag = "0")
m_acc_coef$`Pr(>|z|)` <- sub('^(<)?0[.]', '\\1.', m_acc_coef$`Pr(>|z|)`) # Remove leading zero

cat(knitr::kable(m_acc_coef,
                 align = c("l","r", "r", "r", "r"),
                 digits = c(NA, 3, 3, 2, NA),
                 col.names = c("Effect", "$b$", "SE", "$z$", "$p$"),
                 format = "latex",
                 booktabs = TRUE,
                 escape = FALSE),
    file = "../output/m_acc_table.tex")
```

Visualise the model
fit:

``` r
acc_fit <- expand.grid(period = c("pre-lockdown", "during-lockdown", "post-lockdown"), school_year = c("18/19", "19/20"), mcq = c(TRUE, FALSE))
acc_fit <- cbind(acc_fit, accuracy = predict(m_acc, type = "response", re.form = NA, newdata = acc_fit))
acc_fit
```

    ##             period school_year   mcq  accuracy
    ## 1     pre-lockdown       18/19  TRUE 0.9377293
    ## 2  during-lockdown       18/19  TRUE 0.9376704
    ## 3    post-lockdown       18/19  TRUE 0.9357050
    ## 4     pre-lockdown       19/20  TRUE 0.9303005
    ## 5  during-lockdown       19/20  TRUE 0.9256981
    ## 6    post-lockdown       19/20  TRUE 0.9215727
    ## 7     pre-lockdown       18/19 FALSE 0.8517224
    ## 8  during-lockdown       18/19 FALSE 0.8450077
    ## 9    post-lockdown       18/19 FALSE 0.8396994
    ## 10    pre-lockdown       19/20 FALSE 0.8364283
    ## 11 during-lockdown       19/20 FALSE 0.8695082
    ## 12   post-lockdown       19/20 FALSE 0.8570944

``` r
ggplot(acc_fit, aes(x = period, y = accuracy, colour = school_year, lty = mcq, group = interaction(mcq, school_year))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(.7, 1), labels = scales::percent_format()) +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

Empirical
means:

``` r
accuracy_mean <- accuracy[(course == "English" & mcq == TRUE) | course == "French", .(accuracy = sum(accuracy * n)/sum(n)), by = .(period, school_year, mcq, user, course)][, .(accuracy = mean(accuracy), accuracy_sd = sd(accuracy)), by = .(period, school_year, mcq)]
accuracy_mean
```

    ##              period school_year   mcq  accuracy accuracy_sd
    ##  1:    pre-lockdown       18/19 FALSE 0.7750858  0.17982054
    ##  2:    pre-lockdown       18/19  TRUE 0.9336483  0.05318888
    ##  3: during-lockdown       18/19 FALSE 0.7760562  0.19067498
    ##  4: during-lockdown       18/19  TRUE 0.9321542  0.05725342
    ##  5:   post-lockdown       18/19 FALSE 0.7740634  0.18808125
    ##  6:   post-lockdown       18/19  TRUE 0.9298982  0.06057743
    ##  7:    pre-lockdown       19/20 FALSE 0.7748409  0.17764395
    ##  8:    pre-lockdown       19/20  TRUE 0.9321703  0.05260872
    ##  9: during-lockdown       19/20 FALSE 0.8369730  0.15166914
    ## 10: during-lockdown       19/20  TRUE 0.9251763  0.05434129
    ## 11:   post-lockdown       19/20 FALSE 0.8189800  0.18159554
    ## 12:   post-lockdown       19/20  TRUE 0.9204220  0.06260106

``` r
ggplot(accuracy_mean, aes(x = period, y = accuracy, colour = school_year, lty = mcq, group = interaction(mcq, school_year))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(.7, 1), labels = scales::percent_format()) +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

## Response time

``` r
db <- db_connect()
rt <- dbGetQuery(db, 
                 "SELECT r.method AS 'method',
                  r.book_info_id AS 'book_info_id',
                  DATE(r.date + 3600, 'unixepoch') AS 'doy',
                  r.user_id AS 'user',
                  r.choices > 1 AS 'mcq',
                  r.rt AS 'rt'
                  FROM 'responses_noduplicates' r
                  WHERE r.study == 0
                  AND r.correct == 1
                 "
)
setDT(rt)
db_disconnect(db)
```

``` r
doys <- rt[, .(doy = unique(doy))][, doy_posix := as.POSIXct(doy)][]
doys[, doy_posix_week := cut.POSIXt(as.POSIXct(doy), "week")]
doys[, school_year := ifelse(doy_posix < "2019-08-01", "18/19", "19/20")]
doys[school_year == "18/19", doy_posix_aligned := as.POSIXct(doy_posix + 365*24*60*60, origin = "1970-01-01")]
doys[school_year == "19/20", doy_posix_aligned := doy_posix]
doys[, doy_posix_aligned_week := cut.POSIXt(doy_posix_aligned, "week")]
doys[, period := dplyr::case_when(
  doy_posix_aligned >= date_schools_opened ~ "post-lockdown",
  doy_posix_aligned >= date_schools_closed & doy_posix_aligned < date_schools_opened ~ "during-lockdown",
  doy_posix_aligned < date_schools_opened ~ "pre-lockdown"
)]

# Reorder factor levels so that intercept is pre-lockdown in 19/20
doys[, period := factor(period, levels = c("pre-lockdown", "during-lockdown", "post-lockdown"))]
doys[, school_year := factor(school_year, levels = c("19/20", "18/19"))]
```

``` r
rt <- rt[doys, on = "doy"]
```

``` r
rt[, mcq := as.factor(as.logical(mcq))]
```

``` r
rt[, course := ifelse(method == "Grandes Lignes", "French", ifelse(method == "Stepping Stones", "English", "German"))]
```

Throw out trials with negative RTs (timing
errors)

``` r
rt <- rt[rt > 0]
```

### Whole population

``` r
rt_med <- rt[, .(rt_median = median(rt)), by = .(school_year, mcq, user, course, doy_posix_week)]

rt_by_week <- rt_med[, .(rt = mean(rt_median), rt_se = sd(rt_median)/sqrt(.N)), by = .(school_year, mcq, course, doy_posix_week)]
```

Overlap the two school
years:

``` r
rt_by_week[school_year == "18/19", doy_posix_week_aligned := as.POSIXct(as.POSIXct(doy_posix_week) + 365*24*60*60, origin = "1970-01-01")]
rt_by_week[school_year == "19/20", doy_posix_week_aligned := as.POSIXct(doy_posix_week)]
```

Add question type
labels:

``` r
rt_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

``` r
rt_by_week[, school_year := factor(school_year, levels = c("18/19", "19/20"))]
```

Plot response time by week (mean +/- 1
SE).

``` r
p_rt <- ggplot(rt_by_week[(course == "English" & mcq == TRUE) | course == "French",],
            aes(x = doy_posix_week_aligned, y = rt/1e3, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(. ~ course) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1000, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = rt/1e3 - rt_se/1e3, ymax = rt/1e3 + rt_se/1e3, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::unit_format(unit = "s", accuracy = .1)) +
  coord_cartesian(ylim = c(1.7, 3.7)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Response time",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_rt
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->

``` r
ggsave("../output/rt_by_question_type.pdf", width = 9, height = 3)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

``` r
ggsave("../output/rt_by_question_type.eps", width = 9, height = 3)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/rt_by_question_type.png", width = 9, height = 3)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

### By level and year

Add book
info:

``` r
rt[book_info[book_type == "Hoofdboek",], on  = "book_info_id", c("method_group", "book_title") := .(i.method_group, i.book_title)]
```

Simplify level names:

``` r
# Keep all distinctions
rt[, book_title_simple := stringr::str_sub(book_title, 3, -10)]
rt[, book_title_simple := factor(book_title_simple, levels = c("vmbo b/lwoo", "vmbo b", "vmbo bk", "vmbo k", "vmbo kgt", "vmbo-gt", "vmbo gt", "vmbo-gt/havo", "vmbo (t)hv", "havo", "havo vwo", "vwo"))]

# Simplify to three levels
rt[, level := dplyr::case_when(
  grepl( "hv", book_title) ~ "General secondary\n(havo)",
  grepl("vmbo", book_title) ~ "Pre-vocational\n(vmbo)",
  grepl("havo", book_title) ~ "General secondary\n(havo)",
  grepl("vwo", book_title) ~ "Pre-university\n(vwo)",
  TRUE ~ "Other")]
rt[, level := factor(level, levels = c("Other", "Pre-vocational\n(vmbo)", "General secondary\n(havo)", "Pre-university\n(vwo)"))]
```

Simplify year names:

``` r
rt[, year := dplyr::case_when(
  method_group == "Leerjaar 1 (5e Ed.)" ~ "Year 1",
  method_group == "Leerjaar 2 (5e Ed.)" ~ "Year 2",
  method_group == "Leerjaar 3 (5e Ed.)" ~ "Year 3",
  method_group == "Leerjaar 3/4 (5e Ed.)" ~ "Year 3/4",
  method_group == "Leerjaar 4 (5e Ed.)" ~ "Year 4",
  method_group == "Tweede Fase (6e Ed.)" ~ "Tweede Fase",
  TRUE ~ "Other")]
```

``` r
rt_strat_med <- rt[, .(rt_median = median(rt)), by = .(school_year, mcq, user, course, level, year, doy_posix_week)]

rt_strat_by_week <- rt_strat_med[, .(rt = mean(rt_median), rt_se = sd(rt_median)/sqrt(.N)), by = .(school_year, mcq, course, level, year, doy_posix_week)]
```

Overlap the two school
years:

``` r
rt_strat_by_week[school_year == "18/19", doy_posix_week_aligned := as.POSIXct(as.POSIXct(doy_posix_week) + 365*24*60*60, origin = "1970-01-01")]
rt_strat_by_week[school_year == "19/20", doy_posix_week_aligned := as.POSIXct(doy_posix_week)]
```

Add question type
labels:

``` r
rt_strat_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

``` r
rt_strat_by_week[, school_year := factor(school_year, levels = c("18/19", "19/20"))]
```

Plot response time by week (mean +/- 1 SE).

``` r
p_rt_level_year <- ggplot(rt_strat_by_week[course == "French",],
            aes(x = doy_posix_week_aligned, y = rt/1e3, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(level ~ year) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1000, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = rt/1e3 - rt_se/1e3, ymax = rt/1e3 + rt_se/1e3, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::unit_format(unit = "s", accuracy = .1)) +
  coord_cartesian(ylim = c(1, 4)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Response time",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_rt_level_year
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
ggsave("../output/rt_by_question_type_french_level_year.pdf", width = 9, height = 3)
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

``` r
ggsave("../output/rt_by_question_type_french_level_year.eps", width = 9, height = 3)
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/rt_by_question_type_french_level_year.png", width = 9, height = 3)
```

    ## Warning: Removed 11 row(s) containing missing values (geom_path).

``` r
p_rt_level_year <- ggplot(rt_strat_by_week[course == "English" & question_type == "Multiple\nchoice" & level != "Other",],
            aes(x = doy_posix_week_aligned, y = rt/1e3, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(level ~ year) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1000, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = rt/1e3 - rt_se/1e3, ymax = rt/1e3 + rt_se/1e3, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::unit_format(unit = "s", accuracy = .1)) +
  coord_cartesian(ylim = c(1, 4)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Response time",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_rt_level_year
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

``` r
ggsave("../output/rt_by_question_type_english_level_year.pdf", width = 9, height = 3)
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

``` r
ggsave("../output/rt_by_question_type_english_level_year.eps", width = 9, height = 3)
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/rt_by_question_type_english_level_year.png", width = 9, height = 3)
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).

### By level

``` r
rt_level_med <- rt[, .(rt_median = median(rt)), by = .(school_year, mcq, user, course, level, doy_posix_week)]

rt_level_by_week <- rt_level_med[, .(rt = mean(rt_median), rt_se = sd(rt_median)/sqrt(.N)), by = .(school_year, mcq, course, level, doy_posix_week)]
```

Overlap the two school
years:

``` r
rt_level_by_week[school_year == "18/19", doy_posix_week_aligned := as.POSIXct(as.POSIXct(doy_posix_week) + 365*24*60*60, origin = "1970-01-01")]
rt_level_by_week[school_year == "19/20", doy_posix_week_aligned := as.POSIXct(doy_posix_week)]
```

Add question type
labels:

``` r
rt_level_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

``` r
rt_level_by_week[, school_year := factor(school_year, levels = c("18/19", "19/20"))]
```

Plot response time by week (mean +/- 1
SE).

``` r
p_rt_level <- ggplot(rt_level_by_week[((course == "English" & question_type == "Multiple\nchoice") | course == "French") & level != "Other",],
            aes(x = doy_posix_week_aligned, y = rt/1e3, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(level ~ course) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1000, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = rt/1e3 - rt_se/1e3, ymax = rt/1e3 + rt_se/1e3, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::unit_format(unit = "s", accuracy = .1)) +
  coord_cartesian(ylim = c(1, 6)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Response time",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_rt_level
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
ggsave("../output/rt_by_question_type_level.pdf", width = 9, height = 5)
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

``` r
ggsave("../output/rt_by_question_type_level.eps", width = 9, height = 5)
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/rt_by_question_type_level.png", width = 9, height = 5)
```

    ## Warning: Removed 20 row(s) containing missing values (geom_path).

### By year

``` r
rt_year_med <- rt[, .(rt_median = median(rt)), by = .(school_year, mcq, user, course, year, doy_posix_week)]

rt_year_by_week <- rt_year_med[, .(rt = mean(rt_median), rt_se = sd(rt_median)/sqrt(.N)), by = .(school_year, mcq, course, year, doy_posix_week)]
```

Overlap the two school
years:

``` r
rt_year_by_week[school_year == "18/19", doy_posix_week_aligned := as.POSIXct(as.POSIXct(doy_posix_week) + 365*24*60*60, origin = "1970-01-01")]
rt_year_by_week[school_year == "19/20", doy_posix_week_aligned := as.POSIXct(doy_posix_week)]
```

Add question type
labels:

``` r
rt_year_by_week[, question_type := ifelse(mcq == TRUE, "Multiple\nchoice", "Open\nanswer")]
```

``` r
rt_year_by_week[, school_year := factor(school_year, levels = c("18/19", "19/20"))]
```

Plot response time by week (mean +/- 1
SE).

``` r
p_rt_year <- ggplot(rt_year_by_week[((course == "English" & question_type == "Multiple\nchoice") | course == "French") & year != "Other",],
            aes(x = doy_posix_week_aligned, y = rt/1e3, group = interaction(school_year, question_type), colour = school_year, fill = school_year)) +
  facet_grid(year ~ course) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = 0, ymax = 1000, fill = "grey92", colour = "grey50", lty = 2, alpha = .9) +
  geom_ribbon(aes(ymin = rt/1e3 - rt_se/1e3, ymax = rt/1e3 + rt_se/1e3, colour = NULL), alpha = 0.2) +
  geom_line(aes(lty = question_type)) +
  scale_x_datetime(expand = c(0, 0),
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::unit_format(unit = "s", accuracy = .1)) +
  coord_cartesian(ylim = c(1, 4)) +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  scale_fill_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Response time",
       colour = "School year",
       fill = "School year",
       lty = "Question type") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 1),
         lty = guide_legend(order = 2)) +
  theme_paper

p_rt_year
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-76-1.png)<!-- -->

``` r
ggsave("../output/rt_by_question_type_year.pdf", width = 9, height = 5)
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

``` r
ggsave("../output/rt_by_question_type_year.eps", width = 9, height = 5)
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/rt_by_question_type_year.png", width = 9, height = 5)
```

    ## Warning: Removed 14 row(s) containing missing values (geom_path).

### Regression model

``` r
rt_model_dat <- rt[, .(rt_median = median(rt)), by = .(course, school_year, period, doy_posix, mcq, user)]
```

Fit a generalised linear mixed effects model (assuming a Gamma
distribution for RT and an identity link function; Lo & Andrew, 2015) to
the daily median RT:

``` r
if(!file.exists("../output/m_rt_fit.rds")) {
  m_rt <- glmer(rt_median ~ period*school_year*mcq + (1 | user) + (1 | course),
                 data = rt_model_dat[(course == "English" & mcq == TRUE) | course == "French",],
                 family = Gamma(link = "identity"),
                 nAGQ = 0,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6)))
  saveRDS(m_rt, "../output/m_rt_fit.rds")
} else {
  m_rt <- readRDS("../output/m_rt_fit.rds")
}

m_rt_summary <- summary(m_rt)
m_rt_summary
```

    ## Generalized linear mixed model fit by maximum likelihood (Adaptive
    ##   Gauss-Hermite Quadrature, nAGQ = 0) [glmerMod]
    ##  Family: Gamma  ( identity )
    ## Formula: 
    ## rt_median ~ period * school_year * mcq + (1 | user) + (1 | course)
    ##    Data: rt_model_dat[(course == "English" & mcq == TRUE) | course ==  
    ##     "French", ]
    ## Control: 
    ## glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e+06))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ## 10294902 10295073 -5147436 10294872   666488 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ##  -2.667  -0.337  -0.069   0.235 205.150 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.
    ##  user     (Intercept) 2.935e+05 541.7736
    ##  course   (Intercept) 2.753e+02  16.5917
    ##  Residual             1.390e-01   0.3728
    ## Number of obs: 666503, groups:  user, 133398; course, 2
    ## 
    ## Fixed effects:
    ##                                                Estimate Std. Error t value
    ## (Intercept)                                    2134.021     12.889 165.574
    ## periodduring-lockdown                           208.799      7.148  29.209
    ## periodpost-lockdown                             197.658     11.085  17.831
    ## school_year18/19                                 27.350      6.744   4.055
    ## mcqTRUE                                         150.272      5.544  27.108
    ## periodduring-lockdown:school_year18/19         -281.173     11.675 -24.084
    ## periodpost-lockdown:school_year18/19           -293.256     17.615 -16.648
    ## periodduring-lockdown:mcqTRUE                  -211.632      7.688 -27.526
    ## periodpost-lockdown:mcqTRUE                    -200.943     12.213 -16.454
    ## school_year18/19:mcqTRUE                        -61.876      7.084  -8.734
    ## periodduring-lockdown:school_year18/19:mcqTRUE  279.138     12.590  22.171
    ## periodpost-lockdown:school_year18/19:mcqTRUE    316.380     19.331  16.366
    ##                                                Pr(>|z|)    
    ## (Intercept)                                     < 2e-16 ***
    ## periodduring-lockdown                           < 2e-16 ***
    ## periodpost-lockdown                             < 2e-16 ***
    ## school_year18/19                               5.01e-05 ***
    ## mcqTRUE                                         < 2e-16 ***
    ## periodduring-lockdown:school_year18/19          < 2e-16 ***
    ## periodpost-lockdown:school_year18/19            < 2e-16 ***
    ## periodduring-lockdown:mcqTRUE                   < 2e-16 ***
    ## periodpost-lockdown:mcqTRUE                     < 2e-16 ***
    ## school_year18/19:mcqTRUE                        < 2e-16 ***
    ## periodduring-lockdown:school_year18/19:mcqTRUE  < 2e-16 ***
    ## periodpost-lockdown:school_year18/19:mcqTRUE    < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##                   (Intr) prddr- prdps- sc_18/19 mcTRUE prdd-:_18/19
    ## prddrng-lck       -0.261                                           
    ## prdpst-lckd       -0.165  0.330                                    
    ## schl_y18/19       -0.289  0.484  0.308                             
    ## mcqTRUE           -0.349  0.567  0.363  0.621                      
    ## prdd-:_18/19       0.166 -0.619 -0.204 -0.533   -0.354             
    ## prdp-:_18/19       0.109 -0.214 -0.633 -0.354   -0.234  0.228      
    ## prddr-:TRUE        0.236 -0.862 -0.278 -0.436   -0.651  0.532      
    ## prdps-:TRUE        0.147 -0.272 -0.856 -0.273   -0.408  0.168      
    ## s_18/19:TRU        0.256 -0.441 -0.281 -0.880   -0.705  0.484      
    ## prdd-:_18/19:TRUE -0.148  0.531  0.171  0.475    0.404 -0.883      
    ## prdp-:_18/19:TRUE -0.096  0.176  0.543  0.310    0.263 -0.191      
    ##                   prdp-:_18/19 prdd-:TRUE prdp-:TRUE s_18/19:
    ## prddrng-lck                                                  
    ## prdpst-lckd                                                  
    ## schl_y18/19                                                  
    ## mcqTRUE                                                      
    ## prdd-:_18/19                                                 
    ## prdp-:_18/19                                                 
    ## prddr-:TRUE        0.179                                     
    ## prdps-:TRUE        0.541        0.301                        
    ## s_18/19:TRU        0.321        0.491      0.307             
    ## prdd-:_18/19:TRUE -0.194       -0.615     -0.186     -0.539  
    ## prdp-:_18/19:TRUE -0.873       -0.194     -0.634     -0.353  
    ##                   prdd-:_18/19:TRUE
    ## prddrng-lck                        
    ## prdpst-lckd                        
    ## schl_y18/19                        
    ## mcqTRUE                            
    ## prdd-:_18/19                       
    ## prdp-:_18/19                       
    ## prddr-:TRUE                        
    ## prdps-:TRUE                        
    ## s_18/19:TRU                        
    ## prdd-:_18/19:TRUE                  
    ## prdp-:_18/19:TRUE  0.212

Save coefficients as a table for in the paper:

``` r
m_rt_coef <- as.data.frame(m_rt_summary$coefficients)
setDT(m_rt_coef, keep.rownames = TRUE)
m_rt_coef$rn <- c("Intercept \\small{(Period: pre-lockdown, School year: 19/20, Question type: open answer)}",
                   "Period: lockdown",
                   "Period: post-lockdown",
                   "School year: 18/19",
                   "Question type: multiple choice",
                   "Period: lockdown $\\times$ School year: 18/19",
                   "Period: post-lockdown $\\times$ School year: 18/19",
                   "Period: lockdown $\\times$ Question type: multiple choice",
                   "Period: post-lockdown $\\times$ Question type: multiple choice",
                   "School year: 18/19 $\\times$ Question type: multiple choice",
                   "Period: lockdown $\\times$ School year: 18/19 $\\times$ Question type: multiple choice",
                   "Period: post-lockdown $\\times$ School year: 18/19 $\\times$ Question type: multiple choice")

# Format p-values
m_rt_coef$`Pr(>|z|)` <- format.pval(m_rt_coef$`Pr(>|z|)`, eps = .001, digits = 3, flag = "0")
m_rt_coef$`Pr(>|z|)` <- sub('^(<)?0[.]', '\\1.', m_rt_coef$`Pr(>|z|)`) # Remove leading zero

cat(knitr::kable(m_rt_coef,
                 align = c("l","r", "r", "r", "r"),
                 digits = c(NA, 3, 3, 2, NA),
                 col.names = c("Effect", "$b$", "SE", "$z$", "$p$"),
                 format = "latex",
                 booktabs = TRUE,
                 escape = FALSE),
    file = "../output/m_rt_table.tex")
```

Visualise the model
fit:

``` r
rt_fit <- expand.grid(period = c("pre-lockdown", "during-lockdown", "post-lockdown"), school_year = c("18/19", "19/20"), mcq = c(TRUE, FALSE))
rt_fit <- cbind(rt_fit, rt = predict(m_rt, type = "response", re.form = NA, newdata = rt_fit))
rt_fit
```

    ##             period school_year   mcq       rt
    ## 1     pre-lockdown       18/19  TRUE 2249.768
    ## 2  during-lockdown       18/19  TRUE 2244.900
    ## 3    post-lockdown       18/19  TRUE 2269.607
    ## 4     pre-lockdown       19/20  TRUE 2284.294
    ## 5  during-lockdown       19/20  TRUE 2281.461
    ## 6    post-lockdown       19/20  TRUE 2281.009
    ## 7     pre-lockdown       18/19 FALSE 2161.371
    ## 8  during-lockdown       18/19 FALSE 2088.996
    ## 9    post-lockdown       18/19 FALSE 2065.773
    ## 10    pre-lockdown       19/20 FALSE 2134.021
    ## 11 during-lockdown       19/20 FALSE 2342.820
    ## 12   post-lockdown       19/20 FALSE 2331.680

``` r
ggplot(rt_fit, aes(x = period, y = rt, colour = school_year, lty = mcq, group = interaction(mcq, school_year))) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(1500, 4000)) +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-80-1.png)<!-- -->

Empirical
means:

``` r
rt_mean <- rt_model_dat[(course == "English" & mcq == TRUE) | course == "French", .(rt = mean(rt_median)), by = .(period, school_year, mcq, user, course)][, .(rt = mean(rt), rt_sd = sd(rt)), by = .(period, school_year, mcq, course)]
rt_mean[, school_year := factor(school_year, levels = c("18/19", "19/20"))]
rt_mean
```

    ##              period school_year   mcq  course       rt     rt_sd
    ##  1:    pre-lockdown       19/20 FALSE  French 2441.304 4577.4682
    ##  2:    pre-lockdown       19/20  TRUE  French 2361.956 1002.6358
    ##  3:    pre-lockdown       19/20  TRUE English 2224.106  822.6467
    ##  4: during-lockdown       19/20  TRUE English 2200.428  603.3756
    ##  5: during-lockdown       19/20 FALSE  French 2709.270 3576.3899
    ##  6: during-lockdown       19/20  TRUE  French 2360.277 1374.4151
    ##  7:   post-lockdown       18/19  TRUE English 2234.020 1106.6481
    ##  8:   post-lockdown       18/19  TRUE  French 2364.000 2226.5155
    ##  9:   post-lockdown       18/19 FALSE  French 2217.881 1548.1445
    ## 10:    pre-lockdown       18/19 FALSE  French 2461.292 9161.1828
    ## 11:    pre-lockdown       18/19  TRUE  French 2341.094  581.5368
    ## 12:    pre-lockdown       18/19  TRUE English 2211.323 2572.3131
    ## 13:   post-lockdown       19/20  TRUE English 2215.619  720.8126
    ## 14:   post-lockdown       19/20  TRUE  French 2313.358  561.6664
    ## 15:   post-lockdown       19/20 FALSE  French 2820.872 8731.1647
    ## 16: during-lockdown       18/19  TRUE English 2202.397 1123.5965
    ## 17: during-lockdown       18/19 FALSE  French 2434.324 7199.2482
    ## 18: during-lockdown       18/19  TRUE  French 2369.624 5658.9356

``` r
ggplot(rt_mean, aes(x = period, y = rt, colour = school_year, lty = mcq, group = interaction(mcq, school_year))) +
  facet_grid(~ course) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(1500, 3000)) +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

## Combination plot

``` r
p_legend <- get_legend(p_acc)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).

``` r
p_acc <- p_acc +
  guides(colour = FALSE, fill = FALSE, lty = FALSE)

p_rt <- p_rt +
  guides(colour = FALSE, fill = FALSE, lty = FALSE)
```

Combine plots:

``` r
plot_grid(
  plot_grid(p_acc, p_rt,
            ncol = 1,
            labels = c("A", "B")),
  p_legend,
  rel_widths = c(1, .2)
)
```

    ## Warning: Removed 24 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 24 row(s) containing missing values (geom_path).

![](02_performance_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

``` r
ggsave("../output/combi_acc_rt.pdf", width = 9, height = 3.5)
ggsave("../output/combi_acc_rt.eps", width = 9, height = 3.5)
```

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/combi_acc_rt.png", width = 9, height = 3.5)
```

# Learning progress

Get the unique book chapter IDs on each day:

``` r
db <- db_connect()

progress <- dbGetQuery(db,
                       "SELECT DISTINCT r.book_info_id AS 'book_info_id',
                        r.method AS 'method',
                        DATE(r.date + 3600, 'unixepoch') AS 'doy',
                        COUNT(*) AS 'trials'
                        FROM 'responses_noduplicates' r
                        GROUP BY r.method,
                        r.book_info_id,
                        DATE(r.date + 3600, 'unixepoch');"
                       )

db_disconnect(db)

setDT(progress)
```

Join with the book chapter information:

``` r
db <- db_connect()
book_info <- dbGetQuery(db, "SELECT DISTINCT * FROM 'book_info'")
db_disconnect(db)

setDT(book_info)
```

``` r
progress[book_info[book_type == "Hoofdboek",], on  = "book_info_id", c("method_group", "book_title", "chapter") := .(i.method_group, i.book_title, i.chapter)]
```

Add sensible course
names:

``` r
progress[, course := ifelse(method == "Grandes Lignes", "French", ifelse(method == "Stepping Stones", "English", "German"))]
```

Add a school year column (cutoff date: 1 August):

``` r
progress[, doy_posix := as.POSIXct(doy)]
progress[, school_year := ifelse(doy_posix < "2019-08-01", "18/19", "19/20")]
```

Consolidate count by day and
chapter:

``` r
progress_by_day <- progress[, .(trials = sum(trials)), by = .(school_year, doy_posix, course, method_group, book_title, chapter)]
```

Simplify level names:

``` r
# Keep all distinctions
progress_by_day[, book_title_simple := stringr::str_sub(book_title, 3, -10)]
progress_by_day[, book_title_simple := factor(book_title_simple, levels = c("vmbo b/lwoo", "vmbo b", "vmbo bk", "vmbo k", "vmbo kgt", "vmbo-gt", "vmbo gt", "vmbo-gt/havo", "vmbo (t)hv", "havo", "havo vwo", "vwo"))]

# Simplify to three levels
progress_by_day[, level := dplyr::case_when(
  grepl( "hv", book_title) ~ "General secondary (havo)",
  grepl("vmbo", book_title) ~ "Pre-vocational (vmbo)",
  grepl("havo", book_title) ~ "General secondary (havo)",
  grepl("vwo", book_title) ~ "Pre-university (vwo)",
  TRUE ~ "Other")]
progress_by_day[, level := factor(level, levels = c("Other", "Pre-vocational (vmbo)", "General secondary (havo)", "Pre-university (vwo)"))]
```

Simplify year names:

``` r
progress_by_day[, year := dplyr::case_when(
  method_group == "Leerjaar 1 (5e Ed.)" ~ "Year 1",
  method_group == "Leerjaar 2 (5e Ed.)" ~ "Year 2",
  method_group == "Leerjaar 3 (5e Ed.)" ~ "Year 3",
  method_group == "Leerjaar 3/4 (5e Ed.)" ~ "Year 3/4",
  method_group == "Leerjaar 4 (5e Ed.)" ~ "Year 4",
  method_group == "Tweede Fase (6e Ed.)" ~ "Tweede Fase",
  TRUE ~ "Other")]
```

Simplify chapter names:

``` r
# In most cases, the chapter name starts with a number
progress_by_day[, chapter_simple := factor(as.numeric(stringr::str_extract(chapter, "^\\d{1,2}")))]

# Remaining cases:
unique(progress_by_day[is.na(chapter_simple),]$chapter)
```

    ##  [1] "BS2 Dienstleistung"               
    ##  [2] "BS5 Reisen"                       
    ##  [3] "BS1 Familie und Beziehungen"      
    ##  [4] "BS3 Dienstleistung"               
    ##  [5] "BS4 Reisen und Verkehr"           
    ##  [6] "Lernliste Brückenschlag"          
    ##  [7] "BS2 Freizeit"                     
    ##  [8] "BS1 Schule und Ausbildung"        
    ##  [9] "Bridging the Gap Year 2"          
    ## [10] "Bridging the Gap Year 1"          
    ## [11] "Bridging the Gap Exam Preparation"
    ## [12] "Exam Preparation"                 
    ## [13] "Bridging the Gap mbo"             
    ## [14] "Bridging the Gap havo"

``` r
# Combine these chapters into an "other" category
progress_by_day[is.na(chapter_simple), chapter_simple := "O"]
```

Align school
years:

``` r
progress_by_day[school_year == "18/19", doy_posix_aligned := as.POSIXct(doy_posix + 365*24*60*60, origin = "1970-01-01")]
progress_by_day[school_year == "19/20", doy_posix_aligned := doy_posix]
```

Use cut.Date() to bin dates by week and month. Each day is assigned the
date of the most recent
Monday.

``` r
progress_by_day[, doy_posix_aligned_week := cut.POSIXt(doy_posix_aligned, "week")]
progress_by_day[, doy_posix_aligned_month := cut.POSIXt(doy_posix_aligned, "month")]
```

Calculate proportions by week and
month:

``` r
progress_by_week <- progress_by_day[, .(trials = sum(trials)), by = .(school_year, doy_posix_aligned_week, course, level, year, chapter_simple)]
progress_by_week[, prop := trials/sum(trials), by = .(school_year, doy_posix_aligned_week, course, level, year)]

progress_by_month <- progress_by_day[, .(trials = sum(trials)), by = .(school_year, doy_posix_aligned_month, course, level, year, chapter_simple)]
progress_by_month[, prop := trials/sum(trials), by = .(school_year, doy_posix_aligned_month, course, level, year)]
```

``` r
setorder(progress_by_week, chapter_simple)
setorder(progress_by_month, chapter_simple)
```

### French

``` r
p_french_y1 <- ggplot(progress_by_week[course == "French" & year == "Year 1"], aes(x = as.POSIXct(doy_posix_aligned_week), y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(school_year ~ level) +
  geom_col(alpha = 0.75, width = 7*24*60*60, colour = NA) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = -0.01, ymax = 1.01, fill = NA, colour = "black", lty = 2) +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), breaks = c(0, .5 , 1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter") +
  theme_paper

p_french_y2 <- ggplot(progress_by_week[course == "French" & year == "Year 2"], aes(x = as.POSIXct(doy_posix_aligned_week), y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(school_year ~ level) +
  geom_col(alpha = 0.75, width = 7*24*60*60, colour = NA) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = -0.01, ymax = 1.01, fill = NA, colour = "black", lty = 2) +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), breaks = c(0, .5 , 1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter") +
  theme_paper

p_french_y3 <- ggplot(progress_by_week[course == "French" & year == "Year 3/4"], aes(x = as.POSIXct(doy_posix_aligned_week), y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(school_year ~ level) +
  geom_col(alpha = 0.75, width = 7*24*60*60, colour = NA) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = -0.01, ymax = 1.01, fill = NA, colour = "black", lty = 2) +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), breaks = c(0, .5 , 1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter") +
  theme_paper

p_progress_french <- plot_grid(p_french_y1, p_french_y2, p_french_y3, 
          ncol = 1,
          align = "hv", axis = "tblr",
          labels = c("Year 1", "Year 2", "Year 3/4"),
          hjust = -0.1,
          scale = .95)
```

    ## Warning: Removed 48 rows containing missing values (position_stack).

    ## Warning: Removed 40 rows containing missing values (geom_col).

    ## Warning: Removed 46 rows containing missing values (position_stack).

    ## Warning: Removed 41 rows containing missing values (geom_col).

    ## Warning: Removed 24 rows containing missing values (position_stack).

    ## Warning: Removed 23 rows containing missing values (geom_col).

``` r
p_progress_french
```

![](02_performance_files/figure-gfm/unnamed-chunk-98-1.png)<!-- -->

``` r
ggsave("../output/progress_french.pdf", width = 9, height = 9)
ggsave("../output/progress_french.eps", width = 9, height = 9)
```

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/progress_french.png", width = 9, height = 6)
```

Did the share of trials change between school years? We can simplify the
analysis by aggregating over the whole lockdown
period.

``` r
progress_lockdown <- progress_by_day[between(doy_posix_aligned, date_schools_closed, date_schools_opened), .(trials = sum(trials)), by = .(school_year, course, level, year, chapter_simple)]

# Fill in missing rows (occurs when chapter was only studied in one of the two years)
progress_lockdown <- as.data.table(tidyr::complete(progress_lockdown, tidyr::nesting(course, level, year, chapter_simple), school_year, fill = list(trials = 0))) 
  
progress_lockdown[, prop := trials/sum(trials), by = .(school_year, course, level, year)]

setorder(progress_lockdown, chapter_simple)
```

``` r
ggplot(progress_lockdown[course == "French"], aes(x = school_year, y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(level ~ year) +
  geom_col(colour = NA) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter",
       title = "French") +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-100-1.png)<!-- -->

Perform a chi-square test of homogeneity to determine whether school
years are significantly different.

``` r
for (y in sort(unique(progress_lockdown$year))) {
  for (l in levels(progress_lockdown$level)) {
    d <- progress_lockdown[course == "French" & year == y & level == l]
    if (nrow(d) > 0) {
      print(paste("French", y, l, collapse= " "))
      print(
        chisq.test(
          dcast(d, school_year ~ chapter_simple, value.var = "trials", fill = 0)[, school_year := NULL]
        )
      )
    }
  } 
}
```

    ## [1] "French Year 1 Pre-vocational (vmbo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 26799, df = 5, p-value < 2.2e-16
    ## 
    ## [1] "French Year 1 General secondary (havo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 229477, df = 5, p-value < 2.2e-16
    ## 
    ## [1] "French Year 1 Pre-university (vwo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 40312, df = 5, p-value < 2.2e-16
    ## 
    ## [1] "French Year 2 Pre-vocational (vmbo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 13882, df = 5, p-value < 2.2e-16
    ## 
    ## [1] "French Year 2 General secondary (havo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 50486, df = 5, p-value < 2.2e-16
    ## 
    ## [1] "French Year 2 Pre-university (vwo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 46882, df = 5, p-value < 2.2e-16
    ## 
    ## [1] "French Year 3/4 Pre-vocational (vmbo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 30269, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "French Year 3/4 General secondary (havo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 23169, df = 5, p-value < 2.2e-16
    ## 
    ## [1] "French Year 3/4 Pre-university (vwo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 21545, df = 5, p-value < 2.2e-16

Conclusion: all tests indicate a difference in proportions between
school years (p \<\< 0.001).

Visualise the change between school
years:

``` r
progress_lockdown[, prop_change := prop[school_year == "19/20"] - prop[school_year == "18/19"], by = .(course, level, year, chapter_simple)]

ggplot(progress_lockdown[school_year == "19/20" & course == "French"], aes(x = chapter_simple, y = (prop_change * 100), colour = chapter_simple, group = school_year)) +
  facet_grid(level ~ year, scales = "free_x") +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(xend = chapter_simple), yend = 0) +
  geom_point() +
  scale_y_continuous(limits = c(-25, 25)) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = "Chapter",
       y = "Change in trial share\n(percentage points)",
       title = "French") +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-102-1.png)<!-- -->

Are these changes really important? We may expect a certain amount of
fluctuation between any pair of school years. We don’t have data from
before the 18/19 school year, but we can look at how the magnitude of
changes during the lockdown period compares to changes earlier in the
school year.

To keep things as comparable as possible, use a sliding time window with
the same size as the lockdown
period:

``` r
time_window <- as.numeric(round(date_schools_opened - date_schools_closed))
time_window
```

    ## [1] 78

``` r
date_range <- sort(unique(progress_by_day$doy_posix_aligned))
date_range <- date_range[date_range < date_schools_closed]

prop_change_window <- data.table()

for (i in 1:(length(date_range) - as.numeric(time_window))) {
  d <- date_range[i:(i + time_window - 1)]
  progress_window <- progress_by_day[course %in% c("French", "English") & doy_posix_aligned %in% d,
                                     .(trials = sum(trials)),
                                     by = .(school_year, course, level, year, chapter_simple)]
  
  # Fill in missing rows (occurs when chapter was only studied in one of the two years)
  progress_window <- as.data.table(tidyr::complete(progress_window, tidyr::nesting(course, level, year, chapter_simple), school_year, fill = list(trials = 0))) 
  
  progress_window[, prop := trials/sum(trials), by = .(school_year, course, level, year)]

  progress_window[, prop_change := prop[school_year == "19/20"] - prop[school_year == "18/19"], by = .(course, level, year, chapter_simple)]
  
  prop_change_window <- rbind(prop_change_window, progress_window[school_year == "19/20",][,window := i][,date_min := min(d)][,date_max := max(d)])
}
```

The density of year-to-year changes can be visualised by time
window:

``` r
ggplot(prop_change_window, aes(x = prop_change * 100, y = window, group = window)) +
  ggridges::geom_density_ridges(alpha = 0.1, scale = 25, fill = NA) +
  labs(x = "Change in trial share\n(percentage points)",
         y = "Time window") +
  theme_paper
```

    ## Picking joint bandwidth of 0.584

![](02_performance_files/figure-gfm/unnamed-chunk-105-1.png)<!-- -->

Compare the aggregated density to the changes during the lockdown
period:

``` r
prop_change_combined <- rbind(prop_change_window[, period := "Pre-lockdown"], progress_lockdown[course %in% c("French", "English") & school_year == "19/20", period := "Lockdown"], fill = TRUE)
prop_change_combined[, period := factor(period, levels = c("Pre-lockdown", "Lockdown"))]

ggplot(prop_change_combined, aes(x = prop_change, colour = period)) +
  geom_density() +
  scale_colour_viridis_d(end = .5, direction = -1, na.translate = FALSE) +
  labs(x = "Change in trial share\n(percentage points)",
       y = "Density",
       colour = NULL) +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-106-1.png)<!-- -->

``` r
prop_change_sd <- prop_change_window[, .(sd = sd(prop_change) * 100), by = .(course, year, level)]
```

Add boundaries based on the typical spread to the change
plot:

``` r
p_change_french <- ggplot(progress_lockdown[school_year == "19/20" & course == "French"], aes(colour = chapter_simple)) +
  facet_grid(year ~ level, scales = "free_x") +
  geom_rect(data = prop_change_sd[course == "French"], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "French"], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100))) +
  scale_y_continuous(breaks = c(-20, 0, 20)) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = "Chapter",
       y = "Change in trial share\n(percentage points)") +
  theme_paper +
  theme(panel.grid.major.y = element_blank())

p_change_french
```

![](02_performance_files/figure-gfm/unnamed-chunk-108-1.png)<!-- -->

``` r
ggsave("../output/progress_change_french.pdf", width = 5, height = 4)
ggsave("../output/progress_change_french.eps", width = 5, height = 4)
ggsave("../output/progress_change_french.png", width = 9, height = 3)
```

Make a combination plot for in the paper:

``` r
plot_grid(p_french_y1, p_french_y2, p_french_y3, p_change_french,
          ncol = 1,
          align = "hv", axis = "tblr",
          labels = c("Year 1", "Year 2", "Year 3/4", "Change"),
          rel_heights = c(1, 1, 1, 1.5),
          hjust = -0.1,
          scale = .95)
```

    ## Warning: Removed 48 rows containing missing values (position_stack).

    ## Warning: Removed 40 rows containing missing values (geom_col).

    ## Warning: Removed 46 rows containing missing values (position_stack).

    ## Warning: Removed 41 rows containing missing values (geom_col).

    ## Warning: Removed 24 rows containing missing values (position_stack).

    ## Warning: Removed 23 rows containing missing values (geom_col).

![](02_performance_files/figure-gfm/unnamed-chunk-109-1.png)<!-- -->

``` r
ggsave("../output/progress_combi_french.pdf", width = 9, height = 9)
ggsave("../output/progress_combi_french.eps", width = 9, height = 9)
```

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/progress_combi_french.png", width = 9, height = 9)
```

``` r
p_change_french_y1 <- ggplot(progress_lockdown[school_year == "19/20" & course == "French" & year == "Year 1"], aes(colour = chapter_simple)) +
  facet_grid(. ~ level, scales = "free_x") +
  geom_rect(data = prop_change_sd[course == "French" & year == "Year 1"], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "French" & year == "Year 1"], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0, alpha = .75) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100)), alpha = .75) +
  scale_y_continuous(breaks = c(-20, 0, 20), limits = c(-30, 30), labels = scales::number_format(suffix = " pp")) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = NULL,
       y = "Change") +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

p_change_french_y2 <- ggplot(progress_lockdown[school_year == "19/20" & course == "French" & year == "Year 2"], aes(colour = chapter_simple)) +
  facet_grid(. ~ level, scales = "free_x") +
  geom_rect(data = prop_change_sd[course == "French" & year == "Year 2"], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "French" & year == "Year 2"], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0, alpha = .75) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100)), alpha = .75) +
  scale_y_continuous(breaks = c(-20, 0, 20),  limits = c(-30, 30), labels = scales::number_format(suffix = " pp")) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = NULL,
       y = "Change") +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

p_change_french_y3 <- ggplot(progress_lockdown[school_year == "19/20" & course == "French" & year == "Year 3/4"], aes(colour = chapter_simple)) +
  facet_grid(. ~ level, scales = "free_x") +
  geom_rect(data = prop_change_sd[course == "French" & year == "Year 3/4"], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "French" & year == "Year 3/4"], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0, alpha = .75) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100)), alpha = .75) +
  scale_y_continuous(breaks = c(-20, 0, 20),  limits = c(-30, 30), labels = scales::number_format(suffix = " pp")) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = NULL,
       y = "Change") +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

filler_plot <- qplot() + 
  theme_nothing() + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

plot_grid(
          filler_plot,
          p_french_y1, filler_plot, p_change_french_y1, filler_plot,
          p_french_y2, filler_plot, p_change_french_y2, filler_plot, 
          p_french_y3, filler_plot, p_change_french_y3,
          ncol = 1,
          align = "hv", axis = "tblr",
          labels = c(NA,
                     "Year 1", NA, NA, NA,
                     "Year 2", NA, NA, NA,
                     "Year 3/4", NA, NA),
          rel_heights = c(.1, 
                          1, -.2, .75, .1,
                          1, -.2, .75, .1,
                          1, -.2, .75),
          hjust = -0.1,
          vjust = -0.1,
          scale = .95)
```

    ## Warning: Removed 48 rows containing missing values (position_stack).

    ## Warning: Removed 40 rows containing missing values (geom_col).

    ## Warning: Removed 46 rows containing missing values (position_stack).

    ## Warning: Removed 41 rows containing missing values (geom_col).

    ## Warning: Removed 24 rows containing missing values (position_stack).

    ## Warning: Removed 23 rows containing missing values (geom_col).

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

![](02_performance_files/figure-gfm/unnamed-chunk-110-1.png)<!-- -->

``` r
ggsave("../output/progress_combi_alt_french.pdf", width = 9, height = 9)
```

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

``` r
ggsave("../output/progress_combi_alt_french.eps", width = 9, height = 9)
```

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/progress_combi_alt_french.png", width = 9, height = 9)
```

    ## Warning: Removed 1 rows containing missing values (geom_text).

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

### English

NOTE: chapters without a number (Bridging the Gap, Exam Preparation) are
shown as “O” in the plot. They don’t seem to fit neatly in the chapter
sequence, so I’m grouping them
together.

``` r
p_english_y1 <- ggplot(progress_by_week[level != "Other"][, level := factor(level)][course == "English" & year == "Year 1"], aes(x = as.POSIXct(doy_posix_aligned_week), y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(school_year ~ level, drop = FALSE) +
  geom_col(alpha = 0.75, width = 7*24*60*60, colour = NA) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = -0.01, ymax = 1.01, fill = NA, colour = "black", lty = 2) +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), breaks = c(0, .5 , 1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter") +
  theme_paper

p_english_y2 <- ggplot(progress_by_week[level != "Other"][, level := factor(level)][course == "English" & year == "Year 2"], aes(x = as.POSIXct(doy_posix_aligned_week), y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(school_year ~ level, drop = FALSE) +
  geom_col(alpha = 0.75, width = 7*24*60*60, colour = NA) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = -0.01, ymax = 1.01, fill = NA, colour = "black", lty = 2) +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), breaks = c(0, .5 , 1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter") +
  theme_paper

p_english_y3 <- ggplot(progress_by_week[level != "Other"][, level := factor(level)][course == "English" & year == "Year 3"], aes(x = as.POSIXct(doy_posix_aligned_week), y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(school_year ~ level, drop = FALSE) +
  geom_col(alpha = 0.75, width = 7*24*60*60, colour = NA) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = -0.01, ymax = 1.01, fill = NA, colour = "black", lty = 2) +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), breaks = c(0, .5 , 1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter") +
  theme_paper

p_english_y4 <- ggplot(progress_by_week[level != "Other"][, level := factor(level)][course == "English" & year == "Year 4"], aes(x = as.POSIXct(doy_posix_aligned_week), y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(school_year ~ level, drop = FALSE) +
  geom_col(alpha = 0.75, width = 7*24*60*60, colour = NA) +
  geom_rect(xmin = date_schools_closed, xmax = date_schools_opened, ymin = -0.01, ymax = 1.01, fill = NA, colour = "black", lty = 2) +
  scale_x_datetime(expand = c(0, 0), 
                   breaks = as.POSIXct(c(
                     "2019-10-01 02:00:00 CET",
                     "2019-12-01 02:00:00 CET",
                     "2020-02-01 02:00:00 CET",
                     "2020-04-01 02:00:00 CET",
                     "2020-06-01 02:00:00 CET")),
                   limits = as.POSIXct(c("2019-09-01 02:00:00 CET", "2020-07-01 02:00:00 CET")),
                   date_labels = "%b") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1), breaks = c(0, .5 , 1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter") +
  theme_paper

p_progress_english <- plot_grid(p_english_y1, p_english_y2, p_english_y3, p_english_y4,
          ncol = 1,
          align = "hv", axis = "tblr",
          labels = c("Year 1", "Year 2", "Year 3", "Year 4"),
          hjust = -0.1,
          scale = .95)
```

    ## Warning: Removed 61 rows containing missing values (position_stack).

    ## Warning: Removed 61 rows containing missing values (geom_col).

    ## Warning: Removed 69 rows containing missing values (position_stack).

    ## Warning: Removed 60 rows containing missing values (geom_col).

    ## Warning: Removed 49 rows containing missing values (position_stack).

    ## Warning: Removed 50 rows containing missing values (geom_col).

    ## Warning: Removed 7 rows containing missing values (position_stack).

    ## Warning: Removed 4 rows containing missing values (geom_col).

``` r
p_progress_english
```

![](02_performance_files/figure-gfm/unnamed-chunk-111-1.png)<!-- -->

``` r
ggsave("../output/progress_english.pdf", width = 9, height = 9)
ggsave("../output/progress_english.eps", width = 9, height = 9)
```

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/progress_english.png", width = 9, height = 9)
```

Did the share of trials change between school
years?

``` r
ggplot(progress_lockdown[course == "English" & level != "Other"], aes(x = school_year, y = prop, fill = chapter_simple, group = school_year)) +
  facet_grid(level ~ year) +
  geom_col(colour = NA) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  scale_fill_viridis_d(direction = -1, na.translate = FALSE) +
  labs(x = NULL,
       y = "Share of trials",
       fill = "Chapter",
       title = "English") +
  theme_paper
```

![](02_performance_files/figure-gfm/unnamed-chunk-112-1.png)<!-- -->

Change between school
years:

``` r
p_change_english <- ggplot(progress_lockdown[school_year == "19/20" & course == "English" & level != "Other"], aes(colour = chapter_simple)) +
  facet_grid(year ~ level, scales = "free_x") +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other"], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other"], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100))) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = "Chapter",
       y = "Change in trial share\n(percentage points)") +
  theme_paper +
  theme(panel.grid.major.y = element_blank())

p_change_english 
```

![](02_performance_files/figure-gfm/unnamed-chunk-113-1.png)<!-- -->

``` r
ggsave("../output/progress_change_english.pdf", width = 9, height = 6)
ggsave("../output/progress_change_english.eps", width = 9, height = 6)
ggsave("../output/progress_change_english.png", width = 9, height = 6)
```

Perform a chi-square test of homogeneity to determine whether school
years are significantly different.

``` r
for (y in sort(unique(progress_lockdown$year))) {
  for (l in levels(progress_lockdown$level)) {
    d <- progress_lockdown[course == "English" & year == y & level == l]
    if (nrow(d) > 0) {
      print(paste("English", y, l, collapse= " "))
      print(
        chisq.test(
          dcast(d, school_year ~ chapter_simple, value.var = "trials", fill = 0)[, school_year := NULL]
        )
      )
    }
  } 
}
```

    ## [1] "English Year 1 Other"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 1902.8, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "English Year 1 Pre-vocational (vmbo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 59650, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "English Year 1 General secondary (havo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 236327, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "English Year 1 Pre-university (vwo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 47589, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "English Year 2 Other"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 4117.3, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "English Year 2 Pre-vocational (vmbo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 26122, df = 8, p-value < 2.2e-16
    ## 
    ## [1] "English Year 2 General secondary (havo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 16303, df = 8, p-value < 2.2e-16
    ## 
    ## [1] "English Year 2 Pre-university (vwo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 84027, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "English Year 3 Other"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 16118, df = 6, p-value < 2.2e-16
    ## 
    ## [1] "English Year 3 Pre-vocational (vmbo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 20169, df = 6, p-value < 2.2e-16
    ## 
    ## [1] "English Year 3 General secondary (havo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 60993, df = 8, p-value < 2.2e-16
    ## 
    ## [1] "English Year 3 Pre-university (vwo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 41120, df = 7, p-value < 2.2e-16
    ## 
    ## [1] "English Year 4 Pre-vocational (vmbo)"
    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dcast(d, school_year ~ chapter_simple, value.var = "trials",     fill = 0)[, `:=`(school_year, NULL)]
    ## X-squared = 20187, df = 5, p-value < 2.2e-16

Conclusion: all tests indicate a difference in proportions between
school years (p \<\< 0.001).

Make a combination plot for in the
paper:

``` r
progress_lockdown_english <- progress_lockdown[level != "Other" & school_year == "19/20" & course == "English"]
progress_lockdown_english[, level := factor(level)]

p_change_english_y1 <- ggplot(progress_lockdown_english[year == "Year 1"], aes(colour = chapter_simple)) +
  facet_grid(. ~ level, scales = "free_x", drop = FALSE) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 1"][,level := factor(level)], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 1"][,level := factor(level)], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0, alpha = .75) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100)), alpha = .75) +
  scale_y_continuous(breaks = c(-10, 0, 10), limits = c(-20, 20), labels = scales::number_format(suffix = " pp")) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = NULL,
       y = "Change") +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

p_change_english_y2 <- ggplot(progress_lockdown_english[year == "Year 2"], aes(colour = chapter_simple)) +
  facet_grid(. ~ level, scales = "free_x", drop = FALSE) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 2"][,level := factor(level)], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 2"][,level := factor(level)], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0, alpha = .75) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100)), alpha = .75) +
  scale_y_continuous(breaks = c(-10, 0, 10), limits = c(-20, 20), labels = scales::number_format(suffix = " pp")) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = NULL,
       y = "Change") +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

p_change_english_y3 <- ggplot(progress_lockdown_english[year == "Year 3"], aes(colour = chapter_simple)) +
  facet_grid(. ~ level, scales = "free_x", drop = FALSE) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 3"][,level := factor(level)], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 3"][,level := factor(level)], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0, alpha = .75) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100)), alpha = .75) +
  scale_y_continuous(breaks = c(-10, 0, 10), limits = c(-20, 20), labels = scales::number_format(suffix = " pp")) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = NULL,
       y = "Change") +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

p_change_english_y4 <- ggplot(progress_lockdown_english[year == "Year 4"], aes(colour = chapter_simple)) +
  facet_grid(. ~ level, scales = "free_x", drop = FALSE) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 4"][,level := factor(level)], aes(ymin = -2*sd, ymax = 2*sd), xmin = 0, xmax = 1000, fill = "grey90", colour = NA) +
  geom_rect(data = prop_change_sd[course == "English" & level != "Other" & year == "Year 4"][,level := factor(level)], aes(ymin = -sd, ymax = sd), xmin = 0, xmax = 100, fill = "grey75", colour = NA) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_segment(aes(x = chapter_simple, xend = chapter_simple, y = (prop_change * 100)), yend = 0, alpha = .75) +
  geom_point(aes(x = chapter_simple, y = (prop_change * 100)), alpha = .75) +
  scale_y_continuous(breaks = c(-10, 0, 10), limits = c(-20, 20), labels = scales::number_format(suffix = " pp")) +
  scale_colour_viridis_d(direction = -1, na.translate = FALSE) +
  guides(colour = FALSE) +
  labs(x = NULL,
       y = "Change") +
  theme_paper +
  theme(panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

filler_plot <- qplot() + 
  theme_nothing() + 
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

plot_grid(
          filler_plot,
          p_english_y1, filler_plot, p_change_english_y1, filler_plot,
          p_english_y2, filler_plot, p_change_english_y2, filler_plot, 
          p_english_y3, filler_plot, p_change_english_y3, filler_plot,
          p_english_y4, filler_plot, p_change_english_y4,
          ncol = 1,
          align = "hv", axis = "tblr",
          labels = c(NA,
                     "Year 1", NA, NA, NA,
                     "Year 2", NA, NA, NA,
                     "Year 3", NA, NA, NA,
                     "Year 4", NA, NA),
          rel_heights = c(.1, 
                          1, -.2, .75, .1,
                          1, -.2, .75, .1,
                          1, -.2, .75, .1,
                          1, -.2, .75),
          hjust = -0.1,
          vjust = -0.1,
          scale = .95)
```

    ## Warning: Removed 61 rows containing missing values (position_stack).

    ## Warning: Removed 61 rows containing missing values (geom_col).

    ## Warning: Removed 69 rows containing missing values (position_stack).

    ## Warning: Removed 60 rows containing missing values (geom_col).

    ## Warning: Removed 49 rows containing missing values (position_stack).

    ## Warning: Removed 50 rows containing missing values (geom_col).

    ## Warning: Removed 7 rows containing missing values (position_stack).

    ## Warning: Removed 4 rows containing missing values (geom_col).

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

![](02_performance_files/figure-gfm/unnamed-chunk-115-1.png)<!-- -->

``` r
ggsave("../output/progress_combi_alt_english.pdf", width = 9, height = 11)
```

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

``` r
ggsave("../output/progress_combi_alt_english.eps", width = 9, height = 11)
```

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

    ## Warning in grid.Call.graphics(C_rect, x$x, x$y, x$width, x$height,
    ## resolveHJust(x$just, : semi-transparency is not supported on this device:
    ## reported only once per page

``` r
ggsave("../output/progress_combi_alt_english.png", width = 9, height = 11)
```

    ## Warning: Removed 1 rows containing missing values (geom_text).

    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).
    
    ## Warning: Removed 1 rows containing missing values (geom_text).

# Session info

``` r
sessionInfo()
```

    ## R version 3.6.3 (2020-02-29)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 18.04.5 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=nl_NL.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=nl_NL.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=nl_NL.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] grid      stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ## [1] lmerTest_3.1-0    lme4_1.1-21       Matrix_1.2-18     cowplot_0.9.4    
    ## [5] ggplot2_3.3.2     DBI_1.1.0         data.table_1.13.6
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_0.2.5    xfun_0.21           purrr_0.3.2        
    ##  [4] splines_3.6.3       lattice_0.20-41     colorspace_1.4-1   
    ##  [7] vctrs_0.2.2         htmltools_0.3.6     viridisLite_0.3.0  
    ## [10] yaml_2.2.0          blob_1.2.1          rlang_0.4.10       
    ## [13] pillar_1.4.2        nloptr_1.2.1        glue_1.3.1         
    ## [16] withr_2.3.0         bit64_0.9-7         plyr_1.8.4         
    ## [19] lifecycle_0.1.0     stringr_1.4.0       munsell_0.5.0      
    ## [22] gtable_0.3.0        evaluate_0.14       memoise_1.1.0      
    ## [25] labeling_0.3        knitr_1.23          Rcpp_1.0.6         
    ## [28] scales_1.0.0        bit_1.1-14          digest_0.6.19      
    ## [31] stringi_1.4.3       dplyr_0.8.3         numDeriv_2016.8-1.1
    ## [34] tools_3.6.3         magrittr_1.5        tibble_2.1.3       
    ## [37] RSQLite_2.2.0       crayon_1.3.4        tidyr_1.0.0        
    ## [40] pkgconfig_2.0.2     MASS_7.3-51.4       ellipsis_0.3.0     
    ## [43] ggridges_0.5.1      assertthat_0.2.1    minqa_1.2.4        
    ## [46] rmarkdown_2.6       R6_2.4.0            boot_1.3-25        
    ## [49] nlme_3.1-149        compiler_3.6.3
