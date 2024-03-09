EDA
================
Yida Wang
2024-03-08

``` r
train=read.csv("train.csv")
test=read.csv("test.csv")
```

``` r
head(train)
```

    ##   PassengerId HomePlanet CryoSleep Cabin   Destination Age   VIP RoomService
    ## 1     0001_01     Europa     False B/0/P   TRAPPIST-1e  39 False           0
    ## 2     0002_01      Earth     False F/0/S   TRAPPIST-1e  24 False         109
    ## 3     0003_01     Europa     False A/0/S   TRAPPIST-1e  58  True          43
    ## 4     0003_02     Europa     False A/0/S   TRAPPIST-1e  33 False           0
    ## 5     0004_01      Earth     False F/1/S   TRAPPIST-1e  16 False         303
    ## 6     0005_01      Earth     False F/0/P PSO J318.5-22  44 False           0
    ##   FoodCourt ShoppingMall  Spa VRDeck              Name Transported
    ## 1         0            0    0      0   Maham Ofracculy       False
    ## 2         9           25  549     44      Juanna Vines        True
    ## 3      3576            0 6715     49     Altark Susent       False
    ## 4      1283          371 3329    193      Solam Susent       False
    ## 5        70          151  565      2 Willy Santantines        True
    ## 6       483            0  291      0 Sandie Hinetthews        True

``` r
head(test)
```

    ##   PassengerId HomePlanet CryoSleep Cabin Destination Age   VIP RoomService
    ## 1     0013_01      Earth      True G/3/S TRAPPIST-1e  27 False           0
    ## 2     0018_01      Earth     False F/4/S TRAPPIST-1e  19 False           0
    ## 3     0019_01     Europa      True C/0/S 55 Cancri e  31 False           0
    ## 4     0021_01     Europa     False C/1/S TRAPPIST-1e  38 False           0
    ## 5     0023_01      Earth     False F/5/S TRAPPIST-1e  20 False          10
    ## 6     0027_01      Earth     False F/7/P TRAPPIST-1e  31 False           0
    ##   FoodCourt ShoppingMall  Spa VRDeck             Name
    ## 1         0            0    0      0  Nelly Carsoning
    ## 2         9            0 2823      0   Lerome Peckers
    ## 3         0            0    0      0  Sabih Unhearfus
    ## 4      6652            0  181    585 Meratz Caltilter
    ## 5         0          635    0      0  Brence Harperez
    ## 6      1615          263  113     60     Karlen Ricks

``` r
glimpse(train)
```

    ## Rows: 8,693
    ## Columns: 14
    ## $ PassengerId  <chr> "0001_01", "0002_01", "0003_01", "0003_02", "0004_01", "0…
    ## $ HomePlanet   <chr> "Europa", "Earth", "Europa", "Europa", "Earth", "Earth", …
    ## $ CryoSleep    <chr> "False", "False", "False", "False", "False", "False", "Fa…
    ## $ Cabin        <chr> "B/0/P", "F/0/S", "A/0/S", "A/0/S", "F/1/S", "F/0/P", "F/…
    ## $ Destination  <chr> "TRAPPIST-1e", "TRAPPIST-1e", "TRAPPIST-1e", "TRAPPIST-1e…
    ## $ Age          <dbl> 39, 24, 58, 33, 16, 44, 26, 28, 35, 14, 34, 45, 32, 48, 2…
    ## $ VIP          <chr> "False", "False", "True", "False", "False", "False", "Fal…
    ## $ RoomService  <dbl> 0, 109, 43, 0, 303, 0, 42, 0, 0, 0, 0, 39, 73, 719, 8, 32…
    ## $ FoodCourt    <dbl> 0, 9, 3576, 1283, 70, 483, 1539, 0, 785, 0, 0, 7295, 0, 1…
    ## $ ShoppingMall <dbl> 0, 25, 0, 371, 151, 0, 3, 0, 17, 0, NA, 589, 1123, 65, 12…
    ## $ Spa          <dbl> 0, 549, 6715, 3329, 565, 291, 0, 0, 216, 0, 0, 110, 0, 0,…
    ## $ VRDeck       <dbl> 0, 44, 49, 193, 2, 0, 0, NA, 0, 0, 0, 124, 113, 24, 7, 0,…
    ## $ Name         <chr> "Maham Ofracculy", "Juanna Vines", "Altark Susent", "Sola…
    ## $ Transported  <chr> "False", "True", "False", "False", "True", "True", "True"…

\#baseline summary table Not included: PassengerId,Cabin,Name

``` r
table1 = tbl_summary(train, include = c(HomePlanet,CryoSleep,Destination,Age,VIP,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck,Transported),statistic = list(all_continuous() ~ "{mean} ({sd}),{median} ({p25}, {p75}),{min}-{max}"))  %>%
  as_gt()
table1
```

<div id="ztltisajow" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ztltisajow table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ztltisajow thead, #ztltisajow tbody, #ztltisajow tfoot, #ztltisajow tr, #ztltisajow td, #ztltisajow th {
  border-style: none;
}
&#10;#ztltisajow p {
  margin: 0;
  padding: 0;
}
&#10;#ztltisajow .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#ztltisajow .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ztltisajow .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#ztltisajow .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#ztltisajow .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ztltisajow .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztltisajow .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#ztltisajow .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#ztltisajow .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#ztltisajow .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ztltisajow .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ztltisajow .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#ztltisajow .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ztltisajow .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#ztltisajow .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#ztltisajow .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ztltisajow .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ztltisajow .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#ztltisajow .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztltisajow .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#ztltisajow .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ztltisajow .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ztltisajow .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztltisajow .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ztltisajow .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ztltisajow .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztltisajow .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztltisajow .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ztltisajow .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztltisajow .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ztltisajow .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ztltisajow .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ztltisajow .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztltisajow .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#ztltisajow .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ztltisajow .gt_left {
  text-align: left;
}
&#10;#ztltisajow .gt_center {
  text-align: center;
}
&#10;#ztltisajow .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ztltisajow .gt_font_normal {
  font-weight: normal;
}
&#10;#ztltisajow .gt_font_bold {
  font-weight: bold;
}
&#10;#ztltisajow .gt_font_italic {
  font-style: italic;
}
&#10;#ztltisajow .gt_super {
  font-size: 65%;
}
&#10;#ztltisajow .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ztltisajow .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ztltisajow .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ztltisajow .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ztltisajow .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ztltisajow .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ztltisajow .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 8,693&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>N = 8,693</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">HomePlanet</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">201 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Earth</td>
<td headers="stat_0" class="gt_row gt_center">4,602 (53%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Europa</td>
<td headers="stat_0" class="gt_row gt_center">2,131 (25%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mars</td>
<td headers="stat_0" class="gt_row gt_center">1,759 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">CryoSleep</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">217 (2.5%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    False</td>
<td headers="stat_0" class="gt_row gt_center">5,439 (63%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    True</td>
<td headers="stat_0" class="gt_row gt_center">3,037 (35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Destination</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">182 (2.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    55 Cancri e</td>
<td headers="stat_0" class="gt_row gt_center">1,800 (21%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    PSO J318.5-22</td>
<td headers="stat_0" class="gt_row gt_center">796 (9.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRAPPIST-1e</td>
<td headers="stat_0" class="gt_row gt_center">5,915 (68%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_0" class="gt_row gt_center">29 (14),27 (19, 38),0-79</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">179</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VIP</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">203 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    False</td>
<td headers="stat_0" class="gt_row gt_center">8,291 (95%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    True</td>
<td headers="stat_0" class="gt_row gt_center">199 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RoomService</td>
<td headers="stat_0" class="gt_row gt_center">225 (667),0 (0, 47),0-14,327</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">181</td></tr>
    <tr><td headers="label" class="gt_row gt_left">FoodCourt</td>
<td headers="stat_0" class="gt_row gt_center">458 (1,611),0 (0, 76),0-29,813</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">183</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ShoppingMall</td>
<td headers="stat_0" class="gt_row gt_center">174 (605),0 (0, 27),0-23,492</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">208</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Spa</td>
<td headers="stat_0" class="gt_row gt_center">311 (1,137),0 (0, 59),0-22,408</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">183</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VRDeck</td>
<td headers="stat_0" class="gt_row gt_center">305 (1,146),0 (0, 46),0-24,133</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">188</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Transported</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    False</td>
<td headers="stat_0" class="gt_row gt_center">4,315 (50%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    True</td>
<td headers="stat_0" class="gt_row gt_center">4,378 (50%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Mean (SD),Median (IQR),Minimum-Maximum</td>
    </tr>
  </tfoot>
</table>
</div>

\#NA checking

``` r
sum(is.na(train))
```

    ## [1] 1122

``` r
sum(is.na(test))
```

    ## [1] 558

\#Merged all

``` r
train_raw = train %>% 
  select(-Transported)
merged=rbind(train_raw %>% mutate(type = "Train"), test %>% mutate(type = "Test"))
```

\#table for all

``` r
table2 = tbl_summary(merged, include = c(HomePlanet,CryoSleep,Destination,Age,VIP,RoomService,FoodCourt,ShoppingMall,Spa,VRDeck,type),statistic = list(all_continuous() ~ "{mean} ({sd}),{median} ({p25}, {p75}),{min}-{max}"))  %>%
  as_gt()
table2
```

<div id="uvnyslnezu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#uvnyslnezu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#uvnyslnezu thead, #uvnyslnezu tbody, #uvnyslnezu tfoot, #uvnyslnezu tr, #uvnyslnezu td, #uvnyslnezu th {
  border-style: none;
}
&#10;#uvnyslnezu p {
  margin: 0;
  padding: 0;
}
&#10;#uvnyslnezu .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#uvnyslnezu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#uvnyslnezu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#uvnyslnezu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#uvnyslnezu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#uvnyslnezu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#uvnyslnezu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#uvnyslnezu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#uvnyslnezu .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#uvnyslnezu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#uvnyslnezu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#uvnyslnezu .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#uvnyslnezu .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#uvnyslnezu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#uvnyslnezu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uvnyslnezu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#uvnyslnezu .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#uvnyslnezu .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#uvnyslnezu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uvnyslnezu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#uvnyslnezu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uvnyslnezu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#uvnyslnezu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uvnyslnezu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#uvnyslnezu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#uvnyslnezu .gt_left {
  text-align: left;
}
&#10;#uvnyslnezu .gt_center {
  text-align: center;
}
&#10;#uvnyslnezu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#uvnyslnezu .gt_font_normal {
  font-weight: normal;
}
&#10;#uvnyslnezu .gt_font_bold {
  font-weight: bold;
}
&#10;#uvnyslnezu .gt_font_italic {
  font-style: italic;
}
&#10;#uvnyslnezu .gt_super {
  font-size: 65%;
}
&#10;#uvnyslnezu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#uvnyslnezu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#uvnyslnezu .gt_indent_1 {
  text-indent: 5px;
}
&#10;#uvnyslnezu .gt_indent_2 {
  text-indent: 10px;
}
&#10;#uvnyslnezu .gt_indent_3 {
  text-indent: 15px;
}
&#10;#uvnyslnezu .gt_indent_4 {
  text-indent: 20px;
}
&#10;#uvnyslnezu .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;N = 12,970&lt;/strong&gt;&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>N = 12,970</strong><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">HomePlanet</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">288 (2.2%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Earth</td>
<td headers="stat_0" class="gt_row gt_center">6,865 (53%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Europa</td>
<td headers="stat_0" class="gt_row gt_center">3,133 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Mars</td>
<td headers="stat_0" class="gt_row gt_center">2,684 (21%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">CryoSleep</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">310 (2.4%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    False</td>
<td headers="stat_0" class="gt_row gt_center">8,079 (62%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    True</td>
<td headers="stat_0" class="gt_row gt_center">4,581 (35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Destination</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">274 (2.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    55 Cancri e</td>
<td headers="stat_0" class="gt_row gt_center">2,641 (20%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    PSO J318.5-22</td>
<td headers="stat_0" class="gt_row gt_center">1,184 (9.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    TRAPPIST-1e</td>
<td headers="stat_0" class="gt_row gt_center">8,871 (68%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_0" class="gt_row gt_center">29 (14),27 (19, 38),0-79</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">270</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VIP</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    </td>
<td headers="stat_0" class="gt_row gt_center">296 (2.3%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    False</td>
<td headers="stat_0" class="gt_row gt_center">12,401 (96%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    True</td>
<td headers="stat_0" class="gt_row gt_center">273 (2.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">RoomService</td>
<td headers="stat_0" class="gt_row gt_center">223 (648),0 (0, 49),0-14,327</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">263</td></tr>
    <tr><td headers="label" class="gt_row gt_left">FoodCourt</td>
<td headers="stat_0" class="gt_row gt_center">452 (1,584),0 (0, 77),0-29,813</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">289</td></tr>
    <tr><td headers="label" class="gt_row gt_left">ShoppingMall</td>
<td headers="stat_0" class="gt_row gt_center">175 (591),0 (0, 29),0-23,492</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">306</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Spa</td>
<td headers="stat_0" class="gt_row gt_center">308 (1,130),0 (0, 57),0-22,408</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">284</td></tr>
    <tr><td headers="label" class="gt_row gt_left">VRDeck</td>
<td headers="stat_0" class="gt_row gt_center">307 (1,180),0 (0, 42),0-24,133</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Unknown</td>
<td headers="stat_0" class="gt_row gt_center">268</td></tr>
    <tr><td headers="label" class="gt_row gt_left">type</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Test</td>
<td headers="stat_0" class="gt_row gt_center">4,277 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Train</td>
<td headers="stat_0" class="gt_row gt_center">8,693 (67%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="2"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%); Mean (SD),Median (IQR),Minimum-Maximum</td>
    </tr>
  </tfoot>
</table>
</div>

# Visualization

``` r
ggplot(merged, aes(x = Age,fill=type)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")
```

![](EDA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(merged, aes(x = RoomService,fill=type)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Distribution of RoomService", x = "RoomService", y = "Frequency")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](EDA_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(merged, aes(x = FoodCourt,fill=type)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Distribution of FoodCourt", x = "FoodCourt", y = "Frequency")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](EDA_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(merged, aes(x = ShoppingMall,fill=type)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Distribution of ShoppingMall", x = "ShoppingMall", y = "Frequency")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](EDA_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot(merged, aes(x = Spa,fill=type)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Distribution of Spa", x = "Spa", y = "Frequency")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](EDA_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot(merged, aes(x = VRDeck,fill=type)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Distribution of VRDeck", x = "VRDeck", y = "Frequency")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](EDA_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
