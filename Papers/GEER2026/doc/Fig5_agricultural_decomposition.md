# Fig5 agricultural decomposition

このメモは、`Papers/GEER2026/prog/paperV2.R` の `fig5_agricultural_decomposition_2100.png` が何を示しているか、どのデータのどの変数を使い、どの手順で作っているかを、再現可能な形で整理したものです。

## 1. 図の目的

Fig5 は、農業部門の非 CO2 排出変化を 3 つのウェッジに分解して示しています。

- `Demand side`
- `Technical`
- `Structural`

左パネルは `World + R5`、右パネルは排出削減が大きい上位国です。  
対象年は `2100` 年のみです。

黒点は、ウェッジの和ではなく、`GHG_load` から直接計算した総排出変化です。  
したがって、棒は要因分解、黒点は元データ由来の total change を独立に示しています。

## 2. 入力データ

### 2.1 GDX ファイル

使用している主なデータは次です。

- `Papers/GEER2026/data/0316/all_scenario_World.gdx`
- `Papers/GEER2026/data/0316/baseyear.gdx`
- `Papers/GEER2026/data/0316/AGMIP_all_scenario.gdx`
- `Papers/GEER2026/define/country.set`
- `Papers/GEER2026/define/region.map`

### 2.2 使っている変数

`all_scenario_World.gdx` から使う変数:

- `QDEM_load`
  - 国内総需要
- `QSUP_load`
  - 生産量
- `RR_load`
  - 排出係数に対する削減率
- `GHG_load`
  - 排出量

`baseyear.gdx` から使う変数:

- `EFbase`
  - 基準年排出係数

`AGMIP_all_scenario.gdx` から使う変数:

- `EMIS`
  - 農業排出量
  - これは右パネルの対象国選定に使っています

## 3. シナリオ

比較する 2 つのシナリオは次です。

- `SSP2_BaU_NoCC_No`
  - ここでは baseline
- `SSP2_1100C_CACN_DAC_NoCC_LandInt_global1`
  - ここでは `MITI`

コード中の変数名では、

- `scenario_baseline <- "SSP2_BaU_NoCC_No"`
- `scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"`

です。

## 4. 対象集合

### 4.1 国

国集合は `define/country.set` です。  
`all_scenario_World.gdx` 側の `cty` がこの集合に含まれるものだけを使っています。

### 4.2 ガス

Fig5 の分解は、コード上は `CH4` と `N2O` を使っています。  
最終的には国別・年別に合計して `MtCO2eq/yr` として描いています。

### 4.3 財

`commodity != "ecr"` の制約を入れています。  
つまり `ecr` は除外しています。

## 5. 読み込み後の基本テーブル

`paperV2.R` ではまず次のテーブルを作っています。

### 5.1 `world_qsup`

`QSUP_load` から作る。

列:

- `scenario`
- `commodity`
- `region`
- `year`
- `qsup`

### 5.2 `world_qdem`

`QDEM_load` から作る。

列:

- `scenario`
- `commodity`
- `region`
- `year`
- `qdem`

### 5.3 `world_rr`

`RR_load` から作る。

列:

- `scenario`
- `commodity`
- `source`
- `gas`
- `region`
- `year`
- `rr`

### 5.4 `world_ghg`

`GHG_load` から作る。

列:

- `scenario`
- `commodity`
- `source`
- `gas`
- `region`
- `year`
- `ghg`

### 5.5 `world_bau_ef`

`EFbase` から作る。

列:

- `commodity`
- `source`
- `gas`
- `region`
- `ef_base`

## 6. 排出係数の作り方

Fig5 の分解では、各シナリオの排出係数 `EF` を直接 GDX から読まず、次で作っています。

\[
EF_{r,t,c,s,g}^{MITI} = EFbase_{r,c,s,g} \times (1 - RR_{r,t,c,s,g})
\]

baseline 側は

\[
EF_{r,t,c,s,g}^{BaU} = EFbase_{r,c,s,g}
\]

と扱っています。

実装では `world_decomp_base` で

- `QSUP_load`
- `EFbase`
- `RR_load`

を join し、

- `rr = replace_na(rr, 0)`
- `ef = ef_base * (1 - rr)`

としています。

ここで `RR` が欠ける場合は 0 扱い、つまりその行では技術削減なしです。

## 7. 最終 3 ウェッジの式

Fig5 に使っているのは、最終的に

- `Demand side`
- `Technical`
- `Structural`

の 3 区分です。

### 7.1 まず作る比率

コードでは次を作っています。

\[
qdem\_ratio = \frac{QDEM^{MITI}}{QDEM^{BaU}}
\]

\[
qsup\_ratio = \frac{QSUP^{MITI}}{QSUP^{BaU}}
\]

\[
ef\_ratio = \frac{EF^{MITI}}{EF^{BaU}}
\]

さらに、技術要因として

\[
rr\_{tech} = RR^{MITI}
\]

を使っています。

### 7.2 総排出変化

黒点の元になる total change は、

\[
\Delta GHG = GHG^{MITI} - GHG^{BaU}
\]

です。

コードでは

- `total_change_mt = (GHG_load(MITI) - GHG_load(BaU)) / 1000`

としていて、`ktCO2eq/yr` を `MtCO2eq/yr` に直しています。

### 7.3 Demand side

コードでは

\[
Demand = E_0 \times \frac{(qdem\_ratio - 1) + (qdem\_ratio - 1)\times ef\_ratio}{2}
\]

を使っています。

ここで

- \(E_0 = GHG^{BaU}\)

です。

実装:

```r
demand_mt = bau_ghg_kt * (((qdem_ratio - 1) + ((qdem_ratio - 1) * ef_ratio)) / 2) / 1000
```

### 7.4 Supply side

中間計算として

\[
Supply = E_0 \times \frac{(ef\_ratio - 1) + (ef\_ratio - 1)\times qsup\_ratio}{2}
\]

を作っています。

実装:

```r
supply_mt = bau_ghg_kt * (((ef_ratio - 1) + ((ef_ratio - 1) * qsup_ratio)) / 2) / 1000
```

### 7.5 Trade

中間計算として

\[
Trade = E_0 \times \frac{(qsup\_ratio - qdem\_ratio) + (qsup\_ratio - qdem\_ratio)\times ef\_ratio}{2}
\]

を作っています。

実装:

```r
trade_mt = bau_ghg_kt * ((((qsup_ratio - qdem_ratio)) + ((qsup_ratio - qdem_ratio) * ef_ratio)) / 2) / 1000
```

### 7.6 Technical

`RR` を技術寄与とみなして、

\[
Technical = E_0 \times \frac{(-rr\_{tech}) + (-rr\_{tech})\times qsup\_ratio}{2}
\]

としています。

実装:

```r
technical_mt = bau_ghg_kt * (((-rr_tech) + ((-rr_tech) * qsup_ratio)) / 2) / 1000
```

### 7.7 Structural

最終的な `Structural` は、

\[
Structural = (Supply - Technical) + Trade
\]

です。

実装:

```r
structural_mt = (supply_mt - technical_mt) + trade_mt
```

## 8. 集計手順

### 8.1 国別合計

まず commodity × source × gas 単位で `Demand`, `Technical`, `Structural`, `total_change` を計算し、その後に国別・年別に合計しています。

コード上は `world_final_wedge_country_co2eq` です。

列:

- `region`
- `year`
- `demand_mt`
- `technical_mt`
- `structural_mt`
- `total_change_mt`
- `country`

ここで単位はすべて `MtCO2eq/yr` です。

### 8.2 R5 + World 集計

次に `define/region.map` を使って国を R5 に対応づけ、国別結果を R5 に足し上げています。  
さらに全世界合計も別途足しています。

コード上は `world_final_wedge_r5_world_co2eq` です。

列:

- `region_name`
  - `World`
  - `OECD+EU`
  - `Asia`
  - `Africa/Middle East`
  - `Latin America`
  - `Former Soviet Union`
- `year`
- `demand_mt`
- `technical_mt`
- `structural_mt`
- `total_change_mt`

## 9. Fig5 の左パネル

左パネルは `make_world_final_wedge_r5_total_plot(focus_year)` で作っています。

使う入力:

- `world_final_wedge_r5_world_co2eq`
- 対象年 `2100`

表示:

- stacked bars
  - `Demand side`
  - `Technical`
  - `Structural`
- black point
  - `total_change_mt`

つまり、`World` と `R5` について、2100 年の農業排出変化を 3 要因に分解して示しています。

## 10. Fig5 の右パネル

右パネルは `make_world_final_wedge_total_plot(focus_year)` で作っています。

### 10.1 対象国の選び方

右パネルは AFOLU top 国ではなく、**農業部門の実削減量が大きい国**を上位 15 か国選んでいます。

その基準は `agmip_ag_reduction_rank` です。

このテーブルは `AGMIP_all_scenario.gdx` の `EMIS` から

\[
Agricultural\ actual\ reduction = EMIS^{BaU} - EMIS^{MITI}
\]

を source 合計で求め、2100 年の大きい順に並べたものです。

コード上:

```r
top_regions <- agmip_ag_reduction_rank %>%
  filter(year == plot_year) %>%
  arrange(desc(ag_reduction)) %>%
  slice_head(n = 15) %>%
  pull(region)
```

### 10.2 表示

表示内容は左パネルと同じで、

- stacked bars
  - `Demand side`
  - `Technical`
  - `Structural`
- black point
  - `total_change_mt`

です。

## 11. 黒点の意味

黒点はウェッジの和ではありません。  
`GHG_load` から直接取った総排出変化です。

つまり、

\[
Black\ point = \sum_{c,s,g}\left(GHG^{MITI}_{c,s,g} - GHG^{BaU}_{c,s,g}\right)
\]

です。

この実装にしている理由は、分解棒と元データ total の整合を確認できるようにするためです。

## 12. 図が示していること

Fig5 は、農業部門排出変化を

- 需要変化
- 技術導入
- 構造変化

の 3 要因に分けて示しています。

ここでの意味づけは次です。

- `Demand side`
  - 国内需要変化に伴う削減寄与
- `Technical`
  - `RR_load` で表される技術的排出係数改善
- `Structural`
  - 技術寄与を除いた供給側変化
  - そこに貿易調整を加えたもの

したがって `Structural` は、

- 生産システムの変化
- 生産地配置の変化
- 貿易調整

をまとめた残差的な構造要因です。

## 13. 再現に必要な最小条件

この図を再現するには、少なくとも次が必要です。

- `all_scenario_World.gdx`
  - `QDEM_load`
  - `QSUP_load`
  - `RR_load`
  - `GHG_load`
- `baseyear.gdx`
  - `EFbase`
- `AGMIP_all_scenario.gdx`
  - 右パネルの対象国選定用 `EMIS`
- `define/country.set`
- `define/region.map`

## 14. コード上の対応箇所

`paperV2.R` の該当部分は主に次です。

- データ読み込み
  - `GEER_WORLD_EFBASE`
  - `GEER_WORLD_QDEM`
  - `GEER_WORLD_QSUP`
  - `GEER_WORLD_RR`
  - `GEER_WORLD_GHG`
- 中間テーブル作成
  - `world_qsup`
  - `world_qdem`
  - `world_rr`
  - `world_ghg`
  - `world_bau_ef`
  - `world_final_wedge_country_co2eq`
  - `world_final_wedge_r5_world_co2eq`
- 作図
  - `make_world_final_wedge_r5_total_plot()`
  - `make_world_final_wedge_total_plot()`
  - `fig5`

## 15. 注意点

この図は land-use 部門ではなく、**農業部門の非 CO2 排出変化**の要因分解です。  
したがって、`Fig1` や `Fig2` のような AFOLU 全体の土地利用排出・土地利用削減はここには入りません。

また、右パネルの国選定は AFOLU 総削減量ではなく、**農業削減量**ベースです。この点は `Fig2` の Top 国とは必ずしも一致しません。

---

## 論文用の文章案

Figure 5 shows an ex-post decomposition of agricultural non-CO2 emission changes under the mitigation scenario in 2100. The decomposition is based on `QDEM_load`, `QSUP_load`, `RR_load`, and `GHG_load` from `all_scenario_World.gdx`, together with the baseline emission factor `EFbase` from `baseyear.gdx`. The baseline scenario is `SSP2_BaU_NoCC_No`, and the mitigation scenario is `SSP2_1100C_CACN_DAC_NoCC_LandInt_global1` (MITI). The analysis is conducted for `CH4` and `N2O`, and the results are aggregated to `MtCO2eq/yr`.

The starting point of the decomposition is the baseline emission factor, `EFbase`, which is read from `baseyear.gdx` for each country, commodity, source, and gas. Under the mitigation scenario, the emission factor is reconstructed as `EF = EFbase × (1 - RR)`, where `RR` is the mitigation rate from `RR_load`. If `RR` is missing, it is treated as zero, implying no technical mitigation for that observation. Domestic demand and production are represented by `QDEM_load` and `QSUP_load`, respectively, and total emissions are taken from `GHG_load`.

The final decomposition separates total emission changes into three wedges: demand-side, technical, and structural components. First, the ratios of mitigation to baseline demand, production, and emission factors are computed as `qdem_ratio`, `qsup_ratio`, and `ef_ratio`. Demand-side mitigation is then calculated as the baseline emissions multiplied by the midpoint average of the proportional demand change and its interaction with the emission factor ratio. Supply-side mitigation is similarly obtained from the proportional change in emission factors and its interaction with the production ratio. A trade term is calculated from the difference between the production and demand ratios, again using a midpoint formulation. Technical mitigation is identified from `RR` and represents the direct emission-factor reduction associated with the mitigation rate. Finally, the structural wedge is defined as the residual supply-side component after subtracting technical mitigation and then adding the trade component. In equations, the decomposition is:

`Demand = E0 × {[(QDEM_MITI / QDEM_BaU) - 1] + [(QDEM_MITI / QDEM_BaU) - 1] × (EF_MITI / EF_BaU)} / 2`

`Supply = E0 × {[(EF_MITI / EF_BaU) - 1] + [(EF_MITI / EF_BaU) - 1] × (QSUP_MITI / QSUP_BaU)} / 2`

`Trade = E0 × {[(QSUP_MITI / QSUP_BaU) - (QDEM_MITI / QDEM_BaU)] + [(QSUP_MITI / QSUP_BaU) - (QDEM_MITI / QDEM_BaU)] × (EF_MITI / EF_BaU)} / 2`

`Technical = E0 × {[-RR_MITI] + [-RR_MITI] × (QSUP_MITI / QSUP_BaU)} / 2`

`Structural = (Supply - Technical) + Trade`

where `E0` denotes baseline emissions from `GHG_load`.

After computing the decomposition at the commodity × source × gas level, the results are aggregated by country and year. The black dots shown in the figure are not the sum of the bars, but the independently calculated total emission change directly derived from `GHG_load`, that is, `GHG_MITI - GHG_BaU`. This design allows the agreement between the decomposition and the original emissions output to be checked visually.

The left panel of Figure 5 presents the decomposition for the world and the R5 regions. These regional results are obtained by aggregating the country-level decomposition using the mapping defined in `define/region.map`. The right panel shows the decomposition for the top 15 countries ranked by agricultural emission reduction in 2100. These countries are selected using agricultural actual reductions derived from `EMIS` in `AGMIP_all_scenario.gdx`, calculated as `EMIS_BaU - EMIS_MITI` and summed across agricultural sources. Accordingly, the right panel is based on countries with the largest agricultural mitigation, not necessarily the same countries identified as top AFOLU reducers in Figure 2.

This figure therefore provides an interpretation of how mitigation is achieved in the agricultural sector, distinguishing the roles of demand contraction, direct technical abatement captured by `RR`, and broader structural adjustments including production reallocation and trade effects.


もちろんです。以下に、**Frank et al. (2018) の書き方を参考にしつつ、あなたの Fig. 5 の実装内容に対応した「手法」記述案**を、日本語の論文調でそのまま使いやすい形で示します。

---

## 手法記述案（本文・Methods 向け）

本研究では、農業部門の非 CO2 排出変化を、Frank et al. (2018) の分解枠組みを参考に、**需要側要因（Demand side）**、**技術要因（Technical）**、**構造要因（Structural）**の 3 つに分解した。ここでは、基準シナリオを (BaU)、対策シナリオを (MITI) とし、対象年 (t)、地域 (r)、財 (c)、排出源 (s)、ガス種 (g) ごとに分解を行った。対象ガスは CH(_4) および N(_2)O とし、最終的に CO(_2) 換算で集計した。

各シナリオにおける国内需要を (QDEM)、生産量を (QSUP)、排出量を (GHG) とする。また、基準年排出係数を (EF^{base})、排出係数削減率を (RR) とした。本研究では、対策シナリオの排出係数を次式で定義した。

[
EF^{MITI}*{r,t,c,s,g} = EF^{base}*{r,c,s,g},(1 - RR_{r,t,c,s,g})
]

一方、基準シナリオでは

[
EF^{BaU}*{r,t,c,s,g} = EF^{base}*{r,c,s,g}
]

とした。すなわち、排出係数の変化は排出削減率 (RR) を通じて表現した。

次に、基準シナリオに対する対策シナリオの相対変化として、需要比率、生産比率、排出係数比率をそれぞれ

[
qdem_ratio = \frac{QDEM^{MITI}}{QDEM^{BaU}},
\qquad
qsup_ratio = \frac{QSUP^{MITI}}{QSUP^{BaU}},
\qquad
ef_ratio = \frac{EF^{MITI}}{EF^{BaU}}
]

と定義した。さらに、基準シナリオの排出量を

[
E_0 = GHG^{BaU}
]

とおいた。

総排出変化は、

[
\Delta GHG = GHG^{MITI} - GHG^{BaU}
]

として定義した。これは分解結果とは独立に、元の排出データから直接計算した。

要因分解は Frank et al. (2018) に準じ、まず需要側、供給側、貿易要因に対応する中間項を構成した。需要側寄与は

[
Demand
======

E_0
\times
\frac{
(qdem_ratio - 1)
+
(qdem_ratio - 1),ef_ratio
}{2}
]

とした。供給側寄与は

[
Supply
======

E_0
\times
\frac{
(ef_ratio - 1)
+
(ef_ratio - 1),qsup_ratio
}{2}
]

とし、さらに生産量と需要量の差に由来する貿易・地域間調整の寄与を

[
Trade
=====

E_0
\times
\frac{
(qsup_ratio - qdem_ratio)
+
(qsup_ratio - qdem_ratio),ef_ratio
}{2}
]

と定義した。

本研究では、供給側要因のうち排出係数削減率 (RR) に直接対応する部分を**技術要因（Technical）**として明示的に切り出した。すなわち、

[
Technical
=========

E_0
\times
\frac{
(-RR^{MITI})
+
(-RR^{MITI}),qsup_ratio
}{2}
]

とした。これは、排出係数改善による直接的な排出削減効果を表す。

最後に、**構造要因（Structural）**は、供給側寄与のうち技術要因では説明されない残差部分に、貿易・地域間調整の影響を加えたものとして、

[
Structural = (Supply - Technical) + Trade
]

と定義した。したがって、本研究における最終的な分解は、需要変化に起因する **Demand side**、排出係数改善に起因する **Technical**、ならびに生産構成・地域分布・需給差調整を含む **Structural** の 3 区分からなる。

分解は財、排出源、ガス種の組合せごとに行い、その後、国別および地域別に集計した。地域集計では各国を R5 地域に対応付け、さらに世界計も別途算出した。図では、積み上げ棒グラフとして 3 要因の寄与を示し、黒点として元データから直接計算した総排出変化 (\Delta GHG) を併記した。

---

## もう少し論文らしく整えた短め版

本研究では、農業部門の非 CO(_2) 排出変化を、Frank et al. (2018) の分解枠組みを参考に、需要側要因、技術要因、構造要因の 3 つに分解した。基準シナリオと対策シナリオにおける国内需要、生産量、排出係数、排出量を用い、基準シナリオの排出量を基準値として、需要変化、排出係数変化、需給差に基づく地域間調整の寄与を算出した。対策シナリオの排出係数は、基準年排出係数に排出削減率を乗じることで推計した。さらに、排出削減率に直接対応する部分を技術要因として明示的に抽出し、供給側寄与の残差と貿易・地域間調整の影響を合わせて構造要因と定義した。最終的に、各財・排出源・ガス種について求めた寄与を国別および地域別に集計し、総排出変化と併せて図示した。

---

## 使い分けの目安

* **長め版**：Methods 節にそのまま入れやすい
* **短め版**：本文や図注の前段説明向け
* 表現としては
  **「Frank et al. (2018) の分解枠組みを参考にしつつ、本研究のモデル出力に合わせて適用した」**
  としておくのが最も安全です

必要なら次に、これを
**「論文の図注用に 3〜4 文へ圧縮した版」**
または
**「英語版」**
に整えます。
