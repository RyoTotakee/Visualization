# 生成グラフと計算方法の説明

## 1. 目的

保護区拡大、植林、BECCSを同時に導入したときの土地利用、農業市場、土地利用変化由来排出、生物多様性への影響を広く確認するための図である。図を絞り込む前の探索に使えるよう、国、R10地域、世界、流域、0.5度グリッドという複数の空間単位と、AgMIPの作物・畜産物区分を残している。

PNGとともに寄与分解後のCSVを保存する。CSVには図の表示範囲を調整する前の値があり、後から対象国、財、指標を選び直せる。

## 2. 実行方法

`Labo/260918Interim`を作業ディレクトリとして全図を再生成する。

```bash
bash prog/run.sh
```

各Rスクリプトは単独実行できる。対象年は環境変数`TARGET_YEAR`で指定し、未指定時は2100年になる。指定可能な年は2050年と2100年である。

```bash
TARGET_YEAR=2050 Rscript prog/03_country_indicators.R
TARGET_YEAR=2050 bash prog/run.sh
```

GAMSがPATHにない場合は`GAMS_SYS_DIR`を指定する。使用パッケージは`gdxrrw`、`dplyr`、`tidyr`、`ggplot2`、`patchwork`、`readr`、`sf`、`terra`、`rnaturalearth`、`scales`である。共通の年、シナリオ、財分類、Shapley分解、作図テーマは`prog/common.R`に定義している。

## 3. 出力フォルダ

```text
output/
├── 01_basin_yield/
│   ├── aggregate_food_crops/         非エネルギー作物13品目の流域集約
│   └── by_commodity/                 ALPHA・PLUMの作物別流域収量
├── 02_grid_land_use/                 グリッド土地利用と世界面積集計
├── 03_country_indicators/
│   ├── country/
│   │   ├── aggregate_agmip_items/    AGR・CRP・LSPの国別集約
│   │   └── by_agmip_item/            AgMIPの主要作物・畜産物
│   ├── region/r10/
│   │   ├── aggregate_agmip_items/    R10のAGR・CRP・LSP
│   │   └── by_agmip_item/            R10の品目別棒・ヒートマップ
│   └── world/
│       ├── aggregate_agmip_items/    世界のAGR・CRP・LSP
│       └── by_agmip_item/            世界の品目別比較
├── 04_land_emissions/                土地利用変化排出と土地配分
├── 05_biodiversity/                  BII
├── 06_protection_focus/
    ├── 01_land_competition/          保護区による土地配置変化
    ├── 02_yield_displacement/        収量水準別の作付移動
    ├── 03_agriculture/               条件別の農業市場影響
    ├── 04_climate_biodiversity/      排出・BII・保護面積
    └── 05_cross_domain_summary/      世界影響の横断確認
├── 07_conditional_protection_decomposition/
    ├── 01_agriculture/
    │   ├── country/                  国別の4成分棒・地図
    │   ├── region_r10/               R10の4成分棒
    │   └── world/                    世界の4成分棒
    ├── 02_grid_land/                 土地クラス別の4成分地図・世界面積
│   └── 03_observed_land_channels/    実際の植林・2Gバイオ面積変化との対応
├── 08_land_use_protection/
    ├── 01_land_inventory/            重複のない土地区分とTotal閉合確認
    ├── 02_protection_displacement/   保護区導入後の用途別変化・再配分
    ├── 03_protected_footprint/       保護面積と保護地内の土地被覆
│   └── 04_grid_relocation/world/     グリッド上の増加・減少・再配置診断
├── 09_land_dynamics_story/
    ├── 01_policy_response_matrix/    対策別純変化と政策導入経路
    ├── 02_protection_pushout/        減少セル・増加セル・補填率
    ├── 03_cdr_food_redistribution/   CDR・保護区による食料農地再配置
    └── 04_overlap_evidence/          直接重複と面積応答の区別
├── 10_country_opportunity_profiles/
    ├── 01_main_effect_and_interaction/       保護区主効果とCDR相互作用
    ├── 02_environment_agriculture_tradeoffs/ BII便益と農業影響
    ├── 03_land_market_link/                  土地再配置と市場応答
    └── 04_country_characteristics/           国の特徴との記述的関連
├── 11_results_evidence_chain/
    ├── 01_global_land_system/                 世界土地配分とCDR別シナリオ
    ├── 02_productive_land_displacement/       面積・場所・収量の生産分解
    ├── 03_policy_interaction/                 加算性とAff/BECCS相互作用
    ├── 04_country_transmission/               土地圧力から貿易・食料への波及
    └── 05_climate_biodiversity_outcomes/      緩和実現量・LUC排出・BII
├── 12_simple_overviews/
    ├── 01_world_land_trajectories/             世界土地利用の単純な時系列
    ├── 02_r10_land_trajectories/               土地利用別のR10時系列
    ├── 03_country_simple_relationships/        保護・CDR土地と農業影響の国別散布図
    └── 04_land_change_stacks/                  土地区分の正負積み上げ
├── 13_agmip_area_yield_decomposition/
    ├── 01_world/                               世界・主要作物・時系列の面積収量分解
    ├── 02_r10/                                 R10のCRP面積収量分解
    ├── 03_country/                             国別CRP分解と上位25か国
    └── 04_land_cover_vs_harvested_area/        土地被覆から生産までの診断
├── 14_luc_emissions_balance/
    ├── 01_world/                               世界の累積・10年ごとの正負LUCフロー
    ├── 02_r10/                                 R10の累積とシナリオ別時系列
    ├── 03_removal_sources/                     森林・土壌等の負の吸収源内訳
    ├── 04_protection_diagnostics/              Protection効果と植林時期の診断
    └── 05_2070_spike_diagnostics/              2070年の土地転換排出パルス
└── 15_spatial_reallocation_bivariate/
    ├── 01_direct_comparison/                   Protectなし・あり・グリッド差分
    ├── 02_baseline_response_bivariate/         元の土地集中度×増減の二変量地図
    ├── 03_protection_response_bivariate/       保護圧力×土地増減の図2候補
    ├── 04_data_and_matrices/                   面積マトリックス・CSV・GeoTIFF
    ├── 05_cropland_transition_signals/         農地転換の連続強度図と離散分類図
    ├── 06_avoided_vegetation_loss/             守られた植生と他所への減少移転
    ├── 07_bioenergy_transition_signals/         バイオ農地の転換信号
    └── 08_planted_forest_transition_signals/   植林地の転換信号
```

AgMIP品目別フォルダでは、さらに`wht_wheat`、`ric_rice`、`cgr_coarse_grains`のように1品目ごとのフォルダを作る。それぞれに指標別の国地図、上位国の棒グラフ、数値CSVが入る。

## 4. シナリオと対策の状態

基本的に`LandInt_global2`の8シナリオを使う。3桁の状態は左から保護区拡大 \(P\)、植林 \(A\)、BECCS \(B\) の有無を表す。1は導入、0は非導入である。

| 状態 | 使用するシナリオ | 保護区 | 植林 | BECCS |
|---|---|---:|---:|---:|
| 000 | `SSP2_BaU_NoCC_LandInt_global2` | 0 | 0 | 0 |
| 100 | `SSP2_protect_BaU_NoCC_LandInt_global2` | 1 | 0 | 0 |
| 010 | `SSP2NoBio_1100C_CACN_DAC_NoCC_LandInt_global2` | 0 | 1 | 0 |
| 001 | `SSP2NoAff_1100C_CACN_DAC_NoCC_LandInt_global2` | 0 | 0 | 1 |
| 110 | `SSP2NoBio_protect_1100C_CACN_DAC_NoCC_LandInt_global2` | 1 | 1 | 0 |
| 101 | `SSP2NoAff_protect_1100C_CACN_DAC_NoCC_LandInt_global2` | 1 | 0 | 1 |
| 011 | `SSP2_1100C_CACN_DAC_NoCC_LandInt_global2` | 0 | 1 | 1 |
| 111 | `SSP2_protect_1100C_CACN_DAC_NoCC_LandInt_global2` | 1 | 1 | 1 |

`1100C_CACN_DAC`には植林とBECCSを含む緩和条件が入る。`NoAff`は植林を外してBECCSだけを残し、`NoBio`はBECCSを外して植林だけを残す。`protect`は保護区拡大を追加する。3対策の完全な \(2^3\) 要因実験として扱える。

`data/ALPHA/plum_data.gdx`の`yldPlum`だけはREADMEの指定に従い、対応する`LandInt_global1`の8状態を使う。それ以外はすべて`LandInt_global2`である。

## 5. 対策寄与の計算

### 5.1 Shapley分解

国、R10、世界の農業指標、国別土地利用変化排出、BIIでは8状態をすべて使った3要因Shapley分解を行う。ある指標の各状態の値を \(v_{000},\ldots,v_{111}\) とすると、各対策の寄与は次の式になる。

\[
\phi_P = \frac{v_{100}-v_{000}}{3}
+ \frac{(v_{110}-v_{010})+(v_{101}-v_{001})}{6}
+ \frac{v_{111}-v_{011}}{3}
\]

\[
\phi_A = \frac{v_{010}-v_{000}}{3}
+ \frac{(v_{110}-v_{100})+(v_{011}-v_{001})}{6}
+ \frac{v_{111}-v_{101}}{3}
\]

\[
\phi_B = \frac{v_{001}-v_{000}}{3}
+ \frac{(v_{101}-v_{100})+(v_{011}-v_{010})}{6}
+ \frac{v_{111}-v_{110}}{3}
\]

これは対象対策を最初、2番目、最後に導入する場合の限界効果を、起こり得る6通りの導入順序について平均したものである。相互作用も3対策へ配分され、必ず次が成り立つ。

\[
\phi_P+\phi_A+\phi_B=v_{111}-v_{000}
\]

CSVの`protection`、`afforestation`、`beccs`は元の物理単位による寄与、`total`は`111 - 000`、`baseline`は`000`、`final`は`111`である。`*_pct`は対応する値を`baseline`で割って100を掛けたBaU比である。ゼロまたはほぼゼロのBaUに対する比率は定義できないため、`abs(baseline) <= 1e-10`の行は比率を使う図とCSVから除外する。

### 5.2 収量・土地図の直接比較

流域収量とグリッド土地利用は、土地の押出しを直感的に見るため次の条件付き差を使う。

| 表示名 | 状態の比較 | 読み方 |
|---|---|---|
| Protection without mitigation | `100 - 000` | 土地CDRなしの条件で保護区を追加 |
| Protection under mitigation | `111 - 011` | 植林・BECCSがある条件で保護区を追加 |
| Afforestation contribution | `011 - 001` | 保護区なし・BECCSありの条件で植林を追加 |
| BECCS contribution | `011 - 010` | 保護区なし・植林ありの条件でBECCSを追加 |
| Combined policy | `111 - 000` | 全3対策とBaUの総差 |
| Protection interaction | `(111 - 011) - (100 - 000)` | 保護区効果が緩和ありとなしでどれだけ変化するか |

収量は`100 * (比較後 / 比較前 - 1)`、土地面積シェアは割合の差を100倍したpercentage pointsで示す。

## 6. 財分類とエネルギー作物の扱い

流域収量（第7節）はraw ALPHAの非エネルギー作物集合`a_cro_ne`を使う。国・R10・世界の農業市場（第9、12、13節）は`define/ALPHA/AgMIP_c.map`で定義されたAgMIP品目へ切り替えた。

| 種類 | AgMIPコード | 内容 |
|---|---|---|
| 集約 | `AGR` | 農業全体（作物＋畜産） |
| 集約 | `CRP` | 食料・飼料用作物。エネルギー作物を含まない |
| 集約 | `LSP` | 畜産物 |
| 作物 | `WHT`, `RIC`, `CGR`, `OSD`, `SGC`, `VFN`, `OCR` | 小麦、コメ、粗粒穀物、油糧種子、砂糖作物、野菜・果物・ナッツ等、嗜好品・香辛料 |
| 作物内訳 | `VFN|VEG`, `VFN|FRU`, `VFN|NUT` | 野菜、果物、ナッツ |
| 畜産 | `RUM`, `NRM`, `DRY` | 反芻家畜、非反芻家畜、乳製品 |
| 畜産内訳 | `NRM|PRK`, `NRM|PTM`, `NRM|EGG` | 豚等、鶏肉、卵 |

AgMIPのエネルギー作物は`ECP`という独立品目である。`CRP`の対応表に`ECP`は入っておらず、スクリプトの読込品目にも`ECP`を指定していない。したがって、`CRP`の生産・消費・貿易・面積にエネルギー作物は含まれない。

`VFN`と`NRM`は広い上位品目で、それぞれの`|`付き品目は内訳である。探索用に両方を出すが、`by_agmip_item`内の全行を合計すると重複する。合計値にはモデル側で集計済みの`CRP`、`LSP`、`AGR`を使う。現在のAgMIP市場図は加工品を独立区分として出していない。

## 7. `01_basin_yield`: 流域別収量

### 7.1 ALPHA収量

`data/ALPHA/all_scenario_World.gdx`の次の変数を使う。

- `yldBas_load(SCENARIO,c,bas,cty,Y)`: 財、流域、国、年ごとの流域別収量
- `qlBas_load(SCENARIO,c,bas,cty,Y)`: 同じ区分の収穫面積

対象年のglobal2を抽出し、作物間の集約にはBaU状態`000`の`qlBas_load`を固定ウェイトとして使う。`yldBas_load`の単位はt/ha、`qlBas_load`は1000 haである。対象財は非エネルギー作物13品目に限定し、`ecr`と流域データに含まれる畜産財は集約しない。

\[
\bar y_{s,b,country}=\frac{\sum_c y_{s,c,b,country}q^{land}_{000,c,b,country}}
{\sum_c q^{land}_{000,c,b,country}}
\]

固定ウェイトにすることで、作付構成の変化を物理収量の変化と数えない。`aggregate_food_crops/alpha_yldBas_effects_YEAR.png`は、緩和なし・ありの保護区効果、その差の差、植林、BECCS、全対策の6比較を示す。

### 7.2 PLUM収量

`data/ALPHA/plum_data.gdx`の`yldPlum(sce_plum,a_cro,bas,cty,yr)`を使う。このデータだけglobal1の8状態を抽出する。集約ウェイトは`all_scenario_World.gdx`にある対応するglobal1 BaUの`qlBas_load`で、ALPHA収量と同じ6比較を`aggregate_food_crops/plum_yldPlum_effects_YEAR.png`に示す。

両地図とも`data/Basin_Boundary/Basin_Boundary.shp`を`Basin`と`ISO_A3`で結合する。複雑な流域ポリゴンの処理に対して`sf_use_s2(FALSE)`を設定している。対応CSVは地図に投入した流域、国、比較、変化率を保存する。

### 7.3 財別流域収量

`by_commodity/alpha_yldBas/`と`by_commodity/plum_yldPlum/`に、非エネルギー作物13品目を1財ずつ出力する。各財の地図には上記6比較を並べる。ALPHAはglobal2の`yldBas_load`、PLUMはglobal1の`yldPlum`を使い、財間の加重平均は行わない。各資料について全財を結合したCSVと、財フォルダ内のCSVを保存する。これにより、小麦、コメ、トウモロコシ、その他穀物などの空間応答を集約値と分けて確認できる。

## 8. `02_grid_land_use`: 0.5度土地利用

`data/PLUM/nc/AIM-LUmap_<scenario>.nc`のサブデータセット`LC_area_share`を読む。年レイヤーは2005、2010、2020から2100まで10年刻みであり、対象年の位置を明示的に選ぶ。土地クラスは次の3つである。

- class 1: `Food cropland`、非エネルギー農地
- class 2: `2G bioenergy`、第二世代バイオエネルギー作物
- class 5: `Planted forest`、植林地

各セルについて5種類の直接比較を作り、土地クラス3枚を縦に並べた地図を比較ごとに保存する。値は`LC_area_share`の差を100倍したpercentage pointsである。`Protection interaction`が正なら、緩和策があるときの保護区追加効果がBaU条件より正方向へ変わる。正負の意味は土地クラスごとに読む必要がある。

`pixel_area`を各セルのシェア差に掛けて世界合計へ変換し、`global_area_changes_YEAR.csv`へmillion haで保存する。海洋マスクにはBaU対象年の全12土地被覆クラスの合計を使う。

## 9. `03_country_indicators`: 国・R10・世界の農業指標

### 9.1 使用データとAgMIP変数

農業市場図は国、R10、世界とも`data/ALPHA/AGMIP_all_scenario.gdx`を使う。`define/ALPHA/4_AgMIP_output.gms`で作られた国別シンボル`AGMIP(SCENARIO,cty,AgMIP_VAR,ITEM,AgMIP_UNIT,Y)`と、モデル側集計シンボル`AGMIP_agg(SCENARIO,reg,AgMIP_VAR,ITEM,AgMIP_UNIT,Y)`から、対象年と第4節の8シナリオを抽出する。

| AgMIP変数 | 元単位 | 図の指標 | 図・CSVの単位 |
|---|---|---|---|
| `AREA` | `1000 ha` | Area | `1000 ha` |
| `PROD` | `million kcal` | Production | `PJ/yr` |
| `CONS` | `million kcal` | Consumption | `PJ/yr` |
| `EXPO` | `million kcal` | Exports | `PJ/yr` |
| `IMPO` | `million kcal` | Imports | `PJ/yr` |
| `NETT` | `million kcal` | Net exports | `PJ/yr` |
| `PROD` | `USD`と`million kcal` | Producer price | `USD/GJ` |
| `CONS` | `USD`と`million kcal` | Consumer price | `USD/GJ` |
| `CALI` | `kcal/cap/d` | Food intake | `kcal/cap/day` |

`PROD`、`CONS`、`EXPO`、`IMPO`はALPHAの財別数量に品目別カロリー係数を掛けてAgMIP形式へ出力した値である。`NETT`はAgMIP作成時に`EXPO - IMPO`として計算されている。`CALI`は食料として摂取される`qdFood`に基づく。廃棄分も含む`CALO`は使わない。

### 9.2 エネルギー単位への変換と価格

熱量は国際単位系に近いJベースで比較できるよう、次式で変換する。

\[
Q\,[\mathrm{PJ/yr}]
=Q\,[\mathrm{million\ kcal/yr}]\times4.184\times10^{-6}
\]

これは1 kcal = 4184 J、1 million kcal = 4.184 GJによる。食料摂取量`CALI`は一般的な読みやすさを優先し、AgMIPに入っている`kcal/cap/d`を数値変換せず`kcal/cap/day`として示す。

AgMIPには重量ベースの`XPRP`・`XPRC`（`USD/t`）もあるが、重量単位を避けるため使用しない。同じ`PROD`または`CONS`について保存された総金額`USD`と熱量`million kcal`から、エネルギー価格を次式で作る。

\[
p^{producer}\,[\mathrm{USD/GJ}]
=\frac{PROD\,[\mathrm{USD}]}{PROD\,[\mathrm{million\ kcal}]\times4.184}
\]

消費者価格も分子・分母を`CONS`に置き換える。この方法では、`CRP`や`AGR`の価格もAgMIP内で同じ品目に集計された総金額と総熱量の比になる。raw価格を重量で加重した旧来の固定バスケット指数は使わない。分母の熱量がゼロまたはほぼゼロの行は価格を定義しない。

### 9.3 国別の集約品目と詳細品目

`country/aggregate_agmip_items`には`AGR`、`CRP`、`LSP`を出す。`CRP`が非エネルギー作物の主な集約結果である。`country/by_agmip_item`には第6節の主要作物・畜産物を出す。各品目で利用可能なArea、Production、Consumption、Exports、Imports、Net exports、Producer price、Consumer price、Food intakeを保持する。

各品目・指標について、3対策のShapley寄与を縦に並べた国地図、総効果が大きい国の積み上げ棒、8状態と寄与を含むCSVを保存する。地図はBaU比である。棒はAreaと5つの数量指標を物理単位（`1000 ha`または`PJ/yr`）、価格・食料摂取をBaU比で示す。黒い菱形は`111 - 000`で、3色の寄与の代数和と一致する。

### 9.4 国地図の欠損処理

国境は`rnaturalearth::ne_countries()`の`iso_a3`をALPHAの`cty`へ結合する。値を持つ国だけを`inner_join`したレイヤーに政策ファセットを割り当て、全世界の灰色レイヤーを別に背後へ描く。データがない国は灰色で残り、政策名が欠損した`NA`パネルは生成されない。

### 9.5 R10地域

`AGMIP_agg`の`reg`次元に格納された10地域を直接使う。対象はAfrica、China+、Europe、India+、Latin America、Middle East、North America、Pacific OECD、Reforming economies、Rest of Asiaである。国別値の再合計ではなく、AgMIP作成時の地域集計値である。

各品目・指標について、Shapley寄与のR10棒と、保護区・植林・BECCS・総効果を並べたBaU比ヒートマップを出す。量的指標の棒は`PJ/yr`または`1000 ha`、価格と食料摂取の棒はBaU比である。

### 9.6 世界全体

世界値も`AGMIP_agg`の`reg == "World"`を直接使う。国やR10の再合計ではない。各集約・詳細品目について、利用可能な全指標のShapley寄与をBaU比で並べた概要図を出す。R10と世界のマスターCSVには8状態、物理単位の寄与、BaU比を保存する。

## 10. `04_land_emissions`: 排出と土地配分

### 10.1 土地利用変化由来排出

`data/PLUM/Mergedcomparison.gdx`の`GHGL(i1,i2,i3,i4,i5)`を使う。次元はシナリオ、地域、年、排出カテゴリ、土地カテゴリである。`i4 == "Net"`かつ`i5 == "LUC"`を抽出し、2020年以降を使う。

世界時系列は`i2 == "World"`についてBaU `000`、保護区のみ`100`、植林＋BECCS `011`、全対策`111`の正味LUC排出を示す。単位はMt CO2/yrである。

国別は`data/country_mapping.csv`に収録された国コードを選び、対象年の8状態をShapley分解する。図では排出そのものの寄与にマイナスを掛けて「排出削減」とする。正値はBaUより正味排出が低い、または吸収が大きいことを意味する。地図は3対策、棒は総削減量の絶対値が大きい20か国を示す。国地図は農業地図と同じ背景レイヤー方式を使い、`NA`パネルを作らない。

### 10.2 世界土地配分

同じGDXの`IAMCTempwoU`から地域`WLD`、2020年以降について次を読む。

- `Lan_Cov_Cro_Non_Ene_Cro`: 非エネルギー作物農地
- `Lan_Cov_Cro_Ene_Cro_2nd_gen`: 第二世代バイオエネルギー作物
- `Lan_Cov_Frs_Aff`: 植林地

BaU、保護区のみ、植林＋BECCS、全対策の世界面積推移をmillion haで示す。食料農地、バイオエネルギー、植林の競合がいつ現れるかを見る図である。

## 11. `05_biodiversity`: BII

`data/BII/nc/BIImap_<scenario>_default.nc`のサブデータセット`BII`から対象年の0.5度レイヤーを読む。8状態の各セルにShapley式を適用し、保護区、植林、BECCS、全対策の寄与をBII pointsで地図にする。図では100を掛け、正値はBII改善を表す。

世界平均は`terra::cellSize()`で求めたセル面積を固定ウェイトにし、有効セルだけの面積加重平均を計算して`bii_global_shapley_YEAR.csv`へ保存する。

地域別は`data/BII/csv/BII_regionagg_*_default_area.csv`の対象年列`v_YEAR`を読み、8シナリオを結合してShapley分解する。このCSVに収録された地域をそのまま使うため、国、R5、世界、その他の集約地域が混在する。棒は総変化の絶対値が大きい20地域を探索用に表示し、全地域はCSVへ保存する。

## 12. `06_protection_focus`: 保護区拡大を中心にした追加分析

このフォルダは、READMEの研究目的に沿って「保護区を追加すると何が変わるか」と「その効果が植林・BECCSとの同時導入でどう変わるか」を直接比較する。Shapley分解は全条件を平均した帰属を示すが、ここでは保護区の条件付き効果と相互作用を明示する。

| 研究上の問い | 主に見る出力 |
|---|---|
| RQ1 土地競合・空間再配置 | `01_land_competition`の土地クラス別地図、世界面積、時系列 |
| 高収量地域からの農業の押出し | `02_yield_displacement`の収量五分位別作付面積変化 |
| RQ2 保全と土地CDRの相互作用 | 各図のWithout mitigation、Under mitigation、Context differenceの比較 |
| RQ3 国・地域の農業市場影響 | `03_agriculture`の国地図・上位国棒・R10ヒートマップ |
| 気候・生物多様性とのトレードオフ | `04_climate_biodiversity`と`05_cross_domain_summary` |

### 12.1 条件付き保護区効果

元の物理単位では次を使う。

\[
P_{without}=v_{100}-v_{000},\qquad
P_{under}=v_{111}-v_{011}
\]

\[
I_P=P_{under}-P_{without}
=(v_{111}-v_{011})-(v_{100}-v_{000})
\]

物理単位の`protection_interaction`が0なら、保護区の限界効果は土地CDRの有無で変わらない。正なら土地CDR条件で保護区効果がより正方向、負ならより負方向になる。これは保護区と「植林＋BECCSを一体とした緩和条件」の差の差であり、植林とBECCSを個別に分けた相互作用ではない。

国・R10・世界の農業図は、`Without mitigation`を`000`、`Under mitigation`を`011`で割る。3枚目の`Context difference`は、この2つの相対効果を引いたpercentage pointsである。

\[
P_{without}^{\%}=100\frac{v_{100}-v_{000}}{v_{000}},\quad
P_{under}^{\%}=100\frac{v_{111}-v_{011}}{v_{011}}
\]

\[
Context\ difference\,[\mathrm{pp}]=P_{under}^{\%}-P_{without}^{\%}
\]

CSVには物理量の差の差`protection_interaction`、それを`000`で割った`protection_interaction_pct_of_bau`、図で使う`protection_context_difference_pp`をすべて残す。基準値が違う2つの割合と、物理量の差の差を同じものとして扱わないためである。

### 12.2 土地競合

`01_land_competition`は`AIM-LUmap`のFood cropland、2G bioenergy、Planted forestについて、緩和なし、緩和あり、差の差を土地クラスごとに3段地図で示す。同じセル差に`pixel_area`を掛け、世界合計をmillion haの棒とCSVにする。さらに`Mergedcomparison.gdx`の`IAMCTempwoU`から3土地クラスの世界面積を読み、2020年以降の条件付き保護区効果を時系列で示す。GDXでは0値が保存されない場合があるため、欠けた状態・年は0として補完してから差を取る。

### 12.3 収量水準別の農地移動

`02_yield_displacement`は、小麦、コメ、トウモロコシ、その他穀物、油糧作物、砂糖作物を対象にする。`yldBas_load`のglobal2 BaU収量で作物ごとに流域・国レコードを五分位へ分け、`qlBas_load`から求めた保護区導入時の収穫面積変化を五分位内で合計する。`Q1 low`が低収量、`Q5 high`が高収量である。

高収量側の面積が減り、低収量側が増える場合は、高収量地から低収量地への押出しと整合的である。五分位は各流域・国レコードの個数がほぼ同じになる区分で、面積加重の境界ではない。BaU作付面積と収量が正の場所だけを使うため、BaUで未作付だった新規作付地はこの診断に含まれない。

`source_sink_yield_levels`は、面積が減った場所と増えた場所について、面積変化の絶対値で加重したBaU収量を比較する。減少側の加重収量が増加側より高ければ、同じく高収量地からの移動を示唆する。これは市場調整を含むモデル結果の記述統計であり、因果経路を単独で証明する指標ではない。

### 12.4 農業市場

`03_agriculture`は第9節と同じ`AGMIP`・`AGMIP_agg`、AgMIP品目、Jベースの単位を使う。`country`には`AGR`、`CRP`、`LSP`と主要作物・畜産物について、Area、Production、Consumption、Exports、Imports、Net exports、Producer price、Consumer price、Food intakeを出す。各品目・指標に3段国地図、緩和下の保護区効果が大きい12か国の並列棒、全数値CSVがある。

`region_r10`は各品目・指標について10地域を比較し、`world`は各品目の全指標をまとめる。3系列は`Without mitigation`、`Under mitigation`、`Context difference`である。最初の2つが条件付きの保護区効果、3つ目が両者の相対差である。`Under mitigation`には植林やBECCSの単独導入効果は含まれず、緩和条件を固定したまま保護区だけを追加した差`111-011`である。

### 12.5 保護面積、土地排出、BII

`04_climate_biodiversity`には次を出す。

- `Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`による世界の統合保護面積時系列。BaU、保護区のみ、緩和のみ、保護区＋緩和を表示する。
- `GHGL`のNet・LUCによる世界の保護区起因排出削減時系列と、対象年の国別3段地図。排出値にマイナスを掛け、正を排出削減とする。
- BIIグリッドの条件付き保護区効果と差の差。世界平均時系列はセル面積加重である。

`05_cross_domain_summary`は対象年について、保護面積、食料農地、2Gバイオエネルギー、植林、LUC排出削減、世界平均BIIを物理単位のまま小分けのパネルで並べる。各パネルの尺度が異なるため、指標間の棒の長さは比較せず、同じパネル内で緩和なし、緩和あり、差の差を比較する。

同じフォルダの`grid_bii_land_association`は、緩和下の保護区効果`111 - 011`について、各セルのBII変化とFood cropland、2G bioenergy、Planted forestの面積シェア変化を結ぶ。BIIグリッドの経度範囲はAIM-LUグリッドから0.25度ずれているため、BIIをAIM-LUの0.5度グリッドへbilinear補間してから結合する。散布図は極端値で中心部分が読めなくなるのを避けるため、土地変化とBII変化の絶対値について各財の中心98%を表示し、5万セルまで固定乱数で抽出する。CSVには全セルのSpearman相関と、表示範囲内のPearson相関・回帰傾きを保存する。

`grid_bii_food_cropland_responses`は同じ結合値を、BII増減と食料農地増減の4組合せで地図化する。絶対変化が0.01 percentage points未満の食料農地、または0.01 BII points未満のBIIは`Small change`とする。CSVは各組合せのセル数、面積、面積加重平均変化と、この閾値を保存する。これは保全便益と農地変化が重なる場所を探索する分類であり、生産量や食料安全保障の最終的な良否を直接示す分類ではない。

## 13. `07_conditional_protection_decomposition`: 緩和策下の保護区効果の詳細分解

### 13.1 何を分解するか

この分析が分解する対象は、植林とBECCSがすでにある緩和条件へ保護区を追加したときの総変化である。

\[
P_{under}=v_{111}-v_{011}
\]

たとえば生産量なら、`111`の生産量から`011`の生産量を引く。したがって、この総変化は「緩和策そのものを導入した効果」ではなく、同じ緩和条件のもとで保護区の有無だけを変えた条件付き保護区効果である。ただし、保護区導入後に土地配分、価格、生産、貿易が一般均衡的に調整するため、結果には植林やBECCSとの相互作用が含まれる。

8状態を使い、次の4成分へMöbius型の完全要因分解を行う。

\[
P=v_{100}-v_{000}
\]

\[
P\times A=v_{110}-v_{100}-v_{010}+v_{000}
\]

\[
P\times B=v_{101}-v_{100}-v_{001}+v_{000}
\]

\[
P\times A\times B=
v_{111}-v_{110}-v_{101}-v_{011}
+v_{100}+v_{010}+v_{001}-v_{000}
\]

ここで (P) は保護区、(A) は植林、(B) はBECCSである。4成分は必ず次のように閉じる。

\[
P+(P\times A)+(P\times B)+(P\times A\times B)
=v_{111}-v_{011}
\]

`06_protection_focus`のCSVに保存した物理単位の`protection_interaction`との関係は次式である。

\[
Interaction=(P\times A)+(P\times B)+(P\times A\times B)
\]

したがって、従来の`Under mitigation`は上の4成分を合計した保護区の総効果であり、植林やBECCSを新たに導入した効果ではない。`Under mitigation`が`Without mitigation`より大きい場合は、植林・BECCSがあることで保護区の限界効果が変わったことを意味する。その差を今回の3つの相互作用成分へ分けている。

### 13.2 各成分の読み方と限界

- `Protection main effect`は植林もBECCSもないときの保護区効果`100 - 000`である。
- `Protection × afforestation`は、BECCSがない条件で、植林があることによって保護区効果がどれだけ変わるかを示す。`(110 - 010) - (100 - 000)`と同じである。
- `Protection × BECCS`は、植林がない条件で、BECCSがあることによって保護区効果がどれだけ変わるかを示す。`(101 - 001) - (100 - 000)`と同じである。
- `Three-way interaction`は、植林とBECCSを同時に置いた場合の追加的な非加法性である。植林との相互作用とBECCSとの相互作用を単純に足すだけでは説明できない残りを受け持ち、二重計上を防ぐ。

`Protection × afforestation`をそのまま「植林が押し出されたことだけによる寄与」、`Protection × BECCS`を「BECCSが押し出されたことだけによる寄与」と呼ぶことはできない。ALPHA・PLUMは土地、市場、貿易、価格を同時に調整するため、この成分には押し出された面積の直接影響だけでなく、立地変更、収量差、国際貿易、価格反応など、その対策が存在するときに生じるすべての一般均衡的な効果変更が入る。3者相互作用を植林とBECCSのどちらか一方へ一意に配分することも、現在の8シナリオだけからはできない。

符号は対象指標の方向と合わせて読む。たとえば生産量の`Protection × BECCS`が負なら、BECCSがあることで保護区の生産影響がより負方向になったことを表す。価格で正なら、BECCSがあることで保護区導入時の価格上昇が強まったことを表す。これは政策の良否や因果経路を単独で確定する判定ではない。

### 13.3 農業の積み上げ棒と国地図

`01_agriculture`は第9節と同じ`data/ALPHA/AGMIP_all_scenario.gdx`から国別`AGMIP`、地域・世界`AGMIP_agg`を読み、8シナリオと対象年を抽出する。対象指標、Jへの変換、価格計算も第9.1–9.2節と同じである。地域値と世界値はモデル集約値を直接使い、国の再合計ではない。

対象はAgMIP集約品目`AGR`、非エネルギー作物`CRP`、畜産物`LSP`と、第6節の詳細作物・畜産物である。`ECP`は含めない。食料摂取量もAgMIPの`CALI`を品目ごとに読み、元の`kcal/cap/day`で示す。

比率は4成分と総効果のすべてを同じ緩和ベースライン`v011`で割る。

\[
Component^{\%}=100\frac{Component}{v_{011}}
\]

この統一分母により、パーセント表示でも4成分の代数和が黒い菱形の`Total under mitigation`と一致する。積み上げ棒では正の成分と負の成分をゼロから別方向へ積むため、符号が混在すると黒い菱形は棒の外端ではなく、正負を相殺した位置に来る。黒い菱形が最終的な純変化である。

国別は総効果の絶対値が大きい12か国を棒で示す。地図は4成分と総効果の5面で、背景の全世界レイヤーと値を持つ国のレイヤーを分けているため`NA`ファセットは作られない。R10は品目・指標別の積み上げ棒、世界は品目ごとに全指標をまとめた積み上げ棒を保存する。

### 13.4 土地利用グリッドの分解

`02_grid_land`は8シナリオの`data/PLUM/nc/AIM-LUmap_<scenario>.nc`から対象年の`LC_area_share`を読み、Food cropland（クラス1）、2G bioenergy（クラス2）、Planted forest（クラス5）の各0.5度セルに上記4成分式を直接適用する。地図の単位はセル面積シェアの差を100倍したpercentage pointsである。各土地クラスについて4成分と総効果の5面地図を出す。

同じNetCDFの`pixel_area`を各成分へ掛け、世界面積寄与をmillion haで集計する。`global_land_area_stacked_components_YEAR.png`の黒い菱形は`111 - 011`の世界面積差、色付き棒は4成分である。CSVには5項目を残すため、成分ごとの符号と相殺を数値で確認できる。

### 13.5 実際に変化した植林・BECCS面積との対応

`03_observed_land_channels`は`data/PLUM/Mergedcomparison.gdx`の`IAMCTempwoU`から国別の`Lan_Cov_Frs_Aff`と`Lan_Cov_Cro_Ene_Cro_2nd_gen`を読む。緩和策下で保護区を追加した実際の面積変化を次のように計算する。

\[
\Delta A_{country}=A_{111}-A_{011},\qquad
\Delta B_{country}=B_{111}-B_{011}
\]

地図ではこの符号付き変化をmillion haで示す。負なら保護区導入後にその国の植林または2Gバイオ面積が減り、正なら増えたことを表す。GDXではゼロ値が省略されるため、国×土地用途×8状態を明示的に補完し、欠けた値を0としてから差を取る。

散布図では見やすさのため各軸の絶対値について中心98%を表示するが、`land_displacement_association_summary_YEAR.csv`のPearson・Spearman相関は切り詰め前の全利用可能国で計算する。横軸の`Area displaced after protection`は上式にマイナスを掛け、正を面積減少、負を面積増加としている。縦軸はAgMIPの非エネルギー作物`CRP`について、植林なら`Protection × afforestation`、2Gバイオなら`Protection × BECCS`のArea、Production、Producer priceへの寄与である。ProductionはAgMIP熱量に基づく`PJ/yr`、Producer priceは`USD/GJ`から計算した相対寄与である。

この散布図は「面積減少が大きい国ほど対応する農業相互作用も大きいか」という機構整合性の診断であり、因果寄与の再分解ではない。相関が弱い場合、面積押し出しだけでは相互作用を説明できず、立地、土地生産性、市場・貿易調整、3者相互作用を併せて見る必要がある。

## 14. `08_land_use_protection`: 保護区と土地利用の重なり・再配分

### 14.1 使用データ、シナリオ、土地区分

主な集計には`data/PLUM/Mergedcomparison.gdx`の`IAMCTempwoU(i1,i2,i3,i4,value)`を使う。AIM-PLUMから出力された土地面積を、世界`WLD`、モデル集計済みのR10地域、国コードについて直接読む。2020年から対象年までを読み、対象年の棒・地図と世界時系列を作る。比較する4状態はBaU `000`、保護区のみ`100`、植林・BECCSを含む緩和`011`、緩和＋保護区`111`である。

土地構成の積み上げでは、次の7区分だけを使う。互いに重ならず、合計が`Lan_Cov`に一致するようにした。

| 図の土地区分 | MergedComparison変数・計算 |
|---|---|
| Food cropland | `Lan_Cov_Cro_Non_Ene_Cro` |
| 2G bioenergy cropland | `Lan_Cov_Cro_Ene_Cro_2nd_gen` |
| Fallow cropland | `Lan_Cov_Cro - Lan_Cov_Cro_Non_Ene_Cro - Lan_Cov_Cro_Ene_Cro_2nd_gen` |
| Forest | `Lan_Cov_Frs` |
| Pasture | `Lan_Cov_Pst` |
| Other natural land | `Lan_Cov_Oth_Nat_Lan` |
| Built-up area | `Lan_Cov_Bui_Are` |
| Total | `Lan_Cov` |

`Lan_Cov_Frs_Aff`（植林）と`Lan_Cov_Frs_Ref`（再植林）はForestの内数なので、土地Totalの積み上げには入れず、保護区への応答を見る診断系列として別に示す。`Lan_Cov_Oth_Lan`はPLUMの元の`Lan_Cov`集計式に含まれないため、`Other land outside reported total`として別系列に置く。これらをTotalへ足すと二重計上になる。`land_inventory_closure.csv`には7区分合計、Total、残差を全空間レベル・全状態・全年について保存する。

### 14.2 保護区導入後に変わった面積

緩和策下で保護区を追加した条件付き効果を各土地区分について計算する。

\[
\Delta L_c=L_{c,111}-L_{c,011}
\]

`protection_land_displacement_all_levels.csv`には、面積差million ha、各区分の`011`面積に対する割合、`011`の土地Totalに対する割合、追加保護面積に対する割合を保存する。比較用に緩和なしの`100-000`と差の差`(111-011)-(100-000)`も残す。負はその空間集計単位で面積が減ったこと、正は増えたことを表す。この差は、保護区と物理的に重なった同じ土地片がそのまま移動した面積ではなく、モデルが再均衡した後の土地用途別の純変化である。

重複のない7区分について、増加側と減少側を次式で集計する。

\[
G=\sum_c\max(\Delta L_c,0),\qquad
D=\sum_c\max(-\Delta L_c,0)
\]

\[
R=\frac{G+D}{2}
\]

Totalが閉じる場合は`G=D=R`となる。`R`を`Net inter-class reallocation`と表記し、土地Totalに対する割合と追加保護面積に対する割合を世界、R10、国で示す。これは国・地域内で相殺後に残る用途間の純再配分であり、同じ用途が域内の別セルへ移った量は数えない。そのため、空間的な総移動量としては控えめな値になる。

2100年の世界値では、保護面積指標は`011`の2685.19 million ha（土地Totalの21.88%）から`111`の4876.35 million ha（39.74%）へ増え、追加分は2191.16 million ha、17.86 percentage pointsである。一方、用途間の純再配分は71.39 million haで、土地Totalの0.582%、追加保護面積の3.26%である。保護面積の増加そのものを「動いた土地」と解釈しないことが重要である。

同じ世界比較で、Food croplandは+10.53 million ha（`011`比+0.852%）、2G bioenergy croplandは+9.70 million ha（+3.91%）、Afforestationは-18.96 million ha（-6.34%）となった。Forest全体は-13.14、Pastureは-50.28、Other natural landは+51.15 million haである。世界純量では食料農地と2Gバイオが増えるため、「保護区が必ず両者の世界総面積を減らす」という結果ではない。国別地図とグリッド診断で、減少地域から増加地域への再配置を併せて確認する。

### 14.3 保護区がどの土地被覆と重なるか

保護面積全体には`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`を使う。追加保護面積は`111-011`、保護率は各状態の値を`011`の`Lan_Cov`で割って計算する。保護地内の土地被覆は次のモデル出力をそのまま示す。

| 保護地内訳 | 変数 |
|---|---|
| Primary forest | `Lan_Cov_Pro_Are_Prm_Frs` |
| Secondary forest | `Lan_Cov_Pro_Are_Sec_Frs` |
| Primary grassland | `Lan_Cov_Pro_Are_Prm_Gra` |
| Secondary grassland | `Lan_Cov_Pro_Are_Sec_Gra` |
| Cropland | `Lan_Cov_Pro_Are_Cro` |
| Managed pasture | `Lan_Cov_Pro_Are_Pas` |
| 上記内訳のモデル集計 | `Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra` |

この内訳合計と`..._Int`は異なるモデル指標であり、同じTotalとして無理に一致させない。現在のMergedComparisonには、保護区とAfforestationまたは2G bioenergy croplandの直接交差面積を区別した変数がない。そのため、植林・2Gバイオについて出せるのは、直接の重複面積ではなく`111-011`で観察される面積応答である。厳密な重複を求めるには、保護マスクと用途別グリッドを同一セル・同一年で掛け合わせた追加出力が必要になる。

内訳を対応する土地面積で割る図では、森林内訳は`Lan_Cov_Frs`、草地内訳は`Lan_Cov_Oth_Nat_Lan`、Croplandはエネルギー農地・休耕地を含む`Lan_Cov_Cro`、Managed pastureは`Lan_Cov_Pst`を分母にする。PrimaryとSecondaryは同じ分母を使うため、土地クラス全体の被保護率を見るときは両者を合計する。

国コードについて、用途別面積の`111-011`は160前後の国で非ゼロ値を持つ一方、2100年の`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`の追加分が非ゼロなのは8か国だけである。国別追加保護面積地図の広い空白は描画時のNAではなく、元GDXに国別非ゼロ値が保存されていないために生じる。追加保護面積の総量と地域比較には世界・R10出力を優先し、国別には用途別面積変化の地図を主に使う。

`Lan_Cov_Pro_Are_Cro`には一部地域・状態で負値が含まれる。スクリプトは0へ丸めず原値を保存し、`negative_area_flag`を付ける。面積として物理的に解釈する前に、PLUM側の保護面積集計式を確認する必要がある。

### 14.4 グリッド上の増加・減少・再配置

`04_grid_relocation/world`は`AIM-LUmap_<scenario>.nc`の`LC_area_share`からFood cropland（クラス1）、2G bioenergy cropland（クラス2）、Planted forest（クラス5）を読み、各セルで`111-011`を計算する。セル差に`pixel_area`を掛け、正のセルをArea gained、負の絶対値をArea lost、両者の小さい方をSpatially relocated、差をNet changeとして世界集計する。これは同じ土地用途が減少セルから増加セルへ移った規模を見るための診断である。

NetCDFのクラスとMergedComparison変数は完全に同じ定義・集計過程とは限らない。特にNetCDFのPlanted forestを`Lan_Cov_Frs_Aff`と同一視できず、`LC_area_share`には0未満または1超のセルもある。このため、グリッドの世界純量とMergedComparisonの世界純量は一致しない。`world_grid_vs_merged_diagnostic_YEAR.csv`に両者を並べ、差を隠さず保存する。土地Totalや世界純変化の主集計にはMergedComparisonを使い、グリッド集計は移動場所と増減の広がりを探す用途に限定する。

### 14.5 主な図の場所

- `01_land_inventory/world`と`r10`: 重複のない土地構成を面積・土地Total比で比較する。
- `02_protection_displacement/world`: 世界の用途別`111-011`、緩和なしとの比較、主要3用途と追加保護面積の要約を示す。
- `02_protection_displacement/r10`: 地域別の用途変化と、土地Total・追加保護面積に対する再配分率を示す。
- `02_protection_displacement/country/<land_class>`: 国別面積差、基準面積比、変化率上位国を示す。`country_protection_reallocation_maps`は国別の追加保護率と再配分率をまとめる。
- `03_protected_footprint`: 保護面積全体、保護地内の森林・草地・農地・牧草地内訳、その各土地用途面積に対する比率を示す。
- `04_grid_relocation/world`: 食料農地、2Gバイオ、植林のセル増加量、セル減少量、純変化を示す。

## 15. `09_land_dynamics_story`: 土地の押し出しと再配置を読む図

### 15.1 このフォルダで新たに見るもの

第14節までの面積差地図は、ある場所で減って別の場所で増えた結果を示すが、世界純量では両者が相殺される。`09_land_dynamics_story`は、最終的な純変化だけでなく、次の順序で土地利用の動きを読むための探索用図である。

1. 対策を追加した出発状態と到着状態を明示する。
2. 各セルで減った面積を`Area lost`、増えた面積を`Area gained`として別々に集計する。
3. 減少量のうち別セルの増加で補われた量と、最後に残る純増減を示す。
4. 世界、R10、国について、どの対策・土地用途で再配置が大きいかを比較する。

新しい図にはタイトル、サブタイトル、短い注記を入れ、比較式と図の限界が図だけでも分かるようにした。PNGとともに元の集計CSVを保存する。

### 15.2 比較する7つの政策変化

各列・各パネルは別々の限界比較であり、互いに足し合わせない。

| 図の比較 | 式 | 読み方 |
|---|---:|---|
| Protection without CDR | `100-000` | CDRがない条件で保護区を追加 |
| Afforestation | `010-000` | 保護区・BECCSなしで植林を追加 |
| BECCS | `001-000` | 保護区・植林なしでBECCSを追加 |
| Both CDR | `011-000` | 保護区なしで植林とBECCSを同時追加 |
| Protection with afforestation | `110-010` | 植林がある条件で保護区を追加 |
| Protection with BECCS | `101-001` | BECCSがある条件で保護区を追加 |
| Protection with both CDR | `111-011` | 植林・BECCSがある条件で保護区を追加 |

`01_policy_response_matrix`はMergedComparisonの世界・R10集計値を使う。世界マトリクスでは、列が上記7比較、行が食料農地、2Gバイオ農地、植林、森林、牧草地、その他自然地、休耕農地である。セルの数字と色はいずれも到着状態と出発状態の純面積差million haを示す。AfforestationはForestの内数なので、行同士を合計しない。

`world_policy_path_cdr_then_protection`は、BaU `000`から両CDR `011`を導入し、その後に保護区を加えて`111`へ至る一つの政策経路を示す。紫の`011-000`と緑の`111-011`の和が、黒の`111-000`に一致する。これは政策シナリオ間の段階的な差であり、個々の土地筆が順番に転換したことを追跡する図ではない。

### 15.3 減少セル、増加セル、純変化

`02_protection_pushout`と`03_cdr_food_redistribution`は、AIM-LUmapの0.5度`LC_area_share`を使う。土地クラスはFood cropland（クラス1）、2G bioenergy cropland（クラス2）、Planted forest（クラス5）である。比較ごとのセル面積差を次式で作る。

\[
\Delta a_i=(s_{i,to}-s_{i,from})A_i
\]

ここで (s_i) はセル内面積シェア、(A_i) は`pixel_area`である。世界または対象地域について、次を別々に集計する。

\[
Loss=\sum_i\max(-\Delta a_i,0),\qquad
Gain=\sum_i\max(\Delta a_i,0)
\]

\[
Relocated=\min(Loss,Gain),\qquad
Net=Gain-Loss
\]

`Loss offset elsewhere (%)`は`100 × Relocated / Loss`である。100%なら、減ったセル面積と同量以上が別セルで増えており、世界または地域の純量は小さくても空間分布は大きく変わったことを表す。国別回復率地図では同じ計算を各国内で行うため、100%未満はその国で失われた面積が国内の増加で補われていないこと、100%超は国内では純増したことを示す。

2100年に両CDRがある状態へ保護区を追加した`111-011`の世界グリッド診断では、次のようになる。

| 土地用途 | 減少セル | 増加セル | 純変化 | 減少の補填率 |
|---|---:|---:|---:|---:|
| Food cropland | 225 Mha | 215 Mha | -10.2 Mha | 95.5% |
| 2G bioenergy cropland | 128 Mha | 129 Mha | +0.9 Mha | 100%超 |
| Planted forest | 312 Mha | 243 Mha | -68.6 Mha | 78.0% |

この結果から、保護区導入後の世界純変化が小さくても、食料農地と2Gバイオ農地では100 Mhaを超えるセル間再配置が起きていることが分かる。Planted forestでは減少セルの約22%が別セルの増加で補われず、純減として残る。

同じグリッド診断でFood croplandを見ると、Afforestation `010-000`は減少441 Mha・増加262 Mha、BECCS `001-000`は減少433 Mha・増加243 Mha、両CDR `011-000`は減少486 Mha・増加252 Mhaである。したがってCDR導入は、世界純農地面積だけでなく、食料農地の空間配置を大きく変えている。ただし3比較は独立した反実仮想比較であり、植林とBECCSの減少量を足して両CDRの結果にすることはできない。

### 15.4 地域・国での読み方

グリッドセルはNatural Earthの国境ポリゴンへセル中心で割り当てる。国からR10への集計には`AGMIP_all_scenario.gdx`の`cty_reg_map`に保存された正式なR10対応を使う。

- `r10_protection_pushout_balance`: 保護区追加後のFood cropland、2G bioenergy、Planted forestの減少・増加を地域別に示す。
- `country_protection_loss_vs_gain`: 横軸が減少、縦軸が増加である。45度線より下の国は減少を回復できず、上の国は純増する。
- `country_protection_recovery_ratio_maps`: 保護区導入後に失われた面積の何%が同国内の別セルで補われるかを3用途別に示す。
- `r10_food_cropland_redistribution_by_policy`: 植林、BECCS、両CDR、両CDR下の保護区について、食料農地がどの地域で減り、どの地域で増えるかを比較する。
- `country_dominant_food_cropland_pressure`: 各国について、植林、BECCS、両CDR下の保護区のうち、食料農地の減少セル面積が最大になる比較を色分けする。比較の出発状態が異なるため、政策の優劣ではなくスクリーニングとして使う。

国・R10のグリッド集計は国境をまたぐセルをセル中心で一国へ割り当てる探索値である。最終的な地域純量にはモデル集計済みのMergedComparisonを使う。第14.4節のとおり、NetCDFの面積シェアには範囲外値があり、MergedComparisonとNetCDFの世界純量も一致しないため、グリッド図は再配置の場所と規模感を調べる診断として扱う。

### 15.5 「重複」と「押し出し」を区別する図

`04_overlap_evidence/world_overlap_vs_land_response`の左側は、MergedComparisonが直接出力する保護地内の一次林、二次林、一次草地、二次草地、農地、牧草地面積である。右側は保護区導入後のFood cropland、2G bioenergy cropland、Afforestationの`111-011`面積応答である。

左は保護区との重なりを直接表すモデル出力、右は保護区導入後に再均衡した結果であり、意味が異なる。現在のデータでは保護地内Croplandを食料・休耕・2Gバイオに分離できず、保護区とAfforestationの直接交差もない。そのため「保護区とCDR用地が直接何Mha重なったか」は確定できない。一方、「保護区追加後にCDR用地が何Mhaのセルから消え、何Mhaが別セルで増えたか」はグリッド差から診断できる。この区別を図のタイトルと注記にも明示した。

## 16. `10_country_opportunity_profiles`: 国別の影響・相互作用・便益と負担

### 16.1 目的と基本方針

このフォルダは、本モデルフレームワークの国別解像度を生かし、次の問いを一つの統合表と複数の探索図から調べる。

1. CDRがない場合の保護区効果と、植林・BECCSがある場合の保護区効果は、国ごとにどれほど異なるか。
2. BII改善が大きい国で、作物生産、食料摂取、生産者価格への負担も大きいか。
3. 食料農地のセル間再配置が大きい国ほど、農業市場影響も大きいか。
4. 輸入依存度、貿易開放度、所得水準に国別影響の連続的な傾向があるか。

国を「シナジー国」「高負担国」のように固定分類すると、閾値や指標の重みに結果が左右される。そのため図では連続量を使い、ラベルは図を読み始めるための少数国だけに付ける。全166か国の値は`country_integrated_profile_2100.csv`に残す。

### 16.2 使用データ、条件、計算式

農業市場は`AGMIP_all_scenario.gdx`のAgMIP集約品目`CRP`を使う。`CRP`は非エネルギー作物であり、エネルギー作物`ECP`は含めない。対象指標はArea、Production、Consumption、Producer price、Food intake、Imports、Exportsである。生産・消費・貿易の元物理量はAgMIPのエネルギー量、食料摂取は`kcal/cap/day`、価格は既存のAgMIP変換値を引き継ぐ。図の縦横軸では、いずれも出発シナリオに対する変化率を使う。

CDRなしの保護区主効果は`100-000`、植林とBECCSがある緩和条件での保護区効果は`111-011`である。この二つの差を国別Interactionとする。

\[
Interaction=(111-011)-(100-000)
\]

したがってInteractionは、保護区効果そのものではなく、植林・BECCSが存在することで保護区効果がどれだけ変わったかを示す。`country_main_effect_vs_interaction`では横軸が`100-000`、縦軸がInteraction、色が合計`111-011`である。点線`y=-x`上では`111-011=0`となる。`country_protection_context_dumbbells`では紫点が`100-000`、緑点が`111-011`で、2点間の線分がInteractionに等しい。

BIIは`data/BII/nc/BIImap_*.nc`の2100年`111-011`を使う。BIIグリッドはAIM-LUグリッドと経度位置が0.25度ずれているため、AIM-LUの`pixel_area`格子へbilinear再標本化した後、Natural Earth国境へセル中心で割り当て、セル面積加重平均を取る。図のBII単位はpointsであり、国土内の利用可能セル平均である。

食料農地再配置量は第15節のAIM-LUグリッド診断から取り、各国内で`min(減少セル面積, 増加セル面積)`とする。土地純変化はMergedComparison、LUC排出削減は`06_protection_focus`の国別`111-011`を結合する。したがって、再配置量は場所の入れ替わりを示す探索値、土地純量はモデル集計値であり役割が異なる。

国の特徴はシナリオ`011`から取る。作物輸入依存度は`100 × CRP Imports / CRP Consumption`、作物貿易開放度は`100 × (Imports + Exports) / Consumption`である。人口は`POPT`、一人当たりGDPは`GDPT/POPT`を使う。GDXのGDPT単位ラベルは`bn USD 2017 PPP`だが、`define/ALPHA/4_AgMIP_output.gms`では`gdpPc_load [USD/cap] × pop_load [million]`として作られるため、数値はmillion USD相当である。本分析では割り戻した値を`2017 USD PPP/person`として表記する。

### 16.3 図とCSV

- `01_main_effect_and_interaction/country_main_effect_vs_interaction`: Production、Producer price、Food intakeについて、保護区主効果とInteractionを同時に示す。バブル面積は人口、色は`111-011`の総効果である。
- `01_main_effect_and_interaction/country_protection_context_dumbbells`: CDRの有無によって保護区効果がどちらへ、どれだけ動くかを直接比較する。表示国は各指標で絶対効果が大きい15か国だが、選択前の全点はCSVにある。
- `02_environment_agriculture_tradeoffs/country_bii_agriculture_bubbles`: 横軸は農業結果、縦軸はBII変化、色はLUC排出削減である。生産者価格だけ符号を反転し、右ほど価格圧力が小さい向きに揃える。右上はBII便益と相対的に良好な農業結果が同時に現れるが、3農業指標を一つずつ確認する必要がある。
- `03_land_market_link/country_land_relocation_vs_market`: 横軸が国内の食料農地再配置量、縦軸がCRP生産、食料摂取、生産者価格の`111-011`である。色は食料農地純変化、バブル面積は人口である。
- `04_country_characteristics/country_import_dependence_vs_impacts`: `011`でのCRP輸入依存度と農業影響を比較する。
- `04_country_characteristics/country_income_trade_vs_impacts`: 一人当たりGDPを対数軸、色を輸入依存度として農業影響を比較する。
- `04_country_characteristics/country_association_heatmap`: 食料農地再配置、輸入依存度、貿易開放度、一人当たりGDP、BII変化と3農業影響のSpearman順位相関を示す。セル内に使用国数`n`を併記する。
- `country_continuous_scores_2100.csv`: BII便益、生産増、摂取減の小ささ、価格上昇の小ささ、Interaction絶対値を国間パーセンタイルにする。`agricultural_favourability_score`は生産・摂取・価格の等重み平均、`biodiversity_agriculture_score`はそれとBIIの等重み平均である。これは候補国を探索する索引であり、厚生指標や国分類ではない。

### 16.4 2100年結果から見える傾向

3農業指標のInteraction絶対値を連続パーセンタイル化すると、Belize、Poland、Solomon Islands、Jordan、Pakistanなどが上位に現れる。ただし、Interactionの符号と望ましさは指標ごとに異なる。例えば生産の負のInteractionはCDR下で保護区による生産低下が強まる方向だが、価格の負は価格上昇を弱める場合がある。国名だけで相乗効果と判断せず、主効果、Interaction、総効果の3つを同時に読む。

国別Spearman相関では、食料農地再配置量と農業3指標の相関は`-0.12`から`+0.05`、輸入依存度とは`-0.12`から`+0.14`、貿易開放度とはほぼ0である。大きな土地移動がそのまま大きな市場影響になるわけではなく、国内生産調整と貿易を含む一般均衡応答にかなり吸収されるという見方と整合する。ただし相関が弱いことだけで貿易の因果的な緩衝効果を確定できない。

一人当たりGDPとFood intakeの`111-011`には強い正の順位相関`rho=0.70`がある。この図では多くの国のFood intake効果が負なので、所得の高い国ほど低下が小さい傾向を意味する。一方、GDPとProductionは`rho=-0.19`、Producer priceとは`rho=0.03`であり、所得だけでは生産・価格応答を説明できない。BII変化と農業3指標の相関も絶対値0.12未満であるため、BII便益の大きい国が必ず農業負担の大きい国になるという単純な関係は見えない。

Ireland、Finland、Belarus、Guyana、Romaniaなどは、等重みのBII・農業連続スコアで上位に現れる。ただしこの順位は指標の選択と等重み仮定に依存する。論文上の「低トレードオフ」候補を決める際は、`country_bii_agriculture_bubbles`の3パネル、LUC排出削減、国別土地再配置を個別に確認し、必要なら研究上の優先度に合わせて重みを設定する。

相関は国間の記述統計であり、人口加重せず、外れ値を削除せずに計算する。欠損があるため比較ごとに使用国数が異なる。相関や連続スコアは因果機構を証明せず、次に国別地図、品目別結果、貿易相手の変化を掘り下げる候補を選ぶために使う。

## 17. `11_results_evidence_chain`: Resultsから結論までをつなぐ証拠

### 17.1 分析の目的と読み順

このフォルダは、個別指標を追加することより、論文の中心仮説を次の順に検証することを目的とする。

1. 保護区、植林、BECCSの導入で世界の土地配分が変わる。
2. 保護区があるとCDRと農業の配置が変わり、農地の量だけでなく土地生産性も変わる。
3. 緩和下の保護区効果`111-011`が、CDRなしの保護区主効果`100-000`から乖離する。
4. 流域土地圧力と農地の質の変化が、国別生産、貿易、価格、食料摂取へ波及する。
5. その農業影響と同時に、CDR実現量、LUC排出、保護面積、BIIがどう変わるかを確認する。

各図には探索段階で単独でも読めるよう、タイトル、比較式、データ上の制約を記載した。PNGの作図値は同じフォルダのCSVに保存する。

### 17.2 `01_global_land_system`: 世界の土地制約とCDR別応答

`world_exclusive_land_composition`は第14節で作成したMergedComparisonの重複しない7区分を使い、`000`、`100`、`011`、`111`の世界土地面積を積み上げる。Food cropland、2G bioenergy cropland、Fallow cropland、Pasture、Forest、Other natural land、Built-up areaの合計は報告された土地Totalに一致する。AfforestationはForestの内数なので積み上げには加えない。

`world_target_land_uses_by_scenario`は、Food cropland、Pasture、Afforestation、2G bioenergy croplandを、No policy `000`、Protection only `100`、Afforestation only `010`、BECCS only `001`、Both CDR `011`、Protection + both CDR `111`で比較する。点は独立したシナリオ水準であり、横方向を時間順序として結ばない。NoAff・NoBio状態により、植林とBECCSの土地応答の違いを確認する。

2100年のMergedComparisonでは、`111-011`によりAfforestation面積は約6.3%減る一方、2G bioenergy croplandは約3.9%、Food croplandは約0.9%増える。Food croplandの正の純変化は、AIM-LUグリッドで観察される大きな減少セルと増加セル、およびALPHAの作物別収穫面積変化を否定しない。MergedComparisonの土地被覆純量、グリッドのセル再配置、ALPHAの作物別収穫面積は異なる量なので分けて読む。

### 17.3 `02_productive_land_displacement`: 生産変化を面積・場所・収量へ分解

`all_scenario_World.gdx`の`qlBas_load`（作物別収穫面積、1000 ha）と`yldBas_load`（t/ha）を使う。対象はWheat、Rice、Maize、Other cereals、Oil crops、Sugar cropsであり、エネルギー作物`ecr`は含めない。比較は`000→100`、`000→010`、`000→001`、`000→011`、`011→111`、`000→111`である。

各作物・流域を (i)、比較前後の面積を (A_{0i}, A_{1i})、収量を (Y_{0i},Y_{1i})、比較前の面積加重平均収量を \(\bar{Y}_0\) とする。生産変化は次の3成分へ厳密に分ける。

\[
P_1-P_0=
\underbrace{\left(\sum_i A_{1i}-\sum_i A_{0i}\right)\bar{Y}_0}_{純面積量}
+\underbrace{\sum_i(A_{1i}-A_{0i})(Y_{0i}-\bar{Y}_0)}_{空間的生産性}
+\underbrace{\sum_i A_{1i}(Y_{1i}-Y_{0i})}_{収量応答}
\]

第一項は全収穫面積の純増減、第二項は同じ面積でも高収量・低収量流域のどちらへ移ったか、第三項は各流域の収量自体の変化である。各成分を比較前生産量で割ったpercentage contributionを`crop_production_mechanism_decomposition`に積み上げ、黒菱形で実際の面積×収量の総変化を示す。最大閉合誤差は`1e-10`千t未満である。

緩和策下で保護区を加えた`011→111`では、6作物すべての生産が減少する。総変化はOther cereals -1.29%、Wheat -1.12%、Rice -0.80%、Oil crops -0.67%、Sugar crops -0.48%、Maize -0.09%である。空間的生産性成分はOther cereals -1.34 percentage points、Wheat -1.03、Oil crops -0.72で、生産減を強める。一方、Rice +0.63、Sugar crops +0.81、Maize +0.27では配置変化が面積・収量影響の一部を補う。したがって「保護区が常に作物を低収量地へ押し出す」と一括せず、作物別に異なる配置応答を示す必要がある。

`crop_source_sink_yield_gap`は、面積減少流域をsource、増加流域をsinkとし、それぞれを減少・増加面積で加重した比較前収量を比べる。`011→111`のsinkはsourceよりOther cerealsで37%、Wheatで33%、Oil cropsで25%低い一方、Rice、Sugar crops、Maizeでは高い。これは第二項の向きを直感的に確認する診断であり、収量応答を含む実現生産量そのものではない。

### 17.4 `03_policy_interaction`: 保護区効果は単純な足し算か

`world_main_vs_context_protection_effect`の横軸は保護区主効果`100-000`、縦軸は緩和下の保護区効果`111-011`で、いずれも`011`基準の%である。45度線からの距離が、植林・BECCSを含むcombined interactionである。農業指標はAgMIP `CRP`、土地指標はMergedComparisonを使う。土地指標でも分子・分母を同じMergedComparisonから取得し、グリッド集計値とは混在させない。

`world_afforestation_beccs_interaction_components`はcombined interactionをProtection × afforestation、Protection × BECCS、Three-way interactionへ分ける。3成分の和は次式に一致する。

\[
I_{P,CDR}=(111-011)-(100-000)
\]

世界CRPではProductionの主効果が-0.29%、combined interactionが-0.37%で、緩和下の総保護区効果は約-0.66%となる。Food intakeでも主効果-0.24%に対しinteraction -0.60%、Producer priceでは+0.49%に対しinteraction +1.96%である。少なくともこれらの世界農業指標は、保護区効果をCDRなしの結果だけから近似すると過小評価する。土地側ではAfforestationの主効果が0なのは、`000`と`100`に植林政策がなく、比較対象となるAfforestation面積がないためである。`111-011`のAfforestation減少約6.3%は条件付き応答としてinteraction側へ入る。

### 17.5 `04_country_transmission`: 流域土地圧力から市場への波及

`country_land_market_evidence`は、ALPHAの流域・作物情報、`plum_data.gdx`の`protPlum`、AgMIPの国別CRP市場、AIM-LUグリッド再配置、BII、LUC排出を国コードで結合する。`protPlum`はREADME指定どおり`LandInt_global1`を使い、それ以外は`global2`である。

`publication`のFigure 2は、`data/InputData/`の0.5度保護区政策入力から作る追加保護割合`max(0, T_PROT − T_BAU)`（セル陸地に対する%）を保護圧力とし、10%以下をLow、10–30%をMid、30%超をHighに固定で分ける。流域別の保護面積（MergedComparisonの統合保護面積を`qlTot_load`の流域総面積で割った`111-011`）を使う版は`supplementary_figure_2_basin_protection.*`に残し、流域値は`supplementary_figure_2_basin_protection_pressure.csv`に保存する。定義の詳細は`publication/README.md`を参照。

国別の高収量農地損失割合は、作物ごとに流域収量の上位20%を定め、`011→111`で減った収穫面積のうち上位収量流域から失われた割合とする。sourceとsinkの収量比較では、作物間でt/haを直接平均しないよう、各作物の世界平均収量で標準化してから国集計する。

`country_land_drivers_vs_production`では、流域保護圧力とCRP生産変化のSpearman相関が-0.40、高収量流域からの損失割合とは-0.25、sink-source収量差とは+0.18である。保護圧力が高い生産流域、および高収量地からの損失が大きい国ほど、生産影響が悪化する傾向がある。ただし説明力は限定的で、線は因果推定ではない。

純輸入調整は次式で、AgMIP CRPのエネルギー量から計算する。

\[
100\times\frac{\Delta Imports-\Delta Exports}{Consumption_{011}}
\]

`country_production_trade_food_cascade`では、CRP生産変化と純輸入調整の順位相関が-0.83で、生産が減る国ほど純輸入を増やす明瞭な市場調整がある。一方、純輸入調整とFood intake変化の相関は+0.08に留まる。これは貿易調整が重要である一方、価格、所得、品目代替などを介するため、輸入増加だけで摂取影響が決まらないことを示す。

`country_multi_indicator_screen`は、保護圧力、高収量地損失、食料農地再配置、CRP面積・生産、純輸入、生産者価格、Food intake、BIIを横並びにする。セルの文字は実値、色は列内z-scoreである。表示国は複数指標の最大連続ランクから32か国を選ぶが、分類は行わず、全166か国は`country_land_market_evidence`に残す。

`r10_land_market_pathways`は世界と国の中間スケールとして、10地域について流域土地診断と市場応答を横並びにする。土地側はALPHA流域値をR10へ集約し、保護圧力は比較前作物生産量、高収量地損失は減少収穫面積で加重する。市場側は国別%の平均ではなく、`07_conditional_protection_decomposition`に保存したAgMIP R10集約値から`111-011`を直接計算する。純輸入調整だけはR10の輸入変化、輸出変化、`011`消費量を使って上式から求める。セル内は実値、色は指標ごとのR10内z-scoreなので、異なる単位のセル同士を色の大きさで定量比較しない。

地域差は大きい。Europeでは収穫面積損失10.7 Mha、CRP生産-11.2%、純輸入調整+6.9 percentage pointsで、保護区制約による生産減を輸入側で補う形が明瞭である。Rest of Asiaでも生産-3.1%、純輸入+2.3 pointsである。一方、Latin America、Pacific OECD、North AmericaではCRP生産が増える。Food intakeは全R10地域で減少しており、生産が増える地域でも国内摂取量の改善を意味しない。

`r10_protection_context_interaction`はCRP production、Producer price、Food intakeについて、灰色区間を主効果`100-000`、色区間をcombined interaction、白丸を緩和下の総効果`111-011`として示す。EuropeのCRP生産では主効果約+0.5%から総効果-11.2%へ反転し、interactionが-11.7 percentage pointsを占める。Rest of Asiaでも主効果約+0.4%から総効果-3.1%へ反転する。一方、Latin Americaではinteractionが生産を+2.6 points押し上げる。したがって、Interactionは全地域で一様に影響を増幅する項ではなく、CDRがあることで保護区効果が主効果からどちらへ変わったかを表す地域固有の差分である。

### 17.6 `05_climate_biodiversity_outcomes`: 政策目的側の結果

`world_climate_land_delivery`は、保護区を追加した`111/011-1`を、Afforestation面積、2G bioenergy cropland、Food cropland、MergedComparisonのAfforestation carbon removal、Hubの`QENE_hub`について示す。保護区によりAfforestation面積は約6.3%減るが、Afforestation carbon removalは約5.1%増える。面積と除去量が逆方向なので、植林の場所、タイプ、時間構造または単位面積除去量の変化を次に確認すべき結果である。2G bioenergy土地は約3.9%増えるが、Hub bioenergy demandは実質不変であり、外生需要を満たすため土地配置・面積が調整されるというモデル構造と整合する。`QENE_hub`の供給GDXには単位説明がないため、図では比率だけを使う。

`world_environmental_outcomes`は`111-011`について、追加保護面積約2,191 Mha、LUC排出削減約122 MtCO2/yr、面積加重世界平均BII変化-0.03 pointsを別単位のパネルで示す。現在のBII結果は世界平均でわずかに負であり、「保護区によりBIIが改善した」とは結論できない。国・セルでは正負が混在するため、BIIマップ、保護区の空間設定、BII再標本化、保護外への土地利用リーケージを確認する必要がある。この反証的な結果も、政策が本来目的を達成しているかを検証する重要な材料として保持する。

### 17.7 解釈上の境界

- 生産分解はALPHAの収穫面積×収量に対する恒等分解であり、AgMIP CRPのカロリーベース市場集計とは対象品目・集計方法が異なる。
- 空間的生産性成分はモデル内の流域収量を使う。気候・土壌だけの外生的potential yieldではなく、比較前シナリオの実現収量である。
- `protPlum`指標は同一流域の保護圧力であり、croplandとの直接重複ではない。直接交差を主張するには土地利用と保護マスクが同じグリッドで必要である。
- MergedComparisonの土地被覆純量、AIM-LU NetCDFのgross cell movement、ALPHAの作物別収穫面積を足し合わせない。それぞれ総量、場所、作物市場への入口を調べる別の診断である。
- 国別相関は人口加重せず、欠損国数が指標ごとに異なる。貿易の因果的な緩衝効果や、保護区配置の因果効果を単独では同定しない。
- AfforestationとBECCSの違いには、土地・炭素特性だけでなく、植林の内生的決定とHub bioenergy需要の外生性というモデル構造が含まれる。

## 18. `12_simple_overviews`: 最初に見る単純な時系列と国別関係

### 18.1 世界とR10の土地利用時系列

`world_land_use_trajectories_four_scenarios`はMergedComparisonの2020年以降を使い、No policy `000`、Protection `100`、Both CDR `011`、Protection + both CDR `111`の土地面積を折れ線で示す。対象はFood cropland、2G bioenergy cropland、Afforestation、Forest、Pasture、Other natural land、Integrated protected footprintである。色だけに依存しないよう線種も変え、面積規模が異なる土地利用を同時に読めるようパネルごとに縦軸を変える。AfforestationはForestの内数であり、Integrated protected footprintも土地被覆区分ではないため、これらの線を他の土地利用へ加算しない。

`world_target_land_trajectories_all_scenarios`はFood cropland、2G bioenergy cropland、Afforestationに絞り、NoAff・NoBioを含む8状態をすべて示す。最初に4シナリオ図で全体像を見てから、植林とBECCSを分ける必要がある場合にこの図を使う。

R10は土地利用ごとに1ファイルとし、10地域をfacetに分けた。各パネルの青線`011`と紫線`111`の差が、両CDR下で保護区を追加した影響である。縦軸は地域ごとに異なるので、線の距離は同じパネル内だけで比較し、地域間の絶対差は軸目盛または`land_use_trajectories_world_r10` CSVで確認する。

世界時系列では、Both CDRを入れるとFood croplandとPastureが2070年付近に大きく減り、2G bioenergy croplandとAfforestationが増える。そこへ保護区を追加した`111`では、2100年のFood croplandと2G bioenergy croplandが`011`よりやや大きく、Afforestationは小さい。したがって保護区はCDR需要そのものを単純に減らすというより、植林面積を抑えながらバイオ作物・食料農地の面積と配置を調整する結果になっている。

### 18.2 国別の単純な関係

国別図はバブルサイズと地域別多色を外し、国を同じ大きさの濃灰点、記述的なloess曲線を橙色で示す。縦軸はすべて、両CDR下で保護区を追加した`111-011`のAgMIP CRP production、Producer price、Food intakeである。各パネル左上にSpearman順位相関と使用国数を記載する。

`country_crop_basin_protection_vs_agriculture`の横軸は、`protPlum`の流域保護率増加を比較前の作物生産量で加重した0–100 percentage pointsの指標である。通常の線形軸を使う。生産変化との順位相関は約-0.40で、作物生産流域の保護圧力が大きい国ほど生産減が大きい傾向が最も読みやすい。Producer priceとの相関は約+0.18、Food intakeとはほぼ0であり、土地圧力から摂取量までが一対一に波及する関係ではない。

`country_total_cdr_land_vs_agriculture`は、保護区をまだ加えていないBoth CDRシナリオ`011`のAfforestation面積と2G bioenergy cropland面積を国土面積で割り、その合計を横軸にする。CDR土地率は0付近に多数の国が集中し最大60%程度まで広がるため、0を表示できるpseudo-log軸を使う。生産変化との順位相関は約-0.44で、CDR用地が多い国ほど、追加的な保護区制約による作物生産への影響が悪化する傾向がある。`country_afforestation_land_vs_agriculture`と`country_beccs_land_vs_agriculture`は同じ図をCDR別に分けたもので、植林とBECCSの違いを単純な横軸で確認する。

`country_direct_added_protection_vs_agriculture`はMergedComparisonが直接報告する追加保護面積の国土比を横軸にする。この値が正になるのは8か国に集中しており、そのうち日本はAgMIP CRPの比較基準が欠損して比率を計算できないため、散布図は7か国を表示する。`country_simple_relationship_data`には日本をNAのまま残す。この直接面積と、全体的な土地圧力を表す`protPlum`代理指標は別の量である。

### 18.3 土地区分の正負積み上げ

`world_scenario_land_change_stacks`は2100年の各シナリオを共通のNo policy `000`と比較する。Food cropland、2G bioenergy cropland、Fallow cropland、Forest、Pasture、Other natural land、Built-up areaについて、増えた面積を0より上、減った面積を0より下に積み上げる。各棒はProtection `100`、Afforestation `010`、BECCS `001`、Both CDR `011`、Protection + afforestation `110`、Protection + BECCS `101`、All three `111`の順である。

この積み上げには相互排他的な7区分だけを使う。AfforestationはForestの内数、Integrated protected footprintは土地被覆上のoverlayなので加えない。したがって、土地Totalがシナリオ間で一定なら各棒の正側合計と負側合計は一致する。世界・R10で確認した最大閉合残差は`9.3e-6` million ha未満である。

`r10_scenario_land_change_stacks`は同じ比較を10地域のfacetで示す。縦軸を地域ごとに変えるため構成は見やすいが、棒の高さを地域間で直接比較しない。絶対面積と閉合値は`land_class_changes_from_no_policy_world_r10` CSVに保存する。

`r10_scenario_land_change_stacks_share`は同じ変化を各地域のNo-policy土地Totalで割る。地域間で土地再配置の強度を比較するときはこちらを使い、世界への絶対寄与を調べるときはmillion ha版を使う。`r10_protection_under_cdr_land_change_stacks_share`も同様に、中心比較`111-011`を地域土地面積比で示す。

`r10_protection_under_cdr_land_change_stacks`は研究の中心比較`111-011`だけを取り出す。各地域を横棒1本とし、保護区追加により増えた土地を右、減った土地を左へ積み上げる。EuropeではFood cropland、Forest、Pastureが減り、Other natural landと2G bioenergy croplandが増える大きな再配置が見える。他地域も同じ符号規則で読める。正は面積増を意味し、便益を意味しない。

`world_scenario_land_change_stacks_over_time`は、各年のシナリオ値から同じ年のNo policyを引き、2020–2100年の正負積み上げを7つのシナリオ別パネルで示す。異なる年の絶対面積を直接引いていないため、BaU自体の経年変化ではなく、その年における政策差を表す。CDRを含むシナリオでは2070年頃から森林・2G bioenergy croplandの増加と、Pasture・Food croplandの減少が大きくなる。

`r10_protection_under_cdr_land_change_stacks_over_time`は、各年の`111-011`を10地域に分ける。Europeでは2060年代後半から土地再配置が急拡大し、Africa、China+、Latin Americaなどでも正負の構成が時間とともに変わる。facetごとに縦軸が異なるため、地域間の絶対量はCSVまたは2100年の共通軸図で確認する。

## 19. `13_agmip_area_yield_decomposition`: 生産変化の収穫面積・収量分解

### 19.1 目的と使用データ

このフォルダは、「保護区を加えるとFood croplandが増えるのに、作物生産が減るのは、低収量地域への移転によってより多くの土地が必要になるためか」という問いを、最も単純な会計関係から確認する。中心比較は、植林とBECCSがすでに入った状態へ保護区を追加する`111-011`である。

農業側には`data/ALPHA/AGMIP_all_scenario.gdx`の`AGMIP`（国）と`AGMIP_agg`（R10・世界）を使う。対象品目`CRP`は非エネルギー作物のAgMIP集約であり、エネルギー作物`ECP`は含めない。使用変数と単位は次のとおりである。

| 量 | AgMIP変数 | 元単位 | 図での単位・処理 |
|---|---|---|---|
| 収穫面積 (A) | `AREA` | `1000 ha` | そのまま使用 |
| 生産量 (P) | `PROD` | `million kcal` | `4.184e-6`を掛けて`PJ/yr` |
| 実効カロリー収量 (Y) | 上記から計算 | - | (1000P/A)、`GJ/ha` |

ALPHAのAgMIP出力定義では、品目別`AREA`は`qlBas_load`の合計、物理収量`YILD`は`yldBas_load`を`qlBas_load`で加重した平均、`PROD`は`yldBas_load * qlBas_load`の合計である。このため個別品目では`PROD = AREA * YILD`が数値精度内で成立する。実際に世界・R10のWHT、RIC、CGR、OSD、SGCについて`011`と`111`を直接検算し、最大相対残差は`1.44e-16`だった。

CRPは熱量係数が異なる複数作物を含むため、物理単位`t/ha`の収量を単純平均しない。ここではカロリーベースの`PROD`を同じCRPの`AREA`で割った実効カロリー収量を使う。したがってCRPの実効収量変化には、各場所・作物の収量変化に加えて、作物構成と空間配置の変化も含まれる。

土地被覆との橋渡しには、`output/08_land_use_protection/02_protection_displacement/protection_land_displacement_all_levels.csv`のMergedComparison `Food cropland`を使う。これはAIM-PLUMの物理的な土地被覆面積である。AgMIP `AREA`は収穫面積なので、多期作、休耕、モデル内集約の違いにより両者は一致しない。この2種類の面積を足したり、同じ面積として差し引いたりしない。

### 19.2 面積寄与と実効収量寄与の式

比較前後を0、1とし、(P_0=A_0Y_0)、(P_1=A_1Y_1)とする。面積を先に変える逐次分解は順序によって交差項の帰属が変わるため、次の対称分解を使う。

\[
\Delta P_A=(A_1-A_0)\frac{Y_0+Y_1}{2}
\]

\[
\Delta P_Y=(Y_1-Y_0)\frac{A_0+A_1}{2}
\]

\[
P_1-P_0=\Delta P_A+\Delta P_Y
\]

これは面積と収量の交差項を半分ずつ配る2要因Shapley分解と同じである。各寄与を比較前生産量(P_0)で割り、percentage pointsで積み上げる。黒い菱形は実際の生産変化率(100(P_1/P_0-1))で、2色の棒の合計に厳密に一致する。世界、R10、国、世界時系列の最大閉合誤差は`2.1e-11 PJ/yr`未満である。

`world_crp_area_yield_decomposition`は、Protection without CDR `100-000`、Afforestation `010-000`、BECCS `001-000`、Both CDR `011-000`、Protection with both CDR `111-011`、All policies `111-000`の6比較を同じ形式で示す。`world_crp_area_yield_decomposition_over_time`は、中心比較`111-011`を2020年から対象年まで10年ごとに分解する。

### 19.3 世界の中心結果

2100年に両CDR下で保護区を追加した`111-011`では、世界CRPは次のように変わる。

| 指標 | 変化 |
|---|---:|
| MergedComparison Food cropland | +0.852% |
| AgMIP CRP収穫面積 | +0.062% |
| AgMIP CRP実効カロリー収量 | -0.720% |
| AgMIP CRPカロリー生産 | -0.659% |
| 生産変化への収穫面積寄与 | +0.061 percentage points |
| 生産変化への実効収量寄与 | -0.720 percentage points |

この符号関係は、保護区追加後に収穫面積をわずかに増やしても、平均的に1 haから得られるカロリーが低下し、生産減を防げないことを示す。その意味では「より多くの面積を必要とする」という仮説と整合する。ただし、CRP実効収量低下だけから、農地が物理的に低収量地域へ移ったことをすべて説明したとは言えない。実効収量には、同じ場所での収量応答、作物構成、空間移転がすべて含まれるためである。

低収量地域への移転を直接調べるには、`output/11_results_evidence_chain/02_productive_land_displacement/`のALPHA流域分解を併用する。そこでは生産変化を純面積量、比較前収量を用いた空間的生産性、流域内の収量応答へ分けている。`011→111`では、Other cereals、Wheat、Oil cropsの移転先収量が移転元よりそれぞれ約37%、33%、25%低く、空間的生産性成分も負である。一方、Rice、Sugar crops、Maizeでは空間成分が正なので、すべての作物を「低収量地へ追い出された」とまとめない。

### 19.4 主要作物、R10、国の読み方

`world_major_crop_area_yield_decomposition`は`111-011`をWheat、Rice、Coarse grains、Oilseeds、Sugar cropsへ分ける。Wheatは収穫面積+0.54%に対して実効収量-1.80%で、生産は-1.27%。Coarse grainsは面積+0.98%、実効収量-1.34%、生産-0.37%である。Riceは面積-0.04%、実効収量-0.70%、生産-0.74%。Oilseedsは面積+0.02%、実効収量-0.46%、生産-0.43%。Sugar cropsは面積-1.01%を実効収量+1.06%がほぼ相殺し、生産+0.04%となる。主要財でも機構は同じではない。

`r10_crp_area_yield_decomposition`では地域差が明瞭である。Europeは面積寄与-9.59 points、実効収量寄与-1.59 pointsで生産-11.18%。Africaは面積寄与+0.24 pointsを収量寄与-1.54 pointsが上回り、生産-1.30%。China+は面積寄与+2.20 points、収量寄与-1.19 pointsで生産+1.02%。Latin Americaも面積寄与+3.67 points、収量寄与-0.80 pointsで生産+2.87%となる。このため、世界の小さな純変化は地域間の大きな増減を隠している。

`r10_food_land_harvested_area_yield_chain`は各地域について、Merged Food cropland、AgMIP harvested area、実効収量、生産の4点を同じ横軸へ置く。線は4診断の範囲を見やすくする補助であり、時系列ではない。特にEuropeではFood cropland-8.53%、収穫面積-9.67%、生産-11.18%だが、Middle EastではFood cropland-11.13%に対して収穫面積+1.67%である。土地被覆と収穫面積は地域によって反対方向にも動く。

`country_crp_area_yield_decomposition_top25`は`111-011`のCRP生産変化量の絶対値が大きい25か国を選び、PJ/yrで分解する。割合の大きさだけで小規模生産国が上位になることを避けるため、選択にも表示にも絶対生産変化を使う。全ての国・全6比較の面積、収量、生産、2寄与、閉合誤差は同フォルダのCSVに残す。

### 19.5 この分解で言えることと言えないこと

- 面積寄与が正で収量寄与が負なら、収穫面積拡大が生産を支える一方、平均的なカロリー生産性低下が生産を押し下げたと読める。
- 実効収量寄与は会計上の寄与であり、低収量地への空間移転だけの寄与ではない。空間移転はALPHA流域の3成分分解、grossな場所の移動はAIM-LUグリッドのsource/sink図で確認する。
- CRP集約は作物構成変化を含む。個別作物図ではこの混合が小さくなり、物理`YILD`の変化により近い。
- Merged Food croplandは土地被覆、AgMIP AREAは収穫面積である。前者+0.85%と後者+0.06%の差を、未使用地や低収量地の量として直接解釈しない。
- 2寄与の和は生産変化を厳密に説明するが、保護区、植林、BECCSという政策別寄与ではない。政策別寄与は6比較または`07_conditional_protection_decomposition`を使う。

## 20. `14_luc_emissions_balance`: LUC排出・吸収・ネットの累積と経年変化

### 20.1 使用する変数と符号

`data/PLUM/Mergedcomparison.gdx`の`GHGL`を使う。次元はシナリオ、地域、年、排出区分、土地区分であり、単位は`Mt CO2/yr`である。土地区分は集約済みの`LUC`を選び、排出区分を次のように使う。

| GHGL排出区分 | 図での意味 | 符号 |
|---|---|---:|
| `Positive` | 吸収を差し引く前のgrossな土地利用変化由来排出 | 正 |
| `Negative` | 森林成長、植林、土壌炭素管理等による元のgrossな吸収 | 負 |
| `Net` | 元の`Positive + Negative` | 正負 |

元の`Negative/LUC`にはManaged forest `MNGFRS`が含まれるが、管理林の炭素吸収をCDR・LUC吸収へ算入することには議論があるため、主図から除外する。計算は次のとおりである。`MNGFRS`自体が負値なので、元の負の吸収から負値を引くことで、絶対値が小さい調整後吸収になる。

\[
Negative_{excl.MF}=Negative_{reported}-MNGFRS
\]

\[
Net_{excl.MF}=Positive+Negative_{excl.MF}
\]

2色版の橙色は`Positive`、青色は`Negative_excl.MF`、黒菱形は`Net_excl.MF`である。ソース別版も同じ境界を使い、Managed forestを積み上げない。元の`Negative_reported`、`Net_reported`、除外した`managed_forest_removal_mtco2_yr`は`luc_emissions_annual_balance` CSVに残す。ここでいうLUCはMergedComparisonに保存されたAIM-PLUMの土地利用炭素勘定であり、エネルギーシステム側で回収されるBECCSのCO2は含まれない。BECCSシナリオでも土地配置や森林・土壌フローが変わるため、LUCの正負フロー自体は変化する。

世界は`World`、地域はGDXに直接保存された10個のR10集約を使う。国値を後からR10へ足し上げていない。シナリオは`000`、`100`、`010`、`001`、`110`、`101`、`011`、`111`の8状態すべてである。

### 20.2 2020–2100年累積値

GHGLは2020、2030、…、2100年について年率を報告する。隣接するモデル年の間で年率が直線的に変わると仮定し、正の排出と負の吸収を別々に台形則で積分する。年率を(E_t)、隣接年を(t,t+1)とすると、累積量は次式である。

\[
C=\sum_t (Y_{t+1}-Y_t)\frac{E_t+E_{t+1}}{2}
\]

積分後の`Mt CO2`を1000で割り、`Gt CO2`で示す。これは2020年から2100年までの80年間の累積であり、2020年以前の歴史排出は含まない。単純に9個のモデル年の値を足したものでも、2100年値を80倍したものでもない。

`world_cumulative_luc_balance_2020_2100`は世界の8シナリオを2色で横に並べる。`world_cumulative_luc_balance_by_source_2020_2100`は同じ値を排出源・吸収源別に積み上げる。`r10_cumulative_luc_balance_2020_2100`と`r10_cumulative_luc_balance_by_source_2020_2100`も同じ2種類をR10別facetで示し、地域ごとに縦軸を変える。棒の正側がgross emission、負側がManaged forest除外後のgross removal、黒菱形が両者の累積ネットである。

世界累積値は次のとおりである。

| シナリオ | 正の排出 | 負の吸収 | ネット |
|---|---:|---:|---:|
| No policy `000` | +55.1 | -15.3 | +39.8 |
| Protection `100` | +53.4 | -14.9 | +38.5 |
| Afforestation `010` | +30.6 | -141.0 | -110.3 |
| BECCS `001` | +29.2 | -41.4 | -12.3 |
| Protection + afforestation `110` | +31.1 | -149.8 | -118.7 |
| Protection + BECCS `101` | +29.5 | -42.3 | -12.8 |
| Both CDR `011` | +35.8 | -139.2 | -103.4 |
| All three `111` | +37.5 | -147.0 | -109.5 |

単位はいずれも2020–2100年累積`Gt CO2`である。Managed forestを外すと、No policyとProtectionのみは正味排出、植林またはBECCSを含むシナリオは正味吸収になる。排出削減政策の効果を調べる場合は、各シナリオのネット水準だけでなく、共通基準`000`との差も計算する。また、この値をエネルギー・産業・農業非CO2を含む全GHG収支として解釈しない。

### 20.3 10年ごとの経年変化

`world_decadal_luc_balance_to_2100`は8シナリオをfacetに分け、各モデル年の`Positive`、調整後`Negative`、再計算`Net`を2色で`Mt CO2/yr`表示する。`world_decadal_luc_balance_by_source_to_2100`は正側・負側をソース別に積み上げる。これは累積量ではなく、その年に対応する年間フローである。

R10は10地域×8シナリオを1枚に詰め込まず、シナリオごとに2色版`r10_decadal_luc_balance_STATE_to_2100`とソース別版`r10_decadal_luc_balance_by_source_STATE_to_2100`を出力する。各ファイルの10個のfacetがR10地域で、縦軸は地域別に変える。シナリオ間を比較するときは同じ地域の軸目盛も確認する。2色版の元値は`luc_emissions_annual_balance`、ソース別版は`luc_emissions_source_balance_annual` CSVに保存する。

### 20.4 負の吸収には何が含まれるか

`GHGL Negative/LUC`の内訳を確認するため、集約済み`FRS`や`GL`を重ねて足さず、次の詳細区分を一度ずつ使う。

| GHGL土地区分 | 図の名称 | 主な意味 |
|---|---|---|
| `AFR` | Afforestation | 植林地の成長による吸収 |
| `NRFABDCUM` | Natural forest regrowth | 自然林の再生・回復 |
| `NRGABDCUM` | Natural grassland recovery | 自然草地の回復 |
| `CL` | Cropland stock gain | 農地への土地転換に伴う炭素ストック増加 |
| `CROP_FLW` | Cropland soil management | 明示的な農地土壌炭素管理 |
| `PAS` | Pasture | 牧草地の負の炭素フロー |
| `SL`, `OL` | Settlement/Other land | 都市・その他土地の負のフロー |

元の負側の森林集約`FRS`は`AFR + MNGFRS + NRFABDCUM`だが、主図ではこのうち`MNGFRS`を除く。`AFR + NRFABDCUM`、`NRGABDCUM`、`CL`、`CROP_FLW`、`PAS`、`SL`、`OL`の和は調整後`Negative_excl.MF`へ厳密に一致する。詳細区分と集約`FRS`、`GL`を同時に足すと二重計上になる。

`world_cumulative_luc_removal_sources`と`r10_cumulative_luc_removal_sources`は累積吸収源だけの内訳、`world_decadal_luc_removal_sources`は10年ごとの世界吸収源だけの内訳である。All three `111`の世界累積では、Afforestation -97.8 Gt、Natural forest regrowth -33.9 Gt、Cropland soil management -5.4 Gt、Cropland stock gain -4.8 Gt、Natural grassland recovery -3.7 Gt、Pasture -1.5 Gt CO2で、合計-147.0 Gt CO2となる。

主図のソース別積み上げでは、正側をDeforestation `DEF`、Grassland degradation `DEG`、Cropland-related emissions `CL + CROP_FLW`、Pasture and other emissions `PAS + SL + OL`へ整理する。負側は上表の区分を使う。正側・負側の全ソースの和は、Managed forest除外後の再計算ネットへ一致する。

### 20.5 検算

次の3点を全8シナリオ、世界・R10、全モデル年で検算した。

- 元の報告`Net`と元の`Positive + Negative`の最大差は`2.73e-12 Mt CO2/yr`。
- Managed forestを除いた詳細吸収源の和と調整後`Negative`の最大差は`1.82e-12 Mt CO2/yr`。
- 正負のソース別積み上げ合計と調整後`Net`の最大差は`2e-12 Mt CO2/yr`未満。
- R10の調整後正・負・ネットをそれぞれ合計した値と`World`の最大差は`2.1e-12 Mt CO2/yr`未満。

数値は`luc_emissions_balance_validation_2100.csv`に保存する。これらは集約と符号の閉合確認であり、炭素フロー推計自体の不確実性を評価する検証ではない。

### 20.6 Protectionの排出寄与が小さい理由と植林吸収の時間差

`world_protection_luc_source_effects_2020_2100`は、Protectionを追加する4経路、`100-000`、`110-010`、`101-001`、`111-011`について、2020–2100年累積LUC変化をソース別に示す。負は排出削減または吸収増、正は排出増または吸収減で、黒菱形がManaged forest除外後のネットProtection効果である。

CDRなしの`100-000`では、正の排出は1.68 Gt CO2減るだけである。内訳はGrassland degradation -1.42 Gt、Deforestation -0.28 Gtが中心で、農地・牧草地側の小さな増加が一部を相殺する。R10ではAfricaの正の排出が-1.92 Gt減る一方、China+、Rest of Asia、Reforming economiesなどでは増え、世界合計で相殺される。

この結果には三つの理由が考えられる。第一に、保護面積全体がavoided conversionではなく、BaUで転換される予定だった土地と新規保護区が重なった部分だけが追加的な回避排出になる。第二に、食料・土地需要は保護区導入で消えず、保護区域外へ再配置される。AIM-LUグリッドでは`100-000`でFood croplandが約103 Mhaのセルから減る一方、別セルで約100 Mha増えており、grossな場所の移動に対して世界純量は小さい。第三に、炭素排出は面積だけでなく転換前後の炭素密度に依存する。保護面積が大きくても、BaUで転換圧力が小さい土地や炭素損失の小さい土地を保護した場合、`Positive/LUC`は大きく減らない。

緩和策下の`111-011`では、Protectionによりgross positive emissionsは逆に+1.68 Gt増える。Deforestation +1.07 Gt、Cropland-related emissions +0.34 Gtなどが増えるためで、保護区域外への土地再配置と整合する。一方、Afforestation removal -5.82 Gt、Natural forest regrowth -1.84 Gtなど負の吸収が増え、ネットでは-6.13 Gt改善する。したがってProtectionは常に土地転換排出を直接減らす対策として働くのではなく、土地配置と吸収場所・時期を変える条件として働いている。

`world_afforestation_area_and_sink_010_110_to_2100`は、Afforestation `010`とProtection + afforestation `110`の植林面積と`AFR`吸収フローを上下に並べる。2100年だけでは`110`の植林面積が`010`より5.12 Mha少ないが、2030–2080年は`110`の方が多く、2050–2070年には約10–12 Mha多い。植林吸収フローも2030年以降`110`の方が概ね負に大きい。

PLUMの植林吸収は、その年の植林面積へ一定係数を掛けた値ではない。植林年ごとの面積変化と、グリッド・樹齢別炭素フロー`CFT(G, Forestage)`を用い、過去に造成された植林コホートの吸収を各年に計上する。このため、Protectionにより植林が早期へ前倒しされた場合、2100年の累積植林面積が小さくても、2020–2100年累積吸収は増えうる。`110-010`では植林吸収が-7.50 Gt、自然林再生が-1.60 Gt増え、正の排出+0.50 Gtを差し引いてもネットは-8.34 Gt改善する。場所別の炭素生産性の変化も含まれるため、最終的には植林コホート、森林タイプ、グリッド別`CFT`を確認する必要がある。

### 20.7 2070年頃に土地転換排出が増える理由

`05_2070_spike_diagnostics/`は、緩和シナリオで2070年に現れる正の土地転換排出の山を、報告時点、排出源、土地面積変化、Hubのバイオエネルギー需要、R10地域に分けて確認する。

ここで最初に注意すべき点は、`GHGL`の2070年値が2070年の瞬間的な土地転換だけを表すのではないことである。PLUMでは2010年以降の`Ystep`が10年であり、森林減少`DEF`、草地劣化`DEG`、炭素ストックを持つ土地カテゴリーの減少は、前期からの面積・ストック変化を`Ystep`で割って年率化する。したがって2070年の`Mt CO2/yr`は、基本的に2060–2070年に生じたgrossな土地転換を10年平均の年率として計上した値である。

`world_positive_luc_sources_2060_2070_2080`は、植林、BECCS、両CDRと、それぞれへProtectionを加えた6シナリオについて、2060、2070、2080年の`Positive/LUC`をソース別に比較する。Both CDR `011`の正の排出は、2060年279 Mt CO2/yr、2070年942 Mt CO2/yr、2080年316 Mt CO2/yrである。2070年の内訳はGrassland degradation 499、Cropland-related emissions 170、Deforestation 145、Pasture and other emissions 128 Mt CO2/yrで、最大の要因は草地減少である。All three `111`では合計1,041 Mt CO2/yrまで増え、Protectionを加えてもこの土地需要と再配置は消えない。

`world_land_change_around_2070`は、2060–2070年と2070–2080年の世界土地面積変化を比較する。Both CDR `011`では前半の10年間に2G bioenergy croplandが+151 Mha、Forestが+159 Mha増える一方、Food croplandは-66 Mha、Pastureは-107 Mha、Other natural landは-132 Mha減る。後半の10年間では、それぞれ+40、+48、+7、-24、-66 Mhaであり、前半の再配置が明確に大きい。AfforestationはForestの内数なので積み上げに重ねず、別の時系列図で示す。

`world_cdr_scaleup_and_positive_luc_to_2100`は、Hubの`QENE_hub`、2G bioenergy cropland、Afforestation面積、正のLUC排出を時系列で並べる。`QENE_hub`の物理単位は供給GDXで確認できないため、各シナリオの2060年を1とする指数にした。BECCSを含むシナリオでは、需要指数が2060年の1から2070年に約6.2へ増える。Both CDRでは同じ区間に2G bioenergy croplandが29 Mhaから180 Mha、Afforestationが150 Mhaから253 Mhaへ増える。植林だけの`010`でもAfforestationが166 Mhaから265 Mhaへ増え、正のLUC排出は290から486 Mt CO2/yrへ上がる。したがって2070年の山はBECCSだけに固有ではなく、2060–2070年の土地型CDRの急速な拡大と、それに伴うgrossな土地再配置に対応する。BECCSを含む場合は外生バイオエネルギー需要の急増が重なり、山がより大きい。

このときAfforestationそのものを正の排出源と解釈してはいけない。Afforestationは負の吸収として別途計上される。正の山は、植林・バイオ作物を拡大する過程で、別セル・別地域において草地、森林、農地等の減少や転換が同時に起きるためである。世界のForest純面積が増えていても、あるセルでの森林増加と別セルでの森林減少は相殺されるため、grossなDeforestation排出は残る。同様に草地減少が最大の排出源であることは、CDR用地と農業を確保する土地再配置が主として草地側に波及したことを示す。

Managed forestを除いたネットでは、Both CDR `011`は2060年-1,687、2070年-1,351、2080年-2,236 Mt CO2/yrであり、2070年にも吸収超過だが一時的に吸収幅が縮む。All three `111`も同じ順に-1,788、-1,484、-2,410 Mt CO2/yrである。一方、Afforestationを持たないBECCS-only `001`は-274、+153、-446 Mt CO2/yrとなり、2070年だけLUC勘定が排出超過になる。BECCS回収量はこのPLUM LUC勘定の外にあるため、この値だけでシナリオ全体の気候効果を判断しない。

`r10_2070_positive_luc_excess_by_source_011`は、Both CDRについて、2070年値から2060年と2080年の平均を引いた「一時的な山」をR10別に示す。寄与が大きいのはEurope 198 Mt CO2/yrとAfrica 152 Mt CO2/yrで、両地域ともGrassland degradationが中心である。North AmericaではDeforestationとCropland-related emissions、Rest of AsiaではGrassland degradation、Deforestation、Cropland-related emissionsが複合する。この隣接時点との差は山の場所と中身を読みやすくする記述統計であり、政策効果の因果分解ではない。

以上から、現段階で最も整合的な説明は「緩和策が2070年に突然排出的になる」のではなく、「2060–2070年に土地型CDRを急拡大する際の土地転換パルスが2070年の年率として報告される」である。正の土地転換排出と植林・自然再生による負の吸収は同時に起こりうるため、気候効果の判断には`Positive`だけでなく、Managed forestを除いた`Negative`と再計算した`Net`も併読する。需要と面積の時期的対応はこの機構を支持するが、どのCDR用地がどの土地被覆を直接転換したかを完全に追跡する遷移行列ではない。

## 21. `15_spatial_reallocation_bivariate`: 保護区による土地再配置の二変量地図

### 21.1 目的と中心比較

このフォルダは、結果2「保全地域の拡大によって土地利用はどのように再配置されるか」を空間的に示す。中心比較は、植林とBECCSがすでに導入された緩和シナリオへProtectionを追加する`111-011`である。土地利用には`data/PLUM/nc/AIM-LUmap_<scenario>.nc`の`LC_area_share`を使い、Food cropland（クラス1）、2G bioenergy cropland（クラス2）、Planted forest（クラス5）を対象年の0.5度セルで比較する。

### 21.2 Protectなし・あり・差分の直接比較

`01_direct_comparison/`は、各土地用途について次の3列を同じ図にする。

1. Both CDR、Protectなし`011`の土地面積シェア
2. Protection + both CDR、Protectあり`111`の土地面積シェア
3. セル差分`111-011`

最初の2列は土地用途ごとに共通の色範囲を使う。3列目は赤を減少、青を増加とし、保護区追加により失われた場所と別の場所で増えた土地を示す。表示範囲は、絶対分布ではProtectなし・ありを合わせた99パーセンタイル、差分では非ゼロ絶対値の98パーセンタイルとする。範囲外は端色へ丸めるが、GeoTIFFと集計値は切り詰めない。

グリッド集計では、Food croplandは減少セル225 Mha、増加セル215 Mha、純変化-10 Mha、2G bioenergy croplandは減少128 Mha、増加129 Mha、純変化+0.9 Mha、Planted forestは減少312 Mha、増加243 Mha、純変化-69 Mhaである。同一用途の`min(増加, 減少)`を探索的な再配置量とすると、それぞれ215、128、243 Mhaとなる。これはセル間のgrossな増減であり、ある減少セルから特定の増加セルへ土地が移った経路を追跡した値ではない。

### 21.3 元の土地集中度とProtection応答の3×3二変量地図

`02_baseline_response_bivariate/land_reallocation_bivariate_maps`は、横方向の色の濃さをProtectなし`011`における土地面積シェア、縦方向の色相をProtection追加後の応答`111-011`とする。元の土地集中度は、正の`011`セルを土地用途別に3分位し、Low、Medium、Highに分ける。土地応答は次の共通基準で分ける。

- Decrease：`111-011 < -0.1 percentage points`
- Stable：絶対差が`0.1 percentage points`以下
- Increase：`111-011 > +0.1 percentage points`

3×3凡例では、赤が減少セル、灰色が安定セル、青が増加セルである。同じ色相の濃いセルほど、Protectなしの時点でその土地用途が多かった。したがって、濃い赤は主要な既存立地からの縮小、薄い青は元々少なかった場所での新規拡大候補として読める。個別の連続差分図より値の細部は失うが、「どこから減り、どのような場所で増えたか」を一枚で把握しやすい。

`04_data_and_matrices/baseline_response_area_matrices`は同じ9分類を面積表にし、各セル上段に該当する地理的セル面積、下段にその範囲内の土地用途純変化を示す。地図の色だけに依存せず、各クラスがどれだけの面積を代表するかを確認できる。分類コードと連続差分はGeoTIFFにも保存する。

### 21.4 保護圧力と土地応答を組み合わせた図2候補

`03_protection_response_bivariate/figure2_candidate_protection_land_reallocation`は、研究の説明順に合わせた4面図である。Aは流域総面積に対する追加保護面積割合（% of basin land）、BはPlanted forest、Cは2G bioenergy cropland、DはFood croplandの応答を示す。B–Dの3×3色は、横軸が追加保護面積割合Low–High、縦軸が土地用途のDecrease–Stable–Increaseである。濃い赤は高保護圧力流域での縮小、薄い青は低保護圧力流域での拡大候補を表す。

ただし、供給されたAIM-LU NetCDFには0.5度の保護区マスクが含まれない。そこで保護側には`all_scenario_World.gdx`の`qlProt_load`と`qlTot_load`を使う。`qlProt_load`のprimary forest `primf`、secondary forest `secdf`、grassland `gra`、pasture `pastr`、range `range`を合計し、流域総面積で割った`111-011`を追加保護面積割合として作る。この流域値をAIM-LUの0.5度格子へ割り当て、正の値の3分位でLow、Medium、Highに分ける。

したがって、この図から「この0.5度セルが新規保護区と直接重なった」とは言えない。高圧力流域で土地が減る場所、同じ流域内または低圧力流域で土地が増える場所を探索する図である。現結果ではFood croplandについて、高圧力流域の減少セルで-139 Mha、高圧力流域の増加セルで+107 Mha、中圧力流域では-73 Mhaと+100 Mhaが同時に生じる。単純な高圧力流域から低圧力流域への移転だけではなく、同一の中・高圧力流域内でも細かな再配置が起きている。

図を論文用に絞る場合は、まず`figure2_candidate`で保護圧力と3用途の全体像を示し、本文または補足でFood cropland、2G bioenergy、Planted forestの`without_with_difference`を使う構成が候補になる。厳密な直接重複を主張する場合は、PLUMから`protectfrac`または`protectfracL`のグリッド出力を追加し、現在の流域代理指標を置き換える必要がある。

### 21.5 農地・バイオ・植林の転換信号

`05_cropland_transition_signals/`、`07_bioenergy_transition_signals/`、`08_planted_forest_transition_signals/`は、Protection追加後の`111-011`について、Food cropland、2G bioenergy cropland、Planted forestを一つずつ出発用途として同じ会計を行う。各出発用途について、次の4信号を0.5度セルで計算する。

1. 出発用途の減少と、他の管理土地用途1の増加が同時に起きた量
2. 出発用途の減少と、他の管理土地用途2の増加が同時に起きた量
3. 出発用途が新たに増加した量
4. 他の2用途の増加と対応しない出発用途の減少量

土地面積シェアのNetCDFには、個々の土地筆がどの用途からどの用途へ移ったかを示す遷移行列がない。そのため、出発用途の減少量を、同一セルの他の2用途の増加比率で按分する。セル (i) の出発用途減少を (L_i)、他用途の増加を (G_{i,1})、(G_{i,2}) とすると、対応付ける減少は次式で上限を置く。

\[
M_i=\min(L_i,G_{i,1}+G_{i,2})
\]

\[
C_{i,1}=M_i\frac{G_{i,1}}{G_{i,1}+G_{i,2}},\qquad
C_{i,2}=M_i\frac{G_{i,2}}{G_{i,1}+G_{i,2}}
\]

未対応減少は (U_i=L_i-M_i) とする。これにより、同じ減少を2つの到着用途へ二重計上しない。灰色の未対応減少は、Protectionによる自然地維持、自然地への転換、ここで扱わない別土地用途への転換、セル内で相殺されない純減少などを含みうる。現在のNetCDFに直接の保護区マスクと土地遷移行列がないため、灰色をそのまま「保護区による自然地維持面積」とは解釈しない。

連続強度版の`*_transition_signals_four_panels`は4信号を個別に示す。`dominant_*_transition_signal`はセルごとに最大の信号を、Food croplandは青、Bioenergyは橙、Planted forestは緑、未対応減少は灰色で示す。色の濃さは各信号内の98パーセンタイルまでの大きさに対応する。Stableは非常に淡い灰色にして、未対応減少の灰色と区別した。

`major_*_transition_classes`はグラデーションを使わない見やすさ重視の離散版である。大きな変化をセル面積シェア10 percentage points以上と定義する。出発用途減少と、代表させる到着用途増加がともに10 percentage points以上で、その用途が2用途の増加合計の60%以上なら、その用途を代表遷移とする。対応量は10 percentage points以上だが代表条件を満たさない場合はMixed、未対応減少が10 percentage points以上かつ対応済み減少以上ならOther decline、出発用途の増加が10 percentage points以上ならExpansion、差が±1 percentage point以内ならStableとする。

2100年の連続信号面積は次のとおりである。行内の2つの対応信号と未対応減少の合計が、その出発用途の全減少に一致する。

| 出発用途 | 到着用途1への対応 | 到着用途2への対応 | 未対応減少 | 出発用途の増加 |
|---|---:|---:|---:|---:|
| Food cropland | Planted forest 35.1 Mha | Bioenergy 37.0 Mha | 152.8 Mha | 214.7 Mha |
| Bioenergy cropland | Food cropland 31.8 Mha | Planted forest 9.9 Mha | 86.3 Mha | 128.8 Mha |
| Planted forest | Food cropland 59.1 Mha | Bioenergy 26.3 Mha | 226.5 Mha | 243.4 Mha |

離散分類のセル面積では、Food croplandのOther declineが356.8 Mha、BioenergyのOther declineが207.7 Mha、Planted forestのOther declineが351.0 Mhaである。これは該当グリッドセルの地理的面積であり、表の連続信号面積とは異なる。すべての連続信号、離散分類、閾値、セル面積は`04_data_and_matrices/`のCSVとGeoTIFFへ保存している。

### 21.6 Protectionで維持された森林・草地と別地域の追加減少

`06_avoided_vegetation_loss/`は、AIM-LUのモデル基準年2005年から対象年までに減少した植生を、Protectなし`011`とProtectあり`111`で比較する。2005年の両シナリオはセル単位で完全に一致する。対象クラスは、BtC2の土地クラス対応表に基づくNaturally regenerating forest（クラス4）、Managed grassland / pasture（クラス3）、Natural grassland + other natural land（クラス9）である。クラス9は自然草地`GL`とその他自然地`OL`の合計なので、このNetCDFだけでは自然草地を単独に分離できない。

2005年の土地面積シェアを (V_{i,2005})、対象年の011と111を (V_{i,011})、(V_{i,111}) とすると、各セルの減少は次式で計算する。

\[
L_{i,011}=\max(V_{i,2005}-V_{i,011},0),\qquad
L_{i,111}=\max(V_{i,2005}-V_{i,111},0)
\]

\[
E_i=L_{i,011}-L_{i,111}
\]

(E_i>0)はProtectionにより回避された減少、(E_i<0)はProtectありで別の場所に追加された減少を表す。`vegetation_retained_by_protection`は正の回避量だけを緑で示す。`vegetation_retained_and_displaced_by_protection`は緑を回避、赤を追加減少として同じ図に示す。`forest_grassland_loss_without_with_effect`は、各植生について011の減少、111の減少、両者の差を3列で比較する。

| 植生クラス | 回避された減少 | 他所の追加減少 | 純減少回避 | 011の2005–2100減少 |
|---|---:|---:|---:|---:|
| Naturally regenerating forest | 22.3 Mha | 26.2 Mha | -3.9 Mha | 164.5 Mha |
| Managed grassland / pasture | 76.7 Mha | 154.4 Mha | -77.6 Mha | 1,144.9 Mha |
| Natural grassland + other natural land | 317.7 Mha | 245.6 Mha | +72.1 Mha | 1,280.5 Mha |

この結果は、Protectionが一部セルで植生減少を防ぐ一方、土地利用再均衡により別セルの減少を増やしうることを示す。自然再生林と管理草地では、正の回避量だけを見ると保全便益を過大に読むため、赤の追加減少と純効果を併読する必要がある。クラス9では純減少回避が正だが、自然草地とその他自然地のどちらが寄与したかは分離できない。また、これは反実仮想差であり、新規保護区ポリゴンと維持植生の直接交差面積ではない。直接交差を算定するには、0.5度の`protectfracL`出力が必要である。

## 22. `publication` Figure 6–8：流域・貿易・国別の追加診断

Figure 6–8は、主結果を「流域内の土地再配置」「国境を越える市場調整」「国ごとの土地応答と農業影響」の3段階で深掘りする。いずれも中心比較を2100年の`MITI_PROT − MITI`、すなわち緩和策がある世界で保護区拡大を追加した差に固定する。NoAff・NoBioは使わない。

### 22.1 Figure 6：流域のCDR用地・食料農地・生産量

`all_scenario_World.gdx`から、流域総面積`qlTot_load`、保護面積`qlProt_load`、Planted forest`qlAfr_load`、Bioenergy作物`qlBas_load[ecr]`、食料作物収穫面積`qlBas_load[13 crops]`、作物生産`qsBas_load[13 crops]`を抽出する。土地変化はMITIの流域総面積で割ったpercentage-point差である。作物生産はALPHAの国・品目別`calCoef`を使いPJへ変換する。係数適用率は重量ベースで約93.5%であり、欠損係数を持つ生産はカロリー集計へ入らない。

地図は、Planted forestとBioenergyの合計変化と食料収穫面積変化の符号を組み合わせ、CDR拡大・食料農地縮小、CDR縮小・食料農地拡大、双方拡大、双方縮小、小変化に分類する。散布図は同じ流域差を連続量で示す。結果では、追加保護率と作物生産変化のSpearman相関は-0.32、Planted forest変化とは+0.45、Bioenergy変化とは+0.29である。これは保護面積だけでなく、保護区追加後のCDR用地と食料農地の再配分が生産応答と結び付くことを示唆する。ただし流域間の土地移動経路を直接追跡したものではない。

### 22.2 Figure 7：国別生産ショックと貿易による調整

国別AgMIP CRPのProduction、Consumption、Imports、Exportsをカロリー基準で集約しPJへ変換する。国内生産寄与は`100 × ΔPROD / CONS_MITI`、純輸入寄与は`100 × (ΔIMPO − ΔEXPO) / CONS_MITI`である。両者の符号が逆で大きさが近い国ほど、国内生産変化と反対向きに貿易が動いている。

`|国内生産寄与| ≥ 0.5 percentage points`の国では91.8%で純輸入寄与が逆符号であり、中央値では生産寄与の絶対値2.06 pointsに対して、反対向きの貿易調整が2.40 pointsである。したがって「国別生産ショックの多くが貿易変化と同時に調整されている」という記述は支持される。ただし生産と貿易はモデル内で同時決定されるため、貿易だけを外生的原因とする因果解釈はできない。

消費総変化は、国内生産と純輸入だけでは必ずしも閉じない。その差を`Other balance terms`として表示する。これは在庫、加工、飼料、損失等を含み得る会計残差で、単独の機構としては読まない。

### 22.3 Figure 8：国別の土地応答と市場影響

`Mergedcomparison.gdx`からAdditional protection、Planted forest、Bioenergy、Food croplandの国別面積差を取り、MITI国土面積に対するpercentage pointsへ変換してAgMIPの国別市場指標と結合する。上位国図はAdditional protection、Planted forest、Bioenergyの絶対変化が大きい25か国を選ぶ。散布図は土地応答と作物生産変化を直接示す。

探索的な国別回帰では、各変数を標準化し、R10固定効果とMITI輸入依存度を加え、`sqrt(MITI crop production)`で重み付けする。相関図は重みなしSpearman係数である。今回の単純相関ではAdditional protectionと生産変化は0.05と弱く、Planted forestは0.46、Bioenergyは0.19、Food croplandは0.63である。Food croplandと純輸入寄与は-0.53、Planted forestと純輸入寄与は-0.38である。保護率そのものよりも、保護区追加後の生産用地・CDR用地の国別再配置が生産と貿易の差に対応している可能性が高い。これらは候補機構を絞る記述統計で、政策効果の因果推定ではない。

詳細な式、変数名、係数適用率、回帰仕様は`publication/README.md`、再現コードは`publication/make_main_figures.R`、図に使った数値は`output/publication_figures/source_data/figure_6_*`、`figure_7_*`、`figure_8_*`に保存している。

### 22.4 補足：PLUM保護区政策入力とCDR収量入力のグリッド地図

`supplementary/supplementary_plum_protect_target.*`、`supplementary_plum_protect_additional.*`、`supplementary_plum_protect_all.*`、`supplementary_plum_serious_land_allpolicy.*`は、`data/InputData/`にあるAIM/PLUMの入力から、`_protect`シナリオの保護対象割合、BAUからの追加保護割合、`protect_all`、`serious_land_allpolicy`を0.5度グリッドで1枚ずつ示す。保護対象割合は`min(1, protect_all + serious_land_allpolicy)`、BAUは`min(1, protect_bs)`で、2030年SSP2の人口密度が0.1未満のセルはどちらも1に置く。これは上限適用前の政策目標であり、モデル内で有効な保護率ではない。

`supplementary/supplementary_plum_afforestation_yield.*`と`supplementary_plum_bioenergy_yield.*`は、植林の炭素吸収速度`ACFout("AFRCUR", G)`（Mg C ha⁻¹ yr⁻¹）と、H08の天水ミスカンサス・スイッチグラス平均から作るバイオエネルギー作物収量（t ha⁻¹ yr⁻¹）を同じグリッドで示す。これらは保護区とCDRの配置を読む際の入力側の参照図であり、シナリオ結果ではない。グリッド値は`source_data/supplementary_plum_protection_inputs_grid.csv.gz`に保存する。定義の詳細は`publication/README.md`を参照。

### 22.5 補足：AIM-BIOの種数・種構成類似度・ESHI

`supplementary/supplementary_species_richness_BAU.*`、`_MITI.*`、`_MITI_PROT.*`は、`data/AIMBIO/SPnumber/`の全分類群種数を0.5度グリッドで示し、`supplementary_species_richness_MITI_PROT_minus_MITI.*`はMITI_PROTとMITIの差を示す。年は`TARGET_YEAR`以下で最も遅いAIM-BIO出力年（既定では2090年）である。種構成類似度（`data/AIMBIO/ComSim/`）についても同じ構成の`supplementary_compositional_similarity_*.*`を出力する。海洋セルはAIM-LUの陸地マスクで除く。

`supplementary/supplementary_biodiversity_trajectories.*`は、BAU、MITI、MITI_PROTについて、2030–2090年の全陸地セルの平均を実線、25・75パーセンタイルを破線で示す。Figure 1eは同じ統計量をMITIとMITI_PROTだけで示す。種数はセルごとの現在比変化`(NS_future − NS_current) / NS_current`、種構成類似度は現在との類似度そのものである。`Current`層で種数が0のセルは統計から除く。`supplementary_eshi_regions.*`は、`data/AIMBIO/ESH/`の世界とR5地域別ESHI（`allspecies`、2005年比）の推移をシナリオ別に示す。ESHIは2005年比の生息適地面積の幾何平均で、1が2005年水準である。数値は`source_data/supplementary_biodiversity_grid_trajectories.csv`と`supplementary_eshi_regions.csv`に保存する。

`supplementary/supplementary_basin_species_richness.*`は、MITI_PROT − MITIの種数変化（現在種数比、セル面積加重の流域平均）を流域地図にし、Figure 6と同じ流域指標（追加保護面積、植林、バイオエネルギー農地、食料農地、それらの正味合計、作物生産変化）との散布図に単回帰線とSpearmanの`rho`を添える。土地指標は種数と同じ年で評価する。数値は`source_data/supplementary_basin_species_richness.csv`に保存する。

### 22.6 Figure 9：流域の追加保護と農業・土地応答

流域の追加保護面積（`MITI_PROT − MITI`、MITI流域面積比pp）を横軸に、作物生産量、収量、生産者価格、輸入依存度、植林面積、バイオエネルギー農地の変化を縦軸にとった6パネルの散布図である。生産者価格は`pp_load`を流域のMITI生産量で重み付けしたLaspeyres指数、輸入依存度はFigure 7の国別値を流域に割り当てる。各パネルに通常最小二乗の傾き、決定係数、p値、Spearman順位相関、流域数を記す。定義は`publication/README.md`のFigure 9節にある。

## 23. 図とCSVを読む際の留意点

- 地図の色範囲は、極端な少数セルや国で全体の差が見えなくなることを避けるため、その図に含まれる非ゼロ絶対値の98パーセンタイルを正負対称の上限にする。範囲外は端色に丸めるが、CSVの値は切り詰めない。
- 青または緑を正、茶を負とする。正が望ましいかは指標による。生産やBIIでは正が増加、土地利用排出図では符号反転済みなので正が排出削減である。
- 地図の灰色国は0ではない。地理コードが結合しない、元データがない、またはBaUがほぼゼロで比率を定義しない国である。
- 棒の黒い菱形は全対策`111`とBaU`000`の差であり、3色のShapley寄与は数値上この点と一致する。
- 作物と畜産物は生物物理過程と市場応答が異なる。`CRP`、`LSP`、`AGR`はモデル側のAgMIP集計を使い、詳細品目の単純合計や価格の単純平均は行わない。
- 世界とR10はGDXの集約値を直接使う。国別図の値を足したものではなく、国コードの欠損や地図結合に左右されない。
- 06の物理量`protection_interaction`はShapley寄与ではなく差の差である。図の`Context difference`は2つの相対的Protection effectのpercentage-point差である。07の積み上げ図はこの物理的な差の差を植林、BECCS、三者の相互作用へ分ける。どれも値が大きいだけで望ましい相乗効果とは限らず、指標の正負と単位を合わせて解釈する。
- `PJ/yr`は栄養の質やタンパク質量を表さない。`CALI`は`kcal/cap/day`で示す平均的な食料摂取熱量であり、国内分布、アクセス、価格負担、栄養構成を単独では示さない。
- `AIM-LUmap`の`LC_area_share`は、セルごとの区分合計が1以下である。`publication`のFigure 3b–cでは各セルの区分面積をそのまま使い、減少合計と増加合計が一致しないセルでは小さい方の面積だけを用途間の移動として配分する。class 1は`CL + CROP_FLW`であり、食料生産農地だけでなく休閑地を含む。値を0–1へ丸める処理やMergedComparisonへの比例補正は行わない。
