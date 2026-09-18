# 本番図9枚の再現手順と定義

`make_main_figures.R`はFigure 1–9と補足用Shapley図を一括作成する独立したRスクリプトです。`prog/`以下の探索用コードは読み込まず、GDX・NetCDF・shapefileを直接処理します。図中はパネル記号、短い見出し、軸、単位、凡例に限定し、詳細な定義をこの文書にまとめています。

## 実行方法

`Labo/260918Interim`で実行します。

```bash
Rscript publication/make_main_figures.R
```

既定の評価年は2100年です。2050年版は次のように作成でき、出力先は`output/publication_figures_2050/`になります。

```bash
TARGET_YEAR=2050 Rscript publication/make_main_figures.R
```

出力先は`output/publication_figures/`です。

- `figure_1/`–`figure_10/`：600 dpi PNGとベクターPDF。Figure 1–5と10には本文で引用する数値を1行1値で並べた`figure_N_key_values.csv`（列：`panel`, `quantity`, `comparison`, `region`, `year`, `value`, `unit`）を併せて保存します
- `supplementary/`：BAU／PROT保護面積分布、Figure 2保護指標のデータソース比較、NoAff・NoBioを使う補足Shapley図、保全地域拡大の追加効果の地域版、双方向農地遷移図、全土地を含むサンキー図、流域保護面積版のFigure 2、PLUM保護区政策入力とCDR収量入力のグリッド地図、AIM-BIO生物多様性指標、流域別の種数・種構成類似度と土地変化、緩和策の有無別の生物多様性効果、AIM/Hubの炭素価格とバイオエネルギー需要、Figure 9の加重版、地域別の作物貿易量と供給収支
- `source_data/`：各図に使ったCSVまたは圧縮CSV
- `session_info.txt`：Rとパッケージの実行環境

## 主分析のシナリオ

主図では次の3シナリオだけを使います。

| 表示名 | 状態 | 元シナリオ名 | 用途 |
|---|---:|---|---|
| BAU | `000` | `SSP2_BaU_NoCC_LandInt_global2` | 無政策基準 |
| MITI | `011` | `SSP2_1100C_CACN_DAC_NoCC_LandInt_global2` | 1100C緩和策 |
| MITI_PROT | `111` | `SSP2_protect_1100C_CACN_DAC_NoCC_LandInt_global2` | 1100C緩和策＋保護区拡大 |

研究の中心となるシナリオ差は、

\[
\text{MITI\_PROT}-\text{MITI}=111-011
\]

です。この差を、緩和策が存在する条件下で保護区拡大を追加したときの変化として扱います。BAUは緩和策全体の規模を示す基準として使います。NoAff・NoBioシナリオは主図から除き、要因分解・感度分析に相当する補足Shapley図だけで使用します。

`data/ALPHA/plum_data.gdx`のみ、ファイル内のシナリオ名に合わせて末尾`LandInt_global1`を使います。他の入力は`LandInt_global2`です。

## 入力データ

入力はすべて`data/260917/`以下に置きます（スクリプトの`data_dir`）。以下のパスは`data/260917/`からの相対パスです。

- `data/PLUM/Mergedcomparison.gdx`
  - `IAMCTempwoU`：世界・R10・国・流域の土地面積と統合保護面積
  - `GHGL`：土地利用転換由来の排出・吸収
- `data/PLUM/nc/AIM-LUmap_*.nc`
  - `LC_area_share`：0.5度グリッドの土地利用面積割合
  - `pixel_area`：グリッド面積
- `data/ALPHA/plum_data.gdx`
  - `protPlum`：流域・植生別の保護割合
- `data/ALPHA/all_scenario_World.gdx`
  - `yldBas_load`：作物・流域別収量（t ha⁻¹）
  - `qlBas_load`：作物・流域別収穫面積（1000 ha）
- `data/ALPHA/AGMIP_all_scenario.gdx`
  - `AGMIP_agg`：CRPの`AREA`, `PROD`, `CONS`, `EXPO`, `IMPO`, `NETT`, `CALI`
- `data/Basin_Boundary/Basin_Boundary.shp`
  - ALPHA流域境界
- `data/InputData/`（AIM/PLUMの入力データ。Figure 3b–cの保護区重ね合わせと補足図に使用）
  - `data_prep.gdx`の`MAP_GIJ`：グリッドID`G`と0.5度行列`I, J`の対応
  - `policydata.gdx`の`protect_bs`, `protect_all`, `serious_land_allpolicy`：保護区政策のセル内割合
  - `ssppopmap.gdx`の`popdens`：SSP2の2030年人口密度
  - `visit_forest_growth_function.gdx`の`ACFout`：植林の炭素吸収速度（Mg C ha⁻¹ yr⁻¹）
  - `H08_yield_num_gdx.gdx`の`yield_miscanthus_rainfed_num`, `yield_swichgrass_rainfed_num`：天水バイオエネルギー作物収量（kg ha⁻¹ yr⁻¹）
- `data/AIMBIO/`（AIM-BIOの生物多様性出力。補足図のみに使用）
  - `SPnumber/<run>_LU_Pmig/SPnumber_all_taxon_*.tif`：0.5度グリッドの全分類群種数（`Current`と2030, 2050, 2070, 2090年）
  - `ComSim/<run>_LU_Pmig/JaccardSimilarity_<run>_<year>_LU.tif`：同じグリッドの現在との種構成類似度（Jaccard、0–1）。`Current`層はない
  - `ESH/ESHI_RegionAgg_<run>-*.csv`：R5地域別の生息適地指数ESHI（2005年比）
  - `<run>`はBAUが`Baseline`、PROTが`PROT_BaU`、MITIが`1100C_CACN_DAC`、MITI_PROTが`PROT_1100C_CACN_DAC`

ALPHAの食料作物は`wht, rce, crl, mze, str, sgr, pls, nut, ocr, vgt, frt, stm, spc`です。エネルギー作物`ecr`は含めません。AgMIPも集約済み食料作物群`CRP`を使うため、農業市場図に`ecr`は入りません。

## Figure 1：土地システム全体

### a 世界の土地利用推移

`IAMCTempwoU`から次の世界値を抽出し、BAU、MITI、MITI_PROTを比較します。

- Food cropland (incl. fallow)：`Lan_Cov_Cro − Lan_Cov_Cro_Ene_Cro_2nd_gen`。休耕地を含む非エネルギー耕地で、パネルbのFood croplandとFallow croplandの合計に一致します
- Bioenergy cropland：`Lan_Cov_Cro_Ene_Cro_2nd_gen`
- Planted forest：`Lan_Cov_Frs_Aff`
- Natural land：`Lan_Cov_Frs − Lan_Cov_Frs_Aff + Lan_Cov_Oth_Nat_Lan`。植林を除いた森林と他の自然地の合計で、再植林`Lan_Cov_Frs_Ref`は森林側に含まれます。`Lan_Cov_Frs_Aff`は植林開始前の年に報告されないため0として扱います

単位はmillion haです。軸ラベルは全パネルで括弧内に単位を書き、目盛りには単位記号を付けません。

### b R10の土地利用変化

BAU、MITI、MITI_PROTの各シナリオについて、2020年から2100年までの土地利用変化を示します。各地域内では左からBAU、MITI、MITI_PROTの3本を横に並べ、シナリオは棒の輪郭色でも区別します。値は各シナリオの2020年`Lan_Cov`で割った地域陸地面積比です。全シナリオを同じy軸で比較します。排他的土地分類は次の7区分です。

- Food cropland：`Lan_Cov_Cro_Non_Ene_Cro`
- Bioenergy cropland：`Lan_Cov_Cro_Ene_Cro_2nd_gen`
- Fallow cropland：`Lan_Cov_Cro − Food cropland − Bioenergy cropland`
- Forest：`Lan_Cov_Frs`
- Pasture：`Lan_Cov_Pst`
- Other natural land：`Lan_Cov_Oth_Nat_Lan`
- Built-up area：`Lan_Cov_Bui_Are`

Planted forestはForestの内数、保護面積は既存土地利用に重なる診断量なので、排他的積み上げには加えません。

### c 保護面積推移

`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int / Lan_Cov × 100`です。分母の`Lan_Cov`は農地や森林だけでなく、モデルで扱う全陸域面積です。したがってMITI_PROTの約40%は「世界のモデル対象陸域に占める統合保護面積」のモデル出力であり、政策目標値を40%に設定したという意味ではありません。MergedComparisonの保護面積系列は2030年からなので、2020年はMITIの最初の非欠損保護率を全シナリオ共通の初期値として置き、2020–2030年の政策導入変化を表示します。この補完行はsource dataで`value_origin = common 2020 baseline`と識別できます。

保護面積割合の絶対水準を示すため、y軸は0%から表示します。

### d 累積土地利用転換排出・吸収

MITIとMITI_PROTについて、2020年から評価年までの`GHGL`の`land_category = LUC`を台形積分します。

- Positive：正の土地転換排出
- Negative：土地転換に伴う吸収
- 黒点：両者のネット値

単位はGt CO₂です。Negativeは植林だけに限定した吸収ではなく、`GHGL`の`land_category = LUC`に含まれる土地転換由来の負のフラックス全体です。植林、自然回復、土壌炭素など、モデルがLUC吸収として計上する要素を含み得ます。`MNGFRS`はmanaged forest吸収なので明示的に含めていません。

### e 種数

AIM-BIOの全分類群種数（`data/AIMBIO/SPnumber/`）について、MITIとMITI_PROTのセルごとの2005年（`Current`層）比変化 `(NS_future − NS_current) / NS_current` を2030–2090年で示します。線は全陸地セルの平均（セルごとに等しい重み）です。`Current`層で種数が0のセル（氷床と極乾燥地）は統計から除きます。数値は補足の`supplementary_biodiversity_grid_trajectories.csv`と同じで、`figure_1e_species_richness.csv`に保存します。

## Figure 2：グリッド別の追加保護面積と土地応答

パネルaは、`data/InputData/`にあるAIM/PLUMの0.5度保護区政策入力から作る追加保護割合です。

\[
100\times\max\bigl(0,\ T^{\mathrm{PROT}}-T^{\mathrm{BAU}}\bigr),\qquad
T^{\mathrm{BAU}}=\min(1,\ \texttt{protect\_bs}),\quad
T^{\mathrm{PROT}}=\min(1,\ \texttt{protect\_all}+\texttt{serious\_land\_allpolicy})
\]

2030年SSP2人口密度`popdens`が0.1未満のセルでは\(T^{\mathrm{BAU}}=T^{\mathrm{PROT}}=1\)とします。単位は`% of cell land`（セル陸地に対する割合）です。この値はモデルへの入力である上限適用前の政策目標であり、PLUM内で有効な`protectfrac`は前期の自然植生面積で上限を課された値です。セル値は`figure_2_grid_protection_pressure.csv.gz`に保存します。

土地応答のpercentage pointsは割合どうしの差です。たとえば土地被覆割合が10%から12%になった場合は+2 percentage pointsであり、初期値に対する相対変化率では+20%です。

追加保護割合の階級は固定値で分けます。

- Low：10%以下
- Mid：10%超30%以下
- High：30%超

追加保護がゼロのセルはLowに入ります。境界は`figure_2_class_thresholds.csv`に保存します（各行に単位も記録）。土地応答はAIM-LUのグリッド差`MITI_PROT − MITI`です。

- Decrease：−0.1 percentage points未満
- Stable：±0.1 percentage points以内
- Increase：+0.1 percentage points超

色相が土地応答、濃さが追加保護率階級を示します。凡例の縦軸にもDecrease、Stable、Increaseの数値境界をpercentage pointsで表示します。応答地図はPlanted forest、Bioenergy cropland、Food cropland、Natural landの4つです。Natural landはAIM-LUのnaturally regenerating forest（class 4）とnatural grassland + other natural land（class 9）の合計です。

この4枚の土地応答地図をまとめてパネルbとし、個別のパネル記号は付けません。

### Supplementary：流域別保護面積を使ったFigure 2

`supplementary/supplementary_figure_2_basin_protection.*`は、パネルaの保護圧力を`Mergedcomparison.gdx`の`IAMCTempwoU`から抽出した流域別の`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`に置き換えた版です。流域は`i2`の`国コード_流域コード`（例：`AFG_AMDA`）で識別し、Basin Boundaryの400流域と結合します。この変数はAIM-PLUMの`protect_prmsecR`に対応する統合保護面積で、単位はmillion haです。追加保護面積は`MITI_PROT − MITI`（`111 − 011`）で、`qlTot_load`の流域総土地面積を分母にして`% of total basin land`に変換します。

\[
100\frac{P^{\mathrm{Merged}}_{111}-P^{\mathrm{Merged}}_{011}}
{L^{\mathrm{qlTot}}}
\]

MergedComparisonの`Lan_Cov`はモデルが配分する土地面積であり、流域の地理的総面積より小さい場合があるため、分母には使いません。この版では正の追加保護割合を0.5度グリッド上の三分位でLow、Mid、Highに分け、計算された境界を凡例と`supplementary_figure_2_basin_class_thresholds.csv`に記録します。流域値は`supplementary_figure_2_basin_protection_pressure.csv`、分類コードは`supplementary_figure_2_basin_<class>_bivariate_grid.csv.gz`に保存します。

`qlProt_load`はALPHAの保護対象土地タイプ別面積（1000 ha）であり、MergedComparisonの統合保護面積とは定義と集計経路が異なります。流域・国・地域別の追加保護面積を使う図はすべてMergedComparisonの系列を使い、`qlProt_load`はこの比較図にのみ使います。`supplementary_figure_2_pressure_source_comparison.*`で両者を同じカラースケールで比較し、対応する流域値を`supplementary_figure_2_pressure_source_comparison.csv`と`supplementary_figure_2_qlprot_pressure.csv`に保存します。

### Supplementary：BAUとPROTの保護面積分布

`supplementary_bau_prot_protection_distribution.*`は、MergedComparisonの流域別`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`を使ってBAU（`000`）とPROT（`100`）の保護面積を流域別に示す4パネル図です。上段は保護面積（Mha）、下段は流域総土地面積に対する割合（%）です。両シナリオの比較可能性を保つため、各段では共通のカラースケールを使っています。追加保護面積（PROT−BAU）も同じCSVに収録しています。

## Figure 3：食料農地の再配置

MITIからMITI_PROTへの変化について、食料農地を起点として次の4シグナルを示します。

NetCDF class 1は`CL + CROP_FLW`であり、Figure 3のCroplandには食料生産農地と休閑地の両方が含まれます。MergedComparisonへの比例補正は行いません。

MergedComparisonには休閑地だけを示す独立したIAMC変数はありませんが、`Lan_Cov_Cro - Lan_Cov_Cro_Non_Ene_Cro - Lan_Cov_Cro_Ene_Cro_2nd_gen`として計算できます。したがって、NetCDF class 1と対応する集計量は`Lan_Cov_Cro - Lan_Cov_Cro_Ene_Cro_2nd_gen`です。ただし、グリッド出力とMergedComparisonでは集計経路が異なるため、NetCDFの世界合計がMergedComparisonと完全に一致するとは限りません。

- Cropland → planted forest
- Cropland → bioenergy
- Cropland → Other
- Additional cropland

食料農地減少を\(L_i\)、植林・バイオ増加を\(F_i,B_i\)とすると、負側の対応量は、

\[
M_i^- = \min(L_i,F_i+B_i),\qquad
C_{i,F}^- = M_i^-\frac{F_i}{F_i+B_i},\qquad
C_{i,B}^- = M_i^-\frac{B_i}{F_i+B_i}
\]

です。残差を`Cropland → Other`とします。Additional croplandは、植林・バイオの減少元へ細分せず、食料農地が増えたセルの増加量をそのまま示します。

パネルaは各セルで最大の再配置シグナルだけを色相で示します。区分名はCropland → planted forest、Cropland → bioenergy、Cropland → Other、Additional croplandです。色の濃さはグリッド面積割合の変化（percentage points）です。4区分に共通するスケールを使い、上限は正の変化量を全区分でプールした98パーセンタイルです。小さい変化を読み取れるよう色濃度だけを平方根変換し、凡例の数値軸は元のpercentage-point値で示します。上限を超える値は端色に丸めます。

MITIまたはMITI_PROTで食料農地割合が1%以上あり、両シナリオ間の食料農地変化が±0.1 percentage points以内のセルをUnchanged croplandとしてクリーム色で表示します。これにより、灰色のCropland → Otherと区別します。再配置として表示する閾値も0.1 percentage pointsです。共通上限値は`figure_3_food_transition_summary.csv`の`display_limit_pp`に保存します。平方根変換は図の軸名ではなく本説明に記録しています。

### b 土地区分別の総面積

パネルbは、パネルcで定義する8ノード（下記）でMITIとMITI_PROTの世界陸地面積を積み上げます。灰色のProtected natural landは保護区に重なる自然植生で、両シナリオの差がパネルcで省いた指定のみの流れに対応します。この灰色の面積（MITI 3,368 Mha、MITI_PROT 4,871 Mha）は、Figure 1cのモデル出力`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`（2,685 Mha、4,876 Mha）と定義が異なります。Figure 1cはPLUM内で上限適用後の有効保護率`max(protectfrac, protectfracL(PRM_SEC))`にセル面積を掛けた値で、`1 − SL − OL`の上限により氷・水域等の`OL`を除きます。パネルbの上限に使うNetCDF class 9は`GL + OL`を分離できないため、人口密度0.1未満で目標が1になるセルでは`OL`も灰色に入り、特にMITIで約680 Mha大きくなります。追加保護面積は入力ベースで2,142 Mha、モデル出力で2,191 Mhaと近い値です。値は`figure_3b_grid_land_totals.csv`に、ノード別の総面積、不変面積、流出、流入、純変化として保存します。

### c 土地用途間の移動（サンキー図）

パネルcは、MITIからMITI_PROTへ用途が変わった土地だけを、AIM-LUの0.5度セル単位で追跡して集計します。土地区分はNetCDFの`lc_class`を次の8ノードにまとめます。

| ノード | `lc_class` |
|---|---|
| Cropland | 1（`CL + CROP_FLW`） |
| Bioenergy cropland | 2 |
| Planted forest | 5 |
| Pasture | 3 |
| Restored / abandoned land | 6, 7, 8 |
| Natural forest | 4 |
| Other natural land | 9（`GL + OL`） |
| Protected natural land | 上記のNatural forestとOther natural landのうち保護区に重なる部分 |

class 10（built-up）は現在のNetCDFで0、class 11–12はclass 6–7の内数なので使いません。

保護区は`data/InputData/`の政策入力を同じ0.5度グリッドに重ねて求めます。MITIはBAUの保護対象割合`min(1, protect_bs)`、MITI_PROTは`max(BAU, min(1, protect_all + serious_land_allpolicy))`で、2030年SSP2人口密度が0.1未満のセルはどちらも1です。セルの保護面積は、この割合にセル陸地面積を掛けた値と、セル内の保護対象ノード（Natural forest、Other natural land）の合計面積の小さい方とし、保護対象ノードへ面積比で配分して`Protected natural land`へ移します。これはPLUMの`EQYPROTECT`が`PRM_SEC`（自然植生）に保護率の下限を課す構造に対応します。

セル内では、面積が減ったノードの減少量を、面積が増えたノードへ増加量の比で配分します。ノード\(k\)の減少を\(L_k\)、ノード\(j\)の増加を\(G_j\)とすると、

\[
F_{kj}=L_k\frac{G_j}{\sum_j G_j}
\]

です。セルの減少合計と増加合計が一致しない場合は、小さい方の面積だけを配分し、\(L_k\)を\(\min(\sum_k L_k,\sum_j G_j)/\sum_k L_k\)倍に縮めます。区分割合の再スケールは行いません。全セルの\(F_{kj}\)に`pixel_area`を掛けて合計した値がサンキーの帯幅で、単位はmillion haです。`figure_3b_grid_land_totals.csv`の`outflow_mha`と`inflow_mha`は各区分の減少量と増加量の合計、`matched_outflow_mha`と`matched_inflow_mha`は帯に配分された面積（ノードラベルの数値と同じ）、`unmatched_*`はセル内で対応づけられなかった残りです。本文で引用する数値は`matched_*`と`figure_3c_grid_transitions.csv`の流量を使います。未対応分が生じる原因は`doc/plum_netcdf_land_loss_ja.md`にまとめています。同じノードにとどまる面積\(\min(a_k^{011},a_k^{111})\)は描きません。ノード名の括弧内は、左がそのノードから出た面積、右がそのノードに入った面積です。

Natural forestとOther natural landからProtected natural landへの流れは、土地用途は変わらず保護区に指定されただけの面積です。この流れ（約1,300 Mha）は帯幅の大半を占めて用途転換を読めなくするため、パネルcには描かず、`figure_3c_grid_transitions.csv`に`flow_type = new protection`として保存します。Cropland、Bioenergy cropland、Planted forest、Pasture、Restored / abandoned landからProtected natural landへの流れは、用途が自然植生へ戻り、かつ保護区に入った面積なので描き、`flow_type = other source to protected land`とします。Protected natural landから他ノードへ戻る流れは`from protected land`とし、描きません。

この分解は、同じ年の2つのシナリオを同一セルで比較し、区分間の面積差を整合的に配分した診断量です。時間方向の土地転換や個々の土地parcelの追跡ではなく、シナリオ間の土地配分の対応を示します。

### Supplementary：全土地を含むサンキー図

`supplementary/supplementary_full_land_sankey.*`は、パネルcで省いた不変面積と指定のみの流れを含め、ノード高をパネルbの総面積に一致させた版です。不変面積は薄く、保護区指定の流れはやや薄く、用途転換は濃く描きます。総陸地に対して用途転換は1%未満なので、この版では個々の転換を読み取れません。流れの一覧は`supplementary_full_land_sankey.csv`に`flow_type`（`persistent`、`new protection`、`from protected land`、`other source to protected land`、`land-use change`）付きで保存します。

双方向に細分した6区分版は`supplementary_bidirectional_cropland_transitions.*`として残しています。

## Figure 4：収量と生産量分解

### a–b 流域収量地図

次の2枚を並べます。

- a Yield：`MITI_PROT / BAU − 1`
- b：`MITI_PROT / MITI − 1`

したがってbは、緩和策を共通条件として保護区拡大を追加したシナリオ対の差です。図見出しにも`MITI_PROT − MITI`を明記しています。この差には保護区追加に伴う土地利用・市場調整と、他の緩和策との相互作用が含まれます。

地図の`Basin-level yield change (%)`は、流域内の対象作物収量をBAUの2100年収穫面積ウェイトで集約した値の相対変化率です。固定ウェイトを使うため、作物構成の変化を収量変化として数えません。欠損流域は灰色です。各カラースケールは絶対値98パーセンタイルで対称化し、外れ値を端色に丸めます。

### c–d 生産量分解

主図ではNoAff・NoBioを使いません。パネルcの世界集計は`MITI − BAU`と`MITI_PROT − BAU`の2本を並べ、パネルdはR10別の`MITI_PROT − MITI`を示します。

AgMIPのCRPについて\(P=A Y\)となるeffective caloric yield \(Y=P/A\)を定義し、交差項を半分ずつ配る対称分解を行います。

\[
\Delta P_A=(A_1-A_0)\frac{Y_0+Y_1}{2},\qquad
\Delta P_Y=(Y_1-Y_0)\frac{A_0+A_1}{2}
\]

色付き棒が収穫面積・effective yield寄与、黒い菱形が生産量総変化です。パネルcとdの両方に同じ凡例を表示します。ここでの単位は基準シナリオ生産量に対する割合（% of baseline production）で、2寄与を加算すると黒い菱形の総変化率に一致します。土地割合そのものの差を示すFigure 2–3のpercentage pointsとは意味が異なります。コードは`面積寄与＋収量寄与＝総変化`を検査し、閉じなければ停止します。

## Figure 5：農業市場影響

パネルaとcはBAU、MITI、MITI_PROT、パネルbはさらにPROTを使います。

- a：世界のProduction、Producer price、Food intakeについて、MITIとMITI_PROTのBAU比
- b：保全地域拡大の追加効果を、緩和策なし（`PROT − BAU`、BAU比%）と緩和策あり（`MITI_PROT − MITI`、MITI比%）で並べたもの。同じ保全政策の効果が土地CDRを伴う緩和策の下で何倍になるかを示す。数値は`figure_5b_protection_effect.csv`
- c：R10のProduction、Producer price、Food intakeのBAU比。3指標を横に並べ、地域名は左端に1回だけ示す

地域図は青いMITI棒と朱色のMITI_PROT棒を地域ごとに並べます。Tradeは主図から除外しています。パネルbの地域版は`supplementary/supplementary_protection_effect_regions.*`に保存します。

### 補足：地域別の作物貿易量

`supplementary/supplementary_regional_trade.*`は、10地域のImports、Exports、Net exports（PJ yr⁻¹）をBAU、MITI、MITI_PROTの水準で並べたものです。本文で欧州の輸入増と純輸出の符号反転を引用する根拠で、数値は`supplementary_regional_trade.csv`に保存します。

### 補足：土地CDRの構成別にみた保全地域拡大の追加効果

`supplementary/supplementary_protection_effect_cdr_variants.*`は、保全地域拡大の追加効果（拡大なしのシナリオに対する変化率）を緩和策の4条件で比較します。

| 条件 | 拡大なし | 拡大あり |
|---|---|---|
| No mitigation | `000` BAU | `100` PROT |
| Mitigation without BECCS | `010` MITI_NoBECCS | `110` MITI_PROT_NoBECCS |
| Mitigation without planted forest | `001` MITI_NoPlantedForest | `101` MITI_PROT_NoPlantedForest |
| Full mitigation | `011` MITI | `111` MITI_PROT |

パネルaは世界、bは10地域で、指標はProduction、Producer price、Food intakeです。数値は`supplementary_protection_effect_cdr_variants.csv`に保存します。炭素価格を含み土地CDRを含まない緩和シナリオはないため、緩和策の非CDR要素の寄与は分離できません。

- Production、Consumption、Exports、Imports、Net exports：`million kcal × 4.184 × 10⁻⁶`でPJ yr⁻¹へ変換
- Producer price：`USD / (million kcal × 4.184)`でUSD GJ⁻¹
- Food intake：元の`kcal cap⁻¹ day⁻¹`を維持

## Figure 6：流域の土地圧力と作物生産の再配置

この図は、緩和策を共通条件にして保護区拡大を追加したときの流域応答を、すべて2100年の`MITI_PROT − MITI`として示します。土地面積と生産量は`data/ALPHA/all_scenario_World.gdx`から読みます。

- 流域総面積：`qlTot_load`
- 保護面積：`data/PLUM/Mergedcomparison.gdx`の流域別`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`（Figure 2と同じ系列）
- Planted forest：`qlAfr_load`
- Bioenergy：`qlBas_load`の`ecr`
- Food harvested area：`qlBas_load`の食料作物13品目
- Crop production：`qsBas_load`の同じ食料作物13品目

土地変化はすべて流域総面積を分母とする割合差です。たとえばFood harvested-area changeは、

\[
100\frac{A_{111}-A_{011}}{L_{011}}
\]

で、単位はpercentage pointsです。Planted forestとBioenergyの和を図中のCDR land changeとします。`±0.01 percentage points`以内をSmall changeとし、それ以外はCDR用地と食料収穫面積の増減符号で4区分します。パネルaはこの区分の空間分布、bはCDR用地と食料収穫面積、cは食料収穫面積と作物生産、dは追加保護・Planted forest・Bioenergyと作物生産の関係です。点の大きさはMITIの流域作物生産量です。

流域作物生産量は、ALPHAのAgMIP出力と同じ国・品目別`calCoef`を`baseyear.gdx`から抽出した`data/ALPHA/calCoef.csv`でエネルギー換算します。

\[
P_{PJ}=P_{1000t}\times calCoef_{million\ kcal/1000t}\times4.184\times10^{-6}
\]

係数がある生産量は重量ベースでMITI 93.51%、MITI_PROT 93.52%です。係数欠損品目はALPHAのAgMIP出力と同じくカロリー集計へ加えません。適用率は`figure_6_calorie_coefficient_coverage.csv`に保存します。パネルdの青線はMITI生産量で重み付けした記述的な線形傾向、`rho`は重みなしSpearman順位相関です。セル間・流域間の土地parcel移動を追跡した結果ではなく、同じシナリオ差から見た空間的共変動です。

## Figure 7：国別の生産ショックと貿易調整

`data/ALPHA/AGMIP_all_scenario.gdx`の国別CRPを使い、149か国について2100年の`MITI_PROT − MITI`を示します。Production、Consumption、Imports、Exportsはmillion kcalからPJへ換算し、国の食料作物消費量を基準に次を計算します。

\[
D_i=100\frac{PROD_{111}-PROD_{011}}{CONS_{011}}
\]

\[
T_i=100\frac{(IMPO_{111}-IMPO_{011})-(EXPO_{111}-EXPO_{011})}{CONS_{011}}
\]

ここで`D`は国内生産寄与、`T`は純輸入調整です。パネルaの破線`T=-D`に近い国ほど、国内生産変化を逆向きの貿易変化が相殺しています。パネルbは生産変化と消費変化を示し、色はMITIの輸入依存度`100 × IMPO_011 / CONS_011`、点の大きさは人口です。

パネルcでは、生産寄与、純輸入寄与、Other balance termsを積み上げ、黒い菱形で消費総変化を示します。Other balance termsは、

\[
R_i=100\frac{CONS_{111}-CONS_{011}}{CONS_{011}}-D_i-T_i
\]

という会計残差です。AgMIPの4変数だけで直接表さない在庫、加工、飼料、損失その他の需給項目を含み得るため、個別メカニズムとしては解釈しません。パネルdの縦軸は`-sign(D) × T`で、正値ほど貿易調整が国内生産ショックと反対向きです。図示対象のうち`|D| ≥ 0.5 percentage points`の国では、91.8%で純輸入調整が生産寄与と逆符号でした。これは貿易による完全な因果的吸収を意味せず、モデル内の同時均衡応答です。

## Figure 8：国別の土地応答と農業市場影響

国別土地面積は`data/PLUM/Mergedcomparison.gdx`の`IAMCTempwoU`、農業市場はFigure 7と同じAgMIP CRPを使います。2100年の`MITI_PROT − MITI`について、各土地変化をMITIの国土面積`Lan_Cov`で割ります。

- Additional protection：流域別`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`を国ごとに合計した値（国コード単位の同変数はAIM17地域の主要国にしかないため使いません）
- Planted forest：`Lan_Cov_Frs_Aff`
- Bioenergy：`Lan_Cov_Cro_Ene_Cro_2nd_gen`
- Food cropland：`Lan_Cov_Cro_Non_Ene_Cro`

パネルaはAdditional protection、Planted forest、Bioenergyのいずれかの絶対変化が大きい上位25か国を示します。パネルbは各土地応答と作物生産変化の散布図です。点の大きさは人口、色はR10地域、黒線は人口で重み付けした記述的な線形傾向です。

パネルcは国別の記述的回帰です。目的変数ごとにAdditional protection、Planted forest、Bioenergy、MITI輸入依存度を標準化し、R10固定効果を加え、`sqrt(MITI crop production)`を回帰ウェイトとして推定します。点は標準化係数、線は通常の標準誤差による95%区間です。パネルdは同じ変数間の重みなしSpearman順位相関です。回帰・相関は国の規模と地域差を考慮しながら候補関係を絞る探索的分析であり、保護区、CDR、農業影響の因果寄与を識別するものではありません。

今回の出力では、Additional protectionと生産変化の単純相関は弱い一方、Planted forestおよびFood croplandの変化は生産変化と正に対応しています。これは、保護面積そのものよりも、保護区導入後に生じる生産用地・CDR用地の再配置が国別生産差と結び付く可能性を示すため、Figure 3・6の空間診断と合わせて読みます。

## Figure 9：流域の追加保護と農業・土地応答

横軸は流域の追加保護面積（2100年の`MITI_PROT − MITI`、MITI流域面積に対するpercentage points）で、Figure 2・6と同じMergedComparisonの流域別保護面積と`qlTot_load`の流域総面積から計算します。縦軸は6つの応答で、いずれも同じ年の`MITI_PROT − MITI`です。

- Crop production（%）：Figure 6と同じ食料作物のエネルギー換算生産量の変化率
- Crop yield（%）：Figure 4と同じ、BAU収穫面積で重み付けした食料作物収量`yldBas_load`の変化率
- Producer price（%）：`pp_load`（国・品目別生産者価格）を流域のMITI生産量`qsBas_load`で重み付けしたLaspeyres指数の変化率。食料作物`food_crop_codes`のみを使い、価格は国レベルなので流域差は作物構成の差から生じます
- Net-import dependence（pp of consumption）：Figure 7のAgMIP CRPによる国別の純輸入量（輸入−輸出）／消費量の変化（percentage points）。国の値をその国の全流域に割り当てます
- Planted forest、Bioenergy cropland（pp of basin land）：Figure 6と同じ`qlAfr_load`と`qlBas_load`（`ecr`）の変化をMITI流域面積で割った値

点は流域（重み付けなし）、青線は通常最小二乗の回帰直線と95%信頼帯です。回帰直線と各パネルの数値（回帰の傾き（応答の単位／pp）、決定係数、傾きのp値、Spearman順位相関、流域数）は、外れ値を切り詰める前の全流域で計算します。表示では縦軸を`symmetric_limit`（絶対値の98パーセンタイル）で切り詰め、外側の点は境界に置きます。数値は`figure_9_basin_protection_responses.csv`と`figure_9_basin_regression_statistics.csv`に保存します。

これらは記述的な単回帰であり、流域の規模や地域差を調整していません。

### 加重版（Supplementary）

流域を等しく扱うと、生産量や面積の小さい流域の不安定な変化率が外れ値として回帰と分布を左右します。`supplementary/`には、農業応答（生産量、収量、生産者価格、純輸入依存度）をMITI作物生産量`crop_production_pj_011`、土地応答（植林、バイオエネルギー農地）をMITI流域面積`total_land_1000ha_011`で重み付けした3つの版を保存します。数値は加重最小二乗の傾き、決定係数、p値、流域数で、`supplementary_figure_9_weighted_statistics.csv`に保存します。

- `supplementary_figure_9_decile_bins.*`：追加保護の10分位ごとに加重平均とブートストラップ95%区間（1000回、`set.seed(20260917)`）を赤で示し、加重回帰直線を青、流域の点を灰色で重ねます。数値は`supplementary_figure_9_decile_bins.csv`
- `supplementary_figure_9_protection_classes.*`：追加保護の流域3分位（Low／Mid／High、境界は図中に表示）ごとの箱ひげ（重みなし、`symmetric_limit`で切り詰め）と加重平均±95%区間。数値は`supplementary_figure_9_protection_classes.csv`
- `supplementary_figure_9_bubble.*`：点の面積を重み（パネル内の最大流域を1とする相対値）にしたバブル図と加重回帰直線・95%信頼帯

## Figure 10：国別の保全地域拡大と農業市場応答

横軸はAgMIP対象149か国の追加保護面積（MergedComparisonの流域別`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`を国ごとに合計し、MITIの国土面積`Lan_Cov`で割ったpercentage points）、縦軸は2100年の`MITI_PROT − MITI`で、Crop production（%）、Net-import dependence（作物の（輸入−輸出）／消費のpp）、Food intake（作物由来の一人当たりカロリー摂取、%）の3パネルです。Figure 7・8と同じAgMIPの国別データ（`CRP`）を使います。点の面積はMITIの作物生産量（a）、消費量（b）、人口（c）で、青線はその重みによる加重最小二乗の回帰直線と95%信頼帯です。図中の決定係数`R²`とp値（傾きの検定）は加重回帰、Spearman順位相関`rho`は重みなしで、いずれも切り詰め前の149か国で計算します。加重回帰の傾きは`figure_10_country_statistics.csv`にあります。表示では縦軸を2–98パーセンタイルで切り詰め、各パネルの重みが大きい上位10か国にISO3コードを付けます。点の大きさの凡例は各パネルの内側に置きます。数値は`figure_10_country_responses.csv`と`figure_10_country_statistics.csv`に保存します。

回帰は国横断の記述統計であり、因果効果の推定ではありません。

### 補足：地域別の作物供給収支

`supplementary/supplementary_regional_supply_balance.*`は、R10地域ごとに作物消費量の変化（白丸、MITI消費量比%）を国内生産の変化（黄）と純輸入（輸入−輸出）の変化（青）に分けた図です。どちらもMITIの消費量で割ったpercentage pointsで、両者の和が消費量変化に一致します。数値は`supplementary_regional_supply_balance.csv`に保存します。

## 補足Shapley図

`supplementary/supplementary_shapley.*`は8つのfactorial scenarioを使い、Protection、Planted forest、BECCSのShapley寄与を示します。NoAff・NoBioシナリオを必要とするため、主結果ではなく要因分解・感度分析用の補足図として保存します。

Shapley分解では各政策を追加する全順序の限界効果を平均し、3寄与の合計が`MITI_PROT − BAU`に一致することをコードで検査します。

## 補足の双方向農地遷移図

`supplementary/supplementary_bidirectional_cropland_transitions.*`は、Figure 3本体の4区分に加え、Planted forest → cropland、Bioenergy → cropland、Other land → croplandを別々に配分した6区分版です。主図では食料農地を起点とする解釈を優先し、この双方向版を補足へ移しています。

## 補足のPLUM保護区政策入力とCDR収量入力

AIM/PLUMに与える0.5度グリッドの入力を、1変数1ファイルで`supplementary/`に保存します。保護区政策の値はいずれもセル面積に対する割合です。

| ファイル | 内容 |
|---|---|
| `supplementary_plum_protect_target.*` | `_protect`シナリオの保護対象割合。`min(1, protect_all + serious_land_allpolicy)`で、2030年SSP2人口密度`popdens`が0.1未満のセルは1 |
| `supplementary_plum_protect_additional.*` | 上記からBAUの保護対象割合を引いた追加保護割合。BAUは`min(1, protect_bs)`で、人口密度0.1未満のセルは同じく1 |
| `supplementary_plum_protect_all.*` | `protect_all`入力 |
| `supplementary_plum_serious_land_allpolicy.*` | `serious_land_allpolicy`入力 |
| `supplementary_plum_afforestation_yield.*` | 植林の炭素吸収速度`ACFout("AFRCUR", G)`。`LandUseModel_MCP.gms`の`afftype = ccur_vst`が選ぶ値で、単位はMg C ha⁻¹ yr⁻¹ |
| `supplementary_plum_bioenergy_yield.*` | バイオエネルギー作物収量`YIELDBIO_H08`。H08の天水ミスカンサスとスイッチグラスの収量平均をkg ha⁻¹からt ha⁻¹へ変換した値 |

人口密度による置換は`ALPHA/prog/0_grid_data.R`と同じ政策目標の構成です。PLUM本体では、人口密度0.1未満のセルは既存の一次・二次植生をすべて保護対象とし、さらに前期の土地配分で上限を課します。したがって保護区の図は上限適用前の政策目標であり、モデル内で実際に有効な`protectfrac`ではありません。収量図の上限は各変数の最大値です。グリッド値は`source_data/supplementary_plum_protection_inputs_grid.csv.gz`に保存します。

## 補足のAIM-BIO生物多様性指標

`data/AIMBIO/`のAIM-BIO出力から、種数、種構成類似度、ESHIの補足図を`supplementary/`に保存します。グリッド指標は60S–84Nの0.5度グリッドで、海洋セルは0が入っているためAIM-LUの陸地マスクを適用します。

| ファイル | 内容 |
|---|---|
| `supplementary_species_richness_<scenario>.*` | BAU、MITI、MITI_PROTの全分類群種数の地図。年は`TARGET_YEAR`以下で最も遅いAIM-BIO出力年（既定では2090年）。3枚で色尺度を共有 |
| `supplementary_species_richness_MITI_PROT_minus_MITI.*` | 同じ年のMITI_PROTとMITIの種数の差。色尺度の上限は絶対差の98パーセンタイルで対称 |
| `supplementary_compositional_similarity_*.*` | 種構成類似度について同じ構成の4枚。`ComSim/`にファイルがない場合は出力されない |
| `supplementary_biodiversity_trajectories_mean.*`、`_median.*` | 2030–2090年の各シナリオについて、全陸地セルの推移。`_mean`は平均（実線）、`_median`は中央値（実線）と25・75パーセンタイル（破線）。指標ごとに1パネル。種数はセルごとの現在比変化 `(NS_future − NS_current) / NS_current` で、負が減少。種構成類似度は現在との類似度そのもので、低いほど種の入れ替わりが大きい。`Current`層で種数が0のセル（氷床と極乾燥地）は統計から除く |
| `supplementary_eshi_regions.*` | `ESH.rel2005.geom.Pmigdisp.allspecies`の世界とR5地域別推移（BAU、MITI、MITI_PROT）。ESHIは2005年比の生息適地面積の幾何平均 |

グリッド値は`source_data/supplementary_species_richness_grid.csv.gz`（MITI、MITI_PROT、差分）に、推移は`supplementary_biodiversity_grid_trajectories.csv`と`supplementary_eshi_regions.csv`に保存します。

### 流域別の生物多様性変化と土地変化

`supplementary_basin_species_richness.*`は、MITI_PROT − MITIの種数変化を流域で集約し、Figure 6と同じ流域指標と結びます。セル値は `(NS_111 − NS_011) / NS_current`（現在種数比、%）、流域値は流域内の有効セルのセル面積加重平均です。流域はALPHA流域境界を0.5度グリッドに`touches = TRUE`でラスタ化して割り当てます。土地・生産指標は`build_basin_metrics()`を種数と同じ年（既定では2090年）で評価した値で、Figure 6と同じ定義です。

`supplementary_basin_compositional_similarity_mean.*`と`_median.*`は、種構成類似度（Jaccard）について同じ構成の図です。セル値は `100 × (ComSim_111 − ComSim_011)`（類似度のパーセントポイント）で、`_mean`は流域のセル面積加重平均、`_median`はセル面積加重の中央値を流域値とし、散布図には25–75パーセンタイルを縦線で添えます。セル単位の変化は大半が0なので、中央値は平均より0に近くなります。`ComSim/`にファイルがない場合は出力されません。

- パネルa：流域別の変化地図（対称尺度、絶対値の98パーセンタイル上限）
- パネルb：横軸に追加保護面積、植林、バイオエネルギー農地、食料農地、それらの正味合計（いずれも流域陸地比のpp）、および作物生産変化（%）をとった散布図。点の大きさはMITIの流域陸地面積、青線は単回帰、左上はSpearmanの`rho`と流域数。横軸は各指標の絶対値98パーセンタイルで切り詰めます

数値は`source_data/supplementary_basin_species_richness.csv`と`supplementary_basin_compositional_similarity_<mean|median>.csv`に保存します。列`change_mean`、`change_median`、`change_q25`、`change_q75`が流域のセル統計量です。

### 保全地域拡大の生物多様性効果（緩和策の有無別）

`supplementary_biodiversity_protection_effect.*`は、Figure 5bと同じ構成で、保全地域拡大の効果を緩和策なし（PROT − BAU）と緩和策あり（MITI_PROT − MITI）で比べます。年はAIM-BIO出力年（既定では2090年）です。

- パネルa：世界の5指標。ESHI（全種、絶滅危惧種`psthspecies`）は`data/AIMBIO/ESH/`の`World`行、BIIは`data/BII/BII/csv/BII_regionagg_<run>_NoCC_LandInt_global2_default_area.csv`の`World`行、種構成類似度と種数は有効セルの平均（種数は現在比変化）。指数は100倍してパーセントポイントで示す
- パネルb：種構成類似度と種数のセル平均を、Figure 2aの追加保護割合（≤10%、10–30%、>30%）で分けたもの。保護の内側で得た変化と外側で失った変化を分ける

数値は`source_data/supplementary_biodiversity_protection_effect.csv`に保存します。BIIのシナリオ名は`SSP2_BaU`、`SSP2_protect_BaU`、`SSP2_1100C_CACN_DAC`、`SSP2_protect_1100C_CACN_DAC`です。

### AIM/Hubの炭素価格とバイオエネルギー需要

`data/ALPHA/hub_data.gdx`から、PLUMへ渡されるAIM/Hubの炭素価格`Prc_car_hub`と第2世代バイオエネルギー作物需要`QENE_hub`を年次で示します。どちらもPLUMにとって外生なので、PROT系シナリオは保護なしの対応シナリオと同じ値になり、図には描きません。

`supplementary_hub_inputs.*`の1枚にまとめます。

- パネルa：炭素価格（USD/t CO2）。`Prc_car_hub`は全国で同一なので国平均を世界値とする。MITIの1本。BAUは炭素価格がなくGDXに含まれない。通貨の基準年はGDXに記載がない
- パネルb：第2世代バイオエネルギー作物需要（Mt）。Hubの17地域でファセットし、縦軸は地域ごとに自由。BAUとMITIの2本。`QENE_hub`は`all_scenario_World.gdx`の`qdBio_load`の`ecr`と一致するので、単位を1000 tとみなして1000で割る

数値は`source_data/supplementary_hub_carbon_price.csv`と`supplementary_hub_bioenergy_demand.csv`に保存します。

## 解釈上の注意

- すべての世界地図は南極を除き、南緯60度以北を表示します。
- Figure 2–3のグリッド結果は同一セル内の共変化であり、セル間移動を追跡したparcel flowではありません。
- AIM-LU NetCDFには名目上の0–1範囲をわずかに外れる土地割合が一部あります。世界・地域の正式面積には`Mergedcomparison.gdx`を用い、NetCDFは空間パターン診断に使います。
- 種数とLUC排出は政策名から符号を仮定せず、モデル出力値をそのまま集計します。
- Figure 1の土地積み上げは重複を避けた排他的区分です。Planted forestとprotected areaは既存土地被覆に重なるため別に表示します。
