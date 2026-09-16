# 本番図8枚の再現手順と定義

`make_main_figures.R`はFigure 1–8と補足用Shapley図を一括作成する独立したRスクリプトです。`prog/`以下の探索用コードは読み込まず、GDX・NetCDF・shapefileを直接処理します。図中はパネル記号、短い見出し、軸、単位、凡例に限定し、詳細な定義をこの文書にまとめています。

## 実行方法

`Labo/260918Interim`で実行します。

```bash
Rscript publication/make_main_figures.R
```

既定の評価年は2100年です。2050年版は次のように作成できます。

```bash
TARGET_YEAR=2050 Rscript publication/make_main_figures.R
```

出力先は`output/publication_figures/`です。

- `figure_1/`–`figure_8/`：600 dpi PNGとベクターPDF
- `supplementary/`：NoAff・NoBioを使う補足Shapley図と双方向農地遷移図
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

- `data/PLUM/Mergedcomparison.gdx`
  - `IAMCTempwoU`：世界・R10の土地面積
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
- `data/BII/nc/BIImap_*_default.nc`
  - `BII`：グリッド別Biodiversity Intactness Index
- `data/Basin_Boundary/Basin_Boundary.shp`
  - ALPHA流域境界

ALPHAの食料作物は`wht, rce, crl, mze, str, sgr, pls, nut, ocr, vgt, frt, stm, spc`です。エネルギー作物`ecr`は含めません。AgMIPも集約済み食料作物群`CRP`を使うため、農業市場図に`ecr`は入りません。

## Figure 1：土地システム全体

### a 世界の土地利用推移

`IAMCTempwoU`から次の世界値を抽出し、BAU、MITI、MITI_PROTを比較します。

- Food cropland：`Lan_Cov_Cro_Non_Ene_Cro`
- Bioenergy cropland：`Lan_Cov_Cro_Ene_Cro_2nd_gen`
- Planted forest：`Lan_Cov_Frs_Aff`

単位はmillion haです。

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

### e BII

MITIとMITI_PROTそれぞれの評価年BIIをセル面積で加重した世界平均です。差分だけでなく、両シナリオの水準を並べています。

## Figure 2：流域別の追加保護率と土地応答

パネルaの`Added protected share by basin`は、`protPlum`に含まれるprimary forest (`primf`)、secondary forest (`secdf`)、grassland (`gra`)の保護割合について、流域内で3区分を単純平均し、`MITI_PROT − MITI`を取った値です。単位はpercentage pointsです。

percentage pointsは割合どうしの差です。たとえば保護割合が10%から12%になった場合は+2 percentage pointsであり、初期値に対する相対変化率では+20%です。

これは「対象3植生における平均的な追加保護割合」であり、流域総面積に占める保護区割合や0.5度の直接保護区マスクではありません。

正の追加保護率を0.5度グリッド上の三分位でLow、Mid、Highに分けます。凡例には実際に計算されたLow–Mid境界とMid–High境界をpercentage pointsで表示し、同じ値を`figure_2_class_thresholds.csv`に保存します。土地応答はAIM-LUのグリッド差`MITI_PROT − MITI`です。現在のNetCDFには流域値とR17集計値の重複があるため、土地割合は2で割ってから差分を計算します。

- Decrease：−0.1 percentage points未満
- Stable：±0.1 percentage points以内
- Increase：+0.1 percentage points超

色相が土地応答、濃さが追加保護率階級を示します。凡例の縦軸にもDecrease、Stable、Increaseの数値境界をpercentage pointsで表示します。応答地図はPlanted forest、Bioenergy cropland、Food cropland、Natural landの4つです。Natural landはAIM-LUのnaturally regenerating forest（class 4）とnatural grassland + other natural land（class 9）の合計です。

この4枚の土地応答地図をまとめてパネルbとし、個別のパネル記号は付けません。

## Figure 3：食料農地の再配置

MITIからMITI_PROTへの変化について、食料農地を起点として次の4シグナルを示します。

現在のAIM-LU NetCDFは、流域値とそのR17集計値がグリッド出力時に重複加算されているため、土地割合を2で割って使用します。これは修正済み`combine.gms`でNetCDFを再作成するまでの暫定処理です。NetCDF class 1は`CL + CROP_FLW`であり、Figure 3のCroplandには食料生産農地と休閑地の両方が含まれます。MergedComparisonへの比例補正は行いません。

MergedComparisonには休閑地だけを示す独立したIAMC変数はありませんが、`Lan_Cov_Cro - Lan_Cov_Cro_Non_Ene_Cro - Lan_Cov_Cro_Ene_Cro_2nd_gen`として計算できます。したがって、NetCDF class 1と対応する集計量は`Lan_Cov_Cro - Lan_Cov_Cro_Ene_Cro_2nd_gen`です。ただし、グリッド出力とMergedComparisonでは集計経路が異なるため、既存NetCDFを2で割った世界合計がMergedComparisonと完全に一致するとは限りません。

- Cropland → planted forest
- Cropland → bioenergy
- Cropland → Other
- Newly expanded cropland

食料農地減少を\(L_i\)、植林・バイオ増加を\(F_i,B_i\)とすると、負側の対応量は、

\[
M_i^- = \min(L_i,F_i+B_i),\qquad
C_{i,F}^- = M_i^-\frac{F_i}{F_i+B_i},\qquad
C_{i,B}^- = M_i^-\frac{B_i}{F_i+B_i}
\]

です。残差を`Cropland → Other`とします。Newly expanded croplandは、植林・バイオの減少元へ細分せず、食料農地が増えたセルの増加量をそのまま示します。

パネルaは各セルで最大の遷移シグナルだけを色相で示します。区分名はCropland → planted forest、Cropland → bioenergy、Cropland → Other、Newly expanded croplandです。色の濃さはグリッド面積割合の変化（percentage points）です。4区分に共通するスケールを使い、上限は正の変化量を全区分でプールした98パーセンタイルです。小さい変化を読み取れるよう色濃度だけを平方根変換し、凡例の数値軸は元のpercentage-point値で示します。上限を超える値は端色に丸めます。

MITIまたはMITI_PROTで食料農地割合が1%以上あり、両シナリオ間の食料農地変化が±0.1 percentage points以内のセルをStable croplandとしてクリーム色で表示します。これにより、灰色のCropland → Otherと区別します。遷移として表示する閾値も0.1 percentage pointsです。共通上限値は`figure_3_food_transition_summary.csv`の`display_limit_pp`に保存します。パネルbは`pixel_area`を掛けた世界面積集計で、農地減少を負、農地増加を正、黒点をネット変化として示します。平方根変換は図の軸名ではなく本説明に記録しています。

この分解は同一セル内の共変化を整合的に配分した診断量です。個々の土地parcelを追跡した実測的な遷移行列ではありません。

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

主図はBAU、MITI、MITI_PROTだけを使います。

- a：世界のProduction、Producer price、Food intakeについて、MITIとMITI_PROTのBAU比
- b–d：R10のProduction、Producer price、Food intakeのBAU比

地域図は青いMITI棒と朱色のMITI_PROT棒を地域ごとに並べます。Tradeは主図から除外しています。

- Production、Consumption、Exports、Imports、Net exports：`million kcal × 4.184 × 10⁻⁶`でPJ yr⁻¹へ変換
- Producer price：`USD / (million kcal × 4.184)`でUSD GJ⁻¹
- Food intake：元の`kcal cap⁻¹ day⁻¹`を維持

## Figure 6：流域の土地圧力と作物生産の再配置

この図は、緩和策を共通条件にして保護区拡大を追加したときの流域応答を、すべて2100年の`MITI_PROT − MITI`として示します。土地面積と生産量は`data/ALPHA/all_scenario_World.gdx`から読みます。

- 流域総面積：`qlTot_load`
- 保護面積：`qlProt_load`
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

- Additional protection：`Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int`
- Planted forest：`Lan_Cov_Frs_Aff`
- Bioenergy：`Lan_Cov_Cro_Ene_Cro_2nd_gen`
- Food cropland：`Lan_Cov_Cro_Non_Ene_Cro`

パネルaはAdditional protection、Planted forest、Bioenergyのいずれかの絶対変化が大きい上位25か国を示します。パネルbは各土地応答と作物生産変化の散布図です。点の大きさは人口、色はR10地域、黒線は人口で重み付けした記述的な線形傾向です。

パネルcは国別の記述的回帰です。目的変数ごとにAdditional protection、Planted forest、Bioenergy、MITI輸入依存度を標準化し、R10固定効果を加え、`sqrt(MITI crop production)`を回帰ウェイトとして推定します。点は標準化係数、線は通常の標準誤差による95%区間です。パネルdは同じ変数間の重みなしSpearman順位相関です。回帰・相関は国の規模と地域差を考慮しながら候補関係を絞る探索的分析であり、保護区、CDR、農業影響の因果寄与を識別するものではありません。

今回の出力では、Additional protectionと生産変化の単純相関は弱い一方、Planted forestおよびFood croplandの変化は生産変化と正に対応しています。これは、保護面積そのものよりも、保護区導入後に生じる生産用地・CDR用地の再配置が国別生産差と結び付く可能性を示すため、Figure 3・6の空間診断と合わせて読みます。

## 補足Shapley図

`supplementary/supplementary_shapley.*`は8つのfactorial scenarioを使い、Protection、Planted forest、BECCSのShapley寄与を示します。NoAff・NoBioシナリオを必要とするため、主結果ではなく要因分解・感度分析用の補足図として保存します。

Shapley分解では各政策を追加する全順序の限界効果を平均し、3寄与の合計が`MITI_PROT − BAU`に一致することをコードで検査します。

## 補足の双方向農地遷移図

`supplementary/supplementary_bidirectional_cropland_transitions.*`は、Figure 3本体の4区分に加え、Planted forest → cropland、Bioenergy → cropland、Other land → croplandを別々に配分した6区分版です。主図では食料農地を起点とする解釈を優先し、この双方向版を補足へ移しています。

## 解釈上の注意

- すべての世界地図は南極を除き、南緯60度以北を表示します。
- Figure 2–3のグリッド結果は同一セル内の共変化であり、セル間移動を追跡したparcel flowではありません。
- AIM-LU NetCDFには名目上の0–1範囲をわずかに外れる土地割合が一部あります。世界・地域の正式面積には`Mergedcomparison.gdx`を用い、NetCDFは空間パターン診断に使います。
- BIIとLUC排出は政策名から符号を仮定せず、モデル出力値をそのまま集計します。
- Figure 1の土地積み上げは重複を避けた排他的区分です。Planted forestとprotected footprintは既存土地被覆に重なるため別に表示します。
