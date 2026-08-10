# IAMC Land Variables

以下、`IAMC` 出力のうち特に land-use 系を、「何が入っているか」「どの技術・排出と結びつくか」で整理する。根拠は主に [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms)、[`AIMAFOLU/prog/Analysis_LUC.gms`](/C:/AFOLUB/AIMAFOLU/prog/Analysis_LUC.gms)、[`AIMAFOLU/prog/DataPreparation.gms`](/C:/AFOLUB/AIMAFOLU/prog/DataPreparation.gms)、技術マップ [`AIMAFOLU/individual/AFOLUCountry/map_tech_land.map`](/C:/AFOLUB/AIMAFOLU/individual/AFOLUCountry/map_tech_land.map)、[`AIMAFOLU/individual/AFOLUCountry/map_tech_red.map`](/C:/AFOLUB/AIMAFOLU/individual/AFOLUCountry/map_tech_red.map)、[`AIMAFOLU/individual/AFOLUCountry/map_tech_cost.map`](/C:/AFOLUB/AIMAFOLU/individual/AFOLUCountry/map_tech_cost.map)。

## 前提

- `LFEMF` 技術集約:
  `PL` (`Plantation`, 植林) = `PSR` + `PLR`
  `RF` (`Reforestation`, 再植林) = `RFS` + `RSS`
  `FP` (`Forest Protection`, 森林保全)
  `ENR` (`Enhanced natural regeneration`, 天然更新促進)
  `PFF` (`Prevention of forest fire`, 火災予防)
  `RIL` (`Reduced Impact Logging`, 低インパクト伐採)
  `WM` (`Water management on peatland`, 泥炭地水管理)
  `PR` (`Peatland restoration`, 泥炭地修復)
  `AGF` (`Agro-forestry`, アグロフォレストリー)
  `BCH` (`Biochar`, バイオ炭)
  `SCC` (`Soil carbon sequestration in cropland`, 農地土壌炭素)
  `SCG` (`Soil carbon sequestration in grassland`, 草地土壌炭素)
  根拠は [`AIMAFOLU/prog/DataPreparation.gms`](/C:/AFOLUB/AIMAFOLU/prog/DataPreparation.gms#L733)。

- 以前の話でいう「土地転換を伴う技術」は `LL` 側:
  `PL`, `RF`, `FP`, `ENR`, `PFF`
  逆に `RIL`, `WM`, `PR`, `AGF`, `BCH`, `SCC`, `SCG` は `LLC` 側で、同じ土地の上の管理改善。根拠は [`AIMAFOLU/prog/DataPreparation.gms`](/C:/AFOLUB/AIMAFOLU/prog/DataPreparation.gms#L592) と [`AIMAFOLU/prog/DataPreparation.gms`](/C:/AFOLUB/AIMAFOLU/prog/DataPreparation.gms#L613)。

## 1. ベースラインの土地状態と土地起源排出

これらは「このモデルのベースライン排出・面積」であり、削減量ではない。

### `Lan_Cov_*`

土地被覆のストック面積。`Area0` や `Land0` から直接作っている。

- 例:
  [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L167) から [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L182)
- 代表例:
  `Lan_Cov_Frs`, `Lan_Cov_Frs_Man`, `Lan_Cov_Frs_Unm`, `Lan_Cov_Cro`, `Lan_Cov_Pst`, `Lan_Cov_Oth_Lan`
- 技術別ではなく、土地利用状態そのもの
- 単位は `million ha` 系

### `Emi_CO2_AFO_Lan_*`, `Emi_equ_CH4_AFO_Lan_*`, `Emi_equ_N2O_AFO_Lan_*`

ベースラインの土地起源排出。`EMIT_BaU(...)` をそのまま使っている。

- 例:
  [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L252) から [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L261)
- 技術別ではなく IPCC カテゴリ別の排出
- 例:
  `Emi_CO2_AFO_Lan_For_Gra_Con` = 5B の土地利用転換排出
  `Emi_CO2_AFO_Lan_Aba_Man_Lan` = 5C
  `Emi_CO2_AFO_Lan_Emi_Rem_Soi` = 5D
  `Emi_CO2_AFO_Lan_For_Bur` = 5E
  `Emi_CO2_AFO_Lan_Pea_Fir_For` = 5F

### `Emi_CO2_Lan_Use_Flo_*`

land-use flow 系だが、中身が混ざるので注意。

- 定義:
  [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L264) から [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L268)

- `Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For`
  内容: managed forest の sequestration
  ソース: `EMIT_BaU(...,"5A1","CO2")`
  性質: ベースラインの land carbon flow

- `Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har`
  内容: wood harvest 由来
  ソース: `EMIT_BaU(...,"5A2","CO2")`
  性質: ベースラインの land carbon flow

- `Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha`
  内容: 土地利用転換排出
  ソース: `EMIT_BaU(...,"5B","CO2")`
  性質: ベースラインの land carbon flow

- `Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir`
  内容: fire 排出
  ソース: `EMIT_BaU(...,"5F","CO2")`
  性質: ベースラインの land carbon flow

- `Emi_CO2_Lan_Use_Flo_RemForest`
  内容: remaining forest のネット
  ソース: `Neg_Seq_Man_For + Pos_Emi_Woo_Har`
  集約定義: [`AIMAFOLU/define/map/aggregation.map`](/C:/AFOLUB/AIMAFOLU/define/map/aggregation.map#L127)

- `Emi_CO2_Lan_Use_Flo_Neg_Seq_Aff`
  これは例外で、ベースライン排出ではなく、技術導入による sequestration。
  ソース: `VDELTA_LQP_db(...,"PL") + VDELTA_LQP_db(...,"RF")`
  つまり `PL` (`植林`) と `RF` (`再植林`) の削減量だけを使っている。
  `ENR` は入らない。
  式は [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L266)。

## 2. 技術別の land 削減量

これらはベースラインではなく、LUC 技術導入による削減量。

- 生成ロジック:
  [`AIMAFOLU/prog/Analysis_LUC.gms`](/C:/AFOLUB/AIMAFOLU/prog/Analysis_LUC.gms#L85) で `VDELTA_LQP_db`
  [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L286) で IAMC 変数に展開

- 中身:
  `VDELTA_LQP_db` は「時点 `T` に見える、各技術の cumulative mitigation」。
  コメント上も
  `Cumulative mitigation from present to the future by countermeasure l applied in the year t`
  である。
  ただし IAMC 出力にすると単位は `MtCO2eq/yr` で、年 `T` における技術別削減フローとして使われている。

### land 系 IAMC 変数と技術対応

- `Emi_Red_CO2_AFO_Lan_Pla` -> `PL` (`PSR` + `PLR`)
- `Emi_Red_CO2_AFO_Lan_Ref` -> `RF` (`RFS` + `RSS`)
- `Emi_Red_CO2_AFO_Lan_FPr` -> `FP`
- `Emi_Red_CO2_AFO_Lan_Pff` -> `PFF`
- `Emi_Red_CO2_AFO_Lan_Ril` -> `RIL`
- `Emi_Red_CO2_AFO_Lan_Enr` -> `ENR`
- `Emi_Red_CO2_AFO_Lan_Agf` -> `AGF`
- `Emi_Red_CO2_AFO_Lan_Wmp` -> `WM`
- `Emi_Red_CO2_AFO_Lan_Prr` -> `PR`
- `Emi_Red_CO2_AFO_Lan_Bch` -> `BCH`
- 条件付き:
  `SCC/SCG` は `plumauto` により
  `Car_Seq_Lan_Use_Soi_Car_Man_*` か `Emi_Red_CO2_AFO_Lan_Scc/Scg`
  として出る。
  根拠: [`AIMAFOLU/individual/AFOLUCountry/map_tech_red.map`](/C:/AFOLUB/AIMAFOLU/individual/AFOLUCountry/map_tech_red.map)

### 集約系

- `Emi_Red_CO2_AFO_Lan`
  は各 land 技術の合計。集約定義は [`AIMAFOLU/define/map/aggregation.map`](/C:/AFOLUB/AIMAFOLU/define/map/aggregation.map#L118) 以降。
- `Emi_Red_CO2_AFO_Lan_Pro`
  は `FP + PFF`
- `Emi_Red_CO2_AFO_Lan_Man`
  は `RIL + WM + PR`
- `Emi_Red_CO2_AFO_Lan_Res`
  は `ENR + PL + RF`
  根拠は [`AIMAFOLU/define/map/aggregation.map`](/C:/AFOLUB/AIMAFOLU/define/map/aggregation.map#L112) から [`AIMAFOLU/define/map/aggregation.map`](/C:/AFOLUB/AIMAFOLU/define/map/aggregation.map#L150)。

## 3. 技術別の土地面積

これは land 技術ごとの適用面積。ただし「年当たりの新規導入面積」ではなく、「その年に有効な累積適用面積」。

- 生成ロジック:
  [`AIMAFOLU/prog/AFOLUmodel_LUC.gms`](/C:/AFOLUB/AIMAFOLU/prog/AFOLUmodel_LUC.gms#L1018) の `AACM_out`
  コメントも `Cumulative area ... in the year t`
  [`AIMAFOLU/prog/Analysis_LUC.gms`](/C:/AFOLUB/AIMAFOLU/prog/Analysis_LUC.gms#L89) で `AACM_db`
  [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L288) で IAMC 出力

### 対応変数

- `Lan_Are_Pla` -> `PL`
- `Lan_Are_Ref` -> `RF`
- `Lan_Are_FPr` -> `FP`
- `Lan_Are_Pff` -> `PFF`
- `Lan_Are_Ril` -> `RIL`
- `Lan_Are_Enr` -> `ENR`
- `Lan_Are_Agf` -> `AGF`
- `Lan_Are_Wmp` -> `WM`
- `Lan_Are_Prr` -> `PR`
- `Lan_Are_Bch` -> `BCH`
- `Lan_Are_Scc` -> `SCC`
- `Lan_Are_Scg` -> `SCG`
  根拠: [`AIMAFOLU/individual/AFOLUCountry/map_tech_land.map`](/C:/AFOLUB/AIMAFOLU/individual/AFOLUCountry/map_tech_land.map)

### 注意

- `Lan_Are_*` は `AACM_db` 由来なので cumulative active area
- 「その年に新たに転換した面積」ではない
- 単位ラベルは [`AIMAFOLU/define/map/VU.map`](/C:/AFOLUB/AIMAFOLU/define/map/VU.map#L347) で `million ha/yr` になっているが、式の実体は annual new area ではなく、年 `T` 時点で効いている累積適用面積

### 本当に年当たりの新規適用面積を見たい場合

- `VLALL_out` / `VLALL_db` を見る必要がある
  [`AIMAFOLU/prog/AFOLUmodel_LUC.gms`](/C:/AFOLUB/AIMAFOLU/prog/AFOLUmodel_LUC.gms#L1019) と [`AIMAFOLU/prog/Analysis_LUC.gms`](/C:/AFOLUB/AIMAFOLU/prog/Analysis_LUC.gms#L90)
- ただしこれは IAMC 変数には落としていない
- つまり、IAMC では cumulative area は出るが、annual implementation area は標準では出ていない

## 4. どれが土地転換技術に対応しているか

土地転換技術 `LL` に対応する IAMC 技術変数は次。

- `Emi_Red_CO2_AFO_Lan_Pla` (`PL`, 植林)
- `Emi_Red_CO2_AFO_Lan_Ref` (`RF`, 再植林)
- `Emi_Red_CO2_AFO_Lan_FPr` (`FP`, 森林保全)
- `Emi_Red_CO2_AFO_Lan_Enr` (`ENR`, 天然更新促進)
- `Emi_Red_CO2_AFO_Lan_Pff` (`PFF`, 火災予防)

面積側は対応して

- `Lan_Are_Pla`
- `Lan_Are_Ref`
- `Lan_Are_FPr`
- `Lan_Are_Enr`
- `Lan_Are_Pff`

このうち、「土地転換面積そのもの」を IAMC で出しているのは `Lan_Are_*` だが、上で書いた通り cumulative active area であり、新規転換フローではない。新規の土地転換面積を見たいなら `VLALL_db` か、さらに元の `VDELTA_LA_out` / `VDELTA_RLA_out` を見る必要がある。

## 5. 実務上の読み方

land 系 IAMC 変数は次の3種類に分けると分かりやすい。

- ベースラインの土地状態:
  `Lan_Cov_*`

- ベースラインの土地起源排出・フロー:
  `Emi_CO2_AFO_Lan_*`
  `Emi_equ_CH4_AFO_Lan_*`
  `Emi_equ_N2O_AFO_Lan_*`
  `Emi_CO2_Lan_Use_Flo_*`
  ただし `Emi_CO2_Lan_Use_Flo_Neg_Seq_Aff` だけは技術削減由来

- 技術導入の結果:
  削減量
  `Emi_Red_CO2_AFO_Lan_*`
  `Emi_Red_equ_GHG_AFO_Lan`
  など

  面積
  `Lan_Are_*`
  ただし cumulative active area

  コスト
  `Red_Cos_AFO_Lan_*`

## 6. 注意点

一つ気づいた点として、[`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L178) と [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms#L179) では `Lan_Cov_Frs_Unm` が `flmngnp` で計算されていて、`Unmanaged` なのに managed forest と同じ式になっている。本来は `flumnnp` ではないか、という疑いがある。今回の整理の本筋ではないので未修正だが、出力確認時には注意したほうがよい.

## 技術別対応表

| LFEMF | 技術名 | モデル分類 | 土地転換を伴うか | 削減量 IAMC 変数 | 面積 IAMC 変数 | コスト IAMC 変数 | 備考 |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `PL` | Plantation (植林) | `PSR` + `PLR` | 伴う (`LL`) | `Emi_Red_CO2_AFO_Lan_Pla` | `Lan_Are_Pla` | `Red_Cos_AFO_Lan_Pla` | `Emi_CO2_Lan_Use_Flo_Neg_Seq_Aff` に含まれる |
| `RF` | Reforestation (再植林) | `RFS` + `RSS` | 伴う (`LL`) | `Emi_Red_CO2_AFO_Lan_Ref` | `Lan_Are_Ref` | `Red_Cos_AFO_Lan_Ref` | `Emi_CO2_Lan_Use_Flo_Neg_Seq_Aff` に含まれる |
| `FP` | Forest Protection (森林保全) | `FP` | 伴う扱い (`LL`) | `Emi_Red_CO2_AFO_Lan_FPr` | `Lan_Are_FPr` | `Red_Cos_AFO_Lan_FPr` | 集約では `Pro` に入る |
| `ENR` | Enhanced natural regeneration (天然更新促進) | `ENR` | 伴う (`LL`) | `Emi_Red_CO2_AFO_Lan_Enr` | `Lan_Are_Enr` | `Red_Cos_AFO_Lan_Enr` | `Aff` には含まれない |
| `PFF` | Prevention of forest fire (火災予防) | `PFF` | 伴う扱い (`LL`) | `Emi_Red_CO2_AFO_Lan_Pff` | `Lan_Are_Pff` | `Red_Cos_AFO_Lan_Pff` | 集約では `Pro` に入る |
| `RIL` | Reduced Impact Logging (低インパクト伐採) | `RIL` | 伴わない (`LLC`) | `Emi_Red_CO2_AFO_Lan_Ril` | `Lan_Are_Ril` | `Red_Cos_AFO_Lan_Ril` | 集約では `Man` に入る |
| `WM` | Water management on peatland (泥炭地水管理) | `WM` | 伴わない (`LLC`) | `Emi_Red_CO2_AFO_Lan_Wmp` | `Lan_Are_Wmp` | `Red_Cos_AFO_Lan_Wmp` | 集約では `Man` に入る |
| `PR` | Peatland restoration (泥炭地修復) | `PR` | 伴わない (`LLC`) | `Emi_Red_CO2_AFO_Lan_Prr` | `Lan_Are_Prr` | `Red_Cos_AFO_Lan_Prr` | 集約では `Man` に入る |
| `AGF` | Agro-forestry (アグロフォレストリー) | `AGF` | 伴わない (`LLC`) | `Emi_Red_CO2_AFO_Lan_Agf` | `Lan_Are_Agf` | `Red_Cos_AFO_Lan_Agf` | `AFO_Agr_Seq` 側にも集約 |
| `BCH` | Biochar (バイオ炭) | `BCH` | 伴わない (`LLC`) | `Emi_Red_CO2_AFO_Lan_Bch` | `Lan_Are_Bch` | `Red_Cos_AFO_Lan_Bch` | `AFO_Agr_Seq` 側にも集約 |
| `SCC` | Soil carbon sequestration in cropland (農地土壌炭素) | `SCC` | 伴わない (`LLC`) | `Emi_Red_CO2_AFO_Lan_Scc` または `Car_Seq_Lan_Use_Soi_Car_Man_Cro` | `Lan_Are_Scc` | `Red_Cos_AFO_Lan_Scc` | `plumauto` により出力名が分岐 |
| `SCG` | Soil carbon sequestration in grassland (草地土壌炭素) | `SCG` | 伴わない (`LLC`) | `Emi_Red_CO2_AFO_Lan_Scg` または `Car_Seq_Lan_Use_Soi_Car_Man_Gra` | `Lan_Are_Scg` | `Red_Cos_AFO_Lan_Scg` | `plumauto` により出力名が分岐 |

## 変数カテゴリ表

| 変数カテゴリ | 代表 IAMC 変数 | 中身 | ベースラインか削減量か | 技術別か | 面積の意味 | 備考 |
| --- | --- | --- | --- | --- | --- | --- |
| Land cover | `Lan_Cov_Frs`, `Lan_Cov_Cro`, `Lan_Cov_Pst` | `Land0` / `Area0` 由来の土地被覆ストック | ベースライン | いいえ | ストック面積 | 状態変数 |
| Land CO2 emissions | `Emi_CO2_AFO_Lan_*` | `EMIT_BaU` による土地起源 CO2 排出 | ベースライン | いいえ | 該当なし | IPCC カテゴリ別 |
| Land CH4/N2O emissions | `Emi_equ_CH4_AFO_Lan_*`, `Emi_equ_N2O_AFO_Lan_*` | `EMIT_BaU` による土地起源 CH4/N2O 排出 | ベースライン | いいえ | 該当なし | 森林火災・泥炭火災など |
| Land-use flow | `Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For`, `Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha` | managed forest 吸収、木材収穫、土地利用転換、火災など | ほぼベースライン | いいえ | 該当なし | `Neg_Seq_Aff` だけは技術削減由来 |
| Afforestation / Reforestation flow | `Emi_CO2_Lan_Use_Flo_Neg_Seq_Aff` | `PL` + `RF` の削減量 | 削減量 | 部分的 | 該当なし | `ENR` は含まれない |
| Land mitigation by technology | `Emi_Red_CO2_AFO_Lan_Pla`, `..._Ref`, `..._Enr` | `VDELTA_LQP_db` 由来の技術別削減量 | 削減量 | はい | 該当なし | 年 `T` に見える技術別削減フロー |
| Land mitigation aggregate | `Emi_Red_CO2_AFO_Lan`, `..._Pro`, `..._Man`, `..._Res` | 技術別削減量の集約 | 削減量 | 半分 | 該当なし | `Pro=FP+PFF`, `Man=RIL+WM+PR`, `Res=ENR+PL+RF` |
| Land area by technology | `Lan_Are_Pla`, `Lan_Are_Ref`, `Lan_Are_Enr` | `AACM_db` 由来の技術別適用面積 | 削減結果 | はい | 累積適用面積 | 新規導入面積ではない |
| Land cost by technology | `Red_Cos_AFO_Lan_Pla`, `..._Ref`, `..._Enr` | `VLCP_db` 由来の技術別コスト | 削減結果 | はい | 該当なし | `Million $/yr` |
| Annual new implementation area | IAMC 標準では未出力 | `VLALL_db` が元データ | 削減結果 | はい | 年当たり新規適用面積 | IAMC 変数には落としていない |

## ベースライン排出の階層構造

以下は、`IAMC_templatePlus.gdx` に入る土地利用部門のベースライン排出 (`_Lan_*`) について、どの変数がどれを含むかを整理したものです。実体の定義は [`AIMAFOLU/prog/iiasa_database.gms`](/C:/AFOLUB/AIMAFOLU/prog/iiasa_database.gms) にあり、上位カテゴリへの加法関係は主に [`AIMAFOLU/define/map/aggregation.map`](/C:/AFOLUB/AIMAFOLU/define/map/aggregation.map) で決まっています。

### IPCCカテゴリの意味

- `5A1` (`Managed forest sequestration`, 管理森林による吸収)
- `5A2` (`Wood harvest`, 木材収穫に伴う排出・吸収変化)
- `5B` (`Forest and grassland conversion`, 森林・草地転換)
- `5C` (`Abandonment of managed lands`, 管理地の放棄)
- `5D` (`Emissions and removals from soil`, 土壌からの排出・吸収)
- `5E` (`Biomass burning / forest burning`, 森林バーニング)
- `5F` (`Peat fire / peatland fire`, 泥炭火災)
- `TOT` (`Total`, land部門CO2合計)

### 変数の直接定義

- `Emi_CO2_AFO_Lan_Cha_For_Woo` (`CO2 land emissions from changes in forests and wood harvest`, 森林変化・木材収穫由来CO2)  
  `= 5A1 + 5A2`
- `Emi_CO2_AFO_Lan_For_Gra_Con` (`CO2 from forest and grassland conversion`, 森林・草地転換由来CO2)  
  `= 5B`
- `Emi_CO2_AFO_Lan_Aba_Man_Lan` (`CO2 from abandonment of managed lands`, 管理地放棄由来CO2)  
  `= 5C`
- `Emi_CO2_AFO_Lan_Emi_Rem_Soi` (`CO2 emissions/removals from soils`, 土壌由来CO2)  
  `= 5D`
- `Emi_CO2_AFO_Lan_For_Bur` (`CO2 from forest burning`, 森林バーニング由来CO2)  
  `= 5E` の CO2 成分
- `Emi_equ_CH4_AFO_Lan_For_Bur` (`CH4eq from forest burning`, 森林バーニング由来CH4のCO2換算)  
  `= 5E` の CH4 成分
- `Emi_equ_N2O_AFO_Lan_For_Bur` (`N2Oeq from forest burning`, 森林バーニング由来N2OのCO2換算)  
  `= 5E` の N2O 成分
- `Emi_CO2_AFO_Lan_Pea_Fir_For` (`CO2 from peat fire`, 泥炭火災由来CO2)  
  `= 5F` の CO2 成分
- `Emi_equ_CH4_AFO_Lan_Pea_Fir_For` (`CH4eq from peat fire`, 泥炭火災由来CH4のCO2換算)  
  `= 5F` の CH4 成分
- `Emi_equ_N2O_AFO_Lan_Pea_Fir_For` (`N2Oeq from peat fire`, 泥炭火災由来N2OのCO2換算)  
  `= 5F` の N2O 成分
- `Emi_CO2_AFO_Lan` (`Total CO2 emissions in AFOLU land sector`, 土地利用部門CO2合計)  
  `= TOT`

### aggregation.map 上の階層構造

```text
Emi_equ_GHG_AFO (AFOLU全体GHG排出)
= Emi_equ_GHG_AFO_Agr (農業部門GHG排出)
+ Emi_equ_GHG_AFO_Lan (土地利用部門GHG排出)

Emi_equ_GHG_AFO_Lan (土地利用部門GHG排出)
= Emi_CO2_AFO_Lan (土地利用部門CO2合計)

Emi_CO2_AFO_Lan (土地利用部門CO2合計)
= Emi_CO2_AFO_Lan_For_Gra_Con (森林・草地転換由来CO2)
+ Emi_CO2_AFO_Lan_Aba_Man_Lan (管理地放棄由来CO2)
+ Emi_CO2_AFO_Lan_Emi_Rem_Soi (土壌由来CO2)
+ Emi_CO2_AFO_Lan_For_Bur (森林バーニング由来CO2)
+ Emi_CO2_AFO_Lan_Pea_Fir_For (泥炭火災由来CO2)

Emi_equ_CH4_AFO_Lan (土地利用部門CH4排出のCO2換算)
= Emi_equ_CH4_AFO_Lan_For_Bur (森林バーニング由来CH4)
+ Emi_equ_CH4_AFO_Lan_Pea_Fir_For (泥炭火災由来CH4)

Emi_equ_N2O_AFO_Lan (土地利用部門N2O排出のCO2換算)
= Emi_equ_N2O_AFO_Lan_For_Bur (森林バーニング由来N2O)
+ Emi_equ_N2O_AFO_Lan_Pea_Fir_For (泥炭火災由来N2O)
```

### 注意点

- `Emi_CO2_AFO_Lan_Cha_For_Woo` (`森林変化・木材収穫由来CO2`) は直接定義されていますが、[`AIMAFOLU/define/map/aggregation.map`](/C:/AFOLUB/AIMAFOLU/define/map/aggregation.map#L61) では `Emi_CO2_AFO_Lan` の子としてはコメントアウトされています。したがって、現行の map ベースの加法関係では `Emi_CO2_AFO_Lan` に含まれません。
- `Emi_equ_GHG_AFO_Lan` (`土地利用部門GHG排出`) も、[`AIMAFOLU/define/map/aggregation.map`](/C:/AFOLUB/AIMAFOLU/define/map/aggregation.map#L102) では `Emi_CO2_AFO_Lan` のみを含み、`Emi_equ_CH4_AFO_Lan` と `Emi_equ_N2O_AFO_Lan` はコメントアウトされています。したがって、変数名の印象とは異なり、現行の階層上は land 部門の CO2 部分だけを持っています。
- そのため、現行コードで安全に読むなら、`Emi_equ_GHG_AFO_Lan` (`土地利用部門GHG排出`) は実質 `Emi_CO2_AFO_Lan` (`土地利用部門CO2合計`) と同義です。一方、land 部門の CH4 と N2O は、それぞれ `Emi_equ_CH4_AFO_Lan` と `Emi_equ_N2O_AFO_Lan` を別に見る必要があります。
