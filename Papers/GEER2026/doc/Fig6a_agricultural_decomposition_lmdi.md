# Fig6a 農業部門排出変化の Kaya 型恒等式 + LMDI 分解

本メモは、`Papers/GEER2026/prog/paperV3.R` に追加した LMDI ベースの農業部門排出分解図

- `fig6a_agricultural_decomposition_lmdi_2100.png`

および対応する csv

- `fig6a_world_r5_decomposition_lmdi_summary_2100.csv`
- `fig6a_top_bottom_country_decomposition_lmdi_summary_2100.csv`

について、**何を、どのデータから、どの式で求めているか**を、再現できる粒度で説明したものである。

この図は、従来の midpoint 型の 3 ウェッジ分解とは別に、**Kaya 型の恒等式を置いた上で、加法型 LMDI によって residual なしに 3 要因へ分解する版**である。

---

## 1. 目的

本分解の目的は、BaU から MITI への農業部門非 CO\(_2\) 排出変化を、以下の 3 要因に分けることである。

- `Demand side`
- `Structural`
- `Technical`

ここで、

- `Demand side` は国内需要量の変化、
- `Structural` は国内需要に対する国内供給比率の変化、すなわち貿易変化・国内供給比率変化・生産アロケーション変化を含む構造変化、
- `Technical` は排出係数の改善、

を意味する。

今回の LMDI 版では `Other` や `Residual` は作らず、**3 要因だけで完結する分解**として実装している。

---

## 2. 使用データ

使用している入力データは、`Papers/GEER2026/data/0316/` 以下の以下の GDX である。

### 2.1 `all_scenario_World.gdx`

使用変数:

- `QDEM_load`
- `QSUP_load`
- `RR_load`
- `GHG_load`

### 2.2 `baseyear.gdx`

使用変数:

- `EFbase`

### 2.3 補助ファイル

- `Papers/GEER2026/define/country.set`
- `Papers/GEER2026/define/region.map`

これらは、対象国の抽出と R5 集計に用いている。

---

## 3. シナリオ定義

分解に使う 2 つのシナリオは次である。

- BaU: `SSP2_BaU_NoCC_No`
- MITI: `SSP2_1100C_CACN_DAC_NoCC_LandInt_global1`

コード中では次のように定義している。

```r
scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint  <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
```

---

## 4. 分解の最小単位

分解はまず最小単位セルごとに行い、その後に国別・地域別・世界計へ集計する。

最小単位は

\[
i = (r, c, s, g, t)
\]

であり、ここで

- \(r\): 国
- \(c\): 財
- \(s\): 排出源
- \(g\): ガス種
- \(t\): 年

を表す。

対象ガスは `CH4` と `N2O` である。

---

## 5. Kaya 型恒等式

今回の LMDI 版では、農業部門非 CO\(_2\) 排出を、あなたのモデル変数に合わせて次のように書く。

\[
E_i = Q_i \cdot M_i \cdot EF_i
\]

ここで、

\[
Q_i^k = QDEM_{rct}^k
\]

\[
M_i^k = \frac{QSUP_{rct}^k}{QDEM_{rct}^k}
\]

\[
EF_i^k = EF_{rscgt}^k
\]

であり、\(k\) はシナリオを表し、

- \(k=0\): BaU
- \(k=1\): MITI

である。

このとき、

\[
Q_i^k \cdot M_i^k
=
QDEM_{rct}^k \cdot \frac{QSUP_{rct}^k}{QDEM_{rct}^k}
=
QSUP_{rct}^k
\]

なので、式全体は

\[
E_i^k = QSUP_{rct}^k \cdot EF_{rscgt}^k
\]

と同値である。

つまり、排出量を

- 需要量 \(Q\)
- 需給比率 \(M = QSUP/QDEM\)
- 排出係数 \(EF\)

の 3 つに分けて表現している。

これが今回の「普通の」Kaya 型の書き方であり、ここに LMDI を適用する。

---

## 6. 排出係数の作り方

排出係数には `baseyear.gdx` の `EFbase` を使う。

### 6.1 BaU の排出係数

\[
EF_i^{BaU} = EF_{rscg}^{base}
\]

### 6.2 MITI の排出係数

\[
EF_i^{MITI} = EF_{rscg}^{base} \cdot (1 - RR_{rscgt})
\]

コードでは、`world_decomp_base` の生成時に

```r
rr = replace_na(rr, 0)
ef = ef_base * (1 - rr)
```

として求めている。

すなわち、

- `RR` が存在しない場合は 0 とみなす
- MITI 側の排出係数は `EFbase × (1 - RR)` で計算する

という仕様である。

---

## 7. 実装上の中間テーブル

`paperV3.R` では、LMDI 版のために以下の中間テーブルを作っている。

- `world_qdem`
- `world_qsup`
- `world_rr`
- `world_ghg`
- `world_bau_ef`
- `world_qdem_levels`
- `world_qsup_levels`
- `world_ef_levels`
- `world_lmdi_cell_co2eq`
- `world_lmdi_country_co2eq`
- `world_lmdi_r5_world_co2eq`

### 7.1 `world_qdem`

`QDEM_load` から読み込んだ国内需要量。

列は

- `scenario`
- `commodity`
- `region`
- `year`
- `qdem`

である。

### 7.2 `world_qsup`

`QSUP_load` から読み込んだ生産量。

列は

- `scenario`
- `commodity`
- `region`
- `year`
- `qsup`

である。

### 7.3 `world_rr`

`RR_load` から読み込んだ排出係数削減率。

列は

- `scenario`
- `commodity`
- `source`
- `gas`
- `region`
- `year`
- `rr`

である。

### 7.4 `world_bau_ef`

`EFbase` から読み込んだ基準年排出係数。

列は

- `commodity`
- `source`
- `gas`
- `region`
- `ef_base`

である。

### 7.5 `world_ef_levels`

`world_decomp_base` を BaU/MITI 横持ちにしたものであり、

- `ef_bau`
- `ef_miti`

を持つ。

### 7.6 `world_qdem_levels`, `world_qsup_levels`

それぞれ `QDEM`, `QSUP` をシナリオ横持ちにしたものであり、

- `qdem_bau`, `qdem_miti`
- `qsup_bau`, `qsup_miti`

を持つ。

---

## 8. LMDI に使う排出量

今回の LMDI 版では、排出量は `GHG_load` をそのまま左右に置くのではなく、上の Kaya 型恒等式に沿って**再構成排出量**を使っている。

すなわち、各セル \(i\) について

\[
E_{0,i} = QSUP_i^{BaU} \cdot EF_i^{BaU}
\]

\[
E_{1,i} = QSUP_i^{MITI} \cdot EF_i^{MITI}
\]

と定義する。

コードでは

```r
e0_kt = qsup_bau * ef_bau
e1_kt = qsup_miti * ef_miti
```

として計算している。

ここで単位は `ktCO2eq/yr` であり、最終的な寄与は `/1000` して `MtCO2eq/yr` に直している。

このようにすることで、

\[
E_i = Q_i \cdot M_i \cdot EF_i
\]

という恒等式の上で、Residual を置かずに 3 要因分解を行える。

---

## 9. ゼロ値処理

LMDI では log を取るため、0 や負値をそのまま使えない。

そのため、コードでは

```r
safe_positive <- function(x, eps = 1e-12) {
  ifelse(is.na(x) | x <= 0, eps, x)
}
```

を定義し、以下に適用している。

- `qdem_bau`, `qdem_miti`
- `qsup_bau`, `qsup_miti`
- `ef_bau`, `ef_miti`
- `m0 = qsup_bau / qdem_bau`
- `m1 = qsup_miti / qdem_miti`

したがって、LMDI の計算自体はすべて正の値に対して行われる。

これはゼロ値を完全に除外する代わりに、ごく小さい \(\varepsilon\) を入れて計算を安定化させる実装である。

---

## 10. 対数平均

LMDI の重みには対数平均を使う。

\[
L(E_1, E_0)
=
\begin{cases}
\dfrac{E_1 - E_0}{\ln E_1 - \ln E_0} & (E_1 \neq E_0) \\
E_0 & (E_1 = E_0)
\end{cases}
\]

コードでは

```r
logmean <- function(x, y) {
  ifelse(abs(x - y) < 1e-12, x, (x - y) / (log(x) - log(y)))
}
```

としている。

---

## 11. LMDI の分解式

各セル \(i\) に対して、BaU と MITI の排出変化を

- 需要要因
- 構造要因
- 技術要因

に分解する。

### 11.1 Demand side

\[
\Delta E_{Q,i}
=
L(E_{1,i}, E_{0,i})
\cdot
\ln \left( \frac{Q_i^1}{Q_i^0} \right)
\]

ここで

\[
Q_i^k = QDEM_{rct}^k
\]

である。

### 11.2 Structural

\[
\Delta E_{M,i}
=
L(E_{1,i}, E_{0,i})
\cdot
\ln \left( \frac{M_i^1}{M_i^0} \right)
\]

ここで

\[
M_i^k = \frac{QSUP_{rct}^k}{QDEM_{rct}^k}
\]

である。

この Structural 要因には、

- 貿易変化
- 国内供給比率の変化
- 生産アロケーションの変化

がまとめて入る。

### 11.3 Technical

\[
\Delta E_{EF,i}
=
L(E_{1,i}, E_{0,i})
\cdot
\ln \left( \frac{EF_i^1}{EF_i^0} \right)
\]

ここで

\[
EF_i^0 = EF_i^{BaU}, \qquad EF_i^1 = EF_i^{MITI}
\]

である。

### 11.4 Total

各セルの total 変化は

\[
\Delta E_i = E_{1,i} - E_{0,i}
\]

とする。

コードではこれらを `world_lmdi_cell_co2eq` で次のように計算している。

```r
weight = logmean(e1_kt, e0_kt)

demand_mt = weight * log(qdem_miti / qdem_bau) / 1000
structural_mt = weight * log(m1 / m0) / 1000
technical_mt = weight * log(ef_miti / ef_bau) / 1000
total_change_mt = (e1_kt - e0_kt) / 1000
```

したがって、この LMDI 版では

\[
\Delta E = \Delta E_Q + \Delta E_M + \Delta E_{EF}
\]

の 3 要因だけで分解しており、`Other` は持たない。

---

## 12. 国別・地域別集計

### 12.1 国別集計

セルごとの寄与を国別に足し上げたものが

- `world_lmdi_country_co2eq`

である。

出力列は

- `demand_mt`
- `structural_mt`
- `technical_mt`
- `total_change_mt`
- `check_mt`

であり、`check_mt` は

\[
check = Total - (Demand + Structural + Technical)
\]

として計算している。

LMDI が数値的に安定していれば、`check_mt` は数値誤差レベルで 0 に近い。

### 12.2 R5 と World 集計

国別集計を `region.map` で R5 にまとめ、さらに World を加えたものが

- `world_lmdi_r5_world_co2eq`

である。

---

## 13. 図の作り方

追加した図は

- `fig6a_agricultural_decomposition_lmdi_2100.png`

である。

### 13.1 左パネル

- `World`
- `Former Soviet Union`
- `Latin America`
- `Africa/Middle East`
- `Asia`
- `OECD+EU`

の 2100 年値を、横棒の積み上げで示す。

### 13.2 右パネル

`agmip_ag_reduction_rank` を使って、

- 農業部門削減量上位 10 か国
- 農業部門削減量下位 10 か国

を選び、同じく横棒の積み上げで示す。

### 13.3 棒と黒点

棒は

- `Demand side`
- `Structural`
- `Technical`

の 3 本である。

黒点は

- `Total`

を示す。

単位はすべて `MtCO2eq/yr` である。

---

## 14. CSV 出力

### 14.1 `fig6a_world_r5_decomposition_lmdi_summary_2100.csv`

列は次のとおり。

- `region`
- `year`
- `demand_mt`
- `structural_mt`
- `technical_mt`
- `total_change_mt`
- `check_mt`
- `demand_share_pct`
- `structural_share_pct`
- `technical_share_pct`

ここで share は

\[
100 \times \frac{component}{total\_change}
\]

である。

### 14.2 `fig6a_top_bottom_country_decomposition_lmdi_summary_2100.csv`

列は次のとおり。

- `region`
- `country`
- `year`
- `ag_reduction`
- `demand_mt`
- `structural_mt`
- `technical_mt`
- `total_change_mt`
- `check_mt`
- `demand_share_pct`
- `structural_share_pct`
- `technical_share_pct`

こちらは右パネルの対象国だけを抜き出した csv である。

---

## 15. 各要因の意味

### 15.1 Demand side

\[
Q_i = QDEM_{rct}
\]

なので、需要総量の変化を表す。

これは、食料需要の増減や、価格上昇に伴う需要反応を反映する。

### 15.2 Structural

\[
M_i = \frac{QSUP_{rct}}{QDEM_{rct}}
\]

なので、国内需要に対してどの程度国内供給で賄うかの変化を表す。

このため、Structural には

- 貿易再編
- 国内供給比率変化
- 生産配分変化

が含まれる。

### 15.3 Technical

\[
EF_i^{MITI} = EF_i^{base} \cdot (1 - RR_i)
\]

で定義しているため、排出係数改善に対応する要因である。

これはモデル上の `RR` によって表現される技術的削減を意味する。

---

## 16. 論文での説明文の例

本文用には、たとえば次のように書ける。

> 本研究では，農業部門の非 CO\(_2\) 排出変化を，需要側要因（Demand side），構造要因（Structural），技術要因（Technical）の 3 要因に分解した。各国・財・排出源・ガス種・年のセルにおける排出量は，国内需要，需給比率，排出係数の積として表し，これに対して加法型 LMDI を適用した。需要側要因は国内需要量の変化，構造要因は国内需要に対する国内供給比率の変化，技術要因は排出係数改善を表す。対策シナリオの排出係数は基準年排出係数と削減率から導出し，セル単位の寄与を国別・地域別に集計した。

---

## 17. `paperV3.R` 内での対応箇所

主な実装箇所は次のとおり。

- `logmean()`
- `safe_positive()`
- `world_qdem_levels`
- `world_qsup_levels`
- `world_ef_levels`
- `world_lmdi_cell_co2eq`
- `world_lmdi_country_co2eq`
- `world_lmdi_r5_world_co2eq`
- `make_world_lmdi_country_plot()`
- `make_world_lmdi_r5_plot()`

出力ブロックは `Fig 6a` のコメント以下にある。

---

## 18. 関連出力ファイル

- `Papers/GEER2026/output/paperV3/fig6a_agricultural_decomposition_lmdi_2100.png`
- `Papers/GEER2026/output/paperV3/fig6a_world_r5_decomposition_lmdi_summary_2100.csv`
- `Papers/GEER2026/output/paperV3/fig6a_top_bottom_country_decomposition_lmdi_summary_2100.csv`

