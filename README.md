# asciiwright

[![R-CMD-check](https://github.com/haozhou1988/asciiwright/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haozhou1988/asciiwright/actions/workflows/R-CMD-check.yaml)

`asciiwright` 是一个轻量 R 包，用来生成 Winsteps 风格的 ASCII Wright map。  
`asciiwright` is a lightweight R package for generating Winsteps-style ASCII Wright maps.

它把 person 和 item measure 放到同一条潜变量纵轴上，并以固定宽度文本输出，适合终端、纯文本报告、Markdown 代码块和可复现工作流。  
It places person and item measures on the same latent scale and renders them as fixed-width text for terminals, plain-text reports, Markdown code blocks, and reproducible workflows.

## 功能 | Features

- 把 person 与 item measure 对齐到同一条潜变量纵轴上。 / Aligns person and item measures on the same latent-variable axis.
- 可选显示 `SCORE` 列，并支持更接近 Winsteps `EXTRSCORE` 的极端分数 inward adjustment。 / Optionally shows a `SCORE` column with a more Winsteps-like extreme-score inward adjustment.
- 用 ASCII 固定宽度文本输出，便于在 console、`capture.output()`、Markdown 和 `.txt` 文件中保持对齐。 / Renders fixed-width ASCII output that stays aligned in the console, `capture.output()`, Markdown, and `.txt` files.
- 在轴心位置标出 `M / S / T`，分别表示均值、1 个标准差和 2 个标准差。 / Marks `M / S / T` on the axis for the mean, one SD, and two SDs.
- 支持 Table 1.0、1.1、1.2、1.3 这类基本 Wright map 布局。 / Supports the core Table 1.0, 1.1, 1.2, and 1.3 Wright map layouts.
- 支持 `table_style` 预设，可切到 mirrored easiness 版本，如 Table 1.10、1.11、1.12、1.13。 / Supports `table_style` presets for mirrored easiness variants such as Table 1.10, 1.11, 1.12, and 1.13.
- 分布列支持 Winsteps 风格的 `X`、`#`、`.` 规则。 / Supports Winsteps-like `X`, `#`, and `.` density-column behavior.
- 支持双轴样式 `++ / ||`、右侧 `MEASURE` 列、`line_length` 和 `max_page`。 / Supports `++ / ||` mirrored axes, an optional right-side `MEASURE` column, `line_length`, and `max_page`.
- 提供 `score_table_ascii()`，生成 Winsteps Table 20.1 风格的 raw score-to-measure 表。 / Includes `score_table_ascii()` for Winsteps Table 20.1-style raw score-to-measure tables.
- 提供 `polytomous_threshold_map_ascii()`，支持 half-point、Thurstonian、Andrich 和 category-center threshold maps。 / Includes `polytomous_threshold_map_ascii()` for half-point, Thurstonian, Andrich, and category-center threshold maps.
- 提供 `polytomous_range_map_ascii()`，生成 Table 1.4 风格的 `BOTTOM / MEASURE / TOP` 三列 range map。 / Includes `polytomous_range_map_ascii()` for Table 1.4-style `BOTTOM / MEASURE / TOP` range maps.
- 这两类 polytomous 图也支持 `table_style`、`line_length` 和 `max_page`。 / These polytomous maps also support `table_style`, `line_length`, and `max_page`.
- 提供 `wright_map_mirt()`，可直接从单维 `mirt` 模型生成 item、threshold 和 range 图。 / Includes `wright_map_mirt()` for item, threshold, and range maps from unidimensional `mirt` models.
- 支持智能缩写 `label_abbrev = "smart"`、自定义 `label_overrides`、`preview_label_abbrev()` 和 `make_label_overrides()`。 / Supports smart label abbreviation, custom overrides, preview helpers, and round-tripping edited label overrides.

## 安装 | Installation

在当前目录中加载开发版本：  
Load the development version in the current directory:

```r
devtools::load_all(".")
```

或直接安装：  
Or install it directly:

```bash
R CMD INSTALL .
```

安装后可以用下面的命令查看 vignette：  
After installation, you can browse the vignette with:

```r
browseVignettes("asciiwright")
```

版本摘要见 [NEWS.md](NEWS.md)。  
See [NEWS.md](NEWS.md) for release notes.

## 示例 | Examples

### 基础示例 | Basic Wright map

```r
library(asciiwright)

dat <- example_wright_data()
map <- wright_map_ascii(
  persons = dat$persons,
  items = dat$items,
  person_title = "KID",
  item_title = "TAP",
  lines_per_logit = 2,
  label_width = 34,
  name_trunc = 5
)

cat(as.character(map))
```

### 标签缩写 | Label abbreviation

```r
map_labels <- wright_map_ascii(
  persons = dat$persons,
  items = c(
    `Read books on animals` = -0.8,
    `Find where animal lives` = -1.2
  ),
  person_display = "distribution",
  label_width = 18,
  name_trunc = 18,
  label_abbrev = "smart",
  label_overrides = c(`Find where animal lives` = "Animal habitat")
)

cat(as.character(map_labels))
```

如果你想自己控制不同栏位的缩写，也可以传函数：  
If you want side-specific abbreviations, you can also pass a function:

```r
custom_abbrev <- function(label, width, side) {
  if (side == "item_bottom") return("Bottom animals")
  if (side == "item_center") return("Center animals")
  if (side == "item_top") return("Top animals")
  label
}
```

### 预览后修改标签 | Preview, edit, and reuse label overrides

```r
pdat <- example_polytomous_data()

preview <- preview_label_abbrev(
  pdat$items,
  width = 18,
  style = "smart",
  side = "item"
)

preview$final[preview$label == "Find where animal lives"] <- "Animal habitat"
overrides <- make_label_overrides(preview)

map_custom <- wright_map_ascii(
  persons = pdat$persons,
  items = pdat$items,
  person_display = "distribution",
  label_width = 18,
  name_trunc = 18,
  label_abbrev = "smart",
  label_overrides = overrides
)

cat(as.character(map_custom))
```

### 分布视图 | Distribution view

```r
dat <- example_wright_data()

map <- wright_map_ascii(
  persons = dat$persons,
  items = dat$items,
  person_display = "distribution",
  item_display = "distribution",
  person_scores = dat$person_scores,
  distribution_style = "winsteps",
  label_width = 10
)

cat(as.character(map))
```

### 双轴镜像 | Mirrored double-axis layout

```r
map <- wright_map_ascii(
  persons = dat$persons,
  items = dat$items,
  person_scores = dat$person_scores,
  person_display = "distribution",
  item_display = "distribution",
  distribution_style = "winsteps",
  axis_style = "double",
  right_measure = TRUE,
  label_width = 8
)

cat(as.character(map))
```

### Table 1 预设与分页 | Table 1 presets and paging

```r
map_ease <- wright_map_ascii(
  persons = dat$persons,
  items = dat$items,
  table_style = "table1.12",
  line_length = 80
)

map_pages <- wright_map_ascii(
  persons = dat$persons,
  items = dat$items,
  max_page = 12
)

cat(as.character(map_ease))
cat(format(map_pages, page = 1), sep = "\n")
```

### 导出文本 | Export text output

```r
write_wright_map(map, "wright-map.txt")
write_wright_map(map, "wright-map.md")
```

### 分数换算表 | Raw score-to-measure table

```r
tbl <- score_table_ascii(dat$items)
cat(as.character(tbl))
```

### Polytomous threshold map

```r
pdat <- example_polytomous_data()

map_poly <- polytomous_threshold_map_ascii(
  persons = pdat$persons,
  items = pdat$items,
  steps = pdat$steps,
  table_style = "table1.5",
  line_length = 70,
  max_page = 12,
  label_abbrev = "smart"
)

cat(as.character(map_poly))
```

`polytomous_threshold_map_ascii()` 目前聚焦 Table 1.5 到 1.8。  
`polytomous_threshold_map_ascii()` currently focuses on Table 1.5 through 1.8.

### Table 1.4 风格 range map | Table 1.4-style range map

```r
map_range <- polytomous_range_map_ascii(
  persons = pdat$persons,
  items = pdat$items,
  steps = pdat$steps,
  table_style = "table1.4",
  line_length = 80,
  max_page = 12,
  label_abbrev = "smart",
  label_overrides = c(`Read books on animals` = "Read books animals")
)

cat(as.character(map_range))
```

### `mirt` 接口 | `mirt` interface

```r
if (requireNamespace("mirt", quietly = TRUE)) {
  data("Science", package = "mirt")
  mod <- mirt::mirt(Science, 1, itemtype = "graded", verbose = FALSE)

  map_mirt <- wright_map_mirt(
    mod,
    map_type = "thresholds",
    person_display = "distribution",
    label_width = 18,
    name_trunc = 18
  )

  cat(as.character(map_mirt))
}
```

如果模型数据里带有 `freq` 列，`wright_map_mirt()` 会自动识别并按频数展开 person 分布；polytomous 模型的 `thresholds` 和 `range` 视图则直接使用 `mirt` 的 `b1`, `b2`, ... 边界位置。  
If the model data includes a `freq` column, `wright_map_mirt()` will expand person distributions by frequency; for polytomous models, the `thresholds` and `range` views use the model's `b1`, `b2`, ... boundaries directly.

## 设计说明 | Design Notes

这个包优先复刻 Winsteps 页面里最核心的文本形态。  
This package focuses first on reproducing the core text layouts shown in Winsteps.

- Table 1.0：左右两侧都显示标签。 / Labels on both sides.
- Table 1.1：左右两侧都显示分布。 / Distribution columns on both sides.
- Table 1.2 / 1.3：一侧分布，一侧标签。 / Distribution on one side and labels on the other.
- Table 1.10 / 1.11 / 1.12 / 1.13：mirrored easiness 版本。 / Mirrored easiness-oriented layouts.

当前 `SCORE` 列会根据传入的 `person_scores` 或 person 数据框里的 `score` 列，按代表性位置对齐显示；相同原始分数只显示一次。默认 `score_method = "rasch"` 会用 item difficulty 反推 raw score 在潜变量上的位置；也可以切回 `score_method = "observed"`。  
The current `SCORE` column is aligned from `person_scores` or a person data-frame `score` column, with each unique raw score shown once at a representative location. By default, `score_method = "rasch"` estimates the raw-score location from item difficulties; you can also switch back to `score_method = "observed"`.

极端分数的默认 inward adjustment 是 `0.3`，可以用 `score_extreme_adjust` 调整。  
The default inward adjustment for extreme scores is `0.3`, and it can be tuned with `score_extreme_adjust`.

当前双轴样式、`line_length`、`max_page` 和 mirrored easiness 布局主要对齐 Winsteps 的文本观感与主要控制参数，但还不是其内部表格逻辑的逐项逆向复刻。  
The current mirrored axes, `line_length`, `max_page`, and mirrored easiness layouts aim to match the overall Winsteps look and major controls, but they are not yet a line-by-line reverse engineering of every internal table rule.

## 参考页面 | References

- [Winsteps Table 1 Wright item-person maps](https://www.winsteps.com/winman/table1.htm)
