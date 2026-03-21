# asciiwright

[![R-CMD-check](https://github.com/haozhou1988/asciiwright/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/haozhou1988/asciiwright/actions/workflows/R-CMD-check.yaml)

`asciiwright` 是一个最小可用的 R 包原型，用来生成 Winsteps 风格的 ASCII Wright map。

它做的事情是：

- 把 person 与 item measure 放到同一条潜变量纵轴上
- 可选显示 `SCORE` 列，把原始分数一起对齐到纵轴
- 用固定宽度文本输出，适合终端、`capture.output()`、markdown 代码块和纯文本报告
- 在轴心位置标出 `M / S / T`，分别表示均值、1 个标准差、2 个标准差
- 支持左右两侧分别显示为标签列表或压缩分布列，可覆盖 Table 1.0、1.1、1.2、1.3 这几类核心布局
- 分布列支持更接近 Winsteps 的自动规则：放得下时显示 `X`，放不下时自动切换到 `#` / `.`
- 可选双轴样式 `++ / ||` 和右侧 `MEASURE` 列，更接近 Winsteps 的 mirrored tables
- 提供 `score_table_ascii()`，从 item difficulty 生成 Winsteps Table 20.1 风格的 raw score-to-measure 表
- 提供 `polytomous_threshold_map_ascii()`，生成 polytomous item 的 half-point / Thurstonian / Andrich / category-center threshold maps
- 提供 `polytomous_range_map_ascii()`，生成 Table 1.4 风格的 `BOTTOM / MEASURE / TOP` 三列 polytomous range map
- 提供 `wright_map_mirt()`，可以直接从 `mirt` 单维模型生成 item / threshold / range 版本的 ASCII map
- 标签支持 `label_abbrev = "smart"` 智能缩写，也支持 `label_overrides` 让使用者自己指定缩写
- 提供 `preview_label_abbrev()` 和 `make_label_overrides()`，可以先预览缩写结果，再把手工修改过的标签回灌到 map

## 安装

在当前目录中：

```r
devtools::load_all(".")
```

或：

```bash
R CMD INSTALL .
```

安装后也可以用：

```r
browseVignettes("asciiwright")
```

查看工作流文档，里面有完整的 `preview -> edit -> override -> render` 示例。

## 示例

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

标签缩写示例：

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

```r
custom_abbrev <- function(label, width, side) {
  if (side == "item_bottom") return("Bottom animals")
  if (side == "item_center") return("Center animals")
  if (side == "item_top") return("Top animals")
  label
}
```

如果你想先预览、再改名、最后出图，可以直接走这条流程：

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

分布视图示例：

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

双轴镜像示例：

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

## 设计说明

这个版本优先复刻 Winsteps 页面里最核心的文本形态：

- Table 1.0：左右两侧都显示标签
- Table 1.1：左右两侧都显示分布
- Table 1.2 / 1.3：一侧分布、一侧标签

当前 `SCORE` 列是根据传入的 `person_scores` 或 person 数据框里的 `score` 列，按代表性位置对齐显示的。相同原始分数会合并为一次显示，而不是在多行里重复出现。
默认的 `score_method = "rasch"` 会用 item difficulty 反推 raw score 在潜变量上的位置；如果你只想按观测样本位置摆放，也可以切回 `score_method = "observed"`。

当前双轴样式主要对齐 Winsteps 的文本观感与镜像信息布局，仍然不是其全部内部表格参数的逐项复刻。

导出示例：

```r
write_wright_map(map, "wright-map.txt")
write_wright_map(map, "wright-map.md")
```

分数换算表示例：

```r
tbl <- score_table_ascii(dat$items)
cat(as.character(tbl))
```

Polytomous threshold map 示例：

```r
pdat <- example_polytomous_data()

map_poly <- polytomous_threshold_map_ascii(
  persons = pdat$persons,
  items = pdat$items,
  steps = pdat$steps,
  mode = "halfpoint",
  person_display = "distribution",
  label_width = 18,
  label_abbrev = "smart"
)

cat(as.character(map_poly))
```

`polytomous_threshold_map_ascii()` 当前聚焦 Table 1.5 到 1.8 这几类 threshold maps；`polytomous_range_map_ascii()` 则单独处理 Table 1.4 风格的 bottom/top/range 三列结构。

Table 1.4 风格 range map 示例：

```r
map_range <- polytomous_range_map_ascii(
  persons = pdat$persons,
  items = pdat$items,
  steps = pdat$steps,
  person_display = "distribution",
  item_display = "labels",
  item_width = 18,
  label_abbrev = "smart",
  label_overrides = c(`Read books on animals` = "Read books animals")
)

cat(as.character(map_range))
```

`mirt` 接口示例：

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

参考页面：

- [Winsteps Table 1 Wright item-person maps](https://www.winsteps.com/winman/table1.htm)
