# Development Plan

## Goal

把 `asciiwright` 打磨成一个既能复刻 Winsteps ASCII Wright map，又能方便接入现代 R IRT 工作流的轻量包。

## Current State

- 已支持 Winsteps 风格的 ASCII Wright map、distribution map、score table、polytomous threshold map、polytomous range map。
- 已支持 `mirt` 单维模型接口，可直接生成 item / threshold / range 版本的 ASCII map。
- 已支持智能缩写、自定义 `label_overrides`、缩写预览和从 preview 表生成 overrides。
- 已支持分页、`LINELENGTH`、Table 1 预设，以及一组覆盖核心 ASCII 布局的 snapshot 风格回归测试。

## Near-Term Work

- 完善 `mirt` 接口的边界处理：更清晰地区分 dichotomous、polytomous、mixed-format 模型。
- 增加 `TAM`、`eRm` 等常见 Rasch/IRT 包的适配层。
- 给 `preview_label_abbrev()` 增加更明显的“人工编辑后回灌”示例和 vignette 级文档。
- 把 snapshot 测试继续扩到更多边界情形，特别是长标签、极端分数、窄列宽和多语言标签。

## Mid-Term Work

- 继续向 Winsteps 原版文本布局靠拢，特别是 `SCORE`、extreme score、density column 的细节规则。
- 补更完整的 Table 1 系列表格变体和导出选项。
- 增加更细粒度的 label strategy 接口，例如可配置 stopwords、保留首词、保留末词等。

## Packaging And Delivery

- 建立 GitHub Actions：`R CMD check`、testthat、lintr。
- 增加 release checklist，保证版本号、README、Rd、测试、示例同步更新。
- 维护 `NEWS.md`，让版本演进和里程碑变化更容易追踪。
- 视需要补 `pkgdown` 站点，让控制台示例、文本截图和适配器说明更容易查找。

## Done Criteria For The Next Milestone

- `mirt`、`TAM` 至少两条模型接口稳定可用。
- README 中提供“原始数据 -> preview -> override -> 出图”的完整工作流。
- GitHub 上有自动检查，能在 PR 或 push 后自动验证包可安装、测试可通过。
