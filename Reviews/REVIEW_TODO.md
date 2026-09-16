# BERTopic 论文修改前置 TODO（依据 review）

## 当前执行状态（2026-09-15）

本节记录已完成的工作；下方 review 原文和原稿摘录保留作为历史依据。

- A1：API 对照审计已完成，主稿已同步实际支持范围和行为。证据见 BERTopic/Reviews/API_AUDIT.md。
- B1–B8：现有回归测试在 Windows 完整执行；另修复 lightweight save 的错误遮挡、pt 格式映射和覆盖保护。
- C1–C2：固定 BERTopic 0.16.0，共享安装依赖并记录精确版本；C3 按用户要求仅验证 Windows。
- D1：0.1.2 源码 tag 和确切归档已推送至 GitHub；远端 tag 对应 faee106，归档 SHA-256 不变。
- D2：确切安装的 0.1.2 已完成五组独立 R/Python benchmark 和完整 worked example；正文数字、表格和图已重新生成。
- D3：输入、完整环境、脚本、原始输出、恢复检查、图/HTML、论文源码与 PDF 已归档并推送。大体积 pickle 可由脚本重建，其哈希已记录；公开 DOI 属于 F1。
- 验证：159 PASS、0 FAIL、0 WARN、0 SKIP；R CMD check 为 0 ERROR、0 WARNING、1 条原有 UTF-8 数据 NOTE。五组正式对比与六项恢复检查全部通过；原始分析稿完整编译为 26 页；当前修订稿见 G1。
- E：按用户要求跳过，未改动 SMS 选择方式或伪造历史重建。
- F1：ZIP、离线 Git bundle、Windows 重建入口和 Zenodo metadata 草稿已完成。新工作副本的五组对比、六项恢复检查全部通过，11 份数值 CSV 与正式结果逐字节相同。现有 Windows Python/R 依赖被复用，未再次从零安装 Conda 环境。DOI、账号及 creators/license 确认仍待完成。
- F2–F3：作者信息 JSON 与 CRediT 草案已提供；正式单位、声明和贡献归属待作者确认。
- G1：主稿已补齐 API 表、状态同步、六项恢复检查、Windows 包检查和归档重建说明，加入实际示例图；第三轮修订把安装说明移到示例前，显式绑定 Python 环境，并分别给出轻量 metadata 恢复和完整 pickle 预测一致性检查；新增代码已用保留模型运行通过，PDF 为 28 页。正式 benchmark 数字与三张数值表格不变，同步脚本不会覆盖解释段落。R 依赖未完全锁定，E/F 的事实占位符继续保留，未声称达到投稿完成条件。

## 使用说明

- 本文件是开始修改 `master.tex` 前的工作清单。
- 每项均包含：**要做什么**、**为什么做**、**review 原文**、**master.tex 对应原文**。
- review 明确要求当前阶段优先处理软件修正、可复现性、重跑分析和事实信息，而不是继续做文字润色。因此，涉及数值、版本、功能范围和可复现性的正文，原则上应等新 release 与重跑结果稳定后再改。
- 引文中的反引号、函数名和数字保持原文件写法。个别 review 文本在本地文件中存在字符编码显示问题（例如弯引号和连接号），这里按语义恢复为正常英文标点。

## A. 先确认论文对软件/API 的描述准确

### [x] A1. 逐段核对主稿中的包结构、函数、参数、返回值和行为

**要做什么**

对 `master.tex` 中所有关于当前 R 包实现的事实描述做代码级核查，至少包括：

- `bertopic_fit()` 的输入验证、`...` 传递及 `fit_transform()` 调用；
- `bertopic_r` 对象的 `.py`、`topics`、`probs` 字段；
- 表 1 中全部导出函数与 Python 方法的映射及返回类型；
- S3 方法 `predict()`、`coef()`、`as.data.frame()`、`fortify()`；
- 安装、环境绑定、诊断、可视化与持久化行为；
- 文中所有“支持”“保留”“恢复”“检查”“返回”等功能性表述。

核查后建立“准确 / 需改正文 / 需先改软件 / 尚未验证”的对应表，再据此改稿。

**为什么做**

合作者明确把技术准确性检查列为下一轮的第一步。当前稿已经作了较大结构调整，但并未同步修改或重新测试软件；如果正文描述超过实际 API 行为，后续的验证、示例和结论都会失去依据。

**Review 原文**

> The next round will need to focus on the software itself and the provenance of the results. Could you first read through the revised manuscript and make sure that my descriptions of the package and API are technically correct?

> I have not modified the package code or rerun the analyses, so I would treat this as our new working draft rather than a submission-ready version.

**master.tex 对应原文**

> The main entry point, `bertopic_fit(text, embeddings = NULL, ...)`, calls BERTopic from an R session. The function validates that `text` is a character vector, checks that the Python library is available, imports the Python `bertopic` module, and forwards `...` to the Python `BERTopic()` constructor.

> The return value has S3 class `bertopic_r` and contains three main fields: `.py`, the fitted Python BERTopic object; `topics`, the document-level topic assignments returned during fitting; and `probs`, the membership-strength matrix when available.

> Table 1 summarizes the principal R functions, the Python operation they invoke, and the result returned to R. The functions cover model fitting, inspection, prediction, visualization, modification, and persistence.

> The package also supplies S3 methods. `print()` and `summary()` provide compact model information; `predict()` presents transformation as a familiar prediction task; `coef()` returns top terms across topics; and `as.data.frame()` and `fortify()` support common R data workflows.

---

## B. 软件审计问题（先修代码与测试，再改正文）

### [x] B1. 检查并修复 representative-documents wrapper

**要做什么**

定位代表性文档相关的 R wrapper，核对参数转换、Python 调用、topic ID、返回对象类型及边界情况；添加能真正失败的单元测试，并在支持的环境中验证。

**为什么做**

这是 review 点名的软件审计问题。主稿虽然建议研究者检查每个 topic 的多篇代表性文档，却没有在 API 表或验证范围中明确说明该 wrapper 是否可靠；修复和测试后才能决定是否补充 API 文档或示例。

**Review 原文**

> In particular, please check the representative-documents wrapper [...].

**master.tex 对应原文**

主稿没有直接描述 representative-documents wrapper；相关方法论要求为：

> Researchers should examine multiple documents per topic and document any coding or adjudication procedure used to assign substantive labels.

以及：

> The evaluation remains narrow. It [...] did not test every function that modifies or extracts information from a fitted model.

### [x] B2. 修复 topic reduction 中的 `nr_topics` 处理

**要做什么**

核查 R 参数到 Python `nr_topics` 的类型和值映射，覆盖整数、`"auto"`（若 API 支持）、缺省值、非法值以及 reduction 后的输出；新增回归测试。

**为什么做**

错误的 `nr_topics` 处理会直接改变模型状态和 topic 数量，也会影响后续 topic 元数据、概率矩阵和正文中对修改功能的描述。

**Review 原文**

> In particular, please check [...] `nr_topics` handling in topic reduction [...].

**master.tex 对应原文**

主稿没有直接说明 `nr_topics` 或 topic-reduction wrapper，但把 modification 列为包的功能范围：

> The functions cover model fitting, inspection, prediction, visualization, modification, and persistence.

以及 Discussion 中建议测试：

> The strongest next validation step is a clean-environment test matrix across macOS, Linux, and Windows that covers installation, fitting, transformation, representative documents, topic reduction, state synchronization, visualization, and save--load reuse.

### [x] B3. Python 模型改变后同步 R 侧 `topics` 和 `probs`

**要做什么**

检查所有会改变 Python 模型状态的操作（至少 update/reduce 等），确保 R S3 对象中的 `topics`、`probs` 与 `.py` 的当前状态一致；明确 mutation 是原地发生还是返回新对象；为操作前后的一致性添加测试。

**为什么做**

正文声称对象保留 assignments 和 membership strengths，也声称更新后仍可检查和保存。如果 R 字段陈旧，导出矩阵、下游分析和 save/load 都可能静默使用错误状态。

**Review 原文**

> In particular, please check [...] synchronization of the R-side `topics` and `probs` fields after changes to the Python model [...].

**master.tex 对应原文**

> The return value has S3 class `bertopic_r` and contains three main fields: `.py`, the fitted Python BERTopic object; `topics`, the document-level topic assignments returned during fitting; and `probs`, the membership-strength matrix when available.

> `bertopic_update_topics()` & `update_topics()` & Updated topic representations

> Topic representations can be recomputed after inspection. `bertopic_update_topics(model, docs)`

### [x] B4. 修复 custom topic labels 的 Python key 类型

**要做什么**

核查自定义 topic label 从 R named vector/list 或其他输入转换成 Python mapping 时，key 是否为 Python `int` 而非字符串/浮点数；覆盖 topic `-1`、普通 topic ID、非法或缺失 ID，并增加测试。

**为什么做**

跨语言 key 类型错误可能导致标签不生效、映射到错误 topic 或只在部分环境中报错，也会影响 topic 表、图和持久化后的标签一致性。

**Review 原文**

> In particular, please check [...] Python key types for custom topic labels [...].

**master.tex 对应原文**

主稿没有直接描述自定义标签 wrapper；与其输出相关的现有表述为：

> The resulting object combines document assignments, topic-level term representations, fitted component objects, and optional membership-strength information. The outlier topic is conventionally labeled `-1`.

> `bertopic_topics()` & `get_topic_info()` & Topic metadata as a tibble

### [x] B5. 修复 document-topic matrix 列与真实 topic ID 的映射

**要做什么**

确认 `bertopic_as_document_topic_matrix()` 的每列准确对应 Python 概率矩阵的真实 topic ID，而不是假设 topic ID 连续或按位置等于编号；处理 `-1`、缺号、reduce/update 后重编号等情况；验证列名/前缀及 dense/sparse 两条路径。

**为什么做**

这是可能造成静默科学错误的高优先级问题：矩阵数值本身可能正确，但列标签错误会使下游回归或解释对应到错误 topic。

**Review 原文**

> In particular, please check [...] the mapping between document–topic matrix columns and actual topic IDs.

**master.tex 对应原文**

> `bertopic_as_document_topic_matrix()` & Stored fitting output & Dense or sparse R matrix for downstream analysis

> `document_topic <- bertopic_as_document_topic_matrix(model, sparse = FALSE, prefix = TRUE)`

> When `calculate_probabilities = TRUE`, the matrix contains HDBSCAN-derived membership strengths for the non-outlier topics supported by the fitted model.

### [x] B6. 让 documented self-check 真正完成其声称的 round trip

**要做什么**

找到文档化的 self-check，明确其声称的 round trip 是什么（安装/绑定、R→Python→R、save→load 或其他），修改实现或文档使二者一致；测试必须比较 round trip 前后的关键对象/结果，而不只是检查函数未报错。

**为什么做**

“检查成功”如果没有实际执行所宣称的往返过程，会给用户错误的环境或持久化保证，并削弱可复现性论证。

**Review 原文**

> The documented self-check should also perform the round trip it says it does [...].

**master.tex 对应原文**

主稿未出现 `self-check` 这个函数名；最接近的诊断论断是：

> Two functions support routine diagnosis. `bertopic_available()` checks whether the active interpreter can import BERTopic. `bertopic_session_info()` reports the Python path and version together with the availability of BERTopic, sentence-transformers, PyTorch, UMAP, and HDBSCAN.

### [x] B7. 使 save/load 恢复足够的 R 侧状态

**要做什么**

定义 loaded model 应恢复的 R 侧状态（至少 `.py`、`topics`、`probs` 及下游函数依赖的 metadata），修改序列化/重建逻辑；将示例中的弱检查升级为对 assignments、probability shape/column mapping、topic info，以及适当情况下 prediction 的一致性检查。

**为什么做**

当前示例只比较 load 前后 topic-info 的行数，不能证明加载后的对象行为一致。Review 明确要求恢复足够状态，使 loaded model 能一致工作。

**Review 原文**

> [...] save/load should restore enough R-side state for a loaded model to behave consistently.

**master.tex 对应原文**

> `bertopic_save()` / `bertopic_load()` & `save()`, `load()` & Persistent fitted Python model wrapped for R reuse

> `restored_model <- bertopic_load("sms_bertopic_model")`
>
> `restored_topic_info <- bertopic_topics(restored_model)`
>
> `stopifnot(nrow(restored_topic_info) == nrow(updated_topic_info))`

> Saved model cannot be restored elsewhere & Backend or serialization mismatch & Record versions, prefer a supported portable serialization, and test restoration before archiving

### [x] B8. 修复把失败变成 skip、或遮蔽导出函数的测试

**要做什么**

审查 test suite：

- 删除或收紧会把真实失败转换为 skip 的条件；
- 检查 test helper、局部变量或 mock 是否与导出函数同名并造成 masking；
- 区分“依赖确实不可用”的合理 skip 与“功能失败”的 test failure；
- 确认 CI 中关键测试确实运行，并记录各平台测试数/skip 数。

**为什么做**

如果失败被跳过或被同名对象遮蔽，所谓通过的测试不能支持正文中的功能和跨平台结论。

**Review 原文**

> Please also fix any tests that currently turn failures into skips or inadvertently mask an exported function.

**master.tex 对应原文**

主稿没有直接说明这些测试缺陷；它们会影响以下计划性论断：

> The strongest next validation step is a clean-environment test matrix across macOS, Linux, and Windows [...].

---

## C. 后端版本、安装和跨平台验证

### [x] C1. 决定并文档化实际支持的 Python BERTopic 版本范围

**要做什么**

基于干净环境测试决定支持策略（固定单一版本或明确区间），在包 metadata、安装函数、文档、错误信息和论文中保持一致；不得在未测试的情况下声称兼容 0.17.4 或其他新版本。

**为什么做**

当前 package/benchmark 使用 0.16.0，而主稿称当前 upstream 为 0.17.4。接口高度依赖 Python signatures，版本范围必须由实际测试支撑。

**Review 原文**

> I also made the backend-version issue explicit. The current package and benchmark use Python BERTopic 0.16.0, while the current upstream release is 0.17.4, so we should not claim forward compatibility until we have actually tested it.

> Once those issues are resolved, please decide and document the Python BERTopic version range we actually support [...].

**master.tex 对应原文**

> The released R package and the benchmark in this paper target Python BERTopic 0.16.0, whereas the current upstream release is 0.17.4. This version gap is material because upstream method signatures and dependency constraints can change. Users should not assume compatibility beyond the documented backend, and a revised package release should state and test its supported version range.

> This exposes BERTopic's configurable components within the backend versions explicitly supported by the R package. It does not guarantee forward compatibility with changed Python signatures.

### [x] C2. 统一 Conda 与 virtualenv 安装路线并报告精确依赖版本

**要做什么**

让 Conda、virtualenv 和 dispatcher 的依赖集合、版本约束、默认环境名、验证逻辑及错误信息一致；导出可机器读取的完整环境文件/lock file；让 session info 或配套记录报告关键包的准确版本。

**为什么做**

不同安装路线当前可能解析到不同 transitive dependencies，导致同一 R package 得到不同后端行为；当前 `bertopic_session_info()` 也不足以形成完整可复现记录。

**Review 原文**

> [...] make the Conda and virtual-environment installation routes consistent, report the exact dependency versions [...].

**master.tex 对应原文**

> Version 0.1.0 provides separate helpers for Conda and virtual environments, together with `install_py_deps()` as a dispatcher.

> Transitive dependencies can also resolve differently across installation routes and dates, so researchers should retain the complete environment used for an analysis.

> In version 0.1.0 it does not report the versions of all of those Python packages, so its output is a diagnostic rather than a complete reproducibility record.

### [x] C3. 干净环境完整测试矩阵（本次按用户要求仅 Windows）

**要做什么**

为每个平台从零安装，验证 Conda/virtualenv（按支持范围）、fit、transform、representative documents、topic reduction、状态同步、custom labels、document-topic matrix、visualization、self-check、save/load；保存 OS、R、Python 和全部依赖版本，以及测试日志。

**为什么做**

现有 benchmark 只来自一台 macOS 机器，不能支持跨平台可用性。跨平台干净测试也是发布更正版本和重跑分析的门槛。

**Review 原文**

> [...] run clean tests on macOS, Linux, and Windows.

**master.tex 对应原文**

> Five fresh Python processes and five fresh R processes were run on a 12-core Apple M2 Max computer with 32 GB of memory and macOS 14.4.1.

> The evaluation remains narrow. It used one operating system [...].

> The strongest next validation step is a clean-environment test matrix across macOS, Linux, and Windows [...].

---

## D. 发布、重跑与结果来源

### [x] D1. 发布新的、带版本号的修正版 R package

**要做什么**

完成 B、C 类问题后，更新版本号和 changelog，形成不可变 commit/tag，运行 package checks，并发布/归档该确切源码。记录 release 版本、commit 和 source hash。

**为什么做**

论文当前数值来自 0.1.0/旧 commit；软件改动后必须有一个明确、可引用的分析基线。

**Review 原文**

> We should then make a new versioned package release [...].

**master.tex 对应原文**

> The benchmark used the 2,247 SMS messages and `BERTopic` 0.1.0 (source commit `6e7a8d5`).

> Before submission, correct the audited wrapper and dependency issues, publish a tested versioned release [...].

### [x] D2. 用确切的新 release 重跑 worked example 和 R-vs-Python benchmark

**要做什么**

用 D1 的确切 release 重跑完整 worked example 和 direct R-versus-Python benchmark；重新生成正文所有数字、表格、图、HTML 和恢复检查。不要默认沿用旧结果；逐项比较新旧结果，并据新结果改 abstract、evaluation、discussion 和 declarations。

**为什么做**

软件修复、依赖和版本变化都可能改变 topic assignment、矩阵映射、运行时间和内存，因此旧数值不能自动保留。

**Review 原文**

> [...] rerun both the worked example and the direct R-versus-Python benchmark using that exact release. Since the current numerical results come from the original benchmark, I don't think we should carry them over automatically after making changes to the software.

**master.tex 对应原文**

> The two paths returned identical topic assignments, metadata, ordered top terms, c-TF--IDF weights at the recorded precision, and membership-strength matrices. In this short controlled task, the R path added a median of 0.762 seconds to fitting, 2.13 seconds to cold-process time, and approximately 197 MiB of peak memory.

> The controlled model produced 48 non-outlier topics and assigned 846 messages (37.7%) to the outlier class.

> All five pairs produced the same 2,247 document assignments [...].

> Before submission, the complete example should be rerun in a clean supported environment and its printed results, HTML files, and restoration check should be captured in the archive.

### [x] D3. 保存重跑的完整 provenance 与可复现材料

**要做什么**

保存并核对：运行脚本、原始输出、环境文件、随机种子、输入/数组/package hashes、线程和进程设置、硬件/OS 信息、计时方式、生成的表格/图片/HTML，以及从原始结果生成论文对象的脚本。

**为什么做**

论文对 frozen embeddings、process settings、hashes 和计时范围作了具体断言；没有原始材料就无法核验或复现。

**Review 原文**

> Please keep the scripts, raw outputs, environment files, seeds, hashes, process settings, and regenerated figures from the rerun.

**master.tex 对应原文**

> Holding these stochastic steps fixed allowed a deterministic comparison of the R and Python calls. SHA-256 hashes of the document file and reduced array were [...] complete hashes are included with the archived benchmark materials.

> Both paths read the same UTF-8 documents, used `PYTHONHASHSEED = 42`, and used identical single-thread settings for OpenMP and common numerical libraries.

---

## E. SMS 数据来源与重建

### [ ] E1. 查明 1,500 条 ham 的选择规则和 seed

**要做什么**

追查 package data、git history、旧脚本和原作者记录，确定 4,827 条 ham 中 1,500 条的选择方法、顺序处理、去重/清洗过程和随机 seed（如有）；同时确认 747 条 spam 是否确为全量且未经额外处理。

**为什么做**

当前数据不是 UCI 原集合的自然比例子集，但论文尚不能说明如何产生。选择机制会影响 topic composition 和 benchmark 结果。

**Review 原文**

> The packaged SMS data contain all 747 spam messages but only 1,500 of the 4,827 ham messages in the original collection. Please see if you can determine how those 1,500 messages were selected and document the selection rule and random seed, if there was one.

**master.tex 对应原文**

> The packaged subset contains all 747 spam messages and 1,500 of the 4,827 ham messages in the source collection. [TO DO: Document the rule and random seed, if any, used to select the 1,500 ham messages, and archive a script that reconstructs the packaged data from the UCI source.]

### [ ] E2. 编写从 UCI source 重建 packaged dataset 的脚本；无法还原时改用透明确定性子集

**要做什么**

编写从原始 UCI 数据下载/读取、校验、选择、清洗并生成 package data 的一键脚本，记录 source URL/版本/hash。若旧的 1,500 条无法重建，则制定透明的 deterministic rule，更新 packaged data，并用新数据重跑全部分析。

**为什么做**

仅在正文说明数量不足以复现研究输入；无法恢复历史抽样时，透明替换比猜测原 seed 更可靠，但替换意味着所有结果必须重跑。

**Review 原文**

> We should also have a script that reconstructs the packaged dataset from the UCI source. If the original selection cannot be reconstructed, that is fine—we can replace it with a transparent deterministic subset and rerun the analyses.

**master.tex 对应原文**

> [TO DO: Document the rule and random seed, if any, used to select the 1,500 ham messages, and archive a script that reconstructs the packaged data from the UCI source.]

> Availability of data and materials. The demonstration data are distributed in the `BERTopic` R package. [TO DO: Insert the archival OSF/Zenodo URL containing the exact benchmark inputs or a deterministic generation script, outputs, figures, and manuscript materials.]

---

## F. 公共归档、DOI 与作者信息

### [ ] F1. 建立 versioned OSF 或 Zenodo archive 并取得 DOI

**要做什么**

在软件和结果稳定后创建带版本的公开归档，至少包含：release source、SMS reconstruction script、worked-example/benchmark scripts、原始结果、表格、图片/HTML、environment files、seeds/hashes/settings 和 manuscript source；检查从空环境能否按说明重建关键输出；取得 DOI/URL。

**为什么做**

主稿多处承诺归档，但当前仍是占位符。归档必须与最终论文引用的 release 和结果一一对应。

**Review 原文**

> Once the software and results are stable, please put together a versioned OSF or Zenodo archive with the release source, reconstruction and analysis scripts, raw benchmark results, tables and figures, environment files, and manuscript source. I'll also need the archive DOI [...].

**master.tex 对应原文**

> Availability of data and materials. [...] [TO DO: Insert the archival OSF/Zenodo URL containing the exact benchmark inputs or a deterministic generation script, outputs, figures, and manuscript materials.]

> Code availability. [...] [TO DO: Insert a versioned DOI for the exact release and benchmark scripts used in this manuscript.]

> The R package, source code, documentation, packaged demonstration data, benchmark scripts, raw benchmark outputs, and manuscript source will be available in a versioned public archive at [TO DO: OSF or Zenodo URL/DOI].

### [ ] F2. 补全 affiliations、corresponding author、funding、competing interests

**要做什么**

向作者团队确认并填写：两条完整 affiliation、corresponding author 姓名/邮寄地址/电话/email、funding statement、competing-interests statement。不得自行推断。

**为什么做**

这些是投稿必需的事实信息，当前全部是显式占位符，需要作者批准。

**Review 原文**

> I'll also need [...] both affiliations, corresponding-author information, funding and competing-interests statements [...].

**master.tex 对应原文**

> [TO DO: Affiliation]

> [TO DO: Corresponding author name, postal address, telephone number, and email]

> Funding. [TO DO: Insert complete funding statement or “No funding was received for conducting this study.”]

> Competing interests. [TO DO: Insert author-approved competing-interests statement.]

### [ ] F3. 起草并由作者确认 CRediT statement，区分论文作者与包开发贡献

**要做什么**

收集每位作者在 conceptualization、software、methodology、validation、investigation、data curation、writing、visualization、supervision 等角色中的实际贡献，形成 CRediT 草案并让全体作者确认；明确 package contributors 与 manuscript authorship 不必机械相同，但署名与致谢需一致且有意为之。

**为什么做**

当前稿缺 author-contribution statement；本项目同时包含论文写作和软件开发，两类贡献若不区分，容易造成信用归属不清。

**Review 原文**

> [...] and a proposed CRediT contribution statement. The manuscript authorship and package-development credits do not necessarily need to be identical, but we should make sure everyone's role is clear and intentional.

**master.tex 对应原文**

> Author contributions. [TO DO: Complete an author-approved CRediT contribution statement. Ensure that manuscript authorship and package-development contributions are documented consistently.]

---

## G. 最后才进行的论文更新

### [ ] G1. 用最终证据统一更新全文，不做无依据的前瞻性声称

**要做什么**

完成 A–F 后统一更新 title/abstract（若需要）、software design、worked example、benchmark、tables/figures、Discussion、Availability、Declarations 和 Open Practices。逐项检查：

- 包始终被称为 Python BERTopic 的 R interface，而非官方实现、独立统计方法或普遍更优的接口；
- 对 `bertopicr` 的 novelty 定位保持狭义、可证；
- 结论只覆盖实际测试过的 API、版本、平台和 benchmark 路径；
- 不把 SMS 示例说成 behavioral-construct validation；
- 新数值全部可追溯到最终 release 与 archive；
- 删除所有 `\todo{}` 占位符。

**为什么做**

当前稿的 framing 已被合作者有意收窄。后续修软件和重跑可能改变事实与数字，但不应重新扩大贡献主张。Review 还明确建议眼下不要投入进一步 stylistic editing。

**Review 原文**

> The paper now makes a narrower case for the contribution: the R-facing API, S3 object model, workflow integration, and validation of selected R–Python input/output mappings. I also removed language that could be read as suggesting that the package is an official implementation, a separate statistical method, or generally superior to other R interfaces.

> For now, I would focus your time on the software corrections, reproducibility work, rerunning the analyses, and filling in the missing factual information rather than further stylistic edits to the manuscript.

**master.tex 对应原文**

> The contribution is an R-oriented access layer and a validation of its tested fitting and extraction path, not a new topic model or an independent implementation of BERTopic.

> This comparison examines the wrapper's handling of selected inputs and outputs; it is not a comparison of two topic-modeling algorithms, a feature-completeness study of R packages, or a validation of the substantive topics recovered from the SMS corpus.

> The evidence therefore supports an input--output mapping claim, not a general claim about computational efficiency, topic quality, or the reliability of every exported helper.

## 建议执行顺序 / 门槛

1. **A1 技术核查**：先把论文论断映射到真实代码。
2. **B1–B8 软件修复与可靠测试**：先消除会污染结果或掩盖失败的问题。
3. **C1–C3 版本/安装/跨平台**：确定真正支持范围。
4. **E1–E2 数据来源**：在重跑前锁定最终输入数据。
5. **D1 新 release**：冻结用于论文的代码版本。
6. **D2–D3 重跑并保存 provenance**：生成最终数字、表和图。
7. **F1 归档 DOI**：把 release、数据重建和结果整体固化。
8. **F2–F3 作者事实信息**：取得全体作者确认。
9. **G1 改 master.tex**：用最终证据一次性统一全文。

## 完成判据（提交给合作者前）

- [x] 所有被点名的软件审计问题有修复 commit 和非 skip 的回归测试。
- [x] 支持范围限定为 Windows / BERTopic 0.16.0，并有该范围的干净测试证据；其他平台按用户要求不处理。
- [ ] Conda/virtualenv 安装结果与精确依赖版本可复现。
- [ ] SMS packaged data 可由公开源确定性重建。
- [x] 论文所有结果来自同一个明确的新 package release。
- [x] 原始输出、脚本、环境、seed、hash、process settings、图表均已归档并推送至 GitHub。
- [ ] OSF/Zenodo DOI 与正文链接有效，归档版本不可变且内容齐全。
- [ ] affiliations、corresponding author、funding、competing interests、CRediT 均获作者确认。
- [ ] `master.tex` 无残留 `\todo{}`，且所有数值、版本与功能性声称均能指向证据。
