# Publication materials for BERTopic 0.1.2

The validated package source tag v0.1.2 and retained source archive are public on the authorized GitHub repository. The analysis ZIP is ready to build; DOI publication remains pending.

## Build and verify the upload

From a clean package checkout, using Python 3.10+:

~~~powershell
& $python benchmark/publication/build_archive.py --output ../bertopic-publication-0.1.2
~~~

The builder archives the committed checkout and a Git bundle containing its HEAD and exact package tag. It verifies every retained archive hash against ZIP contents and writes a SHA-256 plus an upload inventory. It rejects existing output files. No machine-learning dependencies are needed for packaging.

The ZIP contains exact package source/archives, packaged SMS input, frozen embeddings, full environments, all five-pair raw results, worked-example results, figures/HTML, manuscript source/PDF and author/metadata drafts. Earlier results are marked historical. Large model.pkl is intentionally omitted and can be regenerated with the recorded hash. E reconstruction remains deferred.

## Reproduce on Windows

Extract the ZIP. To restore the Git provenance required by the benchmark, clone the included SOURCE.bundle into a separate empty directory:

~~~powershell
git clone ./SOURCE.bundle ./reproduction
cd ./reproduction
~~~

Use Windows x64, R 4.4.1, Git and Conda. Create a new environment from the archived lock, rather than changing an existing environment:

~~~powershell
conda env create --name r-bertopic-reproduce-012 --file provenance/windows-bertopic-0.16.0.yml
$python = Join-Path (conda info --base) 'envs/r-bertopic-reproduce-012/python.exe'
Rscript -e 'install.packages(c("reticulate","rlang","tibble","testthat","withr","Matrix","jsonlite","htmltools","ggplot2"), repos="https://cloud.r-project.org")'
powershell.exe -NoProfile -ExecutionPolicy Bypass -File benchmark/publication/reproduce_windows.ps1 -PythonPath $python
~~~

The process-scoped execution policy runs this trusted unsigned archive script without changing system policy. The script first validates archived hashes and versions, installs the retained exact source into an isolated R library, then creates separate benchmark/worked-example outputs. It reuses the frozen input; full example reconstruction may download the immutable MiniLM model revision. Hardware-dependent times are expected to change. Fresh Conda reconstruction was not rerun for this publication package; the retained evidence is the previously validated clean Windows environment and exact-release checks.

## Complete F1

Log into Zenodo (or OSF) and upload the ZIP, SHA-256 and inventory. The Zenodo metadata JSON is a draft; add author-confirmed creators and a suitable license before submitting it. The code is MIT; SMS data, manuscript and embedded third-party Plotly assets keep their respective rights, so the whole mixed archive must not silently inherit a blanket MIT license.

Zenodo account access, creator confirmation and the archive license are still missing. A reserved DOI is provisional; update the manuscript with the published version-specific DOI after publication. API documentation: https://developers.zenodo.org/

## Complete F2/F3

Fill author_information.json and review CREDIT_DRAFT.md with all authors. Preserve the manuscript's author order unless the author team changes it. Affiliations, corresponding author, funding, interests and contributor roles remain unresolved; no package metadata is substituted for confirmed manuscript facts.
