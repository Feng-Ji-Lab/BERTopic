# Manuscript snapshot for the Windows 0.1.2 rerun

Exact manuscript source, bibliography and 26-page PDF synchronized from benchmark/results and benchmark/example-results. Build logs document successful compilation; numerical tables were also visually checked.

Run synchronization from the package repository before copying a new snapshot:

~~~powershell
& $python benchmark/sync_manuscript.py --manuscript ../bert/master.tex
~~~

Compile from this directory with TeX Live / Biber:

~~~powershell
latexmk -pdf -interaction=nonstopmode -halt-on-error master.tex
~~~

This Windows installation required workspace-owned TEMP, TMP, TMPDIR, PAR_GLOBAL_TMPDIR and PAR_GLOBAL_TEMP directories for Biber. E and author/DOI placeholders remain pending.
