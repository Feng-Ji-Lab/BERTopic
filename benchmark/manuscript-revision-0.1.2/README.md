# Revised manuscript for the validated Windows 0.1.2 analysis

This snapshot contains the revised 29-page paper, bibliography, figure source/assets and successful TeX build logs. The earlier manuscript-0.1.2 snapshot remains the historical analysis draft.

Changes cover the missing API mappings, cached-state reassignment, representative-document and matrix semantics, six restoration checks, exact package checks, bounded archive reconstruction, environment setup and versioned public materials. Formal numerical table bodies match benchmark/results/artifacts exactly. The new paper figure is drawn from the full example's own archived topic_info.csv; it does not fit a new model.

Reproduce the figure from this snapshot directory:

~~~powershell
Rscript figures/render_worked_example.R --input ../example-results/topic_info.csv --output figures/worked_example_topic_counts
latexmk -pdf -interaction=nonstopmode -halt-on-error master.tex
~~~

The documented Windows Biber workspace cache workaround remains applicable. R dependencies are recorded but not completely locked; historical SMS reconstruction and author/DOI fields remain pending. The benchmark synchronizer now preserves the edited narrative and updates numerical benchmark facts/table bodies.
