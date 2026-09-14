# Reproducibility records

`windows-session.txt` records the source commit, R CMD check result, operating system, R session, selected Python interpreter, and installed backend versions.

Export the selected Python environment with:

```powershell
Rscript scripts/export_python_lock.R `
  --python "C:\Users\zby15\miniconda\envs\r-bertopic\python.exe" `
  --output provenance/python-environment.yml
```

For a Conda interpreter, the exporter writes `conda env export` YAML including its pip subsection and removes the machine-specific `prefix`. For a virtualenv, it writes sorted `pip freeze --all` requirements and rejects local file references. Both routes run `pip check` first.

The final manuscript environment must be generated from the frozen `bertopic==0.16.0` release environment. A file from another backend version is compatibility evidence only.
