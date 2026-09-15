# Exact local 0.1.1 release source

- Annotated local tag: v0.1.1
- Package source commit: 9b71d4ff082ad8b51aee196d34b2284806c02bd4
- Source archive: BERTopic_0.1.1.tar.gz
- SHA-256: fc6640a1d15931454cb1947a66d3f330ca0733282e3d09198788f3f4aab23b38
- Validated backend: Windows, Python 3.10.21, BERTopic 0.16.0.
- The benchmark harness is maintained separately from the immutable R package release; its execution commit and actual script hashes are recorded in benchmark/results/provenance.json.
- Remote release/tag publication and a public DOI remain pending.

Install the retained exact archive from the repository root:

~~~powershell
R.exe CMD INSTALL provenance/releases/BERTopic_0.1.1.tar.gz
~~~

Pass this archive explicitly to a new benchmark or worked-example run with --package-archive provenance/releases/BERTopic_0.1.1.tar.gz.
