"""Regenerate manuscript benchmark facts/table bodies from validated Windows outputs."""
import argparse
import csv
import json
import re
from pathlib import Path


def rows(path):
    with open(path, encoding="utf-8-sig", newline="") as stream:
        return list(csv.DictReader(stream))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--manuscript", default="../bert/master.tex")
    parser.add_argument("--results", default="benchmark/results")
    parser.add_argument("--example", default="benchmark/example-results")
    args = parser.parse_args()
    root, example = Path(args.results), Path(args.example)
    eq, checks = rows(root / "equivalence.csv"), rows(example / "restoration_checks.csv")
    if not eq or not checks or not all(x["passed"].lower() == "true" for x in checks):
        raise ValueError("Complete worked-example restoration must pass first")
    if not all(v.lower() == "true" for x in eq for k, v in x.items() if "equal" in k):
        raise ValueError("Benchmark equivalence failed")
    p = json.loads((root / "provenance.json").read_text(encoding="utf-8"))
    s = {x["metric"]: x for x in rows(root / "summary.csv")}
    f = {x["metric"]: float(x["value"]) for x in rows(root / "artifacts/facts.csv")}
    rm = {x["metric"]: x["value"] for x in rows(root / "runs/r-01/metrics.csv")}
    pm = {x["metric"]: x["value"] for x in rows(root / "runs/python-01/metrics.csv")}
    version, commit = rm["package_version"], p["release_commit"][:7]
    if p["release_tag"] != "v" + version:
        raise ValueError("Release tag differs from the installed package")
    value = lambda key, digits=3: f'{float(s[key]["median"]):.{digits}f}'
    n, c, o = (int(f[k]) for k in ("documents", "non_outlier_topics", "outliers"))
    path = Path(args.manuscript)
    lines = path.read_text(encoding="utf-8").splitlines()

    def replace(prefix, content):
        indices = [i for i, line in enumerate(lines) if line.startswith(prefix)]
        if not indices and content in lines:
            return
        if len(indices) != 1:
            raise ValueError("Expected exactly one paragraph: " + str(prefix))
        lines[indices[0]] = content

    abstract = next(x for x in lines if x.startswith("BERTopic provides an embedding-based approach"))
    abstract = re.sub(r"The two paths returned identical.*?peak memory\.",
                      f"The two paths returned identical assignments, metadata, and ordered term keys; weights and membership-strength matrices agreed within an absolute tolerance of 1e-12. In this controlled Windows task, paired R-minus-Python differences had medians of {value('fit_difference')} seconds for fitting, {value('cold_difference')} seconds for cold-process time, and {value('peak_rss_difference',1)} MiB for peak memory.", abstract)
    replace("BERTopic provides an embedding-based approach", abstract)
    replace("Because the package calls Python,", rf"Because the package calls Python, backend installation is part of normal use. Version {version} provides Conda and virtualenv helpers and \code{{install\_py\_deps()}} as a dispatcher. Both routes consume the same exact requirements and run the same import validation. The supported tested configuration is Windows, Python 3.10.21, and BERTopic 0.16.0. Full resolved environments are retained with the outputs; other backend versions are outside this support claim.")
    replace("Two functions support routine diagnosis.", r"Two functions support routine diagnosis. \code{bertopic\_available()} checks importability, while \code{bertopic\_session\_info()} reports the interpreter and exact versions for 11 key modules. Full Python distribution versions and R session information are also retained. The documented \code{bertopic\_self\_check()} fits, transforms, saves, loads, and compares cached fields, topic metadata, and post-load transformation outputs.")
    inp = p["input_manifest"]
    short = lambda x: x[:8] + "..." + x[-8:]
    replace("The benchmark used the 2,247 SMS messages", rf"The benchmark used {n:,} SMS messages and installed \pkg{{BERTopic}} {version}, tag \fcode{{v{version}}}, source commit \fcode{{{commit}}}. The exact source archive checksum is retained with the outputs. Embeddings were generated once with \fcode{{all-MiniLM-L6-v2}} at immutable revision \fcode{{{inp['model_revision']}}}, then reduced from 384 to five dimensions with UMAP and \fcode{{random_state = 42}}. The frozen array was stored as float64 in column-major order to match R's conversion through \pkg{{reticulate}}. Both paths used \fcode{{BaseDimensionalityReduction()}}, HDBSCAN with \fcode{{min_cluster_size = 10}} and \fcode{{prediction_data = TRUE}}, an English-stop-word \fcode{{CountVectorizer}}, and \fcode{{calculate_probabilities = TRUE}}. Document and array SHA-256 hashes were \fcode{{{short(inp['documents_file_sha256'])}}} and \fcode{{{short(inp['embeddings_sha256'])}}}; full hashes and preparation settings are retained in the manifest.")
    replace("Five fresh Python processes", rf"Five fresh Python processes and five fresh R processes ran on Windows x64 (build {p['windows_version'][1].split('.')[-1]}), with {p['physical_cpu_count']} physical cores, {p['logical_cpu_count']} logical processors, and {p['total_memory_bytes']/1024**3:.1f} GiB of physical memory visible to the operating system. The stack was R {rm['R_version'].split()[2]}, Python {pm['python_version'].split()[0]}, BERTopic {pm['bertopic_version']}, NumPy {pm['numpy_version']}, scikit-learn {pm['sklearn_version']}, PyTorch {pm['torch_version']}, transformers {pm['transformers_version']}, UMAP {pm['umap_version']}, and HDBSCAN {pm['hdbscan_version']}. Process order alternated, with R first on odd repetitions. Both paths inherited \fcode{{PYTHONHASHSEED = 42}} and identical single-thread settings for OpenMP and common numerical libraries. Components were constructed before timing. Python used \code{{time.perf\_counter()}}; R used elapsed \code{{proc.time()}}. R fit timing includes wrapper validation and conversion. Cold-process time includes startup, imports, input loading, component construction, fitting, and output writing. Windows peak resident memory was read from \fcode{{psutil.Process().memory_info().peak_wset}}. These are workflow measurements, not a language-neutral microbenchmark.")
    replace("The controlled model produced", rf"The controlled model produced {c} non-outlier topics and {o} outlier messages ({100*o/n:.1f}\%). Table~\ref{{tab:topic_examples}} shows the eight largest topics. Terms are model outputs rather than author-assigned labels and do not validate behavioral constructs.")
    replace(r"Table~\ref{tab:equivalence} reports", rf"Table~\ref{{tab:equivalence}} reports the paired comparison. All five pairs returned matching {n:,} assignments, {o} outliers, \code{{Topic}}/\code{{Count}}/\code{{Name}} metadata in {int(f['topic_info_rows'])} rows, and ordered top-ten term keys. Maximum absolute weight and membership-strength differences were \fcode{{{f['max_term_weight_difference']:.3g}}} and \fcode{{{f['max_probability_difference']:.3g}}}, below the $10^{{-12}}$ tolerance. Membership-strength matrices had dimensions $ {n}\times {c}$.")
    replace(r"Table~\ref{tab:benchmark} reports", rf"Table~\ref{{tab:benchmark}} reports medians and interquartile ranges from five fresh processes per interface. Median fitting took {value('python_fit_seconds')} seconds in Python and {value('r_fit_seconds')} seconds in R. Paired R-minus-Python differences had medians of {value('fit_difference')} seconds for fitting, {value('cold_difference')} seconds for cold-process time, and {value('peak_rss_difference',1)} MiB for peak memory. Paired-difference medians are summarized separately from medians for each interface.")
    replace("The direct Python process used less time", f"Direct Python used less time and memory in this Windows configuration. Paired median differences were {value('fit_difference')} seconds for fitting, {value('cold_difference')} seconds for cold-process time, and {value('peak_rss_difference',1)} MiB for peak memory. Embedding generation and reduction were excluded. These five replications of one corpus and configuration do not establish general performance advantages.")
    replace("The archived static figures", r"The complete worked example was rerun using the installed corrected release and pinned model revision. Its printed output, tables, HTML, Plotly JSON, and static figures come from its own fitted model. The safetensors check verifies topic metadata; the additional full pickle round trip verifies cached assignments/strengths and post-load predictions. Lightweight formats omit the reducer and clusterer, so the metadata check does not assert identical HDBSCAN inference after lightweight loading.")
    replace((r"\pkg{BERTopic} version", "The evaluation used the"), rf"The evaluation used the versioned \pkg{{BERTopic}} {version} source archive. Development source and issue tracking are at \url{{https://github.com/Feng-Ji-Lab/BERTopic}}; the CRAN project page is \url{{https://CRAN.R-project.org/package=BERTopic}}. The exact tested source and checksum are retained with the analysis materials. The tested tag is public at \url{{https://github.com/Feng-Ji-Lab/BERTopic/tree/v0.1.2}}, and the exact installed archive is \href{{https://github.com/Feng-Ji-Lab/BERTopic/raw/64907e737025fe456d3673afcf0f467df4deb20d/provenance/releases/BERTopic_0.1.2.tar.gz}}{{retained publicly at an immutable commit}}. An archive DOI remains pending. The R interface is MIT-licensed; dependencies retain their respective licenses.")
    replace("The evaluation remains narrow.", r"The controlled benchmark covers one Windows configuration, one short-text corpus, one backend version, and five timing replications; embedding generation and reduction are excluded. The corrected Windows package passed its full regression suite without failures, warnings, or skips. This evidence does not validate every constructor configuration or establish substantive topic quality.")
    replace(r"\textbf{Code availability.}", rf"\textbf{{Code availability.}} Development source is at \url{{https://github.com/Feng-Ji-Lab/BERTopic}}. The benchmark used version {version}, tag \fcode{{v{version}}}, commit \fcode{{{commit}}}, and the retained source archive checksum. \todo{{Insert the published version-specific archive DOI for the exact source and analysis scripts.}}")
    replace("In its current form,", r"In its current form, \pkg{BERTopic} provides an R-oriented access layer and preserves the selected outputs tested here. The contribution is interface design, workflow documentation, and bounded input--output validation. \todo{Before submission, publish the tested versioned release, supply the archive DOI, and complete outstanding dataset-provenance and author-information fields.}")
    lines = [x for x in lines if not x.startswith((r"\url{https://CRAN.R-project.org/package=BERTopic}; its CRAN package DOI", r"\url{https://github.com/Feng-Ji-Lab/BERTopic}. The R package is released"))]
    text = "\n".join(lines) + "\n"
    text = text.replace("Version 0.1.0", "Version " + version)
    text = text.replace("version 0.1.0", "version " + version)
    text = text.replace('install.packages("BERTopic")', f'install.packages("provenance/releases/BERTopic_{version}.tar.gz",\n                 repos = NULL, type = "source")')
    if "sentence_transformers <-" not in text:
        text = text.replace('set_bertopic_seed(42)\numap <-', 'set_bertopic_seed(42)\nsentence_transformers <- reticulate::import("sentence_transformers", convert = FALSE)\nembedding_model <- sentence_transformers$SentenceTransformer(\n  "all-MiniLM-L6-v2", revision = "'+inp["model_revision"]+'"\n)\numap <-')
        text = text.replace('  embedding_model = "all-MiniLM-L6-v2",', '  embedding_model = embedding_model,')
    for label, name in (("tab:topic_examples","largest_topics_table.tex"),("tab:equivalence","equivalence_table.tex"),("tab:benchmark","runtime_table.tex")):
        pos = text.index(r"\label{" + label + "}")
        begin = text.index(r"\midrule", pos) + len(r"\midrule")
        end = text.index(r"\bottomrule", begin)
        body = (root/"artifacts"/name).read_text(encoding="utf-8").split(r"\midrule",1)[1].split(r"\bottomrule",1)[0]
        text = text[:begin] + body + text[end:]
    text = text.replace("Version " + version + " also targets an older Python BERTopic release.",
                        "The tested backend is pinned to Python BERTopic 0.16.0.")
    text = text.replace("stopifnot(nrow(restored_topic_info) == nrow(updated_topic_info))",
                        'stopifnot(identical(\n  as.data.frame(restored_topic_info)[c("Topic", "Count", "Name")],\n  as.data.frame(updated_topic_info)[c("Topic", "Count", "Name")]\n))')
    text = text.replace("Before submission, publish the tested versioned release, supply the archive DOI", "Before submission, supply the archive DOI")
    path.write_text(text, encoding="utf-8")
    print("Updated factual manuscript content for release", version, "; E/F placeholders remain deferred")


if __name__ == "__main__":
    main()
