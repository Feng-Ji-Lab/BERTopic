# Validated Windows R-versus-Python benchmark

All 5 paired fresh-process comparisons passed. The 2247 documents produced 50 non-outlier topics and 808 outliers (36.0%).
The probability matrices have shape 2247 x 50. Maximum absolute probability difference: 4.8572257327350599e-16; maximum c-TF-IDF weight difference: 4.9960036108132005e-16 (tolerance 1e-12).

| Interface | Fit seconds | Cold-process seconds | Peak RSS MiB |
| --- | ---: | ---: | ---: |
| Python | 0.862 [0.795, 0.883] | 21.460 [21.350, 21.720] | 403.0 [402.9, 403.0] |
| R | 5.320 [5.250, 5.320] | 33.930 [33.270, 34.110] | 601.9 [601.9, 602.1] |

Paired R-minus-Python differences, median [Q1, Q3]: fit 4.388 [4.317, 4.525] seconds; cold process 12.090 [10.840, 12.650] seconds; peak RSS 198.9 [198.7, 199.1] MiB.

Embedding generation and reduction are excluded from fit timing. Both interfaces receive float64 column-major frozen inputs. Hardware, full environments, release and input hashes are in ../provenance.json.
These are measurements of this Windows configuration, not general performance or topic-quality claims. The packaged SMS input is unchanged; historical ham-selection provenance remains deferred (E).
