# Validated Windows R-versus-Python benchmark

All 5 paired fresh-process comparisons passed. The 2247 documents produced 50 non-outlier topics and 808 outliers (36.0%).
The probability matrices have shape 2247 x 50. Maximum absolute probability difference: 4.8572257327350599e-16; maximum c-TF-IDF weight difference: 4.9960036108132005e-16 (tolerance 1e-12).

| Interface | Fit seconds | Cold-process seconds | Peak RSS MiB |
| --- | ---: | ---: | ---: |
| Python | 0.644 [0.640, 0.647] | 17.490 [17.200, 17.720] | 402.3 [402.2, 402.4] |
| R | 4.940 [4.930, 5.170] | 29.420 [28.680, 30.270] | 602.5 [602.3, 604.9] |

Paired R-minus-Python differences, median [Q1, Q3]: fit 4.300 [4.283, 4.523] seconds; cold process 12.280 [11.440, 12.290] seconds; peak RSS 200.4 [200.1, 202.7] MiB.

Embedding generation and reduction are excluded from fit timing. Both interfaces receive float64 column-major frozen inputs. Hardware, full environments, release and input hashes are in ../provenance.json.
These are measurements of this Windows configuration, not general performance or topic-quality claims. The packaged SMS input is unchanged; historical ham-selection provenance remains deferred (E).
