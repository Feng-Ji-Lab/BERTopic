# Validated Windows R-versus-Python benchmark

All 5 paired fresh-process comparisons passed. The 2247 documents produced 50 non-outlier topics and 808 outliers (36.0%).
The probability matrices have shape 2247 x 50. Maximum absolute probability difference: 4.8572257327350599e-16; maximum c-TF-IDF weight difference: 4.9960036108132005e-16 (tolerance 1e-12).

| Interface | Fit seconds | Cold-process seconds | Peak RSS MiB |
| --- | ---: | ---: | ---: |
| Python | 0.630 [0.630, 0.633] | 16.950 [16.870, 16.960] | 402.2 [402.1, 402.4] |
| R | 4.810 [4.810, 4.830] | 28.300 [28.280, 28.410] | 602.9 [601.3, 604.9] |

Paired R-minus-Python differences, median [Q1, Q3]: fit 4.183 [4.180, 4.192] seconds; cold process 11.400 [11.350, 11.540] seconds; peak RSS 200.0 [199.2, 202.5] MiB.

Embedding generation and reduction are excluded from fit timing. Both interfaces receive float64 column-major frozen inputs. Hardware, full environments, release and input hashes are in ../provenance.json.
These are measurements of this Windows configuration, not general performance or topic-quality claims. The packaged SMS input is unchanged; historical ham-selection provenance remains deferred (E).
