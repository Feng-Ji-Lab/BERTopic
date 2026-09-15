# Archive input and UTF-8 corrections (2026-09-15)

The previous public analysis tag remains immutable. These corrections change documentation and source-archive byte handling, while retaining the exact installed BERTopic 0.1.2 package archive and all formal numerical outputs.

## Confirmed failure

The formal SMS CSV SHA-256 is 2f90a92a0503f4260af73ba884c1db7af88c253dd783f12a20a485f3fb036675. Its previous Git blob hash was 22ed59f7ad7b5e94e89bf04406ff0c4ea29776f60641a21ffcfc2fda0b6249d1 after CRLF normalization.

The prior ZIP, built with the original Windows Git configuration, matched the formal hash. Rebuilding the previous commit with core.autocrlf=false produced the normalized hash instead. The full offline clone test had passed under the original configuration, so it did not expose this configuration dependence.

## Correction and checks

- The existing data/sms_spam.csv bytes are now protected with -text. No SMS row, label, text, selection or order changes.
- The verifier's retained-file manifest includes this CSV.
- The ZIP builder fixes core.autocrlf=false and explicitly verifies the packaged CSV against the frozen-input provenance.
- A fresh bundle clone with core.autocrlf=false is checked against the retained file hashes.
- Both review status introductions and affected CRediT role names are restored as valid UTF-8, and Windows-scoped completion markers are corrected.

The earlier five paired numerical comparisons, six restoration checks, and eleven byte-identical example CSV comparisons remain the numerical verification evidence. These archive/documentation corrections do not require changing the package version or rerunning fitted models.