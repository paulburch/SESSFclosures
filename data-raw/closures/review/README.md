# Human review of the closure boundaries

`review_log.csv` records checks of the closure boundaries by a person, one row
per closure version (`version_id`, e.g. `C029_v2`). `data-raw/make_data.R`
joins the latest row for each version to `closures` as `human_review`,
`reviewed_by` and `review_date`. Versions with no row are `"not reviewed"`.

| Column | Values |
|---|---|
| `version_id` | closure version, as in `closures$version_id` |
| `result` | `correct`, `corrected` (geometry or attributes fixed; say how in `notes`), `issue` (problem found, not yet fixed) |
| `reviewed_by` | name |
| `review_date` | `YYYY-MM-DD` |
| `method` | e.g. `instrument text`, `AFMA polygon`, `map`, `instrument text + map` |
| `notes` | what was checked or changed |

A row for each check is kept, so the history stays. The latest row decides
the status.

When all closures have been reviewed:
1. Describe the review in `NEWS.md`: who, when, how, what was found and corrected.
2. Set `Version: 1.0.0` in `DESCRIPTION`.
3. Rebuild with `Rscript data-raw/make_data.R`.
