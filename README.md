# MHAA rapid review – revision workspace (Nov 2026)

Read this before editing anything in this repo. It records where the revision of the
preliminary report stands, what has been decided, and what is still open.

## Current files (`Report-revision/`)

| File | What it is |
|---|---|
| `GE-Rapid_review-Preliminary_report Nov 2026 SL_tracked revisions v2.docx` | **Current working version.** Reviewer-driven revisions as tracked changes (author "Claude"), with replies in the reviewer comment threads (prefixed "[Claude, for SL]"). |
| `Archive/GE-Rapid_review-Preliminary_report with comments Oct 2026 SP 1.docx` | Original draft with reviewer comments. Do not edit. (v1 of the tracked revisions was superseded by v2 and removed.) |
| `GE_RR_Responses_to_reviewers Nov-24-2026.docx` | Draft responses to all 162 tracker comments, with SL's notes (SL's notes are tracked insertions by Sanghwa Lee). |
| `RR_tracked_revision_log_Nov-24-2026.csv` | **Status of every tracker item** (Applied / Applied – partly / Held / No change) and why. Start here to find what is left. |

`Dataset/` holds the extraction workbook and the section analysis scripts (R).

## Working rules

- Never overwrite a report version. Save a new versioned copy (v3, v4…) and edit that.
- Make report changes as tracked changes, and reply in the relevant reviewer comment thread saying which tracker # the change addresses.
- Do not apply a "Held" item from the log until its check is done or SL has signed it off.
- Tracker numbers (1–162) come from the responses document. Word comment IDs are different; the responses document explains the mapping.

## Decisions already made by SL (do not reopen)

- **PICoST:** "C" stays **Context**.
- **Review type:** do not call it a "rapid scoping review". Use "rapid evidence review" with a descriptive analysis. Other existing terms are fine.
- **Verification is finished and the report is being finalised.** Do not say verification or extraction is ongoing. Limitations now says LLM-generated outputs underwent human verification and residual errors may remain (v2).
- **Protocol citation** is Sempe et al. 2026 (not Lucas et al.).
- **Protocol deviations to report:** citation tracking not done; active-learning stopping explored but target recall not reached, so all titles/abstracts screened manually. LLM calibration thresholds and values achieved should also be reported (details still needed).
- **No top-up search.** There are no resources for one; do not offer it to reviewers.
- **No 18–24 subgroup coding exists in the dataset.** Responses that promise 18–24 counts (#15, #34, #53, #62, #70, #71, #80) need redrafting.
- **Age framing:** SL prefers "we broadened the age group following the WHO classification to capture transferable findings, and focused on the MHAA age group where possible", rather than calling it a protocol deviation. Still to agree with Girl Effect.
- **MHAA decisions made after this review** (e.g. Girl Effect's language position) should not change wording that reports what the dataset found (#156).
- **Not added:** #79 (regulatory-incentive sentence; no supporting record); #85 kept internal.
- **References:** Schleider and Weisz 2017 and Schleider et al. 2025 are both correct for the single-session evidence (#72). Sharp et al. 2026 (JEM, published 20 July 2026) is after the search date, so it is cited as context, not as an included study.
- **#84:** no included primary study used a diagnostic outcome, so "symptoms" wording is correct.
- **#162:** SL wants the Sharp et al. 2025 ED ESSI RCT included (EPPI ID 130138323, excluded at title/abstract). This needs extraction and updates to the dataset and counts.

## Open points to check before finalising

1. **Section 2.3 vs Figure 1.** The new sentence says all titles and abstracts were screened manually, but Figure 1 shows 291 records "marked as ineligible by automation tools". Reconcile them.
2. **Table 1 legend.** The heading and title now say "Provisional design hypotheses", but the traffic-light legend and the "Readiness (G/A/R)" column still talk about "locking". Decide whether to keep the colour column, then re-rate the rows (#47, #50).
3. **Eligibility table (#61) not inserted.** Its draft exclusion "AI tools used only for diagnosis or prediction" conflicts with included records (e.g. depression-screening chatbot reviews). Other bracketed cells are unconfirmed.
4. **Moving Section 4.1 into the Discussion (#77)** has not been done. Easier after the tracked changes are accepted. All section numbers and cross-references then shift.
5. **Technical Appendix (Annex B, "Sensitivity of downstream numbers")** still says the MDE and 35% attrition assumption "will be revised when … extractions are complete". This conflicts with the report being final; not yet changed.
6. **Section 3.3** still reports "87 records were keyword-classified as GenAI-specific…". SL wants no "provisional" language. Decide how to describe these counts (#75).
7. **"Preliminary" status.** The title and Section 1.4 still call this a preliminary report. Confirm whether that stays in the final version.
8. **References to check:** Schleider et al. 2025 has no volume page range yet; Ali et al. 2025 is cited in 1.1 but appears to be missing from the reference list; the protocol (Sempe et al. 2026) is not in the reference list, and SL queried whether it was registered on OSF (#11).
9. **Refresh fields** (table of contents, list of tables) in Word after accepting changes.
10. **Internal inconsistencies flagged in the responses document** are mostly not fixed yet: total records 319 / 307 / 325; "35% attrition" vs Table 5 "Retain 35"; uptake n = 58 vs 50 and completion n = 86 vs 67 in 4.2; Prochaska 98% accuracy vs 97%/99%; "cognitive reframing" vs "restructuring"; two "Table 1"s; Wang 2025a/b; Xu vs Xue; Stephens 2020 vs 2022; Meadi → Rahsepar Meadi; Heinz 2025 missing from references.

## Held items needing outside input

- **Girl Effect:** language position (#21, #42, #49, #108, #137, #156), age framing, how Girl Effect and MHAA are described (#1, #5, #7, #9, #32), MHAA design and evaluation choices (#45, #96, #152, #157).
- **Source checks:** Tessa (JMIR 2023;25:e50696; #29, #41, #124, #144, #150), Kuta et al. 2026 completion definition (#104, #105, #149), Heinz et al. 2025 (#138), Rollwage et al. 2024 (#147), 85% treatment-gap source (#2, #51).
- **Dataset or extraction work:** #16, #26, #33, #69, #80, #81, #86, #88, #126, #155, #158, #160, #161, #162.

See the CSV log for the full list and the reason for each.

## How the v1/v2 edits were made

The tracked changes were generated by script from the responses document and SL's notes. Rejecting all of Claude's changes restores the original text exactly; the files pass the docx schema validator. They have not been visually checked page by page: open them in Word before circulating.
