# MHAA rapid review – revision workspace (Nov 2026)

Read this before editing anything in this repo. It records where the revision of the
preliminary report stands, what has been decided, and what is still open.

## Current files (`Report-revision/`)

| File | What it is |
|---|---|
| `GE-Rapid_review-Preliminary_report Nov 2026 SL_tracked revisions v3.docx` | **Current working version.** Reviewer-driven revisions as tracked changes (author "Claude"), with replies in the reviewer comment threads (prefixed "[Claude, for SL]"). Same as v2 except Figure 1 (PRISMA diagram) is repaired. |
| `Archive/GE-Rapid_review-Preliminary_report with comments Oct 2026 SP 1.docx` | Original draft with reviewer comments. Do not edit. (v1 of the tracked revisions was superseded by v2 and removed.) |
| `GE_RR_Responses_to_reviewers Nov-24-2026_pending-first.docx` | **Use this one.** Same responses, reordered: Part A lists the 95 items still pending (a check or decision needed), Part B the 67 resolved ones. Each entry has a status line from the revision log. |
| `Archive/GE_RR_Responses_to_reviewers Nov-24-2026.docx` | Earlier version of the responses, in report order, with SL's notes (tracked insertions by Sanghwa Lee). Superseded by the pending-first version. (Report v1 and v2 were superseded by v3 and removed; v2's Figure 1 drawing data was corrupted.) |
| `RR_tracked_revision_log_Nov-24-2026.csv` | **The tracker.** Status of every tracker item (Applied / Applied – partly / Held / No change) and why, sorted pending first. Start here to find what is left. |

`Dataset/` holds the extraction workbook and the section analysis scripts (R).

- `extraction_final-revised-SL_n=319 (6 are linked from 325)_v2_focus18-24.xlsx` is the **current dataset**: identical to the original workbook except for two new columns in `baseline`, right after `base.5 value`: `base.5 focus_18_24` (Yes / Unclear (not counted) / No / Not reported) and `base.5 focus_18_24 basis`, plus a matching Codebook row. The R scripts still read the original filename.
- `maps_18_24` (inside `base.5 value`) marks any **overlap** with ages 18–24 (151 records). `focus_18_24` = Yes marks studies **mainly about** ages 18–24: most participants aged about 18–24, judged from the reported age range, mean or median age (15 records, checked by SL on 25 Nov). Four university-student records with no ages reported are "Unclear (not counted)".

## Working rules

- Never overwrite a report version. Save a new versioned copy (v3, v4…) and edit that.
- Make report changes as tracked changes, and reply in the relevant reviewer comment thread saying which tracker # the change addresses.
- Do not apply a "Held" item from the log until its check is done or SL has signed it off.
- Always refer to comments by **tracker number** (1–162, from the responses document and the CSV log). Every Claude reply in the report starts with "tracker #N". Word's internal comment IDs are not shown in Word and change every time the file is saved, so the "see #N" comment-ID cross-references in the original tracker spreadsheet (column K) no longer point to the right comments.

## Decisions already made by SL (do not reopen)

- **PICoST:** "C" stays **Context**.
- **Review type:** do not call it a "rapid scoping review". Use "rapid evidence review" with a descriptive analysis. Other existing terms are fine.
- **Verification is finished and the report is being finalised.** Do not say verification or extraction is ongoing. Limitations now says LLM-generated outputs underwent human verification and residual errors may remain (v2).
- **Protocol citation** is Sempe et al. 2026 (not Lucas et al.).
- **Protocol deviations to report:** citation tracking not done; active-learning stopping explored but target recall not reached, so all titles/abstracts screened manually. LLM calibration thresholds and values achieved should also be reported (details still needed).
- **No top-up search.** There are no resources for one; do not offer it to reviewers.
- **Route of broader digital evidence (#69, checked 24 Nov):** it entered mainly through reviews and overviews that covered chatbots alongside other digital tools; 310 of 319 records involve conversational agents. Applied in v3 (1.1, abstract Methods, 2.2). The dataset does not record which search each record came from, so this is inferred from design and intervention fields.
- **18–24 evidence (24 Nov):** reported in the report using the new `focus_18_24` flag: 151 records (47.3%) overlap ages 18–24, but only 15 (4.7%) primary studies focus on them (10 student samples, 8 GenAI, 8 US, 1 SSA, none South African). Applied in v3 for #8, #15, #34, #53, #62, #70, #71, #80, #113, #114, #117. If the 15 changes, these figures appear in the abstract, executive summary, 3.2 (text and table), 4.1 and Table 4.
- **Age framing (final for now, SL 25 Nov):** the review broadened to the WHO age range (10–24) to capture transferable findings while focusing on MHAA's age group (18–24) where possible; not called a protocol deviation. Applied in v3. Girl Effect will review it later.
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

- **Girl Effect:** language position (#21, #42, #49, #108, #137, #156), later review of the age framing (already applied), how Girl Effect and MHAA are described (#1, #5, #7, #9, #32), MHAA design and evaluation choices (#45, #96, #152, #157).
- **Source checks:** Tessa (JMIR 2023;25:e50696; #29, #41, #124, #144, #150), Kuta et al. 2026 completion definition (#104, #105, #149), Heinz et al. 2025 (#138), Rollwage et al. 2024 (#147), 85% treatment-gap source (#2, #51).
- **Dataset or extraction work:** #16, #26, #33, #81, #86, #88, #126, #155, #158, #160, #161, #162.

See the CSV log for the full list and the reason for each.

## How the v1/v2 edits were made

The tracked changes were generated by script from the responses document and SL's notes. Rejecting all of Claude's changes restores the original text exactly; the files pass the docx schema validator. They have not been visually checked page by page: open them in Word before circulating.

Caution for scripted edits: re-serialising document.xml with a minidom-based tool turned the line breaks in the 29 `o:gfxdata` drawing attributes into spaces, which broke Figure 1 in v2. If you edit the XML by script, check afterwards that every `o:gfxdata` value is unchanged. Also, saving in Word renumbers comment IDs, so find a reviewer comment by its text (not by an ID recorded earlier) before replying to it.
