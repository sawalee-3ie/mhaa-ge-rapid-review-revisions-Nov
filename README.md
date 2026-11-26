# MHAA rapid review – revision workspace (Nov 2026)

Read this before editing anything in this repo. It records where the revision of the
preliminary report stands, what has been decided, and what is still open.

## Current files (`Report-revision/`)

| File | What it is |
|---|---|
| `GE-Rapid_review-Preliminary_report Nov 2026 SL_tracked revisions v8.docx` | **Current working version.** Reviewer-driven revisions as tracked changes (author "SL"), with replies in the reviewer comment threads (author "SL", each naming its tracker #). v8 = v7 with the author renamed from "Claude" to "SL" and the "[Claude, for SL]" prefix removed from the replies; content otherwise identical. v7 = v3 plus the Sharp et al. 2025a ED ESSI RCT (#162): counts, single-session wording, comparisons, short findings and the reference. |
| `GE-Rapid_review-Preliminary_report Nov 2026 SL_tracked revisions v7.docx` | Previous version (tracked changes and replies authored "Claude", replies prefixed "[Claude, for SL]"). Superseded by v8. |
| `Archive/GE-Rapid_review-Preliminary_report Nov 2026 SL_tracked revisions v3.docx` … `v6.docx` | Earlier tracked versions. v3 is the base for v7; v4–v6 were intermediate builds of the #162 changes (superseded). |
| `Archive/GE-Rapid_review-Preliminary_report with comments Oct 2026 SP 1.docx` | Original draft with reviewer comments. Do not edit. (v1 of the tracked revisions was superseded by v2 and removed.) |
| `GE_RR_Responses_to_reviewers Nov-24-2026_pending-first.docx` | **Use this one.** Same responses, reordered: Part A lists the items still pending (a check or decision needed), Part B the resolved ones (counts in the Part headings). Each entry has a status line from the revision log. |
| `Archive/GE_RR_Responses_to_reviewers Nov-24-2026.docx` | Earlier version of the responses, in report order, with SL's notes (tracked insertions by Sanghwa Lee). Superseded by the pending-first version. (Report v1 and v2 were superseded by v3 and removed; v2's Figure 1 drawing data was corrupted.) |
| `RR_tracked_revision_log_Nov-24-2026.csv` | **The tracker.** Status of every tracker item (Applied / Applied – partly / Held / No change) and why, sorted pending first. Start here to find what is left. |

`Dataset/` holds the extraction workbook; earlier workbooks, the section analysis scripts (R) and full-text PDFs are in `Dataset/Archive/`.

- `extraction_final-revised-SL_n=320 (6 are linked from 326)_v6_ESSI-RCT-1b3-cleared.xlsx` is the **current dataset**. It is v2 (`focus_18_24` columns in `baseline`, right after `base.5 value`, plus a Codebook row) with the Sharp et al. 2025a ED ESSI RCT added as record `ESSIRCT1` (EPPI 130138323), and 1b.3 (BEST) cleared for ESSIRCT1 and SW62FV8M (see #162 below). The R scripts still read the original filename.
- `ESSI_RCT_extraction_check v3 SL comments (tracker 162).xlsx` is SL's field-by-field verification of ESSIRCT1. `MHAA_extraction_framework-v1.docx` defines the extraction fields.
- `maps_18_24` (inside `base.5 value`) marks any **overlap** with ages 18–24 (152 records). `focus_18_24` = Yes marks studies **mainly about** ages 18–24: most participants aged about 18–24, judged from the reported age range, mean or median age (15 records, checked by SL on 25 Nov). Four university-student records with no ages reported are "Unclear (not counted)".

## Working rules

- Never overwrite a report version. Save a new versioned copy (v3, v4…) and edit that.
- Make report changes as tracked changes, and reply in the relevant reviewer comment thread saying which tracker # the change addresses.
- Do not apply a "Held" item from the log until its check is done or SL has signed it off.
- When an item is resolved, write its **final response to the reviewer** (past tense: what changed and why) in two places that must match: the Responses doc ("Final response to reviewer:" under the entry's status line, with the earlier text relabelled "Draft response (superseded):") and the tracker CSV column "Final response to reviewer". Internal notes go in "What was done / why held".
- Always refer to comments by **tracker number** (1–162, from the responses document and the CSV log). Every reply in the report names its tracker number ("tracker #N"); from v8, tracked changes and replies are authored "SL" (Claude prepares them, SL reviews), so new edits should use author "SL" with no "[Claude, for SL]" prefix. Word's internal comment IDs are not shown in Word and change every time the file is saved, so the "see #N" comment-ID cross-references in the original tracker spreadsheet (column K) no longer point to the right comments.

## Decisions already made by SL (do not reopen)

- **PICoST:** "C" stays **Context**.
- **Review type:** do not call it a "rapid scoping review". Use "rapid evidence review" with a descriptive analysis. Other existing terms are fine.
- **Verification is finished and the report is being finalised.** Do not say verification or extraction is ongoing. Limitations now says LLM-generated outputs underwent human verification and residual errors may remain (v2).
- **Protocol citation** is Sempe et al. 2026 (not Lucas et al.).
- **Protocol deviations to report:** citation tracking not done; active-learning stopping explored but target recall not reached, so all titles/abstracts screened manually. LLM calibration thresholds and values achieved should also be reported (details still needed).
- **No top-up search.** There are no resources for one; do not offer it to reviewers.
- **Kuta et al. 2026 (#104, #105, #149, checked 25 Nov against the paper in `Dataset/Kuta et al. 2026.pdf`):** the ChatGPT arm had no therapeutic prompt but users were asked to use it like a therapy chatbot; the structured arm was a generative AI app delivering solution-focused brief therapy; completion (all nine sessions: 39% vs 62%) was app-logged session starts in one arm and weekly self-reports in the other. Applied in 4.3 and 5.1.
- **Tessa (#29, #41, #124 resolved; Tessa parts of #144, #150 applied, 25 Nov):** Sharp et al. 2023 (JMIR 25:e50696) report Tessa was described as rule-based but included a proprietary AI algorithm, and suggest this and software upgrades were likely factors; the sequence of events is not documented, so the report does not say AI was added after deployment. The full account is told once, in 4.5; other sections point to it.
- **Route of broader digital evidence (#69, checked 24 Nov):** it entered mainly through reviews and overviews that covered chatbots alongside other digital tools; 310 of 319 records involve conversational agents. Applied in v3 (1.1, abstract Methods, 2.2). The dataset does not record which search each record came from, so this is inferred from design and intervention fields.
- **18–24 evidence (24 Nov; overlap count updated for #162 on 26 Nov):** reported in the report using the new `focus_18_24` flag: 152 records (47.5%) overlap ages 18–24, but only 15 (4.7%) primary studies focus on them (10 student samples, 8 GenAI, 8 US, 1 SSA, none South African). Applied in v3 for #8, #15, #34, #53, #62, #70, #71, #80, #113, #114, #117. If the 15 changes, these figures appear in the abstract, executive summary, 3.2 (text and table), 4.1 and Table 4.
- **Age framing (final for now, SL 25 Nov):** the review broadened to the WHO age range (10–24) to capture transferable findings while focusing on MHAA's age group (18–24) where possible; not called a protocol deviation. Applied in v3. Girl Effect will review it later.
- **MHAA decisions made after this review** (e.g. Girl Effect's language position) should not change wording that reports what the dataset found (#156).
- **Not added:** #79 (regulatory-incentive sentence; no supporting record); #85 kept internal.
- **References:** Schleider and Weisz 2017 and Schleider et al. 2025 are both correct for the single-session evidence (#72). Sharp et al. 2026 (JEM, published 20 July 2026) is after the search date, so it is cited as context, not as an included study.
- **#84:** no included primary study used a diagnostic outcome, so "symptoms" wording is correct.
- **#162 (resolved 26 Nov):** the Sharp et al. 2025 ED ESSI RCT (EPPI 130138323, excluded at title/abstract by human reviewers) is now included as `ESSIRCT1`, verified by SL against the extraction framework. Decisions:
  - Not linked to the co-design paper SW62FV8M (same intervention, different study), so both are counted: 320 studies, 326 reports.
  - Report counts: add 1 only where this study applies, starting from the report's own numbers (some report figures were edited by hand, so do not re-derive them from the dataset or R scripts); recalculate percentages over 320. Do not mention the addition in the report text (no "reinstated" sentence).
  - Citations: the RCT is Sharp et al. 2025a, the co-design paper 2025b.
  - 1b.3 (BEST) left blank for ESSIRCT1 and SW62FV8M: the co-design paper links the chatbot to Schleider et al.'s framework only in its Discussion, so it is not known whether the framework was used at the design stage. The report does not mention it.
  - Its information-sheet control counts as a minimal control; 3.3 and Table 5 now say "active, minimal (e.g. information-only) or usual-care comparators" (20).
  - Not recalculated: the 4.2 medians (effect magnitude 0.47 across 60 records; uptake 68% across 50; completion 69% across 67) and the durability split (12/11/6), which need the underlying values.

## Open points to check before finalising

1. **Section 2.3 vs Figure 1.** The new sentence says all titles and abstracts were screened manually, but Figure 1 shows 291 records "marked as ineligible by automation tools". Reconcile them.
2. **Table 1 legend.** The heading and title now say "Provisional design hypotheses", but the traffic-light legend and the "Readiness (G/A/R)" column still talk about "locking". Decide whether to keep the colour column, then re-rate the rows (#47, #50).
3. **Eligibility table (#61) not inserted.** Its draft exclusion "AI tools used only for diagnosis or prediction" conflicts with included records (e.g. depression-screening chatbot reviews). Other bracketed cells are unconfirmed.
4. **Moving Section 4.1 into the Discussion (#77)** has not been done. Easier after the tracked changes are accepted. All section numbers and cross-references then shift.
5. **Technical Appendix (Annex B, "Sensitivity of downstream numbers")** still says the MDE and 35% attrition assumption "will be revised when … extractions are complete". This conflicts with the report being final; not yet changed.
6. ~~Section 3.3 keyword-classified counts (#75)~~ Resolved 25 Nov: verification is complete at this stage, so the main text no longer says counts were "keyword-classified" or "automatically identified".
7. **"Preliminary" status.** The title and Section 1.4 still call this a preliminary report. Confirm whether that stays in the final version.
8. **References to check:** Schleider et al. 2025 has no volume page range yet; Ali et al. 2025 is cited in 1.1 but appears to be missing from the reference list; the protocol (Sempe et al. 2026) is not in the reference list, and SL queried whether it was registered on OSF (#11).
9. **Refresh fields** (table of contents, list of tables) in Word after accepting changes.
10. **Internal inconsistencies flagged in the responses document** are mostly not fixed yet: total records 319 / 307 / 325; "35% attrition" vs Table 5 "Retain 35"; uptake n = 58 vs 50 and completion n = 86 vs 67 in 4.2; Prochaska 98% accuracy vs 97%/99%; "cognitive reframing" vs "restructuring"; two "Table 1"s; Wang 2025a/b; Xu vs Xue; Stephens 2020 vs 2022; Meadi → Rahsepar Meadi; Heinz 2025 missing from references.

## Held items needing outside input

- **Girl Effect:** language position (#21, #42, #49, #108, #137, #156), later review of the age framing (already applied), how Girl Effect and MHAA are described (#1, #5, #7, #9, #32), MHAA design and evaluation choices (#45, #96, #152, #157).
- **Source checks:** Heinz et al. 2025 (#138), Rollwage et al. 2024 (#147), 85% treatment-gap source (#2, #51).
- **Dataset or extraction work:** #16, #26, #33, #81, #86, #88, #155, #158, #161.

See the CSV log for the full list and the reason for each.

## How the v1/v2 edits were made

The tracked changes were generated by script from the responses document and SL's notes. Rejecting all of the scripted changes (author "Claude" up to v7, "SL" from v8) restores the original text exactly; the files pass the docx schema validator. They have not been visually checked page by page: open them in Word before circulating.

Caution for scripted edits: re-serialising document.xml with a minidom-based tool turned the line breaks in the 29 `o:gfxdata` drawing attributes into spaces, which broke Figure 1 in v2. If you edit the XML by script, check afterwards that every `o:gfxdata` value is unchanged. Also, saving in Word renumbers comment IDs, so find a reviewer comment by its text (not by an ID recorded earlier) before replying to it.
