# RQ 1b — Intervention design: analysis scaffold

## Interpretation rule
Counts below describe records and reporting patterns, not independent studies and not effectiveness. Linked reports may represent the same underlying study. Any claim that a design feature is associated with outcomes should be traced to `design_outcome_links`, prioritising experimental comparisons and distinguishing them from observational, qualitative and author-interpreted evidence.

## Direct preliminary answer — evidence to inspect
- Most frequently tagged architecture profiles: Generative AI / LLM (n=81); Rule-based / deterministic (n=72); AI/NLP, architecture unclear (n=57); Other digital or unclear (n=41); Multiple/varied architectures (n=30).
- Most frequently tagged therapeutic approaches: Cognitive behavioural therapy (CBT) (n=170); Mindfulness (n=93); Psychoeducation (n=59); Behavioural activation (n=34); Cognitive restructuring/reframing (n=34); Positive psychology (n=34); Acceptance and commitment therapy (ACT) (n=31).
- Parsed design–outcome links: 216 total; 82 experimentally classified; 98 observationally classified; 57 qualitatively classified; and 65 containing author interpretation. Categories can overlap in the raw extraction.
- Draft the preliminary answer from the strongest recurring findings in `experimental_links` and `feature_outcome_theme_summary`; do not infer effectiveness from the frequency tables.

## Evidence base
- RQ1b contribution: n=307 (96.2%).
- Primary quantitative: n=56 (18.2%).
- Qualitative or mixed-methods: n=79 (25.7%).
- Evidence syntheses: n=114 (37.1%).
- Adolescent/young-person direct: n=122 (39.7%).
- Generative-AI/LLM direct: n=137 (44.6%).
- South Africa direct: n=6 (2.0%).
- Direct on all three operationalised MHAA dimensions: n=3 (1.0%).
- Most common study designs: mixed-methods (n=50); SR (n=46); scoping (n=37); other (n=35); qualitative (n=29); SR+MA (n=28); RCT (n=27); framework (n=20).
- Most common settings: online (n=119); mixed (n=82); Not reported (n=50); other (n=29); not reported (n=11); school (n=7).
- Most represented countries/country labels: USA (n=106); GBR (n=48); AUS (n=38); CHN (n=33); Not reported (n=32); DEU (n=24); KOR (n=24); CAN (n=23); ITA (n=18); JPN (n=18).

## Key findings — tables to use
- Modalities and technology: `architecture_counts`, `modality_tag_counts`, and `architecture_approach_heatmap`.
- Therapeutic approaches and active components: `approach_tag_counts` and the raw 1b.2 rows in `quote_bank`.
- Session dose and structure: `session_structure_counts` and `session_detail`.
- Personalisation, interaction, human support, engagement and co-design: `design_feature_counts` and `design_feature_tags`.
- Feature–outcome evidence: `design_outcome_links`, `experimental_links`, and `feature_outcome_theme_summary`.
- BEST elements: `best_detail` and `best_counts`; reporting is expected to be sparse, so state the denominator explicitly.

## Implications for MHAA — decision points
Use the strongest direct evidence to address: deterministic workflow versus model orchestration; a 15–30-minute single-session format; portability/adaptation of the Project YES corpus; candidate A/B variants; degree of human support; safeguarding and escalation. Label any recommendation that is based on pattern interpretation rather than comparative evidence as team interpretation.

## Gaps and caveats
- Counts are records, not deduplicated studies.
- Keyword tags are transparent aids for synthesis and must be checked against the raw value and quote columns.
- Systematic reviews can describe multiple intervention types; their tags should not be interpreted as one intervention architecture.
- A positive or negative direction in 1b.11 does not by itself establish causality; use `evidence_class` and the original quote.
- BEST-element reporting is sparse and should not be treated as absence unless the source was explicitly assessed.
- The GenAI directness flag is keyword-based and should be manually verified for borderline systems.
- South Africa directness uses the extracted baseline flag; multi-country and regional records require contextual checking.

## Manual checks before drafting
1. Resolve relevant rows in `review_queue_1b`.
2. Spot-check the five records in `verification_sample` against source PDFs.
3. Verify all BEST claims against their evidence spans.
4. Review all experimental links and any design–outcome claim proposed for the report.
5. State the linked-publication/record-overlap caveat.
