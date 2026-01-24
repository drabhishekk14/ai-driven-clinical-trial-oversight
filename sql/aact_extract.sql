/* ============================================================
   AACT DATA EXTRACTION
   Schema: ctgov
   Unit: one row per NCT ID (after aggregation)
   ============================================================ */

SELECT
    s.nct_id,

    /* ---- Core study fields ---- */
    s.phase,
    s.enrollment,
    s.primary_completion_date,
    s.results_first_submitted_date,

    /* ---- Design ---- */
    d.masking,
    d.allocation,

    /* ---- Sponsor ---- */
    sp.agency_class,

    /* ---- Intervention ---- */
    i.intervention_type,

    /* ---- Geography ---- */
    c.name AS country,

    /* ---- Outcomes (count) ---- */
    oc.num_outcomes,

    /* ---- Eligibility ---- */
    e.gender,
    e.healthy_volunteers,

    /* ---- Responsible party ---- */
    rp.responsible_party_type

FROM ctgov.studies s

/* designs */
LEFT JOIN ctgov.designs d
    ON s.nct_id = d.nct_id

/* sponsors */
LEFT JOIN ctgov.sponsors sp
    ON s.nct_id = sp.nct_id
   AND sp.lead_or_collaborator = 'lead'

/* interventions (first per study) */
LEFT JOIN (
    SELECT nct_id, intervention_type
    FROM ctgov.interventions
    GROUP BY nct_id, intervention_type
) i
    ON s.nct_id = i.nct_id

/* countries (first per study) */
LEFT JOIN (
    SELECT nct_id, name
    FROM ctgov.countries
    GROUP BY nct_id, name
) c
    ON s.nct_id = c.nct_id

/* outcomes (count per study) */
LEFT JOIN (
    SELECT nct_id, COUNT(*) AS num_outcomes
    FROM ctgov.outcomes
    GROUP BY nct_id
) oc
    ON s.nct_id = oc.nct_id

/* eligibilities */
LEFT JOIN ctgov.eligibilities e
    ON s.nct_id = e.nct_id

/* responsible parties */
LEFT JOIN ctgov.responsible_parties rp
    ON s.nct_id = rp.nct_id

WHERE
    s.study_type ILIKE '%Interventional%'
    AND s.primary_completion_date IS NOT NULL
    AND s.results_first_submitted_date IS NOT NULL;
