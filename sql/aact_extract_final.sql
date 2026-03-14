/* ============================================================
   AACT DATA EXTRACTION — ONE ROW PER NCT ID
   WITH COVID FLAG
   + LEAD SPONSOR NAME
   + CERTIFICATION INFORMATION
   ============================================================ */

SELECT
    s.nct_id,

    /* Core study fields */
    s.phase,
    s.enrollment,
    s.primary_completion_date,
    s.results_first_submitted_date,

    /* Certification / delayed results information */
    CAST(s.disposition_first_submitted_date AS TEXT) 
    AS disposition_first_submitted_date,

    /* COVID flag */
    CASE
        WHEN s.primary_completion_date 
             BETWEEN '2020-03-01' AND '2022-12-31'
        THEN 1 ELSE 0
    END AS covid_period,

    /* Design */
    d.masking,
    d.allocation,

    /* Sponsor */
    sp.agency_class,
    sp.name AS lead_sponsor_name,

    /* Intervention (single per study) */
    i.intervention_type,

    /* Geography */
    c.num_countries,

    /* Outcomes */
    oc.num_outcomes,

    /* Eligibility */
    e.gender,
    e.healthy_volunteers,

    /* Responsible party */
    rp.responsible_party_type

FROM ctgov.studies s

LEFT JOIN ctgov.designs d
    ON s.nct_id = d.nct_id

LEFT JOIN ctgov.sponsors sp
    ON s.nct_id = sp.nct_id
   AND sp.lead_or_collaborator = 'lead'

LEFT JOIN (
    SELECT 
        nct_id,
        MIN(intervention_type) AS intervention_type
    FROM ctgov.interventions
    GROUP BY nct_id
) i
    ON s.nct_id = i.nct_id

LEFT JOIN (
    SELECT 
        nct_id,
        COUNT(DISTINCT name) AS num_countries
    FROM ctgov.countries
    GROUP BY nct_id
) c
    ON s.nct_id = c.nct_id

LEFT JOIN (
    SELECT 
        nct_id,
        COUNT(*) AS num_outcomes
    FROM ctgov.outcomes
    GROUP BY nct_id
) oc
    ON s.nct_id = oc.nct_id

LEFT JOIN ctgov.eligibilities e
    ON s.nct_id = e.nct_id

LEFT JOIN ctgov.responsible_parties rp
    ON s.nct_id = rp.nct_id

WHERE
    s.study_type ILIKE '%Interventional%'
    AND s.primary_completion_date IS NOT NULL
    AND s.results_first_submitted_date IS NOT NULL;