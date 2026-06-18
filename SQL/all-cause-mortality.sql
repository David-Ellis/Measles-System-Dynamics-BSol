/*=============================================================
    Calculate age-specific mortality rates for Birmingham and
    Solihull residents (2022-2025).

	AgeGroup | 	Population2026	AverageAnnualDeaths	MortalityRate
	-------------------------------------------------------------
	Under 1	 |	6184			58.75				0.00950
	1-4 yrs	 |	68829			68.75				0.00010
	5+ yrs	 |	1559430			11273.50			0.00723

=============================================================*/
DECLARE @StartYear INT = 2022;
DECLARE @EndYear INT = 2025;

WITH deaths_by_age_group AS (
    -- Assign age groups to registered deaths
    SELECT
        CASE
            WHEN DEC_AGEC = 0 THEN 'Under 1'
            WHEN DEC_AGEC <= 4 THEN '1-4 yrs'
            ELSE '5+ yrs'
        END AS AgeGroup
    FROM EAT_Reporting_BSOL.Other.VwDeathsRegister
    WHERE
        ULA_OF_RESIDENCE_CODE IN ('E08000025', 'E08000029')
        AND YEAR(REG_DATE) BETWEEN  @StartYear AND @EndYear
),

average_annual_deaths AS (
    -- Calculate mean annual deaths over 2020–2025
    SELECT
        AgeGroup,
        COUNT(*) * 1.0 / (@EndYear - @StartYear + 1) AS AverageAnnualDeaths
    FROM deaths_by_age_group
    GROUP BY AgeGroup
),

population_by_age_group AS (
    -- Assign age groups to the current registered population
    SELECT
        CASE
            WHEN ProxyAgeAtEOM = 0 THEN 'Under 1'
            WHEN ProxyAgeAtEOM <= 4 THEN '1-4 yrs'
            ELSE '5+ yrs'
        END AS AgeGroup
    FROM EAT_Reporting_BSOL.Demographic.BSOL_Registered_Population
),

population_counts AS (
    -- Count population within each age group
    SELECT
        AgeGroup,
        COUNT(*) AS Population2026
    FROM population_by_age_group
    GROUP BY AgeGroup
)

-- Calculate age-specific mortality rates
SELECT
    p.AgeGroup,
    p.Population2026,
    d.AverageAnnualDeaths,
    d.AverageAnnualDeaths * 1.0 / p.Population2026 AS MortalityRate
FROM population_counts AS p
LEFT JOIN average_annual_deaths AS d
    ON p.AgeGroup = d.AgeGroup;

