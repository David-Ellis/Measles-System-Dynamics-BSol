-- Measles (B05)

WITH MeaslesAdmissions AS (
	SELECT 
		NHSNumber,
		ROW_NUMBER() OVER (PARTITION BY NHSNumber ORDER BY AdmissionDate) AS MeaslesAdmissionNum,
		AdmissionDate,
		AgeOnAdmission,
		SpellDuration,
		Cost,
		GenderCode,
		CASE
			WHEN E.Ethnic_Description_National = 'British' THEN 'White British'
			WHEN E.Ethnic_Description_National = 'Irish' THEN 'White Irish'
			WHEN E.Ethnic_Description_National = 'African' THEN 'Black African'
			WHEN E.Ethnic_Description_National = 'Caribbean' THEN 'Black Caribbean'
			WHEN E.Ethnic_Description_National IS NULL THEN
				CASE 
					WHEN EthnicCategoryCode = 'A' THEN 'White British'
					WHEN EthnicCategoryCode = 'B' THEN 'White Irish'
					WHEN EthnicCategoryCode = 'C' THEN 'Any other white background'
					WHEN EthnicCategoryCode = 'D' THEN 'White and Black Caribbean'
					WHEN EthnicCategoryCode = 'E' THEN 'White and Black African'
					WHEN EthnicCategoryCode = 'F' THEN 'White and Asian'
					WHEN EthnicCategoryCode = 'G' THEN 'Any other mixed background'
					WHEN EthnicCategoryCode = 'H' THEN 'Indian'
					WHEN EthnicCategoryCode = 'J' THEN 'Pakistani'
					WHEN EthnicCategoryCode = 'K' THEN 'Bangladeshi'
					WHEN EthnicCategoryCode = 'L' THEN 'Any other Asian background'
					WHEN EthnicCategoryCode = 'M' THEN 'Black Caribbean'
					WHEN EthnicCategoryCode = 'N' THEN 'Black African'
					WHEN EthnicCategoryCode = 'P' THEN 'Any other Black background'
					WHEN EthnicCategoryCode = 'R' THEN 'Chinese'
					WHEN EthnicCategoryCode = 'R' THEN 'Any other ethnic group'
					ELSE 'Unknown'
				END
			ELSE E.Ethnic_Description_National
		END AS Ethnicity
	FROM 
		[EAT_Reporting_BSOL].[SUS].[VwInpatientEpisodesDiagnosisRelational] AS A
	LEFT JOIN 
		[EAT_Reporting_BSOL].[SUS].[VwInpatientEpisodes] AS B
		ON A.[EpisodeId] = B.[EpisodeId]
	LEFT JOIN [EAT_Reporting_BSOL].[Demographic].Ethnicity AS E
		ON [NHSNumber] = E.[Pseudo_NHS_Number]
	WHERE 
		A.[DiagnosisCode] LIKE 'B05%'
		AND NHSNumber IS NOT NULL
		AND IsDominant = 1
	),
BSolStatuses AS (
	SELECT DISTINCT
		Pseudo_NHS_Number,
		LAD_2022_Desc AS LocalAuthority
	FROM [EAT_Reporting_BSOL].[Demographic].BSOL_Resident_Population
)

SELECT 
	AdmissionDate,
	MeaslesAdmissionNum,
	AgeOnAdmission,
	SpellDuration,
	Cost,
	GenderCode,
	Ethnicity,
	LocalAuthority
FROM
	MeaslesAdmissions
LEFT JOIN 
	BSolStatuses
ON [NHSNumber] = [Pseudo_NHS_Number]
WHERE LocalAuthority in ('Birmingham', 'Solihull')
ORDER BY AdmissionDate
