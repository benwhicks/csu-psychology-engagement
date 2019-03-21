SELECT * 
FROM Stage.ACTIVITY_ACCUMULATOR_archive
where		(Stage.ACTIVITY_ACCUMULATOR_archive.COURSE_PK1 LIKE '%PSY203_201860%' OR
			Stage.ACTIVITY_ACCUMULATOR_archive.COURSE_PK1 LIKE '%PSY208_201860%' OR
			Stage.ACTIVITY_ACCUMULATOR_archive.COURSE_PK1 LIKE '%PSY453_201860%' OR
			Stage.ACTIVITY_ACCUMULATOR_archive.COURSE_PK1 LIKE '%PSY458_201860%')

			-- This query returned no results. Probably needs more joins, to like, something. 