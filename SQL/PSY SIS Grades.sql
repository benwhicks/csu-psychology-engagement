SELECT		*
FROM		CustomSource.SISCourseGrade 
WHERE       (CustomSource.SISCourseGrade.SISCourseKey LIKE '%PSY203_201860%' OR
			CustomSource.SISCourseGrade.SISCourseKey LIKE '%PSY208_201860%' OR
			CustomSource.SISCourseGrade.SISCourseKey LIKE '%PSY453_201860%' OR
			CustomSource.SISCourseGrade.SISCourseKey LIKE '%PSY458_201860%')