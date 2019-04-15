SELECT * 
FROM activity_accumulator aa inner join course_main cm on aa.course_pk1 = cm.pk1
left join course_users cu on cm.pk1 = cu.crsmain_pk1 and aa.user_pk1 = 
left join users u on u.pk1 = cu.users_pk1 and aa.user_pk1 = u.pk1
where		(aa.COURSE_PK1 LIKE '%PSY203_201860%' OR
			aa.COURSE_PK1 LIKE '%PSY208_201860%' OR
			aa.COURSE_PK1 LIKE '%PSY453_201860%' OR
			aa.COURSE_PK1 LIKE '%PSY458_201860%')

			-- This query returned no results. Probably needs more joins, to like, something. 