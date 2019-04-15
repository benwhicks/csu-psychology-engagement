select *
from course_contents cc inner join course_main cm on cc.crsmain_pk1 = cm.pk1
where --cm.course_id like 'S-PSY203_201860%' or cm.course_id like 'S-PSY453_201860%'
--cm.course_id like 'S-PSY208_201760%' or cm.course_id like 'S-PSY458_201760%'
cm.course_id like 'S-OCC104_201930_PT_I'