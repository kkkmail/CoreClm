;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'Suspended', 0, NULL, NULL, NULL)

		) as a (settingId, settingName, settingBool, settingGuid, settingLong, settingText)
	)
insert into Setting
select valTbl.*
from valTbl
left outer join Setting on valTbl.settingId = Setting.settingId
where Setting.settingId is null
go


