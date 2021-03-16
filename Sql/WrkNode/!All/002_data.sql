;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'NoChartGeneration')
			, (1, 'RegularChartGeneration')
			, (2, 'ForceChartGeneration')
		) as a (notificationTypeId, notificationTypeName)
	)
insert into NotificationType
select valTbl.*
from valTbl
left outer join NotificationType on valTbl.notificationTypeId = NotificationType.notificationTypeId
where NotificationType.notificationTypeId is null
go


;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'NotStarted')
			, (1, 'Inactive')
			, (2, 'InProgress')
			, (3, 'Completed')
			, (4, 'Failed')
			, (5, 'CancelRequested')
			, (6, 'Cancelled')

		) as a (runQueueStatusId, runQueueStatusName)
	)
insert into RunQueueStatus
select valTbl.*
from valTbl
left outer join RunQueueStatus on valTbl.runQueueStatusId = RunQueueStatus.runQueueStatusId
where RunQueueStatus.runQueueStatusId is null
go


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


;with 
	valTbl as
	(
		select * 
		from 
		( values
			  (0, 'GuaranteedDelivery')
			, (1, 'NonGuaranteedDelivery')

		) as a (deliveryTypeId, deliveryTypeName)
	)
insert into DeliveryType
select valTbl.*
from valTbl
left outer join DeliveryType on valTbl.deliveryTypeId = DeliveryType.deliveryTypeId
where DeliveryType.deliveryTypeId is null
go


