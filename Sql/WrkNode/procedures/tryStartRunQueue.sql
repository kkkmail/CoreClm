drop procedure if exists dbo.tryStartRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

create procedure dbo.tryStartRunQueue (@runQueueId uniqueidentifier, @processId int)
as
begin
	declare @rowCount int
	set nocount on;

	update dbo.RunQueue
	set
		processId = @processId,
		runQueueStatusId = dbo.RunQueueStatus_InProgress(),
		startedOn = (getdate()),
		modifiedOn = (getdate())
	where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_NotStarted(), dbo.RunQueueStatus_InProgress())


	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

