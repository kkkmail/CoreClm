drop procedure if exists dbo.tryResetRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.tryResetRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	update dbo.RunQueue
	set
		runQueueStatusId = 0,
		errorMessage = null,
		workerNodeId = null,
		startedOn = null,
		modifiedOn = getdate()
	where runQueueId = @runQueueId and runQueueStatusId = 4

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

