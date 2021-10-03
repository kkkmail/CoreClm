drop procedure if exists tryResetRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryResetRunQueue @runQueueId uniqueidentifier
as
begin
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	set nocount on;

	update dbo.RunQueue
	set
		runQueueStatusId = 0,
		errorMessage = null,
		workerNodeId = null,
		startedOn = null,
		modifiedOn = getdate()
	where runQueueId = @runQueueId and runQueueStatusId = 4

	select @@rowcount as [RowCount]
end
go


