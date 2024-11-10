drop procedure if exists dbo.tryFailRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.tryFailRunQueue (@runQueueId uniqueidentifier, @errorMessage nvarchar(max))
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.RunQueue
    set
        runQueueStatusId = dbo.RunQueueStatus_Failed(),
        processId = null,
        modifiedOn = (getdate()),
        errorMessage = @errorMessage
    where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

