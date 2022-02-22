drop procedure if exists tryUpdateProgressRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure tryUpdateProgressRunQueue (
						@runQueueId uniqueidentifier,
						@progress decimal(18, 14),
						@callCount bigint,
						@yRelative float,
						@maxEe float,
						@maxAverageEe float,
						@maxWeightedAverageAbsEe float,
						@maxLastEe float)
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.RunQueue
    set
        progress = @progress,
        callCount = @callCount,
        yRelative = @yRelative,
        maxEe = @maxEe,
        maxAverageEe = @maxAverageEe,
        maxWeightedAverageAbsEe = @maxWeightedAverageAbsEe,
        maxLastEe = @maxLastEe,
        modifiedOn = (getdate())
    where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

