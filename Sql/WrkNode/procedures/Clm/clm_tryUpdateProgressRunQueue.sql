drop procedure if exists clm.tryUpdateProgressRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure clm.tryUpdateProgressRunQueue (
						@runQueueId uniqueidentifier,
						@progress decimal(18, 14),
						@callCount bigint,
						@relativeInvariant float,
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
        relativeInvariant = @relativeInvariant,
--        maxEe = @maxEe,
--        maxAverageEe = @maxAverageEe,
--        maxWeightedAverageAbsEe = @maxWeightedAverageAbsEe,
--        maxLastEe = @maxLastEe,
        modifiedOn = (getdate())
    where runQueueId = @runQueueId and runQueueStatusId in (dbo.RunQueueStatus_InProgress(), dbo.RunQueueStatus_CancelRequested())

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

