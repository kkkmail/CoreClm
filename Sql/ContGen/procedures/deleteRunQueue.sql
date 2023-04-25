drop procedure if exists dbo.deleteRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.deleteRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	delete from clm.RunQueue where runQueueId = @runQueueId
	delete from eeInf.RunQueue where runQueueId = @runQueueId
	delete from dbo.RunQueue where runQueueId = @runQueueId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

