drop procedure if exists deleteRunQueue
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure deleteRunQueue @runQueueId uniqueidentifier
as
begin
	declare @rowCount int
	set nocount on;

	delete from dbo.RunQueue where runQueueId = @runQueueId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

