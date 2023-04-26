drop procedure if exists dbo.clm_updateTask
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure dbo.clm_updateTask
		@taskId uniqueidentifier,
		@remainingRepetitions int
as
begin
	declare @rowCount int
	set nocount on;

    update clm.Task
    set remainingRepetitions = @remainingRepetitions
    where taskId = @taskId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

