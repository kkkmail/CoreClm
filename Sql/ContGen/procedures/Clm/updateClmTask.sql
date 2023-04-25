drop procedure if exists clm.updateTask
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure clm.updateTask
		@taskId uniqueidentifier,
		@remainingRepetitions int
as
begin
	declare @rowCount int
	set nocount on;

    update clm.ClmTask
    set remainingRepetitions = @remainingRepetitions
    where taskId = @taskId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

