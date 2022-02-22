drop procedure if exists updateClmTask
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure updateClmTask
		@clmTaskId uniqueidentifier,
		@remainingRepetitions int
as
begin
	declare @rowCount int
	set nocount on;

    update dbo.ClmTask
    set remainingRepetitions = @remainingRepetitions
    where clmTaskId = @clmTaskId

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

