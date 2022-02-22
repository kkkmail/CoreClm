drop procedure if exists saveMessage
go


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO


create procedure saveMessage (
					@messageId uniqueidentifier,
					@senderId uniqueidentifier,
					@recipientId uniqueidentifier,
					@dataVersion int,
					@deliveryTypeId int,
					@messageData varbinary(max))
as
begin
	declare @rowCount int
	set nocount on;

	insert into Message (messageId, senderId, recipientId, dataVersion, deliveryTypeId, messageData, createdOn)
	select @messageId, @senderId, @recipientId, @dataVersion, @deliveryTypeId, @messageData, getdate()
	where not exists (select 1 from Message where messageId = @messageId)

	set @rowCount = @@rowcount
	select @rowCount as [RowCount]
end
go

