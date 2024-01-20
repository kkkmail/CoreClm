drop function if exists clm.getGroupId
go

create function clm.getGroupId(@defaultValueId bigint)
returns bigint
as
begin
	return (@defaultValueId / 1000000000)
end
go
