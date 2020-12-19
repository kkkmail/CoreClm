drop function if exists dbo.getGroupId
go

create function dbo.getGroupId(@clmDefaultValueId bigint)
returns bigint
as
begin
	return (@clmDefaultValueId / 1000000000)
end
go
