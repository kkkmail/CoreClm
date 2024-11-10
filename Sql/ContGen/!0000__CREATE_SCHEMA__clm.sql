if not exists(select schema_name from information_schema.schemata where schema_name = 'clm') begin
	print 'Creating schema clm...'
	exec sp_executesql N'create schema clm'
end else begin
	print 'Schema clm already exists...'
end
go

