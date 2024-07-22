if not exists(select schema_name from information_schema.schemata where schema_name = 'eeInf') begin
	print 'Creating schema eeInf...'
	exec sp_executesql N'create schema eeInf'
end else begin
	print 'Schema eeInf already exists...'
end
go

