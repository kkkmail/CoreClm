-- TODO kk:20200212 - Reference the source of the original code.
declare @table nvarchar(128)
declare @idcol nvarchar(128)
declare @sql nvarchar(max)
declare @crlf nvarchar(max)
declare @tab nvarchar(max)
declare @extra nvarchar(max)
declare @join nvarchar(max)

set @crlf = CHAR(13) + CHAR(10)
set @tab = '    '

-- Initialize those two values.
set @table = 'ModelData'
set @idcol = 'modelDataId'

-- and optional extra select adn join table
set @extra = 't.clmDefaultValueId, t.numberOfAminoAcids, t.maxPeptideLength'
set @join = 'clmTask t on m.clmTaskId = t.clmTaskId'

set @sql = 'select ' +  @crlf + @tab + @idcol + @crlf + @tab + ',(0' + @crlf

select @sql = @sql + @tab + @tab + ' + cast(isnull(datalength(m.' + name + '), 1) / 1048576.0 as money)' + @crlf 
        from  sys.columns 
        where object_id = object_id(@table)
        and   is_computed = 0
set @sql = @sql + @tab + ') as rowSizeMB' + @crlf + isnull(@tab + ',' + @extra + @crlf, '') + 'from ' + @table + isnull(' m inner join ' + @join, '') + @crlf + 'order by rowSizeMB desc'

PRINT @sql

exec (@sql)

