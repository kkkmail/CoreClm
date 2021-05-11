md .\!All
del .\!All\all.sql
del .\!All\001_all.sql
del .\!All\002_data.sql

copy /b .\tables\*.sql + ..\MsgSvc\tables\*.sql + .\data\*.sql + ..\MsgSvc\data\*.sql .\!All\all.sql
