md .\!All
del .\!All\all.sql
del .\!All\001_all.sql
del .\!All\002_data.sql

copy /b .\tables\*.sql + .\functions\*.sql + .\procedures\*.sql + ..\MsgSvc\tables\*.sql + ..\MsgSvc\procedures\*.sql + .\data\*.sql + ..\MsgSvc\data\*.sql .\!All\all.sql
