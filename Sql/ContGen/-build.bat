md .\!All
del .\!All\all.sql
del .\!All\001_all.sql
del .\!All\002_data.sql

copy /b .\tables\*.sql + .\functions\*.sql + ..\MsgSvc\tables\*.sql .\!All\001_all.sql
copy /b .\data\*.sql + ..\MsgSvc\data\*.sql .\!All\002_data.sql
