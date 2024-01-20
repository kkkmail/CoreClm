md .\!All
del .\!All\all.sql
del .\!All\001_all.sql
del .\!All\002_data.sql

copy /b *.sql + .\tables\*.sql + .\tables\Clm\*.sql + .\tables\EeInf\*.sql + .\functions\*.sql + .\procedures\*.sql + .\procedures\Clm\*.sql + .\procedures\EeInf\*.sql + ..\MsgSvc\tables\*.sql + ..\MsgSvc\procedures\*.sql + .\data\*.sql + ..\MsgSvc\data\*.sql .\!All\all.sql
