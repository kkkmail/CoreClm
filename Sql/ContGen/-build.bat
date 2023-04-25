md .\!All
del .\!All\all.sql
del .\!All\001_all.sql
del .\!All\002_data.sql

copy /b *.sql + .\tables\*.sql + .\tables\Clm\*.sql + .\tables\EeInf\*.sql + .\functions\*.sql + .\functions\Clm\*.sql + .\functions\EeInf\*.sql + .\procedures\*.sql + .\procedures\Clm\*.sql + .\procedures\EeInf\*.sql + ..\MsgSvc\tables\*.sql + ..\MsgSvc\procedures\*.sql + .\data\*.sql + .\data\Clm\*.sql + .\data\EeInf\*.sql + ..\MsgSvc\data\*.sql .\!All\all.sql
