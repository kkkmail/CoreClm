md .\!All
del .\!All\001_all.sql
del .\!All\002_data.sql

copy /b .\tables\*.sql + .\procedures\*.sql + .\data\*.sql .\!All\all.sql
