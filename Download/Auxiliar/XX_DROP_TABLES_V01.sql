
USE WORK_ALISSON
GO

DECLARE @NAME VARCHAR(1000) = 'CONTROLE_%'
SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME LIKE ''+@NAME+'%'

DECLARE @TABLE VARCHAR(1000)
DECLARE C1 CURSOR FOR 
--SELECT TABLE_NAME FROM @TABLE_LIST
SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME LIKE '%'+@NAME+'%'
OPEN C1 
FETCH NEXT FROM C1 INTO @TABLE
WHILE @@FETCH_STATUS <> -1
BEGIN
	EXEC ('DROP TABLE IF EXISTS '+@TABLE)
	FETCH NEXT FROM C1 INTO @TABLE
END	
CLOSE C1
DEALLOCATE C1

SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME LIKE ''+@NAME+'%'