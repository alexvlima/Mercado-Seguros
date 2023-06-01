
USE SUSEP_SES
Go

--SELECT T.*, S.CREATE_DATE, S.OBJECT_ID
--FROM INFORMATION_SCHEMA.TABLES T
--LEFT JOIN SYS.TABLES S ON T.TABLE_NAME= S.NAME
--WHERE 1=1
--AND TABLE_NAME LIKE 'SES_%'
--ORDER BY CREATE_DATE DESC
 
SELECT 
	TABLE_NAME	=	X.TABLE_NAME
,	COLUMN_NAME
,	ORDINAL_POSITION
,	DATA_TYPE
,	CHARACTER_MAXIMUM_LENGTH
,	COLLATION_NAME
FROM INFORMATION_SCHEMA.COLUMNS X
LEFT JOIN CONTROLE_CARGA C ON X.TABLE_NAME = C.TABLE_NAME
WHERE 1=1
AND X.TABLE_NAME LIKE 'SES_%'
--AND COLUMN_NAME LIKE '%CMPID%'
ORDER BY TABLE_NAME, ORDINAL_POSITION