	--
	/*	
	SCRIPT PARA INVESTIGAR QUAIS TABELAS DE UM DETERMINADO BANCO ATENDEM OS CRIT�RIOS DEFINIDOS	
	*/
	--[][]
------------------------------------------------------------------------------------------

	/*	DEFINIR SERVIDOR/BANCO. � PRECISO LIGAR O SQLCMD MODE */

	:SETVAR SERVER SQL33\ATUARIAL2
	:SETVAR DATABASE FIPSUSEP
	--:SETVAR DATABASE TDDB05_CDC

	--SELECT TABLE_NAME
	--FROM [$(SERVER)].[$(DATABASE)].INFORMATION_SCHEMA.TABLES
	--WHERE TABLE_NAME LIKE '%res%'

	SELECT TABLE_NAME, COLUMN_NAME
	FROM [$(SERVER)].[$(DATABASE)].INFORMATION_SCHEMA.COLUMNS
	WHERE COLUMN_NAME LIKE '%ENTCODIGO%'
	ORDER BY 2,1

------------------------------------------------------------------------------------------
	--
	/*	DEFINIR CRITERIOS DE INVESTIGACAO	*/
	--
	--DECLARE @ENTCODIGO	VARCHAR(MAX) = '05631'
	DECLARE @CAMPO		VARCHAR(MAX) = '%ENTCODIGO%'
	DECLARE @OPERA		VARCHAR(MAX) = '='			/* USAR LIKE PARA TEXTO OU IGUALDADE PRA NUMERICO */
	DECLARE @VALOR		VARCHAR(MAX) = '05631'
	--
------------------------------------------------------------------------------------------
	--
	DECLARE @FORMATO	VARCHAR(MAX) = CASE WHEN @OPERA = 'LIKE' THEN 'C' ELSE 'N' END
	DECLARE @TABELA		VARCHAR(MAX)
	DECLARE @COLUNA		VARCHAR(MAX)
	DECLARE @TIPO		VARCHAR(MAX)
	DECLARE @QUERY		NVARCHAR(MAX)
	DECLARE @RESULT		TABLE (
		 TABELA		NVARCHAR(MAX)
		,COLUNA		NVARCHAR(MAX)
		,TIPO		NVARCHAR(MAX)
		,OPERADOR	NVARCHAR(MAX)
		,VALOR		NVARCHAR(MAX)
		,LINHAS		FLOAT
	)
	--
	DECLARE C1 CURSOR FOR ( 												
		SELECT 
		 TABLE_NAME	=	A.TABLE_NAME
		,COLUMN_NAME
		,DATA_TYPE	= UPPER(DATA_TYPE)
		,OPERADOR	= @OPERA
		,VALOR		= @VALOR
		FROM [$(SERVER)].[$(DATABASE)].INFORMATION_SCHEMA.COLUMNS A
		LEFT JOIN [$(SERVER)].[$(DATABASE)].INFORMATION_SCHEMA.TABLES B
		 ON A.TABLE_CATALOG = B.TABLE_CATALOG
		AND A.TABLE_NAME = B.TABLE_NAME
		WHERE B.TABLE_TYPE = 'BASE TABLE'
		AND COLUMN_NAME LIKE @CAMPO
		AND CASE WHEN @FORMATO = 'C' THEN DATA_TYPE ELSE 'CHAR' END IN
			('CHAR','VARCHAR','NVARCHAR')
		AND CASE WHEN @FORMATO = 'N' THEN DATA_TYPE ELSE 'INT' END IN 
			('DECIMAL','INT','SMALLINT','BIGINT','FLOAT')
		--AND A.TABLE_NAME NOT IN ('SOGLOBALPE','T1')
	)
	--
	OPEN C1 
	FETCH NEXT FROM C1 INTO @TABELA, @COLUNA, @TIPO, @OPERA, @VALOR
	WHILE @@FETCH_STATUS <> -1
	BEGIN
		--
		SET @QUERY = 
		' SELECT '  +											CHAR(10)
		+ ' TAB	= ' + CHAR(39) + @TABELA +	CHAR(39) +			CHAR(10)
		+ ',COL	= ' + CHAR(39) + @COLUNA +	CHAR(39) +			CHAR(10)
		+ ',TIP	= ' + CHAR(39) + @TIPO +	CHAR(39) +			CHAR(10)
		+ ',OPE	= ' + CHAR(39) + @OPERA +	CHAR(39) +			CHAR(10)
		+ ',VLR	= ' + CHAR(39) + @VALOR +	CHAR(39) +			CHAR(10)
		+ ',LIN	= COUNT(*)'+									CHAR(10)
		+ 'FROM [$(SERVER)].[$(DATABASE)].[DBO].'+ @TABELA +	CHAR(10)
		+ 'WHERE 1=1 '										+	CHAR(10)
		--+ 'AND ENTCODIGO = '+CHAR(39)+@ENTCODIGO+CHAR(39)	+	CHAR(10)
		+ 'AND '+ @COLUNA + ' ' + @OPERA + ' '
		+ CASE WHEN @TIPO LIKE '%CHAR%'	THEN CHAR(39) ELSE '' END
		+ CASE WHEN @OPERA = 'LIKE'		THEN CHAR(37) ELSE '' END
		+ @VALOR
		+ CASE WHEN @OPERA = 'LIKE'		THEN CHAR(37) ELSE '' END
		+ CASE WHEN @TIPO LIKE '%CHAR%'	THEN CHAR(39) ELSE '' END
		--
		INSERT INTO @RESULT (TABELA, COLUNA, TIPO, OPERADOR, VALOR, LINHAS)
		EXEC(@QUERY)
		--
		FETCH NEXT FROM C1 INTO @TABELA, @COLUNA, @TIPO, @OPERA, @VALOR
		--
	END
	CLOSE C1	
	DEALLOCATE C1
	--
	SELECT * FROM @RESULT
	WHERE LINHAS != 0
	ORDER BY 1,2
	--
------------------------------------------------------------------------------------------