 
-- Parámetros de entrada: @isbn VARCHAR(17), @legajo_alumno INT NULL, @legajo_docente INT NULL

CREATE OR ALTER PROCEDURE SP_Prestar_Libro(
    @isbn VARCHAR(17),
    @legajo_alumno INT = NULL,
    @legajo_docente INT = NULL)
AS
BEGIN
    IF NOT (
        (@legajo_alumno IS NOT NULL AND @legajo_docente IS NULL) OR
        (@legajo_alumno IS NULL AND @legajo_docente IS NOT NULL)
    )
    BEGIN
        RAISERROR('Debe enviar legajo_alumno O legajo_docente, pero no ambos ni ninguno.', 16, 1);
        RETURN;
    END;

    BEGIN TRY
        BEGIN TRAN;
        --Valida existencia de isbn ingresado
        IF NOT EXISTS (SELECT 1 FROM Libro WHERE isbn = @isbn)
        BEGIN
            RAISERROR('El ISBN indicado no existe en la tabla Libro.', 16, 1);
            ROLLBACK TRAN;
            RETURN;
        END;
        --Valida disponibilidad de ese isbn
        IF NOT EXISTS (SELECT 1 FROM Libro WHERE isbn = @isbn AND disponibles > 0)
        BEGIN
            RAISERROR('No hay ejemplares disponibles para este isbn', 16, 1);
            ROLLBACK TRAN;
            RETURN;
        END;
        --Valida que el alumno o el docente existan
        IF @legajo_alumno IS NOT NULL
        BEGIN
            IF NOT EXISTS (SELECT 1 FROM Alumno WHERE legajo = @legajo_alumno)
            BEGIN
                RAISERROR('El legajo alumno indicado no existe', 16, 1);
                ROLLBACK TRAN;
                RETURN;
            END;
        END
        ELSE
        BEGIN
            IF NOT EXISTS (SELECT 1 FROM Docente WHERE legajo = @legajo_docente)
            BEGIN
                RAISERROR('El legajo docente indicado no existe', 16, 1);
                ROLLBACK TRAN;
                RETURN;
            END;
        END

        --Inserta registro en Prestamos_Libros con la información indicada por parámetro y fecha de hoy
        INSERT INTO Prestamos_Libros (isbn, legajo_alumno, legajo_docente, fecha_prestamo)
        VALUES (@isbn, @legajo_alumno, @legajo_docente, CAST(GETDATE() AS DATE));

        --Resta uno a la cantidad de ejemplares disponibles para ese isbn
        UPDATE Libro
        SET disponibles = disponibles - 1
        WHERE isbn = @isbn;

        COMMIT TRAN;
    END TRY

    BEGIN CATCH
        IF @@TRANCOUNT > 0
            ROLLBACK TRAN;
        DECLARE @msg NVARCHAR(4000) = ERROR_MESSAGE();
        RAISERROR('Error interno: %s', 16, 1, @msg);
    END CATCH
END;
GO


CREATE OR ALTER FUNCTION UDF_NotaCursada
(
    @legajo INT,
    @codigo_curso INT
)
RETURNS DECIMAL(5,2)
AS
BEGIN
    DECLARE @nota_reg DECIMAL(5,2);

--Verificar si para cada parcial desaprobado hay un recuperatorio aprobado, caso contrario devuelve NULL
    IF EXISTS (
        SELECT 1
        FROM Examen par
        WHERE par.legajo_alumno = @legajo
          AND par.codigo_curso = @codigo_curso
          AND par.tipo = 'PAR'
          AND par.nota < 4
          AND NOT EXISTS (
                SELECT 1
                FROM Examen rec
                WHERE rec.legajo_alumno = par.legajo_alumno
                  AND rec.codigo_curso = par.codigo_curso
                  AND rec.tipo = 'REC'
                  AND rec.nota >= 4
          )
    )
        RETURN NULL; 

--Toma notas parciales y de recuperatorios >= 4 para calcular el promedio de la cursada
    SELECT @nota_reg = AVG(CAST(nota AS DECIMAL(5,2)))
    FROM Examen
    WHERE legajo_alumno = @legajo
      AND codigo_curso = @codigo_curso
      AND (
            (tipo = 'PAR' AND nota >= 4) OR
            (tipo = 'REC' AND nota >= 4)
          );

    RETURN @nota_reg;
END;
GO


CREATE TRIGGER TR_Cursada_Desaprobada
ON Examen
AFTER INSERT
AS
BEGIN
    --Verifica que ya haya por lo menos un examen parcial para esta cursada
    IF NOT EXISTS (SELECT 1 FROM inserted WHERE tipo = 'PAR')
        RETURN;
    --Si encuentra más de un parcial desaprobado (nota < 4) considera a la Cursada como desaprobada (DES)
    UPDATE C
    SET C.condicion = 'DES'
    FROM Cursada C
    INNER JOIN inserted I 
        ON C.legajo_alumno = I.legajo_alumno
       AND C.codigo_curso = I.codigo_curso
    WHERE I.tipo = 'PAR'
      AND (
            SELECT COUNT(*)
            FROM Examen E
            WHERE E.legajo_alumno = I.legajo_alumno
              AND E.codigo_curso = I.codigo_curso
              AND E.tipo = 'PAR'
              AND E.nota < 4
          ) > 1;
END;
GO


CREATE OR ALTER TRIGGER TR_Limite_Finales
ON Examen
INSTEAD OF INSERT
AS
BEGIN
--Valida, en las inserciones de examenes finales, que no se puedan tener más de 3 para una cursada
    IF EXISTS (
        SELECT 1
        FROM inserted I
        WHERE I.tipo = 'FIN'
        AND (
            SELECT COUNT(*)
            FROM Examen E
            WHERE E.legajo_alumno = I.legajo_alumno
              AND E.codigo_curso = I.codigo_curso
              AND E.tipo = 'FIN'
        ) >= 3
    )
    BEGIN
        RAISERROR('El alumno ya alcanzó el máximo de 3 finales permitidos.', 16, 1);
        RETURN;
    END;

    INSERT INTO Examen (legajo_alumno, codigo_curso, tipo, nota, fecha)
    SELECT legajo_alumno, codigo_curso, tipo, nota, fecha
    FROM inserted;

END;
GO


