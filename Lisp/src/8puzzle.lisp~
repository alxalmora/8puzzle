;; Resolver 8-puzzle utilizando una heuristica de Manhattan
;; EL INDEX DE LA LISTA INICIA EN 0
;;Nodo::Struct (id::number, Estado::list, nivel::number, manhattan(estado),operacion::symbol, padre::number
(SETQ META '(1 2 3 4 5 6 7 8 0))
(SETQ
 OPENSTACK '()
 MOVSOL '()
 CLOSEDSTACK '()
 IDNODO 1
)
(LOAD "/Users/alxalmora/Documents/8puzzle/Java/InFile.lisp")
(DEFUN ELEMENT_AT (INDEX ESTADO)
;;Regresa el elemento en la posicion index (0 a Length - 1)
(RETURN-FROM ELEMENT_AT (NTH INDEX ESTADO))
)
(DEFUN FIRSTN ( N LST)
;;Regresa los primeros N elementos de la lista
  (RETURN-FROM FIRSTN (REVERSE (NTHCDR (- (LENGTH LST) N) (REVERSE LST))))
)

(DEFUN MANHATTAN (ESTADO)
;;Funcion Heuristica que permite contar la distancia
  (SETQ DISTANCIA 0)
  (DOTIMES (N 8)
    (SETQ DISTANCIA
    (+
     (ABS (- (POSX (POSITION  (+ 1 N)  ESTADO)) (POSX N)))
     (ABS (- (POSY (POSITION  (+ 1 N) ESTADO)) (POSY N)))
     DISTANCIA
   )
    )
)
(RETURN-FROM MANHATTAN DISTANCIA)
)
(DEFUN POSX (INDEX)
 ;;Funcion que ayuda a convertir en posicion cartesiana discreta.
  (RETURN-FROM POSX  (MOD INDEX 3) )
)
(DEFUN POSY (INDEX)
  ;;Funcion que ayuda a convertir en posisicon cartesiana discreta
  (RETURN-FROM POSY  (FLOOR INDEX 3) )
)
(DEFUN SWAP (ESTADO INDEXA INDEXB)
  ;;Funcion para cambiar dos valores, regresa un estado.
  (SETQ NEWLIST (COPY-LIST ESTADO))
  (ROTATEF (NTH INDEXA NEWLIST) (NTH INDEXB NEWLIST))
  (RETURN-FROM SWAP NEWLIST)
)
(DEFUN GETZERO (ESTADO)
;;Funcion que regresa la posicion del 0 (Espacio libre)
  (RETURN-FROM GETZERO (POSITION 0 ESTADO))
)
(DEFUN UP (ESTADO)
 ;;REGRESA NIL SI ES INVALIDO, UN NODO NUEVO SI ES VALIDO.
 (COND
  ((EQUAL (POSY (POSITION 0 ESTADO)) 0)
   (RETURN-FROM UP '())
   )
  (T
   (RETURN-FROM UP (SWAP ESTADO (GETZERO ESTADO) (- (GETZERO ESTADO) 3)))
   )
  )
)
(DEFUN DOWN (ESTADO)
 ;;REGRESA NIL SI ES INVALIDO, UN NODO NUEVO SI ES VALIDO.
 (COND
  ((EQUAL (POSY (POSITION 0 ESTADO)) 2)
   (RETURN-FROM DOWN '())
   )
  (T
   (RETURN-FROM DOWN (SWAP ESTADO (GETZERO ESTADO) (+ (GETZERO ESTADO) 3)))
   )
  )
)
(DEFUN RIGHT (ESTADO)
 ;;REGRESA NIL SI ES INVALIDO, UN NODO NUEVO SI ES VALIDO.
 (COND
  ((EQUAL (POSX (POSITION 0 ESTADO)) 2)
   (RETURN-FROM RIGHT '())
   )
  (T
   (RETURN-FROM RIGHT (SWAP ESTADO (GETZERO ESTADO) (+ 1 (GETZERO ESTADO))))
   )
  )
)

(DEFUN LEFT (ESTADO)
 ;;REGRESA NIL SI ES INVALIDO, UN NODO NUEVO SI ES VALIDO.
 (COND
  ((EQUAL (POSX (POSITION 0 ESTADO)) 0)
   (RETURN-FROM LEFT '())
   )
  (T
   (RETURN-FROM LEFT (SWAP ESTADO (GETZERO ESTADO) (- (GETZERO ESTADO) 1)))
   )
  )
)
(DEFUN MAKEMOVE (ESTADO MOVIMIENTO)
  ;;Regresa un nodo despues de aplicar un movimiento.
  (COND
   ((EQUAL MOVIMIENTO 'U)
    (RETURN-FROM MAKEMOVE (UP ESTADO))
    )
   ((EQUAL MOVIMIENTO 'D)
    (RETURN-FROM MAKEMOVE (DOWN ESTADO))
    )
   ((EQUAL MOVIMIENTO 'L)
    (RETURN-FROM MAKEMOVE (LEFT ESTADO))
    )
   ((EQUAL MOVIMIENTO 'R)
    (RETURN-FROM MAKEMOVE (RIGHT ESTADO))
    )
   (T
    (WRITE 'ERROR)
    (RETURN-FROM MAKEMOVE '())
    )
   )
)

(DEFUN EXPAND (NODO)
  ;;Regresa la lista de nodos (Validos) que se generan a partir de alguno.
  (SETQ
   ARRIBA (MAKEMOVE (SECOND NODO) 'U)
   ABAJO (MAKEMOVE (SECOND NODO) 'D)
   DERECHA (MAKEMOVE (SECOND NODO) 'R)
   IZQUIERDA (MAKEMOVE (SECOND NODO) 'L)
   HIJOS NIL
   )
   (IF (NOT(EQUAL ARRIBA NIL))
    (PUSH (LIST  (SETQ IDNODO (+ 1 IDNODO)) ARRIBA (+ (THIRD NODO) 1) (MANHATTAN ARRIBA) 'U (FIRST NODO)) HIJOS)
    )
    (IF (NOT(EQUAL ABAJO NIL))
     (PUSH (LIST  (SETQ IDNODO (+ 1 IDNODO)) ABAJO (+ (THIRD NODO) 1) (MANHATTAN ABAJO) 'D (FIRST NODO)) HIJOS)
    )
   (IF (NOT(EQUAL DERECHA NIL))
    (PUSH (LIST  (SETQ IDNODO (+ 1 IDNODO)) DERECHA (+ (THIRD NODO) 1) (MANHATTAN DERECHA) 'R (FIRST NODO)) HIJOS)
    )
   (IF (NOT(EQUAL IZQUIERDA  NIL))
    (PUSH (LIST (SETQ IDNODO (+ 1 IDNODO)) IZQUIERDA (+ (THIRD NODO) 1) (MANHATTAN IZQUIERDA) 'L (FIRST NODO)) HIJOS)
    )
  (RETURN-FROM EXPAND HIJOS)
)

(DEFUN CALCULACOSTO (NODO)
;;Funcion que calcula el costo de un nodo, utilizando el nivel (#movimientos para llegar a ese estado) y la distancia manhattan (funcion heuristica).
 (RETURN-FROM CALCULACOSTO (+  (THIRD NODO) (FOURTH NODO)))
)
(DEFUN ADDOPEN (NODO)
  ;;Agrega a abierto y va ordenando por costos
  (SETQ
   INDEX 0
   COSTO (CALCULACOSTO NODO)
   )
  (LOOP
   (SETQ NTHNODO (NTH INDEX OPENSTACK))
   (COND
    ((NULL NTHNODO)
     (SETQ OPENSTACK (APPEND OPENSTACK (LIST NODO) ))
     (RETURN-FROM ADDOPEN T)
     )
    ((< COSTO (CALCULACOSTO NTHNODO))
     (SETQ OPENSTACK (APPEND (FIRSTN INDEX OPENSTACK) (LIST NODO) (NTHCDR (+ 1 INDEX) OPENSTACK)))
     (RETURN-FROM ADDOPEN T)
     )
    )
   (SETQ INDEX (+ 1 INDEX))
   )
)
(DEFUN ADDCLOSE (NODO)
  ;;Agrega a cerrado, funcion hecha para seguir los nombres de las funciones
  (PUSH NODO CLOSEDSTACK)
)

(DEFUN ISINCLOSEDSTACK (NODO)
;;Revisa si un nodo esta en cerrado, T -> Esta en cerrado, NIL -> No esta
  (SETQ INDEX 0)
  (LOOP
   (SETQ NTHNODO (NTH INDEX CLOSEDSTACK))
    ;;(PRINT NTHNODO)
   (COND
    ((NULL (SECOND NTHNODO))
     ;;(PRINT NTHNODO)
     (RETURN-FROM ISINCLOSEDSTACK NIL)
    )
    ((EQUAL (SECOND NODO) (SECOND NTHNODO))
     (COND
      ((< (CALCULACOSTO NODO) (CALCULACOSTO NTHNODO))
       (SETF
        (THIRD NTHNODO) (THIRD NODO)
        (FOURTH NTHNODO) (FOURTH NODO)
        (SIXTH NTHNODO) (SIXTH NODO)
        )
       )
      )
     (RETURN-FROM ISINCLOSEDSTACK T)
   )
  )
 (SETQ INDEX (+ 1 INDEX))
 )
)
(DEFUN BACKTRACK (NODO)
  ;;Funcion que busca la ruta que se sigue hasta encontrar el nodo solucion
  (SETQ
   IDFATHER (SIXTH NODO)
   INDEX 0
   BUSQOPTION (NTH INDEX CLOSEDSTACK)
   )
  (PUSH (SECOND NODO) MOVSOL)
  (LOOP
   (COND
    ((= 1 (FIRST BUSQOPTION))
     (PUSH (SECOND BUSQOPTION) MOVSOL)
     (RETURN-FROM BACKTRACK (REVERSE MOVSOL))
     )
    ((NULL BUSQOPTION)
     (PRINT 'ERROR)
     (RETURN-FROM BACKTRACK NIL)
     )
    ((NULL (FIRST BUSQOPTION))
     (PRINT "NULL FRIST")
     )
    ((= IDFATHER (FIRST BUSQOPTION))
     (SETQ IDFATHER (SIXTH BUSQOPTION))
     (PUSH (SECOND BUSQOPTION) MOVSOL)
     )
    )
   (SETQ INDEX (+ 1 INDEX))
   (SETQ BUSQOPTION (NTH INDEX CLOSEDSTACK))
   )
)
(DEFUN PRINT_PUZZLE (NODO)
;;Funcion auxiliar para debuggear
  (SETQ INDEX 0)
  (LOOP
       (COND
        ((= INDEX 9)
         (PRINT "----------")
         (TERPRI)
         (RETURN-FROM PRINT_PUZZLE T))
      ( (= 2 (MOD INDEX 3))
        (PRIN1(NTH INDEX NODO))
        (TERPRI)
      )
      ((NEQ 2 (MOD INDEX 3))
          (PRIN1(NTH INDEX NODO))
      ))
      (SETQ INDEX (+ INDEX 1))
  )
)
(DEFUN PATH()
  ;;Funcion que hace la busqueda
  (ADDOPEN (LIST IDNODO  NODO 0 (MANHATTAN NODO) NIL 0 ))
  (LOOP
    (IF (NULL OPENSTACK)
     (RETURN-FROM PATH NIL)
     )

   (SETQ CURRENT (POP OPENSTACK))
    ;;(PRINT CURRENT)
   (COND
     ((EQUAL (SECOND CURRENT) META)
      (PRINT "Movimientos")
     (PRINT (BACKTRACK CURRENT))
      (PRINT (LENGTH MOVSOL))
     (PRINT "FIN")
     (RETURN-FROM PATH T)
     )
    (T
     (ADDCLOSE CURRENT)
     (SETQ NEXTNODOS (EXPAND CURRENT))
     (LOOP FOR NEXT IN NEXTNODOS
      DO
        (IF (NOT (ISINCLOSEDSTACK NEXT))
           (ADDOPEN NEXT)
          )
       )
      )
     )
   )
)

;;(SETQ ESTADOS '(1 2 3 4 0 5 6 7 8))
(WRITE (PATH))
;;PRINT (GETINDEXOF 1 '(2 3 1 4)))
(SETQ MOVSOL (REVERSE MOVSOL))
(WITH-OPEN-FILE (STR "/Users/alxalmora/Documents/8puzzle/Java/OutFile.txt" 
                        :DIRECTION :OUTPUT 
                        :IF-EXISTS :SUPERSEDE
                        :IF-DOES-NOT-EXIST :CREATE)
(DOLIST (SEGMENT MOVSOL)
  (FORMAT STR "~D~%" SEGMENT)
)
) 
;;(NULL (SECOND '(1)))
;;(SETQ AUX (LIST IDNODO  NODO 0 (MANHATTAN NODO) NIL 0 ))
;;(ISINCLOSEDSTACK (LIST 3  NODO 0 (MANHATTAN NODO) NIL 1 ))
