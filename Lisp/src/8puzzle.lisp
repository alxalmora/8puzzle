;; Resolver 8-puzzle utilizando una heuristica de Manhattan 
(DEFVAR *META* '(1 2 3 4 5 6 7 8 0))
(DEFUN MANHATTAN (ESTADO)
  (SETQ DISTANCIA 0)
  (DOTIMES (N 9)
    (+ 
     DISTANCIA
     (ABS (- (POSX (POSITION N ESTADO)) (POSX N)))
     (ABS (- (POSY (POSITION N ESTADO)) (POSY N)))
   )
)
(DEFUN POSX (INDEX)
 ;;Funcion que ayuda a convertir en posicion cartesiana discreta. 
  (RETURN-FROM POSX (+ (% INDEX 3) 1)) 
)
(DEFUN POSY (INDEX)
  ;;Funcion que ayuda a convertir en posisicon cartesiana discreta
  (RETURN-FROM POSY (+ (floor INDEX 3) 1))
)
(DEFUN SWAP (ESTADO INDEXA INDEXB)
  ;;Funcion para cambiar dos valores, regresa un estado.
  (ROTATEF (NTH INDEXA ESTADO) (NTH INDEXB ESTADO))
)

(DEFUN UP (ESTADO)
 ;;REGRESA NIL SI ES INVALIDO, UN NODO NUEVO SI ES VALIDO.
 

)

