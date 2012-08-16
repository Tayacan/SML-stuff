(* Calculates the derivative of a mathematical function
* From the book pg. 112 *)

load "Math";
load "Real";

(* We want +, - * and / operators that are infix*)
infix 6 ++ --;
infix 7 ** //;

(* A mathematical expression *)
datatype fexpr =
	  Const of real
	| X
	| ++ of fexpr * fexpr | -- of fexpr * fexpr
	| ** of fexpr * fexpr | // of fexpr * fexpr
	| Sin of fexpr | Cos of fexpr
	| Ln of fexpr | Exp of fexpr;

fun D (Const _)		= Const 0.0
  | D X			= Const 1.0
  | D (fe1 ++ fe2)	= (D fe1) ++ (D fe2)
  | D (fe1 -- fe2)	= (D fe1) -- (D fe2)
  | D (fe1 ** fe2)	= (D fe1) ** fe2 ++ fe1 ** (D fe2)
  | D (fe1 // fe2)	= ((D fe1) ** fe2 -- fe1 ** (D fe2)) // (fe2 ** fe2)
  | D (Sin fe)		= (Cos fe) ** (D fe)
  | D (Cos fe)		= ((Const ~1.0) ** (Sin fe)) ** (D fe)
  | D (Ln fe)		= (D fe) // fe
  | D (Exp fe)		= (Exp fe) ** (D fe) ;

fun toString (Const r)		= Real.toString r
  | toString X			= "x"
  | toString (fe1 ++ fe2)	= "(" ^ (toString fe1) ^ " + " ^ (toString fe2) ^ ")"
  | toString (fe1 -- fe2)	= "(" ^ (toString fe1) ^ " - " ^ (toString fe2) ^ ")"
  | toString (fe1 ** fe2)	= "(" ^ (toString fe1) ^ " * " ^ (toString fe2) ^ ")"
  | toString (fe1 // fe2)	= "(" ^ (toString fe1) ^ " / " ^ (toString fe2) ^ ")"
  | toString (Sin fe)		= "(sin(" ^ (toString fe) ^ "))"
  | toString (Cos fe)		= "(cos(" ^ (toString fe) ^ "))"
  | toString (Ln fe)		= "(ln(" ^ (toString fe) ^ "))"
  | toString (Exp fe)		= "(exp(" ^ (toString fe) ^ "))" ;

fun compute (Const r, y)	= r
  | compute (X, y)		= y
  | compute (fe1 ++ fe2,y)	= compute (fe1,y) + compute (fe2,y)
  | compute (fe1 -- fe2,y)	= compute (fe1,y) - compute (fe2,y)
  | compute (fe1 ** fe2,y)	= compute (fe1,y) * compute (fe2,y)
  | compute (fe1 // fe2,y)	= compute (fe1,y) / compute (fe2,y)
  | compute (Sin fe,y)		= Math.sin (compute (fe,y))
  | compute (Cos fe,y)		= Math.cos (compute (fe,y))
  | compute (Ln fe,y)		= Math.ln (compute (fe,y))
  | compute (Exp fe,y)		= Math.exp (compute (fe,y)) ;
