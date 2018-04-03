signature TYPES =
sig
  eqtype unique

  datatype ty = RECORD of (Symbol.symbol * ty) list * unique
              | NIL
              | INT
              | BOOL
              | STRING
              | ARRAY of ty * unique
	      | NAME of Symbol.symbol * ty option ref
	      | UNIT
end

structure Types : TYPES =
struct
  type unique = unit ref

  datatype ty = RECORD of (Symbol.symbol * ty) list * unique
              | NIL
              | INT
              | BOOL
              | STRING
              | ARRAY of ty * unique
	      | NAME of Symbol.symbol * ty option ref
	      | UNIT
end

