signature COLOR =
sig
  structure Frame : FRAME

  type allocation = Frame.register Temp.Table.table

  val color : { interference : Liveness.igraph
              ,      initial : allocation
              ,    spillCost : Graph.node -> int 
              ,    registers : Frame.register list
              } -> allocation * Temp.temp list
end

structure Color : COLOR =
struct
  structure Frame = RiscVFrame

  structure A = Assem
  structure E = ErrorMsg
  structure F = RiscVFrame
  structure G = Graph
  structure M = Temp

  type allocation = F.register M.Table.table

  fun color ({interference, initial, spillCost, registers}) = (M.Table.empty,  [])

end
