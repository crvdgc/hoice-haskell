module Data.CounterExample where

type FuncIx = Int
type VarVal = Int

type FuncData = (FuncIx, [VarVal])

data Dataset = Pos [[FuncData]]               -- ^ at least one is @true@
             | Neg [[FuncData]]               -- ^ at least one is @false@
             | Imp [([FuncData], [FuncData])] -- ^ if lhs all @true@, rhs has at least one @true@


