open LccTypes

val reduce: environment -> expr -> expr
val laze: environment -> expr -> expr
val eager: environment -> expr -> expr
val isalpha: expr -> expr -> bool
