signature ejercicio7 = sig
  val ppEnvEntry: tigersres.EnvEntry -> unit
  val ppTipo: tigertips.Tipo -> unit
  val tabPrint: ('a -> unit) * ('b -> unit) * ('a, 'b) tigertab.Tabla -> unit
end
