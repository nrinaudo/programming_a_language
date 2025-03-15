lazy val root = Project(id = "programming_a_language", base = file("."))
  .settings(moduleName := "root")
  .aggregate(untyped, type_checking, typed_ast, typed_env, type_inference, type_inference_state)

lazy val untyped = project
lazy val type_checking = project
lazy val typed_ast = project
lazy val typed_env = project
lazy val type_inference = project
lazy val type_inference_state = project
