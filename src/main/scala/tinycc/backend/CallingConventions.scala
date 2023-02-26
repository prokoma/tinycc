package tinycc.backend

trait CallingConventions extends Product with Serializable

object CallingConventions {
  case object Cdecl extends CallingConventions
}
