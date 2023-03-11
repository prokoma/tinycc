package tinycc.util

/** Generates unique names. If a name is generated in a program, it is unique across the entire program.
 * If it is generated in a function, it is unique within the function and the program namespace, but not across other functions. */
class NameGen() {
  private var _takenNames = Set.empty[String]
  private var _children = List.empty[NameGen]

  def isAvailableInChildren(name: String): Boolean =
    !_takenNames.contains(name) && _children.forall(_.isAvailableInChildren(name))

  def isAvailableInParents(name: String): Boolean = true

  def releaseName(name: String): Unit = {
    _takenNames -= name
  }

  def apply(name: String = ""): String = { // TODO: cache
    var ctr = if (name == "") 1 else 2
    var newName = name
    while (newName == "" || !isAvailableInChildren(newName) || !isAvailableInParents(newName)) {
      newName = name + ctr
      ctr += 1
    }
    _takenNames += newName
    newName
  }

  def newChild(): NameGen = {
    val parent = this
    val child = new NameGen {
      override def isAvailableInParents(name: String): Boolean = !_takenNames.contains(name) && parent.isAvailableInParents(name)
    }
    _children ::= child
    child
  }
}