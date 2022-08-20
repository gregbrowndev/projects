package lectures.part2OOP

object Enums extends App {
  // Scala 3 has first-class support for enums. Scala 2 this is much more of a headache

  enum Permission {
    // generates a sealed set of enumerated types
    case READ, WRITE, EXECUTE, NONE

    // we can add fields and methods
    def openDocument(): Unit =
      if (this == READ) println("opening document...")
      else println("reading not allowed")
  }

  val somePermission: Permission = Permission.READ
  somePermission.openDocument()

  // enums can take constructor args
  enum PermissionWithBits(bits: Int) {
    case READ extends PermissionWithBits(4)     // 100
    case WRITE extends PermissionWithBits(2)    // 010
    case EXECUTE extends PermissionWithBits(1)  // 001
    case NONE extends PermissionWithBits(0)     // 000
  }

  // enums can have companion objects
  object PermissionWithBits {
    def fromBits(bits: Int): PermissionWithBits = {
      // whatever
      PermissionWithBits.NONE
    }
  }

  // Standard API
  // we can inspect the enum value's "ordinal", i.e. index by which it is defined
  val somePermissionOrdinal = somePermission.ordinal

  // we can get iterator of the enum's values
  val allPermission = PermissionWithBits.values

  // we can create an enum from a string
  val readPermission: Permission = Permission.valueOf("READ")
}
