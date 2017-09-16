
// --- Interfész és Singleton Scala -ban:

trait Greeter {
  def sayHello: Unit
}

trait NameHolder {
  def getName: String
}

object HelloWorld extends Greeter with NameHolder {
  def sayHello = println("Hello " + getName)
  def getName = "World"
}


// próbáld ki:

HelloWorld.sayHello
