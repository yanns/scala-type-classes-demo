import scala.annotation.implicitNotFound

case class Person(name: String, age: Int)

@implicitNotFound("found no JSON serializer for type ${A}")
trait ToJson[A] {
  def serialize(a: A): String
}

object Test extends App {

  def toJson[A : ToJson](a: A): String =
    implicitly[ToJson[A]].serialize(a)

  implicit val stringToJson: ToJson[String] = new ToJson[String] {
    override def serialize(a: String): String = "\"" + a + "\""
  }
  implicit val intToJson: ToJson[Int] = new ToJson[Int] {
    override def serialize(a: Int): String = a.toString
  }
  implicit val personToJson: ToJson[Person] = new ToJson[Person] {
    override def serialize(a: Person): String =
      s"""{
         |  "name": ${toJson(a.name)},
         |  "age": ${toJson(a.age)},
         |}""".stripMargin
  }
  implicit def listToJson[A](implicit toJson: ToJson[A]): ToJson[List[A]] = new ToJson[List[A]] {
    override def serialize(a: List[A]): String =
      a.map(e ⇒ toJson.serialize(e)).mkString("[\n", ",\n", "\n]")
  }

  val persons = List(Person("bob", 34), Person("alice", 34))
  val json = toJson(persons)
  println(json)
}
