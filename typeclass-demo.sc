import scala.annotation.implicitNotFound

case class Person(name: String, age: Int)
val persons = List(Person("bob", 34), Person("alice", 34))

@implicitNotFound("found no JSON serializer for type ${A}")
trait ToJson[A] {
  def serialize(a: A): String
}

def toJson[A](a: A)(implicit serializer: ToJson[A]): String =
  serializer.serialize(a)

implicit val stringToJson = new ToJson[String] {
  override def serialize(a: String): String = "\"" + a + "\""
}
implicit val intToJson = new ToJson[Int] {
  override def serialize(a: Int): String = a.toString
}
implicit val personToJson = new ToJson[Person] {
  override def serialize(a: Person): String =
    s"""{
        |  "name": ${toJson(a.name)},
        |  "age": ${toJson(a.age)},
        |}""".stripMargin
}
implicit def listToJson[A](implicit toJson: ToJson[A]) = new ToJson[List[A]] {
  override def serialize(a: List[A]): String =
    a.map(e ⇒ toJson.serialize(e)).mkString("[\n", ",\n", "\n]")
}
toJson("hello")
toJson(3)
toJson(Person("bob", 34))
toJson(persons)