# json-facile
_json-facile_ is a lightweight, boilerplateless and efficient [JSON] implementation in Scala.

* Converts between scala algebraic data types and String JSON documents directly, without any intermediate representations.
* An efficient JSON parser
* Type-class based conversion (no runtime reflection, no intrusion)
* No boilerplate. The conversion type classes are automaticaly generated at compile-time by macros.
* No external dependencies

_json-facile_ allows you to convert between
 * instances of arbitrary Scala types including parameterized algebraic data types 
 * String JSON documents
 * JSON Abstract Syntax Trees (ASTs) with base type JsValue 
 
### Usage
_json-facile_ is really easy to use.
Just bring all relevant elements in scope with
```scala
import jsfacile.api._
```
and do one or more of the following:

1. Convert a non trivial ADT (algebraic data type) instance to JSON representation and back to ADT.

    ```scala
    object test1 {
        object DistanceUnit extends Enumeration {
            val Meter, Millimeter = Value;
        }

        case class Distance(value: Double, unit: DistanceUnit.Value)

        sealed trait Shape
        case class Box(axis: List[Distance]) extends Shape
        case class Sphere(radius: Distance) extends Shape

        sealed trait Thing {
            def enclosingShape: Shape
            def description: String
        }
        case class Table(enclosingShape: Shape, legsAmount: Int, description: String) extends Thing
        case class Shelf(enclosingShape: Shape, levelsAmount: Option[Int], description: String) extends Thing
        case class Ball(enclosingShape: Shape, description: String) extends Thing

        def main(args: Array[String]) {
            // Create a map of things indexed by a string id.
            val thingsById = Map[String, Thing](
                "table_1" -> Table(
                    legsAmount = 4,
                    description = "dinner room",
                    enclosingShape = Box(List(
                        Distance(1.5, DistanceUnit.Meter),
                        Distance(2, DistanceUnit.Meter),
                        Distance(750, DistanceUnit.Millimeter)
                    ))
                ),
                "shelf_1" -> Shelf(
                    levelsAmount = Some(4),
                    description = "for books",
                    enclosingShape = Box(
                        List(Distance(2.5, DistanceUnit.Meter),
                        Distance(2, DistanceUnit.Meter),
                        Distance(500, DistanceUnit.Millimeter)
                    ))
                ),
                "ball_1" -> Ball(
                    description = "soccer",
                    enclosingShape = Sphere(radius = Distance(20, DistanceUnit.Millimeter))
                )
            )

            // Convert the things map to JSON representation
            import jsfacile.api._
            val json = thingsById.toJson

            // Convert the JSON string back to an algebraic data type instance
            // The type parameter is required. 
            val parsedThings = json.fromJson[Map[String, Thing]]

            // check
            assert(parsedThings == Right(thingsById))

        }
    }
    ```
    
2. Convert a parameterized ADT to JSON and back
    
    ```scala
    object test2 {
        sealed abstract class HList {
            def ::[H](head: H): ::[H, this.type] = new ::(head, this)
        }
        final case class ::[+H, +T <: HList](head: H, tail: T) extends HList
        case object Base extends HList

        def main(args: Array[String]) {
            // create an HList instance
            val hList = true :: "text" :: 3 :: Base

            // convert the things map to JSON representation
            import jsfacile.api._
            val json = hList.toJson

            // convert the JSON string back to an HList instance
            val parsedHList = json.fromJson[Boolean :: String :: Int :: Base.type]

            // check
            assert(parsedHList == Right(hList))
        }
    }
    ```
    
To be continued...

  [JSON]: http://json.org
