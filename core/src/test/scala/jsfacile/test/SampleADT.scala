package jsfacile.test

object SampleADT {

	sealed trait Arbol[V];
	case class Rama[V](a: Arbol[V], b: Arbol[V]) extends Arbol[V];
	case class Hoja[V](v: V) extends Arbol[V];

	object DistanceUnit extends Enumeration {
		val Meter, Millimeter = Value;
	}

	case class Distance(value: Double, unit: DistanceUnit.Value)

	sealed trait Shape
	case class Box(axis: List[Distance]) extends Shape
	case class Sphere(radius: Distance) extends Shape

	@jsfacile.annotations.discriminatorField(value = "type", required = false) sealed trait Thing {
		def enclosingShape: Shape
		def description: String
	}
	case class Table(enclosingShape: Shape, legsAmount: Short, description: String) extends Thing
	case class Shelf(enclosingShape: Shape, levelsAmount: Option[Byte], description: String) extends Thing
	case class Ball(enclosingShape: Shape, description: String) extends Thing

	type Price = BigDecimal
	type ThingId = String
	type Catalog = Map[ThingId, Price]
	type Inventory = Map[ThingId, Int]
	case class PresentationData(catalog: Catalog, inventory: Inventory, things: Map[ThingId, Thing])

	/////////////////
	// Some values //

	val tableA: (ThingId, Table) = "table_A" -> Table(legsAmount = 4, description = "dinner room", enclosingShape = Box(List(Distance(1.5, DistanceUnit.Meter), Distance(2, DistanceUnit.Meter), Distance(750, DistanceUnit.Millimeter))));
	val shelfA: (ThingId, Shelf) = "shelf_A" -> Shelf(levelsAmount = Some(4), description = "for books", enclosingShape = Box(List(Distance(2.5, DistanceUnit.Meter), Distance(2, DistanceUnit.Meter), Distance(500, DistanceUnit.Millimeter))));
	val ballA: (ThingId, Ball) = "ball_A" -> Ball(description = "soccer", enclosingShape = Sphere(radius = Distance(20, DistanceUnit.Millimeter)));
	val catalog = Map("table_A" -> BigDecimal(123.4), "shelf_A" -> BigDecimal(32.1))
	val inventory = Map("table_A" -> 4, "shelf_A" -> 3, "ball_A" -> 8)
	val presentationDataOriginal: PresentationData = PresentationData(catalog, inventory, Map(tableA, shelfA, ballA))

}
