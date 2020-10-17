package read

import scala.collection.immutable.ArraySeq

import ProductParserTest._
import ProductParser.jpProduct
import CoproductParser.jpCoproduct
import IterableParser.iterableParser
import MapParser.unsortedMapParser
import MapParser.sortedMapParser

class BorrameTest {


//
//	{
//		import read.ProductParserHelper.FieldInfo;
//		val builder: scala.collection.mutable.ReusableBuilder[(String, read.ProductParserHelper.FieldInfo[_]), scala.collection.immutable.ListMap[String, read.ProductParserHelper.FieldInfo[_]]] = scala.collection.immutable.ListMap.newBuilder[String, read.ProductParserHelper.FieldInfo[_]];
//		builder.addOne(scala.Tuple2.apply[String, read.ProductParserHelper.FieldInfo[read.ProductParserTest.Catalog]]("catalog", read.ProductParserHelper.FieldInfo.apply[read.ProductParserTest.Catalog](Parser.apply[read.ProductParserTest.Catalog](MapParser.unsortedMapParser[Map, read.ProductParserTest.ThingId, read.ProductParserTest.Price](ProductParserTest.jpString, ProductParserTest.jpBigDecimal, NonVariantHolderOfAMapFactory.mapFactory)), scala.None)));
//		builder.addOne(scala.Tuple2.apply[String, read.ProductParserHelper.FieldInfo[read.ProductParserTest.Inventory]]("inventory", read.ProductParserHelper.FieldInfo.apply[read.ProductParserTest.Inventory](Parser.apply[read.ProductParserTest.Inventory](MapParser.unsortedMapParser[Map, read.ProductParserTest.ThingId, Int](ProductParserTest.jpString, ProductParserTest.jpInt, NonVariantHolderOfAMapFactory.mapFactory)), scala.None)));
//		builder.addOne(
//			scala.Tuple2.apply[String, read.ProductParserHelper.FieldInfo[Map[read.ProductParserTest.ThingId, read.ProductParserTest.Thing]]](
//				"things", read.ProductParserHelper.FieldInfo.apply[Map[read.ProductParserTest.ThingId, read.ProductParserTest.Thing]](
//					Parser.apply[Map[read.ProductParserTest.ThingId, read.ProductParserTest.Thing]](
//						MapParser.unsortedMapParser[Map, read.ProductParserTest.ThingId, read.ProductParserTest.Thing](
//							ProductParserTest.jpString,
//							CoproductParser.jpCoproduct[read.ProductParserTest.Thing](
//								(null: read.CoproductParserHelper[read.ProductParserTest.Thing])
//							),
//							NonVariantHolderOfAMapFactory.mapFactory
//						)
//					), scala.None
//				)
//			)
//		);
//		{
//			final class $anon extends AnyRef with read.ProductParserHelper[read.ProductParserTest.PresentationData] {
//				val className: String = "read.ProductParserTest.PresentationData";
//				val fieldsInfo: scala.collection.immutable.ListMap[String, read.ProductParserHelper.FieldInfo[_]] = builder.result();
//				override def createProduct(args: Seq[Any]): read.ProductParserTest.PresentationData = new read.ProductParserTest.PresentationData(args.apply(0).asInstanceOf[read.ProductParserTest.Catalog], args.apply(1).asInstanceOf[read.ProductParserTest.Inventory], args.apply(2).asInstanceOf[Map[read.ProductParserTest.ThingId, read.ProductParserTest.Thing]])
//			};
//			new $anon()
//		}
//	}
}