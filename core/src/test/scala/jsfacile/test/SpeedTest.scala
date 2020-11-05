package jsfacile.test

import jsfacile.util.SampleADT.{PresentationData, presentationDataOriginal}
import ParserMacrosTest.presentationDataFormat

object SpeedTest {

	def main(args: Array[String]): Unit = {


		for ( j <- 1 to 10) {

			System.gc();
			{
				import jsfacile.api._

				val presentationDataJson = presentationDataOriginal.toJson
				val ppd = parserOf[PresentationData]

				val start = java.lang.System.nanoTime();
				for (i <- 0 to 100000) {
					val cursor = new CursorStr(presentationDataJson)
					ppd.parse(cursor);
				}
				println("jsfacile:\t" + (java.lang.System.nanoTime() - start) / 1000000000f);
			}

			System.gc();
			{
				import spray.json.{DefaultJsonProtocol, JsObject, JsString, JsValue, RootJsonFormat, enrichAny, enrichString}

				val presentationDataJson = presentationDataOriginal.toJson.compactPrint
				val start = java.lang.System.nanoTime();
				for (i <- 0 to 100000) {
					presentationDataFormat.read(presentationDataJson.parseJson);
				}
				println("spray:\t\t" + (java.lang.System.nanoTime() - start) / 1000000000f);
			}
		}
	}
}
