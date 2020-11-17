package jsfacile.test

import SampleADT.{PresentationData, presentationDataOriginal}
import ParserMacrosTest.presentationDataFormat

object SpeedTest {

	def main(args: Array[String]): Unit = {


		for ( j <- 1 to 10) {

			System.gc();
			val jsFacileDuration = {
				import jsfacile.api._

				val presentationDataJson = presentationDataOriginal.toJson
				val ppd = parserOf[PresentationData]

				val start = java.lang.System.nanoTime();
				for (i <- 0 to 1000000) {
					val cursor = new CursorStr(presentationDataJson)
					ppd.parse(cursor);
				}
				val duration = (java.lang.System.nanoTime() - start) / 1000000000f;
				println("\njsfacile:\t" + duration);
				duration
			}

			System.gc();
			val sprayDuration = {
				import spray.json.{enrichAny, enrichString}

				val presentationDataJson = presentationDataOriginal.toJson.compactPrint
				val start = java.lang.System.nanoTime();
				for (i <- 0 to 1000000) {
					presentationDataFormat.read(presentationDataJson.parseJson);
				}
				val duration = (java.lang.System.nanoTime() - start) / 1000000000f;
				println("spray:\t\t" + duration);
				duration
			}
			println(s"difference: ${100*(sprayDuration - jsFacileDuration)/jsFacileDuration}%");
		}
	}
}
