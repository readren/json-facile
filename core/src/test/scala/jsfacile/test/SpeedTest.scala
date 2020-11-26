package jsfacile.test

import SampleADT.{PresentationData, presentationDataOriginal}
import ParserMacrosTest.presentationDataFormat

object SpeedTest {

	def main(args: Array[String]): Unit = {

		var totalJsFacileDuration: Float = 0;
		var totalSprayDuration: Float = 0;
		for ( j <- 0 to 10) {

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
			if(j > 0) totalJsFacileDuration += jsFacileDuration

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
			if(j > 0) totalSprayDuration += sprayDuration;

			val difference = 100*(sprayDuration - jsFacileDuration)/jsFacileDuration
			println(s"difference: $difference%");

		}

		println("-------------")
		println(s"total jsFacile duration	: $totalJsFacileDuration")
		println(s"total spray duration		: $totalSprayDuration")
		println(s"total difference			: jsfacile is ${100*(totalSprayDuration - totalJsFacileDuration)/totalJsFacileDuration}% faster than spray")
	}
}
