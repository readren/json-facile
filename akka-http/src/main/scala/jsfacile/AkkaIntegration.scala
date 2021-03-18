package jsfacile

import java.nio.CharBuffer

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import akka.http.scaladsl.marshalling.{Marshaller, Marshalling, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MessageEntity}
import akka.http.scaladsl.unmarshalling.Unmarshaller.UnsupportedContentTypeException
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.Materializer
import akka.util.ByteString

import jsfacile.api.{FromJsonCharArrayConvertible, ParseError}
import jsfacile.read.Parser
import jsfacile.write.{Appender, RecordStr}

object AkkaIntegration {
	type ParseResult[T] = Either[ParseError, T]

	class ParsingException(val parseError: ParseError) extends RuntimeException(parseError.toString)

	class AppendingException(failures: List[Throwable]) extends RuntimeException(failures.last.getMessage)

	/** Puts in the implicit scope a [[FromEntityUnmarshaller]] that translates entities whose content is represented in JSON format to instances of `T`, exposing the parsing successfulness in the result.
	 * Note that this unmarshaller starts the JSON parsing only after obtaining and consolidating the whole entity content.
	 *
	 * @tparam T the data type to which the entity's content should be translated. */
	implicit def createUnmarshallerFromJsonEntityToParseResultOf[T](implicit parser: Parser[T]): FromEntityUnmarshaller[ParseResult[T]] = {

		new Unmarshaller[HttpEntity, ParseResult[T]] {
			override def apply(entity: HttpEntity)(implicit ec: ExecutionContext, materializer: Materializer): Future[ParseResult[T]] = {

				if (entity.contentType == ContentTypes.NoContentType || entity.contentType.mediaType == `application/json`) {

					if (entity.isKnownEmpty()) {
						FastFuture.successful(Array.emptyCharArray.fromJson[T](parser))

					} else {
						val fByteString: Future[ByteString] = entity match {
							case HttpEntity.Strict(_, byteString) =>
								FastFuture.successful(byteString)
							case _ =>
								entity.dataBytes.runFold(ByteString.empty)(_ ++ _)
						}
						for (byteString <- fByteString) yield {
							val charBuffer = Unmarshaller.bestUnmarshallingCharsetFor(entity).nioCharset.decode(byteString.asByteBuffer)
							val array = new Array[Char](charBuffer.length())
							charBuffer.get(array)
							array.fromJson[T]
						}
					}

				} else {
					FastFuture.failed(UnsupportedContentTypeException(Some(entity.contentType), `application/json`))
				}
			}
		}
	}

	/** Puts in the implicit scope a [[FromEntityUnmarshaller]] that translates instances of [[HttpEntity]] whose content is represented in JSON format to instances of `T`, and fails if the parsing fails.
	 * Note that this unmarshaller starts the JSON parsing only after obtaining and consolidating the whole entity content.
	 *
	 * @tparam T the data type to which the entity's content should be translated. */
	implicit def createUnmarshallerFromJsonEntityTo[T](implicit parser: Parser[T]): FromEntityUnmarshaller[T] = {
		this.createUnmarshallerFromJsonEntityToParseResultOf[T](parser).transform {
			ec =>
				_ =>
					fPr =>
						fPr.transform {
							case Success(pr) =>
								pr match {
									case Right(t) => Success(t)
									case Left(pe) => Failure(new ParsingException(pe))
								}

							case Failure(f) =>
								Failure(f)
						}(ec)
		}
	}


	/** Puts in the implicit scope a [[ToEntityMarshaller]] that translates instances of `T` to instances of [[HttpEntity]], and fails if the appending fails.
	 * @tparam T the type of the instance to be translated to an [[HttpEntity]]. */
	implicit def createMarshallerToJsonEntityFrom[T](implicit appender: Appender[T]): ToEntityMarshaller[T] = {
		Marshaller.apply[T, MessageEntity] {
			_ =>
				t =>
					val sb = new java.lang.StringBuilder;
					val record = new RecordStr(sb);
					appender.append(record, t);
					if (record.failures.isEmpty) {
						val marshalling = Marshalling.WithFixedContentType(
							`application/json`,
							() => {
								val length = sb.length();
								if(length == 0) {
									HttpEntity.empty(`application/json`)
								} else {
									// convert the record's StringBuilder to an array of bytes.
									val charArray = new Array[Char](length);
									sb.getChars(0, length, charArray, 0);
									val charBuffer = CharBuffer.wrap(charArray);
									val byteBuffer = `application/json`.charset.nioCharset.encode(charBuffer);
									val byteArray = new Array[Byte](byteBuffer.remaining());
									byteBuffer.get(byteArray);
									// create an HttpEntity containing the resulting array of bytes.
									HttpEntity(`application/json`, byteArray)
								}
							}
						);
						FastFuture.successful(List(marshalling))

					} else {
						FastFuture.failed(new AppendingException(record.failures))
					}
		}
	}


	//	def simpleFromEntityUnmarshaller[T](implicit parser: Parser[T]): FromEntityUnmarshaller[Either[ParseError, T]] = {
	//		Unmarshaller.byteStringUnmarshaller
	//			.forContentTypes(`application/json`)
	//			.mapWithInput { (entity, bytes) =>
	//				if (entity.isKnownEmpty) ""
	//				else bytes.decodeString(Unmarshaller.bestUnmarshallingCharsetFor(entity).nioCharset)
	//			}.map {_.fromJson[T]}
	//	}

}
