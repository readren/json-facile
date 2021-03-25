package jsfacile.api

import jsfacile.read.Parser

sealed trait ParseError {
	def jsonDoc: String
	def pos: Parser.Pos;
	def toString: String
	def visualizePos: String = {
		val sb = new StringBuilder(jsonDoc);
		sb.append('\n')
		var count = pos;
		while (count > 0) {
			sb.append(' ')
			count -= 1;
		}
		sb.append('^')
		sb.append('\n')
		sb.result();
	}
}
case class ParseIncomplete(jsonDoc: String, pos: Parser.Pos) extends ParseError {
	override def toString = s"""The json input was not entirely consumed. The parsing stopped at position $pos. The remaining fragment surrounded with '>' and '<' is: >${jsonDoc.substring(pos)}<.\nJSON document:\n$visualizePos"""
}
case class ParseMiss(jsonDoc: String, pos: Parser.Pos, expected: String = null) extends ParseError {
	override def toString = s"The parsing missed at position $pos. ${if (expected != null) expected else ""}\nJSON document:\n$visualizePos"
}
case class ParseFailure(jsonDoc: String, pos: Parser.Pos, cause: AnyRef) extends ParseError {
	override def toString = s"The parsing failed at position $pos. Cause: $cause\nJSON document:\n$visualizePos"
}
