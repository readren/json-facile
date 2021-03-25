package jsfacile

import jsfacile.macros.NothingMacros

/** It is not necessary to import any implicit defined in this package object. The compiler finds them anyway because the [[jsfacile.write.Appender]] trait is defined in the same package.
 * Also, it is not recommended to import any of them so that they have lower precedence than any [[jsfacile.write.Appender]] accesible without prefix (imported or declared in the block scope). */
package object write extends PriorityMediumAppenders with DefaultsRules with BasicAppenders {

	implicit def jaNothing: Appender[Nothing] = macro NothingMacros.materializeNothingAppenderImpl

}
